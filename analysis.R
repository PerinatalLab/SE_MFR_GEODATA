### INIT

library(dplyr)
library(tidyr)
library(ggplot2)
library(lme4)
options(stringsAsFactors = F)

setwd("~/Documents/postal_codes/")
scriptdir = "~/Documents/gitrep/SE_MFR_GEODATA/"
geo_dir = paste(scriptdir, "KommunRT90/", sep="")

source(paste(scriptdir, "mapping_helper.R", sep=""))

m = read.table("tmp_mfr_edu_clean.csv", sep=";", h=T, dec=",")


### CATEGORIZE CONT. VARIABLES and ASSIGN REFERENCE GROUPS
m$PTD = m$GRDBS<259

m$edu_cat = cut(m$EDU,breaks = c(1,3,5,7),labels = 1:3)
m$age_cat = cut(m$MALDER,breaks = c(13,19,30,40,50),labels = 1:4)
m$age_cat = factor(m$age_cat,levels = c(2,1,3,4))

m$nonswed_mother = m$MFODLAND!="SVERIGE"
m$ROK1 = factor(m$ROK1,levels = c(1,2,3))
m$GRMETOD = factor(m$GRMETOD)
m$KON = factor(m$KON)

m$PARITET[m$PARITET > 5] = 5
m$PARITET = factor(m$PARITET)

m$DIABETES = !is.na(m$DIABETES)
m$HYPERTON = !is.na(m$HYPERTON)


### KEEP ONLY SPONTANEOUS
spont = filter(m, FLSPONT==1, is.na(FLINDUKT))
spont = filter(spont, ELEKAKUT==2 | is.na(ELEKAKUT) & is.na(SECFORE))

iatr = anti_join(m, spont, by="lpnr_BARN")



### ADJUST AND PLOT SPONT; IATR; ALL
ownPalette = rev(brewer.pal(10, "RdYlGn"))
adjustAndPlot = function(df, name, useFE = FALSE){
    ### RUN A REGRESSION TO GET ADJUSTED GA
	if(useFE){
		name = paste("KommunFE", name, sep="_")
		
		# create a factor using largest kommun as reference:
		df = filter(df, !is.na(lan_kom))
		df$flan_kom = factor(df$lan_kom)
		df$flan_kom = relevel(df$flan_kom, ref="180")
		linearreg = lm(GRDBS ~ age_cat + MLANGD + ROK1 + PARITET + nonswed_mother + 
					   	edu_cat + KON + AR+ DIABETES + HYPERTON + GRMETOD + flan_kom,
					   na.action = na.exclude, data = df)
	} else {
		# do not include lan_kom in the model
		linearreg = lm(GRDBS ~ age_cat + MLANGD + ROK1 + PARITET + nonswed_mother + 
					   	edu_cat + KON + AR+ DIABETES + HYPERTON + GRMETOD,
					   na.action=na.exclude, data = df)
	}

    sink(paste("tmp_regression_summary_", name, ".txt", sep=""))
    print(summary(linearreg))
    sink(NULL)
    
    sink(paste("tmp_demographics_", name, ".txt", sep=""))
    print(summary(df[, c("age_cat", "MLANGD", "ROK1", "PARITET", "nonswed_mother", 
                      "edu_cat", "KON", "AR", "DIABETES", "HYPERTON", "GRMETOD")]))
    sink(NULL)
    
    df$GA_adj = mean(df$GRDBS) + residuals(linearreg)
    df$PTD_adj = df$GA_adj<259
    
    ### SUMMARIZE PER KOMMUN
    df_sum = mutate(df, lan_kom = sprintf("%04d", lan_kom)) %>%
        group_by(lan_kom) %>%
        filter(!is.na(PTD_adj)) %>%
        summarize(ncases = sum(PTD_adj), ncontrs = sum(!PTD_adj), rate = sum(PTD_adj)/n(), mean = mean(GA_adj))
    
    ## these don't exist in the map anyway
    df_sum$rate[df_sum$ncases == 0] = NA    
    df_sum = filter(df_sum, !is.na(rate))
    
    ### PLOT, everything
    na_legend = NA
    pdf(paste("plots/FINAL_", name, "_adj_PTD.pdf", sep=""), width=9.5, height=8)
    fun_plot_final(geo_dir, as.data.frame(df_sum), "rate", ownPalette, na_legend)
    dev.off()
    
    ### PLOT, w/ p filter
    if(!useFE){
    	## store the summaries for funnel plots
    	write.table(df_sum, paste("tmp_kommundata_", name, ".csv", sep=""), col.names=T, row.names=F, quote=F)
    	
    	## regions which kind of reliably differ from mean PTD rate:
    	df_sum$p = unlist(Map(function(x, n) binom.test(x, n, mean(df$PTD_adj, na.rm=T))$p.value,
    						  df_sum$ncases, df_sum$ncases+df_sum$ncontrs))
    	
    	df_sum$rate[df_sum$p > 0.1] = NA
    	df_sum = filter(df_sum, !is.na(rate))
    	
    	na_legend = "p>0.1"
    	pdf(paste("plots/FINAL_", name, "_adj_PTD_p010.pdf", sep=""), width=9.5, height=8)
    	fun_plot_final(geo_dir, as.data.frame(df_sum), "rate", ownPalette, na_legend)
    	dev.off()
    }
    
    return(summary(linearreg))
}

modspont = adjustAndPlot(spont, "spont", FALSE)
modiatr = adjustAndPlot(iatr, "iatr", FALSE)
modall = adjustAndPlot(m, "all", FALSE)


### FIT KOMMUN-LEVEL FIXED EFFECTS & DECOMPOSE VARIANCE
modspontfe = adjustAndPlot(spont, "spont", TRUE)
modiatrfe = adjustAndPlot(iatr, "iatr", TRUE)
modallfe = adjustAndPlot(m, "all", TRUE)

# variance attributed to the added lan_kom factor:
modspont$r.squared; modspontfe$r.squared; modspontfe$r.squared - modspont$r.squared
modiatr$r.squared; modiatrfe$r.squared; modiatrfe$r.squared - modiatr$r.squared
modall$r.squared; modallfe$r.squared; modallfe$r.squared - modall$r.squared

anova(linearreg, linearreg2) # F ca. 11, p below any precision


### MAKE FUNNELS
sumspont = read.table("tmp_kommundata_spont.csv", h=T)
sumiatr = read.table("tmp_kommundata_iatr.csv", h=T)
sumall = read.table("tmp_kommundata_all.csv", h=T)

# m - global mean estimated from the full dataframe
# v - global variance estimated from the full dataframe
# TODO: subtract R^2 from v
plotFunnel = function(df_sum, name, m, v){
	df_sum = mutate(df_sum, n = ncases+ncontrs)
	nullspread = data.frame(y=1:max(sqrt(df_sum$n))) %>%
		mutate(xmax = m + 1.96*v/y, xmin = m - 1.96*v/y)
	p1 = ggplot(df_sum) + geom_point(aes(x=mean, y=sqrt(n)), color="turquoise3") +
		geom_rect(aes(ymin=y, ymax=y+1, xmin=xmin, xmax=xmax), nullspread, alpha=0.2) +
		coord_cartesian(xlim = range(df_sum$mean)) +
		theme_bw()
	ggsave(plot=p1, paste("plots/funnel_", name, ".png", sep=""))
}
plotFunnel(sumspont, "spont", mean(spont$GRDBS), var(spont$GRDBS)*0.98)
plotFunnel(sumiatr, "iatr", mean(iatr$GRDBS), var(iatr$GRDBS)*0.98)
plotFunnel(sumall, "all", mean(m$GRDBS), var(m$GRDBS)*0.98)

### DO CHI^2 TO CHECK WHETHER PTD DISTRIBUTION IS INDEPENDENT OF REGION

test_got = filter(spont_adj, grepl("^01", lan_kom))
test_sto = filter(spont_adj, grepl("^12", lan_kom))
test_ska = filter(spont_adj, grepl("^14", lan_kom))

chisq.test(matrix(c(test_got$ncases, test_got$ncontrs), ncol=2))
chisq.test(matrix(c(test_sto$ncases, test_sto$ncontrs), ncol=2))
chisq.test(matrix(c(test_ska$ncases, test_ska$ncontrs), ncol=2))

test_tmp = filter(spont_adj, ncases>5)
chisq.test(matrix(c(test_tmp$ncases, test_tmp$ncontrs), ncol=2))
