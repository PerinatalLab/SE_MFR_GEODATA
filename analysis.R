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

m = filter(m, !is.na(lan_kom))
m$flan_kom = factor(m$lan_kom)
m$flan_kom = relevel(m$flan_kom, ref="180")


### SPLIT OFF SPONTANEOUS / IATR
spont = filter(m, FLSPONT==1, is.na(FLINDUKT))
spont = filter(spont, ELEKAKUT==2 | is.na(ELEKAKUT) & is.na(SECFORE))
iatr = anti_join(m, spont, by="lpnr_BARN")

nonmiss = filter(m, !is.na(ROK1), !is.na(PARITET), !is.na(nonswed_mother),
				 !is.na(edu_cat), !is.na(KON), !is.na(AR), !is.na(DIABETES),
				 !is.na(HYPERTON), !is.na(GRMETOD), !is.na(flan_kom))
table(nonmiss$age_cat)
table(nonmiss$ROK1)
table(nonmiss$PARITET)
table(nonmiss$nonswed_mother)
table(nonmiss$edu_cat)
table(nonmiss$KON)
table(nonmiss$DIABETES)
table(nonmiss$HYPERTON)
table(nonmiss$GRMETOD)

### RUN A REGRESSION TO GET ADJUSTED GA OR PROVINCE EFFECTS
fitRegression = function(df, name, useFE = FALSE, useRE = FALSE){
	if(useFE){
		name = paste("KommunFE", name, sep="_")
		linearreg = lm(GRDBS ~ age_cat + MLANGD + ROK1 + PARITET + nonswed_mother + 
					   	edu_cat + KON + AR + DIABETES + HYPERTON + GRMETOD + flan_kom,
					   na.action = na.exclude, data = df, model=F)
	} else if(useRE) {
		name = paste("KommunRE", name, sep="_")
		# include lan_kom as a random effect
		linearreg = lmer(GRDBS ~ age_cat + MLANGD + ROK1 + PARITET + nonswed_mother + 
			 			edu_cat + KON + AR + DIABETES + HYPERTON + GRMETOD + (1|flan_kom),
			 		   na.action = na.exclude, data = df, REML=T)
	} else {
		# do not include lan_kom in the model
		linearreg = lm(GRDBS ~ age_cat + MLANGD + ROK1 + PARITET + nonswed_mother + 
					   	edu_cat + KON + AR + DIABETES + HYPERTON + GRMETOD,
					   na.action = na.exclude, data = df, model=F)
	}
	save(linearreg, file=paste0("model_lm_", name, ".RData"))

    sink(paste("tmp_regression_summary_", name, ".txt", sep=""))
    print(summary(linearreg))
    sink(NULL)
    
    return(linearreg)
}

### SUMMARIZE PER KOMMUN
summarizeKommun = function(df, lmObject, name, useFE = FALSE){
	# must remove the ones that were omitted in regression
	df$flan_kom[is.na(df$age_cat) | is.na(df$MLANGD) | is.na(df$ROK1) | is.na(df$PARITET) |
				is.na(df$nonswed_mother) | is.na(df$edu_cat) | is.na(df$KON) | is.na(df$AR) |
				is.na(df$DIABETES) | is.na(df$HYPERTON) | is.na(df$GRMETOD)] = NA
	
	if(useFE){
		# prediction "after removing risk factors":
		# all cont variables at mean, factors at reference
		refind = data.frame(age_cat=factor(2), MLANGD=mean(df$MLANGD), ROK1=factor(1), PARITET=factor(1),
							nonswed_mother=FALSE, edu_cat=factor(1), KON=factor(1), AR=mean(df$AR),
							DIABETES=F, HYPERTON=F, GRMETOD=factor(1), flan_kom=df$flan_kom)
		name = paste("KommunFE", name, sep="_")
	} else {
		refind = data.frame(age_cat=factor(2), MLANGD=mean(df$MLANGD), ROK1=factor(1), PARITET=factor(1),
							nonswed_mother=FALSE, edu_cat=factor(1), KON=factor(1), AR=mean(df$AR),
							DIABETES=F, HYPERTON=F, GRMETOD=factor(1), flan_kom=factor("180"))
	}
	
	# X_ref*beta [+ prov*beta_prov] + epsilons
	# can use E(Y) + epsilons instead, gives ~0.3 d difference
	df$GA_adj = predict(lmObject, refind) + residuals(lmObject)
	df$PTD_adj = df$GA_adj<259
	countryPTD = mean(df$PTD_adj, na.rm=T)
	countryGA = mean(df$GA_adj, na.rm=T)
	
	# get kommun PTD rates, mean GAs, and p-values for each
	# - PTD tested against overall rate w/ binom.test
	# - GA tested against overall mean w/ t.test
	# - overall data taken after "adjustment for risk factors"
	df_sum = mutate(df, lan_kom = sprintf("%04d", lan_kom)) %>%
		group_by(lan_kom) %>%
		filter(!is.na(PTD_adj), sum(!is.na(GA_adj))>1)
	df_sum = df_sum %>%
		summarize(ncases = sum(PTD_adj), ncontrs = sum(!PTD_adj),
				  rate = sum(PTD_adj)/n(), mean = mean(GA_adj),
				  PTD_p = binom.test(ncases, n(), p=countryPTD, alternative="two")$p.value,
				  GA_p = t.test(GA_adj, mu=countryGA, alternative="two")$p.value
		)
	
	## these don't exist in the map anyway
	df_sum$rate[df_sum$ncases == 0] = NA
	df_sum = filter(df_sum, !is.na(rate))
	
	# store it
	write.table(df_sum, paste("sum_kommundata_", name, ".csv", sep=""), col.names=T, row.names=F, quote=F)
	
	# store some demographics
	sink(paste("tmp_demographics_", name, ".txt", sep=""))
	print(summary(df[, c("age_cat", "MLANGD", "ROK1", "PARITET", "nonswed_mother", 
						 "edu_cat", "KON", "AR", "DIABETES", "HYPERTON", "GRMETOD")]))
	sink(NULL)

	return(df_sum)
}

### PLOT, anything
makeMaps = function(kommunSum, name){
	# pad lan_kom with zeroes
	# kommunSum$lan_kom = sprintf("%04d", kommunSum$lan_kom)

	# make complete maps without caring about significance
	na_legend = NA
	
	ownPalette = brewer.pal(10, "RdYlGn")
	pdf(paste("plots/FINAL_", name, "_adj_GA.pdf", sep=""), width=9.5, height=8)
	fun_plot_final(geo_dir, as.data.frame(kommunSum), "mean", ownPalette, na_legend)
	dev.off()
	
	ownPalette = rev(brewer.pal(10, "RdYlGn"))
	pdf(paste("plots/FINAL_", name, "_adj_PTD.pdf", sep=""), width=9.5, height=8)
	fun_plot_final(geo_dir, as.data.frame(kommunSum), "rate", ownPalette, na_legend)
	dev.off()

	# now color only regions which kind of reliably differ from mean PTD rate
	# significance cutoff:
	## FDR
	kommunSum$GA_q = p.adjust(kommunSum$GA_p, method="fdr")
	kommunSumPlot = filter(kommunSum, !is.na(rate), GA_q<0.1)
	na_legend = "FDR > 10%"
	
	ownPalette = brewer.pal(10, "RdYlGn")
	pdf(paste("plots/FINAL_", name, "_adj_GA_q010.pdf", sep=""), width=9.5, height=8)
	fun_plot_final(geo_dir, as.data.frame(kommunSumPlot), "mean", ownPalette, na_legend)
	dev.off()
	
	## P-VALUE
	kommunSumPlot = filter(kommunSum, !is.na(rate), GA_p<0.1)
	na_legend = "p > 0.1"
	
	ownPalette = brewer.pal(10, "RdYlGn")
	pdf(paste("plots/FINAL_", name, "_adj_GA_p010.pdf", sep=""), width=9.5, height=8)
	fun_plot_final(geo_dir, as.data.frame(kommunSumPlot), "mean", ownPalette, na_legend)
	dev.off()
	
	## FDR
	kommunSum$PTD_q = p.adjust(kommunSum$PTD_p, method="fdr")
	kommunSumPlot = filter(kommunSum, !is.na(rate), PTD_q<0.1)
	na_legend = "FDR > 10%"
	
	ownPalette = rev(brewer.pal(10, "RdYlGn"))
	pdf(paste("plots/FINAL_", name, "_adj_PTD_q010.pdf", sep=""), width=9.5, height=8)
	fun_plot_final(geo_dir, as.data.frame(kommunSumPlot), "rate", ownPalette, na_legend)
	dev.off()

	## P-VALUE
	kommunSumPlot = filter(kommunSum, !is.na(rate), PTD_p<0.1)
	na_legend = "p > 0.1"
	
	ownPalette = rev(brewer.pal(10, "RdYlGn"))
	pdf(paste("plots/FINAL_", name, "_adj_PTD_p010.pdf", sep=""), width=9.5, height=8)
	fun_plot_final(geo_dir, as.data.frame(kommunSumPlot), "rate", ownPalette, na_legend)
	dev.off()
}

## function for MAIN FIGURE plots:
## minimal plots and .eps format
makeMapsPubl = function(kommunSum, name){
	# make complete map without caring about significance
	na_legend = NA
	ownPalette = rev(brewer.pal(10, "RdYlGn"))
	
	pdf(paste("plots/MINIMAL_", name, "_adj_PTD.pdf", sep=""), width=7, height=8)
	fun_plot_final(geo_dir, as.data.frame(kommunSum), "rate", ownPalette, na_legend, minimal=T)
	dev.off()
	
	setEPS()
	postscript(paste("plots/MINIMAL_", name, "_adj_PTD.eps", sep=""), width=7, height=8)
	fun_plot_final(geo_dir, as.data.frame(kommunSum), "rate", ownPalette, na_legend, minimal=T)
	dev.off()
	
	# now color only regions which kind of reliably differ from mean PTD rate
	# significance cutoff:
	## FDR
	kommunSum$PTD_q = p.adjust(kommunSum$PTD_p, method="fdr")
	kommunSumPlot = filter(kommunSum, !is.na(rate), PTD_q<0.1)
	na_legend = "FDR > 10%"
	
	pdf(paste("plots/MINIMAL_", name, "_adj_PTD_q010.pdf", sep=""), width=7, height=8)
	fun_plot_final(geo_dir, as.data.frame(kommunSumPlot), "rate", ownPalette, na_legend, minimal=T)
	dev.off()
	
	setEPS()
	postscript(paste("plots/MINIMAL_", name, "_adj_PTD_q010.eps", sep=""), width=7, height=8)
	fun_plot_final(geo_dir, as.data.frame(kommunSumPlot), "rate", ownPalette, na_legend, minimal=T)
	dev.off()
	
	## P-VALUE
	kommunSumPlot = filter(kommunSum, !is.na(rate), PTD_p<0.1)
	na_legend = "p > 0.1"
	
	pdf(paste("plots/MINIMAL_", name, "_adj_PTD_p010.pdf", sep=""), width=7, height=8)
	fun_plot_final(geo_dir, as.data.frame(kommunSumPlot), "rate", ownPalette, na_legend, minimal=T)
	dev.off()
	
	setEPS()
	postscript(paste("plots/MINIMAL_", name, "_adj_PTD_p010.eps", sep=""), width=7, height=8)
	fun_plot_final(geo_dir, as.data.frame(kommunSumPlot), "rate", ownPalette, na_legend, minimal=T)
	dev.off()
}

### ADJUST AND PLOT SPONT; IATR; ALL
modspont = fitRegression(spont, "spont", FALSE, FALSE)
sumspont = summarizeKommun(spont, modspont, "spont", FALSE)
makeMaps(sumspont, "spont")

modiatr = fitRegression(iatr, "iatr", FALSE, FALSE)
sumiatr = summarizeKommun(iatr, modiatr, "iatr", FALSE)
makeMaps(sumiatr, "iatr")

modall = fitRegression(m, "all", FALSE, FALSE)
sumall = summarizeKommun(m, modall, "all", FALSE)
makeMaps(sumall, "all")


### FIT KOMMUN-LEVEL FIXED EFFECTS & DECOMPOSE VARIANCE
modspont = fitRegression(spont, "spont", TRUE, FALSE)
sumspont = summarizeKommun(spont, modspont, "spont", TRUE)
makeMaps(sumspont, "spont_KommunFE")

modiatr = fitRegression(iatr, "iatr", TRUE, FALSE)
sumiatr = summarizeKommun(iatr, modiatr, "iatr", TRUE)
makeMaps(sumiatr, "iatr_KommunFE")

modall = fitRegression(m, "all", TRUE, FALSE)
sumall = summarizeKommun(m, modall, "all", TRUE)
makeMaps(sumall, "all_KommunFE")

### MAIN FIGURE PLOTS
makeMapsPubl(sumall, "all_KommunFE")

# variance attributed to the added lan_kom factor:
modspont$r.squared; modspontfe$r.squared; modspontfe$r.squared - modspont$r.squared
modiatr$r.squared; modiatrfe$r.squared; modiatrfe$r.squared - modiatr$r.squared
modall$r.squared; modallfe$r.squared; modallfe$r.squared - modall$r.squared

anova(linearreg, linearreg2) # F ca11, p below any precision


### MAKE FUNNELS
sumspont = read.table("sum_kommundata_KommunFE_spont.csv", h=T)
sumiatr = read.table("sum_kommundata_KommunFE_iatr.csv", h=T)
sumall = read.table("sum_kommundata_KommunFE_all.csv", h=T)

## read in province names
library(rgdal)
library(ggrepel)
dsn = readOGR(dsn="/home/julius/Documents/gitrep/SE_MFR_GEODATA/KommunRT90/", layer="Kommun_RT90_region")
provnames = data.frame(lan_kom = dsn$KnKod, name = dsn$KnNamn)

# v - global variance estimated from the full dataframe
# (- would be more precise to calculate from the regression object)
plotFunnel = function(df_sum, name, v, label){
	df_sum = mutate(df_sum, n = ncases+ncontrs) %>%
		mutate(lan_kom = sprintf("%04d", lan_kom)) %>%
		left_join(provnames, by="lan_kom")
	mu = weighted.mean(df_sum$mean, df_sum$n)
	
	# get nominal and Bonferroni-adjusted SE thresholds
	level1 = qnorm(0.025, lower.tail = F)
	level2 = qnorm(0.025/nrow(df_sum), lower.tail = F)
	nullspread = data.frame(y=1:max(sqrt(df_sum$n))) %>%
		mutate(xmax = mu + level1*sqrt(v)/y, xmin = mu - level1*sqrt(v)/y)
	nullspread2 = data.frame(y=1:max(sqrt(df_sum$n))) %>%
		mutate(xmax = mu + level2*sqrt(v)/y, xmin = mu - level2*sqrt(v)/y)
	p1 = ggplot(df_sum) +
		geom_point(aes(x=mean, y=sqrt(n)), color="turquoise3") +
		geom_rect(aes(ymin=y, ymax=y+1, xmin=xmin, xmax=xmax), nullspread, alpha=0.2) +
		geom_rect(aes(ymin=y, ymax=y+1, xmin=xmin, xmax=xmax), nullspread2, alpha=0.2) +
		geom_vline(aes(xintercept=mu), color="grey60") +
		coord_cartesian(xlim = range(df_sum$mean)) +
		xlab("Mean gestational age") + ylab(expression(sqrt(population~size))) +
		theme_bw() +
		theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
	
	# add labels for the top 5 provinces
	if(label){
		p1 = p1 + geom_label_repel(aes(x=mean, y=sqrt(n), label=name), top_n(df_sum, 5, n))
	} else {
		name = paste(name, "nolab", sep="_")
	}
	
	ggsave(plot=p1, paste("plots/funnel_", name, ".png", sep=""), width=8, height=6, units="in")
}
plotFunnel(sumspont, "spont_KommunFE", var(spont$GRDBS)*0.984, FALSE)
plotFunnel(sumiatr, "iatr_KommunFE", var(iatr$GRDBS)*0.971, FALSE)
plotFunnel(sumall, "all_KommunFE", var(m$GRDBS)*0.983, FALSE)


##### spam

load("model_lm_KommunFE_spont.RData")
library(broom)
library(lme4)
library(nlme)

### EXPLORE RANDOM EFFECTS
modspont = fitRegression(spont, "spont", FALSE, TRUE)
modiatr = fitRegression(iatr, "iatr", FALSE, TRUE)
modall = fitRegression(m, "all", FALSE, TRUE)

res = tidy(linearreg)
betasRE = tidy(modspont)
betasFE = filter(res, !grepl("flan", term))
bind_rows(rand=betasRE, fixed=betasFE, .id="effects") %>%
	filter(term!="(Intercept)", term!="DIABETESTRUE", !is.na(std.error)) %>%
	ggplot(aes(x=term, color=effects)) +
	geom_pointrange(aes(y=estimate, ymin=estimate-1.96*std.error, ymax=estimate+1.96*std.error),
					position=position_dodge(width=1), size=0.3) +
	ylab(expression(paste("estimated ",  beta))) +
	theme_bw() + theme(axis.text.x=element_text(angle=90))

provbetasRE = ranef(modspont)$flan_kom
provbetasRE = data.frame(estimate = provbetasRE[,1] - provbetasRE[1,1],
						 term=paste("flan_kom", rownames(provbetasRE), sep=""))
provbetasFE = filter(res, grepl("flan", term))
ns = group_by(spont, flan_kom) %>%
	summarize(n=n()) %>%
	mutate(flan_kom=paste("flan_kom", flan_kom, sep=""))

bind_rows(fixed=provbetasFE, rand=provbetasRE, .id="effects") %>%
	inner_join(ns, by=c("term"="flan_kom")) %>%
	filter(n>100) %>%
	ggplot() + geom_density(aes(x=estimate, fill=effects), alpha=0.3) + theme_bw()
table(ns$flan_kom[ns$n>100] %in% sprintf("flan_kom%i", as.numeric(provnames$lan_kom)))
inner_join(provbetasFE, provbetasRE, by="term") %>% 
	inner_join(ns, by=c("term"="flan_kom")) %>%
	filter(n>100) %>%
	ggplot() + geom_point(aes(x=estimate.y, y=estimate.x)) +
	geom_abline(slope=1, intercept=0, col="red") + 
	coord_equal(xlim=c(-2.5,1.5), ylim=c(-2.5,1.5)) +
	ylab("fixed eff") + xlab("random eff") + theme_bw()


### MAP INTERACTIONS

load("model_lm_KommunFE_spont.RData")
length(linearreg$residuals)
length(spontnm$AR)


spontnm = filter(spont, !is.na(GRDBS), !is.na(age_cat), !is.na(MLANGD),
				 !is.na(ROK1), !is.na(PARITET), !is.na(nonswed_mother),
				 !is.na(edu_cat), !is.na(KON), !is.na(AR), !is.na(DIABETES),
				 !is.na(HYPERTON), !is.na(GRMETOD), !is.na(flan_kom))
spontnm$GA_adj = linearreg$residuals + mean(spontnm$GRDBS)
qplot(spontnm$GA_adj)

#table(cut(spont$AR, include.lowest = T, breaks=c(1998, 2003, 2008, 2013)))
spontnm$period = cut(spontnm$AR, include.lowest = T, breaks=c(1998, 2005, 2013))
spontnm_sum = group_by(spontnm, flan_kom, period) %>%
	summarize(n=n(), mean = mean(GA_adj, na.rm=T), rate = mean(GA_adj<259, na.rm=T)) %>%
	mutate(nprov = sum(n))
spontnm_sum = mutate(spontnm_sum, areaNum = as.numeric(substr(sprintf("%04s", flan_kom), 1, 2)))
spontnm_sum$area = "other"
spontnm_sum$area[which(spontnm_sum$areaNum==1)] = "Stockholm"
spontnm_sum$area[which(spontnm_sum$areaNum==12)] = "Skåne"
spontnm_sum$area[which(spontnm_sum$areaNum==14)] = "Göteborg"
spontnm_sum$area = factor(spontnm_sum$area, levels=c("Stockholm", "Göteborg", "Skåne", "other"))

filter(spontnm_sum, nprov>100) %>%
	mutate(size = cut(nprov, include.lowest=T, breaks=c(1e2, 5e2, 1e3, 2e3, 5e3, 1e4, 3e5))) %>%
	ggplot(aes(x=period, y=mean)) + geom_line(aes(group=flan_kom)) +
	facet_wrap(~area) +
	theme_bw()


# pad lan_kom with zeroes
spontnm_sum$lan_kom = sprintf("%04d", as.numeric(as.character(spontnm_sum$flan_kom)))
spontnm_sum = filter(spontnm_sum, n>=100)
spontnm_plot = spread(spontnm_sum[,c("lan_kom", "period", "mean")], period, value=mean) %>%
	mutate(change = `(2005,2013]`-`[1998,2005]`) %>% filter(!is.na(change))
na_legend = "n<100"

ownPalette = brewer.pal(10, "RdYlGn")
name = "interactions"
pdf(paste("plots/FINAL_", name, "_adj_GA.pdf", sep=""), width=9.5, height=8)
fun_plot_final(geo_dir, as.data.frame(spontnm_plot), "change", ownPalette, na_legend)
dev.off()


#### TEMP
sumall = read.table("sum_kommundata_KommunFE_all.csv", h=T)

ownPalette = rev(brewer.pal(10, "RdYlGn"))
sumall$lan_kom = sprintf("%04d", sumall$lan_kom)
na_legend = NA

pdf(paste("plots/TEST_all_KommunFE_adj_PTD.pdf", sep=""), width=7, height=8)
fun_plot_final(geo_dir, as.data.frame(sumall), "rate", ownPalette, na_legend, minimal=T)
dev.off()
