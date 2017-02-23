### INIT

library(dplyr)
library(tidyr)
library(ggplot2)
options(stringsAsFactors = F)

setwd("~/Documents/postal_codes/")
scriptdir = "~/Documents/gitrep/SE_MFR_GEODATA/"
geo_dir = paste(scriptdir, "KommunRT90/", sep="")

source(paste(scriptdir, "mapping_helper.R", sep=""))

m = read.table("tmp/mfr_edu_clean.csv", sep=";", h=T, dec=",")


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
adjustAndPlot = function(df, name){
    ### RUN A REGRESSION TO GET ADJUSTED GA
    linearreg = lm(GRDBS ~ age_cat + MLANGD + ROK1 + PARITET + nonswed_mother + 
                            edu_cat + KON + AR+ DIABETES + HYPERTON + GRMETOD, na.action=na.exclude, data = df)
    sink(paste("tmp/regression_summary_", name, ".txt", sep=""))
    summary(linearreg)
    sink(NULL)
    
    sink(paste("tmp/demographics_", name, ".txt", sep=""))
    summary(df[, c("age_cat", "MLANGD", "ROK1", "PARITET", "nonswed_mother", 
                      "edu_cat", "KON", "AR", "DIABETES", "HYPERTON", "GRMETOD")])
    sink(NULL)
    
    df$GA_adj = mean(df$GRDBS) + residuals(linearreg)
    df$PTD_adj = df$GA_adj<259
    
    ### SUMMARIZE PER KOMMUN
    df_sum = mutate(df, lan_kom = sprintf("%04d", lan_kom)) %>%
        group_by(lan_kom) %>%
        filter(!is.na(PTD_adj)) %>%
        summarize(ncases = sum(PTD_adj), ncontrs = sum(!PTD_adj), rate = sum(PTD_adj)/n())
    
    ## these don't exist in the map anyway
    df_sum$rate[df_sum$ncases == 0] = NA    
    df_sum = filter(df_sum, !is.na(rate))
    
    ### PLOT
    na_legend = NA
    pdf(paste("plots/FINAL_", name, "_adj_PTD.pdf", sep=""), width=9.5, height=8)
    fun_plot_final(geo_dir, as.data.frame(df_sum), "rate", ownPalette, na_legend)
    dev.off()
    
    ## REGIONS WHICH (SORT OF) RELIABLY DIFFER FROM THE MEAN PTD RATE
    df_sum$p = unlist(Map(function(x, n) binom.test(x, n, mean(df$PTD_adj, na.rm=T))$p.value,
                          df_sum$ncases, df_sum$ncases+df_sum$ncontrs))
    
    df_sum$rate[df_sum$p > 0.1] = NA
    df_sum = filter(df_sum, !is.na(rate))
    
    ### PLOT, w/ p filter
    na_legend = "p>0.1"
    pdf(paste("plots/FINAL_", name, "_adj_PTD_p010.pdf", sep=""), width=9.5, height=8)
    fun_plot_final(geo_dir, as.data.frame(df_sum), "rate", ownPalette, na_legend)
    dev.off()
    
}

adjustAndPlot(spont, "spont")
adjustAndPlot(iatr, "iatr")
adjustAndPlot(m, "all")




### DO CHI^2 TO CHECK WHETHER PTD DISTRIBUTION IS INDEPENDENT OF REGION

test_got = filter(spont_adj, grepl("^01", lan_kom))
test_sto = filter(spont_adj, grepl("^12", lan_kom))
test_ska = filter(spont_adj, grepl("^14", lan_kom))

chisq.test(matrix(c(test_got$ncases, test_got$ncontrs), ncol=2))
chisq.test(matrix(c(test_sto$ncases, test_sto$ncontrs), ncol=2))
chisq.test(matrix(c(test_ska$ncases, test_ska$ncontrs), ncol=2))

test_tmp = filter(spont_adj, ncases>5)
chisq.test(matrix(c(test_tmp$ncases, test_tmp$ncontrs), ncol=2))
