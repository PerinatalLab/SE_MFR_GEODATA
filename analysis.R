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

### RUN A REGRESSION TO GET ADJUSTED GA
linearregspont = lm(GRDBS ~ age_cat + MLANGD + ROK1 + PARITET + nonswed_mother + 
                      edu_cat + KON + AR+ DIABETES + HYPERTON + GRMETOD, na.action=na.exclude, data = spont)
sink("tmp/regression_summary.txt")
summary(linearregspont)
sink(NULL)

sink("tmp/demographics_spont.txt")
summary(spont[, c("age_cat", "MLANGD", "ROK1", "PARITET", "nonswed_mother", 
                          "edu_cat", "KON", "AR", "DIABETES", "HYPERTON", "GRMETOD")])
sink(NULL)

spont$GA_adj = mean(spont$GRDBS) + residuals(linearregspont)
spont$PTD_adj = spont$GA_adj<259


### SUMMARIZE AND PLOT
ownPalette = rev(brewer.pal(10, "RdYlGn"))
n_cutoff = 5

spont = mutate(spont, lan_kom = sprintf("%04d", lan_kom)) %>%
        group_by(lan_kom)
spont_unadj = summarize(spont, ncases = sum(PTD), ncontrs = sum(!PTD), rate = sum(PTD)/n())
spont_adj = filter(spont, !is.na(PTD_adj)) %>%
        summarize(ncases = sum(PTD_adj), ncontrs = sum(!PTD_adj), rate = sum(PTD_adj)/n())

spont_unadj$rate[spont_unadj$ncases < n_cutoff] = NA
spont_adj$rate[spont_adj$ncases < n_cutoff] = NA

spont_unadj = filter(spont_unadj, !is.na(rate))
spont_adj = filter(spont_adj, !is.na(rate))

pdf("plots/FINAL_spont_unadj_PTD.pdf",width=9.5, height=8)
fun_plot_final(geo_dir, as.data.frame(spont_unadj), "rate", ownPalette)
dev.off()

pdf("plots/FINAL_spont_adj_PTD.pdf",width=9.5, height=8)
fun_plot_final(geo_dir, as.data.frame(spont_adj), "rate", ownPalette)
dev.off()


### DO CHI^2 TO CHECK WHETHER PTD DISTRIBUTION IS INDEPENDENT OF REGION

test_got = filter(spont_adj, grepl("^01", lan_kom))
test_sto = filter(spont_adj, grepl("^12", lan_kom))
test_ska = filter(spont_adj, grepl("^14", lan_kom))

chisq.test(matrix(c(test_got$ncases, test_got$ncontrs), ncol=2))
chisq.test(matrix(c(test_sto$ncases, test_sto$ncontrs), ncol=2))
chisq.test(matrix(c(test_ska$ncases, test_ska$ncontrs), ncol=2))

test_tmp = filter(spont_adj, ncases>5)
chisq.test(matrix(c(test_tmp$ncases, test_tmp$ncontrs), ncol=2))
