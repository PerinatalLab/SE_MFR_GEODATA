options(stringsAsFactors = F)
library(dplyr)

getRate = function(df, st){
        data.frame(summarize(df, rate=mean(GRDBS<259, na.rm=T), n=n()), stage=st)
}

mfr = read.table("~/Documents/swed_mfr_games/mfr_sarah_maxi_2016june10.csv", h=T, sep=",")
ratedf = getRate(mfr, "initial")

m = filter(mfr, !is.na(lpnr_BARN), !is.na(lpnr_mor))
ratedf = bind_rows(ratedf, getRate(m, "missing_ids"))

m = filter(m, BORDF2==1)
ratedf = bind_rows(ratedf, getRate(m, "singletons"))

b = 0
for (col in grep("[MBG]DIAG", colnames(m))){
         b = b + grepl("^O4[013456]", m[, col])
}
m = mutate(m, placProblems = b) %>% filter(b==0)
ratedf = bind_rows(ratedf, getRate(m, "plac_probl"))

m = filter(m, is.na(MISSB))
ratedf = bind_rows(ratedf, getRate(m, "malformations"))

m = filter(m, GRMETOD==1 | GRMETOD==2)
ratedf = bind_rows(ratedf, getRate(m, "ga_method"))

m = filter(m, MALDER>13 & MALDER<46)
ratedf = bind_rows(ratedf, getRate(m, "mat_age"))

m = filter(m, MLANGD>=140 & MLANGD<=210)
ratedf = bind_rows(ratedf, getRate(m, "mat_height"))

m = filter(m, is.na(NJURSJUK), is.na(EPILEPSI), is.na(ULCOLIT), is.na(SLE), is.na(HYPERTON))
ratedf = bind_rows(ratedf, getRate(m, "mat_conditions"))

m = filter(m, is.na(TSECTIO) | TSECTIO!=1)
ratedf = bind_rows(ratedf, getRate(m, "prev_sectio"))

m = filter(m, is.na(FLINDUKT), !is.na(FLSPONT))
ratedf = bind_rows(ratedf, getRate(m, "spontaneous"))

m = filter(m, ELEKAKUT==2 | (is.na(ELEKAKUT) & is.na(SECFORE)))
ratedf = bind_rows(ratedf, getRate(m, "curr_sectio"))

ratedf

