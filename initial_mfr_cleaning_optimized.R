options(stringsAsFactors = F)
library(dplyr)

getRate = function(df, st){
        data.frame(summarize(df, rate=mean(GRDBS<259, na.rm=T), n=n()), stage=st)
}

setwd("~/Documents/postal_codes/")
mfr = read.table("mfr_sarah_maxi_2016june10.csv", h=T, sep=",")
ratedf = getRate(mfr, "initial")

m = filter(mfr, !is.na(lpnr_BARN), !is.na(lpnr_mor))
ratedf = bind_rows(ratedf, getRate(m, "missing_ids"))

edu = read.table("education_all.txt", h=T)
e = filter(edu, EDU!="*")
m = left_join(m, e, by=c("lpnr_mor"="ID", "AR"="YEAR"))

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

write.table(m, "tmp/mfr_edu_clean.csv", sep=",", quote=F, row.names=F, col.names=T)
write.table(ratedf, "tmp/cleaning_loss.txt", sep="\t", quote=F, row.names=F, col.names=T)
