### INIT
	
library(dplyr)
library(tidyr)
library(ggplot2)

options(stringsAsFactors = F)

setwd("/mnt/ws2/postal_codes/")

### post-analysis of kommun properties
kom = read.table("sum_kommundata_KommunFE_all.csv", h=T)

provnames = read.table("provnames.txt", h=T, sep="\t")
crimesv = read.table("~/Documents/postal_codes/violent_crimes_2001_per100k.txt", sep=";", h=T, encoding = 'latin1')
crimesv = crimesv[,1:2]
# missing values
crimesv = filter(crimesv, violentcrimes_per100k!=0)

komc = inner_join(provnames, crimesv, by=c("name"="Kommun"))
komc = inner_join(kom, komc, by="lan_kom")
komc$n = komc$ncases + komc$ncontrs
komc$label = komc$name
komc$label[floor(komc$lan_kom/100)!=12 & floor(komc$lan_kom/100)!=14 & floor(komc$lan_kom/100)!=1] = NA


ggplot(komc, aes(x=violentcrimes_per100k, y=mean)) +
	geom_point() + geom_smooth(method="lm", aes(weight = n)) +
	theme_bw()

ggplot(komc, aes(x=violentcrimes_per100k, y=mean)) +
	geom_point() + geom_smooth(method="lm", aes(weight = n)) +
	geom_label(aes(label=label)) +
	theme_bw()

summary(lm(mean ~ violentcrimes_per100k, data=komc, weights=n))
summary(glm(rate ~ violentcrimes_per100k, data=komc, weights=n, family="binomial"))

##
inc = read.table("~/Documents/postal_codes/hh_median_income_ksek_2011.csv", sep=";", h=T, encoding="latin1")
inc = inc[,c("lan_kom", "Income")]
komci = inner_join(komc, inc, by="lan_kom")

ggplot(komci, aes(x=Income, y=mean)) +
	geom_point(aes(size=n)) + geom_smooth(method="lm", aes(weight = n)) +
	theme_bw()
ggplot(komci, aes(x=Income, y=mean)) +
	geom_point() + geom_smooth(method="lm", aes(weight = n)) +
	geom_label(aes(label=label)) +
	theme_bw()

summary(lm(mean ~ Income, data=komci, weights=n))

##
land = read.table("~/Documents/postal_codes/builtland_2010.csv", sep=";", h=T, encoding="latin1")
land$Typ = sapply(strsplit(land$Typ, " "), "[[", 1)
land = spread(land, Typ, value=area) %>%
	mutate(fracBuilt = built/total)
land = land[,c("lan_kom", "fracBuilt")]

komcil = inner_join(komci, land, by="lan_kom")
summary(lm(mean ~ fracBuilt, data=komcil, weights=n))

ggplot(komcil, aes(x=fracBuilt, y=mean)) +
	geom_point(aes(size=n)) + geom_smooth(method="lm", aes(weight = n)) +
	theme_bw()
ggplot(komcil, aes(x=fracBuilt, y=mean)) +
	geom_point() + geom_smooth(method="lm", aes(weight = n)) +
	geom_label(aes(label=label)) +
	theme_bw()

##
nat = read.table("~/Documents/postal_codes/nature_distancem_2013.csv", sep=";", h=T, encoding="latin1")
nat = nat[c("lan_kom", "distance")]

komciln = inner_join(komcil, nat, by="lan_kom")
summary(lm(mean ~ distance, data=komciln, weights=n))

ggplot(komciln, aes(x=distance, y=mean)) +
	geom_point(aes(size=n)) + geom_smooth(method="lm", aes(weight = n)) +
	theme_bw()
ggplot(komciln, aes(x=distance, y=mean)) +
	geom_point() + geom_smooth(method="lm", aes(weight = n)) +
	geom_label(aes(label=label)) +
	theme_bw()

##
urb = read.table("~/Documents/postal_codes/urban_fracpop_2010.csv", sep=";", h=T, encoding="latin1")

urb$Typ = sapply(strsplit(urb$Typ, " "), "[[", 1)
urb = spread(urb, Typ, value=pop) %>%
	mutate(fracUrban = within/total)
urb = urb[,c("lan_kom", "fracUrban")]

komcilnu = inner_join(komciln, urb, by="lan_kom")
summary(lm(mean ~ fracUrban, data=komcilnu, weights=n))
summary(glm(rate ~ fracUrban, data=komcilnu, weights=n, family="binomial"))

ggplot(komcilnu, aes(x=fracUrban, y=mean)) +
	geom_point(aes(size=n)) + geom_smooth(method="lm", aes(weight = n)) +
	theme_bw()

cor(as.matrix(komcilnu[,c("fracUrban", "distance", "violentcrimes_per100k", "Income", "fracBuilt")]))
summary(lm(mean ~ fracUrban + distance + violentcrimes_per100k + Income + fracBuilt, data=komcilnu, weights=n))

##
emis = read.table("~/Documents/postal_codes/emissions_co_2010.csv", sep=";", h=T, encoding="latin1")
emis$Typ = sapply(strsplit(emis$Typ, " "), "[[", 1)
emis = spread(emis, Typ, value=tonnes)
emis = emis[,c("lan_kom", "CO", "PM10", "PM25")]

pop = read.table("~/Documents/postal_codes/population_2010.csv", sep=";", h=T, encoding="latin1")
emis = inner_join(emis, pop, by="lan_kom")
emis = mutate(emis, CO=CO/pop, PM10=PM10/pop, PM25=PM25/pop)

komcilnue = inner_join(komcilnu, emis, by="lan_kom")
summary(lm(mean ~ CO + PM25, data=komcilnue, weights=n))

ggplot(komcilnue, aes(x=CO, y=mean)) +
	geom_point(aes(size=n)) + geom_smooth(method="lm", aes(weight = n)) +
	theme_bw()
ggplot(komcilnue, aes(x=PM25, y=mean)) +
	geom_point() + geom_smooth(method="lm", aes(weight = n)) +
	geom_label(aes(label=label)) +
	theme_bw()

summary(lm(mean ~ distance + Income + fracBuilt + PM25 + CO, data=komcilnue, weights=n))

## NOTE: employment is for 16+
empl = read.table("~/Documents/postal_codes/employment_2010.csv", sep=";", h=T, encoding="latin1")
empl$Typ = sapply(strsplit(empl$Typ, " "), "[[", 1)
empl = spread(empl, Typ, value=number)
empl = mutate(empl, fracEmpl = employed/(employed+not))
empl = empl[,c("lan_kom", "fracEmpl")]

komcilnuee = inner_join(komcilnue, empl, by="lan_kom")
summary(lm(mean ~ fracEmpl, data=komcilnuee, weights=n))

ggplot(komcilnuee, aes(x=fracUrban, y=mean)) +
	geom_point() + geom_smooth(method="lm", aes(weight = n)) +
	geom_label(aes(label=label)) +
	theme_bw()

##
komlong = komcilnuee[,c("lan_kom", "n", "mean", "name", "label", "distance", "violentcrimes_per100k", "Income",
			 "fracBuilt", "fracUrban", "fracEmpl", "CO", "PM10", "PM25")] %>%
	gather(key="exposure", value="value", distance:PM25)

ggplot(komlong, aes(x=value, y=mean)) +
	facet_wrap(~exposure, scales="free_x") +
	geom_point(aes(col=log(n, 10))) + geom_smooth(method="lm", aes(weight = n)) +
	scale_color_continuous(low="black", high="orange") +
	theme_bw()

