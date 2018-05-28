# Script for secondary analysis of various
# kommun-level factors as proxies for urbanity.

### INIT
library(dplyr)
library(tidyr)
library(ggplot2)
library(broom)
options(stringsAsFactors = F)
setwd("~/Documents/postal_codes/")

kom = read.table("sum_kommundata_KommunFE_all.csv", h=T)
nrow(kom) # 296

# get population size and names
pop = read.table("population_2010.csv", sep=";", h=T, encoding="latin1")
kom = inner_join(kom, pop, by="lan_kom")

# add names and a label for Stock-Gbg-Malm municipalities
kom$label = kom$Kommun
kom$label[floor(kom$lan_kom/100)!=12 & floor(kom$lan_kom/100)!=14 & floor(kom$lan_kom/100)!=1] = NA
nrow(kom) # 290


## Data from BRÅ
## (Våldsbrott, antal brott)
crimesv = read.table("violent_crimes_2010.csv", sep=";", h=T, encoding = 'latin1')
crimesv = crimesv[,1:2]
# missing values
crimesv = filter(crimesv, violentCrimes!=0)

komc = inner_join(kom, crimesv, by="Kommun") %>%
	mutate(violentCrimes = violentCrimes/pop)

## Data from SCB
## median disposable household income, thousands of crowns
## (Disponibel inkomst för hushåll. Medianvärde, tkr efter region, ålder och år)
inc = read.table("hh_median_income_ksek_2011.csv", sep=";", h=T, encoding="latin1")
inc = inc[,c("lan_kom", "Income")]

komci = inner_join(komc, inc, by="lan_kom")

## fraction of land that has buildings
## (Markanvändningen i Sverige, hektar efter region, markanvändningsklass och vart 5:e år)
land = read.table("builtland_2010.csv", sep=";", h=T, encoding="latin1")
land$Typ = sapply(strsplit(land$Typ, " "), "[[", 1)
land = spread(land, Typ, value=area) %>%
	mutate(fracBuilt = built/total)
land = land[,c("lan_kom", "fracBuilt")]

komcil = inner_join(komci, land, by="lan_kom")

## mean distance to protected nature areas
## (Folkmängd inom zon runt skyddad natur och medelavstånd till skyddad natur, efter region)
## (Medelavstånd, meter i jämna 100-tal efter region och år)
nat = read.table("nature_distancem_2013.csv", sep=";", h=T, encoding="latin1")
nat = nat[c("lan_kom", "distance")]
colnames(nat) = c("lan_kom", "distNature")

komciln = inner_join(komcil, nat, by="lan_kom")

## fraction of population living in urban areas
## (Folkmängd per tätort efter region, typ av område och vart 5:e år)
urb = read.table("urban_fracpop_2010.csv", sep=";", h=T, encoding="latin1")
urb$Typ = sapply(strsplit(urb$Typ, " "), "[[", 1)
urb = spread(urb, Typ, value=pop) %>%
	mutate(fracUrban = within/total)
urb = urb[,c("lan_kom", "fracUrban")]

komcilnu = inner_join(komciln, urb, by="lan_kom")

## CO, PM<10, PM<2.5 emissions from municipality, tons
## (Utsläpp till luft efter region, ämne och år)
emis = read.table("emissions_co_2010.csv", sep=";", h=T, encoding="latin1")
emis$Typ = sapply(strsplit(emis$Typ, " "), "[[", 1)
emis = spread(emis, Typ, value=tonnes)
emis = emis[,c("lan_kom", "CO", "PM10", "PM25")]

komcilnue = inner_join(komcilnu, emis, by="lan_kom")
komcilnue = mutate(komcilnue, CO=CO/pop, PM10=PM10/pop, PM25=PM25/pop)

## fraction of 16+ population employed
## (Befolkningen 16+ år (RAMS) efter region, sysselsättning, ålder och kön. År 2004 - 2016)
empl = read.table("employment_2010.csv", sep=";", h=T, encoding="latin1")
empl$Typ = sapply(strsplit(empl$Typ, " "), "[[", 1)
empl = spread(empl, Typ, value=number)
empl = mutate(empl, fracEmpl = employed/(employed+not))
empl = empl[,c("lan_kom", "fracEmpl")]

komcilnuee = inner_join(komcilnue, empl, by="lan_kom")


## regressions
komcilnuee$n = komcilnuee$ncases + komcilnuee$ncontrs
summary(lm(mean ~ violentCrimes, data=komcilnuee, weights=n))
summary(lm(mean ~ Income, data=komcilnuee, weights=n))
summary(lm(mean ~ fracBuilt, data=komcilnuee, weights=n))
summary(lm(mean ~ distNature, data=komcilnuee, weights=n))
summary(lm(mean ~ fracUrban, data=komcilnuee, weights=n))
summary(lm(mean ~ CO, data=komcilnuee, weights=n))
summary(lm(mean ~ PM10, data=komcilnuee, weights=n))
summary(lm(mean ~ PM25, data=komcilnuee, weights=n))
summary(lm(mean ~ fracEmpl, data=komcilnuee, weights=n))

pvalues = bind_rows(
	tidy(lm(mean ~ violentCrimes, data=komcilnuee, weights=n))[2,],
	tidy(lm(mean ~ Income, data=komcilnuee, weights=n))[2,],
	tidy(lm(mean ~ fracBuilt, data=komcilnuee, weights=n))[2,],
	tidy(lm(mean ~ distNature, data=komcilnuee, weights=n))[2,],
	tidy(lm(mean ~ fracUrban, data=komcilnuee, weights=n))[2,],
	tidy(lm(mean ~ CO, data=komcilnuee, weights=n))[2,],
	tidy(lm(mean ~ PM10, data=komcilnuee, weights=n))[2,],
	tidy(lm(mean ~ PM25, data=komcilnuee, weights=n))[2,],
	tidy(lm(mean ~ fracEmpl, data=komcilnuee, weights=n))[2,])


## correlations and multiple regr
cor(as.matrix(komcilnuee[,c("fracUrban", "distNature", "violentCrimes", "Income", "fracBuilt")]))
summary(lm(mean ~ fracUrban + distNature + violentCrimes + Income +
		   fracBuilt + CO + PM10 + PM25 + fracEmpl,
		   data=komcilnuee, weights=n))

## MAIN PLOT
komlong = komcilnuee[,c("lan_kom", "n", "mean", "Kommun", "label",
						"distNature", "violentCrimes", "Income",
						"fracBuilt", "fracUrban", "fracEmpl",
						"CO", "PM10", "PM25")] %>%
	gather(key="exposure", value="value", distNature:PM25)
pvalues = group_by(komlong, exposure) %>%
	summarize(xmax=(max(value)-min(value))*0.9+min(value), ymax=280.6) %>%
	left_join(pvalues, by=c("exposure"="term"))

## add nice names for plotting
komlong$exposure = factor(komlong$exposure,
						  levels=c("fracBuilt", "fracUrban", "fracEmpl", "Income",
						  		 "violentCrimes", "distNature", "CO", "PM10", "PM25"),
						  labels=c("built land, %", "pop in urban areas, %", "employment, %",
						  		 "median income, kSEK", "violent crimes, yearly pc",
						  		 "mean distance to nature, m", "CO emissions, t", 
						  		 "PM10 emissions, t", "PM2.5 emissions, t"))
pvalues$exposure = factor(pvalues$exposure,
						  levels=c("fracBuilt", "fracUrban", "fracEmpl", "Income",
						  		 "violentCrimes", "distNature", "CO", "PM10", "PM25"),
						  labels=c("built land, %", "pop in urban areas, %", "employment, %",
						  		 "median income, kSEK", "violent crimes, yearly pc",
						  		 "mean distance to nature, m", "CO emissions, t", 
						  		 "PM10 emissions, t", "PM2.5 emissions, t"))

ggplot(komlong, aes(x=value, y=mean)) +
	facet_wrap(~exposure, scales="free_x") +
	geom_point(aes(col=n)) + geom_smooth(method="lm", aes(weight = n)) +
	scale_color_continuous(low="black", high="orange", trans="log", breaks=c(1e3, 1e4, 1e5),
						   labels=c("1", "10", "100"), name="population, thousands") +
	geom_text(aes(x=xmax, y=ymax, label=paste("p =", signif(p.value, 2))), pvalues,
			  col="#3366ff", size=3) +
	ylab("mean gestational age, days") +
	theme_bw() + theme(legend.position="bottom", panel.grid=element_blank())
ggsave("plots/kommunfactors.tiff", width=9, height=6, units="in", dpi=300)

