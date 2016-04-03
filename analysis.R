#
# In this script, I try to see if there is a connection (correlation) between discrimination against
# minorities (e.g. homophobia) and the the state system (capitalism vs. socialism). As a proxy
# for the state system, I use, for example, the effective tax rate, which should be a reaosnable
# proxy for the 'size of the state'.
#
#

#install.packages("plyr")
library(plyr)
#install.packages("data.table")   # install only needed once
library(data.table)   
library(ggplot2)
library(reshape)

setwd("~/projects/sci.homophobia_capitalism")

# load world value survey data
#
# cite as: 
# --------
# WVS (2015). World Value Survey 1981-2015 official aggregate v.20150418, 2015. World Values Survey 
# Association (www.worldvaluessurvey.org). Aggregate File Producer: JDSystems, Madrid.
#
load("WVS_Longitudinal_1981_2014_R_v2015_04_18.rdata")
country_codes = read.csv("country_codes.csv", sep=",", header=TRUE, strip.white=TRUE)
wave_codes = read.csv("wave_codes.csv", sep=",", header=TRUE, strip.white=TRUE)

homophobia_questions = WVS_Longitudinal_1981_2014_R_v2015_04_18[c("S002", "S003", "S012", "A124_09", "F118")]
names(homophobia_questions)[names(homophobia_questions)=="S002"] <- "wave_code"
names(homophobia_questions)[names(homophobia_questions)=="S003"] <- "country_code"
names(homophobia_questions)[names(homophobia_questions)=="S012"] <- "interview_date"
names(homophobia_questions)[names(homophobia_questions)=="A124_09"] <- "homosexual_neighbor"
names(homophobia_questions)[names(homophobia_questions)=="F118"] <- "homosexuality_justifiable"
h_questions = merge(x = homophobia_questions, y = country_codes, by = "country_code", all.x = TRUE)
h_questions = merge(x = h_questions, y = wave_codes, by = "wave_code", all.x = TRUE) 
h_questions = h_questions[, c("wave", "country", "homosexual_neighbor", "homosexuality_justifiable")]

# load tax rates
tax_rates=read.csv("tax_rates.world_bank/caecd391-57a1-4aab-bf4d-73ab8ac366c2_v2.csv", sep=",", header=TRUE, strip.white=TRUE, comment.char = "#", skip = 4)  # the first 4 lines contain notting, John Snow!
names(tax_rates)[names(tax_rates)=="Country.Name"] <- "country"
tax_rates = setDT(tax_rates)
tax_rates[, ":="("2005-2009" = rowMeans(.SD)), by = country, .SDcols = c("X2005", "X2006", "X2007", "X2008", "X2009")]
tax_rates[, ":="("2010-2014" = rowMeans(.SD)), by = country, .SDcols = c("X2010", "X2011", "X2012", "X2013", "X2014")]
tax_rates = tax_rates[, .SD, .SDcols=c("country", "2005-2009", "2010-2014")]
tax_rates = melt(tax_rates, id=c("country"))
setnames(tax_rates, "variable", "wave")
setnames(tax_rates, "value", "tax_rate")

questions_tax = merge(x = h_questions, y = tax_rates, by = c("country", "wave"), all.x = TRUE)
questions_tax = questions_tax[! is.na(questions_tax$tax_rate),]

questions_tax = setDT(questions_tax)
aggregated = ddply(questions_tax, .(wave, country), numcolwise(median))

ggplot(data=aggregated[aggregated$country %in% c("Mexico"),], aes(group=country, x=wave, y=homosexuality_justifiable)) + geom_line()

plot_data = aggregated[aggregated$homosexuality_justifiable >= 0 & aggregated$tax_rate <= 100 ,] 
plt_ha_tax_agg = ggplot(plot_data, aes(homosexuality_justifiable, tax_rate)) +
  geom_point() +
  labs(title = "Homosexuality Acceptable vs. Tax Rate (by country/wave)") +
  geom_smooth(method=lm) 
plot(plt_ha_tax_agg)
hj_tr = plot_data[, c("homosexuality_justifiable", "tax_rate")]
cor(hj_tr, method="pearson")

#plt_ha_tax_raw = ggplot(questions_tax[questions_tax$homosexuality_justifiable >= 0,], aes(homosexuality_justifiable, tax_rate)) +
#  geom_point() +
#  labs(title = "Homosexuality Acceptable vs. Tax Rate")
#plot(plt_ha_tax_raw)

