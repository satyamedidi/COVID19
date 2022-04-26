# load latest Covid-2019 data: confirmed cases


library(reshape2)

us_cases <- as.data.frame(data.table::fread("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_US.csv"))


us_cases <- melt(us_cases, id=c("UID","iso2","iso3","code3","FIPS","Admin2","Province_State","Country_Region","Lat","Long_","Combined_Key"  ))

names(us_cases)[names(us_cases)=="variable"] <- "Date"
names(us_cases)[names(us_cases)=="value"] <- "Cases"



us_deaths <- as.data.frame(data.table::fread("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_US.csv"))




us_deaths <- melt(us_deaths, id=c("UID","iso2","iso3","code3","FIPS","Admin2","Province_State","Country_Region","Lat","Long_","Combined_Key","Population" ))

names(us_deaths)[names(us_deaths)=="variable"] <- "Date_deaths"
names(us_deaths)[names(us_deaths)=="value"] <- "deaths"


us_total <- cbind(us_cases,us_deaths)

write.csv(us_total, "input_data/us_total.csv", row.names=F)

rm(list = ls())
