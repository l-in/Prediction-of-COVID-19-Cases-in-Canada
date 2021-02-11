library(readxl) # to read excel documents
library(R.utils) # to unzip files
library(stringr)
library(stringi)
library(syuzhet) # sentiment analysis
library(padr) # adding rows for missing dates
library(imputeTS)
library(MASS)
library(DataCombine)
library(timeDate)
library(forecast)
library(randomForest)
library(glmnet)
library(e1071)
library(rpart)
library(pls)
library(neuralnet)
library(boot)
library(nnet)


set.seed(2020)

##### Data Wrangling #####

### Import data for cases ###
cases = read.csv("covid19.csv") # all cases

# include cases from 01-Jan-2020 to 31-Aug-2020
cases = cases[as.Date(cases$date, format = "%d-%m-%Y") <= as.Date("31-08-2020", format = "%d-%m-%Y"),]

# convert date to Date object
cases$date = as.Date(cases$date, format = "%d-%m-%Y")

# add rows for missing dates (for all of Canada)
cases.full = pad(cases[cases$prname == "Canada",], 
                 start_val = as.Date("01-01-2020", format = "%d-%m-%Y"), 
                 end_val = as.Date("31-08-2020", format = "%d-%m-%Y"))

# add zero cases for the missing dates at the start of the timeline
# testing capacity was not overwhelmed at the start, so they would have been tabulated
# hence count zero cases for the missing dates at the beginning


# subset cases by province and pad with missing values like above
cases.on = cases[cases$prname == "Ontario",]
cases.on = pad(cases.on, start_val = as.Date("01-01-2020", format = "%d-%m-%Y"), 
               end_val = as.Date("31-08-2020", format = "%d-%m-%Y"))

cases.bc = cases[cases$prname == "British Columbia",]
cases.bc = pad(cases.bc, start_val = as.Date("01-01-2020", format = "%d-%m-%Y"), 
               end_val = as.Date("31-08-2020", format = "%d-%m-%Y"))

cases.qc = cases[cases$prname == "Quebec",]
cases.qc = pad(cases.qc, start_val = as.Date("01-01-2020", format = "%d-%m-%Y"), 
               end_val = as.Date("31-08-2020", format = "%d-%m-%Y"))

cases.al = cases[cases$prname == "Alberta",]
cases.al = pad(cases.al, start_val = as.Date("01-01-2020", format = "%d-%m-%Y"), 
               end_val = as.Date("31-08-2020", format = "%d-%m-%Y"))

cases.sk = cases[cases$prname == "Saskatchewan",]
cases.sk = pad(cases.sk, start_val = as.Date("01-01-2020", format = "%d-%m-%Y"), 
               end_val = as.Date("31-08-2020", format = "%d-%m-%Y"))

cases.mb = cases[cases$prname == "Manitoba",]
cases.mb = pad(cases.mb, start_val = as.Date("01-01-2020", format = "%d-%m-%Y"), 
               end_val = as.Date("31-08-2020", format = "%d-%m-%Y"))

cases.nb = cases[cases$prname == "New Brunswick",]
cases.nb = pad(cases.nb, start_val = as.Date("01-01-2020", format = "%d-%m-%Y"), 
               end_val = as.Date("31-08-2020", format = "%d-%m-%Y"))

cases.nl = cases[cases$prname == "Newfoundland and Labrador",]
cases.nl = pad(cases.nl, start_val = as.Date("01-01-2020", format = "%d-%m-%Y"), 
               end_val = as.Date("31-08-2020", format = "%d-%m-%Y"))

cases.ns = cases[cases$prname == "Nova Scotia",]
cases.ns = pad(cases.ns, start_val = as.Date("01-01-2020", format = "%d-%m-%Y"), 
               end_val = as.Date("31-08-2020", format = "%d-%m-%Y"))

cases.pe = cases[cases$prname == "Prince Edward Island",]
cases.pe = pad(cases.pe, start_val = as.Date("01-01-2020", format = "%d-%m-%Y"), 
               end_val = as.Date("31-08-2020", format = "%d-%m-%Y"))

cases.nt = cases[cases$prname == "Northwest Territories",]
cases.nt = pad(cases.nt, start_val = as.Date("01-01-2020", format = "%d-%m-%Y"), 
               end_val = as.Date("31-08-2020", format = "%d-%m-%Y"))

# set values for before 2020-01-31 to zero because there were no reported cases then
cases.full$numtoday[cases.full$date < as.Date("2020-01-31", format = "%Y-%m-%d")] = 0
cases.full$numconf[cases.full$date < as.Date("2020-01-31", format = "%Y-%m-%d")] = 0

# impute missing values in <numtoday> column in <cases.full> 
# round imputed values to make them represent case counts
cases.full.imp = cases.full
cases.full.imp$numtoday = round(na_interpolation(cases.full$numtoday, option = "linear"))



# Note: Because data sources are independent, impute them separately.


# add week numbers
cases.full.imp$week = as.numeric(as.character(strftime(cases.full.imp$date, format = "%V")))


# add Monday indicator
cases.full.imp$monday = ifelse(weekdays(cases.full.imp$date, abbreviate = T) == "Mon", 1, 0)


# add holiday indicator (Canada-wide holidays, because the reporting system used is federal)
holidays = as.Date(c("2020-01-01", "2020-04-10", "2020-05-18", "2020-06-24", 
                     "2020-07-01", "2020-08-03", "2020-09-07", "2020-10-12",
                     "2020-11-11", "2020-12-25", "2020-12-26"), format = "%Y-%m-%d")
holiday = ifelse(cases.full.imp$date %in% holidays, 1, 0)

cases.full.imp$holiday = holiday



### Import data for stock market ###
stocks.tsx = read.csv("tsx_stocks.csv")
stocks.nasdaq = read.csv("^IXIC.csv")
stocks.sp500 = read.csv("^GSPC.csv")

# convert NASDAQ and S&P500 to CAD 
stocks.nasdaq[,2:4] = stocks.nasdaq[,2:4]*1.3184
stocks.sp500[,2:4] = stocks.sp500[,2:4]*1.3184

# convert date column to Date object
stocks.tsx$Date = as.Date(stocks.tsx$Date, format = "%Y-%m-%d")
stocks.nasdaq$Date = as.Date(stocks.nasdaq$Date, format = "%Y-%m-%d")
stocks.sp500$Date = as.Date(stocks.sp500$Date, format = "%Y-%m-%d")

# select relevant time period
stocks.tsx = stocks.tsx[(as.Date(stocks.tsx$Date, format = "%Y-%m-%d") <= "2020-08-31") &
                          (as.Date(stocks.tsx$Date, format = "%Y-%m-%d") >= "2020-01-01"),]
stocks.nasdaq = stocks.nasdaq[(as.Date(stocks.nasdaq$Date, format = "%Y-%m-%d") <= "2020-08-31") &
                                (as.Date(stocks.nasdaq$Date, format = "%Y-%m-%d") >= "2020-01-01"),]
stocks.sp500 = stocks.sp500[(as.Date(stocks.sp500$Date, format = "%Y-%m-%d") <= "2020-08-31") &
                              (as.Date(stocks.sp500$Date, format = "%Y-%m-%d") >= "2020-01-01"),]

# add rows for missing dates
stocks.tsx.full = pad(stocks.tsx, start_val = as.Date("2020-01-01", format = "%Y-%m-%d"), 
                      end_val = as.Date("2020-08-31", format = "%Y-%m-%d"))
stocks.nasdaq.full = pad(stocks.nasdaq, start_val = as.Date("2020-01-01", format = "%Y-%m-%d"), 
                         end_val = as.Date("2020-08-31", format = "%Y-%m-%d"))
stocks.sp500.full = pad(stocks.sp500, start_val = as.Date("2020-01-01", format = "%Y-%m-%d"), 
                        end_val = as.Date("2020-08-31", format = "%Y-%m-%d"))

# impute missing values
stocks.tsx.full.imp = stocks.tsx.full
stocks.nasdaq.full.imp = stocks.nasdaq.full
stocks.sp500.full.imp = stocks.sp500.full
stocks.tsx.full.imp[,2:ncol(stocks.tsx.full.imp)] = na_interpolation(stocks.tsx.full[,2:ncol(stocks.tsx.full)], method = "linear")
stocks.nasdaq.full.imp[,2:ncol(stocks.nasdaq.full.imp)] = na_interpolation(stocks.nasdaq.full[,2:ncol(stocks.nasdaq.full)], method = "linear")
stocks.sp500.full.imp[,2:ncol(stocks.sp500.full.imp)] = na_interpolation(stocks.sp500.full[,2:ncol(stocks.sp500.full)], method = "linear")



# combine different stocks into one dataset
stocks = data.frame("Date" = stocks.tsx$Date, "Close_TSX" = stocks.tsx$Close, 
               "Close_NASDAQ" = stocks.nasdaq$Close, "Close_SP500" = stocks.sp500$Close,
               "Volume_TSX" = stocks.tsx$Volume, "Volume_NASDAQ" = stocks.nasdaq$Volume,
               "Volume_SP500" = stocks.sp500$Volume)
stocks.full = data.frame("Date" = stocks.tsx.full$Date, "Close_TSX" = stocks.tsx.full$Close, 
                         "Close_NASDAQ" = stocks.nasdaq.full$Close, "Close_SP500" = stocks.sp500.full$Close, 
                         "Volume_TSX" = stocks.tsx.full$Volume, "Volume_NASDAQ" = stocks.nasdaq.full$Volume,
                         "Volume_SP500" = stocks.sp500.full$Volume)
stocks.full.imp = data.frame("Date" = stocks.tsx.full.imp$Date, "Close_TSX" = stocks.tsx.full.imp$Close, 
                             "Close_NASDAQ" = stocks.nasdaq.full.imp$Close, "Close_SP500" = stocks.sp500.full.imp$Close,
                             "Volume_TSX" = stocks.tsx.full.imp$Volume, "Volume_NASDAQ" = stocks.nasdaq.full.imp$Volume,
                             "Volume_SP500" = stocks.sp500.full.imp$Volume)







### Import data for flight patterns ###

# import flight pattern datasets
flights.1 = read.csv('flightlist_20190101_20190131.csv')
flights.2 = read.csv('flightlist_20190201_20190228.csv')
flights.3 = read.csv('flightlist_20190301_20190331.csv')
flights.4 = read.csv('flightlist_20190401_20190430.csv')
flights.5 = read.csv('flightlist_20190501_20190531.csv')
flights.6 = read.csv('flightlist_20190601_20190630.csv')
flights.7 = read.csv('flightlist_20190701_20190731.csv')
flights.8 = read.csv('flightlist_20190801_20190831.csv')
flights.9 = read.csv('flightlist_20190901_20190930.csv')
flights.10 = read.csv('flightlist_20191001_20191031.csv')
flights.11 = read.csv('flightlist_20191101_20191130.csv')
flights.12 = read.csv('flightlist_20191201_20191231.csv')
flights.13 = read.csv('flightlist_20200101_20200131.csv')
flights.14 = read.csv('flightlist_20200201_20200229.csv')
flights.15 = read.csv('flightlist_20200301_20200331.csv')
flights.16 = read.csv('flightlist_20200401_20200430.csv')
flights.17 = read.csv('flightlist_20200501_20200531.csv')
flights.18 = read.csv('flightlist_20200601_20200630.csv')
flights.19 = read.csv('flightlist_20200701_20200731.csv') 
flights.20 = read.csv('flightlist_20200801_20200831.csv')

# destinations from Canada (and some other countries) have codes that start with "C" 
# narrow down and merge the datasets with codes starting with "C"
flights = rbind(flights.1[str_starts(flights.1$destination, "C"),],
                flights.2[str_starts(flights.2$destination, "C"),],
                flights.3[str_starts(flights.3$destination, "C"),],
                flights.4[str_starts(flights.4$destination, "C"),],
                flights.5[str_starts(flights.5$destination, "C"),],
                flights.6[str_starts(flights.6$destination, "C"),],
                flights.7[str_starts(flights.7$destination, "C"),],
                flights.8[str_starts(flights.8$destination, "C"),],
                flights.9[str_starts(flights.9$destination, "C"),],
                flights.10[str_starts(flights.10$destination, "C"),],
                flights.11[str_starts(flights.11$destination, "C"),],
                flights.12[str_starts(flights.12$destination, "C"),],
                flights.13[str_starts(flights.13$destination, "C"),],
                flights.14[str_starts(flights.14$destination, "C"),],
                flights.15[str_starts(flights.15$destination, "C"),],
                flights.16[str_starts(flights.16$destination, "C"),],
                flights.17[str_starts(flights.17$destination, "C"),],
                flights.18[str_starts(flights.18$destination, "C"),],
                flights.19[str_starts(flights.19$destination, "C"),],
                flights.20[str_starts(flights.20$destination, "C"),]) #~45 min for everything


# create datasets from SkyVector airport listings 
# each dataset contains all airport codes for each province
ab.codes = read.csv("ab_airports.txt", header = FALSE)
ab.codes = data.frame(substr(ab.codes[,1], 1, 4))
ab.codes = cbind(ab.codes, rep("AB", nrow(ab.codes)))
colnames(ab.codes) = c("code", "province")
write.csv(ab.codes, "ab_airports_ed.csv", row.names = FALSE)
ab.codes = read.csv("ab_airports_ed.csv")

bc.codes = read.csv("bc_airports.txt", header = FALSE)
bc.codes = data.frame(substr(bc.codes[,1], 1, 4))
bc.codes = cbind(bc.codes, rep("BC", nrow(bc.codes)))
colnames(bc.codes) = c("code", "province")
write.csv(bc.codes, "bc_airports_ed.csv", row.names = FALSE)
bc.codes = read.csv("bc_airports_ed.csv")

mb.codes = read.csv("mb_airports.txt")
mb.codes = data.frame(substr(mb.codes[,1], 1, 4))
mb.codes = cbind(mb.codes, rep("MB", nrow(mb.codes)))
colnames(mb.codes) = c("code", "province")
write.csv(mb.codes, "mb_airports_ed.csv", row.names = FALSE)
mb.codes = read.csv("mb_airports_ed.csv")

nb.codes = read.csv("nb_airports.txt")
nb.codes = data.frame(substr(nb.codes[,1], 1, 4))
nb.codes = cbind(nb.codes, rep("NB", nrow(nb.codes)))
colnames(nb.codes) = c("code", "province")
write.csv(nb.codes, "nb_airports_ed.csv", row.names = FALSE)
nb.codes = read.csv("nb_airports_ed.csv")

nl.codes = read.csv("nl_airports.txt")
nl.codes = data.frame(substr(nl.codes[,1], 1, 4))
nl.codes = cbind(nl.codes, rep("NL", nrow(nl.codes)))
colnames(nl.codes) = c("code", "province")
write.csv(nl.codes, "nl_airports_ed.csv", row.names = FALSE)
nl.codes = read.csv("nl_airports_ed.csv")

nt.codes = read.csv("nt_airports.txt")
nt.codes = data.frame(substr(nt.codes[,1], 1, 4))
nt.codes = cbind(nt.codes, rep("NT", nrow(nt.codes)))
colnames(nt.codes) = c("code", "province")
write.csv(nt.codes, "nt_airports_ed.csv", row.names = FALSE)
nt.codes = read.csv("nt_airports_ed.csv")

ns.codes = read.csv("ns_airports.txt")
ns.codes = data.frame(substr(ns.codes[,1], 1, 4))
ns.codes = cbind(ns.codes, rep("NS", nrow(ns.codes)))
colnames(ns.codes) = c("code", "province")
write.csv(ns.codes, "ns_airports_ed.csv", row.names = FALSE)
ns.codes = read.csv("ns_airports_ed.csv")

nu.codes = read.csv("nu_airports.txt")
nu.codes = data.frame(substr(nu.codes[,1], 1, 4))
nu.codes = cbind(nu.codes, rep("NU", nrow(nu.codes)))
colnames(nu.codes) = c("code", "province")
write.csv(nu.codes, "nu_airports_ed.csv", row.names = FALSE)
nu.codes = read.csv("nu_airports_ed.csv")

on.codes = read.csv("on_airports.txt")
on.codes = data.frame(substr(on.codes[,1], 1, 4))
on.codes = cbind(on.codes, rep("ON", nrow(on.codes)))
colnames(on.codes) = c("code", "province")
write.csv(on.codes, "on_airports_ed.csv", row.names = FALSE)
on.codes = read.csv("on_airports_ed.csv")

pe.codes = read.csv("pe_airports.txt")
pe.codes = data.frame(substr(pe.codes[,1], 1, 4))
pe.codes = cbind(pe.codes, rep("PE", nrow(pe.codes)))
colnames(pe.codes) = c("code", "province")
write.csv(pe.codes, "pe_airports_ed.csv", row.names = FALSE)
pe.codes = read.csv("pe_airports_ed.csv")

qc.codes = read.csv("qc_airports.txt")
qc.codes = data.frame(substr(qc.codes[,1], 1, 4))
qc.codes = cbind(qc.codes, rep("QC", nrow(qc.codes)))
colnames(qc.codes) = c("code", "province")
write.csv(qc.codes, "qc_airports_ed.csv", row.names = FALSE)
qc.codes = read.csv("qc_airports_ed.csv")

sk.codes = read.csv("sk_airports.txt")
sk.codes = data.frame(substr(sk.codes[,1], 1, 4))
sk.codes = cbind(sk.codes, rep("SK", nrow(sk.codes)))
colnames(sk.codes) = c("code", "province")
write.csv(sk.codes, "sk_airports_ed.csv", row.names = FALSE)
sk.codes = read.csv("sk_airports_ed.csv")

yt.codes = read.csv("yt_airports.txt")
yt.codes = data.frame(substr(yt.codes[,1], 1, 4))
yt.codes = cbind(yt.codes, rep("YT", nrow(yt.codes)))
colnames(yt.codes) = c("code", "province")
write.csv(yt.codes, "yt_airports_ed.csv", row.names = FALSE)
yt.codes = read.csv("yt_airports_ed.csv")

# merge provincial airport codes into canadian airport codes
ca.codes = rbind(ab.codes, bc.codes, mb.codes, nb.codes, nl.codes, nt.codes, ns.codes,
                 nu.codes, on.codes, pe.codes, qc.codes, sk.codes, yt.codes)

# select flight pattern subsets with destinations in Canadian provinces
flights.ab = flights[flights$destination %in% ab.codes$code,]
flights.bc = flights[flights$destination %in% bc.codes$code,]
flights.mb = flights[flights$destination %in% mb.codes$code,]
flights.nb = flights[flights$destination %in% nb.codes$code,]
flights.nl = flights[flights$destination %in% nl.codes$code,]
flights.nt = flights[flights$destination %in% nt.codes$code,]
flights.ns = flights[flights$destination %in% ns.codes$code,]
flights.nu = flights[flights$destination %in% nu.codes$code,]
flights.on = flights[flights$destination %in% on.codes$code,]
flights.pe = flights[flights$destination %in% pe.codes$code,]
flights.qc = flights[flights$destination %in% qc.codes$code,]
flights.sk = flights[flights$destination %in% sk.codes$code,]
flights.yt = flights[flights$destination %in% yt.codes$code,]

# merge provincial subsets into national ones
flights.ca = flights[flights$destination %in% ca.codes$code,]

# remove time stamp from each day
flights.ca$day = substr(flights.ca$day, 1, 10)

# make reduced dataset including only 01-Jan-2020 to 31-Aug-2020
flights.ca.reduced = flights.ca[flights.ca$day >= as.Date("2020-01-01"),]

# assign province to each airport code
# get (row) index of each destination airport code as found the <ca.codes> dataset
province.index = match(flights.ca.reduced$destination, ca.codes$code)

# get destination province corresponding to each index above
destination_province = ca.codes$province[province.index]

# add destination province as column to <flights.ca>
flights.ca.reduced$destination_province = destination_province

# save as smaller csv for uploading elsewhere
write.csv(flights.ca, "flightlist_ca.csv")
write.csv(flights.ca.reduced, "flightlist_ca_reduced.csv")

#flights.ca.reduced = read.csv("flightlist_ca_reduced.csv")

# function to count number of flights coming to Canada per day in the study's time period
total_flights_canada = function(date_str) {
  no_flights = sum(flights.ca.reduced$day == date_str)
  return(no_flights)
}

# this produces a vector with the number of flights arriving in Canada per day,
# in chronological order
flights.per.day = data.frame(num_flights = sapply(unique(flights.ca.reduced$day), total_flights_canada))
flights.per.day = cbind(day = rownames(flights.per.day), flights.per.day)
rownames(flights.per.day) = NULL

# write.csv(flights.per.day, "flights.per.day.csv")

# function to count number of flights coming to a province/territory per day
total_flights_prov_ter = function(date_str, prov) {
  no_flights = sum(flights.ca.reduced[flights.ca.reduced$destination_province == prov,]$day == date_str)
  return(no_flights)
}

# make a data frame to store flights landing per day per province
flights.per.day.per.prov = data.frame(day = unique(flights.ca.reduced$day))

for (location in unique(ca.codes$province)) {
  # make a vector with number of flights per day for <location>
  flights.per.day.per.prov[,paste0("no_flights_", location)] = 
    sapply(unique(flights.ca.reduced$day), total_flights_prov_ter, prov = location)
}

# write.csv(flights.per.day.per.prov, "flights.per.day.per.prov.csv")

# function to count number of international flights landing in Canada per day
total_intl_flights_canada = function(date_str) {
  # count the flights with origin being an airport code outside of Canada (i.e. not listed in <ca.codes>)
  no_flights = sum(flights.ca.reduced[!(flights.ca.reduced$origin %in% ca.codes$code),]$day == date_str)
  return(no_flights)
}

# count number of international flights landing in Canada each day
intl.flights.per.day = data.frame(day = unique(flights.ca.reduced$day), 
                                  num_flights = sapply(unique(flights.ca.reduced$day), total_intl_flights_canada))
rownames(intl.flights.per.day) = NULL

#write.csv(intl.flights.per.day, "intl.flights.per.day.csv")

# function to count number of domestic flights landing in Canada per day
total_dom_flights_canada = function(date_str) {
  # count the flights with origin being an airport code within Canada (i.e. listed in <ca.codes>)
  no_flights = sum(flights.ca.reduced[flights.ca.reduced$origin %in% ca.codes$code,]$day == date_str)
  return(no_flights)
}

# count number of domestic flights in Canada each day
dom.flights.per.day = data.frame(day = unique(flights.ca.reduced$day), 
                                 num_flights = sapply(unique(flights.ca.reduced$day), total_dom_flights_canada))
rownames(dom.flights.per.day) = NULL

#write.csv(dom.flights.per.day, "dom.flights.per.day.csv")


# function to count international flights per province per day
total_intl_flights_prov_ter = function(date_str, prov) {
  no_flights = sum(flights.ca.reduced[(flights.ca.reduced$destination_province == prov) & 
                                        !(flights.ca.reduced$origin %in% ca.codes$code),]$day == date_str)
  return(no_flights)
}

# count int'l flights per province per day
intl.flights.per.day.per.prov = data.frame(day = unique(flights.ca.reduced$day))

for (location in unique(ca.codes$province)) {
  # make a vector with number of flights per day for <location>
  intl.flights.per.day.per.prov[,paste0("no_flights_", location)] = 
    sapply(unique(flights.ca.reduced$day), total_intl_flights_prov_ter, prov = location)
}

#write.csv(intl.flights.per.day.per.prov, "intl.flights.per.day.per.prov.csv") #next

# function to count domestic flights per province per day
total_dom_flights_prov_ter = function(date_str, prov) {
  no_flights = sum(flights.ca.reduced[(flights.ca.reduced$destination_province == prov) & 
                                        flights.ca.reduced$origin %in% ca.codes$code,]$day == date_str)
  return(no_flights)
}

# count domestic flights per province per day
dom.flights.per.day.per.prov = data.frame(day = unique(flights.ca.reduced$day))

for (location in unique(ca.codes$province)) {
  # make a vector with number of flights per day for <location>
  dom.flights.per.day.per.prov[,paste0("no_flights_", location)] = 
    sapply(unique(flights.ca.reduced$day), total_dom_flights_prov_ter, prov = location)
}

#write.csv(dom.flights.per.day.per.prov, "dom.flights.per.day.per.prov.csv") 


# make data frame storing number of intl and dom flights landing in Canada per day
flights.per.day = data.frame("date" = dom.flights.per.day$day, 
                             "num_flights" = intl.flights.per.day$num_flights + dom.flights.per.day$num_flights)

#write.csv(flights.per.day, "flights.per.day.csv")







### Import data for weather ###
# import data for all cities
weather.nl.1 = read.csv("climate-daily-stjohns.csv")
weather.nl.2 = read.csv("climate-daily-deerlake.csv")

weather.pe.1 = read.csv("climate-daily-charlottetown.csv")
weather.pe.2 = read.csv("climate-daily-summerside.csv")

weather.ns.1 = read.csv("climate-daily-halifax.csv")
weather.ns.2 = read.csv("climate-daily-capebreton.csv")

weather.nb.1 = read.csv("climate-daily-moncton.csv")
weather.nb.2 = read.csv("climate-daily-saintjohn.csv")

weather.qc.1 = read.csv("climate-daily-montreal.csv")
weather.qc.2 = read.csv("climate-daily-quebec.csv")

weather.on.1 = read.csv("climate-daily-toronto.csv")
weather.on.2 = read.csv("climate-daily-ottawa.csv")

weather.mb.1 = read.csv("climate-daily-winnipeg.csv")
weather.mb.2 = read.csv("climate-daily-brandon.csv")
weather.mb.2$CLIMATE_IDENTIFIER = as.factor(weather.mb.2$CLIMATE_IDENTIFIER) 

weather.sk.1 = read.csv("climate-daily-saskatoon.csv")
weather.sk.2 = read.csv("climate-daily-regina.csv")

weather.ab.1 = read.csv("climate-daily-calgary.csv")
weather.ab.2 = read.csv("climate-daily-edmonton.csv")

weather.bc.1 = read.csv("climate-daily-vancouver.csv")
weather.bc.2 = read.csv("climate-daily-abbotsford.csv")

weather.yt.1 = read.csv("climate-daily-whitehorse.csv")
weather.yt.2 = read.csv("climate-daily-dawson.csv")

weather.nt.1 = read.csv("climate-daily-yellowknife.csv")
weather.nt.2 = read.csv("climate-daily-hayriver.csv")

weather.nu.1 = read.csv("climate-daily-iqaluit.csv")
weather.nu.2 = read.csv("climate-daily-rankin.csv")

# the columns with "FLAG" in the name are empty vectors
# each city weather dataset has the same column names so using the names from the 
# first city's dataset will suffice for all cities 
cols.remove = grep("FLAG", c(colnames(weather.nl.1)), value = TRUE)

# remove empty columns in all cities
weather.nl.1[,cols.remove] = NULL
weather.nl.2[,cols.remove] = NULL
weather.pe.1[,cols.remove] = NULL
weather.pe.2[,cols.remove] = NULL
weather.ns.1[,cols.remove] = NULL
weather.ns.2[,cols.remove] = NULL
weather.nb.1[,cols.remove] = NULL
weather.nb.2[,cols.remove] = NULL
weather.qc.1[,cols.remove] = NULL
weather.qc.2[,cols.remove] = NULL
weather.on.1[,cols.remove] = NULL
weather.on.2[,cols.remove] = NULL
weather.mb.1[,cols.remove] = NULL
weather.mb.2[,cols.remove] = NULL
weather.sk.1[,cols.remove] = NULL
weather.sk.2[,cols.remove] = NULL
weather.ab.1[,cols.remove] = NULL
weather.ab.2[,cols.remove] = NULL
weather.bc.1[,cols.remove] = NULL
weather.bc.2[,cols.remove] = NULL
weather.yt.1[,cols.remove] = NULL
weather.yt.2[,cols.remove] = NULL
weather.nt.1[,cols.remove] = NULL
weather.nt.2[,cols.remove] = NULL
weather.nu.1[,cols.remove] = NULL
weather.nu.2[,cols.remove] = NULL


# shorten date format
weather.nl.1$LOCAL_DATE = substr(weather.nl.1$LOCAL_DATE, 1, 10)
weather.nl.2$LOCAL_DATE = substr(weather.nl.2$LOCAL_DATE, 1, 10)

weather.pe.1$LOCAL_DATE = substr(weather.pe.1$LOCAL_DATE, 1, 10)
weather.pe.2$LOCAL_DATE = substr(weather.pe.2$LOCAL_DATE, 1, 10)

weather.ns.1$LOCAL_DATE = substr(weather.ns.1$LOCAL_DATE, 1, 10)
weather.ns.2$LOCAL_DATE = substr(weather.ns.2$LOCAL_DATE, 1, 10)

weather.nb.1$LOCAL_DATE = substr(weather.nb.1$LOCAL_DATE, 1, 10)
weather.nb.2$LOCAL_DATE = substr(weather.nb.2$LOCAL_DATE, 1, 10)

weather.qc.1$LOCAL_DATE = substr(weather.qc.1$LOCAL_DATE, 1, 10)
weather.qc.2$LOCAL_DATE = substr(weather.qc.2$LOCAL_DATE, 1, 10)

weather.on.1$LOCAL_DATE = substr(weather.on.1$LOCAL_DATE, 1, 10)
weather.on.2$LOCAL_DATE = substr(weather.on.2$LOCAL_DATE, 1, 10)

weather.mb.1$LOCAL_DATE = substr(weather.mb.1$LOCAL_DATE, 1, 10)
weather.mb.2$LOCAL_DATE = substr(weather.mb.2$LOCAL_DATE, 1, 10)

weather.sk.1$LOCAL_DATE = substr(weather.sk.1$LOCAL_DATE, 1, 10)
weather.sk.2$LOCAL_DATE = substr(weather.sk.2$LOCAL_DATE, 1, 10)

weather.ab.1$LOCAL_DATE = substr(weather.ab.1$LOCAL_DATE, 1, 10)
weather.ab.2$LOCAL_DATE = substr(weather.ab.2$LOCAL_DATE, 1, 10)

weather.bc.1$LOCAL_DATE = substr(weather.bc.1$LOCAL_DATE, 1, 10)
weather.bc.2$LOCAL_DATE = substr(weather.bc.2$LOCAL_DATE, 1, 10)

weather.yt.1$LOCAL_DATE = substr(weather.yt.1$LOCAL_DATE, 1, 10)
weather.yt.2$LOCAL_DATE = substr(weather.yt.2$LOCAL_DATE, 1, 10)

weather.nt.1$LOCAL_DATE = substr(weather.nt.1$LOCAL_DATE, 1, 10)
weather.nt.2$LOCAL_DATE = substr(weather.nt.2$LOCAL_DATE, 1, 10)

weather.nu.1$LOCAL_DATE = substr(weather.nu.1$LOCAL_DATE, 1, 10)
weather.nu.2$LOCAL_DATE = substr(weather.nu.2$LOCAL_DATE, 1, 10)


# include observations up to and including 31 Aug 2020
weather.nl.1 = weather.nl.1[weather.nl.1$LOCAL_DATE <= as.Date("2020-08-31"),]
weather.nl.2 = weather.nl.2[weather.nl.2$LOCAL_DATE <= as.Date("2020-08-31"),]

weather.pe.1 = weather.pe.1[weather.pe.1$LOCAL_DATE <= as.Date("2020-08-31"),]
weather.pe.2 = weather.pe.2[weather.pe.2$LOCAL_DATE <= as.Date("2020-08-31"),]

weather.ns.1 = weather.ns.1[weather.ns.1$LOCAL_DATE <= as.Date("2020-08-31"),]
weather.ns.2 = weather.ns.2[weather.ns.2$LOCAL_DATE <= as.Date("2020-08-31"),]

weather.nb.1 = weather.nb.1[weather.nb.1$LOCAL_DATE <= as.Date("2020-08-31"),]
weather.nb.2 = weather.nb.2[weather.nb.2$LOCAL_DATE <= as.Date("2020-08-31"),]

weather.qc.1 = weather.qc.1[weather.qc.1$LOCAL_DATE <= as.Date("2020-08-31"),]
weather.qc.2 = weather.qc.2[weather.qc.2$LOCAL_DATE <= as.Date("2020-08-31"),]

weather.on.1 = weather.on.1[weather.on.1$LOCAL_DATE <= as.Date("2020-08-31"),]
weather.on.2 = weather.on.2[weather.on.2$LOCAL_DATE <= as.Date("2020-08-31"),]

weather.mb.1 = weather.mb.1[weather.mb.1$LOCAL_DATE <= as.Date("2020-08-31"),]
weather.mb.2 = weather.mb.2[weather.mb.2$LOCAL_DATE <= as.Date("2020-08-31"),]

weather.sk.1 = weather.sk.1[weather.sk.1$LOCAL_DATE <= as.Date("2020-08-31"),]
weather.sk.2 = weather.sk.2[weather.sk.2$LOCAL_DATE <= as.Date("2020-08-31"),]

weather.ab.1 = weather.ab.1[weather.ab.1$LOCAL_DATE <= as.Date("2020-08-31"),]
weather.ab.2 = weather.ab.2[weather.ab.2$LOCAL_DATE <= as.Date("2020-08-31"),]

weather.bc.1 = weather.bc.1[weather.bc.1$LOCAL_DATE <= as.Date("2020-08-31"),]
weather.bc.2 = weather.bc.2[weather.bc.2$LOCAL_DATE <= as.Date("2020-08-31"),]

weather.yt.1 = weather.yt.1[weather.yt.1$LOCAL_DATE <= as.Date("2020-08-31"),]
weather.yt.2 = weather.yt.2[weather.yt.2$LOCAL_DATE <= as.Date("2020-08-31"),]

weather.nt.1 = weather.nt.1[weather.nt.1$LOCAL_DATE <= as.Date("2020-08-31"),]
weather.nt.2 = weather.nt.2[weather.nt.2$LOCAL_DATE <= as.Date("2020-08-31"),]

weather.nu.1 = weather.nu.1[weather.nu.1$LOCAL_DATE <= as.Date("2020-08-31"),]
weather.nu.2 = weather.nu.2[weather.nu.2$LOCAL_DATE <= as.Date("2020-08-31"),]


weather.cities = list(weather.pe.1, weather.pe.2, weather.nt.1, weather.nt.2, weather.nl.1, weather.nl.2, 
                      weather.nb.1, weather.nb.2, weather.mb.1, weather.mb.2, weather.yt.1, weather.yt.2,
                      weather.sk.1, weather.sk.2, weather.qc.1, weather.qc.2, weather.on.1, weather.on.2, 
                      weather.nu.1, weather.nu.2, weather.ns.1, weather.ns.2, weather.bc.1, weather.bc.2,
                      weather.ab.1, weather.ab.2)

names(weather.cities) = c("Charlottetown", "Summerside", "Yellowknife", "Hay River", " St. John's",
                          "Deer Lake", "Moncton", "Saint John", "Winnipeg", "Brandon", "Whitehorse",
                          "Dawson", "Saskatoon", "Regina", "Montreal", "Quebec", "Toronto", "Ottawa",
                          "Iqaluit", "Rankin", "Halifax", "Cape Breton", "Vancouver", "Abbotsford", 
                          "Calgary", "Edmonton")

# average data from the cities in each province to make provincial data
weather.nl = data.frame(LOCAL_DATE = weather.nl.1$LOCAL_DATE)
weather.pe = data.frame(LOCAL_DATE = weather.pe.1$LOCAL_DATE)
weather.nt = data.frame(LOCAL_DATE = weather.nt.1$LOCAL_DATE)
weather.nl = data.frame(LOCAL_DATE = weather.nl.1$LOCAL_DATE)
weather.nb = data.frame(LOCAL_DATE = weather.nb.1$LOCAL_DATE)
weather.mb = data.frame(LOCAL_DATE = weather.mb.1$LOCAL_DATE)
weather.yt = data.frame(LOCAL_DATE = weather.yt.1$LOCAL_DATE)
weather.sk = data.frame(LOCAL_DATE = weather.sk.1$LOCAL_DATE)
weather.qc = data.frame(LOCAL_DATE = weather.qc.1$LOCAL_DATE)
weather.on = data.frame(LOCAL_DATE = weather.on.1$LOCAL_DATE)
weather.nu = data.frame(LOCAL_DATE = weather.nu.1$LOCAL_DATE)
weather.ns = data.frame(LOCAL_DATE = weather.ns.1$LOCAL_DATE)
weather.bc = data.frame(LOCAL_DATE = weather.bc.1$LOCAL_DATE)
weather.ab = data.frame(LOCAL_DATE = weather.ab.1$LOCAL_DATE)


weather.provinces = list(weather.pe, weather.nt, weather.nl, weather.nb, weather.mb, weather.yt, 
                         weather.sk, weather.qc, weather.on, weather.nu, weather.ns, weather.bc,
                         weather.ab)


# compute provincial averages of mean temp, max humidity and min humidity per day 
j = 1
for (i in seq(1, length(weather.cities), by = 2)) {
  weather.provinces[[j]]$AVG_MEAN_TEMPERATURE = rowMeans(data.frame(weather.cities[[i]]$MEAN_TEMPERATURE, 
                                                                    weather.cities[[i+1]]$MEAN_TEMPERATURE))
  weather.provinces[[j]]$AVG_MIN_REL_HUMIDITY = rowMeans(data.frame(weather.cities[[i]]$MIN_REL_HUMIDITY, 
                                                                    weather.cities[[i+1]]$MIN_REL_HUMIDITY))
  weather.provinces[[j]]$AVG_MAX_REL_HUMIDITY = rowMeans(data.frame(weather.cities[[i]]$MAX_REL_HUMIDITY, 
                                                                    weather.cities[[i+1]]$MAX_REL_HUMIDITY))
  j = j + 1
}


names(weather.provinces) = c("PE", "NT", "NL", "NB", "MB", "YT", "SK", "QC", "ON", "NU", "NS", 
                             "BC", "AB")


# change date column in weather data to <Date> variable
set_date_object = function(df) {
  df$LOCAL_DATE = as.Date(df$LOCAL_DATE, format = "%Y-%m-%d")
  return(df)
}

weather.provinces = lapply(weather.provinces, set_date_object)


# pad with missing values in case some days are missing
weather.provinces = lapply(weather.provinces, pad, start_val = as.Date("2020-01-01", format = "%Y-%m-%d"), 
                           end_val = as.Date("2020-08-31", format = "%Y-%m-%d"))

# impute data in all columns with missing variables
weather.provinces = lapply(weather.provinces, na_interpolation, method = "linear")

# create mean weather values for the country
weather.canada = data.frame(weather.provinces)

# remove extra date columns
weather.canada[,grep("LOCAL_DATE", colnames(weather.canada))[2:length(grep("LOCAL_DATE", colnames(weather.canada)))]] = NULL
colnames(weather.canada)[1] = "LOCAL_DATE" 

# generate means for humidity and temperature across the country
weather.canada$CAN.AVG_MEAN_TEMPERATURE = rowMeans(weather.canada[,colnames(weather.canada)[grep("AVG_MEAN_TEMPERATURE", colnames(weather.canada))]])
weather.canada$CAN.AVG_MIN_REL_HUMIDITY = rowMeans(weather.canada[,colnames(weather.canada)[grep("AVG_MIN_REL_HUMIDITY", colnames(weather.canada))]])
weather.canada$CAN.AVG_MAX_REL_HUMIDITY = rowMeans(weather.canada[,colnames(weather.canada)[grep("AVG_MAX_REL_HUMIDITY", colnames(weather.canada))]])

#write.csv(weather.canada, "weather.canada.csv")





### Import data for mobility patterns ###
### Import data from Google mobility reports ###
mobility.g = read.csv("2020_CA_Region_Mobility_Report.csv")

# remove empty columns
mobility.g$metro_area = NULL
mobility.g$census_fips_code = NULL


# make another data frame for aggregated values, with empty values to be filled in the columns
mobility.g.aggregate = unique(mobility.g[,c("country_region", "date")])
mobility.g.aggregate = cbind(mobility.g.aggregate, 
                             "retail_and_recreation_percent_change_from_baseline" = NA, 
                             "grocery_and_pharmacy_percent_change_from_baseline" = NA,
                             "parks_percent_change_from_baseline" = NA,
                             "transit_stations_percent_change_from_baseline" = NA,
                             "workplaces_percent_change_from_baseline" = NA,
                             "residential_percent_change_from_baseline" = NA)


# add values at each date
for (x in mobility.g$date) {
  # extract all rows with the same date (i.e. all locations that have an observation at that date)
  # the extracted data is a subframe of the original dataframe, with observations for every colname
  # colSums function produces a mean value across all locations at that date for each colname
  mobility.g.aggregate[mobility.g.aggregate$date == x, 3:ncol(mobility.g.aggregate)] = 
    colMeans(mobility.g[mobility.g$date == x, 7:ncol(mobility.g)], na.rm = T)
}

# set date column as Date object
mobility.g.aggregate$date = as.Date(mobility.g.aggregate$date, format = "%Y-%m-%d")

# set correct date time range
mobility.g.aggregate = mobility.g.aggregate[mobility.g.aggregate$date <= as.Date("2020-08-31", format = "%Y-%m-%d"),]

# add empty values for missing dates
mobility.g.aggregate.full = pad(mobility.g.aggregate, start_val = as.Date("2020-01-01", format = "%Y-%m-%d"), end_val = as.Date("2020-08-31", format = "%Y-%m-%d"))

# impute data
mobility.g.aggregate.full.imp = mobility.g.aggregate.full
mobility.g.aggregate.full.imp[,3:ncol(mobility.g.aggregate.full.imp)] = na_interpolation(mobility.g.aggregate.full[,3:ncol(mobility.g.aggregate.full)], 
                                                 option = "spline")




### Import data from Apple mobility reports ###
mobility.a = read.csv("apple_mobility_report.csv")

# extract data for Canada
mobility.a = mobility.a[mobility.a$country == "Canada",]

# set proper timeline for data points
mobility.a = mobility.a[as.Date(mobility.a$date, format = "%Y-%m-%d") >= as.Date("2020-01-01", format = "%Y-%m-%d") &
                          as.Date(mobility.a$date, format = "%Y-%m-%d") <= as.Date("2020-08-31", format = "%Y-%m-%d"),]

# drop unused levels (i.e. dates past 08-Aug-2020)
mobility.a$date = droplevels(mobility.a$date)
mobility.a$date = as.Date(mobility.a$date, format = "%Y-%m-%d")

# Note: mobility.a contains data for all of Canada (denoted by "Total") and for sub.regions.

# make a dataset with only aggregated Canadian values, with empty cells for the missing days
mobility.a.full = pad(mobility.a[mobility.a$sub.region == "Total",],
                            start_val = as.Date("2020-01-01", format = "%Y-%m-%d"), 
                            end_val = as.Date("2020-08-31", format = "%Y-%m-%d"))

# impute missing values in <driving>, <transit> and <walking> columns
mobility.a.full.imp = mobility.a.full
mobility.a.full.imp[,6:8] = na_interpolation(mobility.a.full.imp[,6:8], method = "linear")





### Import data for government policies ###
# import policies information
policies = read.csv("OxCGRT_latest.csv")

# extract information on Canada's policies
policies = policies[policies$CountryName == "Canada",]

# change date format
policies$Date = as.Date(as.factor(policies$Date), format = "%Y%m%d")

# select relevant timeline
policies = policies[policies$Date <= "2020-08-31" & policies$Date >= "2020-01-01",]

# pad the dataset in case there are missing dates
policies = pad(policies, start_val = as.Date("2020-01-01", format = "%Y-%m-%d"), 
               end_val = as.Date("2020-08-31", format = "%Y-%m-%d"))


# keep relevant columns
policies = policies[,c("Date", "StringencyIndex", "GovernmentResponseIndex", "ContainmentHealthIndex")]





# import risk of openness index 
riskopen = read.csv("riskindex_timeseries_latest.csv") 

# extract Canada's information
riskopen = riskopen[riskopen$CountryName == "Canada",]

# set date as type Date
riskopen$Date = as.Date(riskopen$Date, format = "%Y-%m-%d")

# select relevant timeline
riskopen = riskopen[as.Date(riskopen$Date, format = "%Y-%m-%d") <= "2020-08-31" &
                      as.Date(riskopen$Date, format = "%Y-%m-%d") >= "2020-01-01",]

# pad just in case there are dates with no data points
riskopen = pad(riskopen, start_val = as.Date("2020-01-01", format = "%Y-%m-%d"), 
               end_val = as.Date("2020-08-31", format = "%Y-%m-%d"))

# keep relevant columns
riskopen = riskopen[,c("Date", "community_understanding", "openness_risk")]


# impute <openness_risk> variable
riskopen.imp = riskopen
riskopen.imp$openness_risk = na_interpolation(riskopen$openness_risk, option = "linear")




### Import data for search queries ###

# import all query data csv files into a list
query.files = list.files(pattern = "multiTimeline.*.csv")
queries = lapply(query.files, read.csv, skip = 2)

# function to extract search query from the second column name in a data frame
# this function will be applied to each data frame in the <queries> list
change_second_column = function(x){
  colnames(x)[2] = substr(colnames(x)[2], 1, nchar(colnames(x)[2])-10)
  return(x)
}

# modify second column of each data frame
queries = lapply(queries, change_second_column)


# function to round <1 to 0.5 in the query tables to make them numeric
# note: the query popularity columns that contain "<1" are factors. The cols that do not are numeric vectors.
round_popularity = function(df) {
  if (is.factor(df[,2])) {
    levels(df[,2]) = c(levels(df[,2]), "0.5") # add "0.5" in case it is not an existing factor level
    df[df[,2] == "<1",2] = "0.5" # convert all <1 values to 0.5
    df[,2] = droplevels(df[,2]) # drop unused level <1
    df[,2] = as.numeric(as.character(df[,2])) # convert query vector from factor to numeric
  }
  return(df)
}

queries = lapply(queries, round_popularity)


# function to remove first column in data frame
remove_first_col = function(df){
  df[,1] = NULL
  return(df)
}

# remove first column in all but the first data frame in queries before merging them all
queries.condensed = lapply(queries[2:length(queries)], remove_first_col)
queries = c(queries[[1]], queries.condensed)

# convert matrix made by cbind into data.frame
queries = data.frame(queries)


# spaces in the query names were replaced with periods while importing
# switch the periods to underscores for better readability
colnames(queries) = str_replace_all(colnames(queries), pattern = "[.]", replacement = "_")
colnames(queries)[2:ncol(queries)] = paste0("query_", colnames(queries)[2:ncol(queries)])

# Note: there are no missing values in <queries>.





# import Bing queries
query.files.bing = list.files(pattern = "QueriesByCountry.*.tsv")
queries.bing = lapply(query.files.bing, read.csv, header = TRUE, sep = "\t", fill = TRUE)

# extract Canadian data 
queries.bing = lapply(queries.bing, subset, Country == "Canada")
queries.bing = rbind(queries.bing[[1]], queries.bing[[2]], queries.bing[[3]], queries.bing[[4]], 
                     queries.bing[[5]], queries.bing[[6]], queries.bing[[7]], queries.bing[[8]])

# count the number of unique queries per day (this reveals general popularity of covid searches)
pop_by_date = data.frame("Date" = unique(as.character(queries.bing$Date)))
pop_by_date$Date = as.character(pop_by_date$Date)
pop_by_date$popularity = NA
pop_by_date$cumul_pop = NA

# function to count number of unique covid-related queries per day (mark of general/broad popularity)
set_pop = function(date) {
  pop_by_date[pop_by_date$Date == date,]$popularity <<- length(unique(queries.bing[queries.bing$Date == date,]$Query))
}

invisible(lapply(pop_by_date$Date, set_pop))

# function to sum total popularity of covid-related queries per day
set_cumul_pop = function(date) {
  pop_by_date[pop_by_date$Date == date,]$cumul_pop <<- sum(unique(queries.bing[queries.bing$Date == date,])$PopularityScore)
}

invisible(lapply(pop_by_date$Date, set_cumul_pop))

# pad with missing dates
pop_by_date$Date = as.Date(pop_by_date$Date, format = "%Y-%m-%d")
pop_by_date = pad(pop_by_date, start_val = as.Date("2020-01-01", format = "%Y-%m-%d"), end_val = as.Date("2020-08-31", format = "%Y-%m-%d"))

# impute missing values in <popularity> and <cumul_pop> columns
pop_by_date_imp = na_interpolation(pop_by_date, method = "linear")
pop_by_date_imp[,2:3] = round(pop_by_date_imp[,2:3])






### Import data for twitter ###
twitter.stats.clean = read.table("full_dataset_clean-statistics_13Sep2020.tsv", header = TRUE, sep = "\t")
twitter.stats.full = read.table("full_dataset-statistics_13Sep2020.tsv", header = TRUE, sep = "\t")

twitter.stats.clean = twitter.stats.clean[as.Date(twitter.stats.clean$date, format = "%Y-%m-%d") 
                                          <= as.Date("2020-08-31", format = "%Y-%m-%d"),]
twitter.stats.full = twitter.stats.full[as.Date(twitter.stats.full$date, format = "%Y-%m-%d") 
                                          <= as.Date("2020-08-31", format = "%Y-%m-%d"),]


# change column names
colnames(twitter.stats.clean)[2] = "total_tweets"
colnames(twitter.stats.full)[2] = "total_tweets"

# Note: there are no missing values in <twitter.stats.clean> 
# and <twitter.stats.full>.




# import data on top terms, bigrams and trigrams per day
# find file names
term.files = list.files(pattern = "2020.*top1000terms.csv")
bigram.files = list.files(pattern = "2020.*top1000bigrams.csv")
trigram.files = list.files(pattern = "2020.*top1000trigrams.csv")

# extract date from each file name
term.files.short = substr(term.files, 1, 10)
bigram.files.short = substr(bigram.files, 1, 10)
trigram.files.short = substr(trigram.files, 1, 10)


# read files
terms = lapply(term.files, read.csv)
bigrams = lapply(bigram.files, read.csv)
trigrams = lapply(trigram.files, read.csv)

# function to add column names to each data frame in a list
# because the original "term" data files do not include column names
add_column_names = function(x) {
  colnames(x) = c("term", "count")
  return(x)
}

# change column names
terms = lapply(terms, add_column_names)
bigrams = lapply(bigrams, add_column_names)
trigrams = lapply(trigrams, add_column_names)


# add name to each data frame in the list
names(terms) = term.files



# function to save csv with data frame name as file name
save_using_name = function(framename, lst) {
   write.csv(lst[[framename]], file = framename, row.names = FALSE)
 }


# save each "terms" data frame with original file name
lapply(names(terms), save_using_name, lst = terms)



remove_extra = function(df) {
  rownames(df) = NULL
  return(df)
}

terms = lapply(terms, remove_extra) # skipped this when importing today Oct 21
  
# Note: The terms, bigrams and trigrams datasets can now be used in
# further analysis after importing.

# remove the strings that had non-letter characters added while the data was imported
# if all characters in the term are not all in the set of letters, then remove the term
# if all are true/if sum of TRUEs is equal to the nchar in the word

# make a function to remove terms that contain non-letter and non-numeric characters
remove_nonletter_terms = function(df) {
  df = df[!str_detect(str_replace_all(df$term, " ", ""),  "[^a-zA-Z0-9]"),]
  return(df)
}

# remove terms, bigrams and trigrams containing non-letter characters
terms = lapply(terms, remove_nonletter_terms)
bigrams = lapply(bigrams, remove_nonletter_terms)
trigrams = lapply(trigrams, remove_nonletter_terms)

# some counts vectors in the bigrams data frames have non-numerical input (due to 
# data duplication), making them factors and making it impossible to calculate the weights

# function to find which data frames have non-numeric entries
find_factor  = function(df) {
  return(is.factor(df$count))
}

# compute indices in lists where there are non-numeric inputs (i.e. original column names from raw
# dataset) due to data duplication, and then remove those rows, then replace the data frames
# make function to remove the part of the data frame from original column names onwards

remove_duplicates = function(df) {
  # remove rows from the original col names to the end of the data frame
  df = df[-c(which(df$count == "counts"):nrow(df)),]
  # convert the <count> columns to numeric, now that the non-numeric entry and 
  # duplicate data have been removed
  df$count = as.numeric(as.character(df$count))
  return(df)
}

bigrams[which(lapply(bigrams, find_factor) == TRUE)] = 
  lapply(bigrams[which(lapply(bigrams, find_factor) == TRUE)], remove_duplicates) # a sublist of the <bigrams> list
trigrams[which(lapply(trigrams, find_factor) == TRUE)] = 
  lapply(trigrams[which(lapply(trigrams, find_factor) == TRUE)], remove_duplicates) # a sublist of the <bigrams> list



# sentiment analysis

# make function for calculating polarity of strings (syuzhet package)
extract_sentiment = function(df) {
  # calculate sentiment value for each term in each day using syuzhet dictionary
  sent = get_sentiment(as.character(df$term))
  # produces a vector of sentiment values; one value per n-gram
  return(sent)
}


# extract polarity of strings in each daily collection 
sent_terms = lapply(terms, extract_sentiment)
sent_bigrams = lapply(bigrams, extract_sentiment) 
sent_trigrams = lapply(trigrams, extract_sentiment)

# make function for extracting emotion from each term (syuzhet)
extract_emotion = function(df) {
  # calculate emotion of every n-gram
  emot = get_nrc_sentiment(as.character(df$term))
  # for each n-gram, produces a vector containing values for each emotion  
  return(emot)
}

# extract associated emotions for each day's terms, bigrams and trigrams
emot_terms = lapply(terms, extract_emotion)
emot_bigrams = lapply(bigrams, extract_emotion)
emot_trigrams = lapply(trigrams, extract_emotion)



# terms each day are ordered by popularity (i.e. count)
# make function to generate weights for each term based on count
find_weights = function(df) {
  w = df$count/sum(df$count)
  return(w)
}

# calculate weight of each n-gram in each collection of tweets
weights_terms = lapply(terms, find_weights)
weights_bigrams = lapply(bigrams, find_weights)
weights_trigrams = lapply(trigrams, find_weights)

# make function to calculate weighted mean polarity per day using weights above
weighted_mean = function(i) {
  # return weighted mean polarity for each day's collection of 
  return(weighted.mean(sent_terms[[i]], weights_terms[[i]]))
}

# calculate weighted mean polarity for each day's collection of terms
weighted_sent_terms = lapply(1:length(terms), weighted_mean)
weighted_sent_bigrams = lapply(1:length(bigrams), weighted_mean)
weighted_sent_trigrams = lapply(1:length(trigrams), weighted_mean)

# compress the three lists above into vectors
weighted_sent_terms = unlist(weighted_sent_terms, use.names = FALSE)
weighted_sent_bigrams = unlist(weighted_sent_bigrams, use.names = FALSE)
weighted_sent_trigrams = unlist(weighted_sent_trigrams, use.names = FALSE)

weighted_sent_terms = data.frame("date" = as.Date(str_sub(term.files, 1, 10), format = "%Y-%m-%d"), 
                                 "weighted_sent" = weighted_sent_terms)
weighted_sent_bigrams = data.frame("date" = as.Date(str_sub(bigram.files, 1, 10), format = "%Y-%m-%d"), 
                                   "weighted_sent" = weighted_sent_bigrams)
weighted_sent_trigrams = data.frame("date" = as.Date(str_sub(trigram.files, 1, 10), format = "%Y-%m-%d"), 
                                    "weighted_sent" = weighted_sent_trigrams)

weighted_sent_terms = pad(weighted_sent_terms, start_val = as.Date("2020-01-01", format = "%Y-%m-%d"),
                          end_val = as.Date("2020-08-31", format = "%Y-%m-%d"))
weighted_sent_bigrams = pad(weighted_sent_bigrams, start_val = as.Date("2020-01-01", format = "%Y-%m-%d"),
                          end_val = as.Date("2020-08-31", format = "%Y-%m-%d"))
weighted_sent_trigrams = pad(weighted_sent_trigrams, start_val = as.Date("2020-01-01", format = "%Y-%m-%d"),
                          end_val = as.Date("2020-08-31", format = "%Y-%m-%d"))

# change sent vals to decimal from scientific notation 
weighted_sent_terms$weighted_sent = round(weighted_sent_terms$weighted_sent, 10) 
weighted_sent_bigrams$weighted_sent = round(weighted_sent_bigrams$weighted_sent, 10) 
weighted_sent_trigrams$weighted_sent = round(weighted_sent_trigrams$weighted_sent, 10) 

#write.csv(weighted_sent_terms, "weighted_sent_terms.csv")
#write.csv(weighted_sent_bigrams, "weighted_sent_bigrams.csv")
#write.csv(weighted_sent_trigrams, "weighted_sent_trigrams.csv")

# weighted_sent_terms = read.csv("weighted_sent_terms.csv")
# weighted_sent_bigrams = read.csv("weighted_sent_bigrams.csv")
# weighted_sent_trigrams = read.csv("weighted_sent_trigrams.csv")






### Import data for publications ###
# assign NA to empty cells
publications = read.csv("publications.csv", na.strings = c("", NA))

# remove unnecessary columns
publications[,c("Source", "Database", "Authors", "Type", "Language", "Descriptor.s.", "Fulltext.URL", 
                "Volume.number", "Issue.number", "DOI")] = NULL

# remove the invalid date entries (i.e. appearing as "0")
publications = publications[!(publications$Entry.Date == "0"),]

# change entry date from eight-digit format to standard date format
stri_sub(publications$Entry.Date, from = 5, to = 4) = "-"
stri_sub(publications$Entry.Date, from = 8, to = 7) = "-"



# save as a smaller csv file for quicker future import
# write.csv(publications, "publications_compact.csv")
publications = read.csv("publications_compact.csv")

# remove entries that have missing entries in <Abstract> or <Title> columns
publications = publications[-which(is.na(publications$Title)),]
publications = publications[-which(is.na(publications$Abstract)),]

# remove whitespace at the start and end of each abstract
publications$Abstract = str_squish(publications$Abstract)
publications$Title = str_squish(publications$Title)



# sentiment analysis using syuzhet package
# get polarity of titles and abstracts (in this package, polarity is aka sentiment)
sent_title_sy = get_sentiment(publications$Title) 
sent_abstract_sy = get_sentiment(publications$Abstract)

# compute mean polarity, aggregated by date
mean_sent_title = aggregate(sent_title_sy, by = list(publications$Entry.Date), mean)
mean_sent_abstract = aggregate(sent_abstract_sy, by = list(publications$Entry.Date), mean)


colnames(mean_sent_title) = c("Date", "mean_sent")
colnames(mean_sent_abstract) = c("Date", "mean_sent")

mean_sent_title$Date = as.Date(mean_sent_title$Date, format = "%Y-%m-%d")
mean_sent_abstract$Date = as.Date(mean_sent_abstract$Date, format = "%Y-%m-%d")


# pad
mean_sent_title = pad(mean_sent_title, start_val = as.Date("2020-01-01", format = "%Y-%m-%d"), 
                      end_val = as.Date("2020-08-31", format = "%Y-%m-%d"))
mean_sent_abstract = pad(mean_sent_abstract, start_val = as.Date("2020-01-01", format = "%Y-%m-%d"), 
                         end_val = as.Date("2020-08-31", format = "%Y-%m-%d"))

# write.csv(mean_sent_title, "mean_sent_title.csv")
# write.csv(mean_sent_abstract, "mean_sent_abstract.csv")

# mean_sent_title = read.csv("mean_sent_title.csv")
# mean_sent_abstract = read.csv("mean_sent_abstract.csv")
# 




### Import data for survey responses ###

# import data
survey.files = list.files(pattern = "yougov.*.csv")
surveys = lapply(survey.files, read.csv)

# add names to each data frame corresponding to survey question
names(surveys) = 
  str_replace(str_replace(survey.files, pattern = "yougov-chart-", replacement = ""), pattern = ".csv", replacement = "")


# make a function to edit the first column name in each data frame
edit_first_col = function(df) {
  colnames(df)[1] = stri_sub(colnames(df)[1], 4, nchar(colnames(df)[1]))
  return(df)
}

# remove unnecessary characters from first column
surveys = lapply(surveys, edit_first_col)

# make a function to remove the timestamp from date column in each data frame
remove_timestamp = function(df) {
  df$DateTime = stri_sub(df$DateTime, 1, 10)
  return(df)
}

# remove timestamp from first column
surveys = lapply(surveys, remove_timestamp)


# check if date of first data point starts later than Mar 2020
check_start = function(df) {
  if(!str_starts(df$DateTime[1], "2020-03")) {
    return(TRUE)
  } else {
    return(FALSE)
  }
}


# collect indices of survey data that start later than Mar 2020 and with less than 10 data points
indices.to.remove.1 = which(unlist(lapply(surveys, check_start)))
indices.to.remove.2 = which(lapply(surveys, nrow) < 10)
indices.to.remove = unique(c(indices.to.remove.1, indices.to.remove.2))


# remove data frames at indices above 
surveys = surveys[-indices.to.remove]




### Import data for labour statistics ###
labour = read.csv("1410028701_databaseLoadingData.csv")

# remove unnecessary columns
labour[,c("DGUID", "Sex", "Age.group", "Statistics", "UOM", "UOM_ID", "VECTOR", 
          "COORDINATE", "STATUS", "SYMBOL", "TERMINATED")] = NULL

colnames(labour)[1] = "REF_DATE"

# extract "employment" and "unemployment" labour force characteristics
labour.employment = labour[labour$Labour.force.characteristics == "Employment" &
                             labour$Data.type == "Seasonally adjusted" &
                             substr(labour$REF_DATE, 1, 4) == "2020",]
labour.unemployment = labour[labour$Labour.force.characteristics == "Unemployment" &
                               labour$Data.type == "Seasonally adjusted" &
                               substr(labour$REF_DATE, 1, 4) == "2020",]


# repeat the corresponding monthly value for each date
date.vals = c()

for (date in labour.unemployment$REF_DATE) {
  date.vals = c(date.vals, 
                rep(match(date, labour.unemployment$REF_DATE), nrow(cases.full.imp[substr(cases.full.imp$date, 1, 7) == date,])))
}

# expand datasets so days can be added
labour.employment = labour.employment[date.vals,]
labour.unemployment = labour.unemployment[date.vals,]

# add full dates
labour.employment$date = cases.full.imp$date
labour.unemployment$date = cases.full.imp$date

# add cases
labour.employment$numtoday = cases.full.imp$numtoday
labour.unemployment$numtoday = cases.full.imp$numtoday

rownames(labour.employment) = NULL
rownames(labour.unemployment) = NULL




### Import data for retail sales ###
retail = read.csv("2010007201_databaseLoadingData.csv")


# edit first column name
colnames(retail)[1] = "REF_DATE"


retail = retail[,c("REF_DATE", "GEO", "Sales", "UOM", "SCALAR_FACTOR", "VALUE", "DECIMALS")]
retail$Sales = as.character(retail$Sales)


# retail trade data
retail.trade = retail[startsWith(retail$Sales, "Retail trade"),]

# electronic shopping and mail-order data
retail.electronic = retail[startsWith(retail$Sales, "Electronic"),]

# e-commerce sales data
retail.ecommerce = retail[startsWith(retail$Sales, "Retail E-commerce"),]


# expand monthly data to fit daily data points
month.vals = c()

for (month in retail.trade$REF_DATE) {
  month.vals = c(month.vals, 
                rep(match(month, retail.trade$REF_DATE), nrow(cases.full.imp[substr(cases.full.imp$date, 1, 7) == month,])))
}


retail.trade = retail.trade[month.vals,]
retail.electronic = retail.electronic[month.vals,]
retail.ecommerce = retail.ecommerce[month.vals,]


retail.trade$date = cases.full.imp$date
retail.electronic$date = cases.full.imp$date
retail.ecommerce$date = cases.full.imp$date

retail.trade$REF_DATE = NULL
retail.electronic$REF_DATE = NULL
retail.ecommerce$REF_DATE = NULL




##### Explanatory Data Analysis #####

### EDA for cases ###

# plot (positive and probable) cases across Canada per day
plot(1:nrow(cases[cases$prname == "Canada",]), cases[cases$prname == "Canada",]$numtoday, 
     main = "Total Positive and Probable Cases Per Day", xlab = "", ylab = "Cases", xaxt = "n", type = "l")
axis(1, at = 1:nrow(cases[cases$prname == "Canada",]), labels = cases[cases$prname == "Canada",]$date, las = 3)

# plot number of people tested across Canada per day
plot(1:nrow(cases[cases$prname == "Canada",]), cases[cases$prname == "Canada",]$numtestedtoday, 
     main = "Number of People Tested Per Day", xlab = "", ylab = "People Tested", xaxt = "n", type = "l")
axis(1, at = 1:nrow(cases[cases$prname == "Canada",]), labels = cases[cases$prname == "Canada",]$date, las = 3)



### EDA for stock market ###

# plot closing stock market prices
plot(1:nrow(stocks), stocks$Close_TSX , main = "Closing Stock Prices", 
     xlab = "", ylab = "Price (CAD)", xaxt = "n", type = "l", ylim = c(min(stocks[,2:4])-10, max(stocks[,2:4])))
points(1:nrow(stocks), stocks$Close_NASDAQ, xlab = "", ylab = "Price", xaxt = "n", type = "l", col = 2)
points(1:nrow(stocks), stocks$Close_SP500, xlab = "", ylab = "Price", xaxt = "n", type = "l", col = 3)
axis(1, at = 1:nrow(stocks), labels = stocks$Date, las = 3)
legend(100, 6500, legend = c("TSX", "NASDAQ", "S&P500"), col = c(1, 2, 3), pch = 20, cex = 0.8, pt.cex = 1.8)

# plot volumes
plot(1:nrow(stocks), stocks$Volume_TSX , main = "Stock Volumes", 
     xlab = "", ylab = "Volume", xaxt = "n", type = "l", ylim = c(min(stocks[,5:7])-10, max(stocks[,5:7])))
points(1:nrow(stocks), stocks$Volume_NASDAQ, xlab = "", ylab = "Price", xaxt = "n", type = "l", col = 2)
points(1:nrow(stocks), stocks$Volume_SP500, xlab = "", ylab = "Price", xaxt = "n", type = "l", col = 3)
axis(1, at = 1:nrow(stocks), labels = stocks$Date, las = 3)
legend(110, 2500000000, legend = c("TSX", "NASDAQ", "S&P500"), col = c(1, 2, 3), pch = 20, cex = 0.8, pt.cex = 1.8)



### EDA for flights ###

# plot number of flights (international and domestic) coming to Canada per day
plot(1:length(flights.per.day$num_flights), flights.per.day$num_flights, type = "l", 
     main = "Total No. Flights Landing in Canada", xaxt = "n", xlab = "", ylab = "Flights")
axis(1, at = 1:length(flights.per.day$num_flights), labels = flights.per.day$date, las = 3)

# plot number of flights (international and domestic) coming to each province per day
# (on separate plots)
for (location in unique(ca.codes$province)) {
  # plot sequence as x first for quicker plotting
  # <location> above is the acronym for each province, but since the column names of <flights.per.day.per.prov>
  # contain the province acronym, use grep to get the corresponding column name
  plot(1:nrow(flights.per.day.per.prov), 
       flights.per.day.per.prov[,grep(location, colnames(flights.per.day.per.prov))], type = "l",
       main = paste0("No. Flights Landing in ", location, " per day"), xaxt = "n", xlab = "", ylab = "")
  # add dates as axis labels
  axis(1, at = 1:nrow(flights.per.day.per.prov), labels = flights.per.day.per.prov$day, las = 3)
}

# (on a single plot)
# plot first province
plot(1:nrow(flights.per.day.per.prov), flights.per.day.per.prov[,colnames(flights.per.day.per.prov)[2]], 
     type = "l", col = 1, main = "No. Flights Landing in each Province Per Day", xaxt = "n", xlab = "", 
     ylab = "Flights",
     ylim = c(min(flights.per.day.per.prov[,-c(1)]), max(flights.per.day.per.prov[,-c(1)])))
axis(1, at = 1:nrow(flights.per.day.per.prov), labels = flights.per.day.per.prov$day, las = 3)

# plot remainder of provinces
colours = c(2:8, "aliceblue", "bisque4", "darkgoldenrod2", "darkorchid1", "aquamarine")
i = 1
for (colname in colnames(flights.per.day.per.prov)[-c(1, 2)]) {
  points(1:nrow(flights.per.day.per.prov), flights.per.day.per.prov[, colname], type = "l", col = i)
  i = i + 1
}
legend(150, 950, legend = unique(ca.codes$province), col = colours, pch = 20, ncol = 2, pt.cex = 2)


# plot number of international flights arriving in Canada per day
plot(1:nrow(intl.flights.per.day), intl.flights.per.day$num_flights, type = "l", 
     main = "No. Int'l Flights Arriving in Canada per Day", xaxt = "n", xlab = "", 
     ylab = "Flights")
axis(1, at = 1:nrow(intl.flights.per.day), labels = intl.flights.per.day$day, las = 3)



# plot number of domestic flights arriving in Canada per day
plot(1:nrow(dom.flights.per.day), dom.flights.per.day$num_flights, type = "l", 
     main = "No. Dom Flights Arriving in Canada per Day", xaxt = "n", xlab = "", 
     ylab = "Flights")
axis(1, at = 1:nrow(dom.flights.per.day), labels = dom.flights.per.day$day, las = 3)


# plot number of international flights landing per province/territory per day
# (on separate plots)
for (location in unique(ca.codes$province)) {
  plot(1:nrow(intl.flights.per.day.per.prov), 
       intl.flights.per.day.per.prov[,grep(location, colnames(intl.flights.per.day.per.prov))], type = "l",
       main = paste0("No. Int'l Flights Landing in ", location, " per day"), xaxt = "n", xlab = "", ylab = "")
  # add dates as axis labels
  axis(1, at = 1:nrow(intl.flights.per.day.per.prov), labels = intl.flights.per.day.per.prov$day, las = 3)
}

# (on a single plot)
# plot the first province
plot(1:nrow(intl.flights.per.day.per.prov), 
     intl.flights.per.day.per.prov[,colnames(intl.flights.per.day.per.prov)[2]], 
     type = "l", col = 1, main = "No. Int'l Flights Landing in each Province Per Day", 
     xaxt = "n", xlab = "", ylab = "Flights",
     ylim = c(min(intl.flights.per.day.per.prov[,-c(1)]), max(intl.flights.per.day.per.prov[,-c(1)])))
axis(1, at = 1:nrow(intl.flights.per.day.per.prov), labels = intl.flights.per.day.per.prov$day, las = 3)

# plot the remainder of provinces
i = 1
for (colname in colnames(intl.flights.per.day.per.prov)[-c(1, 2)]) {
  points(1:nrow(intl.flights.per.day.per.prov), intl.flights.per.day.per.prov[, colname], type = "l", col = i)
  i = i + 1
}
legend(150, 620, legend = unique(ca.codes$province), col = colours, pch = 20, ncol = 2, pt.cex = 2)


# plot number of domestic flights landing in each province/territory per day
# (on separate plots)
for (location in unique(ca.codes$province)) {
  plot(1:nrow(dom.flights.per.day.per.prov), 
       dom.flights.per.day.per.prov[,grep(location, colnames(dom.flights.per.day.per.prov))], type = "l",
       main = paste0("No. Dom Flights Landing in ", location, " per day"), xaxt = "n", xlab = "", ylab = "")
  # add dates as axis labels
  axis(1, at = 1:nrow(dom.flights.per.day.per.prov), labels = dom.flights.per.day.per.prov$day, las = 3)
}

# (on a single plot)
# plot the first province
plot(1:nrow(dom.flights.per.day.per.prov), 
     dom.flights.per.day.per.prov[,colnames(dom.flights.per.day.per.prov)[2]], 
     type = "l", col = 1, main = "No. Dom Flights Landing in each Province Per Day", 
     xaxt = "n", xlab = "", ylab = "Flights",
     ylim = c(min(dom.flights.per.day.per.prov[,-c(1)]), max(dom.flights.per.day.per.prov[,-c(1)])))
axis(1, at = 1:nrow(dom.flights.per.day.per.prov), labels = dom.flights.per.day.per.prov$day, las = 3)

# plot the remainder of provinces
i = 1
for (colname in colnames(dom.flights.per.day.per.prov)[-c(1, 2)]) {
  points(1:nrow(dom.flights.per.day.per.prov), dom.flights.per.day.per.prov[, colname], type = "l", col = i)
  i = i + 1
}
legend(110, 400, legend = unique(ca.codes$province), col = colours, pch = 20, ncol = 3, pt.cex = 2)



### EDA for weather ###

# plot mean daily temperature in each city

for (i in 1:length(weather.cities)) {
  points(1:nrow(weather.cities[[i]]), weather.cities[[i]]$MEAN_TEMPERATURE, type = "l", 
       main = paste("Mean Temperature in", names(weather.cities)[i]), xaxt = "n", xlab = "", 
       ylab = "Temperature (Celsius)")
  # add dates to x-axis
  axis(1, at = 1:nrow(weather.cities[[i]]), labels = weather.cities[[i]]$LOCAL_DATE, las = 3)
}


# plot minimum humidity in each city
for (i in 1:length(weather.cities)) {
  plot(1:nrow(weather.cities[[i]]), weather.cities[[i]]$MIN_REL_HUMIDITY, type = "l", 
       main = paste("Min Relative Humidity in", names(weather.cities)[i]), xaxt = "n", xlab = "", 
       ylab = "Relative Humidity")
  # add dates to x-axis
  axis(1, at = 1:nrow(weather.cities[[i]]), labels = weather.cities[[i]]$LOCAL_DATE, las = 3)
}


# plot mean daily temperature in each province (averaged across the two cities)
for (i in 1:length(weather.provinces)) {
  plot(1:nrow(weather.provinces[[i]]), weather.provinces[[i]]$AVG_MEAN_TEMPERATURE, type = "l", 
       main = paste("Average Mean Temperature in", names(weather.provinces)[i]), xaxt = "n", xlab = "", 
       ylab = "Temperature (Celsius)")
  # add dates to x-axis
  axis(1, at = 1:nrow(weather.provinces[[i]]), labels = weather.provinces[[i]]$LOCAL_DATE, las = 3)
}


# plot minimum humidity in each province (averaged across the two cities)
for (i in 1:length(weather.provinces)) {
  plot(1:nrow(weather.provinces[[i]]), weather.provinces[[i]]$AVG_MIN_REL_HUMIDITY, type = "l", 
       main = paste("Average Min Relative Humidity in", names(weather.provinces)[i]), xaxt = "n", xlab = "", 
       ylab = "Relative Humidity")
  # add dates to x-axis
  axis(1, at = 1:nrow(weather.provinces[[i]]), labels = weather.provinces[[i]]$LOCAL_DATE, las = 3)
}





### EDA for mobility patterns ###

# plot Google mobility data aggregated across Canada
i = 3
plot(1:nrow(mobility.g.aggregate), mobility.g.aggregate[,i], 
     main = "Changes in Mobility Patterns- Google", 
     xlab = "", ylab = "Percent Change from Baseline", xaxt = "n", type = "l", ylim = c(-100, 250))
 # plot axis labels
axis(1, at = 1:nrow(mobility.g.aggregate), labels = mobility.g.aggregate$date, las = 3)

for (i in 4:ncol(mobility.g.aggregate)) {
  # plot data points
  points(1:nrow(mobility.g.aggregate), mobility.g.aggregate[,i], 
       main = paste0("Change in Movement to: ", 
                     str_replace(str_replace_all(colnames(mobility.g.aggregate)[i], pattern = "_", replacement = " "), pattern = " percent change from baseline", replacement = "")), 
       xlab = "", ylab = "Percent Change from Baseline", xaxt = "n", type = "l", col = colours[i-3])
 
}
legend(0, 200, legend = c("retail", "grocery", "parks", "transit", "workplaces", "res"), 
       col = c(1, colours[1:5]),pch = 20, pt.cex = 1.5) 


# plot Apple mobility data aggregated across Canada
i = 6
plot(1:nrow(mobility.a[mobility.a$sub.region == "Total",]), 
     mobility.a[mobility.a$sub.region == "Total",i], 
     main = "Changes in Directions Requests - Apple", 
     xlab = "", ylab = "Percent Change from Baseline", xaxt = "n", type = "l",
     ylim = c(-100, 110))

# label axes
axis(1, at = 1:nrow(mobility.a), labels = mobility.a$date, las = 3)
  
for (i in 7:ncol(mobility.a)){
  # plot data points
  points(1:nrow(mobility.a[mobility.a$sub.region == "Total",]), 
       mobility.a[mobility.a$sub.region == "Total",i], 
       main = paste0("Change in Directions Requests for ", capitalize(colnames(mobility.a)[i])), 
       xlab = "", ylab = "Percent Change from Baseline", xaxt = "n", type = "l", col = colours[i-6])

}
legend(0, 100, legend = c("driving", "transit", "walking"), 
       col = c(1, colours[1:3]),pch = 20, pt.cex = 1.5) 





### EDA for government policies ###

# plot government response index
plot(1:nrow(policies), policies$GovernmentResponseIndex, main = "Government Response Index", xaxt = "n",
     xlab = "", ylab = "Index Value", type = "l")
axis(1, at = 1:nrow(policies), labels = policies$Date, las = 3)

# plot containment and health index
plot(1:nrow(policies), policies$ContainmentHealthIndex, main = "Containment and Health Index", xaxt = "n",
     xlab = "", ylab = "Index Value", type = "l")
axis(1, at = 1:nrow(policies), labels = policies$Date, las = 3)

# plot stringency index
plot(1:nrow(policies), policies$StringencyIndex, main = "Stringency Index", xaxt = "n",
     xlab = "", ylab = "Index Value", type = "l")
axis(1, at = 1:nrow(policies), labels = policies$Date, las = 3)

# plot community understanding index
plot(1:nrow(riskopen), riskopen$community_understanding, main = "Community Understanding Index", xaxt = "n",
     xlab = "", ylab = "Index Value", type = "l")
axis(1, at = 1:nrow(riskopen), labels = riskopen$Date, las = 3)

# plot risk of openness index
plot(1:nrow(riskopen), riskopen$openness_risk, main = "Risk of Openness Index", xaxt = "n",
     xlab = "", ylab = "Index Value", type = "l")
axis(1, at = 1:nrow(riskopen), labels = riskopen$Date, las = 3)



### EDA for search queries ###

# plot search query popularity of five most correlated terms

queries_names = colnames(queries_weighted)[3:7]
queries_names_abr = substr(queries_names, 7, nchar(queries_names))

plot(1:nrow(queries), queries[,queries_names[1]], type = "l", 
       main = "Top 5 Most Correlated Queries", 
       xlab = "", ylab = "Popularity (% relative to maximum)", xaxt = "n")
points(1:nrow(queries), queries[,queries_names[2]], type = "l", col = 2)
points(1:nrow(queries), queries[,queries_names[3]], type = "l", col = 3)
points(1:nrow(queries), queries[,queries_names[4]], type = "l", col = 4)
points(1:nrow(queries), queries[,queries_names[5]], type = "l", col = 5)
legend(-5, 0.9, legend = queries_names_abr, col = c(1, 2, 3, 4, 5), pch = 20, pt.cex = 1.5, cex = 0.6)
axis(1, at = 1:nrow(queries), labels = queries$Day, las = 3)


# plot all queries
for (i in 1:ncol(queries)) {
  # plot data points
  plot(1:nrow(queries), queries[,i], type = "l", 
       main = paste0("Popularity of ", "'", colnames(queries)[i], "'"), 
       xlab = "", ylab = "Popularity (% relative to maximum)", xaxt = "n")
  # add axis labels
  axis(1, at = 1:nrow(queries), labels = queries$Day, las = 3)
}

# plot count of unique COVID-related search queries per day
plot(1:nrow(pop_by_date), pop_by_date$popularity, 
     main = "Number of Unique Queries Related to Covid Per Day", 
     xlab = "", ylab = "Count", xaxt = "n", type = "l")
axis(1, at = 1:nrow(pop_by_date), labels = pop_by_date$Date, las = 3)

# plot cumulative popularity of COVID-related search queries per day
plot(1:nrow(pop_by_date), pop_by_date$cumul_pop, 
     main = "Cumulative Popularity of COVID-Related Searches Per Day", 
     xlab = "", ylab = "Count", xaxt = "n", type = "l")
axis(1, at = 1:nrow(pop_by_date), labels = pop_by_date$Date, las = 3)


### EDA for Twitter ###

# plot relevant daily tweet count from cleaned dataset
plot(1:nrow(twitter.stats.clean), twitter.stats.clean$total_tweets, main = "Relevant Tweets Per Day (Cleaned Dataset)", 
     xlab = "", ylab = "Count", xaxt = "n", type = "l")
axis(1, at = 1:nrow(twitter.stats.clean), labels = twitter.stats.clean$date, las = 3)


# plot relevant daily tweet count from full dataset
plot(1:nrow(twitter.stats.full), twitter.stats.full$total_tweets, main = "Relevant Tweets Per Day (Full Dataset)", 
     xlab = "", ylab = "Count", xaxt = "n", type = "l")
axis(1, at = 1:nrow(twitter.stats.full), labels = twitter.stats.full$date, las = 3)


# plot weighted mean polarity per day
plot(82:nrow(weighted_sent_terms), weighted_sent_terms$weighted_sent[complete.cases(weighted_sent_terms$weighted_sent)], 
     main = "Weighted Mean Polarity of Daily Most Frequent Terms", type = "l", xlab = "", 
     ylab = "Polarity (Normalized)", xaxt = "n")
axis(1, at = 82:nrow(weighted_sent_terms), labels = term.files.short, las = 3)

plot(82:nrow(weighted_sent_bigrams), weighted_sent_bigrams$weighted_sent[complete.cases(weighted_sent_bigrams$weighted_sent)], 
     main = "Weighted Mean Polarity of Daily Most Frequent Bigrams", type = "l", xlab = "", 
     ylab = "Polarity (Normalized)", xaxt = "n")
axis(1, at = 82:nrow(weighted_sent_bigrams), labels = bigram.files.short, las = 3)

plot(82:nrow(weighted_sent_trigrams), weighted_sent_trigrams$weighted_sent[82:length(weighted_sent_trigrams$weighted_sent)], 
     main = "Weighted Mean Polarity of Daily Most Frequent Trigrams", type = "l", xlab = "", 
     ylab = "Polarity", xaxt = "n" )
axis(1, at = 82:nrow(weighted_sent_trigrams), labels = twitter.stats.full$date[82:length(twitter.stats.full$date)], las = 3)



### EDA for publications ###

# plot title mean polarity by date
plot(1:nrow(publications), mean_sent_title, main = "Mean Polarity of Article Titles", xlab = "",
     ylab = "Polarity", xaxt= "n", type = "l")
axis(1, at = 1:nrow(publications), labels = publications$Entry.Date, las = 3)

# plot abstract mean polarity by date
plot(1:nrow(publications), mean_sent_abstract, main = "Mean Polarity of Article Abstracts", xlab = "",
     ylab = "Polarity", xaxt= "n", type = "l")
axis(1, at = 1:nrow(publications), labels = publications$Entry.Date, las = 3)


### EDA for retail ###
plot(1:nrow(retail.ecommerce), retail.ecommerce$VALUE, xlab = "", xaxt = "n",
     type = "l", ylab = "CAD (thousands)", main = "Retail Sales by Sector", ylim = c(1000, 48000000))
points(1:nrow(retail.trade), retail.trade$VALUE, col = 2, type = "l")
points(1:nrow(retail.trade), retail.electronic$VALUE, col = 3, type = "l")

axis(1, at = 1:nrow(retail.ecommerce), labels = retail.ecommerce$date, las = 3)
legend(1, 20000000, legend = c("ecommerce", "trade", "electronic"), col = c(1, 2, 3), pch = 20, pt.cex = 1.8)




### EDA for labour ###
plot(1:nrow(labour.employment), labour.employment$VALUE, xlab = "", xaxt = "n",
     type = "l", ylab = "People (thousands)", main = "Labour Numbers", 
     ylim = c(1000, 20000))
points(1:nrow(labour.unemployment), labour.unemployment$VALUE, type = "l", col = 2)
axis(1, at = 1:nrow(labour.employment), labels = labour.employment$REF_DATE, las = 3)
legend(1, 10000, legend = c("employed", "unemployed"), col = c(1, 2), pch = 20, pt.cex = 1.8)



##### Min-Max Normalization #####

# Note: now that EDA is complete, apply min-max normalization to the data 
# before correlation analysis and modelling.

# function to normalize data
minmax_norm = function(vector) {
  return((vector-min(vector))/(max(vector)-min(vector)))
}



### stocks ###
stocks.full.imp[2:ncol(stocks.full.imp)] = sapply(stocks.full.imp[2:ncol(stocks.full.imp)], minmax_norm)


### flights ###
intl.flights.per.day$num_flights = minmax_norm(intl.flights.per.day$num_flights) 

intl.flights.per.day.per.prov.2 = intl.flights.per.day.per.prov

intl.flights.per.day.per.prov[,2:ncol(intl.flights.per.day.per.prov)] = 
  sapply(intl.flights.per.day.per.prov[,2:ncol(intl.flights.per.day.per.prov)], minmax_norm)

dom.flights.per.day$num_flights = minmax_norm(dom.flights.per.day$num_flights)

dom.flights.per.day.per.prov[2:ncol(dom.flights.per.day.per.prov)] = sapply(dom.flights.per.day.per.prov[2:ncol(dom.flights.per.day.per.prov)], minmax_norm)

flights.per.day$num_flights = minmax_norm(flights.per.day$num_flights)

flights.per.day.per.prov[,2:ncol(flights.per.day.per.prov)] = sapply(flights.per.day.per.prov[,2:ncol(flights.per.day.per.prov)], minmax_norm)


# change NaN values back to zero
for(i in 1:ncol(intl.flights.per.day.per.prov)) {
  
  # in that column, find all rows that have NaN values
  # change those rows from NaN to zero
  intl.flights.per.day.per.prov[is.nan(intl.flights.per.day.per.prov[,i]),i] = 0
  dom.flights.per.day.per.prov[is.nan(dom.flights.per.day.per.prov[,i]),i] = 0
  flights.per.day.per.prov[is.nan(flights.per.day.per.prov[,i]),i] = 0
  
}


### weather ###
weather.canada[,grep("AVG_", colnames(weather.canada))] = 
  sapply(weather.canada[,grep("AVG_", colnames(weather.canada))], minmax_norm)



### Google mobility ###
mobility.g.aggregate.full.imp[,3:ncol(mobility.g.aggregate.full.imp)] = sapply(mobility.g.aggregate.full.imp[,3:ncol(mobility.g.aggregate.full.imp)],
                                                                               minmax_norm)


### Apple mobility ###
mobility.a.full.imp[,6:8] = sapply(mobility.a.full.imp[,6:8], minmax_norm)



### government policy data ###
policies[,-match("Date", colnames(policies))] = sapply(policies[,-match("Date", colnames(policies))], minmax_norm)

riskopen.imp[,2:3] = sapply(riskopen.imp[,2:3], minmax_norm)



### search queries ###
# Google
queries[,grep("query_", colnames(queries))] = sapply(queries[,grep("query_", colnames(queries))], minmax_norm)

# Bing
pop_by_date_imp[,2:3] = sapply(pop_by_date_imp[,2:3], minmax_norm)



### twitter ###
# metadata
twitter.stats.clean$total_tweets = minmax_norm(twitter.stats.clean$total_tweets)
twitter.stats.full$total_tweets = minmax_norm(twitter.stats.full$total_tweets)

# n-grams
weighted_sent_terms[!(is.na(weighted_sent_terms$weighted_sent)),]$weighted_sent = 
  minmax_norm(weighted_sent_terms[!(is.na(weighted_sent_terms$weighted_sent)),]$weighted_sent)

weighted_sent_bigrams[!(is.na(weighted_sent_bigrams$weighted_sent)),]$weighted_sent = 
  minmax_norm(weighted_sent_bigrams[!(is.na(weighted_sent_bigrams$weighted_sent)),]$weighted_sent)

weighted_sent_trigrams[!(is.na(weighted_sent_trigrams$weighted_sent)),]$weighted_sent = 
  minmax_norm(weighted_sent_trigrams[!(is.na(weighted_sent_trigrams$weighted_sent)),]$weighted_sent)



### publications ###
mean_sent_title[!(is.na(mean_sent_title$mean_sent)),]$mean_sent = minmax_norm(mean_sent_title[!(is.na(mean_sent_title$mean_sent)),]$mean_sent)
mean_sent_abstract[!(is.na(mean_sent_abstract$mean_sent)),]$mean_sent = minmax_norm(mean_sent_abstract[!(is.na(mean_sent_abstract$mean_sent)),]$mean_sent)



### labour ###
labour.employment$VALUE = minmax_norm(labour.employment$VALUE)
labour.unemployment$VALUE = minmax_norm(labour.unemployment$VALUE)


### retail ###
retail.trade$VALUE = minmax_norm(retail.trade$VALUE)
retail.electronic$VALUE = minmax_norm(retail.electronic$VALUE)
retail.ecommerce$VALUE = minmax_norm(retail.ecommerce$VALUE)





##### Correlation Analysis #####

### Compute correlation between cases and stock market prices ###
ccf.stocks = lapply(stocks.full.imp[,2:4], ccf, y = cases.full.imp$numtoday, lag.max = 30, plot = F) 

# get lags of greatest correlation with cases
ccf.stocks$Close_TSX$lag[match(max(abs(ccf.stocks$Close_TSX$acf[ccf.stocks$Close_TSX$lag <= 0])), 
                               abs(ccf.stocks$Close_TSX$acf[ccf.stocks$Close_TSX$lag <= 0]))]
ccf.stocks$Close_NASDAQ$lag[match(max(abs(ccf.stocks$Close_NASDAQ$acf[ccf.stocks$Close_NASDAQ$lag <= 0])), 
                                  abs(ccf.stocks$Close_NASDAQ$acf[ccf.stocks$Close_NASDAQ$lag <= 0]))]
ccf.stocks$Close_SP500$lag[match(max(abs(ccf.stocks$Close_SP500$acf[ccf.stocks$Close_SP500$lag <= 0])), 
                                 abs(ccf.stocks$Close_SP500$acf[ccf.stocks$Close_SP500$lag <= 0]))]

ccf.stocks$Close_TSX$acf[match(max(abs(ccf.stocks$Close_TSX$acf[ccf.stocks$Close_TSX$lag <= 0])), 
                               abs(ccf.stocks$Close_TSX$acf[ccf.stocks$Close_TSX$lag <= 0]))]
ccf.stocks$Close_NASDAQ$acf[match(max(abs(ccf.stocks$Close_NASDAQ$acf[ccf.stocks$Close_NASDAQ$lag <= 0])), 
                                  abs(ccf.stocks$Close_NASDAQ$acf[ccf.stocks$Close_NASDAQ$lag <= 0]))]
ccf.stocks$Close_SP500$acf[match(max(abs(ccf.stocks$Close_SP500$acf[ccf.stocks$Close_SP500$lag <= 0])), 
                                 abs(ccf.stocks$Close_SP500$acf[ccf.stocks$Close_SP500$lag <= 0]))]


ccf.volume = lapply(stocks.full.imp[,5:7], ccf, y = cases.full.imp$numtoday, lag.max = 30, plot = F)

ccf.volume$Volume_TSX$lag[match(max(abs(ccf.volume$Volume_TSX$acf[ccf.stocks$Close_TSX$lag <= 0])), 
                                abs(ccf.volume$Volume_TSX$acf[ccf.stocks$Close_TSX$lag <= 0]))]
ccf.volume$Volume_NASDAQ$lag[match(max(abs(ccf.volume$Volume_NASDAQ$acf[ccf.stocks$Close_NASDAQ$lag <= 0])), 
                                   abs(ccf.volume$Volume_NASDAQ$acf[ccf.stocks$Close_NASDAQ$lag <= 0]))]
ccf.volume$Volume_SP500$lag[match(max(abs(ccf.volume$Volume_SP500$acf[ccf.stocks$Close_SP500$lag <= 0])), 
                                  abs(ccf.volume$Volume_SP500$acf[ccf.stocks$Close_SP500$lag <= 0]))]

ccf.volume$Volume_TSX$acf[match(max(abs(ccf.volume$Volume_TSX$acf[ccf.stocks$Close_TSX$lag <= 0])), 
                                abs(ccf.volume$Volume_TSX$acf[ccf.stocks$Close_TSX$lag <= 0]))]
ccf.volume$Volume_NASDAQ$acf[match(max(abs(ccf.volume$Volume_NASDAQ$acf[ccf.stocks$Close_NASDAQ$lag <= 0])), 
                                   abs(ccf.volume$Volume_NASDAQ$acf[ccf.stocks$Close_NASDAQ$lag <= 0]))]
ccf.volume$Volume_SP500$acf[match(max(abs(ccf.volume$Volume_SP500$acf[ccf.stocks$Close_SP500$lag <= 0])), 
                                  abs(ccf.volume$Volume_SP500$acf[ccf.stocks$Close_SP500$lag <= 0]))]



### Compute correlation between cases and flights ###
ccf.flights = lapply(data.frame("dom_num_flights" = dom.flights.per.day$num_flights,
                           "intl_num_flights" = intl.flights.per.day$num_flights,
                           "total_num_flights" = flights.per.day$num_flights),
                     ccf, y = cases.full.imp$numtoday, lag.max = 30, plot = F)

ccf.flights$dom_num_flights$lag[match(max(abs(ccf.flights$dom_num_flights$acf[ccf.flights$dom_num_flights$lag <= 0])), 
                                      abs(ccf.flights$dom_num_flights$acf[ccf.flights$dom_num_flights$lag <= 0]))]
ccf.flights$intl_num_flights$lag[match(max(abs(ccf.flights$intl_num_flights$acf[ccf.flights$intl_num_flights$lag <= 0])), 
                                       abs(ccf.flights$intl_num_flights$acf[ccf.flights$intl_num_flights$lag <= 0]))]
ccf.flights$total_num_flights$lag[match(max(abs(ccf.flights$total_num_flights$acf[ccf.flights$total_num_flights$lag <= 0])), 
                                        abs(ccf.flights$total_num_flights$acf[ccf.flights$total_num_flights$lag <= 0]))]

ccf.flights$dom_num_flights$acf[match(max(abs(ccf.flights$dom_num_flights$acf[ccf.flights$dom_num_flights$lag <= 0])), 
                                      abs(ccf.flights$dom_num_flights$acf[ccf.flights$dom_num_flights$lag <= 0]))]
ccf.flights$intl_num_flights$acf[match(max(abs(ccf.flights$intl_num_flights$acf[ccf.flights$intl_num_flights$lag <= 0])), 
                                       abs(ccf.flights$intl_num_flights$acf[ccf.flights$intl_num_flights$lag <= 0]))]
ccf.flights$total_num_flights$acf[match(max(abs(ccf.flights$total_num_flights$acf[ccf.flights$total_num_flights$lag <= 0])), 
                                        abs(ccf.flights$total_num_flights$acf[ccf.flights$total_num_flights$lag <= 0]))]




### Compute correlation between cases and weather ###

# compute cross correlation for weather stats across the country 
ccf.weather = lapply(weather.canada[, grep("CAN.", colnames(weather.canada))], ccf, y = cases.full.imp$numtoday, 
                     lag.max = 30, na.action = na.pass, plot = F)

# find lags of max cross correlation
ccf.weather$CAN.AVG_MEAN_TEMPERATURE$lag[match(max(abs(ccf.weather$CAN.AVG_MEAN_TEMPERATURE$acf[ccf.weather$CAN.AVG_MEAN_TEMPERATURE$lag <= 0])), 
                                               abs(ccf.weather$CAN.AVG_MEAN_TEMPERATURE$acf[ccf.weather$CAN.AVG_MEAN_TEMPERATURE$lag <= 0]))]

ccf.weather$CAN.AVG_MIN_REL_HUMIDITY$lag[match(max(abs(ccf.weather$CAN.AVG_MIN_REL_HUMIDITY$acf[ccf.weather$CAN.AVG_MIN_REL_HUMIDITY$lag <= 0])), 
                                               abs(ccf.weather$CAN.AVG_MIN_REL_HUMIDITY$acf[ccf.weather$CAN.AVG_MIN_REL_HUMIDITY$lag <= 0]))]

ccf.weather$CAN.AVG_MAX_REL_HUMIDITY$lag[match(max(abs(ccf.weather$CAN.AVG_MAX_REL_HUMIDITY$acf[ccf.weather$CAN.AVG_MAX_REL_HUMIDITY$lag <= 0])), 
                                               abs(ccf.weather$CAN.AVG_MAX_REL_HUMIDITY$acf[ccf.weather$CAN.AVG_MAX_REL_HUMIDITY$lag <= 0]))]



ccf.weather$CAN.AVG_MEAN_TEMPERATURE$acf[match(max(abs(ccf.weather$CAN.AVG_MEAN_TEMPERATURE$acf[ccf.weather$CAN.AVG_MEAN_TEMPERATURE$lag <= 0])), 
                                               abs(ccf.weather$CAN.AVG_MEAN_TEMPERATURE$acf[ccf.weather$CAN.AVG_MEAN_TEMPERATURE$lag <= 0]))]

ccf.weather$CAN.AVG_MIN_REL_HUMIDITY$acf[match(max(abs(ccf.weather$CAN.AVG_MIN_REL_HUMIDITY$acf[ccf.weather$CAN.AVG_MIN_REL_HUMIDITY$lag <= 0])), 
                                               abs(ccf.weather$CAN.AVG_MIN_REL_HUMIDITY$acf[ccf.weather$CAN.AVG_MIN_REL_HUMIDITY$lag <= 0]))]

ccf.weather$CAN.AVG_MAX_REL_HUMIDITY$acf[match(max(abs(ccf.weather$CAN.AVG_MAX_REL_HUMIDITY$acf[ccf.weather$CAN.AVG_MAX_REL_HUMIDITY$lag <= 0])), 
                                               abs(ccf.weather$CAN.AVG_MAX_REL_HUMIDITY$acf[ccf.weather$CAN.AVG_MAX_REL_HUMIDITY$lag <= 0]))]




### Compute correlation between cases and mobility patterns ###
# check cross correlation (using unimputed data) for google data
ccf.mobility.g = lapply(mobility.g.aggregate.full[,3:ncol(mobility.g.aggregate)], ccf, y = cases.full$numtoday, 
                      lag.max = 30, na.action = na.pass, plot = F)

ccf.mobility.g$retail_and_recreation_percent_change_from_baseline$lag[match(max(abs(ccf.mobility.g$retail_and_recreation_percent_change_from_baseline$acf)), 
                                                                            abs(ccf.mobility.g$retail_and_recreation_percent_change_from_baseline$acf))]
ccf.mobility.g$grocery_and_pharmacy_percent_change_from_baseline$lag[match(max(abs(ccf.mobility.g$grocery_and_pharmacy_percent_change_from_baseline$acf)), 
                                                                            abs(ccf.mobility.g$grocery_and_pharmacy_percent_change_from_baseline$acf))]
ccf.mobility.g$parks_percent_change_from_baseline$lag[match(max(abs(ccf.mobility.g$parks_percent_change_from_baseline$acf)), 
                                                                            abs(ccf.mobility.g$parks_percent_change_from_baseline$acf))]
ccf.mobility.g$transit_stations_percent_change_from_baseline$lag[match(max(abs(ccf.mobility.g$transit_stations_percent_change_from_baseline$acf)), 
                                                            abs(ccf.mobility.g$transit_stations_percent_change_from_baseline$acf))]
ccf.mobility.g$workplaces_percent_change_from_baseline$lag[match(max(abs(ccf.mobility.g$workplaces_percent_change_from_baseline$acf)), 
                                                                       abs(ccf.mobility.g$workplaces_percent_change_from_baseline$acf))]
ccf.mobility.g$residential_percent_change_from_baseline$lag[match(max(abs(ccf.mobility.g$residential_percent_change_from_baseline$acf)), 
                                                                 abs(ccf.mobility.g$residential_percent_change_from_baseline$acf))]




# check cross correlation (using imputed data) for google data
ccf.mobility.g.imp = lapply(mobility.g.aggregate.full.imp[,3:ncol(mobility.g.aggregate.full.imp)], ccf, y = cases.full$numtoday, 
                         lag.max = 30, na.action = na.pass, plot = F)

ccf.mobility.g.imp$retail_and_recreation_percent_change_from_baseline$lag[match(max(abs(ccf.mobility.g.imp$retail_and_recreation_percent_change_from_baseline$acf)), 
                                                                            abs(ccf.mobility.g.imp$retail_and_recreation_percent_change_from_baseline$acf))]
ccf.mobility.g.imp$grocery_and_pharmacy_percent_change_from_baseline$lag[match(max(abs(ccf.mobility.g.imp$grocery_and_pharmacy_percent_change_from_baseline$acf)), 
                                                                           abs(ccf.mobility.g.imp$grocery_and_pharmacy_percent_change_from_baseline$acf))]
ccf.mobility.g.imp$parks_percent_change_from_baseline$lag[match(max(abs(ccf.mobility.g.imp$parks_percent_change_from_baseline$acf)), 
                                                            abs(ccf.mobility.g.imp$parks_percent_change_from_baseline$acf))]
ccf.mobility.g.imp$transit_stations_percent_change_from_baseline$lag[match(max(abs(ccf.mobility.g.imp$transit_stations_percent_change_from_baseline$acf)), 
                                                                       abs(ccf.mobility.g.imp$transit_stations_percent_change_from_baseline$acf))]
ccf.mobility.g.imp$workplaces_percent_change_from_baseline$lag[match(max(abs(ccf.mobility.g.imp$workplaces_percent_change_from_baseline$acf)), 
                                                                 abs(ccf.mobility.g.imp$workplaces_percent_change_from_baseline$acf))]
ccf.mobility.g.imp$residential_percent_change_from_baseline$lag[match(max(abs(ccf.mobility.g.imp$residential_percent_change_from_baseline$acf)), 
                                                                  abs(ccf.mobility.g.imp$residential_percent_change_from_baseline$acf))]


ccf.mobility.g.imp$retail_and_recreation_percent_change_from_baseline$acf[match(max(abs(ccf.mobility.g.imp$retail_and_recreation_percent_change_from_baseline$acf)), 
                                                                                abs(ccf.mobility.g.imp$retail_and_recreation_percent_change_from_baseline$acf))]
ccf.mobility.g.imp$grocery_and_pharmacy_percent_change_from_baseline$acf[match(max(abs(ccf.mobility.g.imp$grocery_and_pharmacy_percent_change_from_baseline$acf)), 
                                                                               abs(ccf.mobility.g.imp$grocery_and_pharmacy_percent_change_from_baseline$acf))]
ccf.mobility.g.imp$parks_percent_change_from_baseline$acf[match(max(abs(ccf.mobility.g.imp$parks_percent_change_from_baseline$acf)), 
                                                                abs(ccf.mobility.g.imp$parks_percent_change_from_baseline$acf))]
ccf.mobility.g.imp$transit_stations_percent_change_from_baseline$acf[match(max(abs(ccf.mobility.g.imp$transit_stations_percent_change_from_baseline$acf)), 
                                                                           abs(ccf.mobility.g.imp$transit_stations_percent_change_from_baseline$acf))]
ccf.mobility.g.imp$workplaces_percent_change_from_baseline$acf[match(max(abs(ccf.mobility.g.imp$workplaces_percent_change_from_baseline$acf)), 
                                                                     abs(ccf.mobility.g.imp$workplaces_percent_change_from_baseline$acf))]
ccf.mobility.g.imp$residential_percent_change_from_baseline$acf[match(max(abs(ccf.mobility.g.imp$residential_percent_change_from_baseline$acf)), 
                                                                      abs(ccf.mobility.g.imp$residential_percent_change_from_baseline$acf))]



# check cross correlation for apple data
ccf.mobility.a = lapply(mobility.a.full[,c("driving", "transit", "walking")], 
                        ccf, y = cases.full$numtoday, lag.max = 30, na.action = na.pass, plot = F)

ccf.mobility.a$driving$lag[match(max(abs(ccf.mobility.a$driving$acf[ccf.mobility.a$driving$lag <= 0])), 
                                 abs(ccf.mobility.a$driving$acf[ccf.mobility.a$driving$lag <= 0]))]
ccf.mobility.a$transit$lag[match(max(abs(ccf.mobility.a$transit$acf[ccf.mobility.a$transit$lag <= 0])), 
                                 abs(ccf.mobility.a$transit$acf[ccf.mobility.a$transit$lag <= 0]))]
ccf.mobility.a$walking$lag[match(max(abs(ccf.mobility.a$walking$acf[ccf.mobility.a$walking$lag <= 0])), 
                                 abs(ccf.mobility.a$walking$acf[ccf.mobility.a$walking$lag <= 0]))]



ccf.mobility.a.imp = lapply(mobility.a.full.imp[,c("driving", "transit", "walking")], 
                            ccf, y = cases.full$numtoday, lag.max = 30, na.action = na.pass, plot = F)

ccf.mobility.a.imp$driving$lag[match(max(abs(ccf.mobility.a.imp$driving$acf[ccf.mobility.a.imp$driving$lag <= 0])), 
                                 abs(ccf.mobility.a.imp$driving$acf[ccf.mobility.a.imp$driving$lag <= 0]))]
ccf.mobility.a.imp$transit$lag[match(max(abs(ccf.mobility.a.imp$transit$acf[ccf.mobility.a.imp$transit$lag <= 0])), 
                                 abs(ccf.mobility.a.imp$transit$acf[ccf.mobility.a.imp$transit$lag <= 0]))]
ccf.mobility.a.imp$walking$lag[match(max(abs(ccf.mobility.a.imp$walking$acf[ccf.mobility.a.imp$walking$lag <= 0])), 
                                 abs(ccf.mobility.a.imp$walking$acf[ccf.mobility.a.imp$walking$lag <= 0]))]


ccf.mobility.a.imp$driving$acf[match(max(abs(ccf.mobility.a.imp$driving$acf[ccf.mobility.a.imp$driving$lag <= 0])), 
                                     abs(ccf.mobility.a.imp$driving$acf[ccf.mobility.a.imp$driving$lag <= 0]))]
ccf.mobility.a.imp$transit$acf[match(max(abs(ccf.mobility.a.imp$transit$acf[ccf.mobility.a.imp$transit$lag <= 0])), 
                                     abs(ccf.mobility.a.imp$transit$acf[ccf.mobility.a.imp$transit$lag <= 0]))]
ccf.mobility.a.imp$walking$acf[match(max(abs(ccf.mobility.a.imp$walking$acf[ccf.mobility.a.imp$walking$lag <= 0])), 
                                     abs(ccf.mobility.a.imp$walking$acf[ccf.mobility.a.imp$walking$lag <= 0]))]




### Compute correlation between cases and government policies ###
# check correlation between cases and the selected index values in <policies> and <riskopen> below 
ccf.policies = lapply(policies[,c("StringencyIndex", "GovernmentResponseIndex", "ContainmentHealthIndex")], ccf, 
                      y = cases.full$numtoday, lag.max = 30, na.action = na.pass, plot = FALSE)
ccf.policies

# get lags of max cross correlation for each variable
ccf.policies$StringencyIndex$lag[match(max(abs(ccf.policies$StringencyIndex$acf)), 
                                       abs(ccf.policies$StringencyIndex$acf))]
ccf.policies$GovernmentResponseIndex$lag[match(max(abs(ccf.policies$GovernmentResponseIndex$acf)), 
                                               abs(ccf.policies$GovernmentResponseIndex$acf))]
ccf.policies$ContainmentHealthIndex$lag[match(max(abs(ccf.policies$ContainmentHealthIndex$acf)), 
                                              abs(ccf.policies$ContainmentHealthIndex$acf))]

ccf.policies$StringencyIndex$acf[match(max(abs(ccf.policies$StringencyIndex$acf)), 
                                       abs(ccf.policies$StringencyIndex$acf))]
ccf.policies$GovernmentResponseIndex$acf[match(max(abs(ccf.policies$GovernmentResponseIndex$acf)), 
                                               abs(ccf.policies$GovernmentResponseIndex$acf))]
ccf.policies$ContainmentHealthIndex$acf[match(max(abs(ccf.policies$ContainmentHealthIndex$acf)), 
                                              abs(ccf.policies$ContainmentHealthIndex$acf))]




ccf.riskopen = lapply(riskopen[,c("community_understanding", "openness_risk")], ccf, 
                      y = cases.full$numtoday, lag.max = 30, na.action = na.pass, plot = FALSE)
ccf.riskopen

# get lags of max cross correlation for each variable
ccf.riskopen$community_understanding$lag[match(max(abs(ccf.riskopen$community_understanding$acf)), 
                                               abs(ccf.riskopen$community_understanding$acf))]
ccf.riskopen$openness_risk$lag[match(max(abs(ccf.riskopen$openness_risk$acf)), 
                                     abs(ccf.riskopen$openness_risk$acf))]

ccf.riskopen$community_understanding$acf[match(max(abs(ccf.riskopen$community_understanding$acf)), 
                                               abs(ccf.riskopen$community_understanding$acf))]
ccf.riskopen$openness_risk$acf[match(max(abs(ccf.riskopen$openness_risk$acf)), 
                                     abs(ccf.riskopen$openness_risk$acf))]




### Compute correlation between cases and search queries ###
ccf.queries = lapply(queries[,2:ncol(queries)], ccf, y = cases.full.imp$numtoday, 
                            lag.max = 30, na.action = na.pass, plot = FALSE) # search engine fractions

ccf.queries.lag0 = sapply(queries[,2:ncol(queries)], cor, y = cases.full.imp$numtoday)

# for each query, print lag most correlated with cases 
for (name in names(ccf.queries)) {
  print(c(name, ccf.queries[[name]]$lag[match(max(abs(ccf.queries[[name]]$acf[ccf.queries[[name]]$lag <= 0])), 
                             abs(ccf.queries[[name]]$acf[ccf.queries[[name]]$lag <= 0]))]))
}

# for each query, print correlation at lag most correlated with cases 
for (name in names(ccf.queries)) {
  print(c(name, ccf.queries[[name]]$acf[match(max(abs(ccf.queries[[name]]$acf[ccf.queries[[name]]$lag <= 0])), 
                                              abs(ccf.queries[[name]]$acf[ccf.queries[[name]]$lag <= 0]))]))
}


ccf.popularity = lapply(pop_by_date[,2:3], ccf, y = cases.full.imp$numtoday, 
                        lag.max = 30, na.action = na.pass, plot = FALSE) # popularity

ccf.popularity$popularity$lag[match(max(abs(ccf.popularity$popularity$acf[ccf.popularity$popularity$lag <= 0])), 
                                    abs(ccf.popularity$popularity$acf[ccf.popularity$popularity$lag <= 0]))]
ccf.popularity$cumul_pop$lag[match(max(abs(ccf.popularity$cumul_pop$acf[ccf.popularity$cumul_pop$lag <= 0])), 
                                    abs(ccf.popularity$cumul_pop$acf[ccf.popularity$cumul_pop$lag <= 0]))]



ccf.popularity.imp = lapply(pop_by_date_imp[,2:3], ccf, y = cases.full.imp$numtoday, 
                            lag.max = 30, na.action = na.pass, plot = FALSE) # imputed popularity

ccf.popularity.imp$popularity$lag[match(max(abs(ccf.popularity.imp$popularity$acf[ccf.popularity.imp$popularity$lag <= 0])), 
                                    abs(ccf.popularity.imp$popularity$acf[ccf.popularity.imp$popularity$lag <= 0]))]
ccf.popularity.imp$cumul_pop$lag[match(max(abs(ccf.popularity.imp$cumul_pop$acf[ccf.popularity.imp$cumul_pop$lag <= 0])), 
                                   abs(ccf.popularity.imp$cumul_pop$acf[ccf.popularity.imp$cumul_pop$lag <= 0]))]


ccf.popularity.imp$popularity$acf[match(max(abs(ccf.popularity.imp$popularity$acf[ccf.popularity.imp$popularity$lag <= 0])), 
                                        abs(ccf.popularity.imp$popularity$acf[ccf.popularity.imp$popularity$lag <= 0]))]
ccf.popularity.imp$cumul_pop$acf[match(max(abs(ccf.popularity.imp$cumul_pop$acf[ccf.popularity.imp$cumul_pop$lag <= 0])), 
                                       abs(ccf.popularity.imp$cumul_pop$acf[ccf.popularity.imp$cumul_pop$lag <= 0]))]




### Compute correlation between cases and tweets ###
# compute correlation between daily cases and count of relevant tweets per day
ccf.tweets = lapply(data.frame("total_tweets_clean" = twitter.stats.clean$total_tweets,
                               "total_tweets_full" = twitter.stats.full$total_tweets),
                    ccf, y = cases.full.imp$numtoday, lag.max = 30, na.action = na.pass, plot = FALSE)

ccf.tweets$total_tweets_clean$lag[match(max(abs(ccf.tweets$total_tweets_clean$acf[ccf.tweets$total_tweets_clean$lag<=0])), 
                                        abs(ccf.tweets$total_tweets_clean$acf[ccf.tweets$total_tweets_clean$lag<=0]))]
ccf.tweets$total_tweets_full$lag[match(max(abs(ccf.tweets$total_tweets_full$acf[ccf.tweets$total_tweets_full$lag<=0])), 
                                        abs(ccf.tweets$total_tweets_full$acf[ccf.tweets$total_tweets_full$lag<=0]))]

ccf.tweets$total_tweets_clean$acf[match(max(abs(ccf.tweets$total_tweets_clean$acf[ccf.tweets$total_tweets_clean$lag<=0])), 
                                        abs(ccf.tweets$total_tweets_clean$acf[ccf.tweets$total_tweets_clean$lag<=0]))]
ccf.tweets$total_tweets_full$acf[match(max(abs(ccf.tweets$total_tweets_full$acf[ccf.tweets$total_tweets_full$lag<=0])), 
                                       abs(ccf.tweets$total_tweets_full$acf[ccf.tweets$total_tweets_full$lag<=0]))]




# compute correlation between daily cases and daily sentiment of terms, bigrams and trigrams (not imputed)
ccf.sent.tweets = lapply(data.frame("weighted_sent_terms" = weighted_sent_terms$weighted_sent,
                             "weighted_sent_bigrams" = weighted_sent_bigrams$weighted_sent,
                             "weighted_sent_trigrams" = weighted_sent_trigrams$weighted_sent), 
                  ccf, y = cases.full.imp$numtoday, lag.max = 30, na.action = na.pass, plot = FALSE)

ccf.sent.tweets$weighted_sent_terms$lag[match(max(abs(ccf.sent.tweets$weighted_sent_terms$acf[ccf.sent.tweets$weighted_sent_terms$lag <= 0])), 
                                       abs(ccf.sent.tweets$weighted_sent_terms$acf[ccf.sent.tweets$weighted_sent_terms$lag <= 0]))]
ccf.sent.tweets$weighted_sent_bigrams$lag[match(max(abs(ccf.sent.tweets$weighted_sent_bigrams$acf[ccf.sent.tweets$weighted_sent_bigrams$lag <= 0])), 
                                       abs(ccf.sent.tweets$weighted_sent_bigrams$acf[ccf.sent.tweets$weighted_sent_bigrams$lag <= 0]))]
ccf.sent.tweets$weighted_sent_trigrams$lag[match(max(abs(ccf.sent.tweets$weighted_sent_trigrams$acf[ccf.sent.tweets$weighted_sent_trigrams$lag <= 0])), 
                                       abs(ccf.sent.tweets$weighted_sent_trigrams$acf[ccf.sent.tweets$weighted_sent_trigrams$lag <= 0]))]


ccf.sent.tweets$weighted_sent_terms$acf[match(max(abs(ccf.sent.tweets$weighted_sent_terms$acf[ccf.sent.tweets$weighted_sent_terms$lag <= 0])), 
                                              abs(ccf.sent.tweets$weighted_sent_terms$acf[ccf.sent.tweets$weighted_sent_terms$lag <= 0]))]
ccf.sent.tweets$weighted_sent_bigrams$acf[match(max(abs(ccf.sent.tweets$weighted_sent_bigrams$acf[ccf.sent.tweets$weighted_sent_bigrams$lag <= 0])), 
                                                abs(ccf.sent.tweets$weighted_sent_bigrams$acf[ccf.sent.tweets$weighted_sent_bigrams$lag <= 0]))]
ccf.sent.tweets$weighted_sent_trigrams$acf[match(max(abs(ccf.sent.tweets$weighted_sent_trigrams$acf[ccf.sent.tweets$weighted_sent_trigrams$lag <= 0])), 
                                                 abs(ccf.sent.tweets$weighted_sent_trigrams$acf[ccf.sent.tweets$weighted_sent_trigrams$lag <= 0]))]



### Compute correlation between cases and publications ###
# compute correlation between daily mean sentiment (in publication titles and abstracts) and cases

ccf.sent.pubs = lapply(data.frame("mean_sent_title_pubs" = mean_sent_title$mean_sent, 
                                  "mean_sent_abstract_pubs" = mean_sent_abstract$mean_sent),
                       ccf, y = cases.full.imp$numtoday, lag.max = 30, na.action = na.pass, plot = FALSE)

ccf.sent.pubs$mean_sent_title_pubs$lag[match(max(abs(ccf.sent.pubs$mean_sent_title_pubs$acf[ccf.sent.pubs$mean_sent_title_pubs$lag <= 0])), 
                                             abs(ccf.sent.pubs$mean_sent_title_pubs$acf[ccf.sent.pubs$mean_sent_title_pubs$lag <= 0]))]
ccf.sent.pubs$mean_sent_abstract_pubs$lag[match(max(abs(ccf.sent.pubs$mean_sent_abstract_pubs$acf[ccf.sent.pubs$mean_sent_abstract_pubs$lag <= 0])), 
                                             abs(ccf.sent.pubs$mean_sent_abstract_pubs$acf[ccf.sent.pubs$mean_sent_abstract_pubs$lag <= 0]))]


ccf.sent.pubs$mean_sent_title_pubs$acf[match(max(abs(ccf.sent.pubs$mean_sent_title_pubs$acf[ccf.sent.pubs$mean_sent_title_pubs$lag <= 0])), 
                                             abs(ccf.sent.pubs$mean_sent_title_pubs$acf[ccf.sent.pubs$mean_sent_title_pubs$lag <= 0]))]
ccf.sent.pubs$mean_sent_abstract_pubs$acf[match(max(abs(ccf.sent.pubs$mean_sent_abstract_pubs$acf[ccf.sent.pubs$mean_sent_abstract_pubs$lag <= 0])), 
                                                abs(ccf.sent.pubs$mean_sent_abstract_pubs$acf[ccf.sent.pubs$mean_sent_abstract_pubs$lag <= 0]))]



### Compute correlation between cases and labour data ###
ccf.labour = lapply(data.frame("labour_unemployment" = labour.unemployment$VALUE, 
                               "labour_employment" = labour.employment$VALUE), 
                    ccf, y = cases.full.imp$numtoday, lag.max = 30, na.action = na.pass, plot = FALSE)

ccf.labour$labour_unemployment$lag[match(max(abs(ccf.labour$labour_unemployment$acf[ccf.labour$labour_unemployment$lag <= 0])), 
                                         abs(ccf.labour$labour_unemployment$acf[ccf.labour$labour_unemployment$lag <= 0]))]
ccf.labour$labour_employment$lag[match(max(abs(ccf.labour$labour_employment$acf[ccf.labour$labour_employment$lag <= 0])), 
                                         abs(ccf.labour$labour_employment$acf[ccf.labour$labour_employment$lag <= 0]))]


ccf.labour$labour_unemployment$acf[match(max(abs(ccf.labour$labour_unemployment$acf[ccf.labour$labour_unemployment$lag <= 0])), 
                                         abs(ccf.labour$labour_unemployment$acf[ccf.labour$labour_unemployment$lag <= 0]))]
ccf.labour$labour_employment$acf[match(max(abs(ccf.labour$labour_employment$acf[ccf.labour$labour_employment$lag <= 0])), 
                                       abs(ccf.labour$labour_employment$acf[ccf.labour$labour_employment$lag <= 0]))]




### Compute correlation between cases and retail ###
ccf.retail.1 = cor(retail.trade$VALUE, cases.full.imp$numtoday)
ccf.retail.2 = cor(retail.electronic$VALUE, cases.full.imp$numtoday)
ccf.retail.3 = cor(retail.ecommerce$VALUE, cases.full.imp$numtoday)

ccf.retail.1
ccf.retail.2
ccf.retail.3





##### Modelling ##### 

### Model search queries and case counts ###

# Method 1
# multiply each query popularity time series by its computed correlation with cases
queries_weighted = data.frame("Day" = queries$Day, 
                              sweep(queries[,2:ncol(queries)], 2, ccf.queries.lag0, FUN = "*"))


# to facilitate modelling, order the columns in <queries> from most to least correlated to cases
queries_weighted = data.frame("numtoday" = cases.full.imp$numtoday, 
                              queries_weighted[,c("Day",names(sort(ccf.queries.lag0, decreasing = T)))])

# normalize data again after multiplying by correlation
queries_weighted[,3:ncol(queries_weighted)] = sapply(queries_weighted[,3:ncol(queries_weighted)], minmax_norm)


# split into training and testing subsets
queries_weighted_train = queries_weighted[1:round(0.8*nrow(queries_weighted)),]
queries_weighted_test = queries_weighted[(round(0.8*nrow(queries_weighted))+1):nrow(queries_weighted),]


# add queries one at a time to index that is used as a predictor in the negative binomial model

index = c()
nb_mods = list()

for (query in colnames(queries_weighted)[3:ncol(queries_weighted)]){
  # add one query at a time
  index = index + queries_weighted[,query]
  # fit negative binomial model with the index as the predictor
  nb_mods[[query]] = glm(reformulate(query, "numtoday"), data = queries_weighted_train, 
                         family = quasipoisson)
}


# Use cross validation to find the optimal number of predictors in the index
nb_mods_cv = lapply(nb_mods, cv.glm, data = queries_weighted_train, K = 5)

# function to extract adjusted cv error
get_adj_cv_error = function(cvmod) {
  # return the adjusted cross validation estimate of prediction error
  return(cvmod$delta[2])
}

# get adjusted cv error from each model
nb_cv_error = lapply(nb_mods_cv, get_adj_cv_error)
nb_cv_error = unlist(nb_cv_error, use.names = T)
sort(nb_cv_error)

# show up to which query the index should include for the lowest prediction error
which.min(nb_cv_error)




# set index as the sum of queries up to <query_masque> 

index = rowSums(queries_weighted[,3:(which.min(nb_cv_error)+2)])

# add it to data frame as a var
queries_weighted$index = index

# check lag at which composite index is most correlated with cases
ccf.q = ccf(index, queries_weighted$numtoday, lag.max = 30, plot = FALSE)
ccf.q$lag[match(max(ccf.q$acf[ccf.q$lag <= 0]), ccf.q$acf[ccf.q$lag <= 0])] # max correlation
ccf.q$lag[ccf.q$lag <= 0 & ccf.q$acf >= 0.7]  # correlations above 0.7

# Note: correlations > 0.7 are at lags 0 to 16. Highest correlation is at lag 0.
# Test lags 0 to 16 as predictors.



# check ACF of queries. Lags with highest correlation can be useful autoregressive predictors.
acf.q = acf(queries_weighted$numtoday, lag.max = 30, plot = FALSE)
acf.q[acf.q$lag > 0][match(max(acf.q$acf[acf.q$lag > 0]), acf.q$acf[acf.q$lag > 0])] # max autocorrelation that is not at lag 0
acf.q$lag[acf.q$acf >= 0.7 & acf.q$lag > 0] # autocorrelations > 0.7

# Note: Max autocorrelation is at lag 1 (then lag 3, then lag 2).
# Autocorrelations >= 0.7 are from lags 0 to 16. Include AR terms at lags 1 to 16.  


# set up lagged AR counts for the cases from lags 1 to 16
n = paste0(rep("numtoday_lag", length(acf.q$lag[acf.q$acf >= 0.7 & acf.q$lag > 0])), acf.q$lag[acf.q$acf >= 0.7 & acf.q$lag > 0])
for (lag in acf.q$lag[acf.q$acf >= 0.7 & acf.q$lag > 0]) {
    queries_weighted = slide(queries_weighted, Var = "numtoday", 
                             NewVar = n[match(lag, acf.q$lag[acf.q$acf >= 0.7 & acf.q$lag > 0])], 
                             slideBy = -lag)
}



# set up lagged counts for the index
n.ind = paste0(rep("index", length(ccf.q$lag[ccf.q$lag <= 0 & ccf.q$acf >= 0.7])), 
               rep("_lag", length(ccf.q$lag[ccf.q$lag <= 0 & ccf.q$acf >= 0.7])), 
               str_replace(as.character(sort(ccf.q$lag[ccf.q$lag <= 0 & ccf.q$acf >= 0.7], decreasing = T)), "-", ""))

for (lag in sort(ccf.q$lag[ccf.q$lag <= 0 & ccf.q$acf >= 0.7], decreasing = T)) {
    queries_weighted = slide(queries_weighted, Var = "index", 
                             NewVar = n.ind[abs(lag)+1], 
                             slideBy = lag)
}



# make the next set of training and testing sets before the next set of models
queries_weighted_train = queries_weighted[1:round(0.8*nrow(queries_weighted)),]
queries_weighted_test = queries_weighted[(round(0.8*nrow(queries_weighted))+1):nrow(queries_weighted),]


# put lagged index and cases variables into a model
# cases: use lags 1-16
# index: use lags 0-17



# make models with one index lag at a time (index lags 0 to 17)

mods_q_weighted_singleindex = list()
cvs_q_weighted_singleindex = list() # to store cv results for each model
cvs_q_weighted_singleindex_adjerror = list() # to store adjusted cv error for each model

for (name in colnames(queries_weighted)[grep("index_lag", colnames(queries_weighted))]){
  mod = glm(reformulate(name, response = "numtoday"), 
            data = queries_weighted_train[complete.cases(queries_weighted_train),c("numtoday", name)],
            family = quasipoisson)
  
  mods_q_weighted_singleindex[[name]] = mod
  
  cv_mod = cv.glm(data = queries_weighted_train[complete.cases(queries_weighted_train),c("numtoday", name)], 
                                              glmfit = mod, K = 5)
  
  cvs_q_weighted_singleindex[[name]] = cv_mod
  cvs_q_weighted_singleindex_adjerror[[name]] = get_adj_cv_error(cv_mod)
}

# get adjusted cv error for all models
cvs_q_weighted_singleindex_adjerror = unlist(cvs_q_weighted_singleindex_adjerror, use.names = T)

# show which index lag produces the lowest cv error
which.min(cvs_q_weighted_singleindex_adjerror)

# Note: index_lag15 has lowest cv error. 


# make models with one more index lag added at a time
cvs_q_weighted_multindex = list()
cvs_q_weighted_multindex_adjerror = list()
pred = c()
weighted_multindex_mods = list()

for (name in colnames(queries_weighted)[grep("index_lag", colnames(queries_weighted))]) {
  pred = c(pred, name)
  weighted_multindex_mods[[str_c(pred, collapse = " + ")]] = glm(reformulate(pred, response = "numtoday"),
            data = queries_weighted_train[complete.cases(queries_weighted_train),c("numtoday", pred)],
            family = quasipoisson)

  cv_mod = cv.glm(data = queries_weighted_train[complete.cases(queries_weighted_train),c("numtoday", pred)],
                  glmfit = weighted_multindex_mods[[str_c(pred, collapse = " + ")]], K = 5)

  cvs_q_weighted_multindex[[str_c(pred, collapse = " + ")]] = cv_mod
  cvs_q_weighted_multindex_adjerror[[str_c(pred, collapse = " + ")]] = get_adj_cv_error(cv_mod)
}

# get adjusted cv error for all models
cvs_q_weighted_multindex_adjerror = unlist(cvs_q_weighted_multindex_adjerror, use.names = T)

# show which index lag produces the lowest cv error
which.min(cvs_q_weighted_multindex_adjerror)


# compare the two models with the lowest cvs above
sort(c("single index" = min(cvs_q_weighted_singleindex_adjerror),
       "multi index" = min(cvs_q_weighted_multindex_adjerror)))

# Note: the multiple index model produces the lowest cv error. However
# the multiple index model with the lowest error requires most index predictors,
# which leads to overfitting. To avoid overfitting, use the single index predictor models
# with the lowest cv error.


# find the variables used in the single index model with the lowest cv
names(cvs_q_weighted_singleindex_adjerror)[which.min(cvs_q_weighted_singleindex_adjerror)]



# Using the predictors from the single index model with the lowest cv error above, add the autoregressive 
# daily cases predictors to see if the models make better predictions.

cvs_q_weighted_indexar = list()
cvs_q_weighted_indexar_adjerror = list()
mods_q_weighted_indexar = list()

# save predictor that produces smallest cv error in models above
preds = str_split(names(cvs_q_weighted_singleindex_adjerror)[which.min(cvs_q_weighted_singleindex_adjerror)], pattern = "[+]")
preds = unlist(preds)
preds = str_trim(preds)

for (name in colnames(queries_weighted_train)[grep("numtoday_lag", colnames(queries_weighted_train))]) {
  mod = glm(reformulate(c(preds, name), "numtoday"), 
    data = na.omit(queries_weighted_train), family = quasipoisson)
  
  mods_q_weighted_indexar[[name]] = mod
  
  cv_mod = cv.glm(data = queries_weighted_train[complete.cases(queries_weighted_train),c("numtoday", preds, name)], 
                  glmfit = mod, K = 5)
  
  cvs_q_weighted_indexar[[name]] = cv_mod
  cvs_q_weighted_indexar_adjerror[[name]] = get_adj_cv_error(cv_mod) 
  
}

cvs_q_weighted_indexar_adjerror = unlist(cvs_q_weighted_indexar_adjerror)

# find min cv error from the models above
which.min(cvs_q_weighted_indexar_adjerror)


# Result: the lowest error comes from including the AR term at lag 13.

# compare the cv errors from the multi index model without AR term and with AR term
sort(c("with AR" = min(cvs_q_weighted_indexar_adjerror), 
       "without AR" = min(cvs_q_weighted_singleindex_adjerror)))

# Result: model performs better during cross validation with the AR term.





# Method 2a
# (adapted from the paper named: using internet searches for influenza surveillance)
# combine search query numbers (unweighted by correlation to daily cases) into an 
# index value and use the index value as a predictor.
# sum popularity of all influenza searches (weighted and unweighted)
# weighted: queries_weighted
# unweighted: queries


queries_unweighted = queries
queries_unweighted$numtoday = cases.full.imp$numtoday
queries_unweighted$index = minmax_norm(rowSums(queries[,2:ncol(queries)])) # sum queries to make index

# add lags to queries_unweighted
n = paste0(rep("index", 30), rep("_lag", 30), as.character(seq(1, 30, 1)))
for (i in seq(1, 30, by = 1)){
  queries_unweighted = slide(data = queries_unweighted, Var = "index", NewVar = n[i], slideBy = -i)
}


queries_unweighted_train = queries_unweighted[1:round(0.8*nrow(queries_unweighted)),]
queries_unweighted_test = queries_unweighted[(round(0.8*nrow(queries_unweighted))+1):nrow(queries_unweighted),]


# list for negbin objects
nbs = list()
nbs_cv = list()
nbs_cv_adjerror = list()

# fit negbin models with lags up to 30
for (pred in colnames(queries_unweighted)[49:length(colnames(queries_unweighted))]){
  
  nbs[[pred]] = glm(reformulate(pred, response = "numtoday"), 
           data = queries_unweighted_train[complete.cases(queries_unweighted_train),c("numtoday", "Day", pred)])
  
  cv_mod = cv.glm(data = na.omit(queries_unweighted_train[complete.cases(queries_unweighted_train),c("numtoday", "Day", pred)]), 
                  glmfit = nbs[[pred]], K = 5) 
  
  nbs_cv[[pred]] = cv_mod
  nbs_cv_adjerror[[pred]] = get_adj_cv_error(cv_mod)
  
}

nbs_cv_adjerror = unlist(nbs_cv_adjerror)
nbs_cv_adjerror

# use cross validation to find best model
which.min(nbs_cv_adjerror)
min(nbs_cv_adjerror)

# Result: index lag 28 predictor has the lowest cv error.




# Method 2b
# repeat the above modelling iterations using weighted queries in the index
# the weight for each query is its correlation to the cases at each lag.

queries_weighted_2 = queries
queries_weighted_2$numtoday = queries_weighted$numtoday

# at each iteration of the loop, make a new index predictor that is the rowSums of the weighted queries
for (lag in 0:30) {
  
  # add every query at the lag above 
  for (query in colnames(queries)[2:ncol(queries)]) {
    
    queries_weighted_2 = slide(data = queries_weighted_2, Var = query, 
                               NewVar = paste0(query, "_lag", lag), slideBy = -lag)
    
  }
  
  # get correlation between all queries and <numtoday> at that lag
  cors = lapply(queries_weighted_2[,grepl(paste0("_lag", lag), colnames(queries_weighted_2))], 
                cor, y = queries_weighted_2$numtoday, use = "complete.obs")
  cors = unlist(cors)
  
  # multiply each query column at the lag by its correlation to daily cases
  sep_q = sweep(queries_weighted_2[,grepl(paste0("_lag", lag), colnames(queries_weighted_2))], 2, cors, FUN = "*")
  
  # sum across rows into one column and add it to dataset
  queries_weighted_2[,c(paste0("index_lag", lag))] = rowSums(sep_q)
  
}


# apply min max normalization to all indices
for (colname in colnames(queries_weighted_2)[grep("index", colnames(queries_weighted_2))]) {
  
  queries_weighted_2[!is.na(queries_weighted_2[,colname]),colname] = 
    minmax_norm(queries_weighted_2[!is.na(queries_weighted_2[,colname]),colname])
  
}



queries_weighted_2_train = queries_weighted_2[1:round(0.8*nrow(queries_weighted_2)),]
queries_weighted_2_test = queries_weighted_2[(round(0.8*nrow(queries_weighted_2))+1):nrow(queries_weighted_2),]



# make negbin models using the <index> variable at each lag
nbs_2 = list()
cv_nbs_2 = list()
cv_nbs_2_adjerror = list()

for (pred in colnames(queries_weighted_2)[grep("index", colnames(queries_weighted_2))]) {
  
  nbs_2[[pred]] = glm(reformulate(pred, response = "numtoday"), 
                      data = queries_weighted_2_train[complete.cases(queries_weighted_2_train),c("Day", "numtoday", pred)],
                      family = quasipoisson, control = glm.control(maxit = 500))
  
  cv_mod = cv.glm(data = queries_weighted_2_train[complete.cases(queries_weighted_2_train),c("Day", "numtoday", pred)], 
                  glmfit = nbs_2[[pred]], K = 5)
  
  cv_nbs_2[[pred]] = cv_mod
  cv_nbs_2_adjerror[[pred]] = get_adj_cv_error(cv_mod)
  
}

cv_nbs_2_adjerror = unlist(cv_nbs_2_adjerror)

# find index which produces minimum cv error
which.min(cv_nbs_2_adjerror)
min(cv_nbs_2_adjerror)

# Result: index lag 29 produces smallest cv error.



# Compare models from method 1, method 2a, and method 2b
# make sure that the number of fitted values across which cv error was calculated
# is the same across the three different kinds of models.

# check that the three models start at the same index, so that the cv errors are 
# a fair comparison
names(mods_q_weighted_indexar[[which.min(cvs_q_weighted_indexar_adjerror)]]$fitted.values[1]) # method 1
names(nbs[[which.min(nbs_cv_adjerror)]]$fitted.values[1]) # method 2a
names(nbs_2[[which.min(cv_nbs_2_adjerror)]]$fitted.values[1]) # method 2b

# Result: method 1 fitted values start at 18, while method 2a and 2b values start at 31. 

# redo cv on the method 1 model and corresponding dataset to get a cv error for a
# dataset the same size as the dataset used in method 2.
method_1_smaller_cv = cv.glm(glmfit = mods_q_weighted_indexar[[which.min(cvs_q_weighted_indexar_adjerror)]],
                             data = queries_weighted_train[31:nrow(queries_weighted_train),], K = 5)

# note: ignore warning message above.

# min(cvs_q_weighted_indexar_adjerror)
sort(c("method 1" = method_1_smaller_cv$delta[2], 
       "method 2a" = min(nbs_cv_adjerror), "method 2b" = min(cv_nbs_2_adjerror)))

# Result: method 2a produces smallest cv error.


# plot fitted training values made by all three above 
plot(1:(round(nrow(cases.full.imp)*0.8)), 
     cases.full.imp[1:(round(nrow(cases.full.imp)*0.8)),]$numtoday, 
     type = "l", main = "Query Model Fits to Training Data", 
     xlab = "", ylab = "Cases", xaxt = "n",
     ylim = c(0, 3500), lwd = 1) # actual training data

axis(1, at = 1:(round(nrow(cases.full.imp)*0.8)), 
     labels = cases.full.imp[1:(round(nrow(cases.full.imp)*0.8)),]$date, las=3)

points(31:(round(nrow(cases.full.imp)*0.8)), 
       nbs_2[[which.min(cv_nbs_2_adjerror)]]$fitted.values, type = "l", col = 2) # method 2b pred
points(31:(round(nrow(cases.full.imp)*0.8)), 
       nbs[[which.min(nbs_cv_adjerror)]]$fitted.values, type = "l", col = 3) # method 2a pred
points(17:(round(nrow(cases.full.imp)*0.8)), 
       mods_q_weighted_indexar[[which.min(cvs_q_weighted_indexar_adjerror)]]$fitted.values, 
       type = "l", col = 4) # method 2b pred
legend(5, 2500, legend = c("actual", "method 1", "method 2a", "method 2b"), col = c(1, 2, 3, 4), 
       pch = 20, cex = 0.8, pt.cex = 1.8)






# Method 3
# Note: Method 2a produced the best estimates. Add bing data to see if that improves prediction.


# variable names used in Method 2a model
m = nbs[[which.min(nbs_cv_adjerror)]] # save model to extract relevant predictor names
varnames_lowestcv = names(m$coefficients)[2:length(names(m$coefficients))]



queries_method_3 = cbind(pop_by_date_imp, 
                         queries_unweighted[,c("numtoday", "Day", str_trim(varnames_lowestcv))],
                         "monday" = cases.full.imp$monday, 
                         "holiday" = cases.full.imp$holiday)




# chech which <popularity> and <cumul_pop> lags are most highly correlated with daily cases 
ccf.pop = lapply(queries_method_3[,c("popularity", "cumul_pop")], ccf, 
                 y = queries_method_3[,"numtoday"], lag.max = 30, plot = FALSE)

ccf.pop$popularity$lag[match(max(abs(ccf.pop$popularity$acf[ccf.pop$popularity$lag <= 0])), 
                             abs(ccf.pop$popularity$acf[ccf.pop$popularity$lag <= 0]))]
ccf.pop$cumul_pop$lag[match(max(abs(ccf.pop$cumul_pop$acf[ccf.pop$cumul_pop$lag <= 0])), 
                             abs(ccf.pop$cumul_pop$acf[ccf.pop$cumul_pop$lag <= 0]))]

# Note: <popularity> at lag 14 and <cumul_pop> at lag 6 are most 
# highly correlated with daily cases.




# add lags 0 to 21 for <popularity> and <cumul_pop> to the datasets
pop.lag.names = paste0(rep("popularity_lag", 22), 0:21)
cumpop.lag.names = paste0(rep("cumul_pop_lag", 22), 0:21)

# add lagged popularity variables to dataset
for (lag in 0:21) {
  queries_method_3 = slide(queries_method_3, Var = "popularity", NewVar = pop.lag.names[lag+1], slideBy = -lag)
  queries_method_3 = slide(queries_method_3, Var = "cumul_pop", NewVar = cumpop.lag.names[lag+1], slideBy = -lag)
}


queries_method_3_train = queries_method_3[1:round(0.8*nrow(queries_method_3)),]
queries_method_3_test = queries_method_3[(round(0.8*nrow(queries_method_3))+1):nrow(queries_method_3),]




# use the same predictor combination as in method 1 with different combinations 
# of index, week, popularity and cumul popularity vars for each lag
nbs.3.1 = list()
nbs.3.2 = list()
nbs.3.3 = list()
nbs.3.4 = list()
nbs.3.5 = list()
cv.nbs.3.1 = list()
cv.nbs.3.2 = list()
cv.nbs.3.3 = list()
cv.nbs.3.4 = list()
cv.nbs.3.5 = list()
cv.adjerr.nbs.3.1 = list()
cv.adjerr.nbs.3.2 = list()
cv.adjerr.nbs.3.3 = list()
cv.adjerr.nbs.3.4 = list()
cv.adjerr.nbs.3.5 = list()



for (lag in 0:21) {
  
  # Model type 1
  # with index lags, monday, holiday, popularity, cumulative popularity
  mod_type1 = glm(reformulate(termlabels = c(varnames_lowestcv, pop.lag.names[lag+1], 
                                             cumpop.lag.names[lag+1], "monday", "holiday"), 
                              response = "numtoday"), data = na.omit(queries_method_3_train), 
                              family = quasipoisson)
  
  # store model
  nbs.3.1[[paste0("lag",lag,"_type1")]] = mod_type1

  # conduct cross validation
  cv_mod_type1 = cv.glm(data = queries_method_3_train[complete.cases(queries_method_3_train), c("numtoday", 
                                                                                                varnames_lowestcv,
                                                                                                 pop.lag.names[lag+1], 
                                                                                                 cumpop.lag.names[lag+1], 
                                                                                                 "monday", "holiday")], 
                        glmfit = mod_type1, K = 5)

  # store cv
  cv.nbs.3.1[[paste0("lag",lag,"_type1")]] = cv_mod_type1  
  
  # get adjusted cv error
  cv.adjerr.nbs.3.1[[paste0("lag",lag,"_type1")]] = get_adj_cv_error(cv_mod_type1)
  
  
  
  # Model type 2
  # with index lags, monday, holiday, popularity
  mod_type2 = glm(reformulate(termlabels = c(varnames_lowestcv, pop.lag.names[lag+1],
                                             "monday", "holiday"),
                              response = "numtoday"),
                  data = na.omit(queries_method_3_train),
                  family = quasipoisson)

  nbs.3.2[[paste0("lag",lag,"_type2")]] = mod_type2

  cv_mod_type2 = cv.glm(data = queries_method_3_train[complete.cases(queries_method_3_train),c("numtoday", varnames_lowestcv,
                                                                 pop.lag.names[lag+1],
                                                                 "monday", "holiday")],
                        glmfit = mod_type2, K = 5)

  cv.nbs.3.2[[paste0("lag",lag,"_type2")]] = cv_mod_type2
  cv.adjerr.nbs.3.2[[paste0("lag",lag,"_type2")]] = get_adj_cv_error(cv_mod_type2)



  # Model type 3
  # with index lags, monday, holiday, cumulative pop
  mod_type3 = glm(reformulate(termlabels = c(varnames_lowestcv, cumpop.lag.names[lag+1],
                                             "monday", "holiday"),
                              response = "numtoday"),
                  data = na.omit(queries_method_3_train),
                  family = quasipoisson)

  nbs.3.3[[paste0("lag",lag,"_type3")]] = mod_type3

  cv_mod_type3 = cv.glm(data = queries_method_3_train[complete.cases(queries_method_3_train),c("numtoday", varnames_lowestcv,
                                                                 cumpop.lag.names[lag+1],
                                                                 "monday", "holiday")],
                        glmfit = mod_type3, K = 5)

  cv.nbs.3.3[[paste0("lag",lag,"_type3")]] = cv_mod_type3

  cv.adjerr.nbs.3.3[[paste0("lag",lag,"_type3")]] = get_adj_cv_error(cv_mod_type3)



  # Model type 4
  # with index lags and popularity
  mod_type4 = glm(reformulate(termlabels = c(varnames_lowestcv, pop.lag.names[lag+1]),
                                                          response = "numtoday"),
                                              data = na.omit(queries_method_3_train),
                                              family = quasipoisson)

  nbs.3.4[[paste0("lag",lag,"_type4")]] = mod_type4

  cv_mod_type4 = cv.glm(data = queries_method_3_train[complete.cases(queries_method_3_train),c("numtoday", varnames_lowestcv,
                                                                 pop.lag.names[lag+1])],
                        glmfit = mod_type4, K = 5)

  cv.nbs.3.4[[paste0("lag",lag,"_type4")]] = cv_mod_type4

  cv.adjerr.nbs.3.4[[paste0("lag",lag,"_type4")]] = get_adj_cv_error(cv_mod_type4)


  # Model type 5
  # with index lags and cumulative popularity
  mod_type5 = glm(reformulate(termlabels = c(varnames_lowestcv, cumpop.lag.names[lag+1]),
                              response = "numtoday"),
                  data = na.omit(queries_method_3_train),
                  family = quasipoisson)


  nbs.3.5[[paste0("lag",lag,"_type5")]] = mod_type5

  cv_mod_type5 = cv.glm(data = queries_method_3_train[complete.cases(queries_method_3_train),c("numtoday", varnames_lowestcv,
                                                                  cumpop.lag.names[lag+1])],
                        glmfit = mod_type5, K = 5)

  cv.nbs.3.5[[paste0("lag",lag,"_type5")]] = cv_mod_type5

  cv.adjerr.nbs.3.5[[paste0("lag",lag,"_type5")]] = get_adj_cv_error(cv_mod_type5)

}


# Note: model with <index> and <week> as predictors was not included because index and week variables do not change
# across the lags.


# get models with lowest cv error from each model type above
cv.adjerr.nbs.3.1 = unlist(cv.adjerr.nbs.3.1)
cv.adjerr.nbs.3.2 = unlist(cv.adjerr.nbs.3.2)
cv.adjerr.nbs.3.3 = unlist(cv.adjerr.nbs.3.3)
cv.adjerr.nbs.3.4 = unlist(cv.adjerr.nbs.3.4)
cv.adjerr.nbs.3.5 = unlist(cv.adjerr.nbs.3.5)

# "lag" in which.min below refers to the lag of the <popularity> and/or
# <cumul_pop> predictors used in the models.

which.min(cv.adjerr.nbs.3.1)
which.min(cv.adjerr.nbs.3.2)
which.min(cv.adjerr.nbs.3.3)
which.min(cv.adjerr.nbs.3.4)
which.min(cv.adjerr.nbs.3.5)

min(cv.adjerr.nbs.3.1)
min(cv.adjerr.nbs.3.2)
min(cv.adjerr.nbs.3.3)
min(cv.adjerr.nbs.3.4)
min(cv.adjerr.nbs.3.5)

# due to lags in predictors, not all fitted value vectors have the same length.
# compare the cv errors across the fitted values with the same length



# Compare method 3 models to original method 2a model
sort(c("mod_type1" = min(cv.adjerr.nbs.3.1), "mod_type2" = min(cv.adjerr.nbs.3.2), 
       "mod_type3" = min(cv.adjerr.nbs.3.3), "mod_type4" = min(cv.adjerr.nbs.3.4), 
       "mod_type5" = min(cv.adjerr.nbs.3.5), "method 2a" = min(nbs_cv_adjerror)))

# Result: the original method 2a model produces the lowest adj cv error. 




# plot fitted values from the 5 model types and original method 2a model
plot(1:(round(nrow(cases.full.imp)*0.8)), 
     cases.full.imp[1:(round(nrow(cases.full.imp)*0.8)),]$numtoday, 
     type="l", main = "Models 1-5 and Method 2a Comparisons", xlab = "", ylab = "Cases", xaxt = "n",
     ylim = c(0, 3400)) # actual
axis(1, at = 1:(round(nrow(cases.full.imp)*0.8)), 
     labels = cases.full.imp[1:(round(nrow(cases.full.imp)*0.8)),]$date, las=3)

points(31:(round(nrow(cases.full.imp)*0.8)), nbs[[which.min(nbs_cv_adjerror)]]$fitted.values, 
       type = "l", col = 2)
points(29:(round(nrow(cases.full.imp)*0.8)), nbs.3.1[[which.min(cv.adjerr.nbs.3.1)]]$fitted.values,
       type = "l", col = 3)
points(29:(round(nrow(cases.full.imp)*0.8)), nbs.3.2[[which.min(cv.adjerr.nbs.3.2)]]$fitted.values, 
       type = "l", col = 4)
points(29:(round(nrow(cases.full.imp)*0.8)), nbs.3.3[[which.min(cv.adjerr.nbs.3.3)]]$fitted.values, 
       type = "l", col = 5)
points(29:(round(nrow(cases.full.imp)*0.8)), nbs.3.4[[which.min(cv.adjerr.nbs.3.4)]]$fitted.values, 
       type = "l", col = 6)
points(29:(round(nrow(cases.full.imp)*0.8)), nbs.3.5[[which.min(cv.adjerr.nbs.3.5)]]$fitted.values, 
       type = "l", col = 7)
legend(5, 2500, 
       legend = c("actual", "method 2a", "model 1", "model 2", "model 3", "model 4", "model 5"), 
       col = c(1, 2, 3, 4, 5, 6, 7), 
       pch = 20, cex = 0.8, pt.cex = 1.8)





### Model social distancing patterns and case counts ###

# Using Google's data as predictors

# set training and testing datasets
mobility.g.t = cbind(mobility.g.aggregate.full.imp, "numtoday" = cases.full.imp$numtoday)
mobility.g.t$country_region = NULL

# add lags for each variable
for (lag in 0:30) {
  mobility.g.t = slide(mobility.g.t, Var = "retail_and_recreation_percent_change_from_baseline",
                       NewVar = paste0("retail_and_recreation_percent_change_from_baseline_lag", lag),
                       slideBy = -lag)
  mobility.g.t = slide(mobility.g.t, Var = "grocery_and_pharmacy_percent_change_from_baseline",
                       NewVar = paste0("grocery_and_pharmacy_percent_change_from_baseline_lag", lag),
                       slideBy = -lag)
  mobility.g.t = slide(mobility.g.t, Var = "parks_percent_change_from_baseline",
                       NewVar = paste0("parks_percent_change_from_baseline_lag", lag),
                       slideBy = -lag)
  mobility.g.t = slide(mobility.g.t, Var = "transit_stations_percent_change_from_baseline",
                       NewVar = paste0("transit_stations_percent_change_from_baseline_lag", lag),
                       slideBy = -lag)
  mobility.g.t = slide(mobility.g.t, Var = "workplaces_percent_change_from_baseline",
                       NewVar = paste0("workplaces_percent_change_from_baseline_lag", lag),
                       slideBy = -lag)
  mobility.g.t = slide(mobility.g.t, Var = "residential_percent_change_from_baseline",
                       NewVar = paste0("residential_percent_change_from_baseline_lag", lag),
                       slideBy = -lag)
}

# separate into training and testing datasets
mobility.g.train = mobility.g.t[1:round(0.8*nrow(mobility.g.t)),]
mobility.g.test = mobility.g.t[(round(0.8*nrow(mobility.g.t))+1):nrow(mobility.g.t),]
  


# make models with single variables to check which lags are most predictive
nbs.g.retail = list()
nbs.g.grocery = list()
nbs.g.park = list()
nbs.g.transit = list()
nbs.g.work = list()
nbs.g.res = list()
cv.nbs.g.retail = list()
cv.nbs.g.grocery = list()
cv.nbs.g.park = list()
cv.nbs.g.transit = list()
cv.nbs.g.work = list()
cv.nbs.g.res = list()
cv.adjerr.nbs.g.retail = list()
cv.adjerr.nbs.g.grocery = list()
cv.adjerr.nbs.g.park = list()
cv.adjerr.nbs.g.transit = list()
cv.adjerr.nbs.g.work = list()
cv.adjerr.nbs.g.res = list()

for (lag in 0:30) {
  
  # model using retail and recreation variable at given lag as predictor
  mod = 
    glm(reformulate(colnames(mobility.g.train)[match(paste0("retail_and_recreation_percent_change_from_baseline_lag", lag), 
                                                     colnames(mobility.g.train))], response = "numtoday"), 
        data = mobility.g.train[complete.cases(mobility.g.train),], family = quasipoisson)
  
  cv_mod = cv.glm(data = mobility.g.train[complete.cases(mobility.g.train),c("numtoday", colnames(mobility.g.train)[match(paste0("retail_and_recreation_percent_change_from_baseline_lag", lag), 
                                                                                                                          colnames(mobility.g.train))])],
                  glmfit = mod, K = 5)
  
  nbs.g.retail[[paste0("retail_rec_lag", lag)]] = mod
  
  cv.nbs.g.retail[[paste0("retail_rec_lag", lag)]] = cv_mod
  cv.adjerr.nbs.g.retail[[paste0("retail_rec_lag", lag)]] = get_adj_cv_error(cv_mod)
  
  
  # model using grocery and pharmacy variable at given lag as predictor
  mod = 
    glm(reformulate(colnames(mobility.g.train)[match(paste0("grocery_and_pharmacy_percent_change_from_baseline_lag", lag), 
                                                     colnames(mobility.g.train))], response = "numtoday"), 
        data = mobility.g.train[complete.cases(mobility.g.train),], family = quasipoisson)
  
  cv_mod = cv.glm(data = mobility.g.train[complete.cases(mobility.g.train),c("numtoday", colnames(mobility.g.train)[match(paste0("grocery_and_pharmacy_percent_change_from_baseline_lag", lag), 
                                                                                                                          colnames(mobility.g.train))])],
                  glmfit = mod, K = 5)
  
  nbs.g.grocery[[paste0("grocery_pharm_lag", lag)]] = mod
  cv.nbs.g.grocery[[paste0("grocery_pharm_lag", lag)]] = cv_mod
  cv.adjerr.nbs.g.grocery[[paste0("grocery_pharm_lag", lag)]] = get_adj_cv_error(cv_mod)
  
  
  # model using parks variable at given lag as predictor
  mod = 
    glm(reformulate(colnames(mobility.g.train)[match(paste0("parks_percent_change_from_baseline_lag", lag), 
                                                     colnames(mobility.g.train))], response = "numtoday"), 
        data = mobility.g.train[complete.cases(mobility.g.train),], family = quasipoisson)
  
   cv_mod = cv.glm(data = mobility.g.train[complete.cases(mobility.g.train),c("numtoday", colnames(mobility.g.train)[match(paste0("parks_percent_change_from_baseline_lag", lag), 
                                                                                                                          colnames(mobility.g.train))])],
                  glmfit = mod, K = 5)
  
  nbs.g.park[[paste0("parks_lag", lag)]] = mod
  cv.nbs.g.park[[paste0("parks_lag", lag)]] = cv_mod
  cv.adjerr.nbs.g.park[[paste0("parks_lag", lag)]] = get_adj_cv_error(cv_mod)
  
  
  # model using transit variable at given lag as predictor
  mod = 
    glm(reformulate(colnames(mobility.g.train)[match(paste0("transit_stations_percent_change_from_baseline_lag", lag), 
                                                     colnames(mobility.g.train))], response = "numtoday"), 
        data = mobility.g.train[complete.cases(mobility.g.train),], family = quasipoisson)
  
  cv_mod = cv.glm(data = mobility.g.train[complete.cases(mobility.g.train),c("numtoday", colnames(mobility.g.train)[match(paste0("transit_stations_percent_change_from_baseline_lag", lag), 
                                                                                                                          colnames(mobility.g.train))])],
                  glmfit = mod, K = 5)
  
  nbs.g.transit[[paste0("transit_lag", lag)]] = mod
  cv.nbs.g.transit[[paste0("transit_lag", lag)]] = cv_mod
  cv.adjerr.nbs.g.transit[[paste0("transit_lag", lag)]] = get_adj_cv_error(cv_mod) 
  
  
  # model using workplaces variable at given lag as predictor
  mod = 
    glm(reformulate(colnames(mobility.g.train)[match(paste0("workplaces_percent_change_from_baseline_lag", lag), 
                                                     colnames(mobility.g.train))], response = "numtoday"), 
        data = mobility.g.train[complete.cases(mobility.g.train),], family = quasipoisson)
  
  cv_mod = cv.glm(data = mobility.g.train[complete.cases(mobility.g.train),c("numtoday", colnames(mobility.g.train)[match(paste0("workplaces_percent_change_from_baseline_lag", lag), 
                                                                                                                          colnames(mobility.g.train))])],
                  glmfit = mod, K = 5)
  
  nbs.g.work[[paste0("work_lag", lag)]] = mod
  cv.nbs.g.work[[paste0("work_lag", lag)]] = cv_mod
  cv.adjerr.nbs.g.work[[paste0("work_lag", lag)]] = get_adj_cv_error(cv_mod) 
  
  
  # model using residential variable at given lag as predictor 
  mod = 
    glm(reformulate(colnames(mobility.g.train)[match(paste0("residential_percent_change_from_baseline_lag", lag), 
                                                     colnames(mobility.g.train))], response = "numtoday"), 
        data = mobility.g.train[complete.cases(mobility.g.train),], family = quasipoisson)
  
  cv_mod = cv.glm(data = mobility.g.train[complete.cases(mobility.g.train),c("numtoday", colnames(mobility.g.train)[match(paste0("residential_percent_change_from_baseline_lag", lag), 
                                                                                                                          colnames(mobility.g.train))])],
                   glmfit = mod, K = 5)
  
  nbs.g.res[[paste0("res_lag", lag)]] = mod
  cv.nbs.g.res[[paste0("res_lag", lag)]] = cv_mod
  cv.adjerr.nbs.g.res[[paste0("res_lag", lag)]] = get_adj_cv_error(cv_mod)
  
  
}

# get adjusted cv error from the separate types of models above
cv.adjerr.nbs.g.retail = unlist(cv.adjerr.nbs.g.retail)
cv.adjerr.nbs.g.grocery = unlist(cv.adjerr.nbs.g.grocery)
cv.adjerr.nbs.g.park = unlist(cv.adjerr.nbs.g.park)
cv.adjerr.nbs.g.transit = unlist(cv.adjerr.nbs.g.transit)
cv.adjerr.nbs.g.work = unlist(cv.adjerr.nbs.g.work)
cv.adjerr.nbs.g.res = unlist(cv.adjerr.nbs.g.res)

which.min(cv.adjerr.nbs.g.retail)
which.min(cv.adjerr.nbs.g.grocery)
which.min(cv.adjerr.nbs.g.park)
which.min(cv.adjerr.nbs.g.transit)
which.min(cv.adjerr.nbs.g.work)
which.min(cv.adjerr.nbs.g.res)

min(cv.adjerr.nbs.g.retail)
min(cv.adjerr.nbs.g.grocery)
min(cv.adjerr.nbs.g.park)
min(cv.adjerr.nbs.g.transit)
min(cv.adjerr.nbs.g.work)
min(cv.adjerr.nbs.g.res)


# compare best predictor for each model type

sort(c("retail" = min(cv.adjerr.nbs.g.retail), "grocery" = min(cv.adjerr.nbs.g.grocery), 
       "park" = min(cv.adjerr.nbs.g.park), "transit" = min(cv.adjerr.nbs.g.transit), 
       "work" = min(cv.adjerr.nbs.g.work), "res" = min(cv.adjerr.nbs.g.res)))

# Result: the model with the transit predictor at lag 10 has the lowest adjusted cv error.


# get best predictor names
best.transit.pred = paste0(substr(names(which.min(cv.adjerr.nbs.g.transit)), 1, 3), ".*" , 
                           substr(names(which.min(cv.adjerr.nbs.g.transit)), nchar(names(which.min(cv.adjerr.nbs.g.transit)))-4, 
                                                    nchar(names(which.min(cv.adjerr.nbs.g.transit)))))
best.retail.pred = paste0(substr(names(which.min(cv.adjerr.nbs.g.retail)), 1, 3), ".*" , 
                          substr(names(which.min(cv.adjerr.nbs.g.retail)), nchar(names(which.min(cv.adjerr.nbs.g.retail)))-4, 
                                 nchar(names(which.min(cv.adjerr.nbs.g.retail)))))
best.res.pred = paste0(substr(names(which.min(cv.adjerr.nbs.g.res)), 1, 3), ".*" , 
                       substr(names(which.min(cv.adjerr.nbs.g.res)), nchar(names(which.min(cv.adjerr.nbs.g.res)))-4, 
                              nchar(names(which.min(cv.adjerr.nbs.g.res)))))
best.work.pred = paste0(substr(names(which.min(cv.adjerr.nbs.g.work)), 1, 3), ".*" , 
                        substr(names(which.min(cv.adjerr.nbs.g.work)), nchar(names(which.min(cv.adjerr.nbs.g.work)))-4, 
                               nchar(names(which.min(cv.adjerr.nbs.g.work)))))
best.grocery.pred = paste0(substr(names(which.min(cv.adjerr.nbs.g.grocery)), 1, 3), ".*" , 
                           substr(names(which.min(cv.adjerr.nbs.g.grocery)), nchar(names(which.min(cv.adjerr.nbs.g.grocery)))-4, 
                                  nchar(names(which.min(cv.adjerr.nbs.g.grocery)))))
best.park.pred = paste0(substr(names(which.min(cv.adjerr.nbs.g.park)), 1, 3), ".*" , 
                        substr(names(which.min(cv.adjerr.nbs.g.park)), nchar(names(which.min(cv.adjerr.nbs.g.park)))-4, 
                               nchar(names(which.min(cv.adjerr.nbs.g.park)))))

best.g.preds = c(colnames(mobility.g.train)[grep(best.transit.pred, colnames(mobility.g.train))],
                 colnames(mobility.g.train)[grep(best.retail.pred, colnames(mobility.g.train))],
                 colnames(mobility.g.train)[grep(best.res.pred, colnames(mobility.g.train))],
                 colnames(mobility.g.train)[grep(best.work.pred, colnames(mobility.g.train))],
                 colnames(mobility.g.train)[grep(best.grocery.pred, colnames(mobility.g.train))],
                 colnames(mobility.g.train)[grep(best.park.pred, colnames(mobility.g.train))])

# put a model together with multiple predictors and compare that to the single-predictor mods

pred = c()
mods.g.all.preds = list()
cv.mods.g.all.preds = list()
cv.adjerr.mods.g.all.preds = list()

for (name in best.g.preds) {
  pred = c(pred, name)
  mod = glm(reformulate(pred, "numtoday"), data = na.omit(mobility.g.train), family = quasipoisson)
  cv_mod = cv.glm(data = mobility.g.train[complete.cases(mobility.g.train), c("numtoday", pred)],
                  glmfit = mod, K = 5)
  
  mods.g.all.preds[[str_c(pred, collapse = " + ")]] = mod
  
  cv.mods.g.all.preds[[str_c(pred, collapse = " + ")]] = cv_mod
  cv.adjerr.mods.g.all.preds[[str_c(pred, collapse = " + ")]] = get_adj_cv_error(cv_mod)  
  
}

cv.adjerr.mods.g.all.preds = unlist(cv.adjerr.mods.g.all.preds)

which.min(cv.adjerr.mods.g.all.preds) 
min(cv.adjerr.mods.g.all.preds)


# compare whether model with multiple preds has lower cv error than models with one fit
# transit predictor produced best model previously
sort(c("multiple preds" = min(cv.adjerr.mods.g.all.preds), "single pred" = min(cv.adjerr.nbs.g.transit)))

# result: multiple predictor model produced the better fit.


# plot the single and multi predictor models
plot(1:(round(nrow(cases.full.imp)*0.8)), 
     cases.full.imp[1:(round(nrow(cases.full.imp)*0.8)),]$numtoday, 
     type="l", main = "G Single and Multi Predictor Models", xlab = "", ylab = "Cases", xaxt = "n",
     ylim = c(0, 3400), lwd = 2) # actual
axis(1, at = 1:(round(nrow(cases.full.imp)*0.8)), 
     labels = cases.full.imp[1:(round(nrow(cases.full.imp)*0.8)),]$date, las=3)

points(31:(round(nrow(cases.full.imp)*0.8)), nbs.g.retail[[which.min(cv.adjerr.nbs.g.res)]]$fitted.values, 
       type = "l", col = 2)
points(31:(round(nrow(cases.full.imp)*0.8)), nbs.g.grocery[[which.min(cv.adjerr.nbs.g.res)]]$fitted.values,
       type = "l", col = 3)
points(31:(round(nrow(cases.full.imp)*0.8)), nbs.g.park[[which.min(cv.adjerr.nbs.g.res)]]$fitted.values, 
       type = "l", col = 4)
points(31:(round(nrow(cases.full.imp)*0.8)), nbs.g.transit[[which.min(cv.adjerr.nbs.g.res)]]$fitted.values, 
       type = "l", col = 5)
points(31:(round(nrow(cases.full.imp)*0.8)), nbs.g.work[[which.min(cv.adjerr.nbs.g.res)]]$fitted.values, 
       type = "l", col = 6)
points(31:(round(nrow(cases.full.imp)*0.8)), nbs.g.res[[which.min(cv.adjerr.nbs.g.res)]]$fitted.values, 
       type = "l", col = 7)
points(31:(round(nrow(cases.full.imp)*0.8)), mods.g.all.preds[[which.min(cv.adjerr.mods.g.all.preds)]]$fitted.values, 
       type = "l", col = 8)
legend(5, 2600, 
       legend = c("actual", "retail", "grocery", "park", "transit", "work", "res", "multi"), 
       col = c(1, 2, 3, 4, 5, 6, 7, 8), 
       pch = 20, cex = 0.8, pt.cex = 1.8)







# Find variables at most relevant lags for Apple data
mobility.a.t = cbind(mobility.a.full.imp, "numtoday" = cases.full.imp$numtoday)

for (lag in 0:30) {
  mobility.a.t = slide(mobility.a.t, Var = "driving", NewVar = paste0("driving_lag", lag), slideBy = -lag)
  mobility.a.t = slide(mobility.a.t, Var = "transit", NewVar = paste0("transit_lag", lag), slideBy = -lag)
  mobility.a.t = slide(mobility.a.t, Var = "walking", NewVar = paste0("walking_lag", lag), slideBy = -lag)
}

# separate into training and testing datasets
mobility.a.train = mobility.a.t[1:round(0.8*nrow(mobility.a.t)),]
mobility.a.test = mobility.a.t[(round(0.8*nrow(mobility.a.full))+1):nrow(mobility.a.full),]


nbs.a.driving = list()
nbs.a.walking = list()
nbs.a.transit = list()
cv.nbs.a.driving = list()
cv.nbs.a.walking = list()
cv.nbs.a.transit = list()
cv.adjerr.nbs.a.driving = list()
cv.adjerr.nbs.a.walking = list()
cv.adjerr.nbs.a.transit = list()


for (lag in 0:30) {
  
  # model using driving predictor
  mod = glm(reformulate(colnames(mobility.a.t)[match(paste0("driving_lag", lag), colnames(mobility.a.t))],
                                                                response = "numtoday"), 
                                                    data = na.omit(mobility.a.train), family = quasipoisson)
  
  cv_mod = cv.glm(data = mobility.a.train[complete.cases(mobility.a.train), 
                                          c("numtoday", colnames(mobility.a.t)[match(paste0("driving_lag", lag), colnames(mobility.a.t))])],
                  glmfit = mod, K = 5)
  
  nbs.a.driving[[paste0("driving_lag", lag)]] = mod
  cv.nbs.a.driving[[paste0("driving_lag", lag)]] = cv_mod
  cv.adjerr.nbs.a.driving[[paste0("driving_lag", lag)]] = get_adj_cv_error(cv_mod) 
  
  
  # model using walking predictor
  mod = glm(reformulate(colnames(mobility.a.t)[match(paste0("walking_lag", lag), colnames(mobility.a.t))],
                                                                response = "numtoday"), 
                                                    data = na.omit(mobility.a.train), family = quasipoisson)
  
  cv_mod = cv.glm(data = mobility.a.train[complete.cases(mobility.a.train), 
                                          c("numtoday", colnames(mobility.a.t)[match(paste0("walking_lag", lag), colnames(mobility.a.t))])],
                  glmfit = mod, K = 5)
  
  nbs.a.walking[[paste0("walking_lag", lag)]] = mod
  cv.nbs.a.walking[[paste0("walking_lag", lag)]] = cv_mod
  cv.adjerr.nbs.a.walking[[paste0("walking_lag", lag)]] = get_adj_cv_error(cv_mod) 
  
  
  # model using transit predictor
  mod = glm(reformulate(colnames(mobility.a.t)[match(paste0("transit_lag", lag), colnames(mobility.a.t))],
                                                                response = "numtoday"), 
                                                    data = na.omit(mobility.a.train), family = quasipoisson)
  
  cv_mod = cv.glm(data = mobility.a.train[complete.cases(mobility.a.train), 
                                          c("numtoday", colnames(mobility.a.t)[match(paste0("transit_lag", lag), colnames(mobility.a.t))])],
                  glmfit = mod, K = 5)
  
  nbs.a.transit[[paste0("transit_lag", lag)]] = mod
  cv.nbs.a.transit[[paste0("transit_lag", lag)]] = cv_mod
  cv.adjerr.nbs.a.transit[[paste0("transit_lag", lag)]] = get_adj_cv_error(cv_mod)
  
}

cv.adjerr.nbs.a.driving = unlist(cv.adjerr.nbs.a.driving)
cv.adjerr.nbs.a.walking = unlist(cv.adjerr.nbs.a.walking)
cv.adjerr.nbs.a.transit = unlist(cv.adjerr.nbs.a.transit)

# see which lags have lowest adjusted cv error across the three model types
which.min(cv.adjerr.nbs.a.driving)
which.min(cv.adjerr.nbs.a.walking)
which.min(cv.adjerr.nbs.a.transit)



# compare the three model types
sort(c("driving" = min(cv.adjerr.nbs.a.driving), 
      "walking" = min(cv.adjerr.nbs.a.walking), 
      "transit" = min(cv.adjerr.nbs.a.transit)))

# Result: the transit model has the lowest adj cv error.

# compare the single predictor models to model with the best single predictors combined
best.a.preds = c(names(which.min(cv.adjerr.nbs.a.transit)), 
                 names(which.min(cv.adjerr.nbs.a.walking)),
                 names(which.min(cv.adjerr.nbs.a.driving)))


pred = c()
mods.a.all.preds = list()
cv.mods.a.all.preds = list()
cv.adjerr.mods.a.all.preds = list()


for (name in best.a.preds) {
  pred = c(pred, name)
  mod = glm(reformulate(pred, "numtoday"), data = mobility.a.train[complete.cases(mobility.a.train), c("numtoday", pred)],
            family = quasipoisson)
  
  cv_mod = cv.glm(data = mobility.a.train[complete.cases(mobility.a.train), c("numtoday", pred)], glmfit = mod, K = 5)
  
  mods.a.all.preds[[str_c(pred, collapse = " + ")]] = mod
  cv.mods.a.all.preds[[str_c(pred, collapse = " + ")]] = cv_mod
  cv.adjerr.mods.a.all.preds[[str_c(pred, collapse = " + ")]] = get_adj_cv_error(cv_mod) 
  
}

cv.adjerr.mods.a.all.preds = unlist(cv.adjerr.mods.a.all.preds)

# see which combination of apple predictors makes lowest adj cv error
which.min(cv.adjerr.mods.a.all.preds)

# compare multiple predictor model to single predictor mods
sort(c("multiple" = min(cv.adjerr.mods.a.all.preds),
       "driving" = min(cv.adjerr.nbs.a.driving), 
       "walking" = min(cv.adjerr.nbs.a.walking), 
       "transit" = min(cv.adjerr.nbs.a.transit)))

# Result: the multiple predictor model has the lowest adj cv error.


# plot single and multi apple predictors
plot(1:(round(nrow(cases.full.imp)*0.8)), 
     cases.full.imp[1:(round(nrow(cases.full.imp)*0.8)),]$numtoday, 
     type="l", main = "A Single and Multi Predictor Models", xlab = "", ylab = "Cases", xaxt = "n",
     ylim = c(0, 3400), lwd = 2) # actual
axis(1, at = 1:(round(nrow(cases.full.imp)*0.8)), 
     labels = cases.full.imp[1:(round(nrow(cases.full.imp)*0.8)),]$date, las=3)

points(33:(round(nrow(cases.full.imp)*0.8)), nbs.a.transit[[which.min(cv.adjerr.nbs.a.transit)]]$fitted.values, 
       type = "l", col = 2)
points(33:(round(nrow(cases.full.imp)*0.8)), nbs.a.walking[[which.min(cv.adjerr.nbs.a.transit)]]$fitted.values,
       type = "l", col = 3)
points(33:(round(nrow(cases.full.imp)*0.8)), nbs.a.driving[[which.min(cv.adjerr.nbs.a.transit)]]$fitted.values, 
       type = "l", col = 4)
points(33:(round(nrow(cases.full.imp)*0.8)), mods.a.all.preds[[which.min(cv.adjerr.mods.a.all.preds)]]$fitted.values, 
       type = "l", col = 5)
legend(5, 2500, 
       legend = c("actual", "transit", "walking", "driving", "multi"), 
       col = c(1, 2, 3, 4, 5), 
       pch = 20, cex = 0.8, pt.cex = 1.8)





# Combine predictors from Google and Apple models 
best.g.a.preds = c(names(which.min(cv.adjerr.mods.a.all.preds)), 
                   names(which.min(cv.adjerr.mods.g.all.preds)))
best.g.a.preds = trim(unlist(str_split(best.g.a.preds, "[+]")))

# model with all predictors combined

mobility.all = cbind(mobility.a.t, mobility.g.t[, -match("numtoday", colnames(mobility.g.t))], 
                     "monday" = cases.full.imp$monday, "holiday" = cases.full.imp$holiday)
mobility.all = mobility.all[,c("numtoday", "monday", "holiday", 
                               trim(unlist(str_split(best.g.a.preds, "[+]"))))]

mobility.all.train = mobility.all[1:round(0.8*nrow(mobility.all)),]
mobility.all.test = mobility.all[(round(0.8*nrow(mobility.all))+1):nrow(mobility.all),]


# all predictors
mobility.mod.1 = glm(reformulate(best.g.a.preds, "numtoday"),
                    data = na.omit(mobility.all.train), family = quasipoisson)
cv.mobility.mod.1 = cv.glm(data = mobility.all.train[complete.cases(mobility.all.train),
                                                    c("numtoday", best.g.a.preds)],
                          glmfit = mobility.mod.1, K = 5)

cv.adjerr.mobility.mod.1 = get_adj_cv_error(cv.mobility.mod.1)  


# all predictors plus monday predictor
mobility.mod.2 = glm(reformulate(c("monday", best.g.a.preds), "numtoday"),
                     data = na.omit(mobility.all.train), family = quasipoisson)
                     
cv.mobility.mod.2 = cv.glm(data = mobility.all.train[complete.cases(mobility.all.train),
                                                    c("numtoday", "monday", best.g.a.preds)],
                          glmfit = mobility.mod.2, K = 5)

cv.adjerr.mobility.mod.2 = get_adj_cv_error(cv.mobility.mod.2)


# all predictors plus holiday predictor
mobility.mod.3 = glm(reformulate(c("holiday", best.g.a.preds), "numtoday"),
                     data = na.omit(mobility.all.train), family = quasipoisson)

cv.mobility.mod.3 = cv.glm(data = mobility.all.train[complete.cases(mobility.all.train),
                                                    c("numtoday", "holiday", best.g.a.preds)],
                          glmfit = mobility.mod.3, K = 5)

cv.adjerr.mobility.mod.3 = get_adj_cv_error(cv.mobility.mod.3)


# all predictors plus monday and holiday
mobility.mod.4 = glm(reformulate(c("holiday", "monday", best.g.a.preds), "numtoday"),
                     data = na.omit(mobility.all.train), family = quasipoisson)

cv.mobility.mod.4 = cv.glm(data = mobility.all.train[complete.cases(mobility.all.train),
                                                    c("numtoday", "monday", "holiday", best.g.a.preds)],
                          glmfit = mobility.mod.4, K = 5)

cv.adjerr.mobility.mod.4 = get_adj_cv_error(cv.mobility.mod.4)

cv.adjerr.mobility.mods = c("mod1" = cv.adjerr.mobility.mod.1, "mod2" = cv.adjerr.mobility.mod.2, 
       "mod3" = cv.adjerr.mobility.mod.3, "mod4" = cv.adjerr.mobility.mod.4)

sort(cv.adjerr.mobility.mods)

# Result: model type 2 has lowest cv adj error. 


# Compare best composite G and A model with separate G and A models.
sort(c(cv.adjerr.mobility.mods, 
       "all_preds_a" = min(cv.adjerr.mods.a.all.preds), 
       "all_preds_g" = min(cv.adjerr.mods.g.all.preds)))

# Result: the composite G and A type 2 model produces the lowest adj cv error with more
# observations than for the separate G and A models.



# plot all predictions
plot(1:nrow(mobility.all.train), mobility.all.train$numtoday, type = "l", xaxt = "n", xlab = "", 
     ylab = "Cases", lwd = 2, main = "Combined G and A Models")
axis(1, at = 1:nrow(mobility.all.train), labels = mobility.g.train$date, las = 3)

# model type 1 (both G and A preds)
points(19:nrow(mobility.all.train), mobility.mod.1$fitted.values, type = "l", col = 2)

# model type 2 (both G and A preds)
points(19:nrow(mobility.all.train), mobility.mod.2$fitted.values, type = "l", col = 3) 

# model type 3 (both G and A preds)
points(19:nrow(mobility.all.train), mobility.mod.3$fitted.values, type = "l", col = 4)

# model type 4 (both G and A preds)
points(19:nrow(mobility.all.train), mobility.mod.4$fitted.values, type = "l", col = 5)

# model best model with only G predictors
points(31:nrow(mobility.all.train), mods.g.all.preds[[which.min(cv.adjerr.mods.g.all.preds)]]$fitted.values, 
       type = "l", col = 6)

# best model with only A predictors
points(33:nrow(mobility.all.train), mods.a.all.preds[[which.min(cv.adjerr.mods.a.all.preds)]]$fitted.values,
       type = "l", col = 8)

legend(4, 2500, legend = c("actual", "mod1", "mod2", "mod3", "mod4", "only G", "only A"), 
       col = c(1, 2, 3, 4, 5, 6, 8), 
       pch = 20, ncol = 1, cex = 0.8, pt.cex = 1.8)






### Model stock prices and case counts ###

stocks.t = cbind(stocks.full.imp, "numtoday" = cases.full.imp$numtoday)


# add lags to data in train and test datasets
n = paste0(rep("_lag", length(0:30)), 0:30)

for (lag in 0:30) {
  stocks.t = slide(stocks.t, Var = "Close_TSX", NewVar = paste0("Close_TSX", n[lag+1]), slideBy = -lag)
  stocks.t = slide(stocks.t, Var = "Close_NASDAQ", NewVar = paste0("Close_NASDAQ", n[lag+1]), slideBy = -lag)
  stocks.t = slide(stocks.t, Var = "Close_SP500", NewVar = paste0("Close_SP500", n[lag+1]), slideBy = -lag)
  stocks.t = slide(stocks.t, Var = "Volume_TSX", NewVar = paste0("Volume_TSX", n[lag+1]), slideBy = -lag)
  stocks.t = slide(stocks.t, Var = "Volume_NASDAQ", NewVar = paste0("Volume_NASDAQ", n[lag+1]), slideBy = -lag)
  stocks.t = slide(stocks.t, Var = "Volume_SP500", NewVar = paste0("Volume_SP500", n[lag+1]), slideBy = -lag)
  
}

stocks.train = stocks.t[1:round(0.8*nrow(stocks.t)),]
stocks.test = stocks.t[(round(0.8*nrow(stocks.t))+1):nrow(stocks.t),]


# Modelling method 1: quasipoisson using stock prices as predictors

# first make models with individual predictors to see which lags have the greatest predictive ability

glm.lags.tsx.close = list()
glm.lags.nasdaq.close = list()
glm.lags.sp500.close = list()
cv.glm.lags.tsx.close = list()
cv.glm.lags.nasdaq.close = list()
cv.glm.lags.sp500.close = list()
cv.adjerr.glm.lags.tsx.close = list()
cv.adjerr.glm.lags.nasdaq.close = list()
cv.adjerr.glm.lags.sp500.close = list()
glm.lags.tsx.volume = list()
glm.lags.nasdaq.volume = list()
glm.lags.sp500.volume = list()
cv.glm.lags.tsx.volume = list()
cv.glm.lags.nasdaq.volume = list()
cv.glm.lags.sp500.volume = list()
cv.adjerr.glm.lags.tsx.volume = list()
cv.adjerr.glm.lags.nasdaq.volume = list()
cv.adjerr.glm.lags.sp500.volume = list()

n = paste0(rep("_lag", length(0:30)), 0:30)

for (lag in 0:30) {

  # model with TSX closing price
  mod.tsx.close = glm(reformulate(termlabels = colnames(stocks.train)[grep(paste0("Close_TSX", n[lag+1]), colnames(stocks.train))], response = "numtoday"),
                                                          data = na.omit(stocks.train), family = quasipoisson)
  
  cv.mod.tsx.close = cv.glm(data = stocks.train[complete.cases(stocks.train),c("numtoday", colnames(stocks.train)[grep(paste0("Close_TSX", n[lag+1]), colnames(stocks.train))])],
                      glmfit = mod.tsx.close, K = 5)
  
  glm.lags.tsx.close[[paste0("Close_TSX", n[lag+1])]] = mod.tsx.close
  cv.glm.lags.tsx.close[[paste0("Close_TSX", n[lag+1])]] = cv.mod.tsx.close
  cv.adjerr.glm.lags.tsx.close[[paste0("Close_TSX", n[lag+1])]] = get_adj_cv_error(cv.mod.tsx.close)
  
  
  # model with NASDAQ closing price
  mod.nasdaq.close = glm(reformulate(termlabels = colnames(stocks.train)[grep(paste0("Close_NASDAQ", n[lag+1]), colnames(stocks.train))], response = "numtoday"),
                                                                data = na.omit(stocks.train), family = quasipoisson)

  cv.mod.nasdaq.close = cv.glm(data = stocks.train[complete.cases(stocks.train),c("numtoday", colnames(stocks.train)[grep(paste0("Close_NASDAQ", n[lag+1]), colnames(stocks.train))])],
                                             glmfit = mod.nasdaq.close, K = 5)

  glm.lags.nasdaq.close[[paste0("Close_NASDAQ", n[lag+1])]] = mod.nasdaq.close
  cv.glm.lags.nasdaq.close[[paste0("Close_NASDAQ", n[lag+1])]] = cv.mod.nasdaq.close
  cv.adjerr.glm.lags.nasdaq.close[[paste0("Close_NASDAQ", n[lag+1])]] = get_adj_cv_error(cv.mod.nasdaq.close)

  
  
  # model with SP500 closing price
  mod.sp500.close = glm(reformulate(termlabels = colnames(stocks.train)[grep(paste0("Close_SP500", n[lag+1]), colnames(stocks.train))], response = "numtoday"),
                                                              data = na.omit(stocks.train), family = quasipoisson)

  cv.mod.sp500.close = cv.glm(data = stocks.train[complete.cases(stocks.train),c("numtoday", colnames(stocks.train)[grep(paste0("Close_SP500", n[lag+1]), colnames(stocks.train))])],
                        glmfit = mod.sp500.close, K = 5)

  glm.lags.sp500.close[[paste0("Close_SP500", n[lag+1])]] = mod.sp500.close
  cv.glm.lags.sp500.close[[paste0("Close_SP500", n[lag+1])]] = cv.mod.sp500.close
  cv.adjerr.glm.lags.sp500.close[[paste0("Close_SP500", n[lag+1])]] = get_adj_cv_error(cv.mod.sp500.close)


  
  
  # model with TSX volume
  mod.tsx.volume = glm(reformulate(termlabels = colnames(stocks.train)[grep(paste0("Volume_TSX", n[lag+1]), colnames(stocks.train))], response = "numtoday"),
                      data = na.omit(stocks.train), family = quasipoisson)
  
  cv.mod.tsx.volume = cv.glm(data = stocks.train[complete.cases(stocks.train),c("numtoday", colnames(stocks.train)[grep(paste0("Volume_TSX", n[lag+1]), colnames(stocks.train))])],
                            glmfit = mod.tsx.volume, K = 5)
  
  glm.lags.tsx.volume[[paste0("Volume_TSX", n[lag+1])]] = mod.tsx.volume
  cv.glm.lags.tsx.volume[[paste0("Volume_TSX", n[lag+1])]] = cv.mod.tsx.volume
  cv.adjerr.glm.lags.tsx.volume[[paste0("Volume_TSX", n[lag+1])]] = get_adj_cv_error(cv.mod.tsx.volume)
  
  
  # model with NASDAQ volume
  mod.nasdaq.volume = glm(reformulate(termlabels = colnames(stocks.train)[grep(paste0("Volume_NASDAQ", n[lag+1]), colnames(stocks.train))], response = "numtoday"),
                         data = na.omit(stocks.train), family = quasipoisson)
  
  cv.mod.nasdaq.volume = cv.glm(data = stocks.train[complete.cases(stocks.train),c("numtoday", colnames(stocks.train)[grep(paste0("Volume_NASDAQ", n[lag+1]), colnames(stocks.train))])],
                               glmfit = mod.nasdaq.volume, K = 5)
  
  glm.lags.nasdaq.volume[[paste0("Volume_NASDAQ", n[lag+1])]] = mod.nasdaq.volume
  cv.glm.lags.nasdaq.volume[[paste0("Volume_NASDAQ", n[lag+1])]] = cv.mod.nasdaq.volume
  cv.adjerr.glm.lags.nasdaq.volume[[paste0("Volume_NASDAQ", n[lag+1])]] = get_adj_cv_error(cv.mod.nasdaq.volume)
  
  
  
  # model with SP500 volume 
  mod.sp500.volume = glm(reformulate(termlabels = colnames(stocks.train)[grep(paste0("Volume_SP500", n[lag+1]), colnames(stocks.train))], response = "numtoday"),
                        data = na.omit(stocks.train), family = quasipoisson)
  
  cv.mod.sp500.volume = cv.glm(data = stocks.train[complete.cases(stocks.train),c("numtoday", colnames(stocks.train)[grep(paste0("Volume_SP500", n[lag+1]), colnames(stocks.train))])],
                             glmfit = mod.sp500.volume, K = 5)
  
  glm.lags.sp500.volume[[paste0("Volume_SP500", n[lag+1])]] = mod.sp500.volume
  cv.glm.lags.sp500.volume[[paste0("Volume_SP500", n[lag+1])]] = cv.mod.sp500.volume
  cv.adjerr.glm.lags.sp500.volume[[paste0("Volume_SP500", n[lag+1])]] = get_adj_cv_error(cv.mod.sp500.volume)
  
}

# get adjusted cv errors for all three types of models
cv.adjerr.glm.lags.tsx.close = unlist(cv.adjerr.glm.lags.tsx.close)
cv.adjerr.glm.lags.nasdaq.close = unlist(cv.adjerr.glm.lags.nasdaq.close)
cv.adjerr.glm.lags.sp500.close = unlist(cv.adjerr.glm.lags.sp500.close)
cv.adjerr.glm.lags.tsx.volume = unlist(cv.adjerr.glm.lags.tsx.volume)
cv.adjerr.glm.lags.nasdaq.volume = unlist(cv.adjerr.glm.lags.nasdaq.volume)
cv.adjerr.glm.lags.sp500.volume = unlist(cv.adjerr.glm.lags.sp500.volume)

which.min(cv.adjerr.glm.lags.tsx.close)
which.min(cv.adjerr.glm.lags.nasdaq.close)
which.min(cv.adjerr.glm.lags.sp500.close)
which.min(cv.adjerr.glm.lags.tsx.volume)
which.min(cv.adjerr.glm.lags.nasdaq.volume)
which.min(cv.adjerr.glm.lags.sp500.volume)

# compare the different types of predictors
sort(c("tsx_close" = min(cv.adjerr.glm.lags.tsx.close), "nasdaq_close" = min(cv.adjerr.glm.lags.nasdaq.close), 
       "sp500_close" = min(cv.adjerr.glm.lags.sp500.close), "tsx_volume" = min(cv.adjerr.glm.lags.tsx.volume), 
       "nasdaq_volume" = min(cv.adjerr.glm.lags.nasdaq.volume), "sp500_volume" = min(cv.adjerr.glm.lags.sp500.volume)))

# Note: the SP500 close predictor produces the lowest adj cv error.



# Combine TSX, NASDAQ and SP500 predictors 
# Note: for each predictor, use the lag that produced the smallest adj cv error

best.preds.stocks = c(names(which.min(cv.adjerr.glm.lags.tsx.close)), names(which.min(cv.adjerr.glm.lags.nasdaq.close)),
                      names(which.min(cv.adjerr.glm.lags.sp500.close)), names(which.min(cv.adjerr.glm.lags.tsx.volume)),
                      names(which.min(cv.adjerr.glm.lags.nasdaq.volume)), names(which.min(cv.adjerr.glm.lags.sp500.volume)))

                          
                          
stocks.c.1 = glm(reformulate(best.preds.stocks, "numtoday"), data = na.omit(stocks.train), family = quasipoisson)
cv.stocks.c.1 = cv.glm(data = stocks.train[complete.cases(stocks.train), c("numtoday", best.preds.stocks)],
                       glmfit = stocks.c.1, K = 5)
cv.adjerr.stocks.c.1 = get_adj_cv_error(cv.stocks.c.1)


# compare model with three predictors to best models with individual predictors
sort(c("tsx_close" = min(cv.adjerr.glm.lags.tsx.close), "nasdaq_close" = min(cv.adjerr.glm.lags.nasdaq.close), 
       "sp500_close" = min(cv.adjerr.glm.lags.sp500.close), "tsx_volume" = min(cv.adjerr.glm.lags.tsx.volume), 
       "nasdaq_volume" = min(cv.adjerr.glm.lags.nasdaq.volume), "sp500_volume" = min(cv.adjerr.glm.lags.sp500.volume),
       "multi_pred" = cv.adjerr.stocks.c.1))

# Result: SP500 close has the lowest adj cv error.



# plot predictions
plot(1:nrow(stocks.train), stocks.train$numtoday, xlab = "", ylab = "Cases", xaxt = "n", type = "l",
     main = "Predictions from Stock Market Models", ylim = c(0, 3200), lwd = 2)
axis(1, at = 1:nrow(stocks.train), labels = stocks.train$Date, las = 3)

points(31:nrow(stocks.train), glm.lags.tsx.close[[which.min(cv.adjerr.glm.lags.tsx.close)]]$fitted.values, type = "l", col = 2)
points(31:nrow(stocks.train), glm.lags.nasdaq.close[[which.min(cv.adjerr.glm.lags.nasdaq.close)]]$fitted.values, type = "l", col = 3)
points(31:nrow(stocks.train), glm.lags.sp500.close[[which.min(cv.adjerr.glm.lags.sp500.close)]]$fitted.values, type = "l", col = 4)
points(31:nrow(stocks.train), glm.lags.tsx.volume[[which.min(cv.adjerr.glm.lags.tsx.volume)]]$fitted.values, type = "l", col = 5)
points(31:nrow(stocks.train), glm.lags.nasdaq.volume[[which.min(cv.adjerr.glm.lags.nasdaq.volume)]]$fitted.values, type = "l", col = 6)
points(31:nrow(stocks.train), glm.lags.sp500.volume[[which.min(cv.adjerr.glm.lags.sp500.volume)]]$fitted.values, type = "l", col = 7)
points(31:nrow(stocks.train), stocks.c.1$fitted.values, type = "l", col = 8)

legend(-3, 3000, 
       legend = c("actual", "TSX Close", "NASDAQ Close", "S&P500 close", 
                  "TSX Volume", "NASDAQ Volume", "S&P500 Volume", "combined"),
       col = c(1, 2, 3, 4, 5, 6, 7, 8), pch = 20, ncol = 1, cex = 0.8, pt.cex = 1.8)





### Model government policies and case counts ###

# Note: from cross correlation analysis, the variables <policies> are  
# most correlated at lag 0. In <riskopen>, the variables are most correlated at lag 29.


# prepare training and testing subsets
policies.t = data.frame(policies, riskopen.imp)
policies.t = policies.t[,c("Date", "StringencyIndex", "GovernmentResponseIndex", "ContainmentHealthIndex", 
                           "community_understanding", "openness_risk")]
policies.t$numtoday = cases.full.imp$numtoday





# add lagged variables
for (lag in 0:30){
  policies.t = slide(policies.t, Var = "community_understanding", NewVar = paste0("community_understanding_lag", lag), slideBy = -lag)
  policies.t = slide(policies.t, Var = "openness_risk", NewVar = paste0("openness_risk_lag", lag), slideBy = -lag)
  policies.t = slide(policies.t, Var = "StringencyIndex", NewVar = paste0("StringencyIndex_lag", lag), slideBy = -lag)
  policies.t = slide(policies.t, Var = "GovernmentResponseIndex", NewVar = paste0("GovernmentResponseIndex_lag", lag), slideBy = -lag)
  policies.t = slide(policies.t, Var = "ContainmentHealthIndex", NewVar = paste0("ContainmentHealthIndex_lag", lag), slideBy = -lag)
}

policies.t$monday = cases.full.imp$monday
policies.t$holiday = cases.full.imp$holiday


# training and testing subsets
policies.train = policies.t[1:round(0.8*nrow(policies.t)),]
policies.test = policies.t[(round(0.8*nrow(policies.t))+1):nrow(policies.t),]



# check which lag for each predictor produces best predictions
mod.policies.community = list()
mod.policies.openness = list()
mod.policies.stringency = list()
mod.policies.gov = list()
mod.policies.containment = list()
cv.mod.policies.community = list()
cv.mod.policies.openness = list()
cv.mod.policies.stringency = list()
cv.mod.policies.gov = list()
cv.mod.policies.containment = list()
cv.adjerr.mod.policies.community = list()
cv.adjerr.mod.policies.openness = list()
cv.adjerr.mod.policies.stringency = list()
cv.adjerr.mod.policies.gov = list()
cv.adjerr.mod.policies.containment = list()



for (lag in 0:30) {
  
  # model with community understanding predictor
  mod.commund = glm(reformulate(colnames(policies.train)[match(paste0("community_understanding_lag", lag), 
                                                   colnames(policies.train))], "numtoday"), 
                     data = na.omit(policies.train), family = quasipoisson)
   
  cv.mod.commund = cv.glm(data = policies.train[complete.cases(policies.train), c(colnames(policies.train)[match(paste0("community_understanding_lag", lag), 
                                                                                                                  colnames(policies.train))], "numtoday")],
                           glmfit = mod.commund, K = 5)
   
  mod.policies.community[[paste0("community_understanding_lag", lag)]] = mod.commund
  cv.mod.policies.community[[paste0("community_understanding_lag", lag)]] = cv.mod.commund
  cv.adjerr.mod.policies.community[[paste0("community_understanding_lag", lag)]] = get_adj_cv_error(cv.mod.commund)
   
   
  
  # model with openness risk predictor
  mod.open =
    glm(reformulate(colnames(policies.train)[match(paste0("openness_risk_lag", lag),
                                                   colnames(policies.train))], "numtoday"),
        data = na.omit(policies.train), family = quasipoisson)
  
  cv.mod.open = cv.glm(data = policies.train[complete.cases(policies.train), c(colnames(policies.train)[match(paste0("openness_risk_lag", lag),
                                                                                                              colnames(policies.train))], "numtoday")],
                          glmfit = mod.open, K = 5)
  
  mod.policies.openness[[paste0("openness_risk_lag", lag)]] = mod.open
  cv.mod.policies.openness[[paste0("openness_risk_lag", lag)]] = cv.mod.open
  cv.adjerr.mod.policies.openness[[paste0("openness_risk_lag", lag)]] = get_adj_cv_error(cv.mod.open) 
  
  

  # model with stringency predictor
  mod.stringency =
    glm(reformulate(colnames(policies.train)[match(paste0("StringencyIndex_lag", lag),
                                                   colnames(policies.train))], "numtoday"),
        data = na.omit(policies.train), family = quasipoisson)
  
  cv.mod.stringency = cv.glm(data = policies.train[complete.cases(policies.train), c(colnames(policies.train)[match(paste0("StringencyIndex_lag", lag),
                                                                                                                    colnames(policies.train))], "numtoday")],
                       glmfit = mod.stringency, K = 5)
  
  mod.policies.stringency[[paste0("StringencyIndex_lag", lag)]] = mod.stringency
  cv.mod.policies.stringency[[paste0("StringencyIndex_lag", lag)]] = cv.mod.stringency
  cv.adjerr.mod.policies.stringency[[paste0("StringencyIndex_lag", lag)]] = get_adj_cv_error(cv.mod.stringency) 
  
  
  
  # model with government response predictor
  mod.govresponse =
    glm(reformulate(colnames(policies.train)[match(paste0("GovernmentResponseIndex_lag", lag),
                                                   colnames(policies.train))], "numtoday"),
        data = na.omit(policies.train), family = quasipoisson)
  
  cv.mod.govresponse = cv.glm(data = policies.train[complete.cases(policies.train), c(colnames(policies.train)[match(paste0("GovernmentResponseIndex_lag", lag),
                                                                                                                     colnames(policies.train))], "numtoday")],
                             glmfit = mod.govresponse, K = 5)
  
  mod.policies.gov[[paste0("GovernmentResponseIndex_lag", lag)]] = mod.govresponse
  cv.mod.policies.gov[[paste0("GovernmentResponseIndex_lag", lag)]] = cv.mod.govresponse
  cv.adjerr.mod.policies.gov[[paste0("GovernmentResponseIndex_lag", lag)]] = get_adj_cv_error(cv.mod.govresponse)
  
  
  
  # model with containment index predictor
  mod.containment =
    glm(reformulate(colnames(policies.train)[match(paste0("ContainmentHealthIndex_lag", lag),
                                                   colnames(policies.train))], "numtoday"),
        data = na.omit(policies.train), family = quasipoisson)
  
  cv.mod.containment = cv.glm(data = policies.train[complete.cases(policies.train), c(colnames(policies.train)[match(paste0("ContainmentHealthIndex_lag", lag),
                                                                                                                    colnames(policies.train))], "numtoday")],
                             glmfit = mod.containment, K = 5)

  mod.policies.containment[[paste0("ContainmentHealthIndex_lag", lag)]] = mod.containment
  cv.mod.policies.containment[[paste0("ContainmentHealthIndex_lag", lag)]] = cv.mod.containment
  cv.adjerr.mod.policies.containment[[paste0("ContainmentHealthIndex_lag", lag)]] = get_adj_cv_error(cv.mod.containment) 
  
  
}


# get adj cv errors for all separate models above
cv.adjerr.mod.policies.community = unlist(cv.adjerr.mod.policies.community)
cv.adjerr.mod.policies.openness = unlist(cv.adjerr.mod.policies.openness)
cv.adjerr.mod.policies.stringency = unlist(cv.adjerr.mod.policies.stringency) 
cv.adjerr.mod.policies.gov = unlist(cv.adjerr.mod.policies.gov)
cv.adjerr.mod.policies.containment = unlist(cv.adjerr.mod.policies.containment)


# find lags at which min cv errors occur
which.min(cv.adjerr.mod.policies.community)
which.min(cv.adjerr.mod.policies.openness)
which.min(cv.adjerr.mod.policies.stringency)
which.min(cv.adjerr.mod.policies.gov)
which.min(cv.adjerr.mod.policies.containment)


# sort the minimum cv errors in order of model predictor type
policy.preds = c("community" = min(cv.adjerr.mod.policies.community), "openness" = min(cv.adjerr.mod.policies.openness),
       "stringency" = min(cv.adjerr.mod.policies.stringency), "gov" = min(cv.adjerr.mod.policies.gov),
       "containment" = min(cv.adjerr.mod.policies.containment))
sort(policy.preds)

# Result: community has the lowest CV error.


# plot single predictor models
plot(1:nrow(policies.train), policies.train$numtoday, xlab = "", ylab = "Cases", 
     main = "Single Predictor Policies Models", type = "l", xaxt = "n", lwd = 2)
axis(1, at = 1:nrow(policies.train), labels = policies.train$Date, las = 3)

points(31:nrow(policies.train), 
       mod.policies.community[[which.min(cv.adjerr.mod.policies.community)]]$fitted.values,
       type = "l", col = 2)
points(31:nrow(policies.train), 
       mod.policies.openness[[which.min(cv.adjerr.mod.policies.openness)]]$fitted.values, type = "l", col = 3)
points(31:nrow(policies.train), 
       mod.policies.stringency[[which.min(cv.adjerr.mod.policies.stringency)]]$fitted.values, type = "l", col = 4)
points(31:nrow(policies.train), 
       mod.policies.gov[[which.min(cv.adjerr.mod.policies.gov)]]$fitted.values, type = "l", col = 5)
points(31:nrow(policies.train), 
       mod.policies.containment[[which.min(cv.adjerr.mod.policies.containment)]]$fitted.values, type = "l", col = 6)

legend(0, 2500, 
       legend = c("actual", "community", "openness", "stringency", "gov", "containment"),
       col = c(1, 2, 3, 4, 5, 6), pch = 20, ncol = 1, cex = 0.8, pt.cex = 1.8)




# get best predictors from the models above
best.preds.policies = c(names(which.min(cv.adjerr.mod.policies.community)), names(which.min(cv.adjerr.mod.policies.openness)),
                        names(which.min(cv.adjerr.mod.policies.stringency)), names(which.min(cv.adjerr.mod.policies.gov)),
                        names(which.min(cv.adjerr.mod.policies.containment)))


pred = c()
policy.mods.all.vars = list()
cv.policy.mods.all.vars = list()
cv.adjerr.policy.mods.all.vars = list()


# add one predictor at a time, in order of most predictive to least, and compare that model with the previous ones
for (name in best.preds.policies[sort(policy.preds, index.return = T)$ix]) {
  pred = c(pred, name)
  
  mod = glm(reformulate(pred, "numtoday"), data = na.omit(policies.train), family = quasipoisson)
  
  cv.mod = cv.glm(data = policies.train[complete.cases(policies.train), c("numtoday", pred)],
                  glmfit = mod, K = 5)
  
  policy.mods.all.vars[[str_c(pred, collapse = " + ")]] = mod
  cv.policy.mods.all.vars[[str_c(pred, collapse = " + ")]] = cv.mod
  cv.adjerr.policy.mods.all.vars[[str_c(pred, collapse = " + ")]] = get_adj_cv_error(cv.mod)
  
  
}


# get adj cv errors for all models made in loop above
cv.adjerr.policy.mods.all.vars = unlist(cv.adjerr.policy.mods.all.vars)

which.min(cv.adjerr.policy.mods.all.vars)

# compare single predictor models to multi predictor model
sort(c("multi" = min(cv.adjerr.policy.mods.all.vars), policy.preds))

# community predictor alone still performs best

policy.multipred.best = trim(unlist(str_split(names(which.min(cv.adjerr.policy.mods.all.vars)), pattern = "[+]")))


# best multi-pred model with monday variable
mod.policies.1 = glm(reformulate(c("monday", policy.multipred.best), "numtoday"), data = na.omit(policies.train), family = quasipoisson)
cv.mod.policies.1 = cv.glm(data = policies.train[complete.cases(policies.train),c("numtoday", "monday", policy.multipred.best)],
                           glmfit = mod.policies.1, K = 5)
cv.adjerr.mod.policies.1 = get_adj_cv_error(cv.mod.policies.1)


# best multi-pred model with holiday variable
mod.policies.2 = glm(reformulate(c("holiday", policy.multipred.best), "numtoday"), data = na.omit(policies.train), family = quasipoisson)
cv.mod.policies.2 = cv.glm(data = policies.train[complete.cases(policies.train),c("numtoday", "holiday", policy.multipred.best)],
                           glmfit = mod.policies.2, K = 5)
cv.adjerr.mod.policies.2 = get_adj_cv_error(cv.mod.policies.2)

# best multi-pred model with monday and holiday variable
mod.policies.3 = glm(reformulate(c("holiday", "monday", policy.multipred.best), "numtoday"), data = na.omit(policies.train), family = quasipoisson)
cv.mod.policies.3 = cv.glm(data = policies.train[complete.cases(policies.train),c("numtoday", "monday", "holiday", policy.multipred.best)],
                           glmfit = mod.policies.3, K = 5)
cv.adjerr.mod.policies.3 = get_adj_cv_error(cv.mod.policies.3)

cv.adjerr.mod.policies.1 = unlist(cv.adjerr.mod.policies.1)
cv.adjerr.mod.policies.2 = unlist(cv.adjerr.mod.policies.2)
cv.adjerr.mod.policies.3 = unlist(cv.adjerr.mod.policies.3)


# compare original multi-predictor model with three multi-predictor models above
sort(c("orig_multipred" = min(cv.adjerr.policy.mods.all.vars), "multipred_monday" = cv.adjerr.mod.policies.1,
       "multipred_holiday" = cv.adjerr.mod.policies.2, 
       "multipred_mon_holiday" = cv.adjerr.mod.policies.3,
     policy.preds))

# Result: original single predictor community model has lowest adj cv error.



# plot multi index models
plot(1:nrow(policies.train), policies.train$numtoday, xlab = "", ylab = "Cases", 
     main = "Multiple Predictor Policies Models", type = "l", xaxt = "n", lwd = 2)
axis(1, at = 1:nrow(policies.train), labels = policies.train$Date, las = 3)

points(31:nrow(policies.train), policy.mods.all.vars[[which.min(cv.adjerr.policy.mods.all.vars)]]$fitted.values,
       type = "l", col = 2)
points(31:nrow(policies.train), mod.policies.1$fitted.values, type = "l", col = 3)
points(31:nrow(policies.train), mod.policies.2$fitted.values, type = "l", col = 4)
points(31:nrow(policies.train), mod.policies.3$fitted.values, type = "l", col = 5)
legend(0, 2500, 
       legend = c("actual", "multi", "multi + monday", "multi + holiday", 
                  "multi + monday + holiday"),
       col = c(1, 2, 3, 4, 5, 6, 7, 8), pch = 20, ncol = 1, cex = 0.8, pt.cex = 1.8)




### Model cases using flight data ###

flights.per.day.t = data.frame("day" = dom.flights.per.day$day, 
                       "dom_num_flights" = dom.flights.per.day$num_flights, 
                       "intl_num_flights" = intl.flights.per.day$num_flights, 
                       "total_num_flights" = flights.per.day$num_flights,
                       "numtoday" = cases.full.imp$numtoday,
                       "monday" = cases.full.imp$monday, 
                       "holiday" = cases.full.imp$holiday)


# add lagged variables 
for (lag in 0:30) {
  flights.per.day.t = slide(flights.per.day.t, Var = "dom_num_flights", 
                            NewVar = paste0("dom_num_flights_lag", lag), slideBy = -lag)
  flights.per.day.t = slide(flights.per.day.t, Var = "intl_num_flights", 
                            NewVar = paste0("intl_num_flights_lag", lag), slideBy = -lag)
  flights.per.day.t = slide(flights.per.day.t, Var = "total_num_flights", 
                            NewVar = paste0("total_num_flights_lag", lag), slideBy = -lag)
  
}


# training and testing subsets
flights.per.day.train = flights.per.day.t[1:round(0.8*nrow(flights.per.day.t)),]
flights.per.day.test = flights.per.day.t[(round(0.8*nrow(flights.per.day.t))+1):nrow(flights.per.day.t),]

mod.flights.dom = list()
mod.flights.intl = list()
mod.flights.total = list()
cv.mod.flights.dom = list()
cv.mod.flights.intl = list()
cv.mod.flights.total = list()
cv.adjerr.mod.flights.dom = list()
cv.adjerr.mod.flights.intl = list()
cv.adjerr.mod.flights.total = list()

# Find most predictive lags for each variable
for (lag in 0:30) {
  
  # model with domestic flights predictor at different lags
   mod.dom = 
    glm(reformulate(colnames(flights.per.day.train)[match(paste0("dom_num_flights_lag", lag), 
                                                          colnames(flights.per.day.train))], "numtoday"),
        data = na.omit(flights.per.day.train), family = quasipoisson)
   
   cv.mod.dom = cv.glm(data = flights.per.day.train[complete.cases(flights.per.day.train), 
                                                    c("numtoday", 
                                                      colnames(flights.per.day.train)[match(paste0("dom_num_flights_lag", lag), 
                                                                                            colnames(flights.per.day.train))])],
                       glmfit = mod.dom, K = 5)
  
  mod.flights.dom[[paste0("dom_num_flights_lag", lag)]] = mod.dom
  cv.mod.flights.dom[[paste0("dom_num_flights_lag", lag)]] = cv.mod.dom
  cv.adjerr.mod.flights.dom[[paste0("dom_num_flights_lag", lag)]] = get_adj_cv_error(cv.mod.dom)
  
  
  
  # model with intl flights predictor at different lags
  mod.intl = 
    glm(reformulate(colnames(flights.per.day.train)[match(paste0("intl_num_flights_lag", lag), 
                                                          colnames(flights.per.day.train))], "numtoday"),
        data = na.omit(flights.per.day.train), family = quasipoisson)
  
  cv.mod.intl = cv.glm(data = flights.per.day.train[complete.cases(flights.per.day.train), 
                                                   c("numtoday", 
                                                     colnames(flights.per.day.train)[match(paste0("intl_num_flights_lag", lag), 
                                                                                           colnames(flights.per.day.train))])],
                      glmfit = mod.intl, K = 5)
  
  mod.flights.intl[[paste0("intl_num_flights_lag", lag)]] = mod.intl
  cv.mod.flights.intl[[paste0("intl_num_flights_lag", lag)]] = cv.mod.intl
  cv.adjerr.mod.flights.intl[[paste0("intl_num_flights_lag", lag)]] = get_adj_cv_error(cv.mod.intl)
  
  
  
  # model with total flights predictor at different lags
  mod.total = 
    glm(reformulate(colnames(flights.per.day.train)[match(paste0("total_num_flights_lag", lag), 
                                                          colnames(flights.per.day.train))], "numtoday"),
        data = na.omit(flights.per.day.train), family = quasipoisson)
  
  cv.mod.total = cv.glm(data = flights.per.day.train[complete.cases(flights.per.day.train), 
                                                    c("numtoday", 
                                                      colnames(flights.per.day.train)[match(paste0("total_num_flights_lag", lag), 
                                                                                            colnames(flights.per.day.train))])],
                       glmfit = mod.total, K = 5)
  
  mod.flights.total[[paste0("total_num_flights_lag", lag)]] = mod.total
  cv.mod.flights.total[[paste0("total_num_flights_lag", lag)]] = cv.mod.total
  cv.adjerr.mod.flights.total[[paste0("total_num_flights_lag", lag)]] = get_adj_cv_error(cv.mod.total)
  
  
}

cv.adjerr.mod.flights.dom = unlist(cv.adjerr.mod.flights.dom)
cv.adjerr.mod.flights.intl = unlist(cv.adjerr.mod.flights.intl)
cv.adjerr.mod.flights.total = unlist(cv.adjerr.mod.flights.total)

# see which lag produces lowest cv error for each predictor type
which.min(cv.adjerr.mod.flights.dom)
which.min(cv.adjerr.mod.flights.intl)
which.min(cv.adjerr.mod.flights.total)


min(cv.adjerr.mod.flights.dom)
min(cv.adjerr.mod.flights.intl)
min(cv.adjerr.mod.flights.total)


# compare predictors
flights.cvs = c("dom" = min(cv.adjerr.mod.flights.dom), "intl" = min(cv.adjerr.mod.flights.intl),
       "total" = min(cv.adjerr.mod.flights.total))
sort(flights.cvs)

# Result: as a standalone predictor, total flights performs best.


# plot fitted.values for single predictor mods
plot(1:nrow(flights.per.day.train), flights.per.day.train$numtoday, xlab = "", ylab = "Cases",
     main = "Single Predictor Flights Models", xaxt = "n", type = "l", lwd = 2)
axis(1, at = 1:nrow(flights.per.day.train), labels = flights.per.day.train$day, las = 3)

points(31:nrow(flights.per.day.train), 
       mod.flights.dom[[which.min(cv.adjerr.mod.flights.dom)]]$fitted.values, 
       type = "l", col = 2) # original

points(31:nrow(flights.per.day.train), mod.flights.intl[[which.min(cv.adjerr.mod.flights.intl)]]$fitted.values, type = "l", col = 3) 
points(31:nrow(flights.per.day.train), mod.flights.total[[which.min(cv.adjerr.mod.flights.total)]]$fitted.values, type = "l", col = 4) 
legend(0, 2500, 
       legend = c("actual", "domestic", "international", "total"),
       col = c(1, 2, 3, 4), pch = 20, ncol = 1, cex = 0.8, pt.cex = 1.8)





best.preds.flights = c(names(which.min(cv.adjerr.mod.flights.dom)), names(which.min(cv.adjerr.mod.flights.intl)),
                       names(which.min(cv.adjerr.mod.flights.total)))


# combine multiple predictors in order from most to least predictive
pred = c()
flights.multipred.mods = list()
cv.flights.multipred.mods = list()
cv.adjerr.flights.multipred.mods = list()

for (name in best.preds.flights[sort(flights.cvs, index.return = TRUE)$ix]) {
  pred = c(pred, name)
  mod = glm(reformulate(pred, "numtoday"), data = na.omit(flights.per.day.train),
            family = quasipoisson)
  
  cv.mod = cv.glm(data = flights.per.day.train[complete.cases(flights.per.day.train), c("numtoday", pred)],
                  glmfit = mod, K = 5)
  
  flights.multipred.mods[[str_c(pred, collapse = " + ")]] = mod
  cv.flights.multipred.mods[[str_c(pred, collapse = " + ")]] = cv.mod
  cv.adjerr.flights.multipred.mods[[str_c(pred, collapse = " + ")]] = get_adj_cv_error(cv.mod) 
  
}


cv.adjerr.flights.multipred.mods = unlist(cv.adjerr.flights.multipred.mods)

which.min(cv.adjerr.flights.multipred.mods)

# Result: all three flights variables make the smallest adj cv error.

# compare multi predictor model to single predictor models
sort(c("multi" = min(cv.adjerr.flights.multipred.mods), "dom" = min(cv.adjerr.mod.flights.dom),
       "intl" = min(cv.adjerr.mod.flights.intl), "total" = min(cv.adjerr.mod.flights.total)))



most.pred.multindex.flights = trim(unlist(str_split(names(which.min(cv.adjerr.flights.multipred.mods)), pattern = "[+]")))

# Combine most predictive lags from multi-pred models above with monday and/or holiday preds

# with monday
mod.flights.1 = glm(reformulate(c("monday", most.pred.multindex.flights), "numtoday"),
              data = na.omit(flights.per.day.train), family = quasipoisson)
cv.mod.flights.1 = cv.glm(data = flights.per.day.train[complete.cases(flights.per.day.train), 
                                                      c("numtoday", "monday", most.pred.multindex.flights)],
                         glmfit = mod.flights.1, K = 5)
cv.adjerr.mod.flights.1 = get_adj_cv_error(cv.mod.flights.1)


# with holiday
mod.flights.2 = glm(reformulate(c("holiday", most.pred.multindex.flights), "numtoday"),
                    data = na.omit(flights.per.day.train), family = quasipoisson)
cv.mod.flights.2 = cv.glm(data = flights.per.day.train[complete.cases(flights.per.day.train), 
                                                       c("numtoday", "holiday", most.pred.multindex.flights)],
                          glmfit = mod.flights.2, K = 5)
cv.adjerr.mod.flights.2 = get_adj_cv_error(cv.mod.flights.2)


# with monday and holiday
mod.flights.3 = glm(reformulate(c("monday", "holiday", most.pred.multindex.flights), "numtoday"),
                    data = na.omit(flights.per.day.train), family = quasipoisson)
cv.mod.flights.3 = cv.glm(data = flights.per.day.train[complete.cases(flights.per.day.train), 
                                                       c("numtoday", "monday", "holiday", most.pred.multindex.flights)],
                          glmfit = mod.flights.3, K = 5)
cv.adjerr.mod.flights.3 = get_adj_cv_error(cv.mod.flights.3)


# compare the models above to the best multi predictor model without the added vars
sort(c("monday" = cv.adjerr.mod.flights.1, "holiday" = cv.adjerr.mod.flights.2, 
       "monday_holiday" = cv.adjerr.mod.flights.3, "orig" = min(cv.adjerr.flights.multipred.mods)))

# Result: the original multi predictor model fits the best.



# plot fitted.values for multi predictor mods
plot(1:nrow(flights.per.day.train), flights.per.day.train$numtoday, xlab = "", ylab = "Cases",
     main = "Multi Predictor Flights Models", xaxt = "n", type = "l", lwd = 2)
axis(1, at = 1:nrow(flights.per.day.train), labels = flights.per.day.train$day, las = 3)

points(31:nrow(flights.per.day.train), 
       flights.multipred.mods[[which.min(cv.adjerr.flights.multipred.mods)]]$fitted.values, 
       type = "l", col = 2) # original

points(31:nrow(flights.per.day.train), mod.flights.1$fitted.values, type = "l", col = 3) 
points(31:nrow(flights.per.day.train), mod.flights.2$fitted.values, type = "l", col = 4) 
points(31:nrow(flights.per.day.train), mod.flights.3$fitted.values, type = "l", col = 5) 
legend(0, 2500, 
       legend = c("actual", "multi", "multi + monday", "multi + holiday", "multi + monday + holiday"),
       col = c(1, 2, 3, 4, 5), pch = 20, ncol = 1, cex = 0.8, pt.cex = 1.8)




### Model weather and case counts ###

# create training and testing subsets
weather.t = data.frame(weather.canada, "numtoday" = cases.full.imp$numtoday)

# add lagged variables for each weather stat
n = paste0(rep("_lag", length(0:30)), 0:30)

for (lag in 0:30) {
  weather.t = slide(weather.t, Var = "CAN.AVG_MEAN_TEMPERATURE", NewVar = paste0("CAN.AVG_MEAN_TEMPERATURE", n[lag+1]), slideBy = -lag)
  weather.t = slide(weather.t, Var = "CAN.AVG_MIN_REL_HUMIDITY", NewVar = paste0("CAN.AVG_MIN_REL_HUMIDITY", n[lag+1]), slideBy = -lag)
  weather.t = slide(weather.t, Var = "CAN.AVG_MAX_REL_HUMIDITY", NewVar = paste0("CAN.AVG_MAX_REL_HUMIDITY", n[lag+1]), slideBy = -lag)
}

weather.train = weather.t[1:round(0.8*nrow(weather.t)),]
weather.test = weather.t[(round(0.8*nrow(weather.t))+1):nrow(weather.t),]

temp.lags = colnames(weather.train)[grep("CAN.AVG_MEAN_TEMPERATURE_lag", colnames(weather.train))]
max.hum.lags = colnames(weather.train)[grep("CAN.AVG_MAX_REL_HUMIDITY_lag", colnames(weather.train))]
min.hum.lags = colnames(weather.train)[grep("CAN.AVG_MIN_REL_HUMIDITY_lag", colnames(weather.train))]


# models with temperature predictor
temp.mods = list()
cv.temp.mods = list()
cv.adjerr.temp.mods = list()

for (name in temp.lags) {
  
  mod = glm(reformulate(name, response = "numtoday"), data = na.omit(weather.train), family = quasipoisson)
  
  cv.mod = cv.glm(data = weather.train[complete.cases(weather.train), c("numtoday", name)],
                  glmfit = mod, K = 5)
  
  temp.mods[[name]] = mod
  cv.temp.mods[[name]] = cv.mod
  cv.adjerr.temp.mods[[name]] = get_adj_cv_error(cv.mod)
  
}

# models with max humidity predictor
max.hum.mods = list()
cv.max.hum.mods = list()
cv.adjerr.max.hum.mods = list()

for (name in max.hum.lags) {
  
  mod = glm(reformulate(name, response = "numtoday"), data = na.omit(weather.train), family = quasipoisson)
  
  cv.mod = cv.glm(data = weather.train[complete.cases(weather.train), c("numtoday", name)],
                  glmfit = mod, K = 5)
  
  max.hum.mods[[name]] = mod
  cv.max.hum.mods[[name]] = cv.mod
  cv.adjerr.max.hum.mods[[name]] = get_adj_cv_error(cv.mod) 
  
}


# models with min humidity predictor
min.hum.mods = list()
cv.min.hum.mods = list()
cv.adjerr.min.hum.mods = list()

for (name in min.hum.lags) {
  mod = glm(reformulate(name, response = "numtoday"), data = na.omit(weather.train), family = quasipoisson)
  
  cv.mod = cv.glm(data = weather.train[complete.cases(weather.train), c("numtoday", name)],
                  glmfit = mod, K = 5)
  
  min.hum.mods[[name]] = mod
  cv.min.hum.mods[[name]] = cv.mod
  cv.adjerr.min.hum.mods[[name]] = get_adj_cv_error(cv.mod)
  
  
}

cv.adjerr.temp.mods = unlist(cv.adjerr.temp.mods)
cv.adjerr.max.hum.mods = unlist(cv.adjerr.max.hum.mods)
cv.adjerr.min.hum.mods = unlist(cv.adjerr.min.hum.mods)

# find lags that produce minimum cv errors for each model type above
which.min(cv.adjerr.temp.mods)
which.min(cv.adjerr.max.hum.mods)
which.min(cv.adjerr.min.hum.mods)


# compare different predictor types
best.preds.weather = c(names(which.min(cv.adjerr.temp.mods)), names(which.min(cv.adjerr.max.hum.mods)),
                       names(which.min(cv.adjerr.min.hum.mods)))

cv.best.preds.weather = c(min(cv.adjerr.temp.mods), min(cv.adjerr.max.hum.mods), 
                          min(cv.adjerr.min.hum.mods))

best.preds.weather[sort(cv.best.preds.weather, index.return = T)$ix]

pred = c()
mods.weather.multindex = list()
cv.mods.weather.multindex = list()
cv.adjerr.mods.weather.multindex = list()

for (name in best.preds.weather) {
  
  pred = c(pred, name)
  mod = glm(reformulate(pred, "numtoday"), data = na.omit(weather.train), family = quasipoisson)
  cv.mod = cv.glm(data = weather.train[complete.cases(weather.train), c("numtoday", pred)], 
                  glmfit = mod, K = 5)
  
  mods.weather.multindex[[str_c(pred, collapse = " + ")]] = mod
  cv.mods.weather.multindex[[str_c(pred, collapse = " + ")]] = cv.mod
  cv.adjerr.mods.weather.multindex[[str_c(pred, collapse = " + ")]] = get_adj_cv_error(cv.mod) 
  
  
}

cv.adjerr.mods.weather.multindex = unlist(cv.adjerr.mods.weather.multindex)

# compare multi predictor model to individual predictor models
sort(c("temp" = min(cv.adjerr.temp.mods), "max_hum" = min(cv.adjerr.max.hum.mods),
       "min_hum" = min(cv.adjerr.min.hum.mods), cv.adjerr.mods.weather.multindex))

best.weather.preds = trim(unlist(str_split(names(which.min(cv.adjerr.mods.weather.multindex)), pattern = "[+]")))

# Result: the multi predictor model with all three best single predictors has the 
# lowest adj cv error.


# plot fitted values
plot(1:nrow(weather.train), weather.train$numtoday, xlab = "", xaxt = "n", ylab = "Counts",
     main = "Predictions from Weather Models", type = "l", lwd = 2)
axis(1, at = 1:nrow(weather.train), labels = weather.train$LOCAL_DATE, las = 3)

points(31:nrow(weather.train), mods.weather.multindex[[names(which.min(cv.adjerr.mods.weather.multindex))]]$fitted.values, 
       type = "l", col = 2) # best multindex model
points(31:nrow(weather.train), temp.mods[[which.min(cv.adjerr.temp.mods)]]$fitted.values, 
       type = "l", col = 3) # best temp model
points(31:nrow(weather.train), min.hum.mods[[which.min(cv.adjerr.min.hum.mods)]]$fitted.values, 
       type = "l", col = 4) # best min hum model
points(31:nrow(weather.train), max.hum.mods[[which.min(cv.adjerr.max.hum.mods)]]$fitted.values, 
       type = "l", col = 5) # best max hum model

legend(5, 2700, 
       legend = c("actual", "best multi pred", "best temp pred", 
                  "best min hum pred", "best max hum pred"),
       col = c(1, 2, 3, 4, 5), pch = 20, ncol = 1, cex = 0.8, pt.cex = 1.8)




### Model twitter metadata and case counts ###

# Note: total_tweets_clean was most highly correlated at lag 7 and total_tweets_full was
# most highly correlated at lag 13.

# make training and testing datasets
twitter.t = data.frame("date" = twitter.stats.clean$date, 
                       "total_tweets_clean" = twitter.stats.clean$total_tweets,
                       "total_tweets_full" = twitter.stats.full$total_tweets,
                       "numtoday" = cases.full.imp$numtoday, 
                       "holiday" = cases.full.imp$holiday, 
                       "monday" = cases.full.imp$monday)

# add lags
n = paste0(rep("_lag", length(0:30)), 0:30)

for (lag in 0:30) {
  twitter.t = slide(twitter.t, Var = "total_tweets_clean", NewVar = paste0("total_tweets_clean", n[lag+1]), slideBy = -lag)
  twitter.t = slide(twitter.t, Var = "total_tweets_full", NewVar = paste0("total_tweets_full", n[lag+1]), slideBy = -lag)
}

twitter.train = twitter.t[1:round(0.8*nrow(twitter.t)),]
twitter.test = twitter.t[(round(0.8*nrow(twitter.t))+1):nrow(twitter.t),]

n.clean = colnames(twitter.train)[grep("total_tweets_clean_lag", colnames(twitter.train))]
n.full = colnames(twitter.train)[grep("total_tweets_full_lag", colnames(twitter.train))]



# create models with lags 0 to 30 as single predictors for clean and full tweet datasets
mods.twitter.clean = list()
cv.mods.twitter.clean = list()
cv.adjerr.mods.twitter.clean = list()

for (name in n.clean) {
  
  mod = glm(reformulate(name, "numtoday"), data = na.omit(twitter.train), 
                                   family = quasipoisson)
  
  cv.mod = cv.glm(data = twitter.train[complete.cases(twitter.train), c("numtoday", name)],
                  glmfit = mod, K = 5)
  
  mods.twitter.clean[[name]] = mod
  cv.mods.twitter.clean[[name]] = cv.mod
  cv.adjerr.mods.twitter.clean[[name]] = get_adj_cv_error(cv.mod)  
  
}



mods.twitter.full = list()
cv.mods.twitter.full = list()
cv.adjerr.mods.twitter.full = list()

for (name in n.full) {
  
  mod = glm(reformulate(name, "numtoday"), data = na.omit(twitter.train), 
                                   family = quasipoisson)
  
  cv.mod = cv.glm(data = twitter.train[complete.cases(twitter.train), c("numtoday", name)],
                  glmfit = mod, K = 5)
  
  mods.twitter.full[[name]] = mod
  cv.mods.twitter.full[[name]] = cv.mod
  cv.adjerr.mods.twitter.full[[name]] = get_adj_cv_error(cv.mod)
  
  
}

cv.adjerr.mods.twitter.clean = unlist(cv.adjerr.mods.twitter.clean)
cv.adjerr.mods.twitter.full = unlist(cv.adjerr.mods.twitter.full)

which.min(cv.adjerr.mods.twitter.clean)
which.min(cv.adjerr.mods.twitter.full)

# Result: for clean twitter data, lag 3 is best. For full twitter data, lag 17 is best.

# make models using best predictors as found above, with monday and holiday variables
# only twitter variables
best.preds.twitter = c(names(which.min(cv.adjerr.mods.twitter.clean)), names(which.min(cv.adjerr.mods.twitter.full)))

mod.twitter.1 = glm(reformulate(best.preds.twitter, "numtoday"),
                    data = na.omit(twitter.train), family = quasipoisson)

cv.mod.twitter.1 = cv.glm(data = twitter.train[complete.cases(twitter.train), c("numtoday", best.preds.twitter)],
                          glmfit = mod.twitter.1, K = 5)
cv.adjerr.mod.twitter.1 = get_adj_cv_error(cv.mod.twitter.1)



# twitter vars and holiday
mod.twitter.2 = glm(reformulate(c(best.preds.twitter, "holiday"), "numtoday"),
                    data = na.omit(twitter.train), family = quasipoisson)

cv.mod.twitter.2 = cv.glm(data = twitter.train[complete.cases(twitter.train), c("numtoday", best.preds.twitter, "holiday")],
                          glmfit = mod.twitter.2, K = 5)
cv.adjerr.mod.twitter.2 = get_adj_cv_error(cv.mod.twitter.2)



# twitter vars and monday
mod.twitter.3 = glm(reformulate(c(best.preds.twitter, "monday"), "numtoday"),
                    data = na.omit(twitter.train), family = quasipoisson)

cv.mod.twitter.3 = cv.glm(data = twitter.train[complete.cases(twitter.train), c("numtoday", best.preds.twitter, "monday")],
                          glmfit = mod.twitter.3, K = 5)
cv.adjerr.mod.twitter.3 = get_adj_cv_error(cv.mod.twitter.3)


cv.adjerr.mod.twitter.1 = unlist(cv.adjerr.mod.twitter.1)
cv.adjerr.mod.twitter.2 = unlist(cv.adjerr.mod.twitter.2)
cv.adjerr.mod.twitter.3 = unlist(cv.adjerr.mod.twitter.3)

# compare multiple predictor mods above to single predictor models
sort(c("multi pred" = cv.adjerr.mod.twitter.1, 
       "holiday" = cv.adjerr.mod.twitter.2, 
       "monday" = cv.adjerr.mod.twitter.3,
       "twitter_clean" = min(cv.adjerr.mods.twitter.clean),
       "twitter_full" = min(cv.adjerr.mods.twitter.full)))

# Result: the model with twitter and monday predictors has the lowest adj cv error.


# plot fitted values to training data
plot(1:nrow(twitter.train), twitter.train$numtoday, type = "l", 
     xlab = "", ylab = "Cases", xaxt = "n", main = "Twitter Model Predictions", lwd = 2) # actual
axis(1, at = 1:nrow(twitter.train), labels = twitter.train$date, las = 3)
points(31:nrow(twitter.train), mod.twitter.1$fitted.values, type = "l", col = 2) # mult pred (twitter only) mod
points(31:nrow(twitter.train), mod.twitter.2$fitted.values, type = "l", col = 3) # mult pred (with holiday) mod
points(31:nrow(twitter.train), mod.twitter.3$fitted.values, type = "l", col = 4) # mult pred (with monday) mod
points(31:nrow(twitter.train), mods.twitter.clean[[which.min(cv.adjerr.mods.twitter.clean)]]$fitted.values, 
       type = "l", col = 5) # single pred (clean) mod
points(31:nrow(twitter.train), mods.twitter.full[[which.min(cv.adjerr.mods.twitter.full)]]$fitted.values, 
       type = "l", col = 6)# single pred (full) mod
legend(5, 2600, 
       legend = c("actual", "twitter mult", "twitter + holiday", "twitter + monday", "twitter (clean)", "twitter (full)"),
       col = c(1, 2, 3, 4, 5, 6), ncol = 1, cex = 0.8, pt.cex = 1.8, pch = 20)




### Model labour data and cases ###

labour.t = data.frame("date" = labour.unemployment$date, "unemployed" = labour.unemployment$VALUE, 
                      "employed" = labour.employment$VALUE, "numtoday" = labour.employment$numtoday)



# add lags
for (lag in 0:30) {
  labour.t = slide(labour.t, Var = "unemployed", NewVar = paste0("unemployed", n[[lag+1]]), slideBy = -lag)
  labour.t = slide(labour.t, Var = "employed", NewVar = paste0("employed", n[[lag+1]]), slideBy = -lag)
}


# separate into training and testing subsets
labour.train = labour.t[1:round(0.8*nrow(labour.t)),]
labour.test = labour.t[(round(0.8*nrow(labour.t))+1):nrow(labour.t),]



# make separate sets of models (with separate unemployed and employed lagged labour variables)
labour.mods.unem = list()
cv.labour.mods.unem = list()
cv.adjerr.labour.mods.unem = list()


for (colname in colnames(labour.t)[startsWith(colnames(labour.t), "unemployed_lag")]) {
  
  mod = glm(reformulate(colname, "numtoday"), 
            data = na.omit(labour.train), family = quasipoisson)
  
  cv.mod = cv.glm(data = labour.train[complete.cases(labour.train), c("numtoday", colname)],
                  glmfit = mod, K = 5)
  
  labour.mods.unem[[colname]] = mod
  cv.labour.mods.unem[[colname]] = cv.mod
  cv.adjerr.labour.mods.unem[[colname]] = get_adj_cv_error(cv.mod)
  
}



labour.mods.em = list()
cv.labour.mods.em = list()
cv.adjerr.labour.mods.em = list()


for (colname in colnames(labour.t)[startsWith(colnames(labour.t), "employed_lag")]) {
  
  mod = glm(reformulate(colname, "numtoday"), data = na.omit(labour.train),
            family = quasipoisson)
  
  cv.mod = cv.glm(data = labour.train[complete.cases(labour.train),c("numtoday", colname)],
                  glmfit = mod, K = 5)
  
  labour.mods.em[[colname]] = mod
  cv.labour.mods.em[[colname]] = cv.mod
  cv.adjerr.labour.mods.em[[colname]] = get_adj_cv_error(cv.mod)
  
}

cv.adjerr.labour.mods.unem = unlist(cv.adjerr.labour.mods.unem)
cv.adjerr.labour.mods.em = unlist(cv.adjerr.labour.mods.em)

# show which lag produces the smallest adj cv error 
which.min(cv.adjerr.labour.mods.unem)
which.min(cv.adjerr.labour.mods.em)

# compare best <unemployed> and <employed> variables
sort(c("unem" = min(cv.adjerr.labour.mods.unem), "em" = min(cv.adjerr.labour.mods.em)))

# Result: the employed model produces the lower adj.cv error.



# plot model predictions to fitted values
plot(1:nrow(labour.train), labour.train$numtoday, type = "l", xlab = "", ylab = "Cases", xaxt = "n",
     main = "Labour Model Predictions", lwd = 2)
axis(1, at = 1:nrow(labour.train), labels = labour.train$date, las = 3)

points(31:nrow(labour.train), labour.mods.em[[which.min(cv.adjerr.labour.mods.em)]]$fitted.values, 
       type = "l", col = 2) # plot employed
points(31:nrow(labour.train), labour.mods.unem[[which.min(cv.adjerr.labour.mods.unem)]]$fitted.values, 
       type = "l", col = 3) # plot employed
legend(5, 2700, 
       legend = c("actual", "employed", "unemployed"), col = c(1,2,3), 
       ncol = 1, cex = 0.8, pt.cex = 1.8, pch = 20)






### Model cases and retail data

retail.t = data.frame("ecommerce" = retail.ecommerce$VALUE, "electronic" = retail.electronic$VALUE,
                 "trade" = retail.trade$VALUE, "numtoday" = cases.full.imp$numtoday, "date" = cases.full.imp$date)
retail.train = retail.t[1:round(0.8*nrow(retail.t)),]
retail.test = retail.t[(round(0.8*nrow(retail.t))+1):nrow(retail.t),]


# models
# retail ecommerce sales predictor
mod.retail.1 = glm(numtoday ~ ecommerce, data = retail.train, family = quasipoisson)
cv.mod.retail.1 = cv.glm(data = retail.train[,c("numtoday", "ecommerce")], 
                         glmfit = mod.retail.1, K = 5)
cv.adjerr.mod.retail.1 = get_adj_cv_error(cv.mod.retail.1)



# electronic shopping and mail-order houses predictor
mod.retail.2 = glm(numtoday ~ electronic, data = retail.train, family = quasipoisson)
cv.mod.retail.2 = cv.glm(data = retail.train[,c("numtoday", "electronic")], 
                         glmfit = mod.retail.2, K = 5)
cv.adjerr.mod.retail.2 = get_adj_cv_error(cv.mod.retail.2)



# retail trade predictor
mod.retail.3 = glm(numtoday ~ trade, data = retail.train, family = quasipoisson)
cv.mod.retail.3 = cv.glm(data = retail.train[,c("numtoday", "trade")], 
                         glmfit = mod.retail.3, K = 5)
cv.adjerr.mod.retail.3 = get_adj_cv_error(cv.mod.retail.3)


cv.adjerr.mod.retail.1 = unlist(cv.adjerr.mod.retail.1) 
cv.adjerr.mod.retail.2 = unlist(cv.adjerr.mod.retail.2) 
cv.adjerr.mod.retail.3 = unlist(cv.adjerr.mod.retail.3) 


# compare cv errors
sort(c("ecommerce" = cv.adjerr.mod.retail.1, "electronic" = cv.adjerr.mod.retail.2, 
       "retail trade" = cv.adjerr.mod.retail.3))

retail.best.pred = names(which.min(c("ecommerce" = cv.adjerr.mod.retail.1, "electronic" = cv.adjerr.mod.retail.2, 
       "retail trade" = cv.adjerr.mod.retail.3)))

# Result: the ecommerce model produces the lowest adj cv error.


plot(1:nrow(retail.train), retail.train$numtoday, type = "l", xlab = "", ylab = "Cases",
     xaxt = "n", main = "Retail Model Predictions", lwd = 2)
axis(1, at = 1:nrow(retail.train), retail.train$date, las = 3)
points(1:nrow(retail.train), mod.retail.1$fitted.values, type = "l", col = 2) # retail ecommerce
points(1:nrow(retail.train), mod.retail.2$fitted.values, type = "l", col = 3) # electronic
points(1:nrow(retail.train), mod.retail.3$fitted.values, type = "l", col = 4) # retail trade
legend(5, 2700, 
       legend = c("actual", "ecommerce", "electronic", "retail"), col = c(1, 2, 3, 4),
       pch = 20, ncol = 1, cex = 0.8, pt.cex = 1.8)




##### Combine all relevant data types #####

# all relevant data, without lags

# find most revelant variables form above, just without lags and make an ARIMA model for it
data.all.auto = data.frame("date" = cases.full.imp$date,  
                           "numtoday" = cases.full.imp$numtoday, 
                           "holiday" = cases.full.imp$holiday, 
                           "monday" = cases.full.imp$monday,
                           
                           
                           stocks.full.imp[,-match("Date", colnames(stocks.full.imp))], 
                           
                           "num_flights_total" = flights.per.day$num_flights, 
                           "num_flights_intl" = intl.flights.per.day$num_flights, 
                           "num_flights_dom" = dom.flights.per.day$num_flights,
                           
                           mobility.g.aggregate.full.imp[,3:ncol(mobility.g.aggregate.full.imp)],
                           mobility.a.full.imp[,6:ncol(mobility.a.full)],
                           
                           policies[,c("GovernmentResponseIndex", "ContainmentHealthIndex", "StringencyIndex")],
                           riskopen.imp[,c("community_understanding", "openness_risk")],
                           
                           queries[,2:ncol(queries)], 
                           "unweighted_index" = queries_unweighted$index,
                           "weighted_index" = queries_weighted$index,
                           "weighted_index_2" = queries_weighted_2$index_lag0,
                           pop_by_date_imp[,c("popularity", "cumul_pop")],
                           
                           "total_tweets_full" = twitter.stats.full$total_tweets,
                           "total_tweets_clean" = twitter.stats.clean$total_tweets,
                           
                           "weighted_sent_terms" = weighted_sent_terms$weighted_sent,
                           "weighted_sent_bigrams" = weighted_sent_bigrams$weighted_sent,
                           "weighted_sent_trigrams" = weighted_sent_trigrams$weighted_sent,
                           
                           "sent_pubs_title" = mean_sent_title$mean_sent,
                           "sent_pubs_abstract" = mean_sent_abstract$mean_sent,
                           
                           "labour_employment" = labour.employment$VALUE,
                           "labour_unemployment" = labour.unemployment$VALUE,
                           
                           "avg_mean_temp" = weather.canada$CAN.AVG_MEAN_TEMPERATURE,
                           "avg_max_rel_hum" = weather.canada$CAN.AVG_MAX_REL_HUMIDITY,
                           "avg_min_rel_hum" = weather.canada$CAN.AVG_MIN_REL_HUMIDITY,
                           
                           "retail_trade" = retail.trade$VALUE,
                           "electronic_shopping_mail_order" = retail.electronic$VALUE,#,
                           "ecommerce" = retail.ecommerce$VALUE)

data.all.auto.train = data.all.auto[1:round(0.8*nrow(data.all.auto)),]
data.all.auto.test = data.all.auto[(round(0.8*nrow(data.all.auto))+1):nrow(data.all.auto),]



# all relevant data, with best predictors and lags determined in <Modelling> section above
data.all.pre.1 = data.frame("date" = cases.full.imp$date, 
                          "numtoday" = cases.full.imp$numtoday, 
                          "holiday" = cases.full.imp$holiday, 
                          "monday" = cases.full.imp$monday,
                          
                          "Close_SP500_lag3" = stocks.t[,names(which.min(cv.adjerr.glm.lags.sp500.close))], 
                          
                          flights.per.day.t[,most.pred.multindex.flights], 
                          
                          mobility.all[,names(mobility.mod.2$coefficients)[3:length(names(mobility.mod.2$coefficients))]], # exclude monday because already stored above
                          
                          "community_understanding_lag6" = policies.t[,names(which.min(cv.adjerr.mod.policies.community))],

                          "index_lag28" = queries_unweighted[,names(which.min(nbs_cv_adjerror))], 
                          
                          twitter.t[,best.preds.twitter], # monday var removed because it is already included above
                          
                          labour.t[,names(which.min(cv.adjerr.labour.mods.em))],
                          
                          weather.t[,best.weather.preds],
                          
                          retail.t[,retail.best.pred])

for (lag in 1:14) {
  data.all.pre.1 = slide(data.all.pre.1, Var = "numtoday", NewVar = paste0("numtoday_lag", lag), slideBy = -lag)
}


# edit some column names
colnames(data.all.pre.1)[grep("retail.t", colnames(data.all.pre.1))] = retail.best.pred
colnames(data.all.pre.1)[grep("labour", colnames(data.all.pre.1))] = names(which.min(cv.adjerr.labour.mods.em))

data.all.pre.1.train = data.all.pre.1[1:round(0.8*nrow(data.all.pre.1)),]
data.all.pre.1.test = data.all.pre.1[(round(0.8*nrow(data.all.pre.1))+1):nrow(data.all.pre.1),]



data.all.pre.2 = data.frame("date" = cases.full.imp$date, 
                            "numtoday" = cases.full.imp$numtoday, 
                            "holiday" = cases.full.imp$holiday, 
                            "monday" = cases.full.imp$monday,
                            
                            "Close_SP500_lag3" = stocks.t[,names(which.min(cv.adjerr.glm.lags.sp500.close))], 
                            
                            flights.per.day.t[,most.pred.multindex.flights], 
                            
                            mobility.all[,names(mobility.mod.2$coefficients)[3:length(names(mobility.mod.2$coefficients))]], # exclude monday because already stored above
                            
                            "community_understanding_lag6" = policies.t[,names(which.min(cv.adjerr.mod.policies.community))],
                            
                            "index_lag28" = queries_unweighted[,names(which.min(nbs_cv_adjerror))], 
                            
                            twitter.t[,best.preds.twitter], # monday var removed because it is already included above
                            
                            labour.t[,names(which.min(cv.adjerr.labour.mods.em))],
                            
                            weather.t[,best.weather.preds],
                            
                            retail.t[,retail.best.pred],
                            
                            
                            "weighted_sent_terms" = weighted_sent_terms$weighted_sent,
                            "weighted_sent_bigrams" = weighted_sent_bigrams$weighted_sent,
                            "weighted_sent_trigrams" = weighted_sent_trigrams$weighted_sent,
                            
                            "sent_pubs_title" = mean_sent_title$mean_sent,
                            "sent_pubs_abstract" = mean_sent_abstract$mean_sent)


colnames(data.all.pre.2)[grep("retail.t", colnames(data.all.pre.2))] = retail.best.pred
colnames(data.all.pre.2)[grep("labour", colnames(data.all.pre.2))] = names(which.min(cv.adjerr.labour.mods.em))


for (lag in 1:14) {
  data.all.pre.2 = slide(data.all.pre.2, Var = "numtoday", NewVar = paste0("numtoday_lag", lag), slideBy = -lag)
}



data.all.pre.2.train = data.all.pre.2[1:round(0.8*nrow(data.all.pre.2)),]
data.all.pre.2.test = data.all.pre.2[(round(0.8*nrow(data.all.pre.2))+1):nrow(data.all.pre.2),]



##### Performance Metrics #####
# To be used to compare ensemble models

# root mean squared error
rmse = function(predicted, testdata) {
  return(sqrt((1/length(predicted))*sum((predicted - testdata$numtoday)^2)))
}

# root mean squared percentage error
rmspe = function(predicted, testdata) {
  return(sqrt((1/length(predicted))*sum(((predicted - testdata$numtoday)/testdata$numtoday)^2))*100)
}

# mean absolute percentage error
mape = function(predicted, testdata) {
  return((1/length(predicted))*sum(abs(predicted - testdata$numtoday)/testdata$numtoday)*100)
}


##### Ensemble Models #####


### Using predetermined external variable lags (from Modelling section) ###

### ARIMA model ###
# fit the model to training data
vars.remove = c(grep("numtoday_lag", colnames(data.all.pre.1.train)), 
                      match(c("date", "numtoday"), colnames(data.all.pre.1.train)))

arima.pre.1 = auto.arima(y = ts(data.all.pre.1.train$numtoday, start = 1, frequency = 1),
                         lambda = "auto",
                         seasonal = FALSE,
                         xreg = as.matrix(data.all.pre.1.train[,-vars.remove.arima]))


# predict future values based on external regressors in testing data
forecast.arima.pre.1 = forecast(arima.pre.1, 
                              xreg = as.matrix(data.all.pre.1.test[,-vars.remove.arima]))



# performance metrics
# RMSE
rmse.arima = rmse(forecast.arima.pre.1$mean, data.all.pre.1.test)
rmse.arima

# RMSPE
rmspe.arima = rmspe(forecast.arima.pre.1$mean, data.all.pre.1.test)
rmspe.arima

# MAPE
mape.arima = mape(forecast.arima.pre.1$mean, data.all.pre.1.test)
mape.arima

# pearson correlation 
cor.arima = cor(forecast.arima.pre.1$mean, data.all.pre.1.test$numtoday)
cor.arima


# add AR terms to see if that improves the fit above
arima.mods.with.ar = list()
arima.mods.with.ar.predict = list()

for (lag in 1:14) {
  
  vars.to.include = c(colnames(data.all.pre.1.train[,-c(match(c("date", "numtoday"), 
                                                              colnames(data.all.pre.1.train)), 
                                                        grep("numtoday_lag", colnames(data.all.pre.1.train)))]),
                      colnames(data.all.pre.1.train)[match(paste0("numtoday_lag", lag), colnames(data.all.pre.1.train))])
  
  # fit model
  mod = auto.arima(y = ts(data.all.pre.1.train$numtoday, start = 1, frequency = 1),
                   lambda = "auto", seasonal = FALSE, xreg = as.matrix(data.all.pre.1.train[,vars.to.include]))
  
  arima.mods.with.ar[[paste0("numtoday_lag", lag)]] = mod
  
  
  # make prediction
  forecast.mod = forecast(mod, xreg = as.matrix(data.all.pre.1.test[,vars.to.include]))
  
  arima.mods.with.ar.predict[[paste0("numtoday_lag", lag)]] = forecast.mod$mean
  
  
}


# RMSE
rmse.arima.with.ar = sapply(arima.mods.with.ar.predict, rmse, testdata = data.all.pre.1.test)


# RMSPE
rmspe.arima.with.ar = sapply(arima.mods.with.ar.predict, rmspe, testdata = data.all.pre.1.test)

# MAPE
mape.arima.with.ar = sapply(arima.mods.with.ar.predict, mape, testdata = data.all.pre.1.test)

# correlation
cor.arima.with.ar = sapply(arima.mods.with.ar.predict, cor , y = data.all.pre.1.test$numtoday)


sort(c(rmse.arima.with.ar, "without AR" = rmse.arima))
sort(c(rmspe.arima.with.ar, "without AR" = rmspe.arima))
sort(c(mape.arima.with.ar, "without AR" = mape.arima))
sort(c(cor.arima.with.ar, "without AR" = cor.arima))


# Result: including lag 7 produces the better model for rmse, rmspe, mape and correlation. 
 


# ARIMA: plot fitted values over observed training data 
plot(1:nrow(data.all.pre.1.train), data.all.pre.1.train$numtoday, type = "l", 
     main = "ARIMA Ensemble Model - Training Data", 
     xlab = "", ylab = "Cases", xaxt = "n", lwd = 2)
axis(1, at = 1:nrow(data.all.pre.1.train), labels = data.all.pre.1.train$date, las = 3)
points(1:nrow(data.all.pre.1.train), arima.pre.1$fitted, type = "l", col = 3) # without AR
points(1:nrow(data.all.pre.1.train), arima.mods.with.ar[[which.min(rmse.arima.with.ar)]]$fitted, 
       type = "l", col = 4)
legend(5, 2700, 
       legend = c("actual", "without AR", "with AR"), col = c(1, 2, 3),
       pch = 20, ncol = 1, cex = 0.8, pt.cex = 1.8)


# ARIMA: plot forecasted values (test data)
plot(1:nrow(data.all.pre.1.test), data.all.pre.1.test$numtoday, type = "l",
     main = "ARIMA Ensemble Model - Testing Data",
     xlab = "", ylab = "Cases", xaxt = "n", lwd = 2)
axis(1, at = 1:nrow(data.all.pre.1.test), labels = data.all.pre.1.test$date, las = 3)
points(1:nrow(data.all.pre.1.test), forecast.arima.pre.1$mean, type = "l", col = 3)
points(1:nrow(data.all.pre.1.test), arima.mods.with.ar.predict[[which.min(rmse.arima.with.ar)]],
       type = "l", col = 4)
legend(5, 1000, 
       legend = c("actual", "without AR", "with AR"), col = c(1, 2, 3),
       pch = 20, ncol = 1, cex = 0.8, pt.cex = 1.8)




### Random Forest ###

rf.pre.1 = randomForest(reformulate(colnames(data.all.pre.1.train[,-vars.remove]), response = "numtoday"),
                        data = data.all.pre.1.train, importance = TRUE, na.action = na.omit, mtry = 10)

rf.predict.1 = predict(rf.pre.1, newdata = data.all.pre.1.test, type = "response")


# plot error vs. number of trees 
plot(rf.pre.1)

# see which variables were used
varUsed(rf.pre.1)


# variable importance
rf.pre.1$importance
rf.pre.1$importance[,2][order(rf.pre.1$importance[,2], decreasing = T)] # order vars from most to least important
varImpPlot(rf.pre.1, sort = TRUE) # plot variables with most important ones at the top


# variable importance shows that the humidity variables are not important
# re-train the model without the humidity variables

# indices of unimportant vars to be removed
rf.remove.var.ind = c(grep("REL_HUMIDITY", colnames(data.all.pre.1.train)), 
                      grep("date", colnames(data.all.pre.1.train)), 
                      grep("numtoday", colnames(data.all.pre.1.train)), 
                      grep("monday", colnames(data.all.pre.1.train)), 
                      grep("holiday", colnames(data.all.pre.1.train)))


# random forest
rf.pre.2 = randomForest(reformulate(colnames(data.all.pre.1.train[,-rf.remove.var.ind]), response = "numtoday"),
                        data = data.all.pre.1.train, importance = TRUE, na.action = na.omit, mtry = 10)

# predictions
rf.predict.2 = predict(rf.pre.2, newdata = data.all.pre.1.test, type = "response")


# plot error vs. number of trees
plot(rf.pre.2)

# see which variables were used
varUsed(rf.pre.2)


# variable importance
rf.pre.2$importance
rf.pre.2$importance[,2][order(rf.pre.2$importance[,2], decreasing = T)]
varImpPlot(rf.pre.2, sort = T)


# compare prediction performance of random forest models on test data

# RMSE (in counts)
rmse.rf.1 = rmse(rf.predict.1, data.all.pre.1.test)
rmse.rf.2 = rmse(rf.predict.2, data.all.pre.1.test)
sort(c("all vars" = rmse.rf.1, "removed vars" = rmse.rf.2))

# RMSPE (in percentage)
rmspe.rf.1 = rmspe(rf.predict.1, data.all.pre.1.test)
rmspe.rf.2 = rmspe(rf.predict.2, data.all.pre.1.test)
sort(c("all vars" = rmspe.rf.1, "removed vars" = rmspe.rf.2))

# MAPE (in percentage)
mape.rf.1 = mape(rf.predict.1, data.all.pre.1.test)
mape.rf.2 = mape(rf.predict.2, data.all.pre.1.test)
sort(c("all vars" = mape.rf.1, "removed vars" = mape.rf.2))

# correlation
cor.rf.1 = cor(rf.predict.1, data.all.pre.1.test$numtoday)
cor.rf.2 = cor(rf.predict.2, data.all.pre.1.test$numtoday)
cor.rf.1
cor.rf.2

# All vars model performs better on test data.


# add AR terms to model to see if that improves prediction

rf.mods.with.ar = list()
rf.mods.predict = list()

for (lag in 1:14) {
  
  vars.to.include = c(colnames(data.all.pre.1.train[,-c(match(c("date", "numtoday"), 
                                                              colnames(data.all.pre.1.train)), 
                                                        grep("numtoday_lag", colnames(data.all.pre.1.train)))]),
                      colnames(data.all.pre.1.train)[match(paste0("numtoday_lag", lag), colnames(data.all.pre.1.train))])
  
  
  # fit random forest
  mod = randomForest(reformulate(colnames(data.all.pre.1.train[,vars.to.include]), response = "numtoday"),
               data = data.all.pre.1.train, importance = TRUE, na.action = na.omit, mtry = 10)
  
  rf.mods.with.ar[[paste0("numtoday_lag", lag)]] = mod
  
  
    # make predictions on test data
  predictions = predict(mod, newdata = data.all.pre.1.test, type = "response")
  rf.mods.predict[[paste0("numtoday_lag", lag)]] = predictions
  
}

# RMSE
rf.mods.with.ar.rmse = sapply(rf.mods.predict, rmse, testdata = data.all.pre.1.test)

# RMSPE
rf.mods.with.ar.rmspe = sapply(rf.mods.predict, rmspe, testdata = data.all.pre.1.test)

# MAPE
rf.mods.with.ar.mape = sapply(rf.mods.predict, mape, testdata = data.all.pre.1.test)

# correlation
rf.mods.with.ar.cor = sapply(rf.mods.predict, cor, y = data.all.pre.1.test$numtoday)

which.min(rf.mods.with.ar.rmse)
which.min(rf.mods.with.ar.rmspe)
which.min(rf.mods.with.ar.mape)
which.min(rf.mods.with.ar.cor)


# compare metrics with original rf without AR
sort(c(rf.mods.with.ar.rmse, "without AR" = rmse.rf.1))
sort(c(rf.mods.with.ar.rmspe, "without AR" = rmspe.rf.1))
sort(c(rf.mods.with.ar.mape, "without AR" = mape.rf.1))
sort(c(rf.mods.with.ar.cor, "without AR" = cor.rf.1))

# Result: model with AR term performs better.



# plot random forest estimates on training data
plot(1:nrow(data.all.pre.1.train), data.all.pre.1.train$numtoday, type = "l",
     xlab = "", ylab = "Count", main = "Random Forest Ensemble Model - Training", xaxt = "n")
axis(1, at = 1:nrow(data.all.pre.1.train), data.all.pre.1.train$date, las = 3, lwd = 2)
points(29:nrow(data.all.pre.1.train), rf.pre.1$predicted, type = "l", col = 2)
points(29:nrow(data.all.pre.1.train), rf.pre.2$predicted, type = "l", col = 3)
points(29:nrow(data.all.pre.1.train), 
       rf.mods.with.ar[[which.min(rf.mods.with.ar.rmse)]]$predicted, type = "l", col = 4)
points(29:nrow(data.all.pre.1.train), 
       rf.mods.with.ar[[which.min(rf.mods.with.ar.mape)]]$predicted, type = "l", col = 5)
legend(0, 2600, c("actual", "RF1", "RF2", "RF with AR (lowest RMSE)", "RF with AR (lowest MAPE)"), 
       col = c(1, 2, 3, 4, 5), pch = 20, cex = 0.8, pt.cex = 1.8)




# plot random forest estimates on test data
plot(1:nrow(data.all.pre.1.test), data.all.pre.1.test$numtoday, type = "l",
     xlab = "", ylab = "Count", main = "Random Forest Ensemble Model - Testing", xaxt = "n")
axis(1, at = 1:nrow(data.all.pre.1.test), labels = data.all.pre.1.test$date, las = 3)
points(1:nrow(data.all.pre.1.test), rf.predict.1, type = "l", col = 2) # without AR
points(1:nrow(data.all.pre.1.test), rf.predict.2, type = "l", col = 3) # without AR
points(1:nrow(data.all.pre.1.test), rf.mods.predict[[which.min(rf.mods.with.ar.rmse)]], type = "l", col = 4) # best from rmse
points(1:nrow(data.all.pre.1.test), rf.mods.predict[[which.min(rf.mods.with.ar.mape)]], type = "l", col = 5) # best from mape

legend(5, 1000, c("actual", "RF1", "RF2", "RF with AR (lowest RMSE)", "RF with AR (lowest MAPE)"), 
       col = c(1, 2, 3, 4, 5), pch = 20, cex = 0.8, pt.cex = 1.8)




### Lasso ###

# model matrix 
mat = model.matrix(reformulate(colnames(data.all.pre.1.train[,-vars.remove]), "numtoday"), 
                   data = data.all.pre.1.train)


# get response vector for complete cases in the data (some cases have missing values due to lagged vars)
y.mat = data.all.pre.1.train[complete.cases(data.all.pre.1.train),]$numtoday


# fit Lasso glmnet model
glmnet.pre.1 = glmnet(x = mat, y = y.mat, family = "gaussian")
# Note: select gaussian instead of poisson (even though the data are counts) 
# because a poisson model produces near-zero estimates across the data timeline.


# glmnet returns a selection of models with diff numbers of variables, so
# use cv to select the ideal model
cv.glmnet.pre.1 = cv.glmnet(x = mat, y = y.mat)


# this lambda val gives min mean cv error
cv.glmnet.pre.1$lambda.min 


# variables selected for min mean cv error
coef(cv.glmnet.pre.1, s = "lambda.min")
cvm = as.matrix(coef(cv.glmnet.pre.1, s = "lambda.min"))
rownames(cvm)[cvm != 0]

  
# number of variables included in model with lowest mean cv error
glmnet.pre.1$df[round(cv.glmnet.pre.1$lambda.min,6) == round(glmnet.pre.1$lambda,6)]


# fit model to training data using lambda.min 
fit.train.glmnet.1 = predict(glmnet.pre.1, newx = mat, s = cv.glmnet.pre.1$lambda.min)


# make predictions on test data using lambda.min 
x.p = model.matrix(reformulate(colnames(data.all.pre.1.test[,-vars.remove]), "numtoday"), 
                   data = data.all.pre.1.test)

predict.glmnet.1 = predict(glmnet.pre.1, newx = x.p, s = cv.glmnet.pre.1$lambda.min)


# performance metric
# RMSE
rmse.glmnet = rmse(predict.glmnet.1, data.all.pre.1.test)
rmse.glmnet

# RMSPE
rmspe.glmnet = rmspe(predict.glmnet.1, data.all.pre.1.test)
rmspe.glmnet

# MAPE
mape.glmnet = mape(predict.glmnet.1, data.all.pre.1.test)
mape.glmnet

# correlation 
cor.glmnet = cor(predict.glmnet.1, data.all.pre.1.test$numtoday)
cor.glmnet

# Fitting a poisson family model causes nearly zero case counts across the entire timeline, 
# so gaussian family needs to be used. 



# add case lags to the models to see if that improves case counts
glmnet.mods.with.ar = list()
glmnet.mods.cv = list()
glmnet.mods.predict = list()
glmnet.mods.train = list()

for (lag in 1:14) {
  
  vars.to.include = c(colnames(data.all.pre.1.train[,-c(match(c("date", "numtoday"), 
                                                          colnames(data.all.pre.1.train)), 
                                                    grep("numtoday_lag", colnames(data.all.pre.1.train)))]),
                    colnames(data.all.pre.1.train)[match(paste0("numtoday_lag", lag), colnames(data.all.pre.1.train))])
  
  
  # make a model matrix
  mat = model.matrix(reformulate(colnames(data.all.pre.1.train[,vars.to.include]), "numtoday"), 
                     data = data.all.pre.1.train)
  

  # make a response matrix
  y.mat = data.all.pre.1.train[complete.cases(data.all.pre.1.train),]$numtoday
  
  
  # model
  mod = glmnet(x = mat, y = y.mat, family = "gaussian")
  glmnet.mods.with.ar[[paste0("numtoday_lag" , lag)]] = mod
  
  
  # cross validation
  cv.mod = cv.glmnet(x = mat, y = y.mat)
  glmnet.mods.cv[[paste0("numtoday_lag" , lag)]] = cv.mod
  
  
  # fit model to training data using lambda.min 
  mod.train = predict(mod, newx = mat, s = cv.mod$lambda.min)
  glmnet.mods.train[[paste0("numtoday_lag" , lag)]] = mod.train
  
  
  # make predictions on test data using lambda.min 
  x.p = model.matrix(reformulate(colnames(data.all.pre.1.test[,vars.to.include]), "numtoday"), 
                     data = data.all.pre.1.test)
  
  predictions = predict(mod, newx = x.p, s = cv.mod$lambda.min)
  glmnet.mods.predict[[paste0("numtoday_lag" , lag)]] = predictions
  
  
}


# performance metrics for models above
# RMSE
rmse.glmnet.ar = sapply(glmnet.mods.predict, rmse, testdata = data.all.pre.1.test)

# RMSPE
rmspe.glmnet.ar = sapply(glmnet.mods.predict, rmspe, testdata = data.all.pre.1.test)

# MAPE
mape.glmnet.ar = sapply(glmnet.mods.predict, mape, testdata = data.all.pre.1.test)

# correlation
cor.glmnet.ar = sapply(glmnet.mods.predict, cor, y = data.all.pre.1.test$numtoday)
cor.glmnet.ar

# find which lags produce best performing models
which.min(rmse.glmnet.ar)
which.min(rmspe.glmnet.ar)
which.min(mape.glmnet.ar)
which.min(cor.glmnet.ar)



# compare models with AR terms to model without AR terms
sort(c(rmse.glmnet.ar, "without AR" = rmse.glmnet))
sort(c(rmspe.glmnet.ar, "without AR" = rmspe.glmnet))
sort(c(mape.glmnet.ar, "without AR" = mape.glmnet))

# Result: adding AR term improves performance.


# make plots using best performing glmnet model with AR term, and model without AR term
plot(1:nrow(data.all.pre.1.train), data.all.pre.1.train$numtoday, type = "l", 
     main = "Lasso Predictions - Training",
     xlab = "", ylab  = "Cases", xaxt = "n", lwd = 2)
axis(1, at = 1:nrow(data.all.pre.1.train), data.all.pre.1.train$date, las = 3)
points(29:nrow(data.all.pre.1.train), fit.train.glmnet.1, type = "l", col = 2)
points(29:nrow(data.all.pre.1.train), glmnet.mods.train[[which.min(rmse.glmnet.ar)]], type = "l", col = 3)
legend(5, 2000, c("actual", "without AR", "with AR"), 
       col = c(1, 2, 3), pch = 20, cex = 0.8, pt.cex = 1.8)


# plot predictions on testing data
plot(1:nrow(data.all.pre.1.test), data.all.pre.1.test$numtoday, type = "l", 
     main = "Lasso Predictions - Testing", lwd = 2,
     xlab = "", ylab  = "Cases", xaxt = "n", ylim = c(min(data.all.pre.1.test$numtoday, predict.glmnet.1), 
                                                      max(data.all.pre.1.test$numtoday, predict.glmnet.1)))
axis(1, at = 1:nrow(data.all.pre.1.test), data.all.pre.1.test$date, las = 3)
points(1:nrow(data.all.pre.1.test), predict.glmnet.1, type = "l", col = 2)
points(1:nrow(data.all.pre.1.test), glmnet.mods.predict[[which.min(rmse.glmnet.ar)]], type = "l", col = 3)
legend(5, 1000, c("actual", "without AR", "with AR"), 
       col = c(1, 2, 3), pch = 20, cex = 0.8, pt.cex = 1.8)




### SVM ###

# fit svm
svm.pre.1.1 = svm(reformulate(colnames(data.all.pre.1.train[,-vars.remove]), "numtoday"),
    data = data.all.pre.1.train, scale = FALSE, cross = 5, type = "nu-regression") # radial kernel

svm.pre.1.2 = svm(reformulate(colnames(data.all.pre.1.train[,-vars.remove]), "numtoday"),
                  data = data.all.pre.1.train, scale = FALSE, cross = 5, 
                  kernel = "sigmoid", type = "nu-regression") # sigmoid kernel


# predictions
predict.svm.1.1 = predict(svm.pre.1.1, data.all.pre.1.test)
predict.svm.1.2 = predict(svm.pre.1.2, data.all.pre.1.test)


# RMSE
rmse.svm.1.1 = rmse(predict.svm.1.1, data.all.pre.1.test)
rmse.svm.1.2 = rmse(predict.svm.1.2, data.all.pre.1.test)


# RMSPE
rmspe.svm.1.1 = rmspe(predict.svm.1.1, data.all.pre.1.test)
rmspe.svm.1.2 = rmspe(predict.svm.1.2, data.all.pre.1.test)

# MAPE
mape.svm.1.1 = mape(predict.svm.1.1, data.all.pre.1.test)
mape.svm.1.2 = mape(predict.svm.1.2, data.all.pre.1.test)

# correlation
cor.svm.1.1 = cor(predict.svm.1.1, data.all.pre.1.test$numtoday)
cor.svm.1.2 = cor(predict.svm.1.2, data.all.pre.1.test$numtoday)


sort(c("RMSE radial kernel" = rmse.svm.1.1, "RMSE sigmoid kernel" = rmse.svm.1.2))
sort(c("RMSPE radial kernel" = rmse.svm.1.1, "RMSPE sigmoid kernel" = rmse.svm.1.2))
sort(c("MAPE radial kernel" = rmse.svm.1.1, "MAPE sigmoid kernel" = rmse.svm.1.2))
sort(c("cor radial kernel" = cor.svm.1.1, "cor sigmoid kernel" = cor.svm.1.2), decreasing = T)

# Note: the radial kernel performs better for all but cor metric. The sigmoid kernel has
# a stronger correlation to observed test data. 


# add lags to models above and compare performance to models without lags
svm.ar.mods.1 = list()
svm.ar.mods.2 = list()
predict.svm.ar.mod.1 = list()
predict.svm.ar.mod.2 = list()

for (lag in 1:14) {
  
  vars.to.include = c(colnames(data.all.pre.1.train[,-c(match(c("date", "numtoday"), 
                                                              colnames(data.all.pre.1.train)), 
                                                        grep("numtoday_lag", colnames(data.all.pre.1.train)))]),
                      colnames(data.all.pre.1.train)[match(paste0("numtoday_lag", lag), colnames(data.all.pre.1.train))])
  
  
  #  fit models 
  svm.1 = svm(reformulate(colnames(data.all.pre.1.train[,vars.to.include]),"numtoday"),
                    data = data.all.pre.1.train, scale = FALSE, cross = 5,
              type = "nu-regression") # radial kernel
  
  svm.2 = svm(reformulate(colnames(data.all.pre.1.train[,vars.to.include]), "numtoday"),
                    data = data.all.pre.1.train, scale = FALSE, cross = 5, 
                    kernel = "sigmoid",
              type = "nu-regression") # sigmoid kernel
  
  svm.ar.mods.1[[paste0("numtoday_lag", lag)]] = svm.1
  svm.ar.mods.2[[paste0("numtoday_lag", lag)]] = svm.2
  
  
  # make predictions
  predict.svm.1 = predict(svm.1, data.all.pre.1.test)
  predict.svm.2 = predict(svm.2, data.all.pre.1.test)
  
  predict.svm.ar.mod.1[[paste0("numtoday_lag", lag)]] = predict.svm.1
  predict.svm.ar.mod.2[[paste0("numtoday_lag", lag)]] = predict.svm.2
  
}


# RMSE
rmse.svm.ar.1.1 = sapply(predict.svm.ar.mod.1, rmse, testdata = data.all.pre.1.test)
rmse.svm.ar.1.2 = sapply(predict.svm.ar.mod.2, rmse, testdata = data.all.pre.1.test)
which.min(rmse.svm.ar.1.1)
which.min(rmse.svm.ar.1.2)
min(rmse.svm.ar.1.1)
min(rmse.svm.ar.1.2)

# RMSPE
rmspe.svm.ar.1.1 = sapply(predict.svm.ar.mod.1, rmspe, testdata = data.all.pre.1.test)
rmspe.svm.ar.1.2 = sapply(predict.svm.ar.mod.2, rmspe, testdata = data.all.pre.1.test)
which.min(rmspe.svm.ar.1.1)
which.min(rmspe.svm.ar.1.2)
min(rmspe.svm.ar.1.1)
min(rmspe.svm.ar.1.2)

# MAPE
mape.svm.ar.1.1 = sapply(predict.svm.ar.mod.1, mape, testdata = data.all.pre.1.test)
mape.svm.ar.1.2 = sapply(predict.svm.ar.mod.2, mape, testdata = data.all.pre.1.test)
which.min(mape.svm.ar.1.1)
which.min(mape.svm.ar.1.2)
min(mape.svm.ar.1.1)
min(mape.svm.ar.1.2)


# correlation
cor.svm.ar.1.1 = sapply(predict.svm.ar.mod.1, cor, y = data.all.pre.1.test$numtoday)
cor.svm.ar.1.2 = sapply(predict.svm.ar.mod.2, cor, y = data.all.pre.1.test$numtoday)
sort(cor.svm.ar.1.1)
sort(cor.svm.ar.1.2)


# compare models with and without AR term
sort(c("radial with AR" = min(rmse.svm.ar.1.1), "sigmoid with AR" = min(rmse.svm.ar.1.2),
     "radial only" = rmse.svm.1.1, "sigmoid only" = rmse.svm.1.2))

sort(c("radial with AR" = min(rmspe.svm.ar.1.1), "sigmoid with AR" = min(rmspe.svm.ar.1.2),
       "radial only" = rmspe.svm.1.1, "sigmoid only" = rmspe.svm.1.2))

sort(c("radial with AR" = min(mape.svm.ar.1.1), "sigmoid with AR" = min(mape.svm.ar.1.2),
       "radial only" = mape.svm.1.1, "sigmoid only" = mape.svm.1.2))

sort(c("radial with AR" = min(cor.svm.ar.1.1), "sigmoid with AR" = min(cor.svm.ar.1.2),
       "radial only" = cor.svm.1.1, "sigmoid only" = cor.svm.1.2))


# Result: radial kernel with AR cases model is the best.



# plot fit to training data
plot(1:nrow(data.all.pre.1.train), data.all.pre.1.train$numtoday, type = "l", 
     main = "SVM - Training", xlab = "", xaxt = "n", ylab = "Count", lwd = 2)
axis(1, 1:nrow(data.all.pre.1.train), data.all.pre.1.train$date, las = 3)
points(29:nrow(data.all.pre.1.train), svm.pre.1.1$fitted, type = "l", col = 2)
points(29:nrow(data.all.pre.1.train), svm.pre.1.2$fitted, type = "l", col = 3)
points(29:nrow(data.all.pre.1.train), svm.ar.mods.2[[which.min(rmse.svm.ar.1.2)]]$fitted, 
       type = "l", col = 4, lwd = 2)
legend(5, 2500, legend = c("actual", "SVM (radial)", "SVM (sigmoid)", "SVM (sigmoid) with AR"),
       col = c(1, 2, 3, 4), pch = 20, cex = 0.8, pt.cex = 1.8)


# plot predictions on test data
plot(1:nrow(data.all.pre.1.test), data.all.pre.1.test$numtoday, type = "l",
     xlab = "", ylab = "Cases", main = "SVM - Testing", xaxt = "n")
axis(1, at = 1:nrow(data.all.pre.1.test), labels = data.all.pre.1.test$date, las = 3)
points(1:nrow(data.all.pre.1.test), predict.svm.1.1, type = "l", col = 2)
points(1:nrow(data.all.pre.1.test), predict.svm.1.2, type = "l", col = 3)
points(1:nrow(data.all.pre.1.test), predict.svm.ar.mod.2[[which.min(rmse.svm.ar.1.2)]], 
       type = "l", col = 4)
legend(5, 1000, legend = c("actual", "SVM (radial)", "SVM (sigmoid)", "SVM (sigmoid) with AR"),
       col = c(1, 2, 3, 4), pch = 20, cex = 0.8, pt.cex = 1.8)




### Decision Trees ###

# fit decision tree
rp.pre.1 = rpart(reformulate(colnames(data.all.pre.1.train[,-vars.remove]), "numtoday"), 
                 data = data.all.pre.1.train, method = "poisson", cp = 0)


# show variables from most to least important
rp.pre.1$variable.importance

# model info
rf.pre.1

# cross validation results
printcp(rp.pre.1)
plotcp(rp.pre.1)



# To prune tree: use cp of tree that has xerror <= (smaller than or equal to) smallest xerror + sd

# index of smallest xerror
match(min(rp.pre.1$cptable[,c("xerror")]), rp.pre.1$cptable[,c("xerror")])

# sum smallest xerror and its corresponding sd
# return all the trees with xerror smaller than smallest xerror and its corresponding sd
min(rp.pre.1$cptable[rp.pre.1$cptable[,c("xerror")] <=
sum(rp.pre.1$cptable[match(min(rp.pre.1$cptable[,c("xerror")]), rp.pre.1$cptable[,c("xerror")]), c("xerror", "xstd")]) ,][,c("nsplit")])

# Note: smallest tree is of size nsplit = 5.


# show all tree sizes where error is smaller than or equal to smallest xerror + sd
rp.pre.1$cptable[rp.pre.1$cptable[,c("xerror")] <=
                       sum(rp.pre.1$cptable[match(min(rp.pre.1$cptable[,c("xerror")]), rp.pre.1$cptable[,c("xerror")]), c("xerror", "xstd")]) ,]

# use cp slightly larger than cp at nsplit = 3.

# so, cp slightly larger than this one below:
rp.pre.1$cptable[6,1]

rp.pre.1.pruned = prune(rp.pre.1, cp = 0.0042)



# predictions
rp.predict.1 = predict(rp.pre.1, newdata = data.all.pre.1.test, type = "vector") # unpruned tree
rp.predict.1.pruned = predict(rp.pre.1.pruned, newdata = data.all.pre.1.test, type = "vector")

# RMSE
rmse.rp.1 = rmse(rp.predict.1, data.all.pre.1.test)
rmse.rp.1.pruned = rmse(rp.predict.1.pruned, data.all.pre.1.test)

sort(c("orig" = rmse.rp.1, "pruned" = rmse.rp.1.pruned))

# RMSPE
rmspe.rp.1 = rmspe(rp.predict.1, data.all.pre.1.test)
rmspe.rp.1.pruned = rmspe(rp.predict.1.pruned, data.all.pre.1.test)

sort(c("orig" = rmspe.rp.1, "pruned" = rmspe.rp.1.pruned))


# MAPE
mape.rp.1 = mape(rp.predict.1, data.all.pre.1.test)
mape.rp.1.pruned = mape(rp.predict.1.pruned, data.all.pre.1.test)

sort(c("orig" = mape.rp.1, "pruned" = mape.rp.1.pruned))


# correlation
cor.rp.1 = cor(rp.predict.1, data.all.pre.1.test$numtoday)
cor.rp.1.pruned = cor(rp.predict.1, data.all.pre.1.test$numtoday)

sort(c("orig" = cor.rp.1, "pruned" = cor.rp.1.pruned))



# add lagged cases variable to pruned tree to see if it improves prediction

dt.ar.mods = list()
predict.dt.ar.mod = list()

for (lag in 1:14) {
  
  vars.to.include = c(colnames(data.all.pre.1.train[,-c(match(c("date", "numtoday"), 
                                                              colnames(data.all.pre.1.train)), 
                                                        grep("numtoday_lag", colnames(data.all.pre.1.train)))]),
                      colnames(data.all.pre.1.train)[match(paste0("numtoday_lag", lag), colnames(data.all.pre.1.train))])
  
  
  # fit models with cp of the pruned tree 
  mod = rpart(reformulate(colnames(data.all.pre.1.train[,vars.to.include]), "numtoday"), 
                         data = data.all.pre.1.train, method = "poisson", cp = 0.0042)
  
  
  dt.ar.mods[[paste0("numtoday_lag", lag)]] = mod

  
  # make predictions
  predict.dt = predict(mod, data.all.pre.1.test, type = "vector")

  predict.dt.ar.mod[[paste0("numtoday_lag", lag)]] = predict.dt

}

# RMSE
rmse.dt.ar = sapply(predict.dt.ar.mod, rmse, testdata = data.all.pre.1.test)

# RMSPE
rmspe.dt.ar = sapply(predict.dt.ar.mod, rmspe, testdata = data.all.pre.1.test)

# MAPE
mape.dt.ar = sapply(predict.dt.ar.mod, mape, testdata = data.all.pre.1.test)

# correlation
cor.dt.ar = sapply(predict.dt.ar.mod, cor, y = data.all.pre.1.test$numtoday)
# correlation produces unusable results because the predicted values
# are constant.


sort(c(rmse.dt.ar, "original" = rmse.rp.1.pruned))
sort(c(rmspe.dt.ar, "original" = rmspe.rp.1.pruned))
sort(c(mape.dt.ar, "original" = mape.rp.1.pruned))
sort(c(cor.dt.ar, "original" = cor.rp.1.pruned))


# Result: all models except for the one with cases at lag 1 have the same performance metric values.


# plot predictions on testing data
plot(1:nrow(data.all.pre.1.test), data.all.pre.1.test$numtoday, type = "l", 
     xlab = "", ylab = "Cases", xaxt = "n", main = "Decision Tree - Testing", ylim = c(0, 1000))
axis(1, at = 1:nrow(data.all.pre.1.test), data.all.pre.1.test$date, las = 3)
points(1:nrow(data.all.pre.1.test), rp.predict.1, type = "l", col = 2)  # original tree
points(1:nrow(data.all.pre.1.test), rp.predict.1.pruned, type = "l", col = 3)  # pruned tree 
legend(5, 1000, legend = c("actual", "DT", "DT pruned"), col = c(1, 2, 3), pch = 20, cex = 0.8, pt.cex = 1.8)

# Note: removed the model from the loop above because the performance is the same as the orig
# pruned tree. 



### Partial Least Squares ###

# fit pls model
pls.pre.1 = plsr(reformulate(colnames(data.all.pre.1.train[,-vars.remove]), "numtoday"),
                 data = data.all.pre.1.train, validation = "CV", segments = 10)


# observe root mean squared cv errors of prediction
rmsep.pls.pre.1 = RMSEP(pls.pre.1)

# show number of components with lowest error
which.min(rmsep.pls.pre.1$val[estimate = "CV", ,])

# mean squared error of prediction
msep.pls.pre.1 = MSEP(pls.pre.1)

# r-squared for fitted model
r2.pls.pre.1 = R2(pls.pre.1)

# see number of components that provides best r-squared to training data
which.max(r2.pls.pre.1$val[estimate = "CV", ,])


# Note: from rmse, mse and r2 metrics, a 2 component model produces best fit 
# to training data.


# fit another model with ideal number of components
pls.pre.1.best = plsr(reformulate(colnames(data.all.pre.1.train[,-vars.remove]), "numtoday"),
                      data = data.all.pre.1.train, ncomp = 2)


# make predictions using 2-component model
pls.predict.1.ideal = predict(pls.pre.1.best, newdata = data.all.pre.1.test, type = "response", comps = 2)


# RMSE
rmse.pls = rmse(pls.predict.1.ideal, data.all.pre.1.test)

# RMSPE
rmspe.pls = rmspe(pls.predict.1.ideal, data.all.pre.1.test)

# MAPE
mape.pls = mape(pls.predict.1.ideal, data.all.pre.1.test)

# correlation
cor.pls = cor(pls.predict.1.ideal, data.all.pre.1.test$numtoday)


# make predictions and find RMSEP, MSEP, R2 values for models with all numbers of components
pls.mods = list()
predict.pls.mods = list() 
rmsep.pls.mods = list()
msep.pls.mods = list()
r2.pls.mods = list()


for (numcomp in 1:pls.pre.1$ncomp) {
  
  # fit the model
  pls.mods[[paste0(numcomp, "_comp")]] = plsr(reformulate(colnames(data.all.pre.1.train[,-vars.remove]), "numtoday"),
             data = data.all.pre.1.train, ncomp = numcomp)
  
  # make predictions using that number of components
  predict.pls.mods[[paste0(numcomp, "_comp")]] = predict(pls.mods[[numcomp]], newdata = data.all.pre.1.test, 
                                        type = "response", comps = numcomp)
  
  # store RMSEP, MSEP, and R-squared values of all models
  rmsep.pls.mods[[paste0(numcomp, "_comp")]] = RMSEP(pls.mods[[numcomp]], comps = numcomp)
  msep.pls.mods[[paste0(numcomp, "_comp")]] = MSEP(pls.mods[[numcomp]], comps = numcomp)
  r2.pls.mods[[paste0(numcomp, "_comp")]] = R2(pls.mods[[numcomp]], comps = numcomp)
  
}


# rmsep, msep and r-squared values for all models above
get_val = function(obj) {
  return(obj$val)
}


# rmsep, msep, r-squared values for models with different numbers of components above
rmsep.pls.mods.vec = sapply(rmsep.pls.mods, get_val)
msep.pls.mods.vec = sapply(msep.pls.mods, get_val)
r2.pls.mods.vec = sapply(r2.pls.mods, get_val)

# RMSE
rmse.pls.mods = sapply(predict.pls.mods, rmse, testdata = data.all.pre.1.test)

# RMSPE
rmspe.pls.mods = sapply(predict.pls.mods, rmspe, testdata = data.all.pre.1.test)

# MAPE
mape.pls.mods = sapply(predict.pls.mods, mape, testdata = data.all.pre.1.test)

# correlation
cor.pls.mods = sapply(predict.pls.mods, cor, y = data.all.pre.1.test$numtoday)

sort(rmse.pls.mods)
sort(rmspe.pls.mods)
sort(mape.pls.mods)
sort(cor.pls.mods)


# add AR cases lag to the best model determined by the performance metrics
pls.ar.mods.1 = list()
pls.ar.mods.2 = list()
predict.pls.ar.mod.1 = list()
predict.pls.ar.mod.2 = list()

for (lag in 1:14) {
  
  vars.to.include = c(colnames(data.all.pre.1.train[,-c(match(c("date", "numtoday"), 
                                                              colnames(data.all.pre.1.train)), 
                                                        grep("numtoday_lag", colnames(data.all.pre.1.train)))]),
                      colnames(data.all.pre.1.train)[match(paste0("numtoday_lag", lag), colnames(data.all.pre.1.train))])
  
  
  # fit models  
  mod.1 = plsr(reformulate(colnames(data.all.pre.1.train[,vars.to.include]), "numtoday"),
               data = data.all.pre.1.train, ncomp = 1)
  
  mod.2 = plsr(reformulate(colnames(data.all.pre.1.train[,vars.to.include]), "numtoday"),
               data = data.all.pre.1.train, ncomp = 2)
  
  
  pls.ar.mods.1[[paste0("numtoday_lag", lag)]] = mod.1
  pls.ar.mods.2[[paste0("numtoday_lag", lag)]] = mod.2
  
  
  # make predictions
  predict.1 = predict(mod, data.all.pre.1.test, type = "vector")
  predict.2 = predict(mod, data.all.pre.2.test, type = "vector")
  
  
  predict.pls.ar.mod.1[[paste0("numtoday_lag", lag)]] = predict.1
  predict.pls.ar.mod.2[[paste0("numtoday_lag", lag)]] = predict.2
    
}


rmse.pls.ar.1 = sapply(predict.pls.ar.mod.1, rmse, testdata = data.all.pre.1.test)
rmspe.pls.ar.1 = sapply(predict.pls.ar.mod.1, rmspe, testdata = data.all.pre.1.test)
mape.pls.ar.1 = sapply(predict.pls.ar.mod.1, mape, testdata = data.all.pre.1.test)
cor.pls.ar.1 = sapply(predict.pls.ar.mod.1, cor, y = data.all.pre.1.test$numtoday)

# Note: correlation produces unusable results because the predictions are constant.

rmse.pls.ar.2 = sapply(predict.pls.ar.mod.2, rmse, testdata = data.all.pre.1.test)
rmspe.pls.ar.2 = sapply(predict.pls.ar.mod.2, rmspe, testdata = data.all.pre.1.test)
mape.pls.ar.2 = sapply(predict.pls.ar.mod.2, mape, testdata = data.all.pre.1.test)
cor.pls.ar.2 = sapply(predict.pls.ar.mod.2, cor, y = data.all.pre.1.test$numtoday)

# Note: correlation produces unusable results because the predictions are constant.


sort(c(rmse.pls.ar.1, rmse.pls.mods))
sort(c(rmspe.pls.ar.1, rmspe.pls.mods))
sort(c(mape.pls.ar.1, mape.pls.mods))
sort(c(rmse.pls.ar.2, rmse.pls.mods))
sort(c(rmspe.pls.ar.2, rmspe.pls.mods))
sort(c(mape.pls.ar.2, mape.pls.mods))

# Result: the one component model has best rmse, and the model with lag 1 count has best
# rmspe and mape.


# plot best prediction
plot(1:nrow(data.all.pre.1.test), data.all.pre.1.test$numtoday, type = "l", 
     xlab = "", ylab = "Cases", xaxt = "n", main = "PLS - Testing", lwd = 2, ylim = c(0, 1500))
axis(1, at = 1:nrow(data.all.pre.1.test), data.all.pre.1.test$date, las = 3)
points(1:nrow(data.all.pre.1.test), pls.predict.1.ideal, type = "l", col = 2) # best 
points(1:nrow(data.all.pre.1.test), predict.pls.ar.mod.1[[which.min(rmse.pls.ar.1)]], type = "l", col = 3) # best 
legend(5, 1200, legend = c("actual", "PLS", "PLS with AR"), col = c(1, 2, 3), pch = 20, cex = 0.8, pt.cex = 1.8)




### Neural Net ###

set.seed(2020)

# fit neural net with neuralnet package
nn.vars.remove = vars.remove
nn.pre.1 = neuralnet(reformulate(colnames(data.all.pre.1.train[,-vars.remove]), "numtoday"), 
                     data = na.omit(data.all.pre.1.train[,-vars.remove[!(vars.remove %in% match("numtoday", colnames(data.all.pre.1.train)))]]),
                     hidden = 2)
nn.pre.2 = neuralnet(reformulate(colnames(data.all.pre.1.train[,-vars.remove]), "numtoday"), 
                     data = na.omit(data.all.pre.1.train[,-vars.remove[!(vars.remove %in% match("numtoday", colnames(data.all.pre.1.train)))]]),
                     hidden = c(2, 1))

# make predictions with nnet package
nnet.pre.1 = nnet(reformulate(colnames(data.all.pre.1.train[,-vars.remove]), "numtoday"), 
                      data = na.omit(data.all.pre.1.train[,-vars.remove[!(vars.remove %in% match("numtoday", colnames(data.all.pre.1.train)))]]),
                      size = 2, linout = TRUE)
nnet.pre.2 = nnet(reformulate(colnames(data.all.pre.1.train[,-vars.remove]), "numtoday"), 
                  data = na.omit(data.all.pre.1.train[,-vars.remove[!(vars.remove %in% match("numtoday", colnames(data.all.pre.1.train)))]]),
                  size = 5, linout = TRUE)
  
  
# make predictions with neuralnet package
nn.predict.1 = predict(nn.pre.1, newdata = data.all.pre.1.test)
nn.predict.2 = predict(nn.pre.2, newdata = data.all.pre.1.test)

# make predictions with nnet package
nnet.predict.1 = predict(nnet.pre.1, newdata = data.all.pre.1.test, type = "raw")
nnet.predict.2 = predict(nnet.pre.2, newdata = data.all.pre.1.test, type = "raw")


# RMSE
rmse.nn.1 = rmse(nn.predict.1, data.all.pre.1.test)
rmse.nn.2 = rmse(nn.predict.2, data.all.pre.1.test)
rmse.nnet.1 = rmse(nnet.predict.1, data.all.pre.1.test)
rmse.nnet.2 = rmse(nnet.predict.2, data.all.pre.1.test)

rmse.nn.1 
rmse.nn.2 
rmse.nnet.1
rmse.nnet.2



# RMSPE
rmspe.nn.1 = rmspe(nn.predict.1, data.all.pre.1.test)
rmspe.nn.2 = rmspe(nn.predict.2, data.all.pre.1.test)
rmspe.nnet.1 = rmspe(nnet.predict.1, data.all.pre.1.test)
rmspe.nnet.2 = rmspe(nnet.predict.2, data.all.pre.1.test)

rmspe.nn.1 
rmspe.nn.2 
rmspe.nnet.1 
rmspe.nnet.2


# MAPE
mape.nn.1 = mape(nn.predict.1, data.all.pre.1.test)
mape.nn.2 = mape(nn.predict.2, data.all.pre.1.test)
mape.nnet.1 = mape(nnet.predict.1, data.all.pre.1.test)
mape.nnet.2 = mape(nnet.predict.2, data.all.pre.1.test)


mape.nn.1 
mape.nn.2
mape.nnet.1 
mape.nnet.2


# plot test data vs. actual test data
plot(nn.predict.1, data.all.pre.1.test$numtoday, pch = 20, 
     xlab = "nn predictions", ylab = "actual cases", 
     main = "neuralnet Predictions on Test Data 1")
plot(nn.predict.2, data.all.pre.2.test$numtoday, pch = 20, 
     xlab = "nn predictions", ylab = "actual cases", 
     main = "neuralnet Predictions on Test Data 2")
plot(nnet.predict.1, data.all.pre.1.test$numtoday, pch = 20, 
     xlab = "nnet predictions", ylab = "actual cases", 
     main = "nnet Predictions on Test Data 1")
plot(nnet.predict.2, data.all.pre.2.test$numtoday, pch = 20, 
     xlab = "nnet predictions", ylab = "actual cases", 
     main = "nnet Predictions on Test Data 2")



# plot fitted values to training data
plot(1:nrow(data.all.pre.1.train), data.all.pre.1.train$numtoday, type = "l", xaxt = "n", xlab = "",
     ylab = "Counts", main = "Neural Network Fits (Training Data)")
axis(1, 1:nrow(data.all.pre.1.train), data.all.pre.1.train$date, las = 3)
points(29:nrow(data.all.pre.1.train), unlist(nn.pre.1$net.result), type = "l", xaxt = "n", xlab = "",
     ylab = "Counts", col = 2)
points(29:nrow(data.all.pre.1.train), unlist(nn.pre.2$net.result), type = "l", xaxt = "n", xlab = "",
       ylab = "Counts", col = 3)
points(29:nrow(data.all.pre.1.train), nnet.pre.1$fitted.values, type = "l", xaxt = "n", xlab = "",
       ylab = "Counts", col = 4)
points(29:nrow(data.all.pre.1.train), nnet.pre.2$fitted.values, type = "l", xaxt = "n", xlab = "",
       ylab = "Counts", col = 5)
legend(5, 2700, legend = c("actual", "neuralnet 1", "neuralnet 2", "nnet 1", "nnet 2"), 
       col = c(1, 2, 3, 4, 5), pch = 20, cex = 0.8, pt.cex = 1.8)



# plot predicted values for test data
plot(1:nrow(data.all.pre.1.test), data.all.pre.1.test$numtoday, type = "l", xaxt = "n", xlab = "",
     ylab = "Counts", main = "Neural Network Predictions (Test Data)")
axis(1, 1:nrow(data.all.pre.1.test), data.all.pre.1.test$date, las = 3)

points(1:nrow(data.all.pre.1.test), nn.predict.1, type = "l", col = 2)
points(1:nrow(data.all.pre.1.test), nn.predict.2, type = "l", col = 3)
points(1:nrow(data.all.pre.1.test), nnet.predict.1, type = "l", col = 4)
points(1:nrow(data.all.pre.1.test), nnet.predict.2, type = "l", col = 5)
legend(1, 1010, legend = c("actual", "neuralnet 1", "neuralnet 2", "nnet 1", "nnet 2"), 
       col = c(1, 2, 3, 4, 5), pch = 20, cex = 0.8, pt.cex = 1.8)




### Use predetermined lags with added external variables (publications and twitter n-grams) ###

# Note: only using rpart decision trees because other models cannot handle 
# missing values in some predictors.

vars.remove.2 = c(grep("numtoday_lag", colnames(data.all.pre.2.train)), 
                  match(c("date", "numtoday"), colnames(data.all.pre.2.train)))

rp.extravars = rpart(reformulate(colnames(data.all.pre.2.train[,-vars.remove.2]), "numtoday"), 
                 data = data.all.pre.2.train, method = "poisson", cp = 0)

# show variables from most to least important
rp.extravars$variable.importance

# model info
rp.extravars

# cross validation results
printcp(rp.extravars)
plotcp(rp.extravars)

# To prune tree: use cp of tree that has xerror <= (smaller than or equal to) smallest xerror + sd

# index of smallest xerror
match(min(rp.extravars$cptable[,c("xerror")]), rp.extravars$cptable[,c("xerror")])


# sum smallest xerror and its corresponding sd
# return all the trees with xerror smaller than smallest xerror and its corresponding sd
min(rp.extravars$cptable[rp.extravars$cptable[,c("xerror")] <=
                       sum(rp.extravars$cptable[match(min(rp.extravars$cptable[,c("xerror")]), rp.extravars$cptable[,c("xerror")]), c("xerror", "xstd")]) ,][,c("nsplit")])

# Note: smallest tree is of size nsplit = 5.

# show all tree sizes where error is smaller than or equal to smallest xerror + sd
rp.extravars$cptable[rp.extravars$cptable[,c("xerror")] <=
                       sum(rp.extravars$cptable[match(min(rp.extravars$cptable[,c("xerror")]), rp.extravars$cptable[,c("xerror")]), c("xerror", "xstd")]) ,]

# use cp slightly larger than cp at nsplit = 5.

# so, cp slightly larger than this one below:
rp.extravars$cptable[6,1]

rp.extravars.pruned = prune(rp.pre.1, cp = 0.0042)

# predictions
rp.predict.extravars = predict(rp.extravars, newdata = data.all.pre.2.test, type = "vector") # unpruned tree
rp.predict.extravars.pruned = predict(rp.extravars.pruned, newdata = data.all.pre.2.test, type = "vector")


# RMSE
rmse.rp.extravars = rmse(rp.predict.extravars, data.all.pre.2.test)
rmse.rp.extravars.pruned = rmse(rp.predict.extravars.pruned, data.all.pre.2.test)

# compare pruned/full trees with extra vars to pruned/unpruned without extra vars
sort(c("orig_extravars" = rmse.rp.extravars, "pruned_extravars" = rmse.rp.extravars.pruned,
       "orig" = rmse.rp.1, "pruned" = rmse.rp.1.pruned))


# RMSPE
rmspe.rp.extravars = rmspe(rp.predict.extravars, data.all.pre.2.test)
rmspe.rp.extravars.pruned = rmspe(rp.predict.extravars.pruned, data.all.pre.2.test)

sort(c("orig_extravars" = rmspe.rp.extravars, "pruned_extravars" = rmspe.rp.extravars.pruned,
       "orig" = rmspe.rp.1, "pruned" = rmspe.rp.1.pruned))


# MAPE
mape.rp.extravars = mape(rp.predict.extravars, data.all.pre.2.test)
mape.rp.extravars.pruned = mape(rp.predict.extravars.pruned, data.all.pre.2.test)

sort(c("orig_extravars" = mape.rp.extravars, "pruned_extravars" = mape.rp.extravars.pruned,
     "orig" = mape.rp.1, "pruned" = mape.rp.1.pruned))

# Result: the pruned/unpruned models with extra vars perform the same as the 
# pruned/unpruned models without extra vars.  






### Prospective Modelling: Using external variables without lags ###

# In prospective modelling, use the variables that were used in the retrospective models,
# just without lags, plus additional variables.

remove.vars.3 = c(match(c("date"), colnames(data.all.auto)), 
                  grep("sent_", colnames(data.all.auto.train)),
                  grep("query_", colnames(data.all.auto.train)))


# Generate prospective models in a for loop.
# Include variables without lags, as lagged values would not be available each week, 
# and they are needed if the model is needed to run using the most recent data.



mods.dt.pro = list()
mods.rf.pro = list()
predict.dt.pro = list()
predict.rf.pro  = list()

# Start training from the start of the timeline, adding one day to the testing data at each iteration.
# The data for the next day is predicted at each iteration.

for (day in 1:(nrow(data.all.auto)-1)) {
  
  # set all data in the timeline up until day <day> for training
  data.train = data.all.auto[1:day,-remove.vars.3]

  # set the following day for testing
  data.test = data.all.auto[day+1,-remove.vars.3]



  # fit random forest
  mod.rf = randomForest(reformulate(colnames(data.train[,-match("numtoday", colnames(data.train))]),
                                    response = "numtoday"),
                        data = data.train, importance = TRUE, na.action = na.omit, mtry = 10)

  mods.rf.pro[[paste0("up_to_training_day", day)]] = mod.rf


  # random forest prediction
  predict.rf = predict(mod.rf, newdata = data.test, type = "response")

  predict.rf.pro[[paste0("up_to_training_day", day)]] = predict.rf

  
  

  # fit decision tree
  mod.dt = rpart(reformulate(colnames(data.train[,-match("numtoday", colnames(data.train))]), "numtoday"),
                 data = data.train, method = "poisson")

  mods.dt.pro[[paste0("up_to_training_day", day)]] = mod.dt


  # decision tree prediction
  predict.dt = predict(mod.dt, newdata = data.test, type = "vector") # unpruned tree

  predict.dt.pro[[paste0("up_to_training_day", day)]] = predict.dt
     
}


predict.rf.pro = unlist(predict.rf.pro)
predict.dt.pro = unlist(predict.dt.pro)

# RMSE for full timeline in loop
rmse.rf.pro = rmse(predict.rf.pro, data.all.auto[2:nrow(data.all.auto),])
rmse.dt.pro = rmse(predict.dt.pro, data.all.auto[2:nrow(data.all.auto),])
rmse.rf.pro
rmse.dt.pro


# RMSE for timeline of test data from previous ensemble models
rmse.rf.pro.small = rmse(predict.rf.pro[195:243], data.all.pre.1.test)
rmse.dt.pro.small = rmse(predict.dt.pro[195:243], data.all.pre.1.test)



# RMSPE for full timeline in loop
rmspe.rf.pro = rmspe(predict.rf.pro, data.all.auto[2:nrow(data.all.auto),])
rmspe.dt.pro = rmspe(predict.dt.pro, data.all.auto[2:nrow(data.all.auto),])
rmspe.rf.pro
rmspe.dt.pro


# RMSPE for timeline of test data from previous ensemble models
rmspe.rf.pro.small = rmspe(predict.rf.pro[195:243], data.all.pre.1.test)
rmspe.dt.pro.small = rmspe(predict.dt.pro[195:243], data.all.pre.1.test)
rmspe.rf.pro.small


# MAPE for full timeline in loop 
mape.rf.pro = mape(predict.rf.pro, data.all.auto[2:nrow(data.all.auto),])
mape.dt.pro = mape(predict.dt.pro, data.all.auto[2:nrow(data.all.auto),])
mape.rf.pro
mape.dt.pro

# MAPE for timeline of test data from previous ensemble models
mape.rf.pro.small = mape(predict.rf.pro[195:243], data.all.pre.1.test)
mape.dt.pro.small = mape(predict.dt.pro[195:243], data.all.pre.1.test)



# correlation for full timeline in loop
cor.rf.pro = cor(predict.rf.pro, data.all.auto[2:nrow(data.all.auto),]$numtoday)
cor.dt.pro = cor(predict.dt.pro, data.all.auto[2:nrow(data.all.auto),]$numtoday)
cor.rf.pro
cor.dt.pro


# correlation for timeline of test data from previous ensemble models
cor.rf.pro.small = cor(predict.rf.pro[195:243], data.all.pre.1.test$numtoday)
cor.dt.pro.small = cor(predict.dt.pro[195:243], data.all.pre.1.test$numtoday)


# compare predictions to predictions made by ensemble models with predetermined lags
sort(c("prospective full" = rmse.rf.pro.small, "retrospective" = rf.mods.with.ar.rmse, "without AR" = rmse.rf.1))
sort(c("prospective full" = rmspe.rf.pro.small, "retrospective" = rf.mods.with.ar.rmspe, "without AR" = rmspe.rf.1))
sort(c("prospective full" = mape.rf.pro.small, "retrospective" = rf.mods.with.ar.mape, "without AR" = mape.rf.1))
sort(c("prospective full" = cor.rf.pro.small, "retrospective" = rf.mods.with.ar.cor, "without AR" = cor.rf.1))

sort(c("prospective full" = rmse.dt.pro.small, "retrospective" = rmse.dt.ar, "without AR" = rmse.rp.1.pruned))
sort(c("prospective full" = rmspe.dt.pro.small, "retrospective" = rmspe.dt.ar, "without AR" = rmspe.rp.1.pruned))
sort(c("prospective full" = mape.dt.pro.small, "retrospective" = mape.dt.ar, "without AR" = mape.rp.1.pruned))
sort(c("prospective full" = cor.dt.pro.small, "retrospective" = cor.dt.ar, "without AR" = cor.rp.1.pruned))



# plot prospective predictions
plot(1:nrow(data.all.auto), data.all.auto$numtoday, main = "RF Prospective Predictions (Full)",
     xlab = "", ylab = "Cases", xaxt = "n", type = "l", lwd = 2)
axis(1, 1:nrow(data.all.auto), data.all.auto$date, las = 3)
points(2:nrow(data.all.auto), predict.rf.pro, type = "l", col = 2)
points(2:nrow(data.all.auto), predict.dt.pro, type = "l", col = 3)
legend(5, 2500, legend = c("actual", "RF", "DT"), col = c(1, 2, 3), pch = 20, cex = 0.8, pt.cex = 1.8)


# Repeat the loop above, but start at 90 days into the timeline.
              
mods.dt.pro.2 = list()
mods.rf.pro.2 = list()
predict.dt.pro.2 = list()
predict.rf.pro.2 = list()


for (day in 90:(nrow(data.all.auto)-1)) {
  
  # set all data in the timeline up until day <day> for training
  data.train = data.all.auto[1:day,-remove.vars.3]
  
  # set the following day for testing
  data.test = data.all.auto[day+1,-remove.vars.3]
  
  

  # fit random forest
  mod.rf = randomForest(reformulate(colnames(data.train[,-match("numtoday", colnames(data.train))]),
                                    response = "numtoday"),
                        data = data.train, importance = TRUE, na.action = na.omit, mtry = 10)
  
  mods.rf.pro.2[[paste0("up_to_training_day", day)]] = mod.rf
  
  
  # random forest prediction
  predict.rf = predict(mod.rf, newdata = data.test, type = "response")
  
  predict.rf.pro.2[[paste0("up_to_training_day", day)]] = predict.rf
  
  
  
  
  # fit decision tree
  mod.dt = rpart(reformulate(colnames(data.train[,-match("numtoday", colnames(data.train))]), "numtoday"),
                 data = data.train, method = "poisson")
  
  mods.dt.pro.2[[paste0("up_to_training_day", day)]] = mod.dt
  
  
  # decision tree prediction
  predict.dt = predict(mod.dt, newdata = data.test, type = "vector") # unpruned tree
  
  predict.dt.pro.2[[paste0("up_to_training_day", day)]] = predict.dt
  
}


predict.rf.pro.2 = unlist(predict.rf.pro.2)
predict.dt.pro.2 = unlist(predict.dt.pro.2)


# RMSE for days 91 to 244
rmse.rf.pro.2 = rmse(predict.rf.pro.2, data.all.auto[91:nrow(data.all.auto),])
rmse.dt.pro.2 = rmse(predict.dt.pro.2, data.all.auto[91:nrow(data.all.auto),])
rmse.rf.pro.2 
rmse.dt.pro.2


# RMSE for test data from ensemble models with predetermined lags
rmse.rf.pro.2.small = rmse(predict.rf.pro.2[106:154], data.all.pre.1.test)
rmse.dt.pro.2.small = rmse(predict.dt.pro.2[106:154], data.all.pre.1.test)
rmse.rf.pro.2.small 
rmse.dt.pro.2.small


# RMSPE for days 91 to 244
rmspe.rf.pro.2 = rmspe(predict.rf.pro.2, data.all.auto[91:nrow(data.all.auto),])
rmspe.dt.pro.2 = rmspe(predict.dt.pro.2, data.all.auto[91:nrow(data.all.auto),])
rmspe.rf.pro.2 
rmspe.dt.pro.2


# RMSPE for test data from ensemble models with predetermined lags
rmspe.rf.pro.2.small = rmspe(predict.rf.pro.2[106:154], data.all.pre.1.test)
rmspe.dt.pro.2.small = rmspe(predict.dt.pro.2[106:154], data.all.pre.1.test)
rmspe.rf.pro.2.small 
rmspe.dt.pro.2.small


# MAPE for days 91 to 244
mape.rf.pro.2 = mape(predict.rf.pro.2, data.all.auto[91:nrow(data.all.auto),])
mape.dt.pro.2 = mape(predict.dt.pro.2, data.all.auto[91:nrow(data.all.auto),])
mape.rf.pro.2 
mape.dt.pro.2


# MAPE for test data from ensemble models with predetermined lags
mape.rf.pro.2.small = mape(predict.rf.pro.2[106:154], data.all.pre.1.test)
mape.dt.pro.2.small = mape(predict.dt.pro.2[106:154], data.all.pre.1.test)
mape.rf.pro.2.small 
mape.dt.pro.2.small


# correlation for days 91 to 244
cor.rf.pro.2 = cor(predict.rf.pro.2, data.all.auto[91:nrow(data.all.auto),]$numtoday)
cor.dt.pro.2 = cor(predict.dt.pro.2, data.all.auto[91:nrow(data.all.auto),]$numtoday)
cor.rf.pro.2 
cor.dt.pro.2

# correlation for test data from ensemble models with predetermined lags
cor.rf.pro.2.small = cor(predict.rf.pro.2[106:154], data.all.pre.1.test$numtoday)
cor.dt.pro.2.small = cor(predict.dt.pro.2[106:154], data.all.pre.1.test$numtoday)
cor.rf.pro.2.small
cor.dt.pro.2.small

sort(c("prospective short" = rmse.rf.pro.2.small, "retrospective" = rf.mods.with.ar.rmse, "without AR" = rmse.rf.1))
sort(c("prospective short" = rmspe.rf.pro.2.small, "retrospective" = rf.mods.with.ar.rmspe, "without AR" = rmspe.rf.1))
sort(c("prospective short" = mape.rf.pro.2.small, "retrospective" = rf.mods.with.ar.mape, "without AR" = mape.rf.1))
sort(c("prospective short" = cor.rf.pro.2.small, "retrospective" = rf.mods.with.ar.cor, "without AR" = cor.rf.1))





plot(1:nrow(data.all.auto), data.all.auto$numtoday, main = "RF Prospective Predictions (Short)",
     xlab = "", ylab = "Cases", xaxt = "n", type = "l", lwd = 2)
axis(1, 1:nrow(data.all.auto), data.all.auto$date, las = 3)
points(91:nrow(data.all.auto), predict.rf.pro.2, type = "l", col = 2)
points(91:nrow(data.all.auto), predict.dt.pro.2, type = "l", col = 3)
legend(5, 2500, legend = c("actual", "RF", "DT"), col = c(1, 2, 3), pch = 20, cex = 0.8, pt.cex = 1.8)







