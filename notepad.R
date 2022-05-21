install.packages("tidyr")
library("readxl")
library("ggplot2")
library("plotly")
library("dplyr")
library("knitr")
library("DT")
library("readxl")
library("tidyverse")
library("lubridate")
library("tidyr")

#import data from files
gold_prices <- read.csv("Data\\Gold prices.csv")
wd_indicators <- read_excel("Data\\World_Development_Indicators.xlsx", na=c("", ".."))
bitcoin_diff <- read.csv("Data\\BCHAIN-DIFF.csv")
bitcoin_hrate <- read.csv("Data\\BCHAIN-HRATE.csv")
bitcoin_mkpru <- read.csv("Data\\BCHAIN-MKPRU.csv")
bitcoin_trvou <- read.csv("Data\\BCHAIN-TRVOU.csv")

?read_excel

rm(wd_indicators)

# Modify global options in R - disable scientific notation
options(scipen = 999)

# ----------------------------------- Indicators ----------------------------------------------------
#filter vector for indicators
wd_series <- c("SP.URB.TOTL", "SP.POP.TOTL", "SP.POP.TOTL.MA.IN", "SP.POP.TOTL.FE.IN", 
                      "SP.DYN.LE00.IN", "SL.UEM.TOTL.NE.ZS", "SL.UEM.ADVN.ZS", "SP.DYN.TO65.FE.ZS",
                      "SP.DYN.TO65.MA.ZS", "SH.STA.SUIC.P5", "SH.STA.SUIC.FE.P5", "SH.STA.SUIC.MA.P5",
                      "SH.STA.DIAB.ZS")

#'%ni%' <- Negate("%in%")

#filter vector for countries
wd_country <- c("CAN", "CHL", "CHN", "COL", "CZE", "ETH", "FRA", "DEU", "GHA", "HKG", 
                      "IND", "JPN", "KOR", "NPL", "POL", "QAT", "RUS", "SAU", "USA")

wd_filter <- c(5:55)

wd_indicators_f <- wd_indicators %>%
  select(`1995 [YR1995]`:`2018 [YR2018]`)

wd_indicators_f <- wd_indicators %>%
  gather("year", "value", 5:ncol(wd_indicators)) %>%
  mutate(year = substr(year, 1, 4)) %>%
  mutate_if(is.numeric, round, 1) %>%
  filter(`Series Code` %in% wd_series, `Country Code` %in% wd_country) %>%
  select(`Country Name`, `Series Name`, `year`)


wd_indicators_f <- wd_indicators %>%
  mutate_at(wd_filter, as.numeric)

wd_indicators <- wd_indicators_f

#remove unnecessary filter vectors
rm(wd_indicators_f, wd_filter, wd_country, wd_series)

# ----------------------------------------- Gold Prices --------------------------------------------------
#Change character to date
#gold_prices$Date <- as.Date(gold_prices$Date)

#mean from USD A.M. and USD P.M.
gold_prices_f <- gold_prices %>%
  select(Date, USD..AM., USD..PM.) %>%
  mutate(Usd = rowMeans(select(., USD..AM., USD..PM.), na.rm = TRUE)) %>%
  mutate(Date=ymd(Date))
  group_by(Date) %>%
  summarise(GoldPriceinEuro = mean(Usd)) %>%
  mutate(GoldPriceinEuro = round(GoldPriceinEuro, 1)) %>%
  filter(Date >= "1995-01-01" & Date <= "2018-12-31") %>%
  arrange(desc(Date))

#assign to original df
gold_prices <- gold_prices_f
rm(gold_prices_f)

# ------------------------------ mkpru - Bitcoin Market Price USD ----------------------------------------

#change character to date
#bitcoin_mkpru$Date <- as.Date(bitcoin_mkpru$Date)
bitcoin_mkpru$Value <- as.Date(bitcoin_mkpru$Value)

bitcoin_mkpru_f <- bitcoin_mkpru %>%
  select(Date, Value) %>%
  #mutate(bitcoin_mkpru, Date = format(Date, format = "%Y")) %>%
  mutate(Date=ymd(Date)) %>%
  group_by(Date) %>%
  summarise(MeanValue = mean(Value)) %>%
  mutate(MeanValue = round(MeanValue, 2)) %>%
  filter(Date <= "2018-12-31") %>%
  arrange(desc(Date))

#assign to original df
bitcoin_mkpru <- bitcoin_mkpru_f
bitcoin_mkpru <- rbind(bitcoin_mkpru, df)

#remove unnecessary filter vectors
rm(bitcoin_mkpru_f)

#bitcoin_mkpru <- bitcoin_mkpru %>%
 # pivot_wider(names_from = Date, values_from = MeanValue) %>%
  #mutate(Country = "World", Series = "Bitcoin Mkpru") %>%
  #select(Country, Series, everything())

# ----------------------- trvou - Bitcoin USD Exchange Trade Volume --------------------------------------

#change character to date
#bitcoin_trvou$Date <- as.Date(bitcoin_trvou$Date)
bitcoin_trvou$Value <- as.Date(bitcoin_trvou$Value)

bitcoin_trvou_f <- bitcoin_trvou %>%
  select(Date, Value) %>%
  mutate(Date=ymd(Date)) %>%
  #mutate(bitcoin_trvou, Date = format(Date, format = "%Y")) %>%
  group_by(Date) %>%
  summarise(MeanValue = mean(Value)) %>%
  mutate(MeanValue = round(MeanValue, 2)) %>%
  filter(Date <= "2018-12-31") %>%
  arrange(desc(Date))

#assign to original df
bitcoin_trvou <- bitcoin_trvou_f
bitcoin_trvou <- rbind(bitcoin_trvou, df)

#remove unnecessary filter vectors
rm(bitcoin_trvou_f)

#bitcoin_trvou <- bitcoin_trvou %>%
  #pivot_wider(names_from = Date, values_from = MeanValue) %>%
 # mutate(Country = "World", Series = "Bitcoin Trvou") %>%
  #select(Country, Series, everything())

# ----------------------- hrate - Bitcoin Hash Rate --------------------------------------

#bitcoin_hrate$Date <- as.Date(bitcoin_hrate$Date)
bitcoin_hrate$Value <- as.numeric(bitcoin_hrate$Value)

bitcoin_hrate_f <- bitcoin_hrate %>%
  select(Date, Value) %>%
  mutate(Data=ymd(Date)) %>%
  #mutate(bitcoin_hrate, Date = format(Date, format = "%Y")) %>%
  group_by(Date) %>%
  summarise(MeanValue = mean(Value)) %>%
  mutate(MeanValue = round(MeanValue, 2)) %>%
  filter(Date <= "2018") %>%
  arrange(desc(Date))

#assign to original df
bitcoin_hrate <- bitcoin_hrate_f
bitcoin_hrate <- rbind(bitcoin_hrate, df)

#remove unnecessary filter vectors
rm(bitcoin_hrate_f)

#bitcoin_hrate <- bitcoin_hrate %>%
  #pivot_wider(names_from = Date, values_from = MeanValue) %>%
  #mutate(Country = "World", Series = "Bitcoin Hrate") %>%
  #select(Country, Series, everything())

# ----------------------- diff - Bitcoin Hash Rate --------------------------------------

#bitcoin_diff$Date <- as.Date(bitcoin_diff$Date)
bitcoin_diff$Value <- as.numeric(bitcoin_diff$Value)

bitcoin_diff_f <- bitcoin_diff %>%
  select(Date, Value) %>%
  mutate(Date=ymd(Date)) %>%
  #mutate(bitcoin_diff, Date = format(Date, format = "%Y")) %>%
  group_by(Date) %>%
  summarise(MeanValue = mean(Value)) %>%
  mutate(MeanValue = round(MeanValue, 2)) %>%
  filter(Date <= "2018-12-31") %>%
  arrange(desc(Date))

#assign to original df
bitcoin_diff <- bitcoin_diff_f
#bitcoin_diff <- rbind(bitcoin_diff, df)

#remove unnecessary filter vectors
#rm(bitcoin_diff_f, df, Date, MeanValue)

#bitcoin_diff <- bitcoin_diff %>%
 # pivot_wider(names_from = Date, values_from = MeanValue) %>%
  #mutate(Country = "World", Series = "Bit coin Diff") %>%
 # select(Country, Series, everything())

# -------------------------------- Final data ---------------------------------------------

#merge all data about bitcoin and gold prices
all_data <- rbind(bitcoin_diff, bitcoin_hrate, bitcoin_mkpru, bitcoin_trvou, gold_prices)
#all_data <- all_data %>%
 # mutate_if(is.character, as.numeric) %>%
  #mutate_all(round, 1)

summary(all_data)
#remove unnecessary data
rm(bitcoin_diff, bitcoin_hrate, bitcoin_mkpru, bitcoin_trvou, gold_prices)

# ------------------------------------- Plot -----------------------------------------------

ggplot(data=)
