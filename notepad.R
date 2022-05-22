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

# Modify global options in R - disable scientific notation
options(scipen = 999)

# ----------------------------------- Indicators ----------------------------------------------------
#filter vector for indicators
#wd_series <- c("SP.URB.TOTL", "SP.POP.TOTL", "SP.POP.TOTL.MA.IN", "SP.POP.TOTL.FE.IN", 
                     # "SP.DYN.LE00.IN", "SL.UEM.TOTL.NE.ZS", "SL.UEM.ADVN.ZS", "SP.DYN.TO65.FE.ZS",
                      #"SP.DYN.TO65.MA.ZS", "SH.STA.SUIC.P5", "SH.STA.SUIC.FE.P5", "SH.STA.SUIC.MA.P5",
                      #"SH.STA.DIAB.ZS")

wd_series <- c("SH.STA.SUIC.P5", "SH.STA.SUIC.FE.P5", "SH.STA.SUIC.MA.P5")

#filter vector for countries
wd_country <- c("CHN", "DEU", "JPN", "POL", "QAT", "USA")

wd_indicators_f <- wd_indicators %>%
  gather("year", "value", 5:ncol(wd_indicators)) %>%
  mutate(Year = substr(year, 1, 4)) %>% 
  mutate_if(is.numeric, round, 1) %>%
  mutate(Year = as.numeric(Year))  %>%
  filter(`Series Code` %in% wd_series, `Country Code` %in% wd_country, !is.na(value)) %>%
  filter(Year >= "1995" & Year <= "2018")  %>%
  select(`Country Name`, `Series Name`, `Year`, value)

wd_indicators <- wd_indicators_f

#remove unnecessary filter vectors
rm(wd_country, wd_series, wd_indicators_f)

summary(wd_indicators)

# ----------------------------------------- Gold Prices --------------------------------------------------

#mean from USD A.M. and USD P.M.
gold_prices_f <- gold_prices %>%
  select(Date, USD..AM., USD..PM.) %>%
  mutate(GoldPriceUsd = rowMeans(select(., USD..AM., USD..PM.), na.rm = TRUE)) %>%
  mutate(Date = ymd(Date)) %>%
  mutate(GoldPriceUsd = round(GoldPriceUsd, 1)) %>%
  filter(Date >= "1995-01-01" & Date <= "2018-12-31") %>%
  select(Date, GoldPriceUsd) %>%
  arrange(desc(Date))

#assign to original df
gold_prices <- gold_prices_f
rm(gold_prices_f)

summary(gold_prices)

# ------------------------------ mkpru - Bitcoin Market Price USD ----------------------------------------

bitcoin_mkpru_f <- bitcoin_mkpru %>%
  select(Date, Value) %>%
  mutate(Year = ymd(Date)) %>%
  mutate(bitcoin_mkpru, Year = format(Year, format = "%Y")) %>%
  mutate(Value = round(Value, 1)) %>%
  filter(Year >= "2010" & Year <= "2018") %>%
  select(Year, value = Value) %>%
  arrange(desc(Year))

#assign to original df
bitcoin_mkpru <- bitcoin_mkpru_f

#remove unnecessary filter vectors
rm(bitcoin_mkpru_f)

summary(bitcoin_mkpru)

# ----------------------- trvou - Bitcoin USD Exchange Trade Volume --------------------------------------

bitcoin_trvou_f <- bitcoin_trvou %>%
  select(Date, Value) %>%
  mutate(Date = ymd(Date)) %>%
  mutate(Value = round(Value, 1)) %>%
  filter(Date >= "2010-01-01" & Date <= "2018-12-31") %>%
  arrange(desc(Date))

#assign to original df
bitcoin_trvou <- bitcoin_trvou_f

#remove unnecessary filter vectors
rm(bitcoin_trvou_f)

summary(bitcoin_trvou)

# ----------------------- hrate - Bitcoin Hash Rate --------------------------------------

bitcoin_hrate_f <- bitcoin_hrate %>%
  select(Date, Value) %>%
  mutate(Date = ymd(Date)) %>%
  mutate(Value = round(Value, 1)) %>%
  filter(Date >= "2010-01-01" & Date <= "2018-12-31") %>%
  arrange(desc(Date))

#assign to original df
bitcoin_hrate <- bitcoin_hrate_f

#remove unnecessary filter vectors
rm(bitcoin_hrate_f)

summary(bitcoin_hrate)

# ----------------------- diff - Bitcoin Hash Rate --------------------------------------

bitcoin_diff_f <- bitcoin_diff %>%
  select(Date, Value) %>%
  mutate(Date = ymd(Date)) %>%
  mutate(Value = round(Value, 1)) %>%
  filter(Date >= "2010-01-01" & Date <= "2018-12-31") %>%
  arrange(desc(Date))

#assign to original df
bitcoin_diff <- bitcoin_diff_f

#remove unnecessary filter vectors
rm(bitcoin_diff_f)

summary(bitcoin_diff)

# ------------------------------------- Plot -----------------------------------------------

germany <- wd_indicators %>%
  filter(`Country Name` == "Germany")

Suicedes <- wd_indicators %>%
  filter(wd_indicators$`Series Name` == "Suicide mortality rate (per 100,000 population)")


rm(germany)

ggplot(data = bitcoin_mkpru, aes(x = Date, y = Value)) +
  geom_line(size = 1)

plot1 <- ggplot(data = Suicedes, aes(x = Year, y = value, fill = `Country Name`))

plot1 +
  geom_col() + 
  facet_grid(. ~ `Country Name`) +
  theme(axis.text.x = element_text(angle = 90))

plot1 + geom_line(data = bitcoin_mkpru)

#skleić by year dataframe wd i bitcoin