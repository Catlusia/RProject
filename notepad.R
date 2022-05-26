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

wd_series <- c("SH.STA.SUIC.P5", "SH.STA.SUIC.FE.P5", "SH.STA.SUIC.MA.P5", "SP.URB.TOTL")

#filter vector for countries
wd_country <- c("DEU", "JPN", "POL", "FRA", "USA")

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
  filter(Date >= "2014-01-01" & Date <= "2018-12-31") %>%
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
  #mutate(bitcoin_mkpru, Year = format(Year, format = "%Y")) %>%
  #group_by(Year) %>%
  #summarise(MeanValue = mean(Value)) %>%
  #mutate(Year = as.numeric(Year))  %>%
  mutate(Value = round(Value, 1)) %>%
  filter(Year >= "2014-01-01" & Year <= "2018-12-31") %>%
  select(Year, Value) %>%
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

# Suicides
Suicides <- wd_indicators %>%
  filter(wd_indicators$`Series Name` == "Suicide mortality rate (per 100,000 population)")

# Urban population
UrbanPopulation <- wd_indicators %>%
  filter(wd_indicators$`Series Name` == "Urban population") %>%
  filter(Year >= "2000")

# Suicides rate on urban population
Suicides$value <- UrbanPopulation$value * (Suicides$value / 100)

# Plot
g <- ggplot() + 
  geom_col(data = UrbanPopulation, aes(x = Year, y = value, fill = 'Country Name')) + 
  facet_grid(. ~ `Country Name`, scales = "free") +
  geom_line(data = Suicides, aes(x = Year, y = value), size = 1, color = "Black")

#title of the plot)
g + 
  xlab("Year") + 
  ylab("Number of people") +
  ggtitle("Suicide mortality vs Urban population") +
  #label formatting
  theme_bw() +
  theme(axis.title.x = element_text(colour = "DarkGreen", size = 15),
        axis.title.y = element_text(colour = "DarkGreen", size = 15),
        #thick mark formatting
        axis.text.x = element_text(size = 10, angle = 90),
        axis.text.y = element_text(size = 10),
        #legend formatting
        legend.title = element_text(size = 15),
        legend.text = element_text(size = 11),
        legend.justification = c(1,1),
        plot.title = element_text(colour = "DarkBlue",
                                  size = 25,
                                  hjust = 0.5))
ggplotly(g)
