install.packages("tibble")
library("readxl")
library("ggplot2")
library("plotly")
library("dplyr")
library("knitr")
library("DT")
library("readxl")
library("tidyverse")
library("tibble")

#import data from files
gold_prices <- read.csv("Data\\Gold prices.csv")
currency_ex_rates <- read.csv("Data\\CurrencyExchangeRates.csv")
spcomposite <- read.csv("Data\\S&P Composite.csv")
wd_indicators <- read_excel("Data\\World_Development_Indicators.xlsx")

bitcoin_diff <- read.csv("Data\\BCHAIN-DIFF.csv")
bitcoin_hrate <- read.csv("Data\\BCHAIN-HRATE.csv")
bitcoin_mkpru <- read.csv("Data\\BCHAIN-MKPRU.csv")
bitcoin_trvou <- read.csv("Data\\BCHAIN-TRVOU.csv")

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
  mutate(replace(wd_indicators, wd_indicators == "..", "0")) %>%
  mutate_at(wd_filter, as.numeric) %>%
  mutate_if(is.numeric, round, 1) %>%
  filter(`Series Code` %in% wd_series, `Country Code` %in% wd_country) %>%
  select(`Country Name`, `Series Name`, `1995 [YR1995]`:`2018 [YR2018]`)

wd_indicators <- wd_indicators_f

#remove unnecessary filter vectors
rm(wd_indicators_f, wd_filter, wd_country, wd_series)

# ----------------------------------- S & P Composite ----------------------------------------------------
#change type of Year
spcomposite$Year <- as.Date(spcomposite$Year)

#change date to year, filter year
spcomposite_f <- spcomposite %>% 
  select(Year, Earnings, Real.Price, Real.Dividend, Real.Earnings) %>% 
  mutate_if(is.numeric, ~replace(., is.na(.), 0)) %>%
  mutate(spcomposite, Year = format(Year, format = "%Y")) %>% 
  group_by(Year) %>%
  summarise(TotalEarnings = mean(Earnings), TotalRealPrice = mean(Real.Price),
            TotalRealDividend = mean(Real.Dividend), TotalRealEarnings = mean(Real.Earnings)) %>%
  mutate_if(is.numeric, round, 2) %>%
  filter(Year >= "1995" & Year <= "2018") %>%
  arrange(desc(Year))

#assign to original df
spcomposite <- spcomposite_f

head(spcomposite) %>%
  rownames_to_column() %>%
  pivot_longer(, cols = -rowname) %>%
  pivot_wider(, names_from = rowname) %>%
  rename("Year" = 1) %>%
  as.data.frame()

spcomposite_t <- spcomposite %>%
  pivot_wider(names_from = Year)%>%
  mutate(Country = "World", Series = "S&P Composite") %>%
  select(Country, Series, everything())


#remove unnecessary filter vectors
rm(spcomposite_f)

# ----------------------------------------- Gold Prices --------------------------------------------------
#Change character to date
gold_prices$Date <- as.Date(gold_prices$Date)

#mean from USD A.M. and USD P.M.
gold_prices_f <- gold_prices %>%
  select(Date, USD..AM., USD..PM.) %>%
  mutate_if(is.numeric, ~replace(., is.na(.), 0)) %>%
  mutate(Usd = rowMeans(select(., USD..AM., USD..PM.), na.rm = TRUE)) %>%
  mutate(gold_prices, Date = format(Date, format = "%Y")) %>%
  group_by(Date) %>%
  summarise(GoldPriceinEuro = mean(Usd)) %>%
  mutate(GoldPriceinEuro = round(GoldPriceinEuro, 1)) %>%
  filter(Date >= "1995" & Date <= "2018") %>%
  arrange(desc(Date))

#assign to original df
gold_prices <- gold_prices_f

gold_prices <- gold_prices %>%
  pivot_wider(names_from = Date, values_from = GoldPriceinEuro)%>%
  mutate(Country = "World", Series = "Gold Price in Euro") %>%
  select(Country, Series, everything())

#remove unnecessary filter vectors
rm(gold_prices_f)

# ----------------------------------- Currency exchange rates---------------------------------------------
#Change character to date
currency_ex_rates$Date <- as.Date(currency_ex_rates$Date)

currency_ex_rates_f <- currency_ex_rates %>%
  select(Date, Canadian.Dollar, Chilean.Peso, Chinese.Yuan, Colombian.Peso, Czech.Koruna, Euro, Indian.Rupee,
         Japanese.Yen, Korean.Won, Nepalese.Rupee, Polish.Zloty, Qatar.Riyal, Russian.Ruble, Saudi.Arabian.Riyal) %>%
  mutate_if(is.numeric, ~replace(., is.na(.), 0)) %>%
  mutate(currency_ex_rates, Date = format(Date, format = "%Y"))  %>%
  group_by(Date) %>%
  summarise_if(is.numeric, mean, na.rm = TRUE) %>%
  mutate_if(is.numeric, round, 2) %>%
  arrange(desc(Date))

#assign to original df
currency_ex_rates <- currency_ex_rates_f

#remove unnecessary filter vectors
rm(currency_ex_rates_f)

# ------------------------------------- Dataframe for Bicoins---- ----------------------------------------

Date <- c("2008", "2007", "2006", "2005", "2004", "2003", "2002", "2001", 
          "2000", "1999", "1998", "1997", "1996", "1995")

MeanValue <- rep("0", 14)
MeanValue <- as.numeric(MeanValue)
df <- data.frame(Date, MeanValue)

# ------------------------------ mkpru - Bitcoin Market Price USD ----------------------------------------

#change character to date
bitcoin_mkpru$Date <- as.Date(bitcoin_mkpru$Date)
bitcoin_mkpru$Value <- as.Date(bitcoin_mkpru$Value)

bitcoin_mkpru_f <- bitcoin_mkpru %>%
  select(Date, Value) %>%
  mutate(bitcoin_mkpru, Date = format(Date, format = "%Y")) %>%
  group_by(Date) %>%
  summarise(MeanValue = mean(Value)) %>%
  mutate(MeanValue = round(MeanValue, 2)) %>%
  filter(Date <= "2018") %>%
  arrange(desc(Date))

#assign to original df
bitcoin_mkpru <- bitcoin_mkpru_f
bitcoin_mkpru <- rbind(bitcoin_mkpru, df)

#remove unnecessary filter vectors
rm(bitcoin_mkpru_f)

bitcoin_mkpru <- bitcoin_mkpru %>%
  pivot_wider(names_from = Date, values_from = MeanValue) %>%
  mutate(Country = "World", Series = "Bitcoin Mkpru") %>%
  select(Country, Series, everything())

# ----------------------- trvou - Bitcoin USD Exchange Trade Volume --------------------------------------

#change character to date
bitcoin_trvou$Date <- as.Date(bitcoin_trvou$Date)
bitcoin_trvou$Value <- as.Date(bitcoin_trvou$Value)

bitcoin_trvou_f <- bitcoin_trvou %>%
  select(Date, Value) %>%
  mutate(bitcoin_trvou, Date = format(Date, format = "%Y")) %>%
  group_by(Date) %>%
  summarise(MeanValue = mean(Value)) %>%
  mutate(MeanValue = round(MeanValue, 2)) %>%
  filter(Date <= "2018") %>%
  arrange(desc(Date))

#assign to original df
bitcoin_trvou <- bitcoin_trvou_f
bitcoin_trvou <- rbind(bitcoin_trvou, df)

#remove unnecessary filter vectors
rm(bitcoin_trvou_f)

bitcoin_trvou <- bitcoin_trvou %>%
  pivot_wider(names_from = Date, values_from = MeanValue) %>%
  mutate(Country = "World", Series = "Bitcoin Trvou") %>%
  select(Country, Series, everything())

# ----------------------- hrate - Bitcoin Hash Rate --------------------------------------

bitcoin_hrate$Date <- as.Date(bitcoin_hrate$Date)
bitcoin_hrate$Value <- as.numeric(bitcoin_hrate$Value)

bitcoin_hrate_f <- bitcoin_hrate %>%
  select(Date, Value) %>%
  mutate(bitcoin_hrate, Date = format(Date, format = "%Y")) %>%
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

bitcoin_hrate <- bitcoin_hrate %>%
  pivot_wider(names_from = Date, values_from = MeanValue) %>%
  mutate(Country = "World", Series = "Bitcoin Hrate") %>%
  select(Country, Series, everything())

# ----------------------- diff - Bitcoin Hash Rate --------------------------------------

bitcoin_diff$Date <- as.Date(bitcoin_diff$Date)
bitcoin_diff$Value <- as.numeric(bitcoin_diff$Value)

bitcoin_diff_f <- bitcoin_diff %>%
  select(Date, Value) %>%
  mutate(bitcoin_diff, Date = format(Date, format = "%Y")) %>%
  group_by(Date) %>%
  summarise(MeanValue = mean(Value)) %>%
  mutate(MeanValue = round(MeanValue, 2)) %>%
  filter(Date <= "2018") %>%
  arrange(desc(Date))

#assign to original df
bitcoin_diff <- bitcoin_diff_f
bitcoin_diff <- rbind(bitcoin_diff, df)

#remove unnecessary filter vectors
rm(bitcoin_diff_f, df, Date, MeanValue)

bitcoin_diff <- bitcoin_diff %>%
  pivot_wider(names_from = Date, values_from = MeanValue) %>%
  mutate(Country = "World", Series = "Bit coin Diff") %>%
  select(Country, Series, everything())

# -------------------------------- Change name of DF columns ---------------------------------------------

all_data <- rbind(bitcoin_diff, bitcoin_hrate, bitcoin_mkpru, bitcoin_trvou, gold_prices)
all_data <- all_data %>%
  mutate_if(is.character, as.numeric) %>%
  mutate_all(round,1)



summary(all_data)
rm(bitcoin)

#change column  names on df wd_indicators
colnames(wd_indicators) <- c("Country", "Series", "1995", "1996", "1997", "1998", "1999", "2000", "2001",
                             "2002", "2003", "2004", "2005", "2006", "2007", "2008", "2009", "2010",
                             "2011", "2012", "2013", "2014", "2015", "2016", "2017", "2018")



#remove unnecessary filter vectors