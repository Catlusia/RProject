<<<<<<< HEAD
#serwd

install.packages("readxl")
library("readxl")

#import data from files
gold_prices <- read.csv("Data\\Gold prices.csv")
currency_ex_rates <- read.csv("Data\\CurrencyExchangeRates.csv")
spcomposite <- read.csv("Data\\S&P Composite.csv")
wd_indicators <- read_excel("Data\\World_Development_Indicators.xlsx")

bitcoin_metadata <- read.csv("Data\\BCHAIN_metadata.csv")
bitcoin_diff <- read.csv("Data\\BCHAIN-DIFF.csv")
bitcoin_hrate <- read.csv("Data\\BCHAIN-HRATE.csv")
bitcoin_mkpru <- read.csv("Data\\BCHAIN-MKPRU.csv")
bitcoin_trvou <- read.csv("Data\\BCHAIN-TRVOU.csv")

# ----------------------------------- Indicators ----------------------------------------------------

#filter vector for indicators
wd_indicators_sn <- c("SP.URB.TOTL", "SP.POP.TOTL", "SP.POP.TOTL.MA.IN", "SP.POP.TOTL.FE.IN", 
                      "SP.DYN.LE00.IN", "SL.UEM.TOTL.NE.ZS", "SL.UEM.ADVN.ZS", "SP.DYN.TO65.FE.ZS",
                      "SP.DYN.TO65.MA.ZS", "SH.STA.SUIC.P5", "SH.STA.SUIC.FE.P5", "SH.STA.SUIC.MA.P5",
                      "SH.STA.DIAB.ZS")
#wd_indicators_cc <- c("HIC", "UMC", "WLD", "LMY", "LIC", "MIC", "LMC")
#'%ni%' <- Negate("%in%")

#filter vector for countries
wd_indicators_cc <- c("AFG", "CAN", "CHL", "CHN", "COL", "CZE", "ETH", "FRA", "DEU", "GHA", "HKG", 
                      "IND", "JPN", "KOR", "NPL", "POL", "QAT", "RUS", "SAU", "USA")

#make a new dataframe with selected values
wd_indicators_f <- wd_indicators %>% 
  filter(`Series Code` %in% wd_indicators_sn, `Country Code` %in% wd_indicators_cc) %>% 
  select(-`Country Code`, -`Series Code`, -`1970 [YR1970]`)

# ----------------------------------- S & P Composite ----------------------------------------------------

#select columns from Composite
spcomposite_f <- select(spcomposite, Year, Earnings, Real.Price, Real.Dividend, Real.Earnings)

#change type of Year
class(spcomposite_f$Year)
spcomposite_f$Year <- as.Date(spcomposite_f$Year)

#change date to year, filter year
spcomposite_f <- spcomposite_f %>% 
  select(Year, Earnings, Real.Price, Real.Dividend, Real.Earnings) %>% 
  mutate(spcomposite_f, Year = format(Year, format = "%Y")) %>% 
  filter(Year >= "1979" & Year <= "2020")

#Mean of values by year - 2020 not complete
spcomposite_f <- group_by(spcomposite_f, Year) %>%
  summarise(TotalEarnings = mean(Earnings), TotalRealPrice = mean(Real.Price),
            TotalRealDividend = mean(Real.Dividend), TotalRealEarnings = mean(Real.Earnings))

# ----------------------------------------- Gold Prices --------------------------------------------------

gold_prices_f <- gold_prices %>%
  select(Date, EURO..AM., EURO..PM.) %>%
  mutate(Euro = rowMeans(select(., EURO..AM., EURO..PM.), na.rm = TRUE))
  

class(gold_prices_f$EURO..AM.)
class(gold_prices_f$EURO..PM.)



# -------------------------------- Change name of DF columns ---------------------------------------------

#later on final dataframe
colnames(wd_indicators_f) <- c("Country Name", "Indicator", "1971", "1972", "1973", "1974", "1975", "1976", "1977", "1978",
                               "1979", "1980", "1981", "1982", "1983", "1984", "1985", "1986", "1987", "1988", "1989", "1990",
                               "1991", "1992", "1993", "1994", "1995", "1996", "1997", "1998", "1999", "2000")

#remove unnecessary filter vectors
rm(wd_indicators_cc)
rm(wd_indicators_sn)

=======
#serwd

install.packages("readxl")
library("readxl")

#import data from files
gold_prices <- read.csv("Data\\Gold prices.csv")
currency_ex_rates <- read.csv("Data\\CurrencyExchangeRates.csv")
spcomposite <- read.csv("Data\\S&P Composite.csv")
wd_indicators <- read_excel("Data\\World_Development_Indicators.xlsx")

bitcoin_metadata <- read.csv("Data\\BCHAIN_metadata.csv")
bitcoin_diff <- read.csv("Data\\BCHAIN-DIFF.csv")
bitcoin_hrate <- read.csv("Data\\BCHAIN-HRATE.csv")
bitcoin_mkpru <- read.csv("Data\\BCHAIN-MKPRU.csv")
bitcoin_trvou <- read.csv("Data\\BCHAIN-TRVOU.csv")

# ----------------------------------- Indicators ----------------------------------------------------

#filter vector for indicators
wd_indicators_sn <- c("SP.URB.TOTL", "SP.POP.TOTL", "SP.POP.TOTL.MA.IN", "SP.POP.TOTL.FE.IN", 
                      "SP.DYN.LE00.IN", "SL.UEM.TOTL.NE.ZS", "SL.UEM.ADVN.ZS", "SP.DYN.TO65.FE.ZS",
                      "SP.DYN.TO65.MA.ZS", "SH.STA.SUIC.P5", "SH.STA.SUIC.FE.P5", "SH.STA.SUIC.MA.P5",
                      "SH.STA.DIAB.ZS")
#wd_indicators_cc <- c("HIC", "UMC", "WLD", "LMY", "LIC", "MIC", "LMC")
#'%ni%' <- Negate("%in%")

#filter vector for countries
wd_indicators_cc <- c("AFG", "CAN", "CHL", "CHN", "COL", "CZE", "ETH", "FRA", "DEU", "GHA", "HKG", 
                      "IND", "JPN", "KOR", "NPL", "POL", "QAT", "RUS", "SAU", "USA")

#make a new dataframe with selected values
wd_indicators_f <- wd_indicators %>% 
  filter(`Series Code` %in% wd_indicators_sn, `Country Code` %in% wd_indicators_cc) %>% 
  select(-`Country Code`, -`Series Code`, -`1970 [YR1970]`)

# ----------------------------------- S & P Composite ----------------------------------------------------

#select columns from Composite
spcomposite_f <- select(spcomposite, Year, Earnings, Real.Price, Real.Dividend, Real.Earnings)

#change type of Year
class(spcomposite_f$Year)
spcomposite_f$Year <- as.Date(spcomposite_f$Year)

#change date to year, filter year
spcomposite_f <- spcomposite_f %>% 
  select(Year, Earnings, Real.Price, Real.Dividend, Real.Earnings) %>% 
  mutate(spcomposite_f, Year = format(Year, format = "%Y")) %>% 
  filter(Year >= "1979" & Year <= "2020")

#Mean of values by year - 2020 not complete
spcomposite_f <- group_by(spcomposite_f, Year) %>%
  summarise(TotalEarnings = mean(Earnings), TotalRealPrice = mean(Real.Price),
            TotalRealDividend = mean(Real.Dividend), TotalRealEarnings = mean(Real.Earnings))

# ----------------------------------------- Gold Prices --------------------------------------------------

gold_prices_f <- gold_prices %>%
  select(Date, EURO..AM., EURO..PM.) %>%
  mutate(Euro = rowMeans(select(., EURO..AM., EURO..PM.), na.rm = TRUE))
  

class(gold_prices_f$EURO..AM.)
class(gold_prices_f$EURO..PM.)



# -------------------------------- Change name of DF columns ---------------------------------------------

#later on final dataframe
colnames(wd_indicators_f) <- c("Country Name", "Indicator", "1971", "1972", "1973", "1974", "1975", "1976", "1977", "1978",
                               "1979", "1980", "1981", "1982", "1983", "1984", "1985", "1986", "1987", "1988", "1989", "1990",
                               "1991", "1992", "1993", "1994", "1995", "1996", "1997", "1998", "1999", "2000")

#remove unnecessary filter vectors
rm(wd_indicators_cc)
rm(wd_indicators_sn)

>>>>>>> 194ab2552bacd988801eee260937d84697e50fb6
