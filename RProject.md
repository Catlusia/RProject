---
title: "RProject"
author: "Alicja Kilińska"
date: '2022-05-16'
output: 
  html_document: 
    toc: yes
    keep_md: yes
    toc_float: yes
    theme: paper
---

<!-- 
Library to install:

- ggplot2
- plotly
- dplyr
- knitr
- DT
- readxl
-->

<!-- Load the libraries -->
<!-- Hide warnings and messages -->



<!-- Import Data from Files -->

```r
gold_prices <- read.csv("Data\\Gold prices.csv")
currency_ex_rates <- read.csv("Data\\CurrencyExchangeRates.csv")
s_p_composite <- read.csv("Data\\S&P Composite.csv")
wd_indicators <- read_excel("Data\\World_Development_Indicators.xlsx")
bitcoin_metadata <- read.csv("Data\\BCHAIN_metadata.csv")
bitcoin_diff <- read.csv("Data\\BCHAIN-DIFF.csv")
bitcoin_hrate <- read.csv("Data\\BCHAIN-HRATE.csv")
bitcoin_mkpru <- read.csv("Data\\BCHAIN-MKPRU.csv")
bitcoin_trvou <- read.csv("Data\\BCHAIN-TRVOU.csv")
```

# Executive summary

This is an R Markdown document. Markdown is a simple formatting syntax for authoring HTML, PDF, and MS Word documents. For more details on using R Markdown see <http://rmarkdown.rstudio.com>.

When you click the **Knit** button a document will be generated that includes both content as well as the output of any embedded R code chunks within the document. You can embed an R code chunk like this:


```r
summary(cars)
```

```
##      speed           dist       
##  Min.   : 4.0   Min.   :  2.00  
##  1st Qu.:12.0   1st Qu.: 26.00  
##  Median :15.0   Median : 36.00  
##  Mean   :15.4   Mean   : 42.98  
##  3rd Qu.:19.0   3rd Qu.: 56.00  
##  Max.   :25.0   Max.   :120.00
```

## Including Plots

You can also embed plots, for example:

![](RProject_files/figure-html/pressure-1.png)<!-- -->

Note that the `echo = FALSE` parameter was added to the code chunk to prevent printing of the R code that generated the plot.
