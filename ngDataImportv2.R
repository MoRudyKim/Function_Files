library(tidyverse)
library(lubridate)
library(tidyquant)
library(gridExtra)

setwd("P:/R_Dev/Price_Related")
path <- "P:/R_Dev/Price_Related/Function_Files/"
path_f <- "\\\\porfiler02\\RMShared\\NG_Curves\\"
path3 <-  "P:/R_Dev/Price_Related/Output_Files/"

source(paste0(path,"ngDataClean.R"))

vqtr <- 2019.3
vmn <- as.Date("2019-07-01")
vmn1 <- as.Date("2019-08-01")

date <- curveDate(Sys.Date() -1)
cutoff <- as.Date(format(as.Date(date, "%m%d%Y"),"%Y-%m-%d")) - 180

nym <- read_csv(paste0(path_f,"NYMEXCurves_",date,".csv"))
basis <- read_csv(paste0(path_f,"NGBasisCurves_",date,".csv"))

names(nym) <- c("CTM", "Price_Date", "Price")
names(basis) <- c("CTM", "Price_Date", "Price","Comp")
nym <- as.data.frame(nym)
basis <- as.data.frame(basis)

nym <- data_clean(nym)
basis <- data_clean(basis)  

nym <- nym %>%
  filter(Price_Date >= cutoff)

basis <- basis %>%
  filter(Price_Date >= cutoff)

allin <- fullPriceTab(nym, basis)

hub_list <- unique(allin$Comp)

rm(list = ls()[!ls() %in% c("allin","hub_list","date","cutoff","path")])




