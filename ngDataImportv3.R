library(tidyverse)
library(lubridate)
library(tidyquant)
library(gridExtra)

setwd("P:/R_Dev/Price_Related")
path <- "P:/R_Dev/Price_Related/Function_Files/"
path_f <- "\\\\porfiler02\\RMShared\\NG_Curves\\"
path3 <-  "P:/R_Dev/Price_Related/Output_Files/"

source(paste0(path,"ngDataCleanv3.R"))

date <- curveDate(Sys.Date() -1)
cutoff <- as.Date(format(as.Date(date, "%m%d%Y"),"%Y-%m-%d")) - 365

# nym <- read_csv(paste0(path_f,"NYMEXCurves_",date,".csv"))
# basis <- read_csv(paste0(path_f,"NGBasisCurves_",date,".csv"))
nym <- read_csv(paste0(path_f,"NYMEXCurves",".csv"))
basis <- read_csv(paste0(path_f,"NGBasisCurves",".csv"))

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

tmptab <- read_csv("ng_lkup.csv")
tmpnames <- c("CTM", "Price_Date", "Comp","nymPrice", "Qtr", "wday", "basisPrice",
              "fullPrice", "Location")
allin <- fullPriceTab(nym, basis)
allinnym <- fullPriceTab_nym(nym,basis)
allin <- left_join(allin, tmptab, by = "Comp")

names(allin) <- tmpnames

allin <- as.data.frame(allin)
hub_list <- unique(allin$Location)

rm(tmptab, tmpnames)
