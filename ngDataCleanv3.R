library(tidyverse)
library(lubridate)
library(tidyquant)
library(gridExtra)

curveDate <- function(vDate) {
  if(wday(vDate) == 1) {
    res <- format(vDate-2,"%m%d%Y")
    return(res)
  } else {
    res <- format(vDate,"%m%d%Y")
    return(res)
  }
}

data_convert <- function(x) {
  date_con <- as.Date(x, format = "%d-%b-%y")
  date_con
}

data_clean <- function(data) {
  
  tmp <- data %>%
    mutate_at(vars(CTM, Price_Date), data_convert) %>%
    mutate(Qtr = quarter(CTM, with_year = TRUE),
           wday = wday(Price_Date),
           wday = as.integer(wday))
  tmp <- tmp %>%
    group_by(Qtr, Price_Date) %>%
    mutate(avgPrice = mean(Price, na.rm = TRUE))
  
  tmp <- as.data.frame(tmp)
  
  return(tmp)
  
}

fullPriceTab <- function(Nymex, Basis, Jcond = c("CTM", "Price_Date")) {
  tmp <- inner_join(Nymex, Basis, by = Jcond) %>%
    select(CTM, Comp, Location, Price_Date, Price.x, Qtr.x, wday.x, Price.y) %>%
    rename(nymPrice = Price.x,
           Qtr = Qtr.x,
           wday = wday.x,
           basisPrice = Price.y) %>%
    group_by(CTM, Price_Date) %>%
    mutate(fullPrice = nymPrice + basisPrice) %>%
    filter(Price_Date <= format(as.Date(date,"%m%d%Y"),"%Y-%m-%d"))
  
  return(tmp)
}

fullPriceTab_nym <- function(Nymex, Basis, Jcond = c("CTM", "Price_Date")) {
  tmp <- inner_join(Nymex, Basis, by = Jcond) %>%
    select(CTM, Comp, Location, Price_Date, Price.x, Qtr.x, wday.x, Price.y) %>%
    rename(nymPrice = Price.x,
           Qtr = Qtr.x,
           wday = wday.x,
           basisPrice = Price.y) %>%
    group_by(CTM, Price_Date) %>%
    mutate(fullPrice = nymPrice) %>%
    filter(Price_Date <= format(as.Date(date,"%m%d%Y"),"%Y-%m-%d")) %>%
    mutate(Comp = "NYMEX")
  return(tmp)
}

ngmonthlyPriceData <- function(Data, Deliv_Month, Hub) {
  require(tidyverse)
  tmp <- Data %>%
    filter(CTM == format(as.Date(Deliv_Month, "%Y-%m-%d")),
           Location == Hub)
  tmp <- as.data.frame(tmp)
  return(tmp)
}

ngquarterlyPriceData <- function(data, Quarter, Hub) {
  require(tidyverse)
  tmp <- data %>%
    dplyr::group_by(Price_Date, Qtr) %>%
    filter(Location %in% Hub,
           Qtr == Quarter) %>%
    mutate(qStripPrice = mean(fullPrice, na.rm = TRUE)) 
  tmp <- as.data.frame(tmp)
  return(tmp)
}

ngmn_ReturnData <- function(mn_priceData) {
  require(tidyquant)
  require(tidyverse)
  
  retDt <- mn_priceData %>%
    select(Price_Date, fullPrice) %>%
    tq_transmute(
      mutate_fun = periodReturn,
      period = "daily",
      type = "log") %>%
    rename(Return = daily.returns)
  
  return(retDt)
}

ngmn_volData <- function(Data, numDays, numYrDays) {
  require(tidyverse)
  require(tidyquant)
  tmp <- ngmn_ReturnData(Data)
  Vol <- tmp %>%
    tq_mutate(
      select = Return,
      mutate_fun = rollapply,
      width = numDays,
      align = "right",
      by.column = FALSE,
      FUN = sd,
      na.rm = TRUE) %>%
    mutate(vol = (value * sqrt(numYrDays)) * 100)
  Vol <- na.omit(Vol)
  
  return(Vol)
}


ngqt_ReturnData <- function(qt_priceData) {
  require(tidyquant)
  require(tidyverse)
  
  retDt <- qt_priceData %>%
    select(Price_Date, qStripPrice) %>%
    tq_transmute(
      mutate_fun = periodReturn,
      period = "daily",
      type = "log") %>%
    rename(Return = daily.returns)
  
  return(retDt)
}

ngqt_VolData <- function(data, numDays, numYrDays) {
  require(tidyquant)
  require(tidyverse)
  
  tmp <- ngqt_ReturnData(data)
  vdt <- tmp %>%
    tq_mutate(
      select = Return,
      mutate_fun = rollapply,
      width = numDays,
      align = "right",
      by.column = FALSE,
      FUN = sd,
      na.rm = TRUE) %>%
    mutate(vol = (value * sqrt(numYrDays)) * 100)
  vdt <- na.omit(vdt)
  
  return(vdt)
}


