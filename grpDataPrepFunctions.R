library(tidyverse)
library(lubridate)
library(tidyquant)

setwd("P:/R_Dev/Price_Related")

monthlyPriceData <- function(data, Deliv_Month, pod) {
  require(tidyverse)
  tmp <- data %>%
    filter(Tenor == format(as.Date(Deliv_Month, "%Y-%m-%d")),
           POD == pod)
  tmp <- as.data.frame(tmp)
  return(tmp)
}

quarterlyPriceData <- function(data, Quarter, Hub) {
  require(tidyverse)
  tmp <- data %>%
    dplyr::group_by(Qtr, Stamp_Date) %>%
    filter(POD %in% Hub,
           Qtr == Quarter)
  tmp <- as.data.frame(tmp)
  return(tmp)
}

mn_peakReturnData <- function(mn_priceData) {
  require(tidyquant)
  require(tidyverse)
  
  retDt <- mn_priceData %>%
    select(Stamp_Date, Peak) %>%
    tq_transmute(
      select = Peak,
      mutate_fun = periodReturn,
      period = "daily",
      type = "log") %>%
    dplyr::rename(peakReturn = daily.returns)
  
  return(retDt)
}

mn_OffpeakReturnData <- function(mn_priceData) {
  require(tidyquant)
  require(tidyverse)
  
  retDt <- mn_priceData %>%
    select(Stamp_Date,Offpeak) %>%
    tq_transmute(
      select = Offpeak,
      mutate_fun = periodReturn,
      period = "daily",
      type = "log") %>%
    dplyr::rename(OffpeakReturn = daily.returns)
  
  return(retDt)
}


mn_peakVolData <- function(peakReturnData, numDays, numYrDays) {
  require(tidyverse)
  require(tidyquant)
  
  peakVol <- peakReturnData %>%
    tq_mutate(
      select = peakReturn,
      mutate_fun = rollapply,
      width = numDays,
      align = "right",
      by.column = FALSE,
      FUN = sd,
      na.rm = TRUE) %>%
    mutate(vol = (value * sqrt(numYrDays)) * 100)
  peakVol <- na.omit(peakVol)
  
  return(peakVol)
}

mn_OffpeakVolData <- function(OffpeakReturnData, numDays, numYrDays) {
  require(tidyverse)
  require(tidyquant)
  
  offpeakVol <- OffpeakReturnData %>%
    tq_mutate(
      select = OffpeakReturn,
      mutate_fun = rollapply,
      width = numDays,
      align = "right",
      by.column = FALSE, 
      FUN = sd,
      na.rm = TRUE) %>%
    mutate(vol = (value * sqrt(numYrDays)) * 100)
  offpeakVol <- na.omit(offpeakVol)
  
  return(offpeakVol)
}

qt_peakReturnData <- function(quarterlypriceData) {
  require(tidyverse)
  require(tidyquant)
  
  retDt <- quarterlypriceData %>%
    dplyr::group_by(Qtr) %>%
    select(Stamp_Date, Qtr, aPeak) %>%
    distinct(Stamp_Date, .keep_all = TRUE) %>%
    tq_transmute(
      select = aPeak,
      mutate_fun = periodReturn,
      period = "daily",
      type = "log") %>%
    dplyr::rename(peakReturn = daily.returns)
  
  return(retDt)
  
}

qt_offpeakReturnData <- function(quarterlyPriceData) {
  require(tidyquant)
  require(tidyverse)
  
  retDt <- quarterlyPriceData %>%
    dplyr::group_by(Qtr) %>%
    select(Stamp_Date, Qtr, aOffPeak) %>%
    tq_transmute(
      select = aOffPeak,
      mutate_fun = periodReturn,
      period = "daily",
      type = "log") %>%
    dplyr::rename(offpeakReturn = daily.returns)
  
  return(retDt)
  
}

qt_peakVolData <- function(qt_peakReturnData, numDays, numYrDays) {
  require(tidyquant)
  require(tidyverse)
  
  volDt <- qt_peakReturnData %>%
    tq_mutate(
      select = peakReturn,
      mutate_fun = rollapply,
      width = numDays,
      align = "right",
      by.column = FALSE,
      FUN = sd,
      na.rm = TRUE) %>%
    mutate(vol = (value * sqrt(numYrDays)) * 100 )
  
  volDt <- na.omit(volDt)
  
  return(volDt)
}

qt_offpeakVolData <- function(qt_offpeakReturnData, numDays, numYrDays) {
  require(tidyquant)
  require(tidyverse)
  
  volDt <- qt_offpeakReturnData %>%
    tq_mutate(
      select = offpeakReturn,
      mutate_fun = rollapply,
      width = numDays,
      align = "right",
      by.column = FALSE,
      FUN = sd,
      na.rm = TRUE) %>%
    mutate(vol = (value * sqrt(numYrDays)) * 100 )
  
  volDt <- na.omit(volDt)
  
  return(volDt)
}


xy_rollcor <- function(data, time, xhub, yhub, tou = 1) {
  x_prc <- monthlyPriceData(data, time, xhub)
  y_prc <- monthlyPriceData(data, time, yhub)
  
  if(tou == 1) {
    x_ret <- mn_peakReturnData(x_prc)
    y_ret <- mn_peakReturnData(y_prc)
    xy_ret <- inner_join(x_ret, y_ret, by = "Stamp_Date") %>%
      rename(xp = peakReturn.x, yp = peakReturn.y)
  } else {
    x_ret <- mn_OffpeakReturnData(x_prc)
    y_ret <- mn_OffpeakReturnData(y_prc)
    xy_ret <- inner_join(x_ret, y_ret, by = "Stamp_Date") %>%
      rename(xp = OffpeakReturn.x, yp = OffpeakReturn.y)
  }
  
  tmp <- xy_ret %>%
    tq_mutate_xy(
      x = xp,
      y = yp,
      mutate_fun = runCor,
      n = 30, 
      use = "pairwise.complete.obs",
      col_rename = "rolling_cor") %>%
    na.omit()
  
  return(tmp)
}




















