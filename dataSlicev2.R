library(tidyverse)
library(lubridate)
library(tidyquant)

path <- "P:/R_Dev/Price_Related/Price_Change_Functions/"
setwd("P:/R_Dev/Price_Related")

prcDt <- function(data, hub, date) {
  tmp <- data %>%
    filter(POD == hub) %>%
    group_by(Tenor) %>%
    filter(Stamp_Date == date) %>%
    arrange(Tenor)
  tmp <- as.data.frame(tmp)
  return(tmp)
}

dimCheckDt <- function(data, n = 39) {
  if(dim(data)[1] > n) {
    tmp <- dupRemoveDt(data)
    return(tmp)
  } else {
   return(data)
  }
}

dupRemoveDt <- function(data) {
  tmp <- data %>%
    distinct(Stamp_Date, .keep_all = TRUE)
  tmp <- as.data.frame(tmp)
  return(tmp)
}

sliceDt <- function(data) {
  tmp <- data %>%
    select(Stamp_Date, Tenor, Peak, Offpeak) %>%
    arrange(Tenor)  
  tmp <- as.data.frame(tmp)
  return(tmp)
}

graphData <- function(data1, data2) {
  tmp <- left_join(data1, data2, by = "Tenor") %>%
    rename(Peak = Peak.x,
           Offpeak = Offpeak.x,
           Peak_p = Peak.y,
           Offpeak_p = Offpeak.y,
           Curve_Date = Stamp_Date.x,
           Curve_Date_p = Stamp_Date.y,
           POD = POD.x) %>%
    select(Curve_Date, Tenor, POD, Peak, Offpeak, Peak_p, Offpeak_p) %>%
    mutate(peakChg = Peak - Peak_p,
           offpeakChg = Offpeak - Offpeak_p) %>%
    na.omit(tmp)
    
  
  return(tmp)
}

pwdt <- function(data, pwhub) {
  tmp <- data %>%
    filter(POD == pwhub) %>%
    select(Stamp_Date, POD, Tenor, Peak, Offpeak, Qtr, Wday)
  return(tmp)
}

ngdt <- function(data, ghub) {
  tmp <- data %>%
    filter(Comp == ghub)
  colnames(tmp) <- ngnames
  return(tmp)
}

hrdt <- function(powerdata, gasdata, powerhub, gashub) {
  t1 <- pwdt(powerdata, powerhub)
  t2 <- ngdt(gasdata,gashub)
  tmp <- inner_join(t1,t2, by = c("Tenor","Stamp_Date")) %>%
    select(Stamp_Date, POD, Comp, Tenor, Peak, Offpeak, Qtr.x, Wday, fullPrice) %>%
    rename(Qtr = Qtr.x) %>%
    mutate(impHR_peak = Peak/fullPrice,
           impHR_offpeak = Offpeak/fullPrice)
  return(tmp)
}


hrgraphdata <- function(data, powerhub, current_date,prior_date) {
  t1 <- prcDt(data, powerhub, current_date)
  t2 <- prcDt(data, powerhub, prior_date)
  t3 <- left_join(t1,t2, by = "Tenor") %>%
    rename(Stamp_Date = Stamp_Date.x,
           POD = POD.x,
           Comp = Comp.x,
           Peak_cur = Peak.x,
           Peak_pr = Peak.y,
           Offpeak_cur = Offpeak.x,
           Offpeak_pr = Offpeak.y,
           fullPrice_cur = fullPrice.x,
           fullPrice_pr = fullPrice.y,
           peakimphr_cur = impHR_peak.x,
           peakimphr_pr = impHR_peak.y,
           offpeakimphr_cur = impHR_offpeak.x,
           offpeakimphr_pr = impHR_offpeak.y,
           Qtr = Qtr.x, 
           wday = Wday.x) %>%
    select(Stamp_Date, POD, Comp, Tenor, Peak_cur, Offpeak_cur,
           Qtr, wday, fullPrice_cur, peakimphr_cur,peakimphr_pr,Peak_pr, Offpeak_pr,
           fullPrice_pr, offpeakimphr_cur,offpeakimphr_pr)
  tmp <- t3 %>%
    mutate(peakchg = Peak_cur - Peak_pr,
           Offpeakchg = Offpeak_cur - Offpeak_pr,
           fullPricechg = fullPrice_cur - fullPrice_pr,
           peakimphrchg = peakimphr_cur - peakimphr_pr,
           offpeakimphrchg = offpeakimphr_cur - offpeakimphr_pr)
  
  return(tmp)
}

