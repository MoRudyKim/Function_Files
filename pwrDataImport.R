library(tidyverse)
library(lubridate)
library(tidyquant)
library(gridExtra)

setwd("P:/R_Dev/Price_Related")

# date <- Sys.Date() - 3
# date <- format(as.Date(date), "%m%d%Y")

# date <- format(as.Date(ifelse(lubridate::wday(Sys.Date() -1 ) == 1 |
#                                 lubridate::wday(Sys.Date() -1 ) == 7 , "2099-12-31",Sys.Date() - 1)),"%m%d%Y")

# if(wday(Sys.Date()-1) %in% c(1,7)) {
#   stop("Date you entered is a weekend date. Please manually adjust the curve date.")
# } else {
#   date <- format(as.Date(Sys.Date() - 1), "%m%d%Y")
#}

curveDate <- function(vDate) {
  vDate <- ymd(vDate)
  if(wday(vDate) %in% c(2:6)) {
   res <- format(vDate,"%m%d%Y")
   return(res)
 } else {
   error = function(e) stop("Weekend Date")
 }
}
  
date <- curveDate(Sys.Date() -1)
  
  
path <- "\\\\porfiler02\\RMShared\\Power Curves\\"
dt <- read_csv(paste0(path,"PowerCurves_",date,".csv"))
dt <- dt %>%
  dplyr::filter(!`TIME INDEX` == 0)

dtm <- readRDS("PowerCurves.rds")
dtm <- as.data.frame(dtm)
dt <- as.data.frame(dt)


# dtm <- dtm %>%
#   filter(!`TIME INDEX` == 0)
# 
# saveRDS(dtm, "PowerCurves.rds")

names(dt) <- c("Sys_Date", "Stamp_Date", "Time_Index",
               "POD", "Tenor", "Peak","Offpeak")

data_convert <- function(x) {
  date_con <- as.Date(x, format = "%d-%b-%y")
  date_con
}

dt <- dt %>%
  mutate_at(vars(Sys_Date, Stamp_Date, Tenor), data_convert)

dt <- dt %>%
  mutate(Qtr = quarter(Tenor, with_year = TRUE),
         Wday = wday(Stamp_Date))

dt <- dt %>%
  mutate_at(vars(Time_Index,Wday), as.integer)

dt <- dt %>%
  group_by(Qtr, POD, Stamp_Date) %>%
  mutate(aPeak = mean(Peak, na.rm = TRUE),
         aOffPeak = mean(Offpeak, na.rm = TRUE))

dt <- as.data.frame(dt)

dt <- rbind(dtm, dt)
dt <- as.data.frame(dt)
saveRDS(dt,"PowerCurves.rds")
rm(dtm)

# colnames(dtm) <- c("Sys_Date", "Stamp_Date", "Time_Index",
#                    "POD", "Tenor", "Peak","Offpeak")
# dtm <- dtm %>%
#   mutate_at(vars(Sys_Date, Stamp_Date, Tenor), data_convert)
#   
# dtm <- dtm %>%
#   mutate(Qtr = quarter(Tenor, with_year = TRUE),
#          Wday = wday(Stamp_Date))
# 
# dtm <- dtm %>%
#   mutate_at(vars(Time_Index,Wday), as.integer)
# 
# dtm <- dtm %>%
#   group_by(Qtr, POD, Stamp_Date) %>%
#   mutate(aPeak = mean(Peak, na.rm = TRUE),
#          aOffPeak = mean(Offpeak, na.rm = TRUE))