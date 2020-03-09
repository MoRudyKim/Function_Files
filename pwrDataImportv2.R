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

date <- curveDate(Sys.Date() -1)
 
path_p <- "\\\\porfiler02\\RMShared\\Power Curves\\"
# dt <- read_csv(paste0(path_p,"PowerCurves_",date,".csv"))
dt <- read_csv(paste0(path_p,"PowerCurves",".csv"))
dt <- as.data.frame(dt)

names(dt) <- c("Sys_Date", "Stamp_Date", "Time_Index",
               "POD", "Tenor", "Peak","Offpeak")

tpath <- "P:/R_Dev/Price_Related"
tmptab <- read_csv(paste0(tpath,"/","pw_lkup.csv"))

data_convert <- function(x) {
  date_con <- as.Date(x, format = "%d-%b-%y")
  date_con
}

dt <- dt %>%
  mutate_at(vars(Sys_Date, Stamp_Date, Tenor), data_convert) %>%
  as.data.frame()

dt <- dt %>%
  mutate(Qtr = quarter(Tenor, with_year = TRUE),
         Wday = wday(Stamp_Date)) %>%
  as.data.frame()

dt <- dt %>%
  mutate_at(vars(Time_Index,Wday), as.integer) %>%
  as.data.frame()

dt <- dt %>%
  group_by(Qtr, POD, Stamp_Date) %>%
  mutate(aPeak = mean(Peak, na.rm = TRUE),
         aOffPeak = mean(Offpeak, na.rm = TRUE))

dt <- as.data.frame(dt)

cutoff <- max(dt$Stamp_Date) - 180

dt <- dt %>%
  filter(Stamp_Date > cutoff)

dt <- left_join(dt, tmptab, by = "POD")

dt <- as.data.frame(dt)

rm(tmptab, tpath, path_p)
