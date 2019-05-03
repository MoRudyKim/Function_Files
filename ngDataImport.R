library(tidyverse)
library(lubridate)
library(tidyquant)
library(gridExtra)

setwd("P:/R_Dev/Price_Related")
path <- "P:/R_Dev/Price_Related/Function_Files/"
path_f <- "\\\\porfiler02\\RMShared\\NG_Curves\\"
path3 <-  "P:/R_Dev/Price_Related/Output_Files/"

source(paste0(path,"ngDataClean.R"))
source(paste0(path,"ngPlotFunctions.R"))

vqtr <- 2019.3
vmn <- "2019-06-01"
vmn1 <- "2019-07-01"

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


for (each in hub_list) {
  mn_plot <- ondemandMnPrcPlot(allin, vmn, each)
  mn_plot_1 <- ondemandMnPrcPlot(allin, vmn1, each)
  mn_vol_plot <- ondemandMnVolPlot(allin, vmn, each, 30, 252)
  mn_vol_plot_1 <- ondemandMnVolPlot(allin, vmn1, each, 30, 252)
  qt_plot <- ondemandQtrPrcPlot(allin, vqtr, each)
  qt_vol_plot <- ondemandQtrVolPlot(allin, vqtr, each, 30, 252)
  
  pdf(file = paste0(path3,each,"_",date,"_NG_PrcTrends.pdf"), onefile = TRUE, paper = "USr")
  grid.arrange(mn_plot, mn_plot_1, nrow = 2, ncol = 1)
  grid.arrange(mn_vol_plot, mn_vol_plot_1, nrow = 2, ncol = 1)
  grid.arrange(qt_plot, qt_vol_plot, nrow = 2, ncol = 1)
  dev.off()
}




##################
# Test

# mprice <- monthlyPriceData(allin, vmn, "AECO")
# write.csv(mprice,"mprice.csv", row.names = FALSE)
# mreturn <- mn_ReturnData(mprice)
# write.csv(mreturn,"mreturn.csv", row.names = FALSE)
# mvol <- mn_volData(mprice, 30, 252)
# write.csv(mvol, "mvol.csv", row.names = FALSE)









