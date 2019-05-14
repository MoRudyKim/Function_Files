library(tidyverse)
library(lubridate)
library(tidyquant)

setwd("P:/R_Dev/Price_Related")

mn_peakPrcPlot <- function(data, Tenor, Hub) {
  require(tidyverse)
  plot <- data %>%
    ggplot(aes(x = Stamp_Date, y = Peak)) + geom_line(color = "red",size = 1) +
    scale_x_date(date_breaks = "1 month") +
    xlab("Time") + ylab("$/MWh") +
    theme(axis.text.x = element_text(size = 7, angle = 90, hjust = 0.95, vjust = 0.2),
          plot.title = element_text(hjust = 0.5),
          # legend.justification = c("center","bottom"),
          # legend.position = c("bottom"),
          # legend.text = element_text(size = 4),
          # legend.key.size = unit(0.25,"cm"),
          legend.position = "none",
          plot.subtitle = element_text(hjust = 0.5, size = 9)) +
    labs(colour = "") +
    ggtitle(label = paste0(Hub,":"," ",Tenor," Peak Price"), subtitle  = 
              paste0("Tenor: ",Tenor)) +
    geom_smooth(method = "gam", formula = y ~ s(x, bs = "cs"))
    
  
  return(plot)
}

mn_offpeakPrcPlot <- function(data, Tenor, Hub) {
  require(tidyverse)
  plot <- data %>%
    ggplot(aes(x = Stamp_Date, y = Offpeak)) + geom_line(color = "red",size = 1) +
    scale_x_date(date_breaks = "1 month") +
    xlab("Time") + ylab("$/MWh") +
    theme(axis.text.x = element_text(size = 7, angle = 90, hjust = 0.95, vjust = 0.2),
          plot.title = element_text(hjust = 0.5),
          # legend.justification = c("center","bottom"),
          # legend.position = c("bottom"),
          # legend.text = element_text(size = 4),
          # legend.key.size = unit(0.25,"cm"),
          legend.position = "none",
          plot.subtitle = element_text(hjust = 0.5, size = 9)) +
    labs(colour = "") +
    ggtitle(label = paste0(Hub,":"," ",Tenor," Off-Peak Price"), subtitle  = 
              paste0("Tenor: ",Tenor)) +
    geom_smooth(method = "gam", formula = y ~ s(x, bs = "cs"))
  
  return(plot)
}

qt_peakPrcPlot <- function(data, Quarter, Hub) {
 require(tidyverse)
  plot <- data %>%
    ggplot(aes(x = Stamp_Date, y = aPeak)) + geom_line(color = "red",size = 1) +
    scale_x_date(date_breaks = "1 month") +
    xlab("Time") + ylab("$/MWh") +
    theme(axis.text.x = element_text(size = 7, angle = 90, hjust = 0.95, vjust = 0.2),
          plot.title = element_text(hjust = 0.5),
          # legend.justification = c("center","bottom"),
          # legend.position = c("bottom"),
          # legend.text = element_text(size = 4),
          # legend.key.size = unit(0.25,"cm"),
          legend.position = "none",
          plot.subtitle = element_text(hjust = 0.5, size = 9)) +
    labs(colour = "") +
    ggtitle(label = paste0(Hub," Peak Price: ", max(data$Stamp_Date)), subtitle  = 
              paste0("Tenor: ",Quarter)) +
    geom_smooth(method = "gam", formula = y ~ s(x, bs = "cs"))
  
  return(plot)
}

qt_offpeakPrcPlot <- function(data, Quarter, Hub) {
  require(tidyverse)
  plot <- data %>%
    ggplot(aes(x = Stamp_Date, y = aOffPeak)) + geom_line(color = "red",size = 1) +
    scale_x_date(date_breaks = "1 month") +
    xlab("Time") + ylab("$/MWh") +
    theme(axis.text.x = element_text(size = 7, angle = 90, hjust = 0.95, vjust = 0.2),
          plot.title = element_text(hjust = 0.5),
          # legend.justification = c("center","bottom"),
          # legend.position = c("bottom"),
          # legend.text = element_text(size = 4),
          # legend.key.size = unit(0.25,"cm"),
          legend.position = "none",
          plot.subtitle = element_text(hjust = 0.5, size = 9)) +
    labs(colour = "") +
    ggtitle(label = paste0(Hub," Off-Peak Price: ", max(data$Stamp_Date)), subtitle  = 
              paste0("Tenor: ",Quarter)) +
    geom_smooth(method = "gam", formula = y ~ s(x, bs = "cs"))
    
  
  return(plot)
}

mn_peakVolPlot <- function(data, Month, Hub) {
  require(tidyverse)
  plot <- data %>%
    ggplot(aes(x = Stamp_Date, y = vol)) + geom_line(aes(color = "red"), size = 1) +
    xlab("Time") + ylab("Annualized Volatility (%)") +
    ggtitle(label = paste0(Hub,":"," ","Peak Price Volatility"),subtitle = paste0("Tenor: ", Month)) +
    scale_x_date(date_breaks = "1 month") +
    theme(axis.text.x = element_text(size = 7, angle = 90, hjust = 0.95, vjust = 0.2),
          plot.title = element_text(hjust = 0.5),
          # legend.justification = c(1,0),
          # legend.position = c(0.75,0.8),
          # legend.text = element_text(size = 5),
          plot.subtitle = element_text(hjust = 0.5, size = 9),
          legend.position = "none") +
    labs(color = "") +
    #geom_smooth(method = "gam", formula = y ~ s(x, bs = "cs"))
    geom_smooth(method = "loess", formula = y ~ x)
  
  return(plot)
}


mn_offpeakVolPlot <- function(data, Month, Hub) {
  require(tidyverse)
  plot <- data %>%
    ggplot(aes(x = Stamp_Date, y = vol)) + geom_line(aes(color = "red"), size = 1) +
    xlab("Time") + ylab("Annualized Volatility (%)") +
    ggtitle(label = paste0(Hub,":"," ","Off-Peak Price Volatility"),subtitle = paste0("Tenor: ", Month)) +
    scale_x_date(date_breaks = "1 month") +
    theme(axis.text.x = element_text(size = 7, angle = 90, hjust = 0.95, vjust = 0.2),
          plot.title = element_text(hjust = 0.5),
          # legend.justification = c(1,0),
          # legend.position = c(0.75,0.8),
          # legend.text = element_text(size = 5),
          plot.subtitle = element_text(hjust = 0.5, size = 9),
          legend.position = "none") +
    labs(color = "") +
    #geom_smooth(method = "gam", formula = y ~ s(x, bs = "cs"))
    geom_smooth(method = "loess", formula = y ~ x)
  
  return(plot)
}

qt_peakVolPlot <- function(data, Quarter, Hub) {
  require(tidyverse)
  plot <- data %>%
    ggplot(aes(x = Stamp_Date, y = vol)) + geom_line(aes(color = "red"), size = 1) +
    xlab("Time") + ylab("Annualized Volatility (%)") +
    ggtitle(label = paste0(Hub," : ",Quarter, " Peak Volatility"),
            subtitle = paste0("Tenor: ", Quarter)) +
    scale_x_date(date_breaks = "1 month") +
    theme(axis.text.x = element_text(size = 7, angle = 90, hjust = 0.95, vjust = 0.2),
          plot.title = element_text(hjust = 0.5),
          # legend.justification = c(1,0),
          # legend.position = c(0.75,0.8),
          # legend.text = element_text(size = 5),
          plot.subtitle = element_text(hjust = 0.5, size = 9),
          legend.position = "none") +
    labs(color = "") +
    geom_smooth(method = "loess", formula = y ~ x)
  
  return(plot)
}

qt_offpeakVolPlot <- function(data, Quarter, Hub) {
  require(tidyverse)
  plot <- data %>%
    ggplot(aes(x = Stamp_Date, y = vol)) + geom_line(aes(color = "red"), size = 1) +
    xlab("Time") + ylab("Annualized Volatility (%)") +
    ggtitle(label = paste0(Hub," : "," Off-Peak Volatility"),
            subtitle = paste0("Tenor: ", Quarter)) +
    scale_x_date(date_breaks = "1 month") +
    theme(axis.text.x = element_text(size = 7, angle = 90, hjust = 0.95, vjust = 0.2),
          plot.title = element_text(hjust = 0.5),
          # legend.justification = c(1,0),
          # legend.position = c(0.75,0.8),
          # legend.text = element_text(size = 5),
          plot.subtitle = element_text(hjust = 0.5, size = 9),
          legend.position = "none") +
    labs(color = "") +
    geom_smooth(method = "loess", formula = y ~ x)
  
  return(plot)
}


ondemandQtrPrcPlot <- function(data, time, hub, tou) {
  tdt <- quarterlyPriceData(data, time, hub)
  if(tou == 1) {
    tmp_plot <- qt_peakPrcPlot(data = tdt, Quarter = vqtr, Hub = hub)
  } else {
    tmp_plot <- qt_offpeakPrcPlot(data = tdt, Quarter = vqtr, Hub = hub)
  }
}

ondemandQtrVolPlot <- function(data, time, hub, tou) {
  tdt <- quarterlyPriceData(data, time, hub)
  if(tou == 1) {
    rdt <- qt_peakReturnData(tdt)
    vdt <- qt_peakVolData(rdt, 30, 252)
    tmp_plot <- qt_peakVolPlot(vdt, vqtr, hub)
    return(tmp_plot)
  } else {
    rdt <- qt_offpeakReturnData(tdt)
    vdt <- qt_offpeakVolData(rdt, 30, 252)
    tmp_plot <- qt_offpeakVolPlot(vdt, vqtr, hub)
    return(tmp_plot)
  }
}

ondemandMnPrcPlot <- function(data = dt, time, hub, tou) {
  mdt <- monthlyPriceData(data, time, hub)
  if(tou == 1) {
    tmp_plot <- mn_peakPrcPlot(mdt, time, hub)
  } else {
    tmp_plot <- mn_offpeakPrcPlot(mdt, time, hub)
  }
}

ondemandMnVolplot <- function(data = dt, time, hub, tou) {
  mdt <- monthlyPriceData(data, time, hub)
  if(tou ==1) {
    rdt <- mn_peakReturnData(mdt)
    vdt <- mn_peakVolData(rdt, 30, 252)
    tmp_plot <- mn_peakVolPlot(vdt, time, hub)
    return(tmp_plot)
  } else {
    rdt <- mn_OffpeakReturnData(mdt)
    vdt <- mn_OffpeakVolData(rdt, 30, 252)
    tmp_plot <- mn_offpeakVolPlot(vdt, time, hub)
    return(tmp_plot)
  }
}

xy_rollcor_plot <- function(data, time, xhub, yhub, tou = "Peak") {
  plot <- data %>%
    ggplot(aes(x = Stamp_Date, y = rolling_cor)) +
    geom_line(aes(color = "red"), size = 1) +
    xlab("Time") + ylab("Correlation") +
    ggtitle(label = paste0(xhub," : ",yhub," -- ",tou," 30-Day Rolling Correlation"),
            subtitle = paste0("Tenor: ", time)) +
    scale_x_date(date_breaks = "1 week") +
    theme(axis.text.x = element_text(size = 7, angle = 90, hjust = 0.95, vjust = 0.2),
          plot.title = element_text(hjust = 0.5),
          # legend.justification = c(1,0),
          # legend.position = c(0.75,0.8),
          # legend.text = element_text(size = 5),
          plot.subtitle = element_text(hjust = 0.5, size = 9),
          legend.position = "none") +
    labs(color = "") +
    geom_smooth(method = "loess", formula = y ~ x)
  
  return(plot)
}

ondemandRollCorPlot <- function(data, time, xhub, yhub, tou = "Peak") {
  if(tou == "Peak") {
    dt <- xy_rollcor(data, time, xhub, yhub, 1)
    tmp_plot <- xy_rollcor_plot(dt, time, xhub, yhub, tou)
  } else {
    dt <- xy_rollcor(data, time, xhub, yhub, 0)
    tmp_plot <- xy_rollcor_plot(dt, time, xhub, yhub, tou = "OffPeak")
  }
  return(tmp_plot)
}