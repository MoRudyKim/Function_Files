library(tidyverse)
library(lubridate)
library(tidyquant)

mn_price_plot <- function(data, time, hub) {
  tmp_plot <- data %>%
    ggplot(aes(x = Price_Date, y = fullPrice)) +
    geom_line(color = "red", size = 1) +
    xlab("Time") + ylab("$/MMBtu") +
    ggtitle(label = paste0(hub,": ","Price Trends"), subtitle = paste0("Tenor: ",time)) +
    scale_x_date(date_breaks = "1 week") +
    theme(axis.text.x = element_text(size = 7, angle = 90, hjust = 0.95, vjust = 0.2),
          plot.title = element_text(hjust = 0.5),
          plot.subtitle = element_text(hjust = 0.5, size = 9),
          legend.position = "none") +
    labs(color = "") +
    geom_smooth(method = "loess", formula = y ~ x)
}

qt_price_plot <- function(data, time, hub) {
  tmp_plot <- data %>%
    ggplot(aes(x = Price_Date, y = qStripPrice)) +
    geom_line(color = "red", size = 1) +
    xlab("Time") + ylab("$/MMBtu") +
    ggtitle(label = paste0(hub,": ","Price Trends"), subtitle = paste0("Tenor: ",time)) +
    scale_x_date(date_breaks = "1 week") +
    theme(axis.text.x = element_text(size = 7, angle = 90, hjust = 0.95, vjust = 0.2),
          plot.title = element_text(hjust = 0.5),
          plot.subtitle = element_text(hjust = 0.5, size = 9),
          legend.position = "none") +
    labs(color = "") +
    geom_smooth(method = "loess", formula = y ~ x)
}

volPlot <- function(data, time, Hub) {
  require(tidyverse)
  plot <- data %>%
    ggplot(aes(x = Price_Date, y = vol)) + geom_line(color = "red", size = 1) +
    xlab("Time") + ylab("Annualized Volatility (%)") +
    ggtitle(label = paste0(Hub,":"," ","Price Volatility"),subtitle = paste0("Tenor: ", time)) +
    scale_x_date(date_breaks = "1 week") +
    theme(axis.text.x = element_text(size = 7, angle = 90, hjust = 0.95, vjust = 0.2),
          plot.title = element_text(hjust = 0.5),
          plot.subtitle = element_text(hjust = 0.5, size = 9),
          legend.position = "none") +
    labs(color = "") +
    geom_smooth(method = "loess", formula = y ~ x)
  
  return(plot)
}

ondemandMnPrcPlot <- function(data,time, hub) {
  mdt <- monthlyPriceData(data, time, hub)
  tmp_plot <- mn_price_plot(mdt, time, hub)
  return(tmp_plot)
}

ondemandMnVolPlot <- function(data,time,hub, numDays, numYrDays) {
  mdt <- monthlyPriceData(data, time, hub)
  vdt <- mn_volData(mdt, numDays, numYrDays)
  tmp_plot <- volPlot(vdt,time,hub)
  return(tmp_plot)
}


ondemandQtrPrcPlot <- function(data, Quarter, Hub) {
  qdt <- quarterlyPriceData(data, Quarter, Hub)
  tmp_plot <- qt_price_plot(qdt, Quarter, Hub)
  return(tmp_plot)
}

ondemandQtrVolPlot <- function(data, time, hub, numDays, numYrDays) {
  qdt <- quarterlyPriceData(data, time, hub)
  vdt <- qt_VolData(qdt,numDays, numYrDays)
  tmp_plot <- volPlot(vdt, time, hub)
  return(tmp_plot)
}







