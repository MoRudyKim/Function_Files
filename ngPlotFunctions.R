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
    scale_y_continuous(breaks = scales::pretty_breaks(n = 10)) +
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
    scale_y_continuous(breaks = scales::pretty_breaks(n = 10)) +
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
    scale_y_continuous(breaks = scales::pretty_breaks(n = 10)) +
    theme(axis.text.x = element_text(size = 7, angle = 90, hjust = 0.95, vjust = 0.2),
          plot.title = element_text(hjust = 0.5),
          plot.subtitle = element_text(hjust = 0.5, size = 9),
          legend.position = "none") +
    labs(color = "") +
    geom_smooth(method = "loess", formula = y ~ x)
  
  return(plot)
}

nymPlot <- function(data, curvedate = cdate) {
  plot <- data %>%
    filter(Price_Date == as.Date(cdate)) %>%
    ggplot(aes(x = CTM, y = fullPrice)) + geom_line(color = "blue", size = 1.5) +
    xlab("Delivery Month") + ylab("$/MMBtu") +
    ggtitle(label = paste0("NYMEX Price Curve"), subtitle = paste0("As of: ", curvedate)) +
    scale_x_date(date_breaks = "2 month") +
    scale_y_continuous(breaks = scales::pretty_breaks(n = 10)) +
    theme(axis.text.x = element_text(size = 7, angle = 90, hjust = 0.95, vjust = 0.2),
          plot.title = element_text(hjust = 0.5),
          plot.subtitle = element_text(hjust = 0.5, size = 9),
          legend.position = "none") +
    labs(color = "")
  
  return(plot)
}

nymTimePlot <- function(data, current_date, prior_date) {
  plot <- data %>%
    ggplot() +
    geom_line(aes(x = CTM, y = Current_Price), color = "blue", size = 1.25) +
    geom_line(aes(x = CTM, y = Prior_Price), color = "red", size = 1.25) +
    xlab("Delivery Month") + ylab("$/MMBtu") +
    ggtitle(label = paste0("NYMEX Price Curve"," As of ",current_date," and ",prior_date), 
            subtitle = paste0("Blue Line = Current Date"," and ","Red Line = Prior Date")) +
    scale_x_date(date_breaks = "2 month") +
    scale_y_continuous(breaks = scales::pretty_breaks(n = 10)) +
    theme(axis.text.x = element_text(size = 7, angle = 90, hjust = 0.95, vjust = 0.2),
          plot.title = element_text(hjust = 0.5),
          plot.subtitle = element_text(hjust = 0.5, size = 9),
          legend.position = "none") +
    labs(color = "")
}

gasChangePlot <- function(data, hub, date1, date2) {
  plot <- data %>%
    ggplot() +
    geom_line(aes(x = CTM, y = Price_Change), color = "blue", size = 1.250) +
    xlab("Delivery Month") + ylab("$/MMBtu") + 
    ggtitle(label = paste0(hub," Price Curve Change"," from ",date2," to ",date1)) +
    scale_x_date(date_breaks = "2 month") +
    scale_y_continuous(breaks = scales::pretty_breaks(n = 10)) +
    theme(axis.text.x = element_text(size = 7, angle = 90, hjust = 0.95, vjust = 0.2),
          plot.title = element_text(hjust = 0.5),
          plot.subtitle = element_text(hjust = 0.5, size = 9),
          legend.position = "none") +
    labs(color = "")
}

gasBasisPlot <- function(data, hub1, hub2, date1) {
  plot <- data %>% 
    ggplot() +
    geom_line(aes(x = CTM, y = Hub1_Price), color = "blue", size = 1.25) +
    geom_line(aes(x = CTM, y = Hub2_Price), color = "red", size = 1.25) +
    geom_line(aes(x = CTM, y = Locational_Basis), color = "black", size = 1.5) +
    xlab("Contract Month") + ylab("Basis vs. Henry Hub, $/MMBtu") +
    ggtitle(label = paste0(hub1," vs. ",hub2," -- ","As of: ",date1),
            subtitle = paste0("Blue = ",hub1," | ","Red = ",hub2,
                              " | ","Black = Locational Basis")) + 
    scale_x_date(date_breaks = "2 month") +
    scale_y_continuous(breaks = scales::pretty_breaks(n = 10)) +
    theme(axis.text.x = element_text(size = 7, angle = 90, hjust = 0.95, vjust = 0.2),
          plot.title = element_text(hjust = 0.5),
          plot.subtitle = element_text(hjust = 0.5, size = 9),
          legend.position = "none") +
    labs(color = "")
}         


ondemandNgMnPrcPlot <- function(data,time, hub) {
  mdt <- ngmonthlyPriceData(data, time, hub)
  tmp_plot <- mn_price_plot(mdt, time, hub)
  return(tmp_plot)
}

ondemandNgMnVolPlot <- function(data,time,hub, numDays, numYrDays) {
  mdt <- ngmonthlyPriceData(data, time, hub)
  vdt <- ngmn_volData(mdt, numDays, numYrDays)
  tmp_plot <- volPlot(vdt,time,hub)
  return(tmp_plot)
}


ondemandNgQtrPrcPlot <- function(data, Quarter, Hub) {
  qdt <- ngquarterlyPriceData(data, Quarter, Hub)
  tmp_plot <- qt_price_plot(qdt, Quarter, Hub)
  return(tmp_plot)
}

ondemandNgQtrVolPlot <- function(data, time, hub, numDays, numYrDays) {
  qdt <- ngquarterlyPriceData(data, time, hub)
  vdt <- ngqt_VolData(qdt,numDays, numYrDays)
  tmp_plot <- volPlot(vdt, time, hub)
  return(tmp_plot)
}

ondemandNymexPlot <- function(data, current_date, prior_date) {
  tmp <- nymCtmData(data, current_date, prior_date)
  plot <- nymTimePlot(tmp, current_date, prior_date)
  
  return(plot)
}

ondemandGasChangePlot <- function(data, hub, date1 = current_date, date2 = prior_date) {
  tmp <- gasData(data, hub, date1, date2)
  plot <- gasChangePlot(tmp, hub, date1, date2)
  return(plot)
}

ondemandGasBasisPairPlot <- function(data, hub1, hub2, date) {
  tmp <- gasBasisPairData(data, hub1, hub2, date)
  plot <- gasBasisPlot(tmp, hub1, hub2, date)
  return(plot)
}