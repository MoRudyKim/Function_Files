library(tidyverse)
library(lubridate)
library(tidyquant)

setwd("P:/R_Dev/Price_Related")

mn_peakPrcPlot <- function(data, Tenor, Hub) {
  require(tidyverse)
  plot <- data %>%
    ggplot(aes(x = Stamp_Date, y = Peak)) + geom_line(aes(colour = Hub),size = 1) +
    scale_x_date(date_breaks = "1 month") +
    xlab("Time") + ylab("$/MWh") +
    theme(axis.text.x = element_text(size = 7, angle = 90, hjust = 0.95, vjust = 0.2),
          plot.title = element_text(hjust = 0.5),
          legend.justification = c("center","bottom"),
          legend.position = c("bottom"),
          # legend.text = element_text(size = 4),
          legend.key.size = unit(0.25,"cm"),
          plot.subtitle = element_text(hjust = 0.5, size = 6.5)) +
    labs(colour = "") +
    ggtitle(label = paste0("Peak Price: ",Hub), subtitle  = 
              paste0("Tenor: ",Tenor)) +
    geom_smooth(method = "gam", formula = y ~ s(x, bs = "cs"))
  
  return(plot)
}

mn_offpeakPrcPlot <- function(data, Tenor, Hub) {
  require(tidyverse)
  plot <- data %>%
    ggplot(aes(x = Stamp_Date, y = Offpeak)) + geom_line(aes(colour = Hub),size = 1) +
    scale_x_date(date_breaks = "1 month") +
    xlab("Time") + ylab("$/MWh") +
    theme(axis.text.x = element_text(size = 7, angle = 90, hjust = 0.95, vjust = 0.2),
          plot.title = element_text(hjust = 0.5),
          legend.justification = c("center","bottom"),
          legend.position = c("bottom"),
          # legend.text = element_text(size = 4),
          legend.key.size = unit(0.25,"cm"),
          plot.subtitle = element_text(hjust = 0.5, size = 6.5)) +
    labs(colour = "") +
    ggtitle(label = paste0("Off-Peak Price: ",Hub), subtitle  = 
              paste0("Tenor: ",Tenor)) +
    geom_smooth(method = "gam", formula = y ~ s(x, bs = "cs"))
  
  return(plot)
}

qt_peakPrcPlot <- function(data, Quarter, Hub) {
 require(tidyverse)
  plot <- data %>%
    ggplot(aes(x = Stamp_Date, y = aPeak)) + geom_line(aes(colour = Hub),size = 1) +
    scale_x_date(date_breaks = "1 month") +
    xlab("Time") + ylab("$/MWh") +
    theme(axis.text.x = element_text(size = 7, angle = 90, hjust = 0.95, vjust = 0.2),
          plot.title = element_text(hjust = 0.5),
          legend.justification = c("center","bottom"),
          legend.position = c("bottom"),
          # legend.text = element_text(size = 4),
          legend.key.size = unit(0.25,"cm"),
          plot.subtitle = element_text(hjust = 0.5, size = 6.5)) +
    labs(colour = "") +
    ggtitle(label = paste0("Peak Price: ",Hub), subtitle  = 
              paste0("Tenor: ",Quarter,"Q")) +
    geom_smooth(method = "gam", formula = y ~ s(x, bs = "cs"))
  
  return(plot)
}

qt_offpeakPrcPlot <- function(data, Quarter, Hub) {
  require(tidyverse)
  plot <- data %>%
    ggplot(aes(x = Stamp_Date, y = aOffPeak)) + geom_line(aes(colour = Hub),size = 1) +
    scale_x_date(date_breaks = "1 month") +
    xlab("Time") + ylab("$/MWh") +
    theme(axis.text.x = element_text(size = 7, angle = 90, hjust = 0.95, vjust = 0.2),
          plot.title = element_text(hjust = 0.5),
          legend.justification = c("center","bottom"),
          legend.position = c("bottom"),
          # legend.text = element_text(size = 4),
          legend.key.size = unit(0.25,"cm"),
          plot.subtitle = element_text(hjust = 0.5, size = 6.5)) +
    labs(colour = "") +
    ggtitle(label = paste0("Peak Price: ",Hub), subtitle  = 
              paste0("Tenor: ",Quarter,"Q")) +
   geom_smooth(method = "gam", formula = y ~ s(x, bs = "cs"))
  
  return(plot)
}

mn_peakVolPlot <- function(data, Month, Hub) {
  require(tidyverse)
  plot <- data %>%
    ggplot(aes(x = Stamp_Date, y = vol)) + geom_line(aes(color = "red"), size = 1) +
    xlab("Time") + ylab("Annualized Volatility (%)") +
    ggtitle(label = paste0("Peak Annualized Volatility: ", Hub),subtitle = paste0("Tenor: ", Month)) +
    scale_x_date(date_breaks = "2 month") +
    theme(axis.text.x = element_text(size = 7, angle = 90, hjust = 0.95, vjust = 0.2),
          plot.title = element_text(hjust = 0.5, size = 10),
          # legend.justification = c(1,0),
          # legend.position = c(0.75,0.8),
          # legend.text = element_text(size = 5),
          plot.subtitle = element_text(hjust = 0.5, size = 6.5),
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
    ggtitle(label = paste0("Off-Peak Annualized Volatility: ", Hub),subtitle = paste0("Tenor: ", Month)) +
    scale_x_date(date_breaks = "2 month") +
    theme(axis.text.x = element_text(size = 7, angle = 90, hjust = 0.95, vjust = 0.2),
          plot.title = element_text(hjust = 0.5, size = 10),
          # legend.justification = c(1,0),
          # legend.position = c(0.75,0.8),
          # legend.text = element_text(size = 5),
          plot.subtitle = element_text(hjust = 0.5, size = 6.5),
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
    ggtitle(label = paste0("Quarterly Peak Annualized Volatility: ", Hub),
            subtitle = paste0("Tenor: ", Quarter,"Q")) +
    scale_x_date(date_breaks = "2 month") +
    theme(axis.text.x = element_text(size = 7, angle = 90, hjust = 0.95, vjust = 0.2),
          plot.title = element_text(hjust = 0.5, size = 10),
          # legend.justification = c(1,0),
          # legend.position = c(0.75,0.8),
          # legend.text = element_text(size = 5),
          plot.subtitle = element_text(hjust = 0.5, size = 6.5),
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
    ggtitle(label = paste0("Quarterly Off-Peak Annualized Volatility: ", Hub),
            subtitle = paste0("Tenor: ", Quarter,"Q")) +
    scale_x_date(date_breaks = "2 month") +
    theme(axis.text.x = element_text(size = 7, angle = 90, hjust = 0.95, vjust = 0.2),
          plot.title = element_text(hjust = 0.5, size = 10),
          # legend.justification = c(1,0),
          # legend.position = c(0.75,0.8),
          # legend.text = element_text(size = 5),
          plot.subtitle = element_text(hjust = 0.5, size = 6.5),
          legend.position = "none") +
    labs(color = "") +
    geom_smooth(method = "loess", formula = y ~ x)
  
  return(plot)
}