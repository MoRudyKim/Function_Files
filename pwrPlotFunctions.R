library(tidyverse)
library(lubridate)
library(tidyquant)

f <- function(y) seq(floor(min(y)), ceiling(max(y)))
# Monthly Price and Vol Plots

mn_peakPrcPlot <- function(data, Tenor, Hub) {
  require(tidyverse)
  plot <- data %>%
    ggplot(aes(x = Stamp_Date, y = Peak)) + geom_line(color = "red",size = 1) +
    scale_x_date(date_breaks = "1 month") +
    scale_y_continuous(breaks = scales::pretty_breaks(n = 10)) +
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
    scale_y_continuous(breaks = scales::pretty_breaks(n = 10)) +
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

mn_peakVolPlot <- function(data, Month, Hub) {
  require(tidyverse)
  plot <- data %>%
    ggplot(aes(x = Stamp_Date, y = vol)) + geom_line(aes(color = "red"), size = 1) +
    xlab("Time") + ylab("Annualized Volatility (%)") +
    ggtitle(label = paste0(Hub,":"," ","Peak Price Volatility"),subtitle = paste0("Tenor: ", Month)) +
    scale_x_date(date_breaks = "1 month") +
    scale_y_continuous(breaks = scales::pretty_breaks(n = 10)) +
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
    scale_y_continuous(breaks = scales::pretty_breaks(n = 10)) +
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

# Quarterly Price and Vol Plots

qt_peakPrcPlot <- function(data, Quarter, Hub) {
  require(tidyverse)
  plot <- data %>%
    ggplot(aes(x = Stamp_Date, y = aPeak)) + geom_line(color = "red",size = 1) +
    scale_x_date(date_breaks = "1 month") +
    scale_y_continuous(breaks = scales::pretty_breaks(n = 10)) +
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
    scale_y_continuous(breaks = scales::pretty_breaks(n = 10)) +
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



qt_peakVolPlot <- function(data, Quarter, Hub) {
  require(tidyverse)
  plot <- data %>%
    ggplot(aes(x = Stamp_Date, y = vol)) + geom_line(aes(color = "red"), size = 1) +
    xlab("Time") + ylab("Annualized Volatility (%)") +
    ggtitle(label = paste0(Hub," : ",Quarter, " Peak Volatility"),
            subtitle = paste0("Tenor: ", Quarter)) +
    scale_x_date(date_breaks = "1 month") +
    scale_y_continuous(breaks = scales::pretty_breaks(n = 10)) +
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
    scale_y_continuous(breaks = scales::pretty_breaks(n = 10)) +
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

# Correlation Plots

xy_rollcor_plot <- function(data, time, xhub, yhub, tou = "Peak") {
  plot <- data %>%
    ggplot(aes(x = Stamp_Date, y = rolling_cor)) +
    geom_line(aes(color = "red"), size = 1) +
    xlab("Time") + ylab("Correlation") +
    ggtitle(label = paste0(xhub," : ",yhub," -- ",tou," 30-Day Rolling Correlation"),
            subtitle = paste0("Tenor: ", time)) +
    scale_x_date(date_breaks = "1 week") +
    scale_y_continuous(breaks = scales::pretty_breaks(n=15)) +
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

# Heat Rate Plots

hr_plot <- function(data, powerhub, gashub, time) {
  plot <- data %>%
    ggplot() +
    geom_line(aes(x = Stamp_Date, y = impHR_peak), color = "blue", size = 1.5) +
    geom_line(aes(x = Stamp_Date, y = impHR_offpeak), color = "red", size = 1.5) +
    scale_x_date(date_breaks = "1 week") +
    scale_y_continuous(breaks = scales::pretty_breaks(n = 15)) +
    xlab("Tenor") + ylab("Implied Heat Rate") +
    theme(axis.text.x = element_text(size = 7, angle = 90, hjust = 0.95, vjust = 0.2),
          plot.title = element_text(hjust = 0.5),
          legend.position = "none",
          plot.subtitle = element_text(hjust = 0.5, size = 9)) +
    labs(colour = "") +
    ggtitle(label = paste0(powerhub,"--",gashub,":"," "," Imp Heat Rate"), subtitle  = 
              paste0("Tenor: ",time," ","<Blue = Peak, Red = Offpeak>")) 
  
  return(plot)
}


# Price and Heat Rate Change Plots



changePlot <- function(data) {
  tmp_plot <- data %>%
    ggplot() +
    geom_line(aes(x = Tenor, y = peakChg), size = 1.5, color = "blue") +
    geom_line(aes(x = Tenor, y = offpeakChg), size = 1.5, color = "red") +
    scale_x_date(date_breaks = "2 month") +
    scale_y_continuous(labels = scales::dollar, breaks = scales::pretty_breaks(n = 15)) +
    theme(axis.text.x = element_text(size = 9, angle = 90, hjust = 0.95, vjust = 0.2),
          plot.title = element_text(hjust = 0.5),
          legend.justification = c(1,0),
          legend.position = "bottom",
          legend.text = element_text(size = 5),
          plot.subtitle = element_text(hjust = 0.5, size = 6.5)) +
    labs(colour = "") +
    ggtitle(label = paste0(data$POD," ", "Price Curve Change: ",
                           pdate," to ",cdate),
            subtitle = "Blue Line = Peak, Red Line = Off Peak") +
    ylab("Price Changes") + xlab("Delivery Month")
  
  return(tmp_plot)
  
}

hrchangePlot <- function(data) {
#   f1 <- min(data$offpeakimphrchg,na.rm = TRUE)
#   f2 <- max(data$peakimphrchg, na.rm = TRUE)
#   fn <- length(data$Stamp_Date)/10
#   fb <- (abs(f1) + abs(f2))/fn
  tmp_plot <- data %>%
    ggplot() +
    geom_line(aes(x = Tenor, y = peakimphrchg), size = 1.5, color = "blue") +
    geom_line(aes(x = Tenor, y = offpeakimphrchg), size = 1.5, color = "red") +
    scale_x_date(date_breaks = "2 month") +
    scale_y_continuous(breaks = scales::pretty_breaks(n = 15)) +
    theme(axis.text.x = element_text(size = 9, angle = 90, hjust = 0.95, vjust = 0.2),
          plot.title = element_text(hjust = 0.5),
          legend.justification = c(1,0),
          legend.position = "bottom",
          legend.text = element_text(size = 5),
          plot.subtitle = element_text(hjust = 0.5, size = 6.5)) +
    labs(colour = "") +
    ggtitle(label = paste0(data$POD," -- ",data$Comp," :", "Imp. HR Change: ",
                           pdate," to ",cdate),
            subtitle = "Blue Line = Peak, Red Line = Off Peak") +
    ylab("Implied Heat Rate Changes") + xlab("Delivery Month")
  
  return(tmp_plot)
  
}

spreadPlot <- function(data, DelivMonth, Hubx, Huby, TOU) {
  t <- spreadData(data, DelivMonth, Hubx, Huby)
  if(TOU == "Peak") {
    plot <- ggplot(t, aes(x = Price_Date, y = peakSpread)) + 
      geom_line(color = "blue", size = 1.25) +
      scale_x_date(date_breaks = "1 month") +
      scale_y_continuous(breaks = scales::pretty_breaks(n = 15)) +
      theme(axis.text.x = element_text(size = 9, angle = 90, hjust = 0.95, vjust = 0.2),
            plot.title = element_text(hjust = 0.5),
            legend.justification = c(1,0),
            legend.position = "bottom",
            legend.text = element_text(size = 5),
            plot.subtitle = element_text(hjust = 0.5, size = 6.5)) +
      labs(colour = "") +
      ggtitle(label = paste0(Hubx," vs. ",Huby," Peak Spread"),
              subtitle = paste0("Delivery Month: ", DelivMonth)) +
      ylab("Spread ($/MWh)") + xlab("Delivery Month") +
      geom_smooth(method = "loess", formula = y ~ x)
  } else {
    plot <- ggplot(t, aes(x = Price_Date, y = offpeakSpread)) + geom_line(color = "red", size = 1.25) +
      scale_x_date(date_breaks = "1 month") +
      scale_y_continuous(breaks = scales::pretty_breaks(n = 15)) +
      theme(axis.text.x = element_text(size = 9, angle = 90, hjust = 0.95, vjust = 0.2),
            plot.title = element_text(hjust = 0.5),
            legend.justification = c(1,0),
            legend.position = "bottom",
            legend.text = element_text(size = 5),
            plot.subtitle = element_text(hjust = 0.5, size = 6.5)) +
      labs(colour = "") +
      ggtitle(label = paste0(Hubx," vs. ",Huby," Peak Spread"),
              subtitle = paste0("Delivery Month: ", DelivMonth)) +
      ylab("Spread ($/MWh)") + xlab("Delivery Month") +
      geom_smooth(method = "loess", formula = y ~ x)
  }
  return(plot)
}



# On-Demand Plot Functions

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

ondemandPlot <- function(data = dt, hub, current_date = cdate, prior_date = pdate) {
  cur <- prcDt(data = data, hub = hub, date = current_date)
  prior <- prcDt(data = data, hub = hub, date = prior_date)
  prc_dt <- graphData(cur, prior)
  prc_plot <- changePlot(prc_dt)
  
  return(prc_plot)
  
}

ondemandhrchg_plot <- function(powerdata, gasdata,powerhub,gashub,date1=current_date, date2= prior_date) {
  t1 <- hrdt(powerdata,gasdata, powerhub,gashub)
  t2 <- hrgraphdata(t1,powerhub, current_date,prior_date)
  plot <- hrchangePlot(t2)
  return(plot)
}

ondemandhrplot <- function(powerdata = dt,gasdata = allin, powerhub,gashub,time = vmn) {
  tmp <- hrdt(powerdata,gasdata,powerhub,gashub) %>%
    filter(Tenor == time)
  plot <- hr_plot(tmp,powerhub,gashub, time)
  
  return(plot)
}

