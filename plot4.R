library(ggplot2)
library(dplyr)
library(lubridate)
library(gridExtra)

plot4 <- function(){
  # load the data
  power_cons <- read.csv("household_power_consumption.txt", sep=";", 
                         colClasses = c("character","character","numeric","numeric","numeric","numeric","numeric","numeric","numeric"), 
                         na.string="?")
  
  timeperiod <- interval(ymd_hms("2007-02-01 00:00:00"), ymd_hms("2007-02-02 24:00:00"))
  
  power_cons_interval <- power_cons %>% mutate(Datetime=dmy_hms(paste(Date, Time))) %>%filter(Datetime %within% timeperiod)
  data_submeter <- melt(power_cons_interval[,7:10], "Datetime")
  
  g1 <- ggplot(aes(Datetime, Global_active_power), data=power_cons_interval)
  p1 <- g1 + geom_line() + labs( x=NULL, y ="Global Active Power (kilowatts)")
  
  g2 <- ggplot(data=power_cons_interval, aes(Datetime, Voltage))
  p2 <- g2 + geom_line() + labs( x=NULL, y ="Voltage")
  
  g3 <- ggplot(data=data_submeter, aes(Datetime, value, color=variable))
  p3 <- g3 + geom_line() + labs(x=NULL, y ="Energy sub metering", color=NULL) + theme(legend.position=c(0.75, 0.85))

  g4 <- ggplot(data=power_cons_interval, aes(Datetime, Global_reactive_power))
  p4 <- g4 + geom_line() + labs( x=NULL, y ="Global reactive power")
  
  png("plot4.png", width=480, height=480, units="px")
  grid.arrange(p1, p2, p3, p4, ncol = 2)
  dev.off()
}
