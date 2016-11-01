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

  # use base plot 
  png("plot4.png", width = 480, height = 480)
  par(mfrow=c(2,2))
  
  with(power_cons_interval,plot(Datetime, Global_active_power, xlab="", ylab="Global Active Power (kilowatts)", type="l"))
  
  with(power_cons_interval,plot(Datetime, Voltage, xlab="datetime", ylab="Voltage",type="l"))
  
  with(power_cons_interval, plot(Datetime, Sub_metering_1, xlab="", ylab="Energy sub metering", type="l"))
  with(power_cons_interval, lines(Datetime, Sub_metering_2, col="red"))
  with(power_cons_interval, lines(Datetime, Sub_metering_3, col="blue"))
  legend("topright", lty = c(1,1,1), col=c("black","red","blue"), legend=c("Sub_metering_1","Sub_metering_2","Sub_metering_3"), bty="n")
  
  with(power_cons_interval,plot(Datetime, Global_reactive_power, xlab="datetime", ylab="Global_reactive_power",type="l"))
  
  dev.off()
  
  # use ggplot2 
  g1 <- ggplot(aes(Datetime, Global_active_power), data=power_cons_interval)
  p1 <- g1 + geom_line() + labs( x=NULL, y ="Global Active Power (kilowatts)")
  
  g2 <- ggplot(data=power_cons_interval, aes(Datetime, Voltage))
  p2 <- g2 + geom_line() + labs( x=NULL, y ="Voltage")
  
  g3 <- ggplot(data=data_submeter, aes(Datetime, value, color=variable))
  p3 <- g3 + geom_line() + labs(x=NULL, y ="Energy sub metering", color=NULL) + theme(legend.position=c(0.75, 0.85))

  g4 <- ggplot(data=power_cons_interval, aes(Datetime, Global_reactive_power))
  p4 <- g4 + geom_line() + labs( x=NULL, y ="Global reactive power")
  
  png("plot4_gg.png", width=480, height=480, units="px")
  grid.arrange(p1, p2, p3, p4, ncol = 2)
  dev.off()
}
