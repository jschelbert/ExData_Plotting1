library(ggplot2)
library(dplyr)
library(lubridate)


plot3 <- function(){
  # load the data
  power_cons <- read.csv("household_power_consumption.txt", sep=";", 
                         colClasses = c("character","character","numeric","numeric","numeric","numeric","numeric","numeric","numeric"), 
                         na.string="?")
  
  timeperiod <- interval(ymd_hms("2007-02-01 00:00:00"), ymd_hms("2007-02-02 24:00:00"))
  
  power_cons_interval <- power_cons %>% mutate(Datetime=dmy_hms(paste(Date, Time))) %>%filter(Datetime %within% timeperiod)
  data_submeter <- melt(power_cons_interval[,7:10], "Datetime")
  
  # use base plot
  png("plot3.png", width = 480, height = 480)
  with(power_cons_interval, plot(Datetime, Sub_metering_1, xlab="", ylab="Energy sub metering", type="l"))
  with(power_cons_interval, lines(Datetime, Sub_metering_2, col="red"))
  with(power_cons_interval, lines(Datetime, Sub_metering_3, col="blue"))
  legend("topright", lty = c(1,1,1), col=c("black","red","blue"), legend=c("Sub_metering_1","Sub_metering_2","Sub_metering_3"))
  dev.off()
  
  # use ggplot2 
  g <- ggplot(data=data_submeter, aes(Datetime, value, color=variable))
  gfinal <- g + geom_line() + labs(x=NULL, y ="Energy sub metering", color=NULL) + theme(legend.position=c(0.9, 0.9))
  gfinal
  
  ggsave("plot3_gg.png", width=4.80, height=4.80, dpi=100)
  return(gfinal)
}
