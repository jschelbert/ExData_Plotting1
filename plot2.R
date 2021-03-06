library(ggplot2)
library(dplyr)
library(lubridate)


plot2 <- function(){
  # load the data
  power_cons <- read.csv("household_power_consumption.txt", sep=";", 
                         colClasses = c("character","character","numeric","numeric","numeric","numeric","numeric","numeric","numeric"), 
                         na.string="?")
  
  timeperiod <- interval(ymd_hms("2007-02-01 00:00:00"), ymd_hms("2007-02-02 24:00:00"))
  
  power_cons_interval <- power_cons %>% mutate(Datetime=dmy_hms(paste(Date, Time))) %>% filter(Datetime %within% timeperiod)
  
  # use base plot
  png("plot2.png", width = 480, height = 480)
  with(power_cons_interval,plot(Datetime, Global_active_power, xlab="", ylab="Global Active Power (kilowatts)", type="l"))
  dev.off()
  
  # use ggplot2  
  g <- ggplot(aes(Datetime, Global_active_power), data=power_cons_interval)
  gfinal <- g + geom_line() + labs( x=NULL, y ="Global Active Power (kilowatts)")
  gfinal
  ggsave("plot2_gg.png", width=4.80, height=4.80, dpi=100)
  return(gfinal)
}
