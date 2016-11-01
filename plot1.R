library(ggplot2)
library(dplyr)
library(lubridate)


plot1 <- function(){
  # load the data
  power_cons <- read.csv("household_power_consumption.txt", sep=";", 
                         colClasses = c("character","character","numeric","numeric","numeric","numeric","numeric","numeric","numeric"), 
                         na.string="?")
  
  timeperiod <- interval(ymd_hms("2007-02-01 00:00:00"), ymd_hms("2007-02-02 24:00:00"))
  
  power_cons_interval <- power_cons %>% mutate(Datetime=dmy_hms(paste(Date, Time))) %>% filter(Datetime %within% timeperiod)

  # use base plot
  png("plot1.png", width = 480, height = 480)
  hist(power_cons_interval$Global_active_power, col="red", main="Global Active Power", xlab="Global Active Power (kilowatts)")
  dev.off()
  
  # use ggplot2
  g <- ggplot(aes(Global_active_power), data=power_cons_interval)
  gfinal <- g + geom_histogram(bins=18, fill="red", color="black") + 
    labs(title="Global Active Power", x="Global Active Power (kilowatts)", y ="Frequency") + 
    scale_y_continuous(breaks=seq(0,1500,200))
  gfinal
  ggsave("plot1_gg.png", width=4.80, height=4.80, dpi=100)
  
  return(gfinal)
}
