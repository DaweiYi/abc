###Load Packages
library(data.table)
library(ggplot2)
library(lubridate)
library(dplyr)

##Load Data
household_power_consumption <- fread("~/Downloads/household_power_consumption.txt",sep = ";")

##Format Data
for (i in 3:length(colnames(household_power_consumption))) {
  household_power_consumption[,i] <- as.numeric(household_power_consumption[,i])
}

Feb_2007.df<-household_power_consumption %>% 
  mutate(Date = dmy(Date)) %>% 
  filter(Date>="2007-2-1",
         Date<="2007-2-2") %>% 
  mutate(Date_time = ymd_hms(paste(Date,Time)))



rm(household_power_consumption)
###
with(Feb_2007.df, {
  plot(Sub_metering_1~Date_time, type="l",
       ylab="Global Active Power (kilowatts)", xlab="")
  lines(Sub_metering_2~Date_time,col='Red')
  lines(Sub_metering_3~Date_time,col='Blue')
})
legend("topright", col=c("black", "red", "blue"), lwd=c(1,1,1), 
       c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"))

##Save plot
dev.copy(png, file="plot3.png", height=480, width=480)
dev.off()