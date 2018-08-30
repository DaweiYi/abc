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

plot(
  Feb_2007.df$Global_active_power ~ Feb_2007.df$Date_time,
  type = "l",
  ylab = "Global Active Power (kilowatts)",
  xlab = ""
)