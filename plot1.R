###Load Packages
library(data.table)
library(ggplot2)
library(lubridate)
library(dplyr)

##Load Data
household_power_consumption <- fread("Downloads/household_power_consumption.txt",sep = ";")

##Format Data

Feb_2007.df<-household_power_consumption %>% 
  mutate(Date = dmy(Date)) %>% 
  filter(Date>="2007-2-1",
         Date<="2007-2-2")


for (i in 3:length(colnames(Feb_2007.df))) {
  Feb_2007.df[,i] <- as.numeric(Feb_2007.df[,i])
}


rm(household_power_consumption)

##histogram
hist(
  Feb_2007.df$Global_active_power,
  main = "Global Active Power",
  xlab = "Global Active Power (kilowatts)",
  col = "red"
)
#Save plot
dev.copy(png,"plot1.png", width=480, height=480)
dev.off()