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

## Plot4
par(
  mfrow = c(2, 2),
  mar = c(4, 4, 2, 1),
  oma = c(0, 0, 2, 0)
)

with(Feb_2007.df, {
  plot(
    Global_active_power ~ Date_time,
    type = "l",
    ylab = "Global Active Power (kilowatts)",
    xlab = ""
  )
  plot(Voltage ~ Date_time,
       type = "l",
       ylab = "Voltage (volt)",
       xlab = "")
  plot(
    Sub_metering_1 ~ Date_time,
    type = "l",
    ylab = "Global Active Power (kilowatts)",
    xlab = ""
  )
  lines(Sub_metering_2 ~ Date_time, col = 'Red')
  lines(Sub_metering_3 ~ Date_time, col = 'Blue')
  legend(
    "topright",
    col = c("black", "red", "blue"),
    lty = 1,
    lwd = 2,
    bty = "n",
    legend = c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3")
  )
  plot(
    Global_reactive_power ~ Date_time,
    type = "l",
    ylab = "Global Rective Power (kilowatts)",
    xlab = ""
  )
})

##Save
dev.copy(png, file="plot4.png", height=480, width=480)
dev.off()