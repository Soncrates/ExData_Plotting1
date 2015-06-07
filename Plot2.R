#pretty
library(lubridate)

read_data <- function(file="household_power_consumption.txt") {
  col_types <-c(rep('character',2),rep('numeric',7))
  ret <- read.table(file, sep = ";", header = TRUE,
                    na.strings='?',colClasses=col_types)  
}
# date_range format DD/MM/YYY
cache_power_data <- function(date_range = c('1/2/2007','2/2/2007')){
  ret <- read_data()
  # truncate to date range of interest
  ret <-ret[ret$Date %in% date_range,]
}

load_power_data <- function(date_range = c('1/2/2007','2/2/2007')){
  temp <- "modified_household_power_consumption.txt"
  if (!file.exists(temp)) {
    data <- cache_power_data(date_range)
    write.table(data,temp,row.name=FALSE,sep = ";")
  }
  ret <- read_data(temp)
  # pretty
  ret$Date <- dmy(ret$Date)
  ret$Time <- hms(ret$Time)
  ret$DateTime <- ret$Date + ret$Time
  ret
}
main <- function() {
  data <- load_power_data()
  png(filename='plot2.png',width=480,height=480,units='px')
  plot(data$DateTime, data$Global_active_power,
       ylab='Global Active Power (kilowatts)', xlab='', type='l')
  dev.off()
}
main()