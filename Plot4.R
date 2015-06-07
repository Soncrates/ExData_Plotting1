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
  ret$datetime <- ret$Date + ret$Time
  ret
}
main <- function() {
  data <- load_power_data()
  DateTime <- 10
  Power <- 3
  ReactPower <- 4
  Voltage <- 5

  png(filename='plot4.png',width=480,height=480,units='px')
  
  # 2 columns, 2 rows of graphs
  par(mfrow=c(2,2))
  
  # column 1,row 1
  p1 <- data[,c(DateTime,Power)]
  plot(p1[[1]], p1[[2]],
       ylab='Global Active Power', xlab='', type='l')
  
  # column 2,row 1
  
  p2 <- data[,c(DateTime,Voltage)]
  plot(p2[[1]],p2[[2]],
       xlab=colnames(p2)[1],ylab=colnames(p2)[2],type='l')
  
  # column 1,row 2

  p3 <-data[,c(7:10)]
  lcols<-c('black','red','blue')
  plot(p3[,4],p3[,1],type='l',col=lcols[1],
       xlab='',ylab='Energy sub metering')
  lines(p3[,4],p3[,2],col=lcols[2])
  lines(p3[,4],p3[,3],col=lcols[3])
  
  legend('topright',
         legend=colnames(p3)[1:3],
         col=lcols,
         lty='solid')  
  
  # column 2,row 2
  
  p4 <- data[,c(DateTime,ReactPower)]
  plot(p4[[1]],p4[[2]],
       xlab=colnames(p4)[1],ylab=colnames(p4)[2],type='l')

  dev.off()
}
main()