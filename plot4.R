
getData <- function(){
  
  obj <- list()
  
  sol$data <- read.table("household_power_consumption.txt", sep=";", header = TRUE)
  sol$subset <- data[data$Date == "1/2/2007" | data$Date == "2/2/2007",]
  
  obj$subset$Date <- as.Date(obj$subset$Date, format="%d/%m/%Y") 
  str_Date_time <- paste(obj$subset$Date, obj$subset$Time, sep="")
  obj$subset$Date_time <- strptime(str_Date_time, format="%Y-%m-%d %H:%M:%S")
  
  return(obj$subset)
}

makePlot <- function(){
  dev.copy(png, file="plot4.png", width=480, height=480)
  dev.off() 
}

plotA <- function(){
  
  data <- getData()
  
  gap = data$Global_active_power
  dt = data$Date_time
  strXlab = ""
  strYlab = "Global Active Power"
  
  plot(dt, gap, type="l", xlab=strXlab, ylab=strYlab)
}

plotB <- function(){
  
  data <- getData()
  strXlab = "datetime"
  strYlab = "Voltage"
  plot(data$Date_time, 
       data$Voltage, 
       type="l", 
       xlab=strXlab, 
       ylab=strYlab)
}

plotC <- function(){
  
  data <- getData()
  
  sm1 = data$Sub_metering_1
  sm2 = data$Sub_metering_2
  sm3 = data$Sub_metering_3
  dt = data$Date_time
  strXlab = ""
  strYlab = "Energy sub metering"
  
  plot(dt, sm1, type="l", xlab=strXlab, ylab=strYlab)
  lines(dt, sm2, col="red")
  lines(dt, sm3, col="blue")
  
  legend(x="topright",          
         legend = c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"),        
         lwd = 1,
         col = c("black", "red", "blue"),
         cex = 1/2) 
}

plotD <- function() {
  data <- getData()
  strXlab = "datetime"
  strYlab = "Global_reactive_power"
  plot(data$Date_time, 
       data$Global_reactive_power, 
       type="l", 
       xlab=strXlab, 
       ylab=strYlab) 
}

plot4 <- function(){
  
  data <- getData()
  
  par(mfrow = c(2, 2))
  
  with(data, {
    plotA()
    plotB()
    plotC()
    plotD()
  })
  
  makePlot()
}



