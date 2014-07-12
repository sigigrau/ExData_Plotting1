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
  dev.copy(png, file="plot1.png", width=480, height=480)
  dev.off() 
}

plot1 <- function(){
  
  data <- getData()
  
  gap = data$Global_active_power
  strMain = "Global Active Power"
  strXlab = "Global Active Power (kilowatts)"
  
  hist(gap, col="red", main=strMain, xlab=strXlab)
  
  makePlot()
}