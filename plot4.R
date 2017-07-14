plot4 <- function(datafile) {
        #read in data
        data <- read.csv(datafile, sep=";")

        #reformat dates
        data$Date <- as.Date(data$Date, format="%d/%m/%Y")

        #subset data for dates of interest
        datasub <- subset(data, Date=="2007-02-01" | Date=="2007-02-02")

        #make data numeric
        datasub$Global_active_power <- as.numeric(as.character(datasub$Global_active_power))
        datasub$Global_reactive_power <- as.numeric(as.character(datasub$Global_reactive_power))
        
        datasub$Sub_metering_1 <- as.numeric(as.character(datasub$Sub_metering_1))
        datasub$Sub_metering_2 <- as.numeric(as.character(datasub$Sub_metering_2))
        datasub$Sub_metering_3 <- as.numeric(as.character(datasub$Sub_metering_3))

        datasub$Voltage <- as.numeric(as.character(datasub$Voltage))
        
        #extract full date-time and convert to POSIXlt class
        time_seq <- as.POSIXlt(strftime(paste(datasub$Date,datasub$Time),format="",tz="")) 
              
        par(mfcol = c (2,2), mar = c(4, 4, 2, 1), oma = c(0, 0, 2, 0))
        
        #plot upper left panel
        plot(time_seq,datasub$Global_active_power, type="l", xlab="", ylab="Global Active Power (kilowatts)")
        
        #plot lower left panel
        plot(time_seq,datasub$Sub_metering_1, type="l", xlab="", ylab="Energy sub metering")
        lines(time_seq,datasub$Sub_metering_2,col="red")
        lines(time_seq,datasub$Sub_metering_3,col="blue")
        
        #add in legend, but remove bounding box
        legend("topright", col=c("black","red","blue"), c("Sub_metering_1  ","Sub_metering_2  ", "Sub_metering_3  "),
               lty=c(1,1), lwd=c(1,1), bty = "n")
  
        #plot upper right panel
        plot(time_seq,datasub$Voltage, type="l", xlab="datetime", ylab="Voltage")
        
        #plot lower right  panel
        plot(time_seq,datasub$Global_reactive_power, type="l", xlab="datetime", ylab="Global_reactive_power")
        
        #output to png deivce for  hardcopy and then close device
        dev.copy(png, file="plot4.png", width=480, height=480)
        dev.off()
        
        #reset default single plot
        par(mfcol = c (1,1))
}