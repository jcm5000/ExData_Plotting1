plot3 <- function(datafile) {
        #read in data
        data <- read.csv(datafile, sep=";")

        #reformat dates
        data$Date <- as.Date(data$Date, format="%d/%m/%Y")

        #subset data for dates of interest
        datasub <- subset(data, Date=="2007-02-01" | Date=="2007-02-02")

        #make data numeric
        datasub$Sub_metering_1 <- as.numeric(as.character(datasub$Sub_metering_1))
        datasub$Sub_metering_2 <- as.numeric(as.character(datasub$Sub_metering_2))
        datasub$Sub_metering_3 <- as.numeric(as.character(datasub$Sub_metering_3))
        
        #extract full date-time and convert to POSIXlt class
        time_seq <- as.POSIXlt(strftime(paste(datasub$Date,datasub$Time),format="",tz="")) 
                
        #plot 
        plot(time_seq,datasub$Sub_metering_1, type="l", xlab="", ylab="Energy sub metering")
        lines(time_seq,datasub$Sub_metering_2,col="red")
        lines(time_seq,datasub$Sub_metering_3,col="blue")
        
        legend("topright", col=c("black","red","blue"), c("Sub_metering_1  ","Sub_metering_2  ", "Sub_metering_3  "),
               lty=c(1,1), lwd=c(1,1))
        
        #output to png deivce for  hardcopy and then close device
        dev.copy(png, file="plot3.png", width=480, height=480)
        dev.off()
}