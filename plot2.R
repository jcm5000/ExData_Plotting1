plot2 <- function(datafile) {
        #read in data
        data <- read.csv(datafile, sep=";")

        #reformat dates
        data$Date <- as.Date(data$Date, format="%d/%m/%Y")

        #subset data for dates of interest
        datasub <- subset(data, Date=="2007-02-01" | Date=="2007-02-02")

        #make data numeric
        datasub$Global_active_power <- as.numeric(as.character(datasub$Global_active_power))

        #extract full date-time and convert to POSIXlt class
        time_seq <- as.POSIXlt(strftime(paste(datasub$Date,datasub$Time),format="",tz="")) 
                
        #plot 
        plot(time_seq,datasub$Global_active_power, type="l", xlab="", ylab="Global Active Power (kilowatts)")
        
        #output to png deivce for  hardcopy and then close device
        dev.copy(png, file="plot2.png", width=480, height=480)
        dev.off()
}