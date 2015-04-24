#load data files
NEI <- readRDS("exploredataproject2/summarySCC_PM25.rds")
SCC <- readRDS("exploredataproject2/Source_Classification_Code.rds")

#merge sector names from SCC with NEI dataframe
SCC1 <- subset(SCC, select = c("SCC","EI.Sector"))
mergedData <- merge(NEI, SCC1)

#subset motor vehicle emissions
mergedData$motor.vehicles <- mergedData$EI.Sector %in% c("Mobile - Aircraft",
                                                         "Mobile - Commercial Marine Vessels","Mobile - Locomotives",
                                                         "Mobile - Non-Road Equipment - Diesel",
                                                         "Mobile - Non-Road Equipment - Gasoline",
                                                         "Mobile - Non-Road Equipment - Other",
                                                         "Mobile - On-Road Diesel Heavy Duty Vehicles",
                                                         "Mobile - On-Road Diesel Light Duty Vehicles",
                                                         "Mobile - On-Road Gasoline Heavy Duty Vehicles",
                                                         "Mobile - On-Road Gasoline Light Duty Vehicles")
motorVehicleData <- mergedData[mergedData$motor.vehicles == "TRUE", ]

#subset just baltimore
bmoreMotorVehicleData <- motorVehicleData[motorVehicleData$fips == "24510",]

##find emissions sums per year and plot
bmoreMotorVehicleByYear <- aggregate(Emissions~year, bmoreMotorVehicleData, sum)
with(bmoreMotorVehicleByYear, plot(year, col = "red", Emissions, type = 'b', pch = 19, 
                                   main = "Total Motor Vehicle Emissions in Baltimore City, 1999-2008", 
                                   xlab = "Year", ylab = "Emissions (PM2.5)"))

#create png file of figure
dev.copy(png, file = 'plot5.png')
dev.off()
