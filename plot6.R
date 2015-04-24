#load data files
NEI <- readRDS("exploredataproject2/summarySCC_PM25.rds")
SCC <- readRDS("exploredataproject2/Source_Classification_Code.rds")

#load packages
load(ggplot2)

#merge sector names from SCC with NEI dataframe
SCC1 <- subset(SCC, select = c("SCC","EI.Sector"))
mergedData <- merge(NEI, SCC1)

#subset for just motor vehicle emissions data
mergedData$motor.vehicles <- mergedData$EI.Sector %in% c("Mobile - Aircraft","Mobile - Commercial Marine Vessels",
                                                         "Mobile - Locomotives",
                                                         "Mobile - Non-Road Equipment - Diesel",
                                                         "Mobile - Non-Road Equipment - Gasoline",
                                                         "Mobile - Non-Road Equipment - Other",
                                                         "Mobile - On-Road Diesel Heavy Duty Vehicles",
                                                         "Mobile - On-Road Diesel Light Duty Vehicles",
                                                         "Mobile - On-Road Gasoline Heavy Duty Vehicles",
                                                         "Mobile - On-Road Gasoline Light Duty Vehicles")
motorVehicleData <- mergedData[mergedData$motor.vehicles == "TRUE", ]

#use joined dataframe, motorVehicleData
bmoreLAMotorVehicle <- motorVehicleData[motorVehicleData$fips == c("06037","24510"),]

#relabel levels
city_names <- list("06037" = "LA","24510" = "Baltimore City")

city_labeller <- function(variable,value){
  return(city_names[value])
}

#find emissions sums per year for each zipcode (LA and Baltimore City)
bmoreLAMotorVehicleByYear <- aggregate(Emissions~year + fips, bmoreLAMotorVehicle, sum)
g2 <- ggplot(bmoreLAMotorVehicleByYear, aes(year, Emissions ))
g2 + geom_point(alpha = 1, aes(colour = factor(fips))) + geom_line(alpha = 1, aes(colour = factor(fips))) + 
  labs(title = "Motor Vehicle Emissions for 1999-2008 (LA vs Baltimore City)") + labs(x = "Year") + 
  labs(y = expression("Emissions Total " * PM[2.5])) + theme_grey(base_family = "Avenir") + 
  scale_fill_discrete(name="Cities",breaks=c("06037", "24510"),labels=c("LA", "Baltimore City"))

#create png file of figure
dev.copy(png, file = 'plot6.png')
dev.off()
