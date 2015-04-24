#load data files
NEI <- readRDS("exploredataproject2/summarySCC_PM25.rds")
SCC <- readRDS("exploredataproject2/Source_Classification_Code.rds")

#load packages
load(ggplot2)

#Assignment
 
#The overall goal of this assignment is to explore the National Emissions Inventory database and 
#see what it say about fine particulate matter pollution in the United states over the 10-year 
#period 1999–2008. You may use any R package you want to support your analysis.

#Questions: You must address the following questions and tasks in your exploratory analysis. For each question/task 
#you will need to make a single plot. Unless specified, you can use any plotting system in R to make your plot.

#ONE: Have total emissions from PM2.5 decreased in the United States from 1999 to 2008? Using the base plotting system, 
#make a plot showing the total PM2.5 emission from all sources for each of the years 1999, 2002, 2005, and 2008.

totalByYear <- aggregate(Emissions~year,NEI,sum)
with(totalByYear, plot(year, Emissions, type = 'b', pch = 19, col = "red", main = "Total Emissions in the United States, 1999-2008",xlab = "Year", ylab = "Emissions (PM2.5)"))
totalByYear

#TWO: Have total emissions from PM2.5 decreased in the Baltimore City, Maryland (fips == "24510") from 1999 to 2008? 
#Use the base plotting system to make a plot answering this question.
bmoreNEI <- NEI[NEI$fips == "24510", ]
bmoreTotalByYear <- aggregate(Emissions~year,bmoreNEI,sum)
with(bmoreTotalByYear, plot(year, col = "red", type = 'b', pch = 19, Emissions, main = "Total Emissions in Baltimore City, 1999-2008", xlab = "Year", ylab = "Emissions (PM2.5)"))
bmoreTotalByYear

#THREE: Of the four types of sources indicated by the type (point, nonpoint, onroad, nonroad) variable, which of these
#four sources have seen decreases in emissions from 1999–2008 for Baltimore City? Which have seen increases in 
#emissions from 1999–2008? Use the ggplot2 plotting system to make a plot answer this question.

##in baltimore city, emissions totals for four years modified by type
bmoreTotalByYearType <- aggregate(Emissions~year + type,bmoreNEI,sum)
g <- ggplot(bmoreTotalByYearType, aes(year, Emissions ))
g + geom_point(alpha = 1/3, col = "red", size = 3) + geom_line(col = 'red') + facet_grid(.~type) + labs(title = "Emission Totals by Type for 1999-2008 (Baltimore City)") + labs(x = "Year") + labs(y = expression("Emissions Total " * PM[2.5])) + theme_grey(base_family = "Avenir")

#FOUR: Across the United States, how have emissions from coal combustion-related sources changed from 1999–2008?
#join NEI and SCC, form new dataframe
SCC1 <- subset(SCC, select = c("SCC","EI.Sector"))
mergedData <- merge(NEI, SCC1)

##subset for appropriate levels 
levels(mergedData$EI.Sector) #Fuel Comb- Comm/Institutional - Coal(13), Fuel Comb - Electric Generation - Coal(18), and Fuel Comb - Industrial Boilers, ICEs - Coal(23)
mergedData$coal <- mergedData$EI.Sector %in% c("Fuel Comb- Comm/Institutional - Coal","Fuel Comb - Electric Generation - Coal","Fuel Comb - Industrial Boilers, ICEs - Coal")
coalData <- mergedData[mergedData$coal == "TRUE", ]

##aggregate by year and plot
coalByYear <- aggregate(Emissions~year, coalData, sum)
with(coalByYear, plot(year, Emissions, type = 'b', pch = 19, col = "red", main = "Total Coal Combustion-Related Emissions in the United States, 1999-2008", xlab = "Year", ylab = "Emissions (PM2.5)"))

#FIVE: How have emissions from motor vehicle sources changed from 1999–2008 in Baltimore City?
##use merged dataframe formed in previous problem but subset for motor vehicle sources
mergedData$motor.vehicles <- mergedData$EI.Sector %in% c("Mobile - Aircraft","Mobile - Commercial Marine Vessels","Mobile - Locomotives","Mobile - Non-Road Equipment - Diesel",
                                                         "Mobile - Non-Road Equipment - Gasoline","Mobile - Non-Road Equipment - Other",
                                                         "Mobile - On-Road Diesel Heavy Duty Vehicles","Mobile - On-Road Diesel Light Duty Vehicles",
                                                         "Mobile - On-Road Gasoline Heavy Duty Vehicles","Mobile - On-Road Gasoline Light Duty Vehicles")
motorVehicleData <- mergedData[mergedData$motor.vehicles == "TRUE", ]

#subset just baltimore
bmoreMotorVehicleData <- motorVehicleData[motorVehicleData$fips == "24510",]

##aggregate and plot
bmoreMotorVehicleByYear <- aggregate(Emissions~year, bmoreMotorVehicleData, sum)
with(bmoreMotorVehicleByYear, plot(year, col = "red", Emissions, type = 'b', pch = 19, main = "Total Motor Vehicle Emissions in Baltimore City, 1999-2008", xlab = "Year", ylab = "Emissions (PM2.5)"))

#SIX: Compare emissions from motor vehicle sources in Baltimore City with emissions from motor vehicle sources in 
#Los Angeles County, California (fips == "06037"). Which city has seen greater changes over time in motor vehicle emissions?

#use joined dataframe, motorVehicleData
bmoreLAMotorVehicle <- motorVehicleData[motorVehicleData$fips == c("06037","24510"),]

#relabel levels
city_names <- list("06037" = "LA","24510" = "Baltimore City")
  
city_labeller <- function(variable,value){
    return(city_names[value])
  }

bmoreLAMotorVehicleByYear <- aggregate(Emissions~year + fips, bmoreLAMotorVehicle, sum)
g2 <- ggplot(bmoreLAMotorVehicleByYear, aes(year, Emissions ))
g2 + geom_point(alpha = 1, aes(colour = factor(fips))) + geom_line(alpha = 1, aes(colour = factor(fips))) + 
  labs(title = "Motor Vehicle Emissions for 1999-2008 (LA vs Baltimore City)") + labs(x = "Year") + 
  labs(y = expression("Emissions Total " * PM[2.5])) + theme_grey(base_family = "Avenir") + 
  scale_fill_discrete(name="Cities",breaks=c("06037", "24510"),labels=c("LA", "Baltimore City"))


