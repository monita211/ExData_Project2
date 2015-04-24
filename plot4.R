#load data files
NEI <- readRDS("exploredataproject2/summarySCC_PM25.rds")
SCC <- readRDS("exploredataproject2/Source_Classification_Code.rds")

#merge sector names from SCC with NEI dataframe
SCC1 <- subset(SCC, select = c("SCC","EI.Sector"))
mergedData <- merge(NEI, SCC1)
#subset coal data
mergedData$coal <- mergedData$EI.Sector %in% c("Fuel Comb- Comm/Institutional - Coal",
                                               "Fuel Comb - Electric Generation - Coal",
                                               "Fuel Comb - Industrial Boilers, ICEs - Coal")
coalData <- mergedData[mergedData$coal == "TRUE", ]

#find coal emissions totals per year and plot
coalByYear <- aggregate(Emissions~year, coalData, sum)
with(coalByYear, plot(year, Emissions, type = 'b', pch = 19, col = "red", 
                      main = "Total Coal Combustion-Related Emissions in the United States, 1999-2008", 
                      xlab = "Year", ylab = "Emissions (PM2.5)"))

#create png file of figure
dev.copy(png, file = 'plot4.png')
dev.off()