#load data files
NEI <- readRDS("exploredataproject2/summarySCC_PM25.rds")
SCC <- readRDS("exploredataproject2/Source_Classification_Code.rds")

#find emissions totals per year and plot
totalByYear <- aggregate(Emissions~year,NEI,sum)
with(totalByYear, plot(year, Emissions, type = 'b', pch = 19, col = "red", 
                       main = "Total Emissions in the United States, 1999-2008",
                       xlab = "Year", ylab = "Emissions (PM2.5)"))

#create png file of figure
dev.copy(png, file = 'plot1.png')
dev.off()
