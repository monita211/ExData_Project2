#load data files
NEI <- readRDS("exploredataproject2/summarySCC_PM25.rds")
SCC <- readRDS("exploredataproject2/Source_Classification_Code.rds")

#subset data for baltimore city
bmoreNEI <- NEI[NEI$fips == "24510", ]

#find emissions totals per year and plot
bmoreTotalByYear <- aggregate(Emissions~year,bmoreNEI,sum)
with(bmoreTotalByYear, plot(year, col = "red", type = 'b', pch = 19, Emissions, 
                            main = "Total Emissions in Baltimore City, 1999-2008", 
                            xlab = "Year", ylab = "Emissions (PM2.5)"))

#create png file of figure
dev.copy(png, file = 'plot2.png')
dev.off()


