#load data files
NEI <- readRDS("exploredataproject2/summarySCC_PM25.rds")
SCC <- readRDS("exploredataproject2/Source_Classification_Code.rds")
#load packages
load(ggplot2)

#subset for baltimore
bmoreNEI <- NEI[NEI$fips == "24510", ]

#find emissions totals per year and plot by type of emissions
bmoreTotalByYearType <- aggregate(Emissions~year + type,bmoreNEI,sum)
g <- ggplot(bmoreTotalByYearType, aes(year, Emissions ))
g + geom_point(alpha = 1/3, col = "red", size = 3) + geom_line(col = 'red') + facet_grid(.~type) + 
  labs(title = "Emission Totals by Type for 1999-2008 (Baltimore City)") + labs(x = "Year") + 
  labs(y = expression("Emissions Total " * PM[2.5])) + theme_grey(base_family = "Avenir")

#create png file of figure
dev.copy(png, file = 'plot3.png')
dev.off()
