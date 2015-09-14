#PLOT 1
nei <- readRDS("summarySCC_PM25.rds")

pm25sum <- tapply(nei$Emissions, nei$year, sum)

plot(names(pm25sum), pm25sum, type="l", 
     main = expression("Total PM2.5 Emissions in the US by Year"),
     xlab = "Year", ylab = expression("Total PM2.5 Emissions (Tons)") 
)

dev.copy(png, file="plot1.png", height=480, width=600)
dev.off()



#PLOT 2
nei <- readRDS("summarySCC_PM25.rds")

blt <- subset(nei, fips == "24510")

pm25sum <- tapply(blt$Emissions, blt$year, sum)

plot(names(pm25sum), pm25sum, type="l", 
     main = expression("Total PM2.5 Emissions in Baltimore by Year"),
     xlab = "Year", ylab = expression("Total PM2.5 Emissions (Tons)") 
)

dev.copy(png, file="plot2.png", height=480, width=600)
dev.off()



#PLOT 3
nei <- readRDS("summarySCC_PM25.rds")

library(ggplot2)

blt <- subset(nei, fips == "24510")

pm25sum <- aggregate(Emissions ~ year + type, blt, sum)
pm25sum$type[pm25sum$type=="POINT"] <- "Point Emissions"
pm25sum$type[pm25sum$type=="NONPOINT"] <- "Non-Point Emissions"
pm25sum$type[pm25sum$type=="ON-ROAD"] <- "On-Road Emissions"
pm25sum$type[pm25sum$type=="NON-ROAD"] <- "Non-Road Emissions"
colnames(pm25sum)[colnames(pm25sum)=="type"] <- "EmissionType"

qplot(year, Emissions, data=pm25sum, color=EmissionType, geom="line",
      main = expression("Total PM2.5 Emissions in Baltimore by Type and Year"),
      xlab = "Year", ylab = expression("Total PM2.5 Emissions (Tons)")
)

dev.copy(png, file="plot3.png", height=480, width=600)
dev.off()



#PLOT 4
nei <- readRDS("summarySCC_PM25.rds")
scc <- readRDS("Source_Classification_Code.rds")
nei_scc <- merge(nei, scc)

library(ggplot2)

coal <- grepl("Coal", nei_scc$Short.Name)
coal2 <- nei_scc[coal,]

pm25sum <- aggregate(Emissions ~ year, coal2, sum)

qplot(year, Emissions, data=pm25sum, geom="line", ylim=c(0,600000),
      main = expression("Total PM2.5 Emissions in the US from Coal Sources by Year"),
      xlab = "Year", ylab = expression("Total PM2.5 Emissions (Tons)")
)

dev.copy(png, file="plot4.png", height=480, width=600)
dev.off()



#PLOT 5
nei <- readRDS("summarySCC_PM25.rds")
scc <- readRDS("Source_Classification_Code.rds")
nei_scc <- merge(nei, scc)

library(ggplot2)

blt <- subset(nei_scc, fips == "24510")
veh <- grepl("Vehicles", blt$EI.Sector)
veh2 <- blt[veh,]

pm25sum <- aggregate(Emissions ~ year, veh2, sum)

qplot(year, Emissions, data=pm25sum, geom="line",
      main = expression("Total PM2.5 Emissions in Baltimore from On-Road Motor Vehicle Sources by Year"),
      xlab = "Year", ylab = expression("Total PM2.5 Emissions (Tons)")
)

dev.copy(png, file="plot5.png", height=480, width=600)
dev.off()



#PLOT 6
nei <- readRDS("summarySCC_PM25.rds")
scc <- readRDS("Source_Classification_Code.rds")
nei_scc <- merge(nei, scc)

library(ggplot2)

blt <- subset(nei_scc, fips == "24510")
la <- subset(nei_scc, fips=="06037")

veh_blt <- grepl("Vehicles", blt$EI.Sector)
veh2_blt <- blt[veh_blt,]
veh_la <- grepl("Vehicles", la$EI.Sector)
veh2_la <- la[veh_la,]
blt_la <- rbind(veh2_la, veh2_blt)

pm25sum <- aggregate(Emissions ~ year + fips, blt_la, sum)
colnames(pm25sum)[colnames(pm25sum)=="fips"] <- "County"
pm25sum$County[pm25sum$County=="24510"] <- "Baltimore"
pm25sum$County[pm25sum$County=="06037"] <- "Los Angeles"

qplot(year, Emissions, data=pm25sum, geom="line", color=County,
      main = expression("Total PM2.5 Emissions in Los Angeles and Baltimore from On-Road Motor Vehicle Sources by Year"),
      xlab = "Year", ylab = expression("Total PM2.5 Emissions (Tons)")
)

dev.copy(png, file="plot6.png", height=480, width=600)
dev.off()