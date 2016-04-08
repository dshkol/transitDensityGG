require("rgdal") # requires sp, will use proj.4 if installed
require("maptools")
require("ggplot2")
require("plyr")

line <- readOGR(dsn=".", layer = "SkyTline")
line.points <- fortify(line, region="OBJECTID")

#ggplot(line.points) + aes(long, lat, group = group) + geom_path()

station <- readOGR(dsn=".", layer = "SkyTStation")
station.points <- data.frame(station)

#setwd("../Canadaline")
cline <- readOGR(dsn=".", layer = "CanLine")
cline.points <- fortify(cline, region="OBJECTID_1")
cstation <- readOGR(dsn=".", layer = "CanLine_Stn")
cstation.points <- data.frame(cstation)
#setwd("..")


#ggplot(station.points) + aes(coords.x1, coords.x2) + geom_point()

densities <- read.csv("stations_densities.csv", header = TRUE, stringsAsFactors = TRUE)
station.densities <- merge(station.points,densities, by = "PL_NAME", all.x= TRUE, all.y = TRUE)
station.densities <- merge(cstation.points,station.densities, by.x = "Station", by.y = "PL_NAME", all.x= TRUE, all.y = TRUE)
station.densities$coords.x1.x <- ifelse(is.na(station.densities$coords.x1.x), station.densities$coords.x1.y, station.densities$coords.x1.x)
station.densities$coords.x2.x <- ifelse(is.na(station.densities$coords.x2.x), station.densities$coords.x2.y, station.densities$coords.x2.x)

station.densities$long <- station.densities$coords.x1.x
station.densities$lat <- station.densities$coords.x2.x

station.densities <- station.densities[,c(1,15:18)]

p <- ggplot() + geom_path(data = line.points, aes(x = long, y = lat, group = group), size = 2, colour = "darkgrey") + geom_path(data = cline.points, aes(x = long, y = lat, group = group), size = 2, colour = "darkgrey") + geom_point(data = station.densities, aes(x = long, y = lat, fill = r800), size = 12, pch = 21, colour = "black") + geom_point(data = station.densities, aes(x = long, y = lat, fill = r400), size = 7, pch = 21, colour = "white") + theme(panel.background = element_blank(), panel.grid.minor = element_blank(), axis.line = element_blank(), axis.text.x = element_blank(), axis.text.y = element_blank(), axis.ticks = element_blank(), axis.title.x = element_blank(), axis.title.y = element_blank()) 

p + scale_fill_gradient(low = "red", high = "green")

