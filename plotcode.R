require("rgdal")
require("maptools")
require("ggplot2")
require("plyr")

setwd("SkyTrain")

line <- readOGR(dsn=".", layer = "SkyTline")
line.points <- fortify(line, region="OBJECTID")

#ggplot(line.points) + aes(long, lat, group = group) + geom_path()

station <- readOGR(dsn=".", layer = "SkyTStation")
station.points <- data.frame(station)

setwd("../Canadaline")

cline <- readOGR(dsn=".", layer = "CanLine")
cline.points <- fortify(cline, region="OBJECTID_1")
cstation <- readOGR(dsn=".", layer = "CanLine_Stn")
cstation.points <- data.frame(cstation)

setwd("..")


#ggplot(station.points) + aes(coords.x1, coords.x2) + geom_point()

densities <- read.csv("stations_densities.csv", header = TRUE, stringsAsFactors = TRUE)
station.densities <- merge(station.points,densities, by = "PL_NAME", all.x= TRUE, all.y = TRUE)
station.densities <- merge(cstation.points,station.densities, by.x = "Station", by.y = "PL_NAME", all.x= TRUE, all.y = TRUE)
station.densities$coords.x1.x <- ifelse(is.na(station.densities$coords.x1.x), station.densities$coords.x1.y, station.densities$coords.x1.x)
station.densities$coords.x2.x <- ifelse(is.na(station.densities$coords.x2.x), station.densities$coords.x2.y, station.densities$coords.x2.x)

station.densities$long <- station.densities$coords.x1.x
station.densities$lat <- station.densities$coords.x2.x

station.densities <- station.densities[,c(1,10,15:18)]
station.densities <- station.densities[!is.na(station.densities$r400),]

station.densities$CITY <- as.character(station.densities$CITY)
station.densities$CITY[c(1,2,5,10)] <- "Richmond"
station.densities$CITY[c(3,4,6,7,8,9,13,15)] <- "Vancouver"
station.densities$CITY[c(11,12,16)] <- "Airport"

# offset labels

labels <- station.densities

# labels.left <- labels[labels$Station %in% c("Richmond-Brighouse","Landsdowne","Aberdeen","Marine Drive","Langara-49th Avenue","Oakridge-41st Avenue", "King Edward","Broadway-City Hall","Olympic Village","Yaletown-Roundhouse","Granville","Burrard","Nanaimo","29th Ave","Joyce","Patterson","Metrotown","Royal Oak","Edmonds","22nd St","Sapperton"),]
# 
# labels.right <- labels[labels$Station %in% c("Bridgeport","Stadium - Chinatown","Vancouver City Centre","Main","Lougheed","Gateway","Surrey Central","King George","Braid"),]
# 
# labels.top <- labels[labels$Station %in% c("Waterfront","YVR-Airport","Templeton","Scott Road", "Production Way","Lake City","Holdom","Brentwood","Rupert", "VCC - Clark"),]
# 
# labels.bottom <- labels[labels$Station %in% c("New West","Sea Island Centre","Sperling","Gilmore"),]

##

p <- ggplot() + geom_path(data = line.points, aes(x = long, y = lat, group = group), size = 2, colour = "darkgrey") + geom_path(data = cline.points, aes(x = long, y = lat, group = group), size = 2, colour = "darkgrey") + geom_point(data = station.densities, aes(x = long, y = lat, fill = r800), size = 12, pch = 21, colour = "black") + geom_point(data = station.densities, aes(x = long, y = lat, fill = r400), size = 7, pch = 21, colour = "white") + theme(panel.background = element_blank(), panel.grid.minor = element_blank(), axis.line = element_blank(), axis.text.x = element_blank(), axis.text.y = element_blank(), axis.ticks = element_blank(), axis.title.x = element_blank(), axis.title.y = element_blank()) + geom_text(data = labels.left, aes(long-2500, lat, label = Station), size = 3) + geom_text(data = labels.right, aes(long+1500, lat, label = Station), size = 3) + geom_text(data = labels.top, aes(long, lat+1000, label = Station), size = 3) + geom_text(data = labels.bottom, aes(long, lat-1000, label = Station), size = 3)


p + scale_fill_gradient(low = "#ffeda0", high = "#f03b20")

ggplot() + geom_bar(data = labels, aes(Station,r400, fill = CITY ), stat = "identity")



