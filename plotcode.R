require("rgdal")
require("maptools")
require("plyr")
require("ggplot2")


setwd("SkyTrain")

line <- readOGR(dsn=".", layer = "SkyTline")
line.points <- fortify(line, region="OBJECTID")

eline <- line[!is.na(line@data$EXPO),]
mline <- line[is.na(line@data$EXPO),]
eline.points <- fortify(eline, region="OBJECTID")
mline.points <- fortify(mline, region="OBJECTID")


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

p <- ggplot() + geom_path(data = line.points, aes(x = long, y = lat, group = group), size = 2, colour = "darkgrey") + geom_path(data = cline.points, aes(x = long, y = lat, group = group), size = 2, colour = "darkgrey") + geom_point(data = station.densities, aes(x = long, y = lat, fill = mshelter), size = 12, pch = 21, colour = "black") + geom_point(data = station.densities, aes(x = long, y = lat, fill = r400), size = 7, pch = 21, colour = "white") + theme(panel.background = element_blank(), panel.grid.minor = element_blank(), axis.line = element_blank(), axis.text.x = element_blank(), axis.text.y = element_blank(), axis.ticks = element_blank(), axis.title.x = element_blank(), axis.title.y = element_blank()) 

# geom_text(data = labels.left, aes(long-2500, lat, label = Station), size = 3) + geom_text(data = labels.right, aes(long+1500, lat, label = Station), size = 3) + geom_text(data = labels.top, aes(long, lat+1000, label = Station), size = 3) + geom_text(data = labels.bottom, aes(long, lat-1000, label = Station), size = 3)


p + scale_fill_gradient(low = "#ffeda0", high = "#f03b20")

#####


#Experiment

#mshelter adjusted for inflation 
station.densities$mshelterinf <- station.densities$mshelter * 1.095
station.densities$mshelterinfdisc <- cut(station.densities$mshelterinf, breaks = c(800,900,1000,1100,1200,1300,1400,1500,1600,1700))
levels(station.densities$mshelterinfdisc) <- c("Under 900","900-999","1000-1099","1100-1199","1200-1299","1300-1399","1400-1499","1500-1599","1600+")

# line plot
#Expo #0060A9
#M #ffD520
#Canada #009AC8

blanktheme <- theme(panel.background = element_blank(), panel.grid.minor = element_blank(), axis.line = element_blank(), axis.text.x = element_blank(), axis.text.y = element_blank(), axis.ticks = element_blank(), axis.title.x = element_blank(), axis.title.y = element_blank()) 

captionmm <- "Monthly shelter costs represent combined monthly rent and utility costs\nMedian shelter costs calculated based on average of median shelter costs in census tracts within 800m radii from Skytrain station locations\nData from Statistics Canada\nAverage shelter costs for each station calculated by Jens von Bergmann at censusmapper.ca\nVisualization by Dmitry Shkolnik"

mm <- ggplot() + geom_path(data = eline.points, aes(x = long, y = lat, group = group), size = 2, colour = "#0060A9")  
mm <- mm + geom_path(data = mline.points, aes(x = long, y = lat, group = group), size = 2, colour = "#ffD520") 
mm <- mm + geom_path(data = cline.points, aes(x = long, y = lat, group = group), size = 2, colour = "#009AC8")
mm <- mm + geom_point(data = station.densities[!is.na(station.densities$mshelterinfdisc),], aes(x = long, y = lat, fill = mshelterinfdisc), size = 5, pch = 21, colour = "black")
mm <- mm + geom_point(data = station.densities[is.na(station.densities$mshelterinfdisc),], aes(x = long, y = lat), fill = "darkgrey", size = 5, pch = 21, colour = "black")
mm <- mm + blanktheme
mm <- mm + scale_fill_brewer(palette = "Reds", "Median Shelter Costs") + theme(legend.position = c(0.92,0.8)) + theme(legend.key = element_blank())
mm <- mm + labs(x = NULL, y = NULL, title = "Monthly Shelter Costs by Skytrain Station", caption = captionmm)
mm <- mm + theme(plot.title=element_text(face="bold", hjust = 0.5, size = 18))
mm <- mm + theme(plot.caption=element_text(size=8, margin=margin(t=10, b = 5)))
mm
ggsave("skytrain_geo_graph.pdf",dpi = 300)
ggsave("skytrain_geo_graph.png",dpi = 300)
