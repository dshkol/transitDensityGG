# Lollipop plot
# Working with dev versions of ggplot and ggalt
devtools::install_github("hadley/ggplot2")
devtools::install_github("hrbrmstr/ggalt")

#ggplot() + geom_bar(data = labels, aes(Station,r400, fill = CITY ), stat = "identity")

stations <- read.csv("stations_densities_lines_distances.csv")

stations$line <- factor(stations$line, levels = c("YVR Airport","Canada","Millenium","Expo"))

gg <- ggplot(stations, aes(reorder(line, station),cdistance))
gg <- gg + geom_lollipop()
gg <- gg + coord_flip()

# distances from https://upload.wikimedia.org/wikipedia/commons/d/d4/Vancouver_SkyTrain_track_diagram.svg

# generic minimal white theme
# whitetheme <- theme(panel.background = element_blank(), panel.grid.minor = element_blank(), axis.line = element_blank(), axis.text.x = element_blank(), axis.text.y = element_blank(), axis.ticks = element_blank(), axis.title.x = element_blank(), axis.title.y = element_blank()) 

# density plot
d <- ggplot() + geom_line(data = stations, aes(factor(line),cdistance))
d <- d + geom_point(data = stations, aes(factor(line),cdistance, size = d400, fill = city), pch = 21, colour= "black")
d <- d + coord_flip()


# price plots

p1 <- ggplot() + geom_line(data = stations, aes(factor(line),cdistance))
p1 <- p1 + geom_point(data = stations, aes(factor(line),cdistance, size = ashelter, fill = ashelter), colour = "black", pch = 21)
p1 <- p1 + coord_flip()
p1 <- p1 + whitetheme
p1 <- p1 + scale_fill_gradient(low = "#ffeda0", high = "#f03b20")

# lollipop

caption <- "Monthly shelter costs represent combined monthly rent and utility costs. Data from Statistics Canada\nAverage shelter costs for each station calculated by Jens von Bergmann at censusmapper.ca \nVisualization by Dmitry Shkolnik"
subtitle <- "Average shelter costs calculated based on 800m radii from Skytrain station locations"

#caption <- paste0(strwrap(caption, 100), sep="", collapse="n")


gg <- ggplot(stations[stations$line != "YVR Airport",], aes(reorder(station, ashelter),ashelter))
gg <- gg + geom_lollipop(point.size = 3, point.colour = "white")
# Individual colour markers for each line
gg <- gg + geom_lollipop(data = stations[stations$line == "Canada",],point.size = 3, point.colour = "#009AC8")
gg <- gg + geom_lollipop(data = stations[stations$line == "Millenium",],point.size = 3, point.colour = "#ffD520")
gg <- gg + geom_lollipop(data = stations[stations$line == "Expo",],point.size = 3, point.colour = "#0060A9")
gg <- gg + coord_flip()
# Style the theme
gg <- gg + theme_minimal(base_family = "sans")
gg <- gg + theme(panel.grid.major.y=element_blank())
gg <- gg + theme(axis.title.y = element_blank())
gg <- gg + theme(axis.text.y=element_text(margin=margin(r=-30, l=0)))
# Title, subtitle, caption
gg <- gg + labs(x = NULL, y = NULL, title = "Monthly Shelter Costs by Skytrain Station", subtitle = subtitle, caption = caption)
gg <- gg + theme(plot.title=element_text(face="bold", hjust = 0.13))
gg <- gg + theme(plot.subtitle=element_text(hjust = 0.50, margin=margin(b=12, t=3)))
gg <- gg + theme(plot.caption=element_text(size=8, margin=margin(t=10, b = 5)))
gg


# line plot
#Expo #0060A9
#M #ffD520
#Canada #009AC8

