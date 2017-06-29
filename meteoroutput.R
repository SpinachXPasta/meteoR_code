library(ggplot2)
library(EDAWR)
library(dplyr)
library(tidyr)
library(gridExtra)
library(rworldmap)
library(ggmap)
library(maps)
library(maptools)
library(lubridate)


meteor <- read.csv("Meteorite_Landings.csv")
meteor <- select(meteor,mass..g.,year,fall,reclong,reclat)
meteor <- subset(meteor, fall != " ")
meteor <- subset(meteor, mass..g. != " ")
meteor <- subset(meteor, reclong != " ")
meteor <- subset(meteor, reclat != " ")
meteor <- subset(meteor, year != " ")
meteor <- mutate(meteor, Year = (as.Date(year,"%d/%m/%Y")))
meteor <- subset(meteor, !is.na(Year))
meteor <- subset(meteor,reclong < 354 )



meteorfell <- subset(meteor, fall == "Fell")
meteorfound <- subset(meteor, fall == "Found")



world_map <- map_data("world")
p <- ggplot() + coord_fixed() +xlab("") + ylab("")

base_world_messy <- p + geom_polygon(data=world_map, aes(x=long, y=lat, group=group), 
                                     colour="light green", fill="light green")

cleanup <- 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_rect(fill = 'white', colour = 'white'), 
        axis.line = element_line(colour = "white"), legend.position="none",
        axis.ticks=element_blank(), axis.text.x=element_blank(),
        axis.text.y=element_blank())

base_world <- base_world_messy + cleanup

mp <- base_world +geom_point(data=meteorfell,aes(x=reclong, y=reclat, colour=year(as.Date(meteorfell$Year,"%d/%m/%Y"))),size = 1, alpha=I(0.2)) + ggtitle("Fallen meteors recorded timeline") + theme(legend.position = "bottom",legend.title = element_blank(),legend.key.width  = unit(0.1,"npc")) + scale_color_gradient(low = "#D2B4DE", high = "#6C3483",space = "Lab", guide = "colourbar") + theme(plot.title = element_text(family = "Trebuchet MS", color="#666666", face="bold", size=16, hjust= 0.5))  
mp2 <- base_world +geom_point(data=meteor,aes(x=reclong, y=reclat, colour=sqrt(mass..g.)), size = 1.1, alpha=I(0.49)) + ggtitle("Recorded meteors and their weight(sq root of actual grams)") + theme(legend.position = "bottom",legend.title = element_blank(),legend.key.width  = unit(0.1,"npc") )+ scale_color_gradient(low = "#EC7063", high = "#78281F",space = "Lab", guide = "colourbar") + theme(plot.title = element_text(family = "Trebuchet MS", color="#666666", face="bold", size=16, hjust= 0.5))
mp3 <- base_world +geom_point(data=meteorfound,aes(x=reclong, y=reclat, colour=year(as.Date(meteorfound$Year,"%d/%m/%Y"))), size = 1, alpha=I(0.5)) + theme(legend.position = "bottom",legend.title = element_blank(),legend.key.width  = unit(0.1,"npc")) + scale_color_gradient(low = "#7D6608", high = "#F1C40F",space = "Lab", guide = "colourbar", na.value = "transparent") + ggtitle("Meteors found and recorded timeline") + theme(plot.title = element_text(family = "Trebuchet MS", color="#666666", face="bold", size=16, hjust= 0.5)) 
mp4 <- base_world +geom_point(data=meteorfound,aes(x=reclong, y=reclat), colour="purple", fill="Pink",pch=21, alpha=I(0.7)) + ggtitle("Meteors found around the world")+ theme(plot.title = element_text(family = "Trebuchet MS", color="#666666", face="bold", size=16, hjust= 0.5))

grid.arrange(mp,mp2,mp3,mp4,ncol = 2)







