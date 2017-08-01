library(ggplot2)
library(dplyr)
library(rgeos)
library(maps)
library(mapdata)
library(maptools)
library(tidyr)
library(reshape)
counts <- read.csv("Data/Survey_data_PuertoRico_counts.csv",  header = T)
summary(counts)
str(counts)
tmp <- cast(counts, Species~Species.code)
tmp$count <- rowSums(tmp[,2:71] > 1)
max(counts$Count, na.rm = T)  



counts.wide <-
  counts %>%
  group_by(Point.ID, Species.code)%>%
  summarise(total = sum(Count),
            Date = first(Date))%>%
  spread(key = Species.code, value = total,fill = 0)

write.csv(points.long,"Data/Survey_data_PuertoRico_pointinfo_long.csv")
##
bith.gbif <- read.delim(file = "Data/BITH_GBIFall.csv", header = T)
bith.gbif %>%
  filter(!countrycode %in% c("US","CA","MX","VE","BM","CO")) %>%
ggplot(., aes(x = countrycode)) + 
  geom_bar() + xlab("Country") + ylab("Number of verified\noccurences in GBIF") + 
  theme(axis.title.x = element_text(size = 20),
        axis.title.y = element_text(size = 20),
        axis.text.x = element_text(size = 18),
        axis.text.y = element_text(size = 18))
##Points surveyed
points <- read.csv("Data/Survey_data_PuertoRico_pointinfo.csv", header = T)
##Elevations of each point as determined in GEE using SRTM DEM (30m)
elevations <- read.csv("Data/elevations_estimated.csv", header = T)
colnames(elevations) <- c("Point.ID","elevation")
##Join elevations to points file
points <- left_join(points, elevations, by = "Point.ID")
##Predicted habitat suitability at each point, as determined in GEE
pointSuitability <- read.csv("Data/predicted_suitability.csv", header = T)
##Join points with habitat suitability file
points <- left_join(points, pointSuitability, by = "Point.ID")
##Summarize
summary(points)
##Elevations ranged from 0 - 1297 m, median of 705 m, IQR = 408 - 825 m.
##Habitat suitability ranged from 0.02 - 0.8820, median = 0.6230, IQR = 0.3170 - 0.8025
png(filename = "pointsHabHisto.png", width = 2000, height = 1850,res = 300)
hist(points$habitatValue, main = "",xlab = "Predicted probability of Bicknell's Thrush occurrence",
     ylab = "Number of point-count locations")
dev.off()

png(filename = "pointsElevHisto.png", width = 2000, height = 1850,res = 300)
hist(points$elevation, main = "", xlab = "Elevation (m)", ylab = "Number of point-count locations")
dev.off()

points.long <- gather(points, interval, time, Time.1:Time.4)
points.long$Visit <- substr(points.long$interval, 6,6)
points.long$interval <- NULL
points.long$newdate <- with(points.long, as.POSIXct(paste(Date, time), format="%m/%e/%y %H:%M"))
points.long$Date <- NULL
points.long$time <- NULL
names(points.long)[names(points.long) == 'newdate'] <- 'Date'

cells <- readShapePoly("~/Dropbox/VCE/BITH Puerto Rico/GIS/GRTS_cells_surveyed_2015.shp")
map("worldHires","Puerto Rico", xlim = c(-67.3, -65.6), ylim = c(17.9,18.6),
    mar = c(0.1,0.1,0.1,0.1))
plot(cells, add = TRUE, col = alpha("darkgreen",0.6), border = TRUE)
points(points$X.coordinate, points$Y.coordinate, pch=19, col="red", cex=0.5)

