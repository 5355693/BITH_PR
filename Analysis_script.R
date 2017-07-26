library(ggplot2)
library(dplyr)
library(rgeos)
library(maps)
library(mapdata)
library(maptools)
library(tidyr)
counts <- read.csv("Data/Survey_data_PuertoRico_counts.csv",  header = T)
summary(counts)
counts.wide <-
  counts %>%
  group_by(Point.ID, Species.code)%>%
  summarise(total = sum(Count),
            Date = first(Date))%>%
  spread(key = Species.code, value = total,fill = 0)

counts.wide <- subset(df, select = -c(Detection.mode,Species,Distance) )
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
cells <- readShapePoly("~/Dropbox/VCE/BITH Puerto Rico/GIS/GRTS_cells_surveyed_2015.shp")
map("worldHires","Puerto Rico", xlim = c(-67.3, -65.6), ylim = c(17.9,18.6),
    mar = c(0.1,0.1,0.1,0.1))
plot(cells, add = TRUE, col = alpha("darkgreen",0.6), border = TRUE)
points(points$X.coordinate, points$Y.coordinate, pch=19, col="red", cex=0.5)

