#load the libraries

#Important: Sequence of loading is important

if (!require(raster)) install.packages('raster')
library(raster)

if (!require(rgdal)) install.packages('rgdal')
library(rgdal)


library(ggplot2)

if (!require(sp)) install.packages('sp')
library(sp)

if (!require(sf)) install.packages('sf')
library(sf)

if (!require(ggrepel)) install.packages('ggrepel')
library(ggrepel)




# Read Data files


collected_data <- read.csv("forovi.csv", stringsAsFactors=FALSE)


# dsn is the destination folder for the shape files
bangladesh <- readOGR(dsn = "C:\\Users\\fuadh\\Downloads\\bgd_adm_bbs_20201113_shp\\bgd_adm_bbs_20201113_SHP", layer = "bgd_admbnda_adm3_bbs_20201113")

#summary(bangladesh@data)

# separate dhaka and chittagong
chittagong <- subset(bangladesh, ADM2_EN=="Chittagong")
dhaka <- subset(bangladesh, ADM1_EN == "Dhaka")
dh_nj <- subset(bangladesh, ADM2_EN %in% c("Dhaka", "Narayanganj"))

# Base maps
map_ch <- ggplot() + geom_polygon(data = chittagong, aes(x = long, y = lat, group = group), colour = "black", fill = NA)

map_dh <- ggplot() + geom_polygon(data = dhaka, aes(x = long, y = lat, group = group), colour = "black", fill = NA)

# Dhaka Map plot
map_dh + geom_point(data = collected_data[6:14,], aes(x = Lat, y = Long), color="red") + theme_minimal() + 
  xlab("Longitude") + ylab("Latitude") + coord_fixed()

# Chittagong Map PLot
map_ch + geom_point(data = collected_data[1:5,], aes(x = Lat, y = Long), color="red") + theme_minimal() +
  xlab("Longitude") + ylab("Latitude") + coord_fixed()

shp_df <- broom::tidy(chittagong, region = "ADM3_EN")
lapply(shp_df, class)
cnames <- aggregate(cbind(long, lat) ~ id, data=shp_df, FUN=mean)

map_dh + geom_point(data = collected_data[6:14,], aes(x = Lat, y = Long), color="red") + 
  theme_minimal() + xlab("Longitude") + ylab("Latitude") + coord_fixed() +
  geom_text_repel(data = cnames, aes(x = long, y = lat, label = id), size = 3, color="black") +
  coord_sf(xlim = c(90.3, 90.55), ylim = c(23.55, 23.95), expand = FALSE)

map_ch + geom_point(data = collected_data[1:5,], aes(x = Lat, y = Long), color="red") + 
  theme_minimal() + xlab("Longitude") + ylab("Latitude") + coord_fixed() +
  geom_text_repel(data = cnames, aes(x = long, y = lat, label = id), size = 3, color="black") +
  coord_sf(xlim = c(91.7, 92), ylim = c(22.20, 22.40), expand = FALSE) +
  
