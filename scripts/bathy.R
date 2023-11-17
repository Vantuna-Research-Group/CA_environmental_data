##############################################################################
#California Bathymetry Data
#Zoë J. Kitchel
#Created 30 October 2023
#Modified 30 October 2023
##############################################################################
#There are multiple options for where to get bathymetry data:
  #1) Marmap package (but our sites are too close to shore for this to work, example below anyway though)
  #1) We have some (limited) site specific bathymetry. Talk to Chelsea.
  #2) Full state of California out to 200 eez lower rez: https://wildlife.ca.gov/Conservation/Marine/GIS/Downloads (Arc/Info Binary Grid (ADF) )
  #3) Full state of California higher rez (but data is currently offline) http://seafloor.otterlabs.org/SFMLwebDATA_SURVEYMAP.htm
      #Or, another fine scale option: https://pubs.usgs.gov/ds/487/ds487_text.pdf (3m res)

##############################################################################
#SETUP#
library(sf)
library(ggplot2)
library(dplyr)
library(raster)
library(rnaturalearth)
library(viridis)
library(marmap) #to pull depth data

source("Functions_pulling_vrg_data.R")
############################################################################
#Load up all VRG lat longs
############################################################################
events <- pull_VRG_data_files(system = "macos", event = T)
VRG_lat_lon_all <- unique(events[,.(Latitude, Longitude,SurveyDepth)])

VRG_lat_lon_all[,Longitude := ifelse(!(Longitude >= -150 & Longitude <= -50),NA,Longitude)]
VRG_lat_lon_all <- VRG_lat_lon_all[complete.cases(VRG_lat_lon_all[, .(Longitude, Latitude)])]


#minimum and maximum lat and long
min_lat <- min(VRG_lat_lon_all$Latitude, na.rm = T)
min_long <- min(VRG_lat_lon_all$Longitude, na.rm = T)
max_lat <- max(VRG_lat_lon_all$Latitude, na.rm = T)
max_long <- max(VRG_lat_lon_all$Longitude, na.rm = T)

############################################################################
#Make a map
############################################################################

#map of world at high-ish resolution
world <- rnaturalearth::ne_countries(scale = "large", returnclass = "sf")

ggplot() + 
  geom_sf(data = world, fill = "palegreen", color ="darkgrey") + 
  geom_point(data = VRG_lat_lon_all, aes(x = Longitude, y = Latitude), size = 0.5) +
  coord_sf(xlim = c(-121,-115), ylim = c(30, 35)) +
  theme_classic()

############################################################################
#1) Use Etopo2 from marmap package
############################################################################

#set square from which to extract bathy data from NOAA server
bathy_VRG <- getNOAA.bathy(min_long-2, max_long+2, min_lat, max_lat+0.5, resolution = 0.000001) #bathymetry matrix
VRG_lat_lon_all[,ETOPODepth := get.depth(bathy_VRG, VRG_lat_lon_all[, .(Longitude, Latitude)], locator=F)$depth]
VRG_lat_lon_all[,diff := abs(ETOPODepth-SurveyDepth)]

#Not as bad as you might expect, but not perfect. Also, issues arise because some coordinates are on land

#map of bathymetry
autoplot.bathy(bathy_VRG, geom=c("tile"
                                # ,"contour" exclude contour
                                 )) +
  scale_fill_etopo(breaks = c(-12000,-6000, 0, 6000, 10000), labels = c(12, 6, 0, 6, 10)) + #from marmap, great way to visualize land and water instead of 'world' object
  geom_point(data = VRG_lat_lon_all, aes(x = Longitude, y = Latitude), size = 0.2, color = "red") +
  coord_sf(xlim = c(-121,-115), ylim = c(30, 35)) +
  labs(y = "Latitude", x = "Longitude", fill = "Elevation/Depth\n(1000s of m)") +
  scale_x_continuous(breaks = c(-121:-115), labels = c("121˚W" ,"120˚W" ,"119˚W" ,"118˚W" ,"117˚W" ,"116˚W" ,"115˚W")) +
  scale_y_continuous(breaks = c(30:35),labels = c("30˚N" ,"31˚N" ,"32˚N" ,"33˚N" ,"34˚N" ,"35˚N")) +
  theme_classic() +
  theme(axis.title = element_blank(),
        legend.position = c(0.2, 0.35),
        legend.justification = c("right", "top"),
        legend.box.just = "right")

#flag some Lat/Lon if surprising locations or depths


###########################################################################
#Load up all VRG lat longs
############################################################################

############################################################################
#Load in shapefile
ca_bathy <- raster(file.path("raw_data","200mEEZ_BathyGrids","bd200m_v2i","w001001.adf"))
ca_bathy_df <- as.data.frame(ca_bathy, xy = TRUE)

DEM_locations <- st_read("/Users/kitchel/Downloads/DEMCoverageAreas/DEMCoverageAreas.shp")

#5m contours

#or try raster
la1_raster <- raster("/Users/kitchel/Downloads/la1.txt")
############################################################################


st_crs(ca_bathy)

bathy_scb_plot <- ggplot(data = ca_bathy_df, aes(x = x, y = y, fill = w001001_COUNT)) +
  geom_raster() +
  scale_fill_viridis() +
  lims(y = c(-707500,-300000), x = c(0, 200000))

ggsave(bathy_scb_plot, path = file.path("figures"), filename = "bathy_scb_plot.pdf")




#Match site lat lon to depth

https://data.usgs.gov/datacatalog/data/USGS:1313d046-7e62-495e-8cb0-88b6018dc644

############################################################################
#Load in all VRG Lat Lons
############################################################################
#Link to VRG Files dropbox

  VRG_files <- file.path(path.expand("~"),"Dropbox","VRG Files")
  VRG_files <- sub("/Documents", "", VRG_files)
  
#exclude anything in old versions of files folder
  VRG_old_files <- file.path(VRG_files, file.path("VRG Data Files","Old Versions of Files"))
  
  all_integrated_event_files <- list.files(path = VRG_files, recursive = T, pattern = "Integrated Event Data.*\\.csv")
  
  newest_integrated_event_file <- all_integrated_event_files[!grepl("Old Versions",all_integrated_event_files)]
