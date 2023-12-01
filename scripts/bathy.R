##############################################################################
#California Bathymetry Data
#Zoë J. Kitchel
#Created 30 October 2023
#Modified 30 October 2023
##############################################################################
#There are multiple options for where to get bathymetry data:
  #1) Marmap package (but our sites may be too close to shore for this to work, example below anyway though)
  #2) Full state of California out to 200 eez lower rez: https://wildlife.ca.gov/Conservation/Marine/GIS/Downloads (Arc/Info Binary Grid (ADF) )
            #200mEEZ_BathyGrids.zip > bd200m_v2i
  #3) Full state of California higher rez (but data is currently offline) http://seafloor.otterlabs.org/SFMLwebDATA_SURVEYMAP.htm
      #Or, another fine scale option: https://pubs.usgs.gov/ds/487/ds487_text.pdf (3m res)
  #4) We have some (limited) site specific bathymetry. Talk to Chelsea.

##############################################################################
#SETUP#
library(sf)
library(data.table)
library(ggplot2)
library(dplyr)
library(maps)
library(raster)
library(rnaturalearth)
library(viridis)
library(RColorBrewer)
library(marmap) #to pull depth data
library(rasterVis)

source("Functions_pulling_vrg_data.R") #this function pulls most recent clean version of raw data
############################################################################
#Load up all VRG lat longs
############################################################################
events <- pull_VRG_data_files(system = "macos", event = T) #specifically we want raw event data

#flag some strange latitudes and longitudes
events[,flag_lat := ifelse(Latitude < 31, TRUE, FALSE)][,flag_long := ifelse(Latitude > 33.6 & Longitude > -117.5, TRUE, FALSE)]

VRG_lat_lon_all <- unique(events[,.(Latitude, Longitude,SurveyDepth, flag_lat, flag_long)])

VRG_lat_lon_all[,Longitude := ifelse(!(Longitude >= -150 & Longitude <= -50),NA,Longitude)]
VRG_lat_lon_all <- VRG_lat_lon_all[complete.cases(VRG_lat_lon_all[, .(Longitude, Latitude)])]


#minimum and maximum lat and long
min_lat <- min(VRG_lat_lon_all$Latitude, na.rm = T)
min_long <- min(VRG_lat_lon_all$Longitude, na.rm = T)
max_lat <- max(VRG_lat_lon_all$Latitude, na.rm = T)
max_long <- max(VRG_lat_lon_all$Longitude, na.rm = T)

############################################################################
#Make a map with our survey points
############################################################################

#map of world at high-ish resolution
world <- rnaturalearth::ne_countries(scale = "large", returnclass = "sf")

ggplot() + 
  geom_sf(data = world, fill = "palegreen", color ="darkgrey") + 
  geom_point(data = VRG_lat_lon_all, aes(x = Longitude, y = Latitude), size = 0.5) +
  coord_sf(xlim = c(-121,-115), ylim = c(30, 35)) +
  theme_classic()

#delete unexpected points
VRG_lat_lon_all.r <- VRG_lat_lon_all[flag_lat == FALSE & flag_long == FALSE,]

############################################################################
#1) Use Etopo2 from marmap package
############################################################################

#set square from which to extract bathy data from NOAA server
bathy_VRG <- getNOAA.bathy(min_long-2, max_long+2, min_lat, max_lat+0.5, resolution = 0.000001) #bathymetry matrix
VRG_lat_lon_all.r[,ETOPODepth := get.depth(bathy_VRG, VRG_lat_lon_all.r[, .(Longitude, Latitude)], locator=F)$depth]
VRG_lat_lon_all.r[,diff_survey_ETOPO := abs(ETOPODepth-SurveyDepth)]

#Not as bad as you might expect, but not perfect

#map of bathymetry
autoplot.bathy(bathy_VRG, geom=c("tile"
                                # ,"contour" exclude contour
                                 )) +
  scale_fill_etopo(breaks = c(-12000,-6000, 0, 6000, 10000), labels = c(12, 6, 0, 6, 10), #from marmap, great way to visualize land and water instead of 'world' object
                   guide = guide_colorbar(frame.colour = "black", ticks.colour = "black")) + 
  borders("world", colour = "black") +
  geom_point(data = VRG_lat_lon_all.r, aes(x = Longitude, y = Latitude), size = 0.2, color = "red") +
  labs(y = "Latitude", x = "Longitude", fill = "Elevation/Depth\n(1000s of m)") +
  scale_x_continuous(breaks = c(-121:-115), labels = c("121˚W" ,"120˚W" ,"119˚W" ,"118˚W" ,"117˚W" ,"116˚W" ,"115˚W")) +
  scale_y_continuous(breaks = c(32:35),labels = c("32˚N" ,"33˚N" ,"34˚N" ,"35˚N")) +
  coord_sf(xlim = c(-121,-115), ylim = c(32, 35), expand = T) +
  theme_classic() +
  theme(axis.title = element_blank(),
        legend.position = c(0.2, 0.45),
        legend.justification = c("right", "top"),
        legend.box.just = "right")


###########################################################################
#2) California from CDFW
############################################################################

#depth data
bathy_200m <- raster::raster("raw_data/200mEEZ_BathyGrids/bd200m_v2i/w001001.adf")
bathy_200m.dt <- data.table(as.data.frame(values(bathy_200m))) #just depth values
bathy_200m.xy <- data.table(as.data.frame(bathy_200m, xy = T)) #this should extract x/y and depth values correctly, but for some reason data are factored, so I have to do manually in next step

bathy_200m.dt <- cbind(bathy_200m.xy[,.(x,y)],bathy_200m.dt[,.(`values(bathy_200m)`)])

#coordinate system of raster
crs_Raster <- sf::st_crs(raster::crs(bathy_200m))

#convert points to sf with non-projected crs (must do first)
VRG_lat_lon_all.sf <- st_as_sf(VRG_lat_lon_all.r, coords = c("Longitude", "Latitude"),
                               crs = 4326) # WGS84 - a good default for unprojected coords)

VRG_lat_lon_all.t <- st_transform(VRG_lat_lon_all.sf, crs = crs_Raster) #convert to raster projection for extraction

#match sites to depths
VRG_lat_lon_all.r$CDFW_200m <- extract(bathy_200m, VRG_lat_lon_all.t)

#difference
VRG_lat_lon_all.r[,diff_survey_CDFW := abs(CDFW_200m-SurveyDepth)]

# Create a ggplot using the raster data frame
ggplot() +
  geom_raster(data=bathy_200m.dt, aes(x = x, y = y, fill = `values(bathy_200m)`)) +
  geom_sf(data = VRG_lat_lon_all.t, color = "red", size = 0.5) +
  scale_fill_gradientn(colors = c("midnightblue","steelblue4","steelblue3","steelblue1","lightsteelblue3","lightsteelblue1","white","darkgreen"),
                       breaks = c(-4830,-500,-20,-10,-5,-2,-0.01,0), name = "Depth") +
  lims(x = c(-63900,307400), y = c(-707400,-300000)) +
  theme_classic() +
  theme(axis.title = element_blank(),
        legend.position = c(0.3, 0.3),
        legend.justification = c("right", "top"),
        legend.box.just = "right")


###########################################################################
#3) California from 
############################################################################

############################################################################
#Link to 200m isobath (typically high Chl-a along the shelf break that closely follows the 200 m isobath)
############################################################################
  
CA_contours_5m <- read_sf(file.path("raw_data","contours_5m","contours_5m.shp"))

CA_200m_contour <- CA_contours_5m %>% filter(CONTOUR == -200)
CA_200m_contour.t <- st_transform(CA_200m_contour, crs = crs(VRG_lat_lon_all.sf)) #match crs
CA_0m_contour <- CA_contours_5m %>% filter(CONTOUR == 0)

#calculate distance of point to each of 37 spatial features in CA_200m_contour.t
VRG_site_dist_200m <- data.table(st_distance(VRG_lat_lon_all.sf, CA_200m_contour.t))

#we only want closest value (minimum distance)
# Find the minimum distance for each row
VRG_site_dist_200m_min <- apply(VRG_site_dist_200m, 1, min)

#add back to point dt
VRG_lat_lon_all.r[,dist_200m_bath := VRG_site_dist_200m_min]

#same map as above, but with 200m isobath and points colored by distance to isobath
bathy_vrg_xyz <- as.xyz(bathy_VRG) #set as xyz to extract contour

autoplot.bathy(bathy_VRG, geom=c("tile"
                                 # ,"contour" exclude contour
)) +
  scale_fill_etopo(breaks = c(-12000,-6000, 0, 6000, 10000), labels = c(12, 6, 0, 6, 10), #from marmap, great way to visualize land and water instead of 'world' object
                   guide = guide_colorbar(frame.colour = "black", ticks.colour = "black")) + 
  borders("world", colour = "black") +
  labs(y = "Latitude", x = "Longitude", fill = "Elevation/Depth\n(1000s of m)") +
  geom_contour(data = bathy_vrg_xyz, 
               aes(x = V1, y = V2, z = V3),
               breaks = -200, color = "white", linewidth = 0.5) +
  geom_point(data = VRG_lat_lon_all.r, aes(x = Longitude, y = Latitude, color = dist_200m_bath), size = 0.2) +
  scale_colour_gradientn(colors = c("red","gold","white"), name = "Distance to 200m\nIsobath") +
  scale_x_continuous(breaks = c(-121:-115), labels = c("121˚W" ,"120˚W" ,"119˚W" ,"118˚W" ,"117˚W" ,"116˚W" ,"115˚W")) +
  scale_y_continuous(breaks = c(32:35),labels = c("32˚N" ,"33˚N" ,"34˚N" ,"35˚N")) +
  coord_sf(xlim = c(-121,-115), ylim = c(32, 35), expand = T) +
  theme_classic() +
  theme(axis.title = element_blank(),
        legend.position = c(0.93, 1.1),
        legend.justification = c("right", "top"),
        legend.box.just = "right")
