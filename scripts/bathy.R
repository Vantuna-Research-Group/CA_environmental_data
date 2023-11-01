##############################################################################
#California Bathymetry Data
#Zoë J. Kitchel
#Created 30 October 2023
#Modified 30 October 2023
##############################################################################
#There are multiple options for where to get bathymetry data:
  #1) We have some (limited) site specific bathymetry. Talk to Chelsea.
  #2) Full state of California out to 200 eez lower rez: https://wildlife.ca.gov/Conservation/Marine/GIS/Downloads (Arc/Info Binary Grid (ADF) )
  #3) Full state of California higher rez (but data is currently offline) http://seafloor.otterlabs.org/SFMLwebDATA_SURVEYMAP.htm
      #Or, another fine scale option: https://pubs.usgs.gov/ds/487/ds487_text.pdf (3m res)

##############################################################################
#SETUP#
library(sf)
library(ggplot2)
library(raster)
library(marmap)
library(viridis)

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