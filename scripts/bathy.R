##############################################################################
#California Bathymetry Data
#Zoë J. Kitchel
#Created 30 October 2023
#Modified 30 Octoer 2023
##############################################################################
#Bathymetry data accessed from https://filelib.wildlife.ca.gov/Public/R7_MR/BATHYMETRY/ on October 20, 2023
#Or, finer scale: https://pubs.usgs.gov/ds/487/ds487_text.pdf (3m res)
##############################################################################
#SETUP#
library(sf)
library(ggplot2)
library(raster)
############################################################################
#Load in shapefile
ca_bathy <- st_read(file.path("raw_data","contours_5m","contours_5m.shp"))

DEM_locations <- st_read("/Users/kitchel/Downloads/DEMCoverageAreas/DEMCoverageAreas.shp")

#or try raster
la1_raster <- raster("/Users/kitchel/Downloads/la1.txt")
############################################################################


st_crs(ca_bathy)

bathy_scb_plot <- ggplot(data = ca_bathy) +
  geom_sf() +
  lims(y = c(-642426.5,-500000), x = c(0, 280713))

ggsave(bathy_scb_plot, path = file.path("figures"), filename = "bathy_scb_plot.pdf")




#Match site lat lon to depth

https://data.usgs.gov/datacatalog/data/USGS:1313d046-7e62-495e-8cb0-88b6018dc644