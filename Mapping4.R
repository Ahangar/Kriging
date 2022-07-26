library(sf)
library(sp)
library(ggmap)
library(dplyr)
library(gstat)
#Import PM2.5 data#############################################################
pm25<-read.csv('ad_viz_plotval_data.csv') %>% 
  filter(Date=='04/20/2021')%>% 
  select(Date,Daily.Mean.PM2.5.Concentration,Site.Name,
         AQS_PARAMETER_CODE,AQS_PARAMETER_DESC,
                SITE_LATITUDE,SITE_LONGITUDE)

pm25 %>% glimpse()
##dropping duplicated observations at the same location
pm25 <- pm25[!duplicated(pm25["Site.Name"]),] 
##convert lat/lon to UTM projection
pm25 <- st_as_sf(pm25, coords = c("SITE_LONGITUDE", "SITE_LATITUDE"), 
                 crs = 4326)%>% st_transform(crs = 6340) 
##create x and y columns for kriging
pm25 <- pm25 %>% mutate(x=st_coordinates(pm25)[,c(1)],
                        y=st_coordinates(pm25)[,c(2)]) %>% as.data.frame()

#Creating the Grid over Texas #################################################
Texas_boundary<-st_read("./State.shp") %>% 
  st_transform(crs = 6340)

grid <- 
  st_make_grid(st_as_sfc(st_bbox(Texas_boundary)),
               cellsize = 20000,what = "polygons",
               square = TRUE,crs = 6340) %>% st_sf()

Texas_grid <- st_filter(grid, Texas_boundary, join = st_intersects)
##create x and y for kriging
Texas_grid <- Texas_grid %>% mutate(x=st_coordinates(st_centroid(Texas_grid))[,c(1)],
                                    y=st_coordinates(st_centroid(Texas_grid))[,c(2)])

#Kriging ######################################################################
##create grid centroid dataframe for kriging estimates
centroids <- Texas_grid %>% select(x,y) %>% as.data.frame()
##format data for gstat kriging
coordinates(pm25) <- ~ x+y
coordinates(centroids) <- ~ x+y
## calculates sample variogram values 
pm.vgm <- variogram(log(Daily.Mean.PM2.5.Concentration)~1, pm25) 
## fit exponential model to variogram
pm.fit <- fit.variogram(pm.vgm, model=vgm("Exp"))
plot(pm.vgm, pm.fit) 
## estimate kriged values on the grid
pm.kriged <- krige(log(Daily.Mean.PM2.5.Concentration) ~ 1, pm25,  centroids
                    , model=pm.fit)%>% as.data.frame

# Plot Final Interpolated map ################################################
##provide google map API
register_google(key = "AIzaSyDiGlFJqmxMyPRXwhAM2-FI_TfpnD_OjSk")  

basemap <- get_map(location = "Brady, TX", zoom = 6,maptype = "hybrid")

ggmap(basemap) +
  geom_sf(data = Texas_grid, aes(fill = exp(pm.kriged$var1.pred)), col = NA, inherit.aes = FALSE, alpha=.7) +
  geom_sf(data = Texas_boundary, inherit.aes = FALSE,alpha=0)+
  coord_sf(crs = 4326)+
  scale_fill_gradient(low = "khaki2", high = "brown4", 
                      guide = guide_colorbar(title = 'PM2.5',barheight = 10))

