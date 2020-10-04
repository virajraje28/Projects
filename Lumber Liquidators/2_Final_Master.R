setwd('D:/MODEL')
load('RDATA/DataCleanup_Final.RData')

### CREATE SITE VARIABLES
library(sp)
library(rgdal)
library(geosphere)
library(tidyverse)

# convert data to a SpatialPointsDataFrame object
xy <- SpatialPointsDataFrame(
  matrix(c(final_site$lon, final_site$lat), ncol=2), final_site,#  data.frame(ID=seq(1:nrow(final_site))),
  proj4string=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84"))

# use the distm function to generate a geodesic distance matrix in meters
mdist <- distm(xy)

# cluster all points using a hierarchical clustering approach
hc <- hclust(as.dist(mdist), method="complete")

# define the distance threshold, in this case 40 m
d=160934 #1 mile = 1609.34 --> 100 miles

# define clusters based on a tree "height" cutoff "d" and add them to the SpDataFrame
final_site$clust <- cutree(hc, h=d)

### CREATE SALE VARIABLES

final_sales %>% 
  left_join(select(final_site, store, lon, lat),  by = c('store' = 'store')) %>% 
  rename(orders.lon = lon, orders.lat = lat)  %>% 
  left_join(select(final_site, store, clust),  by = c('store' = 'store')) %>% 
  rename(orders.clust = clust)  %>% 
  left_join(select(final_site, store, lon, lat),  by = c('ship.from.site' = 'store')) %>% 
  rename(fill.lon = lon, fill.lat = lat)  %>% 
  left_join(select(final_site, store, clust),  by = c('ship.from.site' = 'store')) %>% 
  rename(fill.clust = clust) %>% 
  mutate(shipment_distance = distHaversine(cbind(orders.lon, orders.lat), cbind(fill.lon, fill.lat),r=3958.8)) -> final_sales

final_sales %>% 
  mutate(OUTofCLUST = if_else(orders.clust!=fill.clust,1,0)) -> final_sales

final_sales %>% 
  mutate(OUTofOrderStore = if_else(store != ship.from.site ,1,0)) -> final_sales

### Join Files

library(sqldf)
final_master <- final_sales %>% 
  left_join(final_site,  by = c('store' = 'store')) %>% 
  left_join(final_art, by=c('article'='article'))
colnames(final_art)
final_master <- final_master[order(as.Date(final_master$day, format="%d/%m/%Y")),]


final_master <- sqldf('
                      SELECT* FROM final_master
                      LEFT JOIN final_prom
                      ON final_master.day
                      BETWEEN final_prom.start
                      AND final_prom.end
                      ')

#########Adding demographic information#########
#Below zip codes for below stores were not present. Updating zip code in demographic file with neighbouring one
# store - zip   -- zip for demographc
# 1153 - 14623  -- 14642
# 1323 - 16066  --16063
# 1214 - 44512  --44513
# 1087 - 50322  --50325
# 1053 - 14225  --14043
# 1272 - 44060 -- 44061
# 1017 - 32839 -- 32809
# 1313 - 44070 -- 44145
# 1066 - 73127 --73128
# 1363 - 76205 -- 76202
# 1304 - 07083 -- 07033
# 1322 - 55337 -- 55306

final_master <- final_master %>%
  left_join(final_demographic, by = c('postal.code' = 'Zipcode'))

final_master <- final_master %>%
  left_join(final_weather, by = c('postal.code' = 'Zipcode'))


#Sum Sales Data for each Site------------------------------------------------
#STORE_TOT: This is creating a chart for total sales ------------------------
final_master %>% 
  group_by(store) %>% 
  summarise(ORDER_DOLLARS = sum(orders.dol), Mean_Order_Dollars = mean(orders.dol)) %>%
  arrange(store) -> STORE_TOT
#CLASS_TOT: This is creating a chart TOT and PCT for class sales by each store ----------------------
final_master %>% 
  group_by(store,class.desc) %>% 
  summarise(CLASS_DOLLARS = sum(orders.dol)) %>% 
  arrange(store) %>%
  spread(class.desc, CLASS_DOLLARS) -> CLASS_TOT
colnames(CLASS_TOT) <- c('store','Bamboo','Cork_Flooring','Engineered','Laminate','Solid_Domestics','Solid_Exotics','Vinyl_Plank','Wood_Plank_Tile')

CLASS_TOT[is.na(CLASS_TOT)] <- 0
CLASS_TOT$Bamboo_PCT <- CLASS_TOT$Bamboo/STORE_TOT$ORDER_DOLLARS
CLASS_TOT$Cork_Flooring_PCT <- CLASS_TOT$Cork_Flooring/STORE_TOT$ORDER_DOLLARS
CLASS_TOT$Engineered_PCT <- CLASS_TOT$Engineered/STORE_TOT$ORDER_DOLLARS
CLASS_TOT$Laminate_PCT <- CLASS_TOT$Laminate/STORE_TOT$ORDER_DOLLARS
CLASS_TOT$Solid_Domestics_PCT <- CLASS_TOT$Solid_Domestics/STORE_TOT$ORDER_DOLLARS
CLASS_TOT$Solid_Exotics_PCT <- CLASS_TOT$Solid_Exotics/STORE_TOT$ORDER_DOLLARS
CLASS_TOT$Vinyl_Plank_PCT <- CLASS_TOT$Vinyl_Plank/STORE_TOT$ORDER_DOLLARS
CLASS_TOT$Wood_Plank_Tile_PCT <- CLASS_TOT$Wood_Plank_Tile/STORE_TOT$ORDER_DOLLARS


### CAT_TOT: This is creating a chart for TOT and PCT category sales by each store ----------------------
final_master %>% 
  group_by(store,cat.desc) %>% 
  summarise(CAT_DOLLARS = sum(orders.dol)) %>% 
  arrange(store) %>%
  spread(cat.desc, CAT_DOLLARS)-> CAT_TOT
colnames(CAT_TOT) <- c('store','Bamboo_Com','Bamboo_Eng','Bamboo_Odd_Lots','Bamboo_Solid','Bamboo_SP','Bellawood_Dom','Bellawood_Eng','Bellawood_Exotics','Ceramic_Comp_Plank','CLK_Porcelain_Plank','Cork','Cork_Odd_Lots','Cork_SP','Distressed_Dom','Distressed_Exotics','Domestic_Odd_Lots','Domestic_SP','Eng_Commercial','Eng_Distressed','Eng_Domestics','Eng_Exotics','Eng_Odd_Lots','Eng_SP','Eng_Vinyl_Plank','Exotic_Odd_Lots','Exotic_SP','Laminate_Odd_Lots','Laminate_SP','Laminates','Porcelain_Stone_Look','Porcelain_Wood_Plank','Pre_Finished_Dom','Pre_Finished_Exotics','Unfinished_Dom','Unfinished_Exotics','Vinyl','Vinyl_Plank_Comm','Vinyl_Plank_Odd_Lots','Vinyl_Plank_SP','Wood_Plank_Odd_Lots')

CAT_TOT[is.na(CAT_TOT)] <- 0

CAT_TOT$Bamboo_Com_PCT <- CAT_TOT$Bamboo_Com/CLASS_TOT$Bamboo #STORE_TOT$ORDER_DOLLARS
CAT_TOT$Bamboo_Eng_PCT <- CAT_TOT$Bamboo_Eng/CLASS_TOT$Bamboo #STORE_TOT$ORDER_DOLLARS
CAT_TOT$Bamboo_Odd_Lots_PCT <- CAT_TOT$Bamboo_Odd_Lots/CLASS_TOT$Bamboo #STORE_TOT$ORDER_DOLLARS
CAT_TOT$Bamboo_Solid_PCT <- CAT_TOT$Bamboo_Solid/CLASS_TOT$Bamboo # STORE_TOT$ORDER_DOLLARS
CAT_TOT$Bamboo_SP_PCT <- CAT_TOT$Bamboo_SP/CLASS_TOT$Bamboo #STORE_TOT$ORDER_DOLLARS
CAT_TOT$Bellawood_Dom_PCT <- CAT_TOT$Bellawood_Dom/CLASS_TOT$Solid_Domestics #STORE_TOT$ORDER_DOLLARS 
CAT_TOT$Bellawood_Eng_PCT <- CAT_TOT$Bellawood_Eng/CLASS_TOT$Engineered #STORE_TOT$ORDER_DOLLARS
CAT_TOT$Bellawood_Exotics_PCT <- CAT_TOT$Bellawood_Exotics/CLASS_TOT$Solid_Exotics #STORE_TOT$ORDER_DOLLARS
CAT_TOT$Ceramic_Comp_Plank_PCT <- CAT_TOT$Ceramic_Comp_Plank/CLASS_TOT$Wood_Plank_Tile #STORE_TOT$ORDER_DOLLARS
CAT_TOT$CLK_Porcelain_Plank_PCT <- CAT_TOT$CLK_Porcelain_Plank/CLASS_TOT$Wood_Plank_Tile #STORE_TOT$ORDER_DOLLARS
CAT_TOT$Cork_PCT <- CAT_TOT$Cork/CLASS_TOT$Cork_Flooring #STORE_TOT$ORDER_DOLLARS
CAT_TOT$Cork_Odd_Lots_PCT <- CAT_TOT$Cork_Odd_Lots/CLASS_TOT$Cork_Flooring #STORE_TOT$ORDER_DOLLARS
CAT_TOT$Cork_SP_PCT <- CAT_TOT$Cork_SP/CLASS_TOT$Cork_Flooring #STORE_TOT$ORDER_DOLLARS/STORE_TOT$ORDER_DOLLARS
CAT_TOT$Distressed_Dom_PCT <- CAT_TOT$Distressed_Dom/CLASS_TOT$Solid_Domestics #STORE_TOT$ORDER_DOLLARS
CAT_TOT$Distressed_Exotics_PCT <- CAT_TOT$Distressed_Exotics/CLASS_TOT$Solid_Exotics #STORE_TOT$ORDER_DOLLARS
CAT_TOT$Domestic_Odd_Lots_PCT <- CAT_TOT$Domestic_Odd_Lots/CLASS_TOT$Solid_Domestics #STORE_TOT$ORDER_DOLLARS
CAT_TOT$Domestic_SP_PCT <- CAT_TOT$Domestic_SP/CLASS_TOT$Solid_Domestics #STORE_TOT$ORDER_DOLLARS
CAT_TOT$Eng_Commercial_PCT <- CAT_TOT$Eng_Commercial/CLASS_TOT$Engineered #STORE_TOT$ORDER_DOLLARS
CAT_TOT$Eng_Distressed_PCT <- CAT_TOT$Eng_Distressed/CLASS_TOT$Engineered #STORE_TOT$ORDER_DOLLARS
CAT_TOT$Eng_Domestics_PCT <- CAT_TOT$Eng_Domestics/CLASS_TOT$Engineered #STORE_TOT$ORDER_DOLLARS
CAT_TOT$Eng_Exotics_PCT <- CAT_TOT$Eng_Exotics/CLASS_TOT$Engineered #STORE_TOT$ORDER_DOLLARS
CAT_TOT$Eng_Odd_Lots_PCT <- CAT_TOT$Eng_Odd_Lots/CLASS_TOT$Engineered #STORE_TOT$ORDER_DOLLARS
CAT_TOT$Eng_SP_PCT <- CAT_TOT$Eng_SP/CLASS_TOT$Engineered #STORE_TOT$ORDER_DOLLARS
CAT_TOT$Eng_Vinyl_Plank_PCT <- CAT_TOT$Eng_Vinyl_Plank/CLASS_TOT$Vinyl_Plank #STORE_TOT$ORDER_DOLLARS
CAT_TOT$Exotic_Odd_Lots_PCT <- CAT_TOT$Exotic_Odd_Lots/CLASS_TOT$Solid_Exotics #STORE_TOT$ORDER_DOLLARS
CAT_TOT$Exotic_SP_PCT <- CAT_TOT$Exotic_SP/CLASS_TOT$Solid_Exotics #STORE_TOT$ORDER_DOLLARS
CAT_TOT$Laminate_Odd_Lots_PCT <- CAT_TOT$Laminate_Odd_Lots/CLASS_TOT$Laminate #STORE_TOT$ORDER_DOLLARS
CAT_TOT$Laminate_SP_PCT <- CAT_TOT$Laminate_SP/CLASS_TOT$Laminate #STORE_TOT$ORDER_DOLLARS
CAT_TOT$Laminates_PCT <- CAT_TOT$Laminates/CLASS_TOT$Laminate #STORE_TOT$ORDER_DOLLARS
CAT_TOT$Porcelain_Stone_Look_PCT <- CAT_TOT$Porcelain_Stone_Look/CLASS_TOT$Wood_Plank_Tile #STORE_TOT$ORDER_DOLLARS
CAT_TOT$Porcelain_Wood_Plank_PCT <- CAT_TOT$Porcelain_Wood_Plank/CLASS_TOT$Wood_Plank_Tile #STORE_TOT$ORDER_DOLLARS
CAT_TOT$Pre_Finished_Dom_PCT <- CAT_TOT$Pre_Finished_Dom/CLASS_TOT$Solid_Domestics #STORE_TOT$ORDER_DOLLARS
CAT_TOT$Pre_Finished_Exotics_PCT <- CAT_TOT$Pre_Finished_Exotics/CLASS_TOT$Solid_Exotics #STORE_TOT$ORDER_DOLLARS
CAT_TOT$Unfinished_Dom_PCT <- CAT_TOT$Unfinished_Dom/CLASS_TOT$Solid_Domestics #STORE_TOT$ORDER_DOLLARS
CAT_TOT$Unfinished_Exotics_PCT <- CAT_TOT$Unfinished_Exotics/CLASS_TOT$Solid_Exotics #STORE_TOT$ORDER_DOLLARS
CAT_TOT$Vinyl_PCT <-CAT_TOT$Vinyl/CLASS_TOT$Vinyl_Plank #STORE_TOT$ORDER_DOLLARS
CAT_TOT$Vinyl_Plank_Comm_PCT <- CAT_TOT$Vinyl_Plank_Comm/CLASS_TOT$Vinyl_Plank #STORE_TOT$ORDER_DOLLARS
CAT_TOT$Vinyl_Plank_Odd_Lots_PCT <- CAT_TOT$Vinyl_Plank_Odd_Lots/CLASS_TOT$Vinyl_Plank #STORE_TOT$ORDER_DOLLARS
CAT_TOT$Vinyl_Plank_SP_PCT <- CAT_TOT$Vinyl_Plank_SP/CLASS_TOT$Vinyl_Plank #STORE_TOT$ORDER_DOLLARS
CAT_TOT$Wood_Plank_Odd_Lots_PCT <- CAT_TOT$Wood_Plank_Odd_Lots/CLASS_TOT$Wood_Plank_Tile #STORE_TOT$ORDER_DOLLARS




### Add to final_site -------
final_site %>% 
  left_join(CLASS_TOT,  by = c('store' = 'store')) %>% 
  left_join(CAT_TOT,  by = c('store' = 'store')) %>% 
  left_join(STORE_TOT,  by = c('store' = 'store')) -> final_site
final_site[is.na(final_site)] <- 0

#joining demographic info
final_site <- final_site %>%
  left_join(final_demographic, by = c('postal.code' = 'Zipcode'))

final_site <- final_site %>%
  distinct_all()




#CLusters based inventory
#Clusters based on shipments
#clusters based on density
#clusters based on demographics


###########################################Order units - NEW CODE############################################################
final_master %>% 
  group_by(store) %>% 
  arrange(store) %>%
  summarise(ORDER_UNITS = sum(orders.units)) -> STORE_TOT_units
### ARTICLE_TOT: NOT YET MADE ---
### CLASS_TOT: This is creating a chart TOT and PCT for class sales by each store ----------------------
final_master %>% 
  group_by(store,class.desc) %>% 
  summarise(CLASS_UNITS = sum(orders.units)) %>% 
  spread(class.desc, CLASS_UNITS) -> CLASS_TOT_units

colnames(CLASS_TOT_units) <- c('store','Bamboo_Units','Cork_Flooring_Units','Engineered_Units','Laminate_Units','Solid_Domestics_Units','Solid_Exotics_Units','Vinyl_Plank_Units','Wood_Plank_Tile_Units')
unique(final_art$class.desc)
CLASS_TOT_units[is.na(CLASS_TOT_units)] <- 0
CLASS_TOT_units$Bamboo_PCT_Units <- CLASS_TOT_units$Bamboo_Units/STORE_TOT_units$ORDER_UNITS
CLASS_TOT_units$Cork_Flooring_PCT_Units <- CLASS_TOT_units$Cork_Flooring_Units/STORE_TOT_units$ORDER_UNITS
CLASS_TOT_units$Engineered_PCT_Units <- CLASS_TOT_units$Engineered_Units/STORE_TOT_units$ORDER_UNITS
CLASS_TOT_units$Laminate_PCT_Units <- CLASS_TOT_units$Laminate_Units/STORE_TOT_units$ORDER_UNITS
CLASS_TOT_units$Solid_Domestics_PCT_Units <- CLASS_TOT_units$Solid_Domestics_Units/STORE_TOT_units$ORDER_UNITS
CLASS_TOT_units$Solid_Exotics_PCT_Units <- CLASS_TOT_units$Solid_Exotics_Units/STORE_TOT_units$ORDER_UNITS
CLASS_TOT_units$Vinyl_Plank_PCT_Units <- CLASS_TOT_units$Vinyl_Plank_Units/STORE_TOT_units$ORDER_UNITS
CLASS_TOT_units$Wood_Plank_Tile_PCT_Units <- CLASS_TOT_units$Wood_Plank_Tile_Units/STORE_TOT_units$ORDER_UNITS






### CAT_TOT: This is creating a chart for TOT and PCT category sales by each store ----------------------
final_master %>% 
  group_by(store,cat.desc) %>% 
  summarise(CAT_UNITS = sum(orders.units)) %>% 
  spread(cat.desc, CAT_UNITS)-> CAT_TOT_units
colnames(CAT_TOT_units) <- c('store','Bamboo_Com_Units','Bamboo_Eng_Units','Bamboo_Odd_Lots_Units','Bamboo_Solid_Units','Bamboo_SP_Units','Bellawood_Dom_Units','Bellawood_Eng_Units','Bellawood_Exotics_Units','Ceramic_Comp_Plank_Units','CLK_Porcelain_Plank_Units','Cork_Units','Cork_Odd_Lots_Units','Cork_SP_Units','Distressed_Dom_Units','Distressed_Exotics_Units','Domestic_Odd_Lots_Units','Domestic_SP_Units','Eng_Commercial_Units','Eng_Distressed_Units','Eng_Domestics_Units','Eng_Exotics_Units','Eng_Odd_Lots_Units','Eng_SP_Units','Eng_Vinyl_Plank_Units','Exotic_Odd_Lots_Units','Exotic_SP_Units','Laminate_Odd_Lots_Units','Laminate_SP_Units','Laminates_Units','Porcelain_Stone_Look_Units','Porcelain_Wood_Plank_Units','Pre_Finished_Dom_Units','Pre_Finished_Exotics_Units','Unfinished_Dom_Units','Unfinished_Exotics_Units','Vinyl_Units','Vinyl_Plank_Comm_Units','Vinyl_Plank_Odd_Lots_Units','Vinyl_Plank_SP_Units','Wood_Plank_Odd_Lots_Units')

CAT_TOT_units[is.na(CAT_TOT_units)] <- 0
CAT_TOT_units$Bamboo_Com_PCT_Units <- CAT_TOT_units$Bamboo_Com_Units/CLASS_TOT_units$Bamboo_Units #STORE_TOT_units$ORDER_UNITS
CAT_TOT_units$Bamboo_Eng_PCT_Units <- CAT_TOT_units$Bamboo_Eng_Units/CLASS_TOT_units$Bamboo_Units #STORE_TOT_units$ORDER_UNITS
CAT_TOT_units$Bamboo_Odd_Lots_PCT_Units <- CAT_TOT_units$Bamboo_Odd_Lots_Units/CLASS_TOT_units$Bamboo_Units #STORE_TOT_units$ORDER_UNITS
CAT_TOT_units$Bamboo_Solid_PCT_Units <- CAT_TOT_units$Bamboo_Solid_Units/CLASS_TOT_units$Bamboo_Units #STORE_TOT_units$ORDER_UNITS
CAT_TOT_units$Bamboo_SP_PCT_Units <- CAT_TOT_units$Bamboo_SP_Units/CLASS_TOT_units$Bamboo_Units #STORE_TOT_units$ORDER_UNITS
CAT_TOT_units$Bellawood_Dom_PCT_Units <- CAT_TOT_units$Bellawood_Dom_Units/CLASS_TOT_units$Solid_Domestics_Units #STORE_TOT_units$ORDER_UNITS
CAT_TOT_units$Bellawood_Eng_PCT_Units <- CAT_TOT_units$Bellawood_Eng_Units/CLASS_TOT_units$Engineered_Units #STORE_TOT_units$ORDER_UNITS
CAT_TOT_units$Bellawood_Exotics_PCT_Units <- CAT_TOT_units$Bellawood_Exotics_Units/CLASS_TOT_units$Solid_Exotics_Units #STORE_TOT_units$ORDER_UNITS
CAT_TOT_units$Ceramic_Comp_Plank_PCT_Units <- CAT_TOT_units$Ceramic_Comp_Plank_Units/CLASS_TOT_units$Wood_Plank_Tile_Units #STORE_TOT_units$ORDER_UNITS
CAT_TOT_units$CLK_Porcelain_Plank_PCT_Units <- CAT_TOT_units$CLK_Porcelain_Plank_Units/CLASS_TOT_units$Wood_Plank_Tile_Units #STORE_TOT_units$ORDER_UNITS
CAT_TOT_units$Cork_PCT_Units <- CAT_TOT_units$Cork_Units/CLASS_TOT_units$Cork_Flooring_Units #STORE_TOT_units$ORDER_UNITS
CAT_TOT_units$Cork_Odd_Lots_PCT_Units <- CAT_TOT_units$Cork_Odd_Lots_Units/CLASS_TOT_units$Cork_Flooring_Units #STORE_TOT_units$ORDER_UNITS
CAT_TOT_units$Cork_SP_PCT_Units <- CAT_TOT_units$Cork_SP_Units/CLASS_TOT_units$Cork_Flooring_Units #STORE_TOT_units$ORDER_UNITS
CAT_TOT_units$Distressed_Dom_PCT_Units <- CAT_TOT_units$Distressed_Dom_Units/CLASS_TOT_units$Solid_Domestics_Units #STORE_TOT_units$ORDER_UNITS
CAT_TOT_units$Distressed_Exotics_PCT_Units <- CAT_TOT_units$Distressed_Exotics_Units/CLASS_TOT_units$Solid_Exotics_Units #STORE_TOT_units$ORDER_UNITS
CAT_TOT_units$Domestic_Odd_Lots_PCT_Units <- CAT_TOT_units$Domestic_Odd_Lots_Units/CLASS_TOT_units$Solid_Domestics_Units #STORE_TOT_units$ORDER_UNITS
CAT_TOT_units$Domestic_SP_PCT_Units <- CAT_TOT_units$Domestic_SP_Units/CLASS_TOT_units$Solid_Domestics_Units #STORE_TOT_units$ORDER_UNITS
CAT_TOT_units$Eng_Commercial_PCT_Units <- CAT_TOT_units$Eng_Commercial_Units/CLASS_TOT_units$Engineered_Units #STORE_TOT_units$ORDER_UNITS
CAT_TOT_units$Eng_Distressed_PCT_Units <- CAT_TOT_units$Eng_Distressed_Units/CLASS_TOT_units$Engineered_Units #STORE_TOT_units$ORDER_UNITS
CAT_TOT_units$Eng_Domestics_PCT_Units <- CAT_TOT_units$Eng_Domestics_Units/CLASS_TOT_units$Engineered_Units #STORE_TOT_units$ORDER_UNITS
CAT_TOT_units$Eng_Exotics_PCT_Units <- CAT_TOT_units$Eng_Exotics_Units/CLASS_TOT_units$Engineered_Units #STORE_TOT_units$ORDER_UNITS
CAT_TOT_units$Eng_Odd_Lots_PCT_Units <- CAT_TOT_units$Eng_Odd_Lots_Units/CLASS_TOT_units$Engineered_Units #STORE_TOT_units$ORDER_UNITS
CAT_TOT_units$Eng_SP_PCT_Units <- CAT_TOT_units$Eng_SP_Units/CLASS_TOT_units$Engineered_Units #STORE_TOT_units$ORDER_UNITS
CAT_TOT_units$Eng_Vinyl_Plank_PCT_Units <- CAT_TOT_units$Eng_Vinyl_Plank_Units/CLASS_TOT_units$Vinyl_Plank_Units #STORE_TOT_units$ORDER_UNITS
CAT_TOT_units$Exotic_Odd_Lots_PCT_Units <- CAT_TOT_units$Exotic_Odd_Lots_Units/CLASS_TOT_units$Solid_Exotics_Units #STORE_TOT_units$ORDER_UNITS
CAT_TOT_units$Exotic_SP_PCT_Units <- CAT_TOT_units$Exotic_SP_Units/CLASS_TOT_units$Solid_Exotics_Units #STORE_TOT_units$ORDER_UNITS
CAT_TOT_units$Laminate_Odd_Lots_PCT_Units <- CAT_TOT_units$Laminate_Odd_Lots_Units/CLASS_TOT_units$Laminate_Units #STORE_TOT_units$ORDER_UNITS
CAT_TOT_units$Laminate_SP_PCT_Units <- CAT_TOT_units$Laminate_SP_Units/CLASS_TOT_units$Laminate_Units #STORE_TOT_units$ORDER_UNITS
CAT_TOT_units$Laminates_PCT_Units <- CAT_TOT_units$Laminates_Units/CLASS_TOT_units$Laminate_Units #STORE_TOT_units$ORDER_UNITS
CAT_TOT_units$Porcelain_Stone_Look_PCT_Units <- CAT_TOT_units$Porcelain_Stone_Look_Units/CLASS_TOT_units$Wood_Plank_Tile_Units #STORE_TOT_units$ORDER_UNITS
CAT_TOT_units$Porcelain_Wood_Plank_PCT_Units <- CAT_TOT_units$Porcelain_Wood_Plank_Units/CLASS_TOT_units$Wood_Plank_Tile_Units #STORE_TOT_units$ORDER_UNITS
CAT_TOT_units$Pre_Finished_Dom_PCT_Units <- CAT_TOT_units$Pre_Finished_Dom_Units/CLASS_TOT_units$Solid_Domestics_Units #STORE_TOT_units$ORDER_UNITS
CAT_TOT_units$Pre_Finished_Exotics_PCT_Units <- CAT_TOT_units$Pre_Finished_Exotics_Units/CLASS_TOT_units$Solid_Exotics_Units #STORE_TOT_units$ORDER_UNITS
CAT_TOT_units$Unfinished_Dom_PCT_Units <- CAT_TOT_units$Unfinished_Dom_Units/CLASS_TOT_units$Solid_Domestics_Units #STORE_TOT_units$ORDER_UNITS
CAT_TOT_units$Unfinished_Exotics_PCT_Units <- CAT_TOT_units$Unfinished_Exotics_Units/CLASS_TOT_units$Solid_Exotics_Units #STORE_TOT_units$ORDER_UNITS
CAT_TOT_units$Vinyl_PCT_Units <-CAT_TOT_units$Vinyl_Units/CLASS_TOT_units$Vinyl_Plank_Units #STORE_TOT_units$ORDER_UNITS
CAT_TOT_units$Vinyl_Plank_Comm_PCT_Units <- CAT_TOT_units$Vinyl_Plank_Comm_Units/CLASS_TOT_units$Vinyl_Plank_Units #STORE_TOT_units$ORDER_UNITS
CAT_TOT_units$Vinyl_Plank_Odd_Lots_PCT_Units <- CAT_TOT_units$Vinyl_Plank_Odd_Lots_Units/CLASS_TOT_units$Vinyl_Plank_Units #STORE_TOT_units$ORDER_UNITS
CAT_TOT_units$Vinyl_Plank_SP_PCT_Units <- CAT_TOT_units$Vinyl_Plank_SP_Units/CLASS_TOT_units$Vinyl_Plank_Units #STORE_TOT_units$ORDER_UNITS
CAT_TOT_units$Wood_Plank_Odd_Lots_PCT_Units <- CAT_TOT_units$Wood_Plank_Odd_Lots_Units/CLASS_TOT_units$Wood_Plank_Tile_Units #STORE_TOT_units$ORDER_UNITS




### Add to final_site -------
final_site %>% 
  left_join(CLASS_TOT_units,  by = c('store' = 'store')) %>% 
  left_join(CAT_TOT_units,  by = c('store' = 'store')) %>% 
  left_join(STORE_TOT_units,  by = c('store' = 'store')) -> final_site
final_site[is.na(final_site)] <- 0

################################################Articles dollars########################################
final_master %>% 
  group_by(store,article) %>% 
  summarise(article_DOLLARS = sum(orders.dol)) -> ART_TOT 

ART_TOT <- ART_TOT %>%
  left_join(final_art, by = c('article' = 'article')) %>%
  select(store,article,article_DOLLARS,cat.desc)

#To identifyarticles easily, concatenated cat description to article id
ART_TOT$article <- paste(ART_TOT$cat.desc, ART_TOT$article, sep = '_')

ART_TOT <- ART_TOT[1:3]

ART_TOT <- ART_TOT %>%
  spread(article, article_DOLLARS)

ART_TOT[is.na(ART_TOT)] <- 0

Col_count <- ncol(ART_TOT)

for(j in 2:Col_count)
{
  print(j)
  ART_TOT[,j+Col_count-1] <- ART_TOT[,j] / STORE_TOT[,2]
  names(ART_TOT)[j+Col_count-1] <- paste(names(ART_TOT)[j],"pct",sep = '_')
}

###############################################Articles dollars - END###################################
################################################Articles Units########################################
final_master %>% 
  group_by(store,article) %>% 
  summarise(article_Units = sum(orders.units)) -> ART_TOT_Units 

ART_TOT_Units <- ART_TOT_Units %>%
  left_join(final_art, by = c('article' = 'article')) %>%
  select(store,article,article_Units,cat.desc)

#To identifyarticles easily, concatenated cat description to article id
ART_TOT_Units$article <- paste(ART_TOT_Units$cat.desc, ART_TOT_Units$article, 'Units', sep = '_')

ART_TOT_Units <- ART_TOT_Units[1:3]

ART_TOT_Units <- ART_TOT_Units %>%
  spread(article, article_Units)

ART_TOT_Units[is.na(ART_TOT_Units)] <- 0

Col_count <- ncol(ART_TOT_Units)

for(j in 2:Col_count)
{
  print(j)
  ART_TOT_Units[,j+Col_count-1] <- ART_TOT_Units[,j] / STORE_TOT_units[,2]
  names(ART_TOT_Units)[j+Col_count-1] <- paste(names(ART_TOT_Units)[j],"pct",sep = '_')
}


final_site %>% 
  left_join(ART_TOT,  by = c('store' = 'store')) %>% 
  left_join(ART_TOT_Units,  by = c('store' = 'store')) -> final_site
final_site[is.na(final_site)] <- 0

###############################################Articles Units - END###################################



#CLusters based inventory
#Clusters based on shipments
#clusters based on density
#clusters based on demographics
rm(CAT_TOT)
rm(CLASS_TOT)
rm(STORE_TOT)
rm(CAT_TOT_units)
rm(CLASS_TOT_units)
rm(STORE_TOT_units)
rm(ART_TOT)
rm(ART_TOT_Units)
rm(j)
rm(Col_count)
rm(d)
rm(hc)
rm(xy)
rm(mdist)


save.image('RDATA/MASTER.RData')

#rm(dic)
#rm(web)
#rm(final_art)
#rm(final_prom)
#rm(final_site)
#rm(final_sales)
#rm(final_master)
#rm(final_weather)
#rm(final_inventory)
#rm(final_demographic)

