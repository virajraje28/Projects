#rm(list = ls(all.names = TRUE)) #will clear all objects includes hidden objects.
#gc() #free up memrory and report the memory usage.

#These code lines allow to clear the panels and so free memory.
#Let me explain the code line by line:
#This is equivalent to click on the button clear all plots in the plots panel.
if(!is.null(dev.list())) dev.off()
#This is equivalent to click on the button clear objects from the workspace in the environment panel.
rm(list=ls())
#This is identical to Ctrl+L or to click on the clear console within the Edit menu.
cat("\014") 



setwd('D:/MODEL')
load('RDATA/MASTER.RData')
load('RDATA/InventorySalesMatrices.RData')

library(readxl)
library(tidyverse)

PCT_Cluster<- read_excel("DATA/Class Data File with PCT Cluster.xlsx",1)
SiteSalesCluster <- PCT_Cluster[,c('store','TSC_1447')] #read_excel("DATA/DemoCluster.xlsx",1)
colnames(SiteSalesCluster) <- c('store','SiteSalesCluster')
#SiteSalesCluster$store <- as.character(SiteSalesCluster$store)

DemoCluster <- read_excel("DATA/SavedCluster.xlsx",1)
DemoCluster <- DemoCluster[,c('store','ClusterJustDemographic')]
colnames(DemoCluster) <- c('store','DemographicCluster')

# AVERAGE INVENTORY COST
AVGINV <- read.csv('D:/MODEL/DATA/inventory_avg.csv')
AVGINV$article <- as.character(AVGINV$article)

AVGINV %>% 
  left_join(final_art, by=c('article','article')) %>% 
  select(store, article, class.desc, averageInventoryPerStore, inventoryCostPerUnit) -> AVGINV

#AVGINV %>% 
#left_join(final_inventory, by = c('article' = 'article')) %>%
#  group_by(store, article) %>%
#  summarise(averageInventory = (sum(inv.units, na.rm = TRUE)/84), inventoryCostPerUnit = sum(inv.cost, na.rm = TRUE)/n()) %>% 
#  select( averageInventory, inv.cost)




AVGINV$class.desc <- as.factor(AVGINV$class.desc)
AVGINV %>% 
  group_by(store, class.desc) %>% 
  summarise(TOT_DOLLARS = (mean(inventoryCostPerUnit))) %>% 
  arrange(store) %>%
  spread(na.omit(class.desc), TOT_DOLLARS) -> AVGINV
colnames(AVGINV) <- c("store", "Bamboo_INV", "Cork_Flooring_INV", "Engineered_INV", "Laminate_INV", "Solid_Domestics_INV", "Solid_Exotics_INV", "Vinyl_Plank_INV", "Wood_Plank_Tile_INV", "UNKNOWN_INV")



DemoCluster$store <- as.numeric(DemoCluster$store)
final_site$store <- as.numeric(final_site$store)
#final_site$store <- as.numeric(final_site$store)
site_sales_demographic <- final_site[,c('store','lat','lon','Bamboo','Bamboo_PCT','Cork_Flooring','Cork_Flooring_PCT','Engineered','Engineered_PCT','Laminate','Laminate_PCT','Solid_Domestics','Solid_Domestics_PCT','Solid_Exotics','Solid_Exotics_PCT','Vinyl_Plank','Vinyl_Plank_PCT','Wood_Plank_Tile','Wood_Plank_Tile_PCT','ORDER_DOLLARS')]
colnames(site_sales_demographic) <- c('store','lat','lon','Bamboo_SALES','Bamboo_PCT','Cork_Flooring_SALES','Cork_Flooring_PCT','Engineered_SALES','Engineered_PCT','Laminate_SALES','Laminate_PCT','Solid_Domestics_SALES','Solid_Domestics_PCT','Solid_Exotics_SALES','Solid_Exotics_PCT','Vinyl_Plank_SALES','Vinyl_Plank_PCT','Wood_Plank_Tile_SALES','Wood_Plank_Tile_PCT','ORDER_DOLLARS')
site_sales_demographic$Bamboo_PCT_quartile <- as.integer(cut(final_site$Bamboo_PCT, quantile(final_site$Bamboo_PCT, probs=0:3/3), include.lowest=TRUE))
site_sales_demographic$Cork_Flooring_PCT_quartile <- as.integer(cut(final_site$Cork_Flooring_PCT, quantile(final_site$Cork_Flooring_PCT, probs=0:3/3), include.lowest=TRUE))
site_sales_demographic$Engineered_PCT_quartile <- as.integer(cut(final_site$Engineered_PCT, quantile(final_site$Engineered_PCT, probs=0:3/3), include.lowest=TRUE))
site_sales_demographic$Laminate_PCT_quartile <- as.integer(cut(final_site$Laminate_PCT, quantile(final_site$Laminate_PCT, probs=0:3/3), include.lowest=TRUE))
site_sales_demographic$Solid_Domestics_PCT_quartile <- as.integer(cut(final_site$Solid_Domestics_PCT, quantile(final_site$Solid_Domestics_PCT, probs=0:3/3), include.lowest=TRUE))
site_sales_demographic$Solid_Exotics_PCT_quartile <- as.integer(cut(final_site$Solid_Exotics_PCT, quantile(final_site$Solid_Exotics_PCT, probs=0:3/3), include.lowest=TRUE))
site_sales_demographic$Vinyl_Plank_PCT_quartile <- as.integer(cut(final_site$Vinyl_Plank_PCT, quantile(final_site$Vinyl_Plank_PCT, probs=0:3/3), include.lowest=TRUE))
site_sales_demographic$Wood_Plank_Tile_PCT_quartile <- as.integer(cut(final_site$Wood_Plank_Tile_PCT, quantile(final_site$Wood_Plank_Tile_PCT, probs=0:3/3), include.lowest=TRUE))

#site_sales_demographic %>% 
#  left_join(DemoCluster, by = c('store'='store')) -> site_sales_demographic

#write.xlsx(site_sales_demographic, '/Users/trehall/Lumber Liquidators/Model/site_sales_demographic.xlsx', row.names=TRUE)

site_sales_demographic$store <- as.numeric(site_sales_demographic$store)
site_sales_demographic <- site_sales_demographic %>% 
  left_join(DemoCluster, by=c('store'='store')) %>% 
  select(store, lat, lon, DemographicCluster, Bamboo_SALES, Bamboo_PCT, Bamboo_PCT_quartile, Cork_Flooring_SALES, Cork_Flooring_PCT, Cork_Flooring_PCT_quartile, Engineered_SALES, Engineered_PCT, Engineered_PCT_quartile, Laminate_SALES, Laminate_PCT, Laminate_PCT_quartile, Solid_Domestics_SALES, Solid_Domestics_PCT, Solid_Domestics_PCT_quartile, Solid_Exotics_SALES, Solid_Exotics_PCT, Solid_Exotics_PCT_quartile, Vinyl_Plank_SALES, Vinyl_Plank_PCT, Vinyl_Plank_PCT_quartile, Wood_Plank_Tile_SALES, Wood_Plank_Tile_PCT, Wood_Plank_Tile_PCT_quartile, ORDER_DOLLARS)

#site_sales_demographic$store <- as.numeric(site_sales_demographic$store)
#site_sales_demographic <- site_sales_demographic %>% 
#  left_join(DemoCluster, by=c('store'='store')) %>% 
#  select(store, lat, lon, DemographicCluster, Bamboo_SALES, Bamboo_PCT, Bamboo_PCT_quartile, Cork_Flooring_SALES, Cork_Flooring_PCT, Cork_Flooring_PCT_quartile, Engineered_SALES, Engineered_PCT, Engineered_PCT_quartile, Laminate_SALES, Laminate_PCT, Laminate_PCT_quartile, Solid_Domestics_SALES, Solid_Domestics_PCT, Solid_Domestics_PCT_quartile, Solid_Exotics_SALES, Solid_Exotics_PCT, Solid_Exotics_PCT_quartile, Vinyl_Plank_SALES, Vinyl_Plank_PCT, Vinyl_Plank_PCT_quartile, Wood_Plank_Tile_SALES, Wood_Plank_Tile_PCT, Wood_Plank_Tile_PCT_quartile, ORDER_DOLLARS)


site_article_inventory <- DistinctStoreArticle %>% 
  left_join(final_art, by=c('article'='article')) %>% 
  select(store, article, class.desc, averageInventory) -> test

site_class_inventory <- site_article_inventory %>% 
  group_by(store,class.desc) %>% 
  summarise(CLASS_INV = sum(averageInventory)) %>% 
  arrange(store) %>%
  spread(class.desc, CLASS_INV)
colnames(site_class_inventory) <- c('store', 'Bamboo_AvgInventory', 'Cork_Flooring_AvgInventory', 'Engineered_AvgInventory', 'Laminate_AvgInventory', 'Solid_Domestics_AvgInventory', 'Solid_Exotics_AvgInventory', 'Vinyl_Plank_AvgInventory', 'Wood_Plank_Tile_AvgInventory', 'UNKNOWN')

site_class_inventory$store <- as.numeric(site_class_inventory$store)

site_demographic_sales_class_inventory <- site_class_inventory %>% 
  left_join(site_sales_demographic, by=c('store'='store')) %>% 
  select(store, lat, lon, DemographicCluster, Bamboo_SALES, Bamboo_PCT, Bamboo_PCT_quartile, Bamboo_AvgInventory, Cork_Flooring_SALES, Cork_Flooring_PCT, Cork_Flooring_PCT_quartile, Cork_Flooring_AvgInventory, Engineered_SALES, Engineered_PCT, Engineered_PCT_quartile, Engineered_AvgInventory, Laminate_SALES, Laminate_PCT, Laminate_PCT_quartile, Laminate_AvgInventory, Solid_Domestics_SALES, Solid_Domestics_PCT, Solid_Domestics_PCT_quartile, Solid_Domestics_AvgInventory, Solid_Exotics_SALES, Solid_Exotics_PCT, Solid_Exotics_PCT_quartile, Solid_Exotics_AvgInventory, Vinyl_Plank_SALES, Vinyl_Plank_PCT, Vinyl_Plank_PCT_quartile, Vinyl_Plank_AvgInventory, Wood_Plank_Tile_SALES, Wood_Plank_Tile_PCT, Wood_Plank_Tile_PCT_quartile, Wood_Plank_Tile_AvgInventory, ORDER_DOLLARS)

#write.xlsx(site_demographic_sales_class_inventory, '/Users/trehall/Lumber Liquidators/Model/site_demographic_sales_class_inventory.xlsx', row.names=TRUE)

# -----------------------------------------------------------------------------------------Matrix of all stores and distance
#STORE DISTANCE MATRIX:
StoreMatrix <- AllStoreCombinations[,c('store1','store2','distance')]
#StoreMatrix$store1 <- as.numeric(StoreMatrix$store1)
#StoreMatrix$store2 <- as.numeric(StoreMatrix$store2)

# SET SEARCH ZONE TO 15 MILES---------------------------------------------------------------------------
search_distance=15 #SET SEARCH ZONE TO 15 MILES

StoreDistanceMatrix<- StoreMatrix %>% 
  filter(as.numeric(store1)<as.numeric(store2) & distance < search_distance) 
StoreDistanceMatrix$store1 <- as.numeric(StoreDistanceMatrix$store1)
StoreDistanceMatrix$store2 <- as.numeric(StoreDistanceMatrix$store2)


# Add Class Sales by PCT Groupings, site demographics---------------------------------------------------------------------------
StoreDistanceMatrix %>% 
  left_join(site_demographic_sales_class_inventory,  by = c('store1' = 'store')) %>% 
  select(store1, DemographicCluster, store2,distance,Bamboo_PCT_quartile, Bamboo_AvgInventory, Cork_Flooring_PCT_quartile, Cork_Flooring_AvgInventory, Engineered_PCT_quartile, Engineered_AvgInventory, Laminate_PCT_quartile, Laminate_AvgInventory, Solid_Domestics_PCT_quartile, Solid_Domestics_AvgInventory, Solid_Exotics_PCT_quartile, Solid_Exotics_AvgInventory, Vinyl_Plank_PCT_quartile, Vinyl_Plank_AvgInventory, Wood_Plank_Tile_PCT_quartile, Wood_Plank_Tile_AvgInventory) -> site_demographics_distance_class_matrix
names(site_demographics_distance_class_matrix)[names(site_demographics_distance_class_matrix) =="DemographicCluster"] <- "DemographicCluster_store1"
names(site_demographics_distance_class_matrix)[names(site_demographics_distance_class_matrix) =="Bamboo_PCT_quartile"] <- "Bamboo_PCT_quartile_store1"
names(site_demographics_distance_class_matrix)[names(site_demographics_distance_class_matrix) =="Bamboo_AvgInventory"] <- "Bamboo_AvgInventory_store1"
names(site_demographics_distance_class_matrix)[names(site_demographics_distance_class_matrix) =="Cork_Flooring_PCT_quartile"] <- "Cork_Flooring_PCT_quartile_store1"
names(site_demographics_distance_class_matrix)[names(site_demographics_distance_class_matrix) =="Cork_Flooring_AvgInventory"] <- "Cork_Flooring_AvgInventory_store1"
names(site_demographics_distance_class_matrix)[names(site_demographics_distance_class_matrix) =="Engineered_PCT_quartile"] <- "Engineered_PCT_quartile_store1"
names(site_demographics_distance_class_matrix)[names(site_demographics_distance_class_matrix) =="Engineered_AvgInventory"] <- "Engineered_AvgInventory_store1"
names(site_demographics_distance_class_matrix)[names(site_demographics_distance_class_matrix) =="Laminate_PCT_quartile"] <- "Laminate_PCT_quartile_store1"
names(site_demographics_distance_class_matrix)[names(site_demographics_distance_class_matrix) =="Laminate_AvgInventory"] <- "Laminate_AvgInventory_store1"
names(site_demographics_distance_class_matrix)[names(site_demographics_distance_class_matrix) =="Solid_Domestics_PCT_quartile"] <- "Solid_Domestics_PCT_quartile_store1"
names(site_demographics_distance_class_matrix)[names(site_demographics_distance_class_matrix) =="Solid_Domestics_AvgInventory"] <- "Solid_Domestics_AvgInventory_store1"
names(site_demographics_distance_class_matrix)[names(site_demographics_distance_class_matrix) =="Solid_Exotics_PCT_quartile"] <- "Solid_Exotics_PCT_quartile_store1"
names(site_demographics_distance_class_matrix)[names(site_demographics_distance_class_matrix) =="Solid_Exotics_AvgInventory"] <- "Solid_Exotics_AvgInventory_store1"
names(site_demographics_distance_class_matrix)[names(site_demographics_distance_class_matrix) =="Vinyl_Plank_PCT_quartile"] <- "Vinyl_Plank_PCT_quartile_store1"
names(site_demographics_distance_class_matrix)[names(site_demographics_distance_class_matrix) =="Vinyl_Plank_AvgInventory"] <- "Vinyl_Plank_AvgInventory_store1"
names(site_demographics_distance_class_matrix)[names(site_demographics_distance_class_matrix) =="Wood_Plank_Tile_PCT_quartile"] <- "Wood_Plank_Tile_PCT_quartile_store1"
names(site_demographics_distance_class_matrix)[names(site_demographics_distance_class_matrix) =="Wood_Plank_Tile_AvgInventory"] <- "Wood_Plank_Tile_AvgInventory_store1"

site_demographics_distance_class_matrix %>% 
  left_join(site_demographic_sales_class_inventory,  by = c('store2' = 'store')) %>% 
  select(store1, DemographicCluster_store1, store2, DemographicCluster, distance,Bamboo_PCT_quartile_store1, Bamboo_PCT_quartile, Bamboo_AvgInventory_store1, Bamboo_AvgInventory,  Cork_Flooring_PCT_quartile_store1, Cork_Flooring_PCT_quartile, Cork_Flooring_AvgInventory_store1, Cork_Flooring_AvgInventory, Engineered_PCT_quartile_store1, Engineered_PCT_quartile, Engineered_AvgInventory_store1, Engineered_AvgInventory, Laminate_PCT_quartile_store1, Laminate_PCT_quartile_store1, Laminate_PCT_quartile, Laminate_AvgInventory_store1, Laminate_AvgInventory, Solid_Domestics_PCT_quartile_store1, Solid_Domestics_PCT_quartile, Solid_Domestics_AvgInventory_store1, Solid_Domestics_AvgInventory, Solid_Exotics_PCT_quartile_store1, Solid_Exotics_PCT_quartile, Solid_Exotics_AvgInventory_store1, Solid_Exotics_AvgInventory, Vinyl_Plank_PCT_quartile_store1, Vinyl_Plank_PCT_quartile, Vinyl_Plank_AvgInventory_store1, Vinyl_Plank_AvgInventory, Wood_Plank_Tile_PCT_quartile_store1, Wood_Plank_Tile_PCT_quartile, Wood_Plank_Tile_AvgInventory_store1, Wood_Plank_Tile_AvgInventory) -> site_demographics_distance_class_matrix
names(site_demographics_distance_class_matrix)[names(site_demographics_distance_class_matrix) =="DemographicCluster"] <- "DemographicCluster_store2"
names(site_demographics_distance_class_matrix)[names(site_demographics_distance_class_matrix) =="Bamboo_PCT_quartile"] <- "Bamboo_PCT_quartile_store2"
names(site_demographics_distance_class_matrix)[names(site_demographics_distance_class_matrix) =="Bamboo_AvgInventory"] <- "Bamboo_AvgInventory_store2"
names(site_demographics_distance_class_matrix)[names(site_demographics_distance_class_matrix) =="Cork_Flooring_PCT_quartile"] <- "Cork_Flooring_PCT_quartile_store2"
names(site_demographics_distance_class_matrix)[names(site_demographics_distance_class_matrix) =="Cork_Flooring_AvgInventory"] <- "Cork_Flooring_AvgInventory_store2"
names(site_demographics_distance_class_matrix)[names(site_demographics_distance_class_matrix) =="Engineered_PCT_quartile"] <- "Engineered_PCT_quartile_store2"
names(site_demographics_distance_class_matrix)[names(site_demographics_distance_class_matrix) =="Engineered_AvgInventory"] <- "Engineered_AvgInventory_store2"
names(site_demographics_distance_class_matrix)[names(site_demographics_distance_class_matrix) =="Laminate_PCT_quartile"] <- "Laminate_PCT_quartile_store2"
names(site_demographics_distance_class_matrix)[names(site_demographics_distance_class_matrix) =="Laminate_AvgInventory"] <- "Laminate_AvgInventory_store2"
names(site_demographics_distance_class_matrix)[names(site_demographics_distance_class_matrix) =="Solid_Domestics_PCT_quartile"] <- "Solid_Domestics_PCT_quartile_store2"
names(site_demographics_distance_class_matrix)[names(site_demographics_distance_class_matrix) =="Solid_Domestics_AvgInventory"] <- "Solid_Domestics_AvgInventory_store2"
names(site_demographics_distance_class_matrix)[names(site_demographics_distance_class_matrix) =="Solid_Exotics_PCT_quartile"] <- "Solid_Exotics_PCT_quartile_store2"
names(site_demographics_distance_class_matrix)[names(site_demographics_distance_class_matrix) =="Solid_Exotics_AvgInventory"] <- "Solid_Exotics_AvgInventory_store2"
names(site_demographics_distance_class_matrix)[names(site_demographics_distance_class_matrix) =="Vinyl_Plank_PCT_quartile"] <- "Vinyl_Plank_PCT_quartile_store2"
names(site_demographics_distance_class_matrix)[names(site_demographics_distance_class_matrix) =="Vinyl_Plank_AvgInventory"] <- "Vinyl_Plank_AvgInventory_store2"
names(site_demographics_distance_class_matrix)[names(site_demographics_distance_class_matrix) =="Wood_Plank_Tile_PCT_quartile"] <- "Wood_Plank_Tile_PCT_quartile_store2"
names(site_demographics_distance_class_matrix)[names(site_demographics_distance_class_matrix) =="Wood_Plank_Tile_AvgInventory"] <- "Wood_Plank_Tile_AvgInventory_store2"


INVENTORY_CHECK_PERCENTAGE = .7 #Check if inventory is atleast 70% different


##########################
###### BEGIN BAMBOO ######
##########################

site_demographics_distance_class_matrix %>% 
  filter(Bamboo_PCT_quartile_store1!=Bamboo_PCT_quartile_store2 & Bamboo_PCT_quartile_store1-Bamboo_PCT_quartile_store2!=1 &Bamboo_PCT_quartile_store1-Bamboo_PCT_quartile_store2!=-1) -> TreViraj_sitesales_bamboo_pct

TreViraj_sitesales_bamboo_pct %>% 
  filter(as.numeric(store1)<as.numeric(store2)) %>% 
  select(store1, DemographicCluster_store1, store2, DemographicCluster_store2, distance, Bamboo_PCT_quartile_store1, Bamboo_PCT_quartile_store2, Bamboo_AvgInventory_store1, Bamboo_AvgInventory_store2)-> TreViraj_sitesales_bamboo_pct

TreViraj_sitesales_bamboo_pct$Bamboo_Inventory_DELTA <- abs(TreViraj_sitesales_bamboo_pct$Bamboo_AvgInventory_store1-TreViraj_sitesales_bamboo_pct$Bamboo_AvgInventory_store2)/ifelse(TreViraj_sitesales_bamboo_pct$Bamboo_AvgInventory_store1 > TreViraj_sitesales_bamboo_pct$Bamboo_AvgInventory_store2, TreViraj_sitesales_bamboo_pct$Bamboo_AvgInventory_store1, TreViraj_sitesales_bamboo_pct$Bamboo_AvgInventory_store2)



TreViraj_sitesales_bamboo_pct$cluster_test <- TreViraj_sitesales_bamboo_pct$DemographicCluster_store1==TreViraj_sitesales_bamboo_pct$DemographicCluster_store2
TreViraj_sitesales_bamboo_pct$inventory_test <- TreViraj_sitesales_bamboo_pct$Bamboo_Inventory_DELTA > INVENTORY_CHECK_PERCENTAGE
TreViraj_sitesales_bamboo_pct %>% 
  filter(TreViraj_sitesales_bamboo_pct$cluster_test == TRUE & TreViraj_sitesales_bamboo_pct$inventory_test == TRUE) -> BAMBOO

#TreViraj_sitesales_bamboo_pct -> BAMBOO

#BAMBOO %>% 
#  left_join(final_site, by=c('store1'='store')) %>% 
#  select(store1, lat, lon, store2, distance, Bamboo_PCT_quartile_store1, Bamboo_PCT_quartile_store2,DemographicCluster_store1,DemographicCluster_store2, cluster_test) -> BAMBOO
#names(BAMBOO)[names(BAMBOO) =="lat"] <- "lat_store1"
#names(BAMBOO)[names(BAMBOO) =="lon"] <- "lon_store1"
#BAMBOO %>% 
#  left_join(final_site, by=c('store1'='store')) %>% 
#  select(store1, lat_store1, lon_store1, store2, lat, lon, distance, Bamboo_PCT_quartile_store1, Bamboo_PCT_quartile_store2,DemographicCluster_store1,DemographicCluster_store2, cluster_test) -> BAMBOO
#names(BAMBOO)[names(BAMBOO) =="lat"] <- "lat_store2"
#names(BAMBOO)[names(BAMBOO) =="lon"] <- "lon_store2"

#write.xlsx(BAMBOO, '/Users/trehall/Lumber Liquidators/Model/BAMBOO.xlsx', row.names=TRUE)

########################
###### END BAMBOO ######
########################


#################################
###### BEGIN CORK FLOORING ######
#################################

site_demographics_distance_class_matrix %>% 
  filter(Cork_Flooring_PCT_quartile_store1!=Cork_Flooring_PCT_quartile_store2 & Cork_Flooring_PCT_quartile_store1-Cork_Flooring_PCT_quartile_store2!=1 &Cork_Flooring_PCT_quartile_store1-Cork_Flooring_PCT_quartile_store2!=-1) -> TreViraj_sitesales_cork_flooring_pct

TreViraj_sitesales_cork_flooring_pct %>% 
  filter(as.numeric(store1)<as.numeric(store2)) %>% 
  select(store1, DemographicCluster_store1, store2, DemographicCluster_store2, distance, Cork_Flooring_PCT_quartile_store1, Cork_Flooring_PCT_quartile_store2, Cork_Flooring_AvgInventory_store1, Cork_Flooring_AvgInventory_store2)-> TreViraj_sitesales_cork_flooring_pct

TreViraj_sitesales_cork_flooring_pct$Cork_Flooring_Inventory_DELTA <- abs(TreViraj_sitesales_cork_flooring_pct$Cork_Flooring_AvgInventory_store1-TreViraj_sitesales_cork_flooring_pct$Cork_Flooring_AvgInventory_store2)/ifelse(TreViraj_sitesales_cork_flooring_pct$Cork_Flooring_AvgInventory_store1 > TreViraj_sitesales_cork_flooring_pct$Cork_Flooring_AvgInventory_store2, TreViraj_sitesales_cork_flooring_pct$Cork_Flooring_AvgInventory_store1, TreViraj_sitesales_cork_flooring_pct$Cork_Flooring_AvgInventory_store2)



TreViraj_sitesales_cork_flooring_pct$cluster_test <- TreViraj_sitesales_cork_flooring_pct$DemographicCluster_store1==TreViraj_sitesales_cork_flooring_pct$DemographicCluster_store2
TreViraj_sitesales_cork_flooring_pct$inventory_test <- TreViraj_sitesales_cork_flooring_pct$Cork_Flooring_Inventory_DELTA > INVENTORY_CHECK_PERCENTAGE
TreViraj_sitesales_cork_flooring_pct %>% 
  filter(TreViraj_sitesales_cork_flooring_pct$cluster_test == TRUE & TreViraj_sitesales_cork_flooring_pct$inventory_test == TRUE) -> CORK

#TreViraj_sitesales_cork_flooring_pct -> CORK

#CORK %>% 
#  left_join(final_site, by=c('store1'='store')) %>% 
#  select(store1, lat, lon, store2, distance, Cork_Flooring_PCT_quartile_store1, Cork_Flooring_PCT_quartile_store2,DemographicCluster_store1,DemographicCluster_store2, cluster_test) -> CORK
#names(CORK)[names(CORK) =="lat"] <- "lat_store1"
#names(CORK)[names(CORK) =="lon"] <- "lon_store1"
#CORK %>% 
#  left_join(final_site, by=c('store1'='store')) %>% 
#  select(store1, lat_store1, lon_store1, store2, lat, lon, distance, Cork_Flooring_PCT_quartile_store1, Cork_Flooring_PCT_quartile_store2,DemographicCluster_store1,DemographicCluster_store2, cluster_test) -> CORK
#names(CORK)[names(CORK) =="lat"] <- "lat_store2"
#names(CORK)[names(CORK) =="lon"] <- "lon_store2"
#write.xlsx(CORK, '/Users/trehall/Lumber Liquidators/Model/CORK.xlsx', row.names=TRUE)

###############################
###### END CORK FLOORING ######
###############################

##############################
###### BEGIN ENGINEERED ######
##############################
site_demographics_distance_class_matrix %>% 
  filter(Engineered_PCT_quartile_store1!=Engineered_PCT_quartile_store2 & Engineered_PCT_quartile_store1-Engineered_PCT_quartile_store2!=1 &Engineered_PCT_quartile_store1-Engineered_PCT_quartile_store2!=-1) -> TreViraj_sitesales_engineered_pct

TreViraj_sitesales_engineered_pct %>% 
  filter(as.numeric(store1)<as.numeric(store2)) %>% 
  select(store1, DemographicCluster_store1, store2, DemographicCluster_store2, distance, Engineered_PCT_quartile_store1, Engineered_PCT_quartile_store2, Engineered_AvgInventory_store1, Engineered_AvgInventory_store2)-> TreViraj_sitesales_engineered_pct

TreViraj_sitesales_engineered_pct$Engineered_Inventory_DELTA <- abs(TreViraj_sitesales_engineered_pct$Engineered_AvgInventory_store1-TreViraj_sitesales_engineered_pct$Engineered_AvgInventory_store2)/ifelse(TreViraj_sitesales_engineered_pct$Engineered_AvgInventory_store1 > TreViraj_sitesales_engineered_pct$Engineered_AvgInventory_store2, TreViraj_sitesales_engineered_pct$Engineered_AvgInventory_store1, TreViraj_sitesales_engineered_pct$Engineered_AvgInventory_store2)



TreViraj_sitesales_engineered_pct$cluster_test <- TreViraj_sitesales_engineered_pct$DemographicCluster_store1==TreViraj_sitesales_engineered_pct$DemographicCluster_store2
TreViraj_sitesales_engineered_pct$inventory_test <- TreViraj_sitesales_engineered_pct$Engineered_Inventory_DELTA > INVENTORY_CHECK_PERCENTAGE
TreViraj_sitesales_engineered_pct %>% 
  filter(TreViraj_sitesales_engineered_pct$cluster_test == TRUE & TreViraj_sitesales_engineered_pct$inventory_test == TRUE) -> ENGINEERED

#TreViraj_sitesales_engineered_pct -> ENGINEERED

#ENGINEERED %>% 
#  left_join(final_site, by=c('store1'='store')) %>% 
#  select(store1, lat, lon, store2, distance, Engineered_PCT_quartile_store1, Engineered_PCT_quartile_store2,DemographicCluster_store1,DemographicCluster_store2, cluster_test) -> ENGINEERED
#names(ENGINEERED)[names(ENGINEERED) =="lat"] <- "lat_store1"
#names(ENGINEERED)[names(ENGINEERED) =="lon"] <- "lon_store1"
#ENGINEERED %>% 
#  left_join(final_site, by=c('store1'='store')) %>% 
#  select(store1, lat_store1, lon_store1, store2, lat, lon, distance, Engineered_PCT_quartile_store1, Engineered_PCT_quartile_store2,DemographicCluster_store1,DemographicCluster_store2, cluster_test) -> ENGINEERED
#names(ENGINEERED)[names(ENGINEERED) =="lat"] <- "lat_store2"
#names(ENGINEERED)[names(ENGINEERED) =="lon"] <- "lon_store2"
#write.xlsx(ENGINEERED, '/Users/trehall/Lumber Liquidators/Model/ENGINEERED.xlsx', row.names=TRUE)

############################
###### END ENGINEERED ######
############################

############################
###### BEGIN LAMINATE ######
############################
site_demographics_distance_class_matrix %>% 
  filter(Laminate_PCT_quartile_store1!=Laminate_PCT_quartile_store2 & Laminate_PCT_quartile_store1-Laminate_PCT_quartile_store2!=1 &Laminate_PCT_quartile_store1-Laminate_PCT_quartile_store2!=-1) -> TreViraj_sitesales_laminate_pct

TreViraj_sitesales_laminate_pct %>% 
  filter(as.numeric(store1)<as.numeric(store2)) %>% 
  select(store1, DemographicCluster_store1, store2, DemographicCluster_store2, distance, Laminate_PCT_quartile_store1, Laminate_PCT_quartile_store2, Laminate_AvgInventory_store1, Laminate_AvgInventory_store2)-> TreViraj_sitesales_laminate_pct

TreViraj_sitesales_laminate_pct$Laminate_Inventory_DELTA <- abs(TreViraj_sitesales_laminate_pct$Laminate_AvgInventory_store1-TreViraj_sitesales_laminate_pct$Laminate_AvgInventory_store2)/ifelse(TreViraj_sitesales_laminate_pct$Laminate_AvgInventory_store1 > TreViraj_sitesales_laminate_pct$Laminate_AvgInventory_store2, TreViraj_sitesales_laminate_pct$Laminate_AvgInventory_store1, TreViraj_sitesales_laminate_pct$Laminate_AvgInventory_store2)



TreViraj_sitesales_laminate_pct$cluster_test <- TreViraj_sitesales_laminate_pct$DemographicCluster_store1==TreViraj_sitesales_laminate_pct$DemographicCluster_store2
TreViraj_sitesales_laminate_pct$inventory_test <- TreViraj_sitesales_laminate_pct$Laminate_Inventory_DELTA > INVENTORY_CHECK_PERCENTAGE
TreViraj_sitesales_laminate_pct %>% 
  filter(TreViraj_sitesales_laminate_pct$cluster_test == TRUE & TreViraj_sitesales_laminate_pct$inventory_test == TRUE) -> LAMINATE

#TreViraj_sitesales_laminate_pct %>% 
#  left_join(final_site, by=c('store1'='store')) %>% 
#  select(store1, lat, lon, store2, distance, Laminate_PCT_quartile_store1, Laminate_PCT_quartile_store2,DemographicCluster_store1,DemographicCluster_store2, cluster_test) -> LAMINATE
#names(LAMINATE)[names(LAMINATE) =="lat"] <- "lat_store1"
#names(LAMINATE)[names(LAMINATE) =="lon"] <- "lon_store1"
#LAMINATE %>% 
#  left_join(final_site, by=c('store1'='store')) %>% 
#  select(store1, lat_store1, lon_store1, store2, lat, lon, distance, Laminate_PCT_quartile_store1, Laminate_PCT_quartile_store2,DemographicCluster_store1,DemographicCluster_store2, cluster_test) -> LAMINATE
#names(LAMINATE)[names(LAMINATE) =="lat"] <- "lat_store2"
#names(LAMINATE)[names(LAMINATE) =="lon"] <- "lon_store2"

#write.xlsx(LAMINATE, '/Users/trehall/Lumber Liquidators/Model/LAMINATE.xlsx', row.names=TRUE)

##########################
###### END LAMINATE ######
##########################

###################################
###### BEGIN SOLID DOMESTICS ######
###################################
site_demographics_distance_class_matrix %>% 
  filter(Solid_Domestics_PCT_quartile_store1!=Solid_Domestics_PCT_quartile_store2 & Solid_Domestics_PCT_quartile_store1-Solid_Domestics_PCT_quartile_store2!=1 &Solid_Domestics_PCT_quartile_store1-Solid_Domestics_PCT_quartile_store2!=-1) -> TreViraj_sitesales_solid_domestics_pct

TreViraj_sitesales_solid_domestics_pct %>% 
  filter(as.numeric(store1)<as.numeric(store2)) %>% 
  select(store1, DemographicCluster_store1, store2, DemographicCluster_store2, distance, Solid_Domestics_PCT_quartile_store1, Solid_Domestics_PCT_quartile_store2, Solid_Domestics_AvgInventory_store1, Solid_Domestics_AvgInventory_store2)-> TreViraj_sitesales_solid_domestics_pct

TreViraj_sitesales_solid_domestics_pct$Solid_Domestics_Inventory_DELTA <- abs(TreViraj_sitesales_solid_domestics_pct$Solid_Domestics_AvgInventory_store1-TreViraj_sitesales_solid_domestics_pct$Solid_Domestics_AvgInventory_store2)/ifelse(TreViraj_sitesales_solid_domestics_pct$Solid_Domestics_AvgInventory_store1 > TreViraj_sitesales_solid_domestics_pct$Solid_Domestics_AvgInventory_store2, TreViraj_sitesales_solid_domestics_pct$Solid_Domestics_AvgInventory_store1, TreViraj_sitesales_solid_domestics_pct$Solid_Domestics_AvgInventory_store2)



TreViraj_sitesales_solid_domestics_pct$cluster_test <- TreViraj_sitesales_solid_domestics_pct$DemographicCluster_store1==TreViraj_sitesales_solid_domestics_pct$DemographicCluster_store2
TreViraj_sitesales_solid_domestics_pct$inventory_test <- TreViraj_sitesales_solid_domestics_pct$Solid_Domestics_Inventory_DELTA > INVENTORY_CHECK_PERCENTAGE
TreViraj_sitesales_solid_domestics_pct %>% 
  filter(TreViraj_sitesales_solid_domestics_pct$cluster_test == TRUE & TreViraj_sitesales_solid_domestics_pct$inventory_test == TRUE) -> DOMESTICS 

#TreViraj_sitesales_solid_domestics_pct -> DOMESTICS

#DOMESTICS %>% 
#  left_join(final_site, by=c('store1'='store')) %>% 
#  select(store1, lat, lon, store2, distance, Solid_Domestics_PCT_quartile_store1, Solid_Domestics_PCT_quartile_store2,DemographicCluster_store1,DemographicCluster_store2, cluster_test) -> DOMESTICS
#names(DOMESTICS)[names(DOMESTICS) =="lat"] <- "lat_store1"
#names(DOMESTICS)[names(DOMESTICS) =="lon"] <- "lon_store1"
#DOMESTICS %>% 
#  left_join(final_site, by=c('store1'='store')) %>% 
#  select(store1, lat_store1, lon_store1, store2, lat, lon, distance, Solid_Domestics_PCT_quartile_store1, Solid_Domestics_PCT_quartile_store2,DemographicCluster_store1,DemographicCluster_store2, cluster_test) -> DOMESTICS
#names(DOMESTICS)[names(DOMESTICS) =="lat"] <- "lat_store2"
#names(DOMESTICS)[names(DOMESTICS) =="lon"] <- "lon_store2"

#write.xlsx(DOMESTICS, '/Users/trehall/Lumber Liquidators/Model/DOMESTICS.xlsx', row.names=TRUE)

#################################
###### END SOLID DOMESTICS ######
#################################

#################################
###### BEGIN SOLID EXOTICS ######
#################################
site_demographics_distance_class_matrix %>% 
  filter(Solid_Exotics_PCT_quartile_store1!=Solid_Exotics_PCT_quartile_store2 & Solid_Exotics_PCT_quartile_store1-Solid_Exotics_PCT_quartile_store2!=1 &Solid_Exotics_PCT_quartile_store1-Solid_Exotics_PCT_quartile_store2!=-1) -> TreViraj_sitesales_solid_exotics_pct

TreViraj_sitesales_solid_exotics_pct %>% 
  filter(as.numeric(store1)<as.numeric(store2)) %>% 
  select(store1, DemographicCluster_store1, store2, DemographicCluster_store2, distance, Solid_Exotics_PCT_quartile_store1, Solid_Exotics_PCT_quartile_store2, Solid_Exotics_AvgInventory_store1, Solid_Exotics_AvgInventory_store2)-> TreViraj_sitesales_solid_exotics_pct

TreViraj_sitesales_solid_exotics_pct$Solid_Exotics_Inventory_DELTA <- abs(TreViraj_sitesales_solid_exotics_pct$Solid_Exotics_AvgInventory_store1-TreViraj_sitesales_solid_exotics_pct$Solid_Exotics_AvgInventory_store2)/ifelse(TreViraj_sitesales_solid_exotics_pct$Solid_Exotics_AvgInventory_store1 > TreViraj_sitesales_solid_exotics_pct$Solid_Exotics_AvgInventory_store2, TreViraj_sitesales_solid_exotics_pct$Solid_Exotics_AvgInventory_store1, TreViraj_sitesales_solid_exotics_pct$Solid_Exotics_AvgInventory_store2)



TreViraj_sitesales_solid_exotics_pct$cluster_test <- TreViraj_sitesales_solid_exotics_pct$DemographicCluster_store1==TreViraj_sitesales_solid_exotics_pct$DemographicCluster_store2
TreViraj_sitesales_solid_exotics_pct$inventory_test <- TreViraj_sitesales_solid_exotics_pct$Solid_Exotics_Inventory_DELTA > INVENTORY_CHECK_PERCENTAGE
TreViraj_sitesales_solid_exotics_pct %>% 
  filter(TreViraj_sitesales_solid_exotics_pct$cluster_test == TRUE & TreViraj_sitesales_solid_exotics_pct$inventory_test == TRUE) -> EXOTICS

#TreViraj_sitesales_solid_exotics_pct -> EXOTICS

#EXOTICS %>% 
#  left_join(final_site, by=c('store1'='store')) %>% 
#  select(store1, lat, lon, store2, distance, Solid_Exotics_PCT_quartile_store1, Solid_Exotics_PCT_quartile_store2,DemographicCluster_store1,DemographicCluster_store2, cluster_test) -> EXOTICS
#names(EXOTICS)[names(EXOTICS) =="lat"] <- "lat_store1"
#names(EXOTICS)[names(EXOTICS) =="lon"] <- "lon_store1"
#EXOTICS %>% 
#  left_join(final_site, by=c('store1'='store')) %>% 
#  select(store1, lat_store1, lon_store1, store2, lat, lon, distance, Solid_Exotics_PCT_quartile_store1, Solid_Exotics_PCT_quartile_store2,DemographicCluster_store1,DemographicCluster_store2, cluster_test) -> EXOTICS
#names(EXOTICS)[names(EXOTICS) =="lat"] <- "lat_store2"
#names(EXOTICS)[names(EXOTICS) =="lon"] <- "lon_store2"

#write.xlsx(EXOTICS, '/Users/trehall/Lumber Liquidators/Model/EXOTICS.xlsx', row.names=TRUE)

###############################
###### END SOLID EXOTICS ######
###############################

###############################
###### BEGIN VINYL PLANK ######
###############################
site_demographics_distance_class_matrix %>% 
  filter(Vinyl_Plank_PCT_quartile_store1!=Vinyl_Plank_PCT_quartile_store2 & Vinyl_Plank_PCT_quartile_store1-Vinyl_Plank_PCT_quartile_store2!=1 &Vinyl_Plank_PCT_quartile_store1-Vinyl_Plank_PCT_quartile_store2!=-1) -> TreViraj_sitesales_vinyl_plank_pct

TreViraj_sitesales_vinyl_plank_pct %>% 
  filter(as.numeric(store1)<as.numeric(store2)) %>% 
  select(store1, DemographicCluster_store1, store2, DemographicCluster_store2, distance, Vinyl_Plank_PCT_quartile_store1, Vinyl_Plank_PCT_quartile_store2, Vinyl_Plank_AvgInventory_store1, Vinyl_Plank_AvgInventory_store2)-> TreViraj_sitesales_vinyl_plank_pct

TreViraj_sitesales_vinyl_plank_pct$Vinyl_Plank_Inventory_DELTA <- abs(TreViraj_sitesales_vinyl_plank_pct$Vinyl_Plank_AvgInventory_store1-TreViraj_sitesales_vinyl_plank_pct$Vinyl_Plank_AvgInventory_store2)/ifelse(TreViraj_sitesales_vinyl_plank_pct$Vinyl_Plank_AvgInventory_store1 > TreViraj_sitesales_vinyl_plank_pct$Vinyl_Plank_AvgInventory_store2, TreViraj_sitesales_vinyl_plank_pct$Vinyl_Plank_AvgInventory_store1, TreViraj_sitesales_vinyl_plank_pct$Vinyl_Plank_AvgInventory_store2)



TreViraj_sitesales_vinyl_plank_pct$cluster_test <- TreViraj_sitesales_vinyl_plank_pct$DemographicCluster_store1==TreViraj_sitesales_vinyl_plank_pct$DemographicCluster_store2
TreViraj_sitesales_vinyl_plank_pct$inventory_test <- TreViraj_sitesales_vinyl_plank_pct$Vinyl_Plank_Inventory_DELTA > INVENTORY_CHECK_PERCENTAGE
TreViraj_sitesales_vinyl_plank_pct %>% 
  filter(TreViraj_sitesales_vinyl_plank_pct$cluster_test == TRUE & TreViraj_sitesales_vinyl_plank_pct$inventory_test == TRUE)  -> VINYL_PLANK

#TreViraj_sitesales_vinyl_plank_pct -> VINYL_PLANK

#VINYL_PLANK %>% 
#  left_join(final_site, by=c('store1'='store')) %>% 
#  select(store1, lat, lon, store2, distance, Vinyl_Plank_PCT_quartile_store1, Vinyl_Plank_PCT_quartile_store2,DemographicCluster_store1,DemographicCluster_store2, cluster_test) -> VINYL_PLANK
#names(VINYL_PLANK)[names(VINYL_PLANK) =="lat"] <- "lat_store1"
#names(VINYL_PLANK)[names(VINYL_PLANK) =="lon"] <- "lon_store1"
#VINYL_PLANK %>% 
#  left_join(final_site, by=c('store1'='store')) %>% 
#  select(store1, lat_store1, lon_store1, store2, lat, lon, distance, Vinyl_Plank_PCT_quartile_store1, Vinyl_Plank_PCT_quartile_store2,DemographicCluster_store1,DemographicCluster_store2, cluster_test) -> VINYL_PLANK
#names(VINYL_PLANK)[names(VINYL_PLANK) =="lat"] <- "lat_store2"
#names(VINYL_PLANK)[names(VINYL_PLANK) =="lon"] <- "lon_store2"

#write.xlsx(VINYL_PLANK, '/Users/trehall/Lumber Liquidators/Model/VINYL_PLANK.xls', row.names=TRUE)


#############################
###### END VINYL PLANK ######
#############################

####################################
###### BEGIN WOOD PLANK TITLE ######
####################################
site_demographics_distance_class_matrix %>% 
  filter(Wood_Plank_Tile_PCT_quartile_store1!=Wood_Plank_Tile_PCT_quartile_store2 & Wood_Plank_Tile_PCT_quartile_store1-Wood_Plank_Tile_PCT_quartile_store2!=1 &Wood_Plank_Tile_PCT_quartile_store1-Wood_Plank_Tile_PCT_quartile_store2!=-1) -> TreViraj_sitesales_wood_plank_tile_pct

TreViraj_sitesales_wood_plank_tile_pct %>% 
  filter(as.numeric(store1)<as.numeric(store2)) %>% 
  select(store1, DemographicCluster_store1, store2, DemographicCluster_store2, distance, Wood_Plank_Tile_PCT_quartile_store1, Wood_Plank_Tile_AvgInventory_store1, Wood_Plank_Tile_PCT_quartile_store2, Wood_Plank_Tile_AvgInventory_store1, Wood_Plank_Tile_AvgInventory_store2)-> TreViraj_sitesales_wood_plank_tile_pct

TreViraj_sitesales_wood_plank_tile_pct$Wood_Plank_Tile_Inventory_DELTA <- abs(TreViraj_sitesales_wood_plank_tile_pct$Wood_Plank_Tile_AvgInventory_store1-TreViraj_sitesales_wood_plank_tile_pct$Wood_Plank_Tile_AvgInventory_store2)/ifelse(TreViraj_sitesales_wood_plank_tile_pct$Wood_Plank_Tile_AvgInventory_store1 > TreViraj_sitesales_wood_plank_tile_pct$Wood_Plank_Tile_AvgInventory_store2, TreViraj_sitesales_wood_plank_tile_pct$Wood_Plank_Tile_AvgInventory_store1, TreViraj_sitesales_wood_plank_tile_pct$Wood_Plank_Tile_AvgInventory_store2)



TreViraj_sitesales_wood_plank_tile_pct$cluster_test <- TreViraj_sitesales_wood_plank_tile_pct$DemographicCluster_store1==TreViraj_sitesales_wood_plank_tile_pct$DemographicCluster_store2
TreViraj_sitesales_wood_plank_tile_pct$inventory_test <- TreViraj_sitesales_wood_plank_tile_pct$Wood_Plank_Tile_Inventory_DELTA > INVENTORY_CHECK_PERCENTAGE
TreViraj_sitesales_wood_plank_tile_pct %>% 
  filter(TreViraj_sitesales_wood_plank_tile_pct$cluster_test == TRUE & TreViraj_sitesales_wood_plank_tile_pct$inventory_test == TRUE) -> WOOD_PLANK_TILE

#TreViraj_sitesales_wood_plank_tile_pct -> WOOD_PLANK_TILE


#WOOD_PLANK_TILE %>% 
#  left_join(final_site, by=c('store1'='store')) %>% 
#  select(store1, lat, lon, store2, distance, Wood_Plank_Tile_PCT_quartile_store1, Wood_Plank_Tile_PCT_quartile_store2,DemographicCluster_store1,DemographicCluster_store2, cluster_test) -> WOOD_PLANK_TILE
#names(WOOD_PLANK_TILE)[names(WOOD_PLANK_TILE) =="lat"] <- "lat_store1"
#names(WOOD_PLANK_TILE)[names(WOOD_PLANK_TILE) =="lon"] <- "lon_store1"
#WOOD_PLANK_TILE %>% 
#  left_join(final_site, by=c('store1'='store')) %>% 
#  select(store1, lat_store1, lon_store1, store2, lat, lon, distance, Wood_Plank_Tile_PCT_quartile_store1, Wood_Plank_Tile_PCT_quartile_store2,DemographicCluster_store1,DemographicCluster_store2, cluster_test) -> WOOD_PLANK_TILE
#names(WOOD_PLANK_TILE)[names(WOOD_PLANK_TILE) =="lat"] <- "lat_store2"
#names(WOOD_PLANK_TILE)[names(WOOD_PLANK_TILE) =="lon"] <- "lon_store2"

#write.xlsx(WOOD_PLANK_TILE, '/Users/trehall/Lumber Liquidators/Model/WOOD_PLANK_TILE.xlsx', row.names=TRUE)

##################################
###### END WOOD PLANK TITLE ######
##################################









cf <- CORK

final_site$store <- as.numeric(final_site$store)
cf$store1 <- as.numeric(cf$store1)

cf <- cf %>% 
  left_join(final_site, by=c('store1'='store')) %>% 
  select(store1, lat, lon, store2, distance, Cork_Flooring, Cork_Flooring_PCT, Cork_Flooring_PCT_quartile_store1, Cork_Flooring_PCT_quartile_store2, DemographicCluster_store1, DemographicCluster_store2, cluster_test)
names(cf)[names(cf) =="lat"] <- "lat_store1" 
names(cf)[names(cf) =="lon"] <- "lon_store1" 
names(cf)[names(cf) =="Cork_Flooring"] <- "Cork_Flooring_SALES_store1" 
names(cf)[names(cf) =="Cork_Flooring_PCT"] <- "Cork_Flooring_PCT_store1" 
cf$store2 <- as.numeric(cf$store2)
cf <- cf %>% 
  left_join(final_site, by=c('store2'='store')) %>% 
  select(store1, lat_store1, lat_store1, store2, lat, lon, distance, Cork_Flooring_SALES_store1, Cork_Flooring_PCT_store1, Cork_Flooring_PCT_quartile_store1, Cork_Flooring, Cork_Flooring_PCT, Cork_Flooring_PCT_quartile_store2, DemographicCluster_store1, DemographicCluster_store2, cluster_test)
names(cf)[names(cf) =="lat"] <- "lat_store2" 
names(cf)[names(cf) =="lon"] <- "lon_store2" 
names(cf)[names(cf) =="Cork_Flooring"] <- "Cork_Flooring_SALES_store2" 
names(cf)[names(cf) =="Cork_Flooring_PCT"] <- "Cork_Flooring_PCT_store2" 

cf$SALES_OPPORTUNITY <- abs(cf$Cork_Flooring_SALES_store1-cf$Cork_Flooring_SALES_store2)


###

vin <- VINYL_PLANK

final_site$store <- as.numeric(final_site$store)
vin$store1 <- as.numeric(vin$store1)

vin <- vin %>% 
  left_join(final_site, by=c('store1'='store')) %>% 
  select(store1, lat, lon, store2, distance, Vinyl_Plank, Vinyl_Plank_PCT, Vinyl_Plank_PCT_quartile_store1, Vinyl_Plank_PCT_quartile_store2, DemographicCluster_store1, DemographicCluster_store2, cluster_test)
names(vin)[names(vin) =="lat"] <- "lat_store1" 
names(vin)[names(vin) =="lon"] <- "lon_store1" 
names(vin)[names(vin) =="Vinyl_Plank"] <- "Vinyl_Plank_SALES_store1" 
names(vin)[names(vin) =="Vinyl_Plank_PCT"] <- "Vinyl_Plank_PCT_store1" 
vin$store2 <- as.numeric(vin$store2)
vin <- vin %>% 
  left_join(final_site, by=c('store2'='store')) %>% 
  select(store1, lat_store1, lon_store1, store2, lat, lon, distance, Vinyl_Plank_SALES_store1, Vinyl_Plank_PCT_store1, Vinyl_Plank_PCT_quartile_store1, Vinyl_Plank, Vinyl_Plank_PCT, Vinyl_Plank_PCT_quartile_store2, DemographicCluster_store1, DemographicCluster_store2, cluster_test)
names(vin)[names(vin) =="lat"] <- "lat_store2" 
names(vin)[names(vin) =="lon"] <- "lon_store2" 
names(vin)[names(vin) =="Vinyl_Plank"] <- "Vinyl_Plank_SALES_store2" 
names(vin)[names(vin) =="Vinyl_Plank_PCT"] <- "Vinyl_Plank_PCT_store2" 

vin$SALES_OPPORTUNITY <- abs(vin$Vinyl_Plank_SALES_store1-vin$Vinyl_Plank_SALES_store2)






###
bam <-BAMBOO

final_site$store <- as.numeric(final_site$store)
bam$store1 <- as.numeric(bam$store1)

bam <- bam %>% 
  left_join(final_site, by=c('store1'='store')) %>% 
  select(store1, lat, lon, store2, distance, Bamboo, Bamboo_PCT, Bamboo_PCT_quartile_store1, Bamboo_PCT_quartile_store2, DemographicCluster_store1, DemographicCluster_store2, Bamboo_AvgInventory_store1, Bamboo_AvgInventory_store2, cluster_test, inventory_test)
names(bam)[names(bam) =="lat"] <- "lat_store1" 
names(bam)[names(bam) =="lon"] <- "lon_store1"
names(bam)[names(bam) =="Bamboo"] <- "Bamboo_SALES_store1" 
names(bam)[names(bam) =="Bamboo_PCT"] <- "Bamboo_PCT_store1" 
bam$store2 <- as.numeric(bam$store2)
bam <- bam %>% 
  left_join(final_site, by=c('store2'='store')) %>% 
  select(store1, lat_store1, lon_store1, store2, lat, lon, distance, Bamboo_SALES_store1, Bamboo_PCT_store1, Bamboo_PCT_quartile_store1, Bamboo, Bamboo_PCT, Bamboo_PCT_quartile_store2, DemographicCluster_store1, DemographicCluster_store2, Bamboo_AvgInventory_store1, Bamboo_AvgInventory_store2, cluster_test, inventory_test)
names(bam)[names(bam) =="lat"] <- "lat_store2" 
names(bam)[names(bam) =="lon"] <- "lon_store2" 
names(bam)[names(bam) =="Bamboo"] <- "Bamboo_SALES_store2" 
names(bam)[names(bam) =="Bamboo_PCT"] <- "Bamboo_PCT_store2" 

bam <-bam %>% 
  left_join(AVGINV, by=c('store1'='store')) %>% 
  select(store1, lat_store1, lon_store1, store2, lat_store2, lon_store2, distance, Bamboo_SALES_store1, Bamboo_PCT_store1, Bamboo_PCT_quartile_store1, Bamboo_SALES_store2, Bamboo_PCT_store2, Bamboo_PCT_quartile_store2, DemographicCluster_store1, DemographicCluster_store2, Bamboo_AvgInventory_store1, Bamboo_AvgInventory_store2, cluster_test, inventory_test, Bamboo_INV)

bam$SALES_OPPORTUNITY <- abs(bam$Bamboo_SALES_store1-bam$Bamboo_SALES_store2)

bam$INV_OPPORTUNITY_COST <- abs(bam$Bamboo_AvgInventory_store1-bam$Bamboo_AvgInventory_store2)*bam$Bamboo_INV
bam$SALES_LIFT  <- bam$SALES_OPPORTUNITY-bam$INV_OPPORTUNITY_COST





###

eng <- ENGINEERED

final_site$store <- as.numeric(final_site$store)
eng$store1 <- as.numeric(eng$store1)

eng <- eng %>% 
  left_join(final_site, by=c('store1'='store')) %>% 
  select(store1, lat, lon, store2, distance, Engineered, Engineered_PCT, Engineered_PCT_quartile_store1, Engineered_PCT_quartile_store2, DemographicCluster_store1, DemographicCluster_store2, Engineered_AvgInventory_store1, Engineered_AvgInventory_store2, cluster_test, inventory_test)
names(eng)[names(eng) =="lat"] <- "lat_store1" 
names(eng)[names(eng) =="lon"] <- "lon_store1" 
names(eng)[names(eng) =="Engineered"] <- "Engineered_SALES_store1" 
names(eng)[names(eng) =="Engineered_PCT"] <- "Engineered_PCT_store1" 
eng$store2 <- as.numeric(eng$store2)
eng <- eng %>% 
  left_join(final_site, by=c('store2'='store')) %>% 
  select(store1, lat_store1, lon_store1, store2, lat, lon, distance, Engineered_SALES_store1, Engineered_PCT_store1, Engineered_PCT_quartile_store1, Engineered, Engineered_PCT, Engineered_PCT_quartile_store2, DemographicCluster_store1, DemographicCluster_store2, Engineered_AvgInventory_store1, Engineered_AvgInventory_store2, cluster_test, inventory_test)
names(eng)[names(eng) =="lat"] <- "lat_store2" 
names(eng)[names(eng) =="lon"] <- "lon_store2" 
names(eng)[names(eng) =="Engineered"] <- "Engineered_SALES_store2" 
names(eng)[names(eng) =="Engineered_PCT"] <- "Engineered_PCT_store2" 

eng <- eng %>% 
  left_join(AVGINV, by=c('store1'='store')) %>% 
  select(store1, lat_store1, lon_store1, store2, lat_store2, lon_store2, distance, Engineered_SALES_store1, Engineered_PCT_store2, Engineered_PCT_quartile_store1, Engineered_SALES_store2, Engineered_PCT_store1, Engineered_PCT_quartile_store2, DemographicCluster_store1, DemographicCluster_store2, Engineered_AvgInventory_store1, Engineered_AvgInventory_store2, cluster_test, inventory_test, Engineered_INV)

eng$SALES_OPPORTUNITY <- abs(eng$Engineered_SALES_store1-eng$Engineered_SALES_store2)

eng <- eng %>%
  mutate(Engineered_INV = if_else(is.na(Engineered_INV), 1.871429, Engineered_INV))

eng$INV_OPPORTUNITY_COST <- abs(eng$Engineered_AvgInventory_store1-eng$Engineered_AvgInventory_store2)*eng$Engineered_INV
eng$SALES_LIFT  <- eng$SALES_OPPORTUNITY-eng$INV_OPPORTUNITY_COST



###

lam <- LAMINATE

final_site$store <- as.numeric(final_site$store)
lam$store1 <- as.numeric(lam$store1)

lam <- lam %>% 
  left_join(final_site, by=c('store1'='store')) %>% 
  select(store1, lat, lon, store2, distance, Laminate, Laminate_PCT, Laminate_PCT_quartile_store1, Laminate_PCT_quartile_store2, DemographicCluster_store1, DemographicCluster_store2, Laminate_AvgInventory_store1, Laminate_AvgInventory_store2, cluster_test, inventory_test)
names(lam)[names(lam) =="lat"] <- "lat_store1" 
names(lam)[names(lam) =="lon"] <- "lon_store1" 
names(lam)[names(lam) =="Laminate"] <- "Laminate_SALES_store1" 
names(lam)[names(lam) =="Laminate_PCT"] <- "Laminate_PCT_store1" 
lam$store2 <- as.numeric(lam$store2)
lam <- lam %>% 
  left_join(final_site, by=c('store2'='store')) %>% 
  select(store1, lat_store1, lon_store1, store2, lat, lon, distance, Laminate_SALES_store1, Laminate_PCT_store1, Laminate_PCT_quartile_store1, Laminate, Laminate_PCT, Laminate_PCT_quartile_store2, DemographicCluster_store1, DemographicCluster_store2, Laminate_AvgInventory_store1, Laminate_AvgInventory_store2, cluster_test, inventory_test)
names(lam)[names(lam) =="lat"] <- "lat_store2" 
names(lam)[names(lam) =="lon"] <- "lon_store2" 
names(lam)[names(lam) =="Laminate"] <- "Laminate_SALES_store2" 
names(lam)[names(lam) =="Laminate_PCT"] <- "Laminate_PCT_store2" 

lam <- lam %>% 
  left_join(AVGINV, by=c('store1'='store')) %>% 
  select(store1, lat_store1, lon_store1, store2, lat_store2, lon_store2, distance, Laminate_SALES_store1, Laminate_PCT_store2, Laminate_PCT_quartile_store1, Laminate_SALES_store2, Laminate_PCT_store1, Laminate_PCT_quartile_store2, DemographicCluster_store1, DemographicCluster_store2, Laminate_AvgInventory_store1, Laminate_AvgInventory_store2, cluster_test, inventory_test, Laminate_INV)

lam$SALES_OPPORTUNITY <- abs(lam$Laminate_SALES_store1-lam$Laminate_SALES_store2)
lam$INV_OPPORTUNITY_COST <- abs(lam$Laminate_AvgInventory_store1-lam$Laminate_AvgInventory_store2)*lam$Laminate_INV
lam$SALES_LIFT  <- lam$SALES_OPPORTUNITY-lam$INV_OPPORTUNITY_COST

###

se <- EXOTICS

final_site$store <- as.numeric(final_site$store)
se$store1 <- as.numeric(se$store1)

se <- se %>% 
  left_join(final_site, by=c('store1'='store')) %>% 
  select(store1, lat, lon, store2, distance, Solid_Exotics, Solid_Exotics_PCT, Solid_Exotics_PCT_quartile_store1, Solid_Exotics_PCT_quartile_store2, DemographicCluster_store1, DemographicCluster_store2, Solid_Exotics_AvgInventory_store1, Solid_Exotics_AvgInventory_store2, cluster_test, inventory_test)
names(se)[names(se) =="lat"] <- "lat_store1" 
names(se)[names(se) =="lon"] <- "lon_store1" 
names(se)[names(se) =="Solid_Exotics"] <- "Solid_Exotics_SALES_store1" 
names(se)[names(se) =="Solid_Exotics_PCT"] <- "Solid_Exotics_PCT_store1" 
se$store2 <- as.numeric(se$store2)
se <- se %>% 
  left_join(final_site, by=c('store2'='store')) %>% 
  select(store1, lat_store1, lon_store1, store2, lat, lon, distance, Solid_Exotics_SALES_store1, Solid_Exotics_PCT_store1, Solid_Exotics_PCT_quartile_store1, Solid_Exotics, Solid_Exotics_PCT, Solid_Exotics_PCT_quartile_store2, DemographicCluster_store1, DemographicCluster_store2, Solid_Exotics_AvgInventory_store1, Solid_Exotics_AvgInventory_store2, cluster_test, inventory_test)
names(se)[names(se) =="lat"] <- "lat_store2" 
names(se)[names(se) =="lon"] <- "lon_store2" 
names(se)[names(se) =="Solid_Exotics"] <- "Solid_Exotics_SALES_store2" 
names(se)[names(se) =="Solid_Exotics_PCT"] <- "Solid_Exotics_PCT_store2" 
se <- se %>% 
  left_join(AVGINV, by=c('store1'='store')) %>% 
  select(store1, lat_store1, lon_store1, store2, lat_store2, lon_store2, distance, Solid_Exotics_SALES_store1, Solid_Exotics_PCT_store2, Solid_Exotics_PCT_quartile_store1, Solid_Exotics_SALES_store2, Solid_Exotics_PCT_store1, Solid_Exotics_PCT_quartile_store2, DemographicCluster_store1, DemographicCluster_store2, Solid_Exotics_AvgInventory_store1, Solid_Exotics_AvgInventory_store2, cluster_test, inventory_test, Solid_Exotics_INV)

se$SALES_OPPORTUNITY <- abs(se$Solid_Exotics_SALES_store1-se$Solid_Exotics_SALES_store2)
se$INV_OPPORTUNITY_COST <- abs(se$Solid_Exotics_AvgInventory_store1-se$Solid_Exotics_AvgInventory_store2)*se$Solid_Exotics_INV
se$SALES_LIFT  <- se$SALES_OPPORTUNITY-se$INV_OPPORTUNITY_COST

###


sd <- DOMESTICS

final_site$store <- as.numeric(final_site$store)
sd$store1 <- as.numeric(sd$store1)

sd <- sd %>% 
  left_join(final_site, by=c('store1'='store')) %>% 
  select(store1, lat, lon, store2, distance, Solid_Domestics, Solid_Domestics_PCT, Solid_Domestics_PCT_quartile_store1, Solid_Domestics_PCT_quartile_store2, DemographicCluster_store1, DemographicCluster_store2, Solid_Domestics_AvgInventory_store1, Solid_Domestics_AvgInventory_store2, cluster_test, inventory_test)
names(sd)[names(sd) =="lat"] <- "lat_store1" 
names(sd)[names(sd) =="lon"] <- "lon_store1" 
names(sd)[names(sd) =="Solid_Domestics"] <- "Solid_Domestics_SALES_store1" 
names(sd)[names(sd) =="Solid_Domestics_PCT"] <- "Solid_Domestics_PCT_store1" 
sd$store2 <- as.numeric(sd$store2)
sd <- sd %>% 
  left_join(final_site, by=c('store2'='store')) %>% 
  select(store1, lat_store1, lon_store1, store2, lat, lon, distance, Solid_Domestics_SALES_store1, Solid_Domestics_PCT_store1, Solid_Domestics_PCT_quartile_store1, Solid_Domestics, Solid_Domestics_PCT, Solid_Domestics_PCT_quartile_store2, DemographicCluster_store1, DemographicCluster_store2, Solid_Domestics_AvgInventory_store1, Solid_Domestics_AvgInventory_store2, cluster_test, inventory_test)
names(sd)[names(sd) =="lat"] <- "lat_store2" 
names(sd)[names(sd) =="lon"] <- "lon_store2" 
names(sd)[names(sd) =="Solid_Domestics"] <- "Solid_Domestics_SALES_store2" 
names(sd)[names(sd) =="Solid_Domestics_PCT"] <- "Solid_Domestics_PCT_store2" 
sd <- sd %>% 
  left_join(AVGINV, by=c('store1'='store')) %>% 
  select(store1, lat_store1, lon_store1, store2, lat_store2, lon_store2, distance, Solid_Domestics_SALES_store1, Solid_Domestics_PCT_store2, Solid_Domestics_PCT_quartile_store1, Solid_Domestics_SALES_store2, Solid_Domestics_PCT_store1, Solid_Domestics_PCT_quartile_store2, DemographicCluster_store1, DemographicCluster_store2, Solid_Domestics_AvgInventory_store1, Solid_Domestics_AvgInventory_store2, cluster_test, inventory_test, Solid_Domestics_INV)

sd$SALES_OPPORTUNITY <- abs(sd$Solid_Domestics_SALES_store1-sd$Solid_Domestics_SALES_store2)
sd$INV_OPPORTUNITY_COST <- abs(sd$Solid_Domestics_AvgInventory_store1-sd$Solid_Domestics_AvgInventory_store2)*sd$Solid_Domestics_INV
sd$SALES_LIFT  <- sd$SALES_OPPORTUNITY-sd$INV_OPPORTUNITY_COST
###
wpt <- WOOD_PLANK_TILE

final_site$store <- as.numeric(final_site$store)
wpt$store1 <- as.numeric(wpt$store1)

wpt <- wpt %>% 
  left_join(final_site, by=c('store1'='store')) %>% 
  select(store1, lat, lon, store2, distance, Wood_Plank_Tile, Wood_Plank_Tile_PCT, Wood_Plank_Tile_PCT_quartile_store1, Wood_Plank_Tile_PCT_quartile_store2, DemographicCluster_store1, DemographicCluster_store2, Wood_Plank_Tile_AvgInventory_store1, Wood_Plank_Tile_AvgInventory_store2, cluster_test, inventory_test)
names(wpt)[names(wpt) =="lat"] <- "lat_store1"
names(wpt)[names(wpt) =="lon"] <- "lon_store1"
names(wpt)[names(wpt) =="Wood_Plank_Tile"] <- "Wood_Plank_Tile_SALES_store1" 
names(wpt)[names(wpt) =="Wood_Plank_Tile_PCT"] <- "Wood_Plank_Tile_PCT_store1" 
wpt$store2 <- as.numeric(wpt$store2)
wpt <- wpt %>% 
  left_join(final_site, by=c('store2'='store')) %>% 
  select(store1, lat_store1, lon_store1, store2, lat, lon, distance, Wood_Plank_Tile_SALES_store1, Wood_Plank_Tile_PCT_store1, Wood_Plank_Tile_PCT_quartile_store1, Wood_Plank_Tile, Wood_Plank_Tile_PCT, Wood_Plank_Tile_PCT_quartile_store2, DemographicCluster_store1, DemographicCluster_store2, Wood_Plank_Tile_AvgInventory_store1, Wood_Plank_Tile_AvgInventory_store2, cluster_test, inventory_test)
names(wpt)[names(wpt) =="lat"] <- "lat_store2" 
names(wpt)[names(wpt) =="lon"] <- "lon_store2" 
names(wpt)[names(wpt) =="Wood_Plank_Tile"] <- "Wood_Plank_Tile_SALES_store2" 
names(wpt)[names(wpt) =="Wood_Plank_Tile_PCT"] <- "Wood_Plank_Tile_PCT_store2" 
wpt %>% 
  left_join(AVGINV, by=c('store1'='store')) %>% 
  select(store1, lat_store1, lon_store1, store2, lat_store2, lon_store2, distance, Wood_Plank_Tile_SALES_store1, Wood_Plank_Tile_PCT_store2, Wood_Plank_Tile_PCT_quartile_store1, Wood_Plank_Tile_SALES_store2, Wood_Plank_Tile_PCT_store1, Wood_Plank_Tile_PCT_quartile_store2, DemographicCluster_store1, DemographicCluster_store2, Wood_Plank_Tile_AvgInventory_store1, Wood_Plank_Tile_AvgInventory_store2, cluster_test, inventory_test, Wood_Plank_Tile_INV) -> wpt

wpt$SALES_OPPORTUNITY <- abs(wpt$Wood_Plank_Tile_SALES_store1-wpt$Wood_Plank_Tile_SALES_store2)
wpt$INV_OPPORTUNITY_COST <- abs(wpt$Wood_Plank_Tile_AvgInventory_store1-wpt$Wood_Plank_Tile_AvgInventory_store2)*wpt$Wood_Plank_Tile_INV
wpt$SALES_LIFT  <- wpt$SALES_OPPORTUNITY-wpt$INV_OPPORTUNITY_COST




sum(
  sum(sd$SALES_OPPORTUNITY)+
    sum(wpt$SALES_OPPORTUNITY)+
    sum(cf$SALES_OPPORTUNITY)+
    sum(vin$SALES_OPPORTUNITY)+
    sum(bam$SALES_OPPORTUNITY)+
    sum(eng$SALES_OPPORTUNITY)+
    sum(lam$SALES_OPPORTUNITY)+
    sum(se$SALES_OPPORTUNITY)+
    sum(sd$SALES_OPPORTUNITY)
)

sum(eng$SALES_OPPORTUNITY)+sum(cf$SALES_OPPORTUNITY)

sum(
  sum(sd$INV_OPPORTUNITY_COST)+
    sum(wpt$INV_OPPORTUNITY_COST)+
    sum(cf$INV_OPPORTUNITY_COST)+
    sum(vin$INV_OPPORTUNITY_COST)+
    sum(bam$INV_OPPORTUNITY_COST)+
    sum(eng$INV_OPPORTUNITY_COST)+
    sum(lam$INV_OPPORTUNITY_COST)+
    sum(se$INV_OPPORTUNITY_COST)+
    sum(sd$INV_OPPORTUNITY_COST)
)

sum(eng$INV_OPPORTUNITY_COST)+sum(cf$INV_OPPORTUNITY_COST)

sum(
  sum(sd$SALES_LIFT)+
    sum(wpt$SALES_LIFT)+
    sum(cf$SALES_LIFT)+
    sum(vin$SALES_LIFT)+
    sum(bam$SALES_LIFT)+
    sum(eng$SALES_LIFT)+
    sum(lam$SALES_LIFT)+
    sum(se$SALES_LIFT)+
    sum(sd$SALES_LIFT)
)

sum(eng$SALES_LIFT)+sum(cf$SALES_LIFT)



#library(openxlsx)
#write.xlsx(cf, '/Users/trehall/Lumber Liquidators/Model/NearbyCorkFlooring.xlsx', row.names=TRUE)
#write.xlsx(vin, '/Users/trehall/Lumber Liquidators/Model/NearbyVinyl.xlsx', row.names=TRUE)
#write.xlsx(bam, '/Users/trehall/Lumber Liquidators/Model/NearbyBamboo.xlsx', row.names=TRUE)
#write.xlsx(eng, '/Users/trehall/Lumber Liquidators/Model/NearbyEngineered.xlsx', row.names=TRUE)
#write.xlsx(lam, '/Users/trehall/Lumber Liquidators/Model/NearbyLaminate.xlsx', row.names=TRUE)
#write.xlsx(se, '/Users/trehall/Lumber Liquidators/Model/NearbySolidExoticts.xlsx', row.names=TRUE)
#write.xlsx(sd, '/Users/trehall/Lumber Liquidators/Model/NearbySolidDomestics.xlsx', row.names=TRUE)
#write.xlsx(wpt, '/Users/trehall/Lumber Liquidators/Model/NearbyWoodPlankTile.xlsx', row.names=TRUE)

