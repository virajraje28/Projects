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

#find top 10-15 items sold per store and find a match

#View(head(final_sales))


#Sales Group by store/article
salesGroupedByStoreArticle <- final_sales %>%
  group_by(store, article) %>%
  summarise(distinctCount = n(), totalOrderDollars = sum(orders.dol), totalUnitsSold = sum(orders.units))
# 
# View(salesGroupedByStoreArticle %>%
#        arrange(desc(distinctCount)))
# View(final_art %>%
#      filter(article == '10041025'))

# Identify top 15 articles sold in each store. save it in a unordered list

rankedSalesPerStore <-salesGroupedByStoreArticle %>%
  arrange(store, desc(distinctCount), desc(totalOrderDollars)) %>%
  group_by(store) %>%
  mutate(rank = rank(desc(distinctCount),ties.method = "first"))


#keep only top 15 articles per store
rankedSalesPerStoreTopN <- rankedSalesPerStore %>%
  filter(rank <=15)

#View(rankedSalesPerStoreTopN)

#Top 15 articlets are kept stored a list
rankedSalesSmashed <- rankedSalesPerStoreTopN %>%
  group_by(store) %>%
  summarise(listofArticles = list(article))
#View(rankedSalesSmashed)
#GEt combination of all possible stores
AllStores <- final_sales %>%
  distinct(store)
#View(AllStores)

AllStoreCombinations <- expand.grid(AllStores$store,AllStores$store)
#View(AllStoreCombinations) 

#rename of columns to store 1 and store 2
names(AllStoreCombinations)[1] <- "store1"
names(AllStoreCombinations)[2] <- "store2"
nrow(AllStoreCombinations) #-- 145161
#Join AllStore combination with rankedSalesSmashed 2 times to get the best 15 for both stores
AllStoreCombinations <- AllStoreCombinations %>%
  left_join(rankedSalesSmashed, by = c("store1" = "store"))

names(AllStoreCombinations)[3] <- "store1Articles"

AllStoreCombinations <- AllStoreCombinations %>%
  left_join(rankedSalesSmashed, by = c("store2" = "store"))

names(AllStoreCombinations)[4] <- "store2Articles"
#View(AllStoreCombinations) 

#Find intersect and store it in another column



AllStoreCombinations <- AllStoreCombinations %>%
  mutate(matchingArticles =mapply(intersect, store1Articles, store2Articles)) # / length(store1Articles)))



AllStoreCombinations <- AllStoreCombinations %>%
  mutate(totalArticles =  0)


for(i in 1:nrow(AllStoreCombinations)){
  AllStoreCombinations$totalArticles[i] <- length(AllStoreCombinations$matchingArticles[[i]])
}

AllStoreCombinations <- AllStoreCombinations %>%
  mutate(percentMAtch = totalArticles/15)


#Remove records which has same store 1 and 2
nrow(AllStoreCombinations) 

AllStoreCombinations <- AllStoreCombinations %>%
  filter(store1 != store2)

nrow(AllStoreCombinations) 



#Find stores that match at cat desc level or class desc level but have different products sold....

salesJoinedArticle <- final_sales %>%
  left_join(final_art,by = c("article","article"))

salesGroupedByStoreCatDesc <- salesJoinedArticle %>%
  group_by(store, cat.desc) %>%
  summarise(distinctCount = n(), totalOrderDollars = sum(orders.dol), totalUnitsSold = sum(orders.units))

#View(salesGroupedByStoreCatDesc)

rankedSalesPerStoreCatDesc <-salesGroupedByStoreCatDesc %>%
  arrange(store, desc(distinctCount), desc(totalOrderDollars)) %>%
  group_by(store) %>%
  mutate(rank = rank(desc(distinctCount),ties.method = "first"))

#View(rankedSalesPerStoreCatDesc)

#keep only top 6 articles class desc
rankedSalesPerStoreCatDescTopN <- rankedSalesPerStoreCatDesc %>%
  filter(rank <=6)

#Top 15 articlets are kept stored a list
rankedSalesCatDescSmashed <- rankedSalesPerStoreCatDescTopN %>%
  group_by(store) %>%
  summarise(listofCatDesc = list(cat.desc))
#View(rankedSalesCatDescSmashed)
#View(AllStoreCombinations)

#Join AllStore combination with rankedSalesSmashed 2 times to get the best 15 for both stores
AllStoreCombinations <- AllStoreCombinations %>%
  left_join(rankedSalesCatDescSmashed, by = c("store1" = "store"))

names(AllStoreCombinations)[8] <- "store1CatDesc"

AllStoreCombinations <- AllStoreCombinations %>%
  left_join(rankedSalesCatDescSmashed, by = c("store2" = "store"))

names(AllStoreCombinations)[9] <- "store2CatDesc"
#View(AllStoreCombinations) 

#Find intersect and store it in another column

AllStoreCombinations <- AllStoreCombinations %>%
  mutate(matchingCatDesc = mapply(intersect, store1CatDesc, store2CatDesc)) # / length(store1Articles)))



AllStoreCombinations <- AllStoreCombinations %>%
  mutate(totalCatDesc =  0)


for(i in 1:nrow(AllStoreCombinations)){
  AllStoreCombinations$totalCatDesc[i] <- length(AllStoreCombinations$matchingCatDesc[[i]])
}

AllStoreCombinations <- AllStoreCombinations %>%
  mutate(percentMAtchCatDesc = totalCatDesc/6)


#############

#Adding distance to the store

#View(head(AllStoreCombinations))
#View(head(final_site))
library(geosphere)
library(dplyr)

AllStoreCombinations <- AllStoreCombinations %>%
  left_join(final_site[,c("store","lon","lat")], by = c("store1" = "store"))

names(AllStoreCombinations)[names(AllStoreCombinations) =="lat"] <- "store1Lat"
names(AllStoreCombinations)[names(AllStoreCombinations) =="lon"] <- "store1Lon"

AllStoreCombinations <- AllStoreCombinations %>%
  left_join(final_site[,c("store","lon","lat")], by = c("store2" = "store"))

names(AllStoreCombinations)[names(AllStoreCombinations) =="lat"] <- "store2Lat"
names(AllStoreCombinations)[names(AllStoreCombinations) =="lon"] <- "store2Lon"



AllStoreCombinations <- AllStoreCombinations %>%
  mutate(distance = distHaversine(cbind(store1Lon, store1Lat), cbind(store2Lon, store2Lat)))

AllStoreCombinations$distance = AllStoreCombinations$distance * 0.000621371



AllStoreCombinations <- AllStoreCombinations %>%
  left_join(final_site[,c("store","state")], by = c("store1" = "store"))
names(AllStoreCombinations)[names(AllStoreCombinations) =="state"] <- "store1State"

AllStoreCombinations <- AllStoreCombinations %>%
  left_join(final_site[,c("store","state")], by = c("store2" = "store"))
names(AllStoreCombinations)[names(AllStoreCombinations) =="state"] <- "store2State"

#View(AllStoreCombinations %>%
#       filter(distance <=50))

###########################START#############################
storesWithin50Miles <- AllStoreCombinations %>%
  filter(distance <=50) %>%
  distinct(store1)

storesWithin50Miles <- storesWithin50Miles %>%
  left_join(rankedSalesPerStoreTopN, by=c('store1' = 'store')) %>%
  select(store1, article, rank)



storesWithin50Miles <- storesWithin50Miles %>%
  left_join(AllStoreCombinations, by=c('store1' = 'store1')) %>%
  filter(distance <= 50) %>%
  select(store1, article,rank,store2, distance)
#####

distinctStoreArticle_FromSales <- final_sales %>%
  distinct(store,article)
nrow(distinctStoreArticle_FromSales)
#View(distinctStoreArticle_FromSales)

distinctStoreArticle_FromInventory <- final_inventory %>%
  distinct(store,article)
nrow(distinctStoreArticle_FromInventory)
#View(distinctStoreArticle_FromInventory)

DistinctStoreArticle <- merge(distinctStoreArticle_FromInventory, distinctStoreArticle_FromSales, all=TRUE)

nrow(DistinctStoreArticle)
#View(DistinctStoreArticle)

#Total weeks of inventory - 84
DistinctStoreArticle <- DistinctStoreArticle %>%
  left_join(final_inventory, by = c('store' = 'store' , 'article' = 'article'))%>%
  group_by(store, article) %>%
  summarise(averageInventory = sum(inv.units)/84,
            distinctInventoryRecords = n())

#We have 78 weeks of data
DistinctStoreArticle <- DistinctStoreArticle %>%
  left_join(final_sales, by = c('store' = 'store' , 'article' = 'article'))%>%
  group_by(store, article, averageInventory, distinctInventoryRecords) %>%
  summarise(averageSalesPerWeek = sum(orders.units)/78,
            distinctSalesOrders = n())

#View(DistinctStoreArticle)

#DistinctStoreArticle <- DistinctStoreArticle %>% ungroup(averageInventory) 
DistinctStoreArticle$averageInventory <- as.integer(DistinctStoreArticle$averageInventory)
DistinctStoreArticle[is.na(DistinctStoreArticle)] <- 0

DistinctStoreArticle$averageInventory[DistinctStoreArticle$averageInventory == 0] <- 0.001
DistinctStoreArticle$averageSalesPerWeek[DistinctStoreArticle$averageSalesPerWeek == 0] <- 0.001
DistinctStoreArticle$distinctInventoryRecords[DistinctStoreArticle$averageInventory == 0.001] <- 0
DistinctStoreArticle$distinctSalesOrders[DistinctStoreArticle$averageSalesPerWeek == 0.001] <- 0

DistinctStoreArticle <- DistinctStoreArticle %>%
  mutate(salesPerInventory = averageSalesPerWeek/averageInventory)

DistinctStoreArticle$salesPerInventory[DistinctStoreArticle$averageSalesPerWeek == 0.001] <- 0

DistinctStoreArticle$salesPerInventory <- DistinctStoreArticle$salesPerInventory *100

DistinctStoreArticle$salesPerInventory[DistinctStoreArticle$averageInventory == 0.001] <- 99999
DistinctStoreArticle$salesPerInventory <- as.integer(DistinctStoreArticle$salesPerInventory)

#######Run the sales_Site_Article file#####################################
#View(head(DistinctStoreArticle))
#View(head(final_master))
distinctSalesCluster <- final_master %>%
  group_by(store, article) %>%
  summarise(totalOutOfCluster = sum(OUTofCLUST),
            totalOutOfStore = sum(OUTofOrderStore))

DistinctStoreArticle <- DistinctStoreArticle %>%
  left_join(distinctSalesCluster, by = c('store'= 'store', 'article' = 'article'))


DistinctStoreArticle[is.na(DistinctStoreArticle)] <- 0


DistinctStoreArticle$OutOfClusterPercent = DistinctStoreArticle$totalOutOfCluster / DistinctStoreArticle$distinctSalesOrders
DistinctStoreArticle$OutOfStorePercent = DistinctStoreArticle$totalOutOfStore / DistinctStoreArticle$distinctSalesOrders
DistinctStoreArticle$OutOfClusterPercent[is.nan(DistinctStoreArticle$OutOfClusterPercent)]<-0
DistinctStoreArticle$OutOfStorePercent[is.nan(DistinctStoreArticle$OutOfStorePercent)]<-0

#View(DistinctStoreArticle)

# View(DistinctStoreArticle)
# 
# View(DistinctStoreArticle %>%
#   filter(salesPerInventory == 99999) %>%
#   arrange(desc(distinctSalesOrders)))
# 
# View(final_art %>% filter(article == '10035133'))
# View(final_sales %>% filter(article == '10035133' & store = '1055'))
# View(final_site %>% filter(store == '1055'))
# View(DistinctStoreArticle)
# 
# sapply(DistinctStoreArticle, class)

#View(head(final_master))
#####


storesWithin50Miles <- storesWithin50Miles %>%
  left_join(DistinctStoreArticle, by = c('store1' = 'store', 'article' = 'article'))%>%
  select(store1, article,rank,store2,distance, averageInventory, averageSalesPerWeek)

names(storesWithin50Miles)[names(storesWithin50Miles) =="averageInventory"] <- "store1averageInventory"  
names(storesWithin50Miles)[names(storesWithin50Miles) =="averageSalesPerWeek"] <- "store1averageSalesPerWeek"

storesWithin50Miles <- storesWithin50Miles %>%
  left_join(DistinctStoreArticle, by = c('store2' = 'store', 'article' = 'article'))%>%
  select(store1, article,rank,store2, distance, store1averageInventory, store1averageSalesPerWeek,averageInventory,averageSalesPerWeek)


names(storesWithin50Miles)[names(storesWithin50Miles) =="averageInventory"] <- "store2averageInventory"  
names(storesWithin50Miles)[names(storesWithin50Miles) =="averageSalesPerWeek"] <- "store2averageSalesPerWeek"

names(storesWithin50Miles)[names(storesWithin50Miles) =="rank"] <- "store1rank"

storesWithin50Miles <- storesWithin50Miles %>%
  left_join(rankedSalesPerStore, by=c("store2" = "store", 'article' = 'article')) %>%
  select(store1, article,store1rank,store2,distance, store1averageInventory, store1averageSalesPerWeek,store2averageInventory, store2averageSalesPerWeek, rank)

names(storesWithin50Miles)[names(storesWithin50Miles) =="rank"] <- "store2rank"

storesWithin50Miles <- storesWithin50Miles %>%
  left_join(final_site, by = c('store1' = 'store')) %>%
  select(article, store1, store1rank,store2,store2rank, state, distance, store1averageInventory,store2averageInventory, store1averageSalesPerWeek,store2averageSalesPerWeek)
names(storesWithin50Miles)[names(storesWithin50Miles) =="state"] <- "store1state"

storesWithin50Miles <- storesWithin50Miles %>%
  left_join(final_site, by = c('store2' = 'store')) %>%
  select(article, store1, store1rank,store2,store2rank, store1state, state, distance, store1averageInventory,store2averageInventory, store1averageSalesPerWeek,store2averageSalesPerWeek)

names(storesWithin50Miles)[names(storesWithin50Miles) =="state"] <- "store2state"


#View(storesWithin50Miles)
#View(AllStoreCombinations)
#View(final_site)

#View(storesWithin50Miles %>%
#       filter(store1 == '1016'))


####Rules
#Highly selling item in store 1 (rank less than 10)
#LEast selling item in store 2 (rank more than 50)
#Percentage of inventory between store 1 and store 2 is less than 20%
#average store 2 sq foot is greater than 2000

storesWithin50Miles <- storesWithin50Miles %>%
  left_join(final_site, by = c('store2' = 'store')) %>%
  mutate(store2StoragePlace = store.sf - showroom.sf, percentInventoryStore2 = ifelse(store1averageInventory == 0,NA, store2averageInventory/store1averageInventory)) %>%
  select(article, store1, store1rank,store2,store2rank,store2StoragePlace,percentInventoryStore2, store1state, store2state, distance, store1averageInventory,store2averageInventory, store1averageSalesPerWeek,store2averageSalesPerWeek)


#View(storesWithin50Miles %>%
#       filter(store1rank <= 10 & store2rank > 50 & percentInventoryStore2 < 0.5 & store2StoragePlace > 2000))

final_storesWithin50Miles <- storesWithin50Miles %>%
  filter(store1rank <= 10 & store2rank > 50 & percentInventoryStore2 < 0.5 & store2StoragePlace > 2000)

#write.csv(final_storesWithin50Miles, '/Users/virajraje/Documents/Viraj/Masters/VCU/StudyMaterial/GitRepo/LumberVCU/Data/DerivedFiles/final_storesWithin50Miles.csv')
remove(final_storesWithin50Miles)
#######################END################################
#View(final_art %>%
#       filter(article =='10028427'))

#View(final_sales %>%
#       filter(article =='10030007' & (store == '1298' | store == '1227')))

#View(final_site %>%
#       filter(store =='1346' | store == '1136'))
############RANDOM STUFF#######
# 10035133
# 
# final_inventory %>%
#   filter(store == '1055' | store == '1003') %>%
#   filter(article == '10035133') %>%
#   group_by(store) %>%
#   summarise(n = n(), avg = mean(inv.units))
# 
# View(final_sales %>%
#   filter(store == '1055' & article == '10035133'))
# 
# View(final_sales %>%
#   filter(store == '1003' & article =='10035133'))
# 
# View(head(final_inventory))
# article_num <- '10043301'
# #Random analysis ----
# View(final_sales %>%
#   filter(article == article_num & store == '1131'))
# 
# View(final_sales %>%
#   filter(article == article_num & store == '1026'))
# 
# 
# View(final_inventory %>%
#   filter(article == article_num & store == '1131') %>%
#   arrange(date.inv.measured))
# 
# View(final_inventory %>%
#        filter(article == article_num & store == '1026') %>%
#        arrange(date.inv.measured))
# 
# #Top 10 items shipped outside the cluster... LEast selling items of that store but are in the inventory...
# 
# 
# #Average inventory data ----
# View(head(final_inventory))
# 
# Distinct_inventory_Measured <- final_inventory %>%
#   distinct(store, date.inv.measured)
# 
# View(Distinct_inventory_Measured)
# 
# inventory_Measurement_Frequency_Count_Per_Store <- Distinct_inventory_Measured %>%
#   group_by(store) %>%
#   summarise(TimesInventoryCounted = n())
# 
# View(inventory_Measurement_Frequency_Count_Per_Store)
# 
# View(site %>%
#        distinct(tv.dma))
# 
# View(final_master %>%
#        group_by(store) %>%
#        summarise(n= n()) %>%
#        arrange(desc(n)))
# 
# test <- final_master %>%
#        filter(is.na(class.desc))
# 
# test <- test %>%
#        distinct(article)
# 
# View(final_master %>%
#   filter(article == '10043052'))
# 
# View(final_art %>%
#        filter(article == '10043052'))
# 
# nrow(test)  

#View(head(final_master))

rm(i)
rm(AllStores)
rm(rankedSalesSmashed)
rm(salesJoinedArticle)
rm(storesWithin50Miles)
rm(distinctSalesCluster)
rm(rankedSalesCatDescSmashed)
rm(rankedSalesPerStoreCatDesc)
rm(salesGroupedByStoreCatDesc)
rm(salesGroupedByStoreArticle)
rm(rankedSalesPerStoreCatDescTopN)
rm(distinctStoreArticle_FromSales)
rm(distinctStoreArticle_FromInventory)

rm(dic)
rm(web)
rm(final_art)
rm(final_prom)
rm(final_site)
rm(final_sales)
rm(final_master)
rm(final_weather)
rm(final_inventory)
rm(final_demographic)

save.image('RDATA/InventorySalesMatrices.RData')

