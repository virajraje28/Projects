
# Descriptives for final_master file

setwd('/Users/trehall/Lumber Liquidators/Model')
load('RDATA/MASTER.RData')


# load libraries
library(data.table)
library(dplyr)
library(tidyr)
library(ggplot2)
library(lubridate)
library(DataExplorer)

# read file
descriptives <- final_master

# filter out negative asp values and orders.dol values
descriptives <- descriptives[!(descriptives$asp<0 & descriptives$orders.dol<0),]

# summarize the data in the file -- discrete, continuous, missing values
introduce(descriptives)

# data structure of each file -- look at the attributes for each file
plot_str(final_demographic)
plot_str(final_art)
plot_str(final_inventory)
plot_str(web)
plot_str(final_prom)
plot_str(final_sales)
plot_str(final_site)
plot_str(final_weather)

# visualize the table
plot_intro(descriptives)

# missing values - complete file
plot_missing(descriptives)

# missing values by file
plot_missing(final_demographic)
plot_missing(final_art)
plot_missing(final_inventory)
plot_missing(web)
plot_missing(final_prom)
plot_missing(final_sales)
plot_missing(final_site)
plot_missing(final_weather)

# drop columns that won't be used, duplicates, and those with high numbers of missing values
descriptives %>% select(-c(orders.clust, fill.clust, shipment_distance, OUTofCLUST, clust, fill.lon, fill.lat, OUTofOrderStore, Event)) -> descriptives
descriptives %>% select(-c(V1, cust.type, primary.dc.y, old.article, Tier, Financing, `New Orders Merch GM%`, `New Orders Flooring GM%`, `New Orders Non-Flooring GM%`, `New Orders per Ad Spend`, `New Orders Plan`, `Avg Daily Total`, `Avg Daily Merch`, `Avg Daily Install`, `Avg Daily Pro`, `Avg Daily Web`, `Total Working Spend`, `Daily Working Spend`)) -> descriptives
descriptives %>% select(-c(install.date, merch.cat, merch.class, merch.family, grade, day, sold.to.party, article, sales.order, orders.lon, orders.lat, store.desc, tv.dma, address, city, postal.code, lon, lat, pog.end.date, Start, End, City)) -> descriptives
# drop duplicate columns from census file
descriptives %>% select(-c(`Est Daily Merch GM$`, CBSAPopulation, CBSAType, CBSAName, CBSANumber, CBSADivisionPopulation, StateFIPS, CountyFIPS, CongressionalLandArea, CongressionalDistrict, DayLightSavings, AreaCode, ACSRaceBlack, ACSRaceHawaiian, ACSRaceIndian, ACSRaceIndian, ACSRaceOther, ACSRaceWhite, ACSPopulation, ACSPopulationFemale, ACSPopulationMale, ACSRaceAsian, ACSAgeMedian, ACSAgeMedianFemale, ACSAgeMedianMale, ACSHousingValueMedian, ACSHouseholdsTotal, ACSHouseholdSizeAverage, ACSHouseholdIncomeMedian)) -> descriptives
descriptives %>% select(-c(PopulationEstimate, BusinessFirstQuarterPayroll, DeliveryBusiness, DeliveryResidential, DeliveryTotal, CityAliasName, StateFullName, CountyName, Latitude, Longitude, GrowingCountiesA, GrowingCountiesB, GrowthRank)) -> descriptives


# column conversions
descriptives$customer.type <- as.factor(descriptives$customer.type)
descriptives$cust.type <- as.factor(descriptives$cust.type)
descriptives$gbb <- as.factor(descriptives$gbb)
descriptives$store <- as.factor(descriptives$store)
descriptives$ship.from.site <- as.factor(descriptives$ship.from.site)
descriptives$primary.dc.x <- as.factor(descriptives$primary.dc.x)
descriptives$article <- as.factor(descriptives$article)
descriptives$zone <- as.factor(descriptives$zone)
descriptives$region.desc <- as.factor(descriptives$region.desc)
descriptives$ac.rating <- as.factor(descriptives$ac.rating)
descriptives$primary.dc.y <- as.factor(descriptives$primary.dc.y)
descriptives$merch.cat <- as.factor(descriptives$merch.cat)
descriptives$cat.desc <- as.factor(descriptives$cat.desc)
descriptives$merch.class <- as.factor(descriptives$merch.class)
descriptives$class.desc <- as.factor(descriptives$class.desc) 
descriptives$merch.family <- as.factor(descriptives$merch.family)
descriptives$finish.stain <- as.factor(descriptives$finish.stain)
descriptives$tv.dma <- as.factor(descriptives$tv.dma)
descriptives$postal.code <- as.factor(descriptives$postal.code)
descriptives$compt.status <- as.factor(descriptives$compt.status)
descriptives$marketing.name <- as.factor(descriptives$marketing.name) 
descriptives$grade <- as.factor(descriptives$grade) 
descriptives$trade.name <- as.factor(descriptives$trade.name)
descriptives$installation.type <- as.factor(descriptives$installation.type)
descriptives$with.pad <- as.factor(descriptives$with.pad)
descriptives$abc.indicator <- as.factor(descriptives$abc.indicator) 
descriptives$`Start DOW` <- as.factor(descriptives$`Start DOW`) 
descriptives$`End DOW` <- as.factor(descriptives$`End DOW`) 
descriptives$Tier <- as.factor(descriptives$Tier) 
descriptives$width <- as.factor(descriptives$width)
descriptives$overall.thickness <- as.factor(descriptives$overall.thickness)
descriptives$Event <- as.factor(descriptives$Event)

# change date from character to date
#descriptives$day <- as_date(descriptives$day)
#descriptives$install.date <- as_date(descriptives$install.date)
descriptives$open.date <- as_date(descriptives$open.date)
#descriptives$day <- as_date(descriptives$day)
#descriptives$created <- as_date(descriptives$created)
descriptives$pog.begin.date <- as_date(descriptives$pog.begin.date)
#descriptives$pog.end.date <- as_date(descriptives$pog.end.date)
#descriptives$Start <- as_date(descriptives$Start)
#descriptives$End <- as_date(descriptives$End)

sapply(descriptives, class)

# Generate summaries
# install.packages("summarytools")
library(summarytools)

# numerical variables - view in the console
descr(descriptives)

# note - this takes awhile to run 
dfSummary(descriptives)
view(dfSummary(descriptives))
print(dfSummary(descriptives),
      file = "descriptive summary.html",
      report.title = "Descriptive Summary")

# discrete variables - view in the console
freq(descriptives)

# discrete variables - look at individually since the list is long
freq(descriptives$store)
freq(descriptives$customer.type)
freq(descriptives$class.desc)
freq(descriptives$cat.desc)
freq(descriptives$art.desc)
freq(descriptives$trade.name)
freq(descriptives$marketing.name)
freq(descriptives$finish.stain)

# delete temporary html files created
cleartmp(all = TRUE, silent = FALSE, verbose = FALSE)

# change themes
theme_set(theme_bw())+theme_replace(panel.grid.major=element_blank(),panel.grid.minor=element_blank())

# histograms
# orders.dol
ggplot(data=descriptives, aes(descriptives$orders.dol))+
  geom_histogram(breaks=seq(50, 12000, by=200),
                 col="black",
                 fill="blue",
                 alpha=0.4) +
  labs(title="Histogram of Orders.dol", x="Orders.dol", y="Count") +
  xlim(c(0,12000)) +
  ylim(c(0,200000))

# orders.units
ggplot(data=descriptives, aes(descriptives$orders.units))+
  geom_histogram(breaks=seq(15, 6000, by=150),
                 col="black",
                 fill="green",
                 alpha=0.4) +
  labs(title="Histogram of Orders.units", x="Orders.units", y="Count") +
  xlim(c(0,6000)) +
  ylim(c(0,150000))

# compt.index
ggplot(data=descriptives, aes(descriptives$compt.index))+
  geom_histogram(breaks=seq(0, 14, by=1),
    col="black",
    fill="yellow",
    alpha=0.4) +
  labs(title="Histogram of Competitive Index", x="Compt.index", y="Count")

# households.10mi
ggplot(data=descriptives, aes(descriptives$households.10mi))+
  geom_histogram(
    col="black",
    fill="red",
    alpha=0.4) +
  labs(title="Histogram of Households within 10 Miles", x="Households.10mi", y="Count")

# households.20mi
ggplot(data=descriptives, aes(descriptives$households.20mi))+
  geom_histogram(
    col="black",
    fill="gray",
    alpha=0.6) +
  labs(title="Histogram of Households within 20 Miles", x="Households.20mi", y="Count")

# household.income.10.mi
ggplot(data=descriptives, aes(descriptives$household.income.10mi))+
  geom_histogram(
    col="black",
    fill="red",
    alpha=0.6) +
  labs(title="Histogram of Household Income within 10 Miles", x="Household.income.10mi", y="Count")

# household.income.20mi
ggplot(data=descriptives, aes(descriptives$household.income.20mi))+
  geom_histogram(
    col="black",
    fill="gray",
    alpha=0.6) +
  labs(title="Histogram of Household Income within 20 Miles", x="Household.income.20mi", y="Count")

# showroom.sf
ggplot(data=descriptives, aes(descriptives$showroom.sf))+
  geom_histogram(
    col="black",
    fill="black",
    alpha=0.6) +
  labs(title="Histogram of Showroom Square Footage", x="showroom.sf", y="Count")

# store.sf
ggplot(data=descriptives, aes(descriptives$store.sf))+
  geom_histogram(
    col="black",
    fill="black",
    alpha=0.4) +
  labs(title="Histogram of Store Square Footage", x="store.sf", y="Count")

# Days
ggplot(data=descriptives, aes(descriptives$Days))+
  geom_histogram(
    col="black",
    fill="brown",
    alpha=0.6) +
  labs(title="Histogram of Promotion Days", x="Days", y="Count")

# asp
ggplot(data=descriptives, aes(descriptives$asp))+
  geom_histogram(col="black",
                 fill="blue",
                 alpha=0.4) +
  labs(title="Histogram of ASP", x="ASP", y="Count") +
  xlim(c(0,15)) +
  ylim(c(0,30000))

# scatterplots
# orders.dol by store -- takes a while to run
ggplot(data=descriptives, mapping = aes(x = store, y = orders.dol)) + 
  geom_point(fill="green", alpha=0.8) + 
  geom_smooth() + 
  labs(title="Store & Orders.dol", x = "store", y = "orders.dol")

# orders.units and orders.dol -- takes a while to run
ggplot(data=descriptives, mapping = aes(x = orders.units, y = orders.dol)) + 
  geom_point() + 
  geom_smooth() + 
  labs(title="Orders.dol & Orders.units", x = "orders.units", y = "orders.dol")

# class.desc and orders.dol -- takes a while to run
ggplot(data=descriptives, mapping = aes(x = class.desc, y = orders.dol)) + 
  geom_point() + 
  geom_smooth() + 
  labs(title="Class.desc & Orders.units", x = "Class Description", y = "Sales")

# cat.desc and orders.dol -- takes a while to run
ggplot(data=descriptives, mapping = aes(x = cat.desc, y = orders.dol)) + 
  geom_point() + 
  geom_smooth() + 
  labs(title="Category Description & Orders.units", x = "Category Description", y = "Sales") +
  theme(axis.text.x=element_text(angle=90, hjust=1))

# bar charts
# number of records by customer type
ggplot(data = descriptives) + 
  geom_bar(mapping = aes(x = customer.type), col="gray", fill="purple", alpha=0.4) +
  labs(title="Count of Customer Type", x = "Customer Type", y = "Number of Records")

# number of records by class description
ggplot(data = descriptives) + 
  geom_bar(mapping = aes(x = class.desc), col="gray", fill="green", alpha=0.4) + 
  labs(title="Count of Class", x = "Class Description", y = "Number of Records")
  theme(axis.text.x=element_text(angle=90, hjust=1))

# number of records by category description  
ggplot(data = descriptives) + 
  geom_bar(mapping = aes(x = cat.desc), col="gray", fill="brown", alpha=0.4) + 
  labs(title="Count of Category", x = "Category Description", y = "Number of Records") +
  theme(axis.text.x=element_text(angle=90, hjust=1))

# box plots
# class description and orders
ggplot(data = descriptives, mapping = aes(x = class.desc, y = orders.dol)) +
  geom_boxplot() + 
  labs(title="Class.desc by Orders.dol") +
  theme(axis.text.x=element_text(angle=90, hjust=1))

# Category description and orders.dol
ggplot(data = descriptives, mapping = aes(x = cat.desc, y = orders.dol)) +
  geom_boxplot() +
  labs(title="Product Category by Orders.dol") +
  theme(axis.text.x=element_text(angle=90, hjust=1)) 

# Category description and orders.units
ggplot(data = descriptives, mapping = aes(x = cat.desc, y = orders.units)) +
  geom_boxplot() +
  labs(title="Product Category by Units Sold") +
  theme(axis.text.x=element_text(angle=90, hjust=1))

# boxplot of class and asp
ggplot(data = descriptives, mapping = aes(x = class.desc, y = asp)) +
  geom_boxplot() +
  labs(title="Product Class by Average Selling Price") +
  theme(axis.text.x=element_text(angle=90, hjust=1))

# boxplot of category and asp
ggplot(data = descriptives, mapping = aes(x = cat.desc, y = asp)) +
  geom_boxplot() +
  labs(title="Product Category by Average Selling Price") +
  theme(axis.text.x=element_text(angle=90, hjust=1))

# boxplot of product class and households within 10 mi of the store
ggplot(data = descriptives, mapping = aes(x = class.desc, y = households.10mi)) +
  geom_boxplot() +
  labs(title="Product Class by Households within 10 mi of Store") +
  ylim(c(0,3000000)) +
theme(axis.text.x=element_text(angle=90, hjust=1))

# boxplot of product category and households within 10 mi of the store
ggplot(data = descriptives, mapping = aes(x = cat.desc, y = households.10mi)) +
  geom_boxplot() +
  labs(title="Product Category by Households within 10 mi of Store") +
  theme(axis.text.x=element_text(angle=90, hjust=1))

# boxplot of product class within 20 mi of the store
ggplot(data = descriptives, mapping = aes(x = class.desc, y = households.20mi)) +
  geom_boxplot() +
  labs(title="Product Class by Households within 20 mi of Store")
theme(axis.text.x=element_text(angle=90, hjust=1))

# boxplot of product category within 20 mi of the store
ggplot(data = descriptives, mapping = aes(x = cat.desc, y = households.20mi)) +
  geom_boxplot() +
  labs(title="Product Category by Households within 20 mi of Store") +
  theme(axis.text.x=element_text(angle=90, hjust=1))

# boxplot of class and household income within 20 mi of a store
ggplot(data = descriptives, mapping = aes(x = class.desc, y = household.income.20mi)) +
  geom_boxplot() +
  labs(title="Product Class by Household Income within 20 mi of Store")
theme(axis.text.x=element_text(angle=90, hjust=1))

# boxplot of category and household income within 20 mi of a store
ggplot(data = descriptives, mapping = aes(x = cat.desc, y = household.income.20mi)) +
  geom_boxplot() +
  labs(title="Product Category by Household Income within 20 mi of Store") +
  theme(axis.text.x=element_text(angle=90, hjust=1))

# boxplot of class and household income within 10 mi
ggplot(data = descriptives, mapping = aes(x = class.desc, y = household.income.10mi)) +
  geom_boxplot() +
  labs(title="Product Class by Household Income within 10 mi of Store")
theme(axis.text.x=element_text(angle=90, hjust=1))

# boxplot of category and household income within 10 mi
ggplot(data = descriptives, mapping = aes(x = cat.desc, y = household.income.10mi)) +
  geom_boxplot() +
  labs(title="Product Category by Household Income within 10 mi of Store") +
  theme(axis.text.x=element_text(angle=90, hjust=1))

# boxplot of class description and numerical competitive index
ggplot(data = descriptives, mapping = aes(x = class.desc, y = compt.index)) +
  geom_boxplot() +
  labs(title="Product Class by Store Competition")

# boxplot of category description and numerical competitive index
ggplot(data = descriptives, mapping = aes(x = cat.desc, y = compt.index)) +
  geom_boxplot() +
  labs(title="Product Category by Store Competition") +
  theme(axis.text.x=element_text(angle=90, hjust=1))

# boxplot of class and abc indicator
ggplot(data = descriptives, mapping = aes(x = class.desc, y = abc.indicator)) +
  geom_boxplot() +
  labs(title="Product Class by ABC Classification") +
  theme(axis.text.x=element_text(angle=90, hjust=1))

# boxplot of category and abc indicator
ggplot(data = descriptives, mapping = aes(x = cat.desc, y = abc.indicator)) +
  geom_boxplot() +
  labs(title="Product Category by ABC Classification") +
  theme(axis.text.x=element_text(angle=90, hjust=1))


# correlation between 2 columns
cor.test(descriptives$orders.dol, descriptives$orders.units)
cor.test(descriptives$orders.dol, descriptives$compt.index)
cor.test(descriptives$orders.dol, descriptives$households.10mi)
cor.test(descriptives$orders.dol, descriptives$household.income.20mi)
cor.test(descriptives$orders.dol, descriptives$household.income.10mi)
cor.test(descriptives$orders.dol, descriptives$households.20mi)
cor.test(descriptives$orders.dol, descriptives$Households)
cor.test(descriptives$orders.dol, descriptives$store.sf)
cor.test(descriptives$orders.dol, descriptives$showroom.sf)
cor.test(descriptives$orders.dol, descriptives$MedianAge)
cor.test(descriptives$orders.dol, descriptives$BlackorAfricanAmericanPopulation)
cor.test(descriptives$orders.dol, descriptives$WhitePopulation)
cor.test(descriptives$orders.dol, descriptives$AsianPopulation)
cor.test(descriptives$orders.dol, descriptives$AmericanIndianorAlaskanNativePopulation)
cor.test(descriptives$orders.dol, descriptives$NativeHawaiianorotherPacificIslanderPopulation)
cor.test(descriptives$orders.dol, descriptives$OtherPopulation)
cor.test(descriptives$orders.dol, descriptives$Temperature)




