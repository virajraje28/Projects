# Set working Drive AKA where your data is stored
setwd('D:/MODEL/DATA')
#Load Packages ---------
library(readxl)
library(dplyr)
library (readr)

#Read ALl the files that were provided by Lumber Liquidators-------------
inventory <- read.csv('AvailInvData_DAPTFall19_2019-09-04_CONFIDENTIAL NOT FOR DISTRIBUTION.csv')
sales <- read.csv('SalesData_DAPTFall19_2019-09-04_CONFIDENTIAL NOT FOR DISTRIBUTION.csv')
web <- read.csv('WebData_DAPTFall19_2019-09-12_CONFIDENTIAL NOT FOR DISTRIBUTION.csv')
art <- read.csv('ArtMaster20200124.csv')
#colnames(art) = c("article","art.desc","old.article","merch.cat","cat.desc","merch.class","class.desc","merch.family","fam.desc","finish.stain","marketing.name","grade","trade.name","width","overall.thickness","created","gbb","ac.rating","installation.type","with.pad","pog.begin.date","pog.end.date","abc.indicator" )

prom <- read_excel('PromoCalendar_CONFIDENTIAL NOT FOR DISTRIBUTION.xlsx',1)
dic <- read_excel('DataDictionary_DAPTFall19_2019-09-12.xlsx',1)
site <- read_excel('SiteMaster_DAPTFall19_2019-09-12_CONFIDENTIAL NOT FOR DISTRIBUTION.xlsx',1)

#Read additional data extracted by VCU team-------------
demographic <- read.csv('DemographicInfo.csv') #See WebScraping_CDX.py file for scraping code
weather <- read.csv('weatherAvg.csv') #See WeatherData.r file for code to create this data set

# Site File cleanup ---------------


sapply(site, class)
site$store <- as.character(site$store)
site$store.desc <- as.character(site$store.desc)
site$zone <- as.character(site$zone)
site$region.desc <- as.character(site$region.desc)
site$primary.dc <- as.character(site$primary.dc)
site$tv.dma <- as.character(site$tv.dma)
site$compt.index <- as.numeric(site$compt.index)
site$address <- as.character(site$address)
site$city <- as.character(site$city)
site$state <- as.character(site$state)
site$postal.code <- as.character(site$postal.code)
site$lon <- as.numeric(site$lon)
site$lat <- as.numeric(site$lat)
site$open.date <- as.Date(site$open.date, origin = "1899-12-30")
site$households.10mi <- as.numeric(site$households.10mi)
site$households.20mi <- as.numeric(site$households.20mi)
site$housing.density <- as.character(site$housing.density)
site$household.income.10mi <- as.numeric(site$household.income.10mi)
site$household.income.20mi <- as.numeric(site$household.income.20mi)
site$store.sf <- as.numeric(site$store.sf)
site$showroom.sf <- as.numeric(site$showroom.sf)
site$compt.status <- as.character(site$compt.status)

### ADD MISSING LAT LONG to store 1407 & 1408
site$lat[site$store == '2002'] <- 39.421723
site$lon[site$store == '1407'] <- -87.415582
site$lat[site$store == '1408'] <- 39.884961
site$lon[site$store == '1408'] <- -104.973153

### Update OPEN Date for stores 1407, 1408, 1417, 1420

### Cleanup Housing density column - Replace text NA to NA
site <- site %>%
  mutate(housing.density = na_if(housing.density, "NA"))


### GOTTA TAKE OUT CANADIAN STORES, temporary stores and stores opened before Jan 1 2018
site_Cleanup1 <- site %>%
  #Remove Canada stores
  filter(as.integer(store) < 2000) %>%
  #Remove temporary stores (1700 ones)
  filter(as.integer(store) < 1700 | as.integer(store) >= 1800) %>%
  #Remove stores opened after Jan 2008
  filter(open.date < as.Date('2018-01-01'))
  
final_site <- site_Cleanup1





#Article master -----------
#Manually updated the excel to update blanks to NA

art <- art %>% mutate(gbb = na_if(gbb, ""))
art <- art %>% mutate(grade = na_if(grade, ""))
art <- art %>% mutate(width = na_if(width, ""))
art <- art %>% mutate(overall.thickness = na_if(overall.thickness, ""))
art <- art %>% mutate(trade.name = na_if(trade.name, ""))
art <- art %>% mutate(ac.rating = na_if(ac.rating, ""))
art <- art %>% mutate(installation.type = na_if(installation.type, ""))
art <- art %>% mutate(with.pad = na_if(with.pad, ""))

colnames(art) <- c("article","art.desc","old.article","merch.cat","cat.desc","merch.class","class.desc","merch.family","fam.desc","finish.stain","marketing.name","grade","trade.name","width","overall.thickness","created","gbb","ac.rating","installation.type","with.pad","pog.begin.date","pog.end.date","abc.indicator")
art$article <- as.character(art$article)
art$art.desc <- as.character(art$art.desc)
art$old.article <- as.character(art$old.article)
art$merch.cat <- as.character(art$merch.cat)
art$cat.desc <- as.character(art$cat.desc)
art$merch.class <- as.character(art$merch.class)
art$class.desc <- as.character(art$class.desc)
art$merch.family <- as.character(art$merch.family)
art$fam.desc <- as.character(art$fam.desc)
art$finish.stain <- as.character(art$finish.stain)
art$marketing.name <- as.character(art$marketing.name)
art$grade <- as.character(art$grade)
art$trade.name <- as.character(art$trade.name)
art$width <- as.character(art$width)
art$overall.thickness <- as.character(art$overall.thickness)
art$created <- as.Date(art$created)
art$gbb <- as.character(art$gbb)
art$ac.rating <- as.character(art$ac.rating)
art$installation.type <- as.character(art$installation.type)
art$with.pad <- as.character(art$with.pad)
art$pog.begin.date <- as.Date(art$pog.begin.date)
art$pog.end.date <- as.Date(art$pog.end.date)
art$abc.indicator <- as.character(art$abc.indicator)

final_art <- art

# Inventory File cleanup ---------------

#Match datatypes with excel
sapply(inventory, class)
inventory$store <- as.character(inventory$store)
inventory$article <- as.character(inventory$article)
inventory$xsite.status <- as.character(inventory$xsite.status)
inventory$inv.dol <- as.numeric(inventory$inv.dol)
inventory$inv.units <- as.numeric(inventory$inv.units)
inventory$date.inv.measured <- as.Date(inventory$date.inv.measured)

#Add additional columns for codes/abbreviations. xsite.status --> inventory code
inventory$inventory.code[inventory$xsite.status == "OP"] <- "Out of Program"
inventory$inventory.code[inventory$xsite.status == "BL"] <- "Blocked"
inventory$inventory.code[inventory$xsite.status == "WO"] <- "Write Off"
inventory$inventory.code[inventory$xsite.status == "GI"] <- "Going In"
inventory$inventory.code[inventory$xsite.status == "GO"] <- "Going Out"
inventory$inventory.code[inventory$xsite.status == "OD"] <- "Odd Lot"
inventory$inventory.code[inventory$xsite.status == "SP"] <- "Special Purchase"
inventory$inventory.code[inventory$xsite.status == "IP"] <- "In Program"

#delete unwanted items
inventory_Cleanup1 <- inventory %>%
                      #Keep only items with inventory code - GI, IP, GO
                      #filter(xsite.status == 'GI' | xsite.status == 'IP' | xsite.status == 'GO') %>%
                      #Remove Canada stores
                      filter(as.integer(store) < 2000) %>%
                      #Remove temporary stores (1700 ones)
                      filter(as.integer(store) < 1700 | as.integer(store) >= 1800)

inventory_Cleanup2 <- inventory_Cleanup1 %>%
                      #Remove stores that are opened after 2008 and #Remove stores that are not in site file. Joining with final_site object takes care of both the scenarios.
                      inner_join(final_site, c = ("store" = "store")) %>%
                      select(everything(inventory_Cleanup1))
  
final_inventory <- inventory_Cleanup2

final_inventory$inv.cost <- final_inventory$inv.dol/final_inventory$inv.units


#Sales file cleanup ------------
sapply(sales, class)

sales <- sales %>% mutate(install.date = na_if(install.date, ""))

sales$day <- as.Date(sales$day)
sales$store <- as.character(sales$store)
sales$ship.from.site <- as.character(sales$ship.from.site)
sales$primary.dc <- as.character(sales$primary.dc)
sales$sold.to.party <- as.character(sales$sold.to.party)
sales$cust.type <- as.character(sales$cust.type)
sales$article <- as.character(sales$article)
sales$sales.order <- as.character(sales$sales.order)
sales$orders.dol <- as.numeric(sales$orders.dol)
sales$orders.units <- as.numeric(sales$orders.units)
sales$asp <- as.numeric(sales$asp)
sales$install.date <- as.Date(sales$install.date)
sales$install.order <- as.logical(sales$install.order)
sales$pro <- as.logical(sales$pro)



sales <- sales %>% 
  mutate(day = as.Date(day),
         install.date = as.Date(install.date),
         customer.type = case_when(cust.type == '#'|cust.type == '1A' ~ 'Non-Pro',
                                   cust.type == '1B' ~ 'Public Spaces',
                                   cust.type == '1C' ~ 'Installer',
                                   cust.type == '1D' ~ 'Remodeler',
                                   cust.type == '1E' ~ 'Res.Property.Mgr',
                                   cust.type == '1F' ~ 'Influencer',
                                   cust.type == '1G' ~ 'Builder',
                                   cust.type == '1H' ~ 'Wholesaler',
                                   cust.type == '1I' ~ 'International',
                                   TRUE ~ 'NA'))  # change to NA


#Removed sales from stores not in store file  
final_sales <- sales %>%
  #Remove stores that are opened after 2008 and #Remove stores that are not in site file. Joining with final_site object takes care of both the scenarios.
  inner_join(final_site, c = ("store" = "store")) %>%
  select(everything(sales))

#remove sales for articles that are either not available in article file or are in the article file with GBB = Out of program(OPP) or DEAL
#Final_art file takes care of both the above conditions
final_sales <- final_sales %>%
  #Remove stores that are opened after 2008 and #Remove stores that are not in site file. Joining with final_site object takes care of both the scenarios.
  inner_join(final_art, c = ("article" = "article")) %>%
  select(everything(final_sales))


#PRomotion File ------------
prom$Start <- as.Date(prom$Start, origin = "1899-12-30")
prom$End <- as.Date(prom$End, origin = "1899-12-30")
final_prom <- prom

#Cleaning demographic data
#Convert 0 to NA
demographic[demographic == 0] <- NA
#zipcode is saved as integer. convert it to character

demographic$Zipcode <- as.character(demographic$Zipcode)
demographic$Zipcode <- sapply(demographic$Zipcode, function(x) paste0(paste0(rep("0",5 - nchar(x)), collapse = ""), x))

final_demographic <- demographic

#remove duplicates
final_demographic <- final_demographic %>%
  distinct_all()

final_demographic$Zipcode[final_demographic$Zipcode == "14642"] <- "14623"
final_demographic$Zipcode[final_demographic$Zipcode == "16063"] <- "16066"
final_demographic$Zipcode[final_demographic$Zipcode == "44513"] <- "44512"
final_demographic$Zipcode[final_demographic$Zipcode == "50325"] <- "50322"
final_demographic$Zipcode[final_demographic$Zipcode == "14043"] <- "14225"
final_demographic$Zipcode[final_demographic$Zipcode == "44061"] <- "44060"
final_demographic$Zipcode[final_demographic$Zipcode == "32809"] <- "32839"
final_demographic$Zipcode[final_demographic$Zipcode == "44145"] <- "44070"
final_demographic$Zipcode[final_demographic$Zipcode == "73128"] <- "73127"
final_demographic$Zipcode[final_demographic$Zipcode == "76202"] <- "76205"
final_demographic$Zipcode[final_demographic$Zipcode == "07033"] <- "07083"
final_demographic$Zipcode[final_demographic$Zipcode == "55306"] <- "55337"


###########################################Adding weather data #############################

weather <- weather[2:7]
names(weather)[names(weather) =="Address"] <- "Zipcode"

weather$Zipcode <- as.character(weather$Zipcode)
weather$Zipcode <- sapply(weather$Zipcode, function(x) paste0(paste0(rep("0",5 - nchar(x)), collapse = ""), x))

final_weather <- weather





rm(art)
rm(site)
rm(sales)
rm(prom)
rm(weather)
rm(inventory)
#rm(site_Cleanup1)
rm(inventory_Cleanup1)
rm(inventory_Cleanup2)
rm(demographic)


setwd('D:/MODEL')
save.image('RDATA/DataCleanup_Final.RData')

