#Objective is to implement apriorialgorithm on sales data.

#Load sales and article(SKU) files for LL 
library(xlsx)
sales <- read.csv("/Users/virajraje/Documents/Viraj/Masters/VCU/StudyMaterial/Sem3/Practicum_LumberLiquidator/DAPT_Data/SalesData_DAPTFall19_2019-09-04_CONFIDENTIAL NOT FOR DISTRIBUTION.csv")
art <- read.xlsx("/Users/virajraje/Documents/Viraj/Masters/VCU/StudyMaterial/Sem3/Practicum_LumberLiquidator/DAPT_Data/ArtMaster_DAPTFall19_2019-09-12_CONFIDENTIAL NOT FOR DISTRIBUTION.xlsx",sheetIndex = 1)

#arules package was used to implement apriori
#install.packages('arules')
library(arules)
library("dplyr")

sapply(sales, class)
sapply(art, class)

#Converting the data type to help in joining the files alter
sales$article <- as.character(sales$article)
art$article <- as.character(art$article)

View(head(sales))

#Joining sales and article
salesWithArticleDetails <- sales %>%
  left_join(art, by = c("article" = "article"))

View(head(salesWithArticleDetails))


salesWithCommaSeperatedArticles_intermediate <- salesWithArticleDetails %>%
  group_by(sales.order, class.desc) %>%
  summarise(n = n())

View(head(salesWithCommaSeperatedArticles_intermediate))

sales %>%
  filter(sales.order == "101665621")

salesWithArticleDetails %>%
  filter(sales.order == "101665621")

View(salesWithCommaSeperatedArticles_intermediate %>%
  filter(sales.order == "101665621"))

salesWithCommaSeperatedArticles <- salesWithCommaSeperatedArticles_intermediate %>%
  group_by(sales.order) %>%
  summarise(n = n(), commaSeperatedARticles = toString(class.desc))
  
View(head(salesWithCommaSeperatedArticles))
salesWithCommaSeperatedArticles %>%
  filter(sales.order == "101665621")

nrow(salesWithCommaSeperatedArticles) #1225990
salesWithCommaSeperatedArticles <- salesWithCommaSeperatedArticles %>%
  filter(n > 1)

nrow(salesWithCommaSeperatedArticles) #27640
View(head(salesWithCommaSeperatedArticles))

salesWithCommaSeperatedArticles <- salesWithCommaSeperatedArticles %>% rename(OrderID = sales.order, itemList = commaSeperatedARticles )

View(head(salesWithCommaSeperatedArticles))

salesWithCommaSeperatedArticles <- select(salesWithCommaSeperatedArticles, -c(2))
View(head(salesWithCommaSeperatedArticles))


write.csv(salesWithCommaSeperatedArticles,'/Users/virajraje/Documents/Viraj/Masters/VCU/StudyMaterial/Sem3/Practicum_LumberLiquidator/DAPT_Data/DerivedFiles/salesWithCommaSeperatedArticles.csv') 
#, qoute = FALSE, row.names = TRUE)
txn = read.transactions(file = '/Users/virajraje/Documents/Viraj/Masters/VCU/StudyMaterial/Sem3/Practicum_LumberLiquidator/DAPT_Data/DerivedFiles/salesWithCommaSeperatedArticles.csv', rm.duplicates= TRUE, format="basket",sep=",",cols=1);
View(head(txn))

basket_rules <- apriori(txn,parameter = list(sup = 0.001, conf = 0.001,target="rules"))
inspect(basket_rules)

View(salesWithArticleDetails %>%
  group_by(cat.desc) %>%
  summarise(n = n()) %>%
  arrange(n))

nrow(salesWithArticleDetails)

art %>%
  distinct(cat.desc) %>%
  arrange(cat.desc)

View(sales %>%
  group_by(sales.order) %>%
  summarise(n = n()) %>%
  filter(n > 1)) #118,382

View(sales %>%
  distinct(sales.order)) #1,225,990
#Only 9% customers have boutght more than 1 type of article at a time


#Apriori algorithm was not appropriate for Lumber Liquidator's sales data. 
#Customers seldom buy multiple types of products at the same time. Also non tiles data was not shared with us by LL

####################
# May be try to do it at customer ID level
View(sales %>%
  filter(!sold.to.party %in% c('CSH')) %>%
  group_by(sold.to.party) %>%
  summarise(n = n()) %>%
  filter(n > 1)) #286,000


