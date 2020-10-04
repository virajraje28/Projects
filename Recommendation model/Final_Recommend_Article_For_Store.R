# This code is to recommend articles for stores. So if we pass the store ID to the model, it will recommend articles.

#Below 2 files are pre-requisite. final_master variable used in this analysis comes from the below 2 R files.

#execute 1_DataCleanup_Final.R
#Execute 2_Final_Master.R
# https://rpubs.com/jt_rpubs/285729

################################################Build Dataset for recommendation model########################################

#Once the 2 R files are executed, final_master variable is available. This variable has the source data for the analysis
#Usage of final_master variable was not mandatory. Even final_sales variable would have been sufficient. final_master was used because it was considered as the single source for all analysis in the project.


#A new variable is created called ART_TOT_Norm.It has 3 columns, store id, article id and sum of order dollars.
#Dont know if we should have used order dollars or order units for this analysis. Order dollars would be bias towards expensive products whereas order units would be bias towards smaller sized products.
#We went with the dollars 

final_master %>% 
  group_by(store,article) %>% 
  summarise(article_DOLLARS = sum(orders.dol)) -> ART_TOT_Norm 

#Joined it with final_art (Article master) table. 
#This is not required but since articleID is just a number, the next two statements are to concatenate class description with article ID just so we can visually see what category of article it is recommending
ART_TOT_Norm <- ART_TOT_Norm %>%
  left_join(final_art, by = c('article' = 'article')) %>%
  select(store,article,article_DOLLARS,cat.desc)

#Concatenation with category description happens in next step
ART_TOT_Norm$article <- paste(ART_TOT_Norm$cat.desc, ART_TOT_Norm$article, sep = '_')

#removing all the unwanted columns that got added due to join with final_art table. Just keeping 3 columns - store, article and sum of total dollars
ART_TOT_Norm <- ART_TOT_Norm[1:3]

#This spreads the data so that store ID stays in rows but article ID moves to columns.
#It provides the matrix required for recommendation model. 
ART_TOT_Norm <- ART_TOT_Norm %>%
  spread(article, article_DOLLARS)


#Before running the matrix through recommendation model, the order dollars needs to be normalized so that it acts as the ratings.
#Ratings are calculated by below formula
#(Order dollars for a store and article combination MINUS mean Order Dollars for a store) DIVIDED BY total order dollars for a store  
#ITs basically calculating standard deviation. STORE_TOT variable used in below query comes from 2_Final_Master.R file

Col_count <- ncol(ART_TOT_Norm)

for(j in 2:Col_count)
{
  print(j)
  ART_TOT_Norm[,j+Col_count-1] <- (ART_TOT_Norm[,j] - STORE_TOT[,3]) / STORE_TOT[,2]
  names(ART_TOT_Norm)[j+Col_count-1] <- paste(names(ART_TOT_Norm)[j],"norm",sep = '_')
}

#The for loop above has added normalized as well as non normalized columns. Create a dataframe with only normalized columns
Art_Recommend_DF <- select(ART_TOT_Norm, contains("norm"))

#Keep the store list in a separate variable and delete it from the final dataset.
StoreList = as.data.frame(Art_Recommend_DF$store)
Art_Recommend_DF$store <- NULL

#Art_Recommend_DF is the final dataset that will be used by the recommendation model. 
#It just has a column for each article.

###############################################Building the recommendation model###################################

#Code below referenced from # https://rpubs.com/jt_rpubs/285729

#Convert the dataframe to a matrix
Art_Recommend_rmat <- as.matrix(Art_Recommend_DF)
Art_Recommend_rmat <- as(Art_Recommend_rmat,"realRatingMatrix")

#split the data into 80-20
Art_Recommend_e <- evaluationScheme(Art_Recommend_rmat, method="split", train=0.8, given=50, goodRating=0)


#tried 3 different recommendation techniques
Art_Recommend_UBCF_Z_C <- Recommender(getData(Art_Recommend_e,"train"), "UBCF", 
                                      param=list(normalize = "Z-score",method="Cosine"))

Art_Recommend_UBCF_Z_E <- Recommender(getData(Art_Recommend_e,"train"), "UBCF", 
                                      param=list(normalize = "Z-score",method="Euclidean"))

Art_Recommend_UBCF_Z_P <- Recommender(getData(Art_Recommend_e,"train"), "UBCF", 
                                      param=list(normalize = "Z-score",method="pearson"))


Art_Recommend_p1 <- predict(Art_Recommend_UBCF_Z_C, getData(Art_Recommend_e, "known"), type="ratings")

Art_Recommend_p2 <- predict(Art_Recommend_UBCF_Z_E, getData(Art_Recommend_e, "known"), type="ratings")

Art_Recommend_p3 <- predict(Art_Recommend_UBCF_Z_P, getData(Art_Recommend_e, "known"), type="ratings")

#compare the results
Art_Recommend_error_UCOS <- rbind(
  Art_Recommend_UBCF_Z_C = calcPredictionAccuracy(Art_Recommend_p1, getData(Art_Recommend_e, "unknown")),
  Art_Recommend_UBCF_Z_E = calcPredictionAccuracy(Art_Recommend_p2, getData(Art_Recommend_e, "unknown")),
  Art_Recommend_UBCF_Z_P = calcPredictionAccuracy(Art_Recommend_p3, getData(Art_Recommend_e, "unknown"))
)
Art_Recommend_error_UCOS

#Art_Recommend_UBCF_Z_P is lowest. Using PEarson for prediction
Art_Recommend_UBCF_Z_P_Final <- Recommender(Art_Recommend_rmat, "UBCF", 
                                            param=list(normalize = "Z-score",method="pearson"))

numberofRecommendations <- 5

#Create a new dataframe to store final results. Created 3 columns. Recommended Rating is the rating provided by recommendation model
Final_ArticleRecommendationInStore <- data.frame(store = character(), RecommendedArticle = character(), RecommendedRating = numeric())
Final_ArticleRecommendationInStore$store <- as.character(Final_ArticleRecommendationInStore$store)
Final_ArticleRecommendationInStore$RecommendedArticle <- as.character(Final_ArticleRecommendationInStore$RecommendedArticle)
Final_ArticleRecommendationInStore$RecommendedRating <- as.numeric(Final_ArticleRecommendationInStore$RecommendedRating)

#381 - number of stores
for(i in 1:381) {
  print(i)
  p1 <- predict(Art_Recommend_UBCF_Z_P_Final, Art_Recommend_rmat[i], n = numberofRecommendations)
  for(j in 1:numberofRecommendations){
    Final_ArticleRecommendationInStore[nrow(Final_ArticleRecommendationInStore) + 1,1] <- as.character(StoreList[i,1])  
    Final_ArticleRecommendationInStore[nrow(Final_ArticleRecommendationInStore),2] <- p1@itemLabels[p1@items[[1]][j]]  #Recommended Article
    Final_ArticleRecommendationInStore[nrow(Final_ArticleRecommendationInStore),3] <- p1@ratings[[1]][j]    #Recommended article's rating
  }
  p1 <- NULL
}

#This dataset has normalised rating. 
View(Final_ArticleRecommendationInStore)

#Below 2 statements are to convert the normalised recommended rating back to recommended order dollars
Final_ArticleRecommendationInStore <- Final_ArticleRecommendationInStore %>%
  left_join(STORE_TOT,  by = c("store"="store"))
  
Final_ArticleRecommendationInStore <- Final_ArticleRecommendationInStore %>%
  mutate(ExpectedSales = (ORDER_DOLLARS * RecommendedRating) + Mean_Order_Dollars)


Final_ArticleRecommendationInStore$ORDER_DOLLARS <- NULL
Final_ArticleRecommendationInStore$Mean_Order_Dollars <- NULL
Final_ArticleRecommendationInStore$RecommendedRating <- NULL

library(openxlsx)
write.xlsx(Final_ArticleRecommendationInStore, '/Users/virajraje/Documents/Viraj/Masters/VCU/StudyMaterial/GitRepo/LumberVCU/Data/DerivedFiles/Final_CollaborativeFiltering_ArticleRecommendation.xlsx')



