# This code is to recommend stores for articles. So if we pass the article ID to the model, it will recommend stores where it can sell.

#Below 2 files are pre-requisite. final_master variable used in this analysis comes from the below 2 R files.

#execute 1_DataCleanup_Final.R
#Execute 2_Final_Master

# https://rpubs.com/jt_rpubs/285729

################################################Build Dataset for recommendation model########################################

#Once the 2 R files are executed, final_master variable is available. This variable has the source data for the analysis
#Usage of final_master variable was not mandatory. Even final_sales variable would have been sufficient. final_master was used because it was considered as the single source for all analysis in the project.


#A new variable is created called Store_TOT_Norm.It has 3 columns, store id, article id and sum of order dollars.
#Dont know if we should have used order dollars or order units for this analysis. Order dollars would be bias towards expensive products whereas order units would be bias towards smaller sized products.
#We went with the dollars .


final_master %>% 
  group_by(store,article) %>% 
  summarise(article_DOLLARS = sum(orders.dol)) -> Store_TOT_Norm 

#Joined it with final_art (Article master) table. 
#This is not required but since articleID is just a number, the next two statements are to concatenate class description with article ID just so we can visually see what category of article it is recommending

Store_TOT_Norm <- Store_TOT_Norm %>%
  left_join(final_art, by = c('article' = 'article')) %>%
  select(store,article,article_DOLLARS,cat.desc)


#Concatenation with category description happens in next step
Store_TOT_Norm$article <- paste(Store_TOT_Norm$cat.desc, Store_TOT_Norm$article, sep = '_')

#removing all the unwanted columns that got added due to join with final_art table. Just keeping 3 columns - store, article and sum of total dollars
Store_TOT_Norm <- Store_TOT_Norm[1:3]


#This spreads the data so that article ID stays in rows but store ID moves to columns.
#It provides the matrix required for recommendation model. 
Store_TOT_Norm <- Store_TOT_Norm %>%
  spread(store, article_DOLLARS)

#The above stmt has created the matrix. next step is to normalise the data. Create columns with row total and row means
Store_TOT_Norm$RowTOTAL <- rowSums(Store_TOT_Norm[,2:382],na.rm = TRUE)
Store_TOT_Norm$RowMEAN <- Store_TOT_Norm$RowTOTAL/381
Store_TOT_Norm$RowCount <- apply(Store_TOT_Norm, 1, function(x) sum(is.na(x)))

#delete rows that have 300 or more null values. This is for those articles which don't see in any store
Store_TOT_Norm <- Store_TOT_Norm %>%
  filter(RowCount < 300)

Col_count <- ncol(Store_TOT_Norm)

#Before running the matrix through recommendation model, the order dollars needs to be normalized so that it acts as the ratings.
#Ratings are calculated by below formula
#(Order dollars for a store and article combination MINUS mean Order Dollars for an article) DIVIDED BY total order dollars for an article  
#ITs basically calculating standard deviation. Store_TOT_Norm variable used in below query comes from 2_Final_Master.R file

for(j in 2 : (Col_count-3))
{
  print(j)
  Store_TOT_Norm[,j] <- (Store_TOT_Norm[,j] - Store_TOT_Norm$RowMEAN) / Store_TOT_Norm$RowTOTAL
}

#Keeping articles in a seperate dataset along with the derived columns - total, mean and count
Article_Total <- Store_TOT_Norm[1]

Article_Total <- Article_Total %>%
  left_join(Store_TOT_Norm, by=c("article" = "article") )%>%
  select(article, RowTOTAL, RowMEAN,RowCount)



#Removed Rowmean,  Rowtotal and Rowcount columns
Store_TOT_Norm <- Store_TOT_Norm[1:382]

#Storing the first column - Article list
ArticleList = Store_TOT_Norm[1]

#Removing the first column - Article list
Store_TOT_Norm$article <- NULL


###############################################Building the recommendation model###################################

#Code below referenced from # https://rpubs.com/jt_rpubs/285729

#Convert the dataframe to a matrix
Store_TOT_Norm_Recommend_rmat <- as.matrix(Store_TOT_Norm)
Store_TOT_Norm_Recommend_rmat <- as(Store_TOT_Norm_Recommend_rmat,"realRatingMatrix")

#split the data into 80-20
Store_TOT_Norm_e <- evaluationScheme(Store_TOT_Norm_Recommend_rmat, method="split", train=0.8, given=20, goodRating=0)


#tried 3 different recommendation techniques
Store_TOT_Norm_UBCF_Z_C <- Recommender(getData(Store_TOT_Norm_e), "UBCF", 
                                      param=list(normalize = "Z-score",method="Cosine"))

Store_TOT_Norm_UBCF_Z_E <- Recommender(getData(Store_TOT_Norm_e), "UBCF", 
                                      param=list(normalize = "Z-score",method="Euclidean"))

Store_TOT_Norm_UBCF_Z_P <- Recommender(getData(Store_TOT_Norm_e), "UBCF", 
                                      param=list(normalize = "Z-score",method="pearson"))


Store_TOT_Norm_p1 <- predict(Store_TOT_Norm_UBCF_Z_C, getData(Store_TOT_Norm_e, "known"), type="ratings")

Store_TOT_Norm_p2 <- predict(Store_TOT_Norm_UBCF_Z_E, getData(Store_TOT_Norm_e, "known"), type="ratings")

Store_TOT_Norm_p3 <- predict(Store_TOT_Norm_UBCF_Z_P, getData(Store_TOT_Norm_e, "known"), type="ratings")

#compare the results
Store_TOT_Norm_error_UCOS <- rbind(
  Store_TOT_Norm_UBCF_Z_C = calcPredictionAccuracy(Store_TOT_Norm_p1, getData(Store_TOT_Norm_e, "unknown")),
  Store_TOT_Norm_UBCF_Z_E = calcPredictionAccuracy(Store_TOT_Norm_p2, getData(Store_TOT_Norm_e, "unknown")),
  Store_TOT_Norm_UBCF_Z_P = calcPredictionAccuracy(Store_TOT_Norm_p3, getData(Store_TOT_Norm_e, "unknown"))
)
Store_TOT_Norm_error_UCOS


#Store_TOT_Norm_UBCF_Z_P is lowest. Using PEarson for prediction
Store_TOT_Norm_UBCF_Z_P_Final <- Recommender(Store_TOT_Norm_Recommend_rmat, "UBCF", 
                                            param=list(normalize = "Z-score",method="pearson"))


#Create a new dataframe to store final results. Created 3 columns. Recommended Rating is the rating provided by recommendation model
Final_StoreRecommendationForArticle <- data.frame(article = character(), RecommendedStore = character(), RecommendedRating = numeric())

Final_StoreRecommendationForArticle$article <- as.character(Final_StoreRecommendationForArticle$article)
Final_StoreRecommendationForArticle$RecommendedStore <- as.character(Final_StoreRecommendationForArticle$RecommendedStore)
Final_StoreRecommendationForArticle$RecommendedRating <- as.numeric(Final_StoreRecommendationForArticle$RecommendedRating)

numberofRecommendations <- 5

#573 is number of articles
for(i in 1:573) {
  print(i)
  p1 <- predict(Store_TOT_Norm_UBCF_Z_P_Final, Store_TOT_Norm_Recommend_rmat[i], n = numberofRecommendations)
  for(j in 1:numberofRecommendations){
    Final_StoreRecommendationForArticle[nrow(Final_StoreRecommendationForArticle) + 1,1] <- as.character(Article_Total[i,1])  
    Final_StoreRecommendationForArticle[nrow(Final_StoreRecommendationForArticle),2] <- p1@itemLabels[p1@items[[1]][j]] #Recommended store
    Final_StoreRecommendationForArticle[nrow(Final_StoreRecommendationForArticle),3] <- p1@ratings[[1]][j]   #Recommended store's rating
  }
  p1 <- NULL
}

#This dataset has normalised rating. 
View(Final_StoreRecommendationForArticle)


#Below 2 statements are to convert the normalised recommended rating back to recommended order dollars
Final_StoreRecommendationForArticle <- Final_StoreRecommendationForArticle %>%
  left_join(Article_Total, by = c('article' = 'article'))

Final_StoreRecommendationForArticle <- Final_StoreRecommendationForArticle %>%
  mutate(ExpectedSales = (RowTOTAL * RecommendedRating) + RowMEAN)

View(Final_StoreRecommendationForArticle)


Final_StoreRecommendationForArticle$RowTOTAL <- NULL
Final_StoreRecommendationForArticle$RowMEAN <- NULL
Final_StoreRecommendationForArticle$RowCount <- NULL
Final_StoreRecommendationForArticle$RecommendedRating <- NULL

library(openxlsx)
write.xlsx(Final_StoreRecommendationForArticle, '/Users/virajraje/Documents/Viraj/Masters/VCU/StudyMaterial/GitRepo/LumberVCU/Data/DerivedFiles/Final_CollaborativeFiltering_StoreRecommendationForArticle.xlsx')


##################################################



