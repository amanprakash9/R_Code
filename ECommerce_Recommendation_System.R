
#Packages to be used:
#recommenderlab
#ggplot



#Loading the required libraries.
library(dplyr)
library(recommenderlab)
library(ggplot2)


#loading the beer data and converting blank values to NA.
beer_data <- read.csv("beer_data.csv", stringsAsFactors = F , na.strings =   c("","NA") )
#475984 variables

#Data Cleaning
sum(is.na(beer_data))
#100 NA values 

#Removing the rows having NA values
beer_data <- na.omit(beer_data)

#Reoving the duplicates based on beer id reviewer name with the assumption that
#one beer should be rated once this will avoid rating rise in real rating matrix
beer_data <- beer_data[!duplicated(beer_data[,c(1,2)]),]

#checking the summary
summary(beer_data)

#Ques 1.1 Choose only those beers that have at least N number of reviews

tt<- beer_data %>% group_by(beer_beerid) %>% 
  summarise(freq=n()) %>% arrange(desc(freq))

#finding the summary of data frame
summary(tt)

#creating histogram to find value of N for frequency
hist(tt$freq,breaks=80)

#We find most of values lie under 100 from histogram so it would be good to take 
#value as N considering performance tradeoff of evaltuion this value is neither
#too high nor too low and 10% max value

#subseting the data frame with minimum 100 values
tt <- subset(tt, tt$freq > 100)

#Similarly Choosing only those beers that have at least N number of reviews
tt1<- beer_data %>% group_by(review_profilename) %>% 
  summarise(freq=n()) %>% arrange(desc(freq))

#creating histogram to find value of N for frequency
hist(tt1$freq,breaks=80)

#finding the summary of data frame
summary(tt1)

#We find most of values lie under 50 from histogram so it would be good to take 
#value as N considering performance tradeoff of evaltuion

#subseting the data frame with minimum 50 values
tt1 <- subset(tt1, tt1$freq > 50)

#merging the data frames
beer_df_upd <- merge(beer_data, tt , by.x="beer_beerid",by.y="beer_beerid" )
beer_df_updated <- merge(beer_df_upd, tt1 , by.x="review_profilename",by.y="review_profilename" )



summary(beer_df_updated)


#Ques 1.2 Convert this data frame to a "realratingMatrix"
#before you build your collaborative filtering models


#creating real rating matrix
beer_realrating <- as(beer_df_updated, "realRatingMatrix")
summary(getRatings(beer_realrating))



# coerce the matrix to a dataframe which will be used for data exploration
beer_df <- as(beer_realrating, "data.frame")
str(beer_df)



#Ques 2.1 Determine how similar the first ten users are
#with each other and visualise it


#How similar are the first ten users are with each other
similar_users <- similarity(beer_realrating[1:10, ],
                               method = "cosine",
                               which = "users")

#checking the structure
str(similar_users)

#Similarity matrix
as.matrix(similar_users)

#Visualise similarity matrix
image(as.matrix(similar_users), main = "reviewer similarity")

#Inference 
#Users 7, 2 also 3, 6 are similar 

#Ques 2.2 Compute and visualise the similarity between the first 10 beers

#How similar are the first five items are with each other
similar_items <- similarity(beer_realrating[ ,1:10],
                            method = "cosine",
                            which = "items")
#checking the structure
str(similar_items)

as.matrix(similar_items)
image(as.matrix(similar_items), main = "Item similarity")

#Inference 
#You can notice more items being similar 
#:Eg items 7 and 8 are similar; 2 and 3 are similar 

#Ques 2.3 What are the unique values of ratings?
unique(beer_df$rating)

# 5.0 3.0 4.0 4.5 2.5 3.5 2.0 1.0 1.5

#--------------------------Understand users and ratings----------#



#Ques 2.4.1 Visualizing ratings The average beer ratings
beer_ratings_avg <- beer_df %>% group_by(item) %>% 
summarise(avg_rating =mean(rating))

summary(beer_ratings_avg$avg_rating)
#mean is 3.84 can be considered as average beer rating

#visualising the same
ggplot(beer_ratings_avg, aes(x=avg_rating)) + geom_histogram()

#Ques 2.4.2 Visualizing ratings The average user ratings  
beer_ratings_avg_user <- beer_df %>% group_by(user) %>% 
summarise(avg_rating =mean(rating))
  
summary(beer_ratings_avg_user$avg_rating)
#mean is 3.904 can be considered as average user rating

#visualising the same
ggplot(beer_ratings_avg_user, aes(x=avg_rating)) + geom_histogram()

#Ques 2.4.3 Visualizing the average number of ratings given to the beers   
beer_ratings_count_avg <- beer_df_updated %>% group_by(beer_beerid) %>% 
summarise(avg_reviews =mean(freq.x))
  
summary(beer_ratings_count_avg$avg_reviews)
#mean is 220 can be considered as average number of rating in our dataframe
#which considers mimimum no of beer reviewed and minimum number of review by user


ggplot(beer_ratings_count_avg, aes(x=avg_reviews)) + geom_histogram()
  
#Ques 2.4.4 Visualizing the average number of ratings given to the beers  
user_ratings_count_avg <- beer_df_updated %>% group_by(review_profilename) %>% 
summarise(avg_reviews =mean(freq.y))
  
summary(user_ratings_count_avg$avg_reviews)
#mean is 160 can be considered as average number of rating in our dataframe
#which considers mimimum no of beer reviewed and minimum number of review by user

#visulaising the same
  ggplot(user_ratings_count_avg, aes(x=avg_reviews)) + geom_histogram()
  
  
#List of models available
recommender_models <- recommenderRegistry$get_entries(dataType = "realRatingMatrix")
names(recommender_models)# 9 types of models


#description of recommendation system algorithms/models used
lapply(recommender_models, "[[", "description")

#This gives you different types of recommendation models
#In this case study , let's compare user based and item based
#collaborative filtering

#checking the parameters of these two models
recommender_models$IBCF_realRatingMatrix$parameters

#Ques 3.1   Divide your data into training and testing datasets
#Experiment with 'split' and 'cross-validation' evaluation schemes


#--arguments
#train and test
#Here we create an evaluation scheme which splits the users 
#into a training set (70%) and a test set (30%). 

#
#With goodRating=4 all items with actual user rating of greater or equal 4 are considered 
#positives in the evaluation process


#Divide data into test split method
scheme <- evaluationScheme(beer_realrating, method = "split", train = .7,
                           k = 1, given = -1, goodRating = 4)


#Divide data into test cross-validation method with number of fold as 4.
cross_scheme <- evaluationScheme(beer_realrating, method = "cross", train = .7,
                           k = 4, given = -1, goodRating = 4)



scheme
cross_scheme

#Ques 3.2 Build IBCF and UBCF models
algorithms <- list(
  "user-based CF" = list(name="UBCF", param=list(normalize = "Z-score",
                                                 method="Cosine",
                                                 nn=30)),
  "item-based CF" = list(name="IBCF", param=list(normalize = "Z-score"
  ))
)

#Ques 3.3 Compare the performance of the two models and suggest the one that should be deployed
#Plot the ROC curves for UBCF and IBCF and compare them

#split method
# run algorithms, predict next n movies
results_split <- evaluate(scheme, algorithms, n=c(1, 3, 5, 10, 15, 20))
class(results_split)

# Draw ROC curve
plot(results_split, annotate = 1:4, legend="topleft")

#inference
#UBCF is better then IBCF having greater postive as depicted by ROC curve
#for split method

#cross validation method 
# run algorithms, predict next n movies
results_cross <- evaluate(cross_scheme, algorithms, n=c(1, 3, 5, 10, 15, 20))
class(results_cross)

# Draw ROC curve
plot(results_cross, annotate = 1:4, legend="topleft")

#inference
#UBCF is better then IBCF having greater postive as depicted by ROC curve
#for cross validation method aswell.


#Ques 3.4 Give the names of the top 5 beers that you would recommend to the 
#users "cokes", "genog" & "giblet"

#building recommedation model
recommend_ubcf <- Recommender(beer_realrating, method="UBCF")


recommend_ubcf_cokes <- predict(recommend_ubcf, beer_realrating['cokes'] , n=5) 

as(recommend_ubcf_cokes,"list")
#inference below beer id can be recmmended to cokes.
#"47658" "19960" "7971"  "41815" "1826" 


recommend_ubcf_genog <- predict(recommend_ubcf, beer_realrating['genog'] , n=5) 

as(recommend_ubcf_genog,"list")
#inference below beer id can be recmmended to genog.
#"56973" "782"   "92"    "1545"  "1309" 


recommend_ubcf_genog <- predict(recommend_ubcf, beer_realrating['giblet'] , n=5) 

as(recommend_ubcf_genog,"list")
#inference below beer id can be recmmended to giblet.
#"19960" "1545"  "10325" "571"   "59"  

