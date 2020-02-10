# Load packages
library(ggplot2) # visualization
library(ggrepel)
library(ggthemes) # visualization
library(scales) # visualization
library(VIM)
library(data.table)
library(formattable)
library(plotly)
library(corrplot)
library(GGally)
library(caret)
library(car)
library(stringr)
library(plyr)
library(tidytext)
library(wordcloud)
library(reshape2)
library(forcats) 
library(tidyr)
library(igraph)
library(ggraph)
library(dplyr)
library(tree)
library(partykit)
library(randomForest)
library(glmnet)

source("DataAnalyticsFunctions.R")

rawData <- read.csv("movie_metadata.csv")
summary(rawData)
dim(rawData)

# duplicate rows
sum(duplicated(rawData))
# delete duplicate rows
rawData <- rawData[!duplicated(rawData), ]

#clean the title name
typeof(rawData$movie_title)
rawData$movie_title <- gsub("Â", "", as.character(factor(rawData$movie_title)))
rawData$movie_title <-str_trim(rawData$movie_title, side = "right")
rawData$movie_title
#clean the same title leaving the one with largest num of voters
rawData<-ddply(rawData, "movie_title", function(func1) return(func1[func1$num_voted_users==max(func1$num_voted_users),]))
rawData<-ddply(rawData, "movie_title", function(func2) return(func2[func2$cast_total_facebook_likes==max(func2$cast_total_facebook_likes),]))

#Remove the link column
rawData <- subset(rawData, select = -c(movie_imdb_link))
#remove aspect_ration from dataset
rawData <- subset(rawData, select = -c(aspect_ratio))

#Run the following lines to check the effect of aspect ratio on Score
#table(rawData$aspect_ratio)
#rawData$aspect_ratio[is.na(rawData$aspect_ratio)] <- 0
#mean(rawData$rawData_score[rawData$aspect_ratio == 1.85])
#mean(rawData$rawData_score[rawData$aspect_ratio == 2.35])
#mean(rawData$rawData_score[rawData$aspect_ratio != 1.85 & rawData$aspect_ratio != 2.35])

#find missing values
colSums(sapply(rawData, is.na))
#heatmap of visualizing missing value
missing.values <- aggr(rawData, sortVars = T, prop = T, sortCombs = T, cex.lab = 1.5, cex.axis = .6, cex.numbers = 5, combined = F, gap = -.2)
rawData <- rawData[!is.na(rawData$budget), ]
colSums(sapply(rawData, is.na))
#rawData <- subset(rawData, select = -c(gross))
#dim(rawData)
summary(rawData$gross)
#delete rows with missing gross value
#rawData <- rawData[!is.na(rawData$gross), ]
#show the missing-value column
colSums(sapply(rawData, is.na))
# replace NA with column average for facenumber_in_poster
rawData$facenumber_in_poster[is.na(rawData$facenumber_in_poster)] <- round(mean(rawData$facenumber_in_poster, na.rm = TRUE))
# convert 0s into NAs for other predictors
rawData[,c(5,6,8,13,24,26)][rawData[,c(5,6,8,13,24,26)] == 0] <- NA
# impute missing value with column mean
rawData$num_critic_for_reviews[is.na(rawData$num_critic_for_reviews)] <- round(mean(rawData$num_critic_for_reviews, na.rm = TRUE))
rawData$duration[is.na(rawData$duration)] <- round(mean(rawData$duration, na.rm = TRUE))
rawData$director_facebook_likes[is.na(rawData$director_facebook_likes)] <- round(mean(rawData$director_facebook_likes, na.rm = TRUE))
rawData$actor_3_facebook_likes[is.na(rawData$actor_3_facebook_likes)] <- round(mean(rawData$actor_3_facebook_likes, na.rm = TRUE))
rawData$actor_1_facebook_likes[is.na(rawData$actor_1_facebook_likes)] <- round(mean(rawData$actor_1_facebook_likes, na.rm = TRUE))
rawData$cast_total_facebook_likes[is.na(rawData$cast_total_facebook_likes)] <- round(mean(rawData$cast_total_facebook_likes, na.rm = TRUE))
rawData$actor_2_facebook_likes[is.na(rawData$actor_2_facebook_likes)] <- round(mean(rawData$actor_2_facebook_likes, na.rm = TRUE))
rawData$movie_facebook_likes[is.na(rawData$movie_facebook_likes)] <- round(mean(rawData$movie_facebook_likes, na.rm = TRUE))
rawData$num_user_for_reviews[is.na(rawData$num_user_for_reviews)] <- round(mean(rawData$num_user_for_reviews, na.rm = TRUE))
#check again missing values
colSums(sapply(rawData, is.na))
rawData <- rawData[!is.na(rawData$title_year), ]
colSums(sapply(rawData, is.na))

#change the old rating standard to present ones
table(rawData$content_rating)
levels(rawData$content_rating) <- c(levels(rawData$content_rating), "Not Known")
rawData$content_rating[rawData$content_rating == 'M']   <- 'PG' 
rawData$content_rating[rawData$content_rating == 'GP']  <- 'PG' 
rawData$content_rating[rawData$content_rating == 'X']   <- 'NC-17'
rawData$content_rating[rawData$content_rating == 'Approved']  <- 'Not Rated'
rawData$content_rating[rawData$content_rating == 'Passed']    <- 'Not Rated' 
rawData$content_rating[rawData$content_rating == 'Unrated']   <- 'Not Rated'
table(rawData$content_rating)
#change the tv ratings to movie standards
rawData$content_rating[rawData$content_rating == "TV-14"] <- "PG"
rawData$content_rating[rawData$content_rating == "TV-14"] <- "PG"
rawData$content_rating[rawData$content_rating == "TV-G"] <- "G"
rawData$content_rating[rawData$content_rating == "TV-MA"] <- "R"
rawData$content_rating[rawData$content_rating == "TV-PG"] <- "PG"
table(rawData$content_rating)
#assign the values with spaces as "Not Known"
rawData$content_rating[!((rawData$content_rating == 'G')|(rawData$content_rating == 'NC-17')|(rawData$content_rating == 'Not Rated')|(rawData$content_rating == 'PG')|(rawData$content_rating == 'PG-13')|(rawData$content_rating == 'R'))] <- "Not Known"
#Check all the values in content_rating
table(rawData$content_rating) #Now only having NC-17, Not Rated, PG, PG-13, R, and Not Known 6 categories

#create a sub dataset for observations not having gross for further to look into
missing_gross <- rawData[is.na(rawData$gross), ]
missing_gross <- subset(missing_gross, select = -c(gross))

#assign the cleaned data the name of imdb
imdb <- rawData[!is.na(rawData$gross), ]
#check again missing values
colSums(sapply(imdb, is.na))

#clean the language column
levels(imdb$language) <- c(levels(imdb$language), "Non-English")
imdb$language[(imdb$language != 'English')] <- "Non-English"
imdb$language <- factor(imdb$language)
table(imdb$language)

#leaving movie after 1970
imdb <- imdb[imdb$title_year >= 1970,]

#Check for collinearity
numericvars <- c( "imdb_score","num_critic_for_reviews", "duration", "director_facebook_likes", "actor_3_facebook_likes","actor_1_facebook_likes","gross","num_voted_users","cast_total_facebook_likes","facenumber_in_poster","num_user_for_reviews","budget","actor_2_facebook_likes","movie_facebook_likes")
CorMatrix <- cor(imdb[,numericvars])
corrplot(CorMatrix, method = "square")


#------------End of Data Cleaning-----------------

#------------Data Exploration---------------------
#Gross and IMDB Score
imdb %>%
  filter(content_rating != "Not Known")%>%
  ggplot(aes(x=imdb_score,y=gross/10^6,color = content_rating)) +
  geom_point(stat="summary", fun.y=mean) +
  labs (color ="Content Rating", x="IMDb Score", y="Gross (million dollars)",
        title="Audience Reception vs. Commercial Success") + 
  facet_wrap(~content_rating) +geom_smooth()

#------------Text Mining on Plot Description-----------------
#Before going into analysis, we want to have a look on how movies are rated
sortScore <- sort(imdb$imdb_score,decreasing = TRUE)
sortScore
#So we see that there are 197 movies in the cleaned dataset that are rated above or equal to 8 among 3809, which is about the top rated 5%
#We consider movies rated equal or above 8 to be 
imdb$movie_title[imdb$imdb_score>=8.0]
#------------Plot
typeof(imdb$plot_keywords) #Finding that the plot keywords are stored as integers
imdb$plot_keywords<-as.character(imdb$plot_keywords) #change the storage of keywords into characters
imdb$plot_keywords[1] #make sure they are strings now but there are | between words
plot_keywords<-str_replace_all(imdb$plot_keywords,"\\|"," ") #replacing all the | with space for text cleaning
plot_keywords
#create a matrix which each line store a line of plot description
text_df <- tibble(line = 1:length(plot_keywords), text = plot_keywords)
#show the output
text_df

#
plot_keywords_top <- plot_keywords[imdb$imdb_score>=8.0]
#create a matrix which each line store a line of plot description
text_df_top <- tibble(line = 1:length(plot_keywords_top), text = plot_keywords_top)
#show the output
text_df_top

#make each line a storage of one word
text_df2 <- text_df %>%
  unnest_tokens(word, text)
text_df2
#clean the stop words from the texts
data(stop_words)
text_df2 <- text_df2 %>%
  anti_join(stop_words)
#count the appearance of all words in the sample
text_df2 %>%
  dplyr::count(word, sort = TRUE) 
#print out the words by frequency of words showing up more than 100 times
text_df2 %>%
  dplyr::count(word, sort = TRUE) %>%
  filter(n > 100) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip()
#draw wordcloud for the graph
#show 100 words wordcloud
text_df2 %>%
  anti_join(stop_words) %>%
  dplyr::count(word) %>%
  with(wordcloud(word, n,colors = brewer.pal(8, 'Dark2'), max.words = 100))

#----------------Association among Any Two Words for Plot Description----------------------
#make each line a storage of two word to see association
text_bigram <- text_df %>%
  unnest_tokens(bigram, text,token = "ngrams",n=2)
text_bigram
text_bigram %>%
  dplyr::count(bigram, sort = TRUE)

bigrams_separated <- text_bigram %>%
  separate(bigram, c("word1", "word2"), sep = " ")

bigrams_filtered <- bigrams_separated %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word)

# new bigram counts:
bigram_counts <- bigrams_filtered %>% 
  dplyr::count(word1, word2, sort = TRUE)
bigram_counts

bigrams_united <- bigrams_filtered %>%
  unite(bigram, word1, word2, sep = " ")
bigrams_united

# original counts
bigram_counts
# filter for only relatively common combinations
bigram_graph <- bigram_counts %>%
  filter(n > 10) %>%
  graph_from_data_frame()
bigram_graph
set.seed(1)
#draw the graph between associated groups of words
ggraph(bigram_graph, layout = "fr") +
  geom_edge_link() +
  geom_node_point() +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1)
set.seed(1)
arrow <- grid::arrow(type = "closed", length = unit(.15, "inches"))
ggraph(bigram_graph, layout = "fr") +
  geom_edge_link(aes(edge_alpha = n), show.legend = FALSE,
                 arrow = arrow, end_cap = circle(.07, 'inches')) +
  geom_node_point(color = "lightblue", size = 5) +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1) +
  theme_void()
#------------End of Text Mining-----------------


#####--------Modeling---------------------------
###regression
set.seed(3)
index=sample(1:nrow(imdb),3000)
score_data_test=imdb[-index,]
score_data_train=imdb[index,]

nfold <- 10
n=nrow(score_data_train)
set.seed(3)
foldid <- rep(1:nfold,each=ceiling(n/nfold))[sample(1:n)]
OOS <- data.frame(a=rep(NA,nfold), b=rep(NA,nfold), 
                  c=rep(NA,nfold), d=rep(NA,nfold),e=rep(NA,nfold)) 
Mx<- model.matrix(imdb_score ~ ., data=score_data_train[,-c(2,7,10,11,12,14,15,17,18,20,23)])[,-1]
xlevs <- lapply(Mx[,sapply(df, is.factor), drop = F], function(j){
  levels(j)
})
My<- score_data_train$imdb_score 
lasso <- glmnet(Mx,My, family="gaussian")
lassoCV <- cv.glmnet(Mx,My, family="gaussian")
par(mar=c(1.5,1.5,2,1.5))
par(mai=c(1.5,1.5,2,1.5))
#plot(lassoCV, main="Fitting Graph for CV Lasso \n \n # of non-zero coefficients  ", xlab = expression(paste("log(",lambda,")")))
lassoCV$lambda[which.min(lassoCV$cvm)]
features.min <- support(lasso$beta[,which.min(lassoCV$cvm)])
length(features.min)
features.1se <- support(lasso$beta[,which.min( (lassoCV$lambda-lassoCV$lambda.1se)^2)])
length(features.1se) 
data.min <- data.frame(Mx[,features.min],My)
data.1se <- data.frame(Mx[,features.1se],My)
for(k in 1:nfold){ 
  train <- which(foldid!=k) # train on all but fold `k'
  
  #Post Lasso Estimates
  rmin <- glm(My~., data=data.min, subset=train, family="gaussian")
  if ( length(features.1se) == 0){  r1se <- glm(imdb_score~1, score_data_test, subset=train, family="gaussian") 
  } else {r1se <- glm(My~., data=data.1se, subset=train, family="gaussian")
  }
  
  predmin <- predict(rmin, newdata=data.min[-train,], type="response")
  pred1se  <- predict(r1se, newdata=data.1se[-train,,drop=FALSE], type="response")
  
  lassomin  <- glmnet(Mx[train,],My[train], family="gaussian",lambda = lassoCV$lambda.min)
  lasso1se  <- glmnet(Mx[train,],My[train], family="gaussian",lambda = lassoCV$lambda.1se)
  predlassomin <- predict(lassomin, newx=Mx[-train,], type="response")
  predlasso1se  <- predict(lasso1se, newx=Mx[-train,], type="response")
  #-County-FIPS-Clinton-Obama-TotalVote
  linear <-lm(imdb_score~., data=score_data_train[train,-c(2,7,10,11,12,14,15,17,18,20,23)])
  summary(linear)
  prelinear  <- predict(linear, newdata=score_data_train[-train,-c(2,7,10,11,12,14,15,17,18,20,23)], type="response")
  print(prelinear )
  
  ## calculate and log R2
  OOS$a[k] <- R2(y=score_data_train$imdb_score[-train], pred=predmin, family="gaussian")
  OOS$a[k]
  
  OOS$b[k] <- R2(y=score_data_train$imdb_score[-train], pred=pred1se, family="gaussian")
  OOS$b[k]
  
  OOS$c[k] <- R2(y=score_data_train$imdb_score[-train], pred=predlassomin, family="gaussian")
  OOS$c[k]
  
  OOS$d[k] <- R2(y=score_data_train$imdb_score[-train], pred=predlasso1se, family="gaussian")
  OOS$d[k]
  
  OOS$e[k] <- R2(y=score_data_train$imdb_score[-train], pred=prelinear)
  OOS$e[k]
  
  
  ## We will loop this nfold times (I setup for 10)
  ## this will print the progress (iteration that finished)
  print(paste("Iteration",k,"of",nfold,"(thank you for your patience)"))
}
OOS

###first model
lassomin  <- glmnet(Mx,My, family="gaussian",lambda = lassoCV$lambda.min)
lasso_supp <- support(lassomin$beta)
colnames(Mx[, lasso_supp])
lasso_inthemodel <- unique(c(lasso_supp))
lasso_train_data <- Mx[,lasso_inthemodel]
lasso_train_data <- as.data.frame(as.matrix(lasso_train_data))
lasso_supp <- support(lassomin$beta)
colnames(Mx[, lasso_supp])
lasso_inthemodel <- unique(c(lasso_supp))
lasso_train_data <- Mx[,lasso_inthemodel]
lasso_train_data <- as.data.frame(as.matrix(lasso_train_data))
lasso_fit <- lm(My ~ ., data = lasso_train_data)
summary(lasso_fit)
#after remove the non-significant term
Mx1<- model.matrix(imdb_score ~ ., data=score_data_train[,-c(1,2,6,7,10,11,12,14,15,17,18,20,23)])[,-1]
lassomin1  <- glmnet(Mx1,My, family="gaussian",lambda = lassoCV$lambda.min)
lasso_supp <- support(lassomin1$beta)
colnames(Mx1[, lasso_supp])
lasso_inthemodel <- unique(c(lasso_supp))
lasso_train_data <- Mx1[,lasso_inthemodel]
lasso_train_data <- as.data.frame(as.matrix(lasso_train_data))
lasso_fit <- lm(My ~ ., data = lasso_train_data)
summary(lasso_fit)
#predict
Mxtest<- model.matrix(~., data=score_data_test[,-c(1,2,6,7,10,11,12,14,15,17,18,20,23,25)],xlev=xlevs)[,-1]
predlassomin <- predict(lassomin1, newx=Mxtest, type="response")
error=predlassomin-score_data_test[25]
sum(abs(error))/nrow(error) #AVG error
lassoCV$lambda.min

### Classification on cleaned data
# Assigning "score_level" based on quantile
newimdb <- imdb
quantile(newimdb$imdb_score)
newimdb$imdb_level <- cut(newimdb$imdb_score, breaks = c(1.5,5.8,6.5,7.2,9.3))
#drop names, texts and other categorical variables with more than 50 classes
newimdb <- subset(newimdb, select = -c(director_name,actor_2_name,actor_1_name,genres,language,title_year,movie_title,actor_3_name,plot_keywords,country))
colSums(sapply(newimdb, is.na))
#adding train and test set
index=sample(1:nrow(newimdb),3000)
newimdb_data_test=newimdb[-index,]
newimdb_data_train=newimdb[index,]
summary(newimdb_data_train)

##Classification Tree
scoretree <- tree(imdb_level ~.-imdb_score, data=newimdb_data_train) 
summary(scoretree)
plot(scoretree)
text(scoretree, label="yval")

# Extract vector of probabilities
scoretree[[1]]$yprob[,2]

# FULL MODEL with tree.
scoretreeFull <- tree(imdb_level ~ .-imdb_score, data=newimdb_data_train, control=tree.control(nobs = 7043, mincut = 2, minsize = 5, mindev = 0.002)) 
summary(scoretreeFull)
text(scoretreeFull, label="yval")
plot(scoretreeFull)
text(scoretreeFull,label="yval")

## Random Forest
set.seed(1)
model1 <- randomForest(imdb_level~.-imdb_score, data=newimdb_data_train, importance = TRUE)
model1                     
plot(model1)
legend('topright', colnames(model1$err.rate), col=1:5, fill=1:5)
# Rank features based on importance
importance <- importance(model1)
importance
varImportance <- data.frame(Variables = row.names(importance), 
                            Importance = round(importance[ ,'MeanDecreaseGini'],2))
rankImportance <- varImportance %>%
  mutate(Rank = paste0('#',dense_rank(desc(Importance))))
rankImportance

###Classification on missing gross values
# Assigning "score_level" based on quantile
nogross <- missing_gross
quantile(nogross$imdb_score)
nogross$imdb_level <- cut(nogross$imdb_score, breaks = c(1.6,5.3,6.3,7.2,9.1))
#drop names, texts and other categorical variables with more than 50 classes
nogross <- subset(nogross, select = -c(director_name,actor_2_name,actor_1_name,genres,language,title_year,movie_title,actor_3_name,plot_keywords,country))
colSums(sapply(nogross, is.na))
summary(nogross)
##Classification
scoretree <- tree(imdb_level ~.-imdb_score, data=nogross) 
summary(scoretree)
plot(scoretree)
text(scoretree, label="yval")

### Extract the vector of probabilities
scoretree[[1]]$yprob[,2]

### FULL MODEL with tree. 
scoretreeFull <- tree(imdb_level ~ .-imdb_score, data=nogross, control=tree.control(nobs = 7043, mincut = 2, minsize = 5, mindev = 0.002)) 
summary(scoretreeFull)
plot(scoretreeFull) 
text(scoretreeFull, label="yval")

##Random Forest
set.seed(1)
model2 <- randomForest(imdb_level~.-imdb_score, data=nogross, importance = TRUE)
model2                     
plot(model2)
legend('topright', colnames(model2$err.rate), col=1:5, fill=1:5)
# Create a rank variable based on importance
importance2 <- importance(model2)
varImportance2 <- data.frame(Variables = row.names(importance2), 
                             Importance = round(importance2[ ,'MeanDecreaseGini'],2))
rankImportance2 <- varImportance2 %>%
  mutate(Rank = paste0('#',dense_rank(desc(Importance))))
rankImportance2











#------------Appendix for Text Mining-----------
#These findings are not included in our final write-up, but would be 
#enlightening to the overall topic

#----------------Top Movies
#make each line a storage of two word to see association
text_bigram <- text_df_top %>%
  unnest_tokens(bigram, text,token = "ngrams",n=2)
text_bigram
text_bigram %>%
  dplyr::count(bigram, sort = TRUE)

bigrams_separated <- text_bigram %>%
  separate(bigram, c("word1", "word2"), sep = " ")

bigrams_filtered <- bigrams_separated %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word)

# new bigram counts:
bigram_counts <- bigrams_filtered %>% 
  dplyr::count(word1, word2, sort = TRUE)
bigram_counts

bigrams_united <- bigrams_filtered %>%
  unite(bigram, word1, word2, sep = " ")
bigrams_united

# original counts
bigram_counts
# filter for only relatively common combinations
bigram_graph <- bigram_counts %>%
  filter(n >= 2) %>%
  graph_from_data_frame()
bigram_graph
set.seed(1)
#draw the graph between associated groups of words
ggraph(bigram_graph, layout = "fr") +
  geom_edge_link() +
  geom_node_point() +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1)
set.seed(1)
arrow <- grid::arrow(type = "closed", length = unit(.15, "inches"))
ggraph(bigram_graph, layout = "fr") +
  geom_edge_link(aes(edge_alpha = n), show.legend = FALSE,
                 arrow = arrow, end_cap = circle(.07, 'inches')) +
  geom_node_point(color = "lightblue", size = 5) +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1) +
  theme_void()


#-------Top Movies
#make each line a storage of one word
text_df2 <- text_df_top %>%
  unnest_tokens(word, text)
text_df2
#clean the stop words from the texts
data(stop_words)
text_df2 <- text_df2 %>%
  anti_join(stop_words)
#count the appearance of all words in the sample
text_df2 %>%
  dplyr::count(word, sort = TRUE) 
#print out the words by frequency of words showing up more than 100 times
text_df2 %>%
  dplyr::count(word, sort = TRUE) %>%
  filter(n > 100) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip()
#draw wordcloud for the graph
#show 100 words wordcloud
text_df2 %>%
  anti_join(stop_words) %>%
  dplyr::count(word) %>%
  with(wordcloud(word, n,colors = brewer.pal(8, 'Dark2'), max.words = 100))

#Sentiment Analysis for Plot Description-------------------
#Using the bing sentiment lexicons in the package of tidytext, which assign positive and negative mood to words
text_centiment_count <- text_df2 %>%
  inner_join(get_sentiments("bing")) %>%
  dplyr::count(word, sentiment, sort = TRUE) %>%
  ungroup()
#display the counting of words most appeared
text_centiment_count
#display the contibution to the negative and positive mood for the most freqently shown 20 words
text_centiment_count %>%
  group_by(sentiment) %>%
  top_n(20) %>%
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y") +
  labs(y = "Contribution to sentiment",
       x = NULL) +
  coord_flip()
#Word Cloud with Blue representing negative words, Red representing positive words
text_df2 %>%
  inner_join(get_sentiments("bing")) %>%
  dplyr::count(word, sentiment, sort = TRUE) %>%
  acast(word ~ sentiment, value.var = "n", fill = 0) %>%
  comparison.cloud(colors = c("light sky blue", "tomato"),
                   max.words = 100)
#--------------Genre
#Same cleaning process for genres
imdb$genres <- as.character(imdb$genres)
imdb$genres[1]
b<-str_replace_all(imdb$genres,"\\|"," ")
b
text_df3 <- tibble(line = 1:length(b), text = b)
text_df3
text_df3 <- text_df3 %>%
  unnest_tokens(word, text)
text_df3 %>%
  dplyr::count(word, sort = TRUE) 
#Show the words which appears more than 100 times in Genre
text_df3 %>%
  dplyr::count(word, sort = TRUE) %>%
  filter(n > 100) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip()
#word cloud for most shown genre
text_df3 %>%
  anti_join(stop_words) %>%
  dplyr::count(word) %>%
  with(wordcloud(word, n,colors = brewer.pal(11,"Spectral"), random.color = F,max.words = 100))

#--------Top Movies
b<-str_replace_all(imdb$genres,"\\|"," ")
b_top<- b[imdb$imdb_score>=8.0]
text_df3 <- tibble(line = 1:length(b_top), text = b_top)
text_df3
text_df3 <- text_df3 %>%
  unnest_tokens(word, text)
text_df3 %>%
  dplyr::count(word, sort = TRUE) 
#Show the words which appears more than 25 times in Genre
text_df3 %>%
  dplyr::count(word, sort = TRUE) %>%
  filter(n > 25) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip()
#word cloud for most shown genre
text_df3 %>%
  anti_join(stop_words) %>%
  dplyr::count(word) %>%
  with(wordcloud(word, n,colors = brewer.pal(11,"Spectral"), random.color = F,max.words = 100))

#--------------Title
##Same Text cleaning for title
imdb$genres <- as.character(imdb$movie_title)
imdb$genres[1]
c<-str_replace_all(imdb$genres,"\\|"," ")
c<-str_replace_all(imdb$genres,"ii","2")
c<-str_replace_all(imdb$genres,"iii","3")
c
text_df4 <- tibble(line = 1:length(c), text = c)
#show the output
text_df4
#make each line a storage of one word
text_df5 <- text_df4 %>%
  unnest_tokens(word, text)
text_df5
#clean the stop words from the texts
data(stop_words)
text_df5 <- text_df5 %>%
  anti_join(stop_words)
#count the appearance of all words in the sample
text_df5 %>%
  dplyr::count(word, sort = TRUE) 
#print out the words by frequency of words showing up more than 100 times
text_df5 %>%
  dplyr::count(word, sort = TRUE) %>%
  filter(n > 15) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip()
#draw wordcloud for the graph
#show 100 words wordcloud
text_df5 %>%
  anti_join(stop_words) %>%
  dplyr::count(word) %>%
  with(wordcloud(word, n,colors = brewer.pal(11,"Spectral"), random.color = F,max.words = 100))
##
#------------End of Appendix Text Mining-----------------

