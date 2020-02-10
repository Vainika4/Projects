###Obtaining and Analyzing Twitter Data using R###


#1. Registering APRI using Twitter account
#https://apps.twitter.com

#2. Insert Values
api_key <-'sEsc5AhZt63SosJ9l5c4eLlDT'
api_secret <- 'jboxK2swNYplkTwtIG1MrXfL8ExowtI4EMdlg9yWdiCqiiIfgZ'
access_token <- "156924028-L1ixzJJvf639XUaLXWixRGYCDSuLiqvpgC3hHAuE"
access_token_secret <-"Uw7OKQg4sY5ye9XWJp6NspioxjeQ4CSRp4sjEBc5xqetI"


install.packages("twitteR")
library(twitteR)
install.packages("ROAuth")
library(ROAuth)
setup_twitter_oauth(api_key, 
                    api_secret,
                    access_token, 
                    access_token_secret)

#3. Extract tweets
#Take a look at houstonrockets tweets
#pull 1200 tweets
tweets <-searchTwitter("#houstonrockets", n=1200, lang='en')

length(tweets)
#[1] 1200

tweets #Check your tweets

#convert into list to data frame
houstonrockets<-twListToDF(tweets)

#4. Create CSV file and save it
write.csv(houstonrockets, file = '~/Desktop/houstonrockets.csv', row.names = F)

# Data Cleaning and Preparation
#1. Reading Data File
houstonrockets<-read.csv(file.choose(), header=T) #choose the houstonrockets.csv file
str(houstonrockets) #look at structure of the  file (has 1000 obs and 16 var)
#first col is text, whether it is favorited, what is the count, when it was created, 
#id of person who tweeted, whether it is a retweet etc.....)

#clean the text of special characters such as symbols and emoticons
houstonrockets$text <- sapply(houstonrockets$text,function(row) iconv(row, "latin1", "ASCII", sub=""))

#2. Building Corpus
library(tm)
library(NLP)
corpus <-iconv(houstonrockets$text, to='utf-8-mac') #need only the first col text from file
corpus <- Corpus(VectorSource(corpus)) #corpus is a collection of texts
inspect(corpus[1:5]) #inspect the first five tweets

#3. Cleaning Data
#convert data to lower case for analysis
corpus <-tm_map(corpus, tolower) #convert all alphabet to lower case
inspect(corpus[1:5]) #inspect the first five tweets

#remove punctuations
corpus <-tm_map(corpus, removePunctuation)
inspect(corpus[1:5]) #inspect the first five tweets

#remove numbers
corpus <-tm_map(corpus, removeNumbers)
inspect(corpus[1:5]) #inspect the first five tweets

#remove common words-they dont add any informational value
#use the stopwords function in english
#select stopwords(english) to see what words are removed
cleanset <-tm_map(corpus, removeWords, stopwords('english'))
inspect(cleanset[1:5])

#remove URLs (https://etc.)
#make use of function http
removeURL <- function(x) gsub("http[[:alnum:]]*", '', x)
cleanset <-tm_map(cleanset, content_transformer(removeURL))
inspect(cleanset[1:5])

#tweets were pulled using houstonrockets so we can clean it from the text
cleanset <-tm_map(cleanset, removeWords, c('houstonrockets','houston','rockets','quickly','will','either','like','can','even','get','gets','knows','asked','begun'))
inspect(cleanset[1:5])

#remove white spaces
cleanset <- tm_map(cleanset, stripWhitespace)
inspect(cleanset[1:5])

#lets now provide some structure to tweets by creating a matrix of rows/coloums
#this is called term document matrix (tdm)
#Create term document matrix

tdm <- TermDocumentMatrix(cleanset)
tdm 

#<<TermDocumentMatrix (terms: 2243, documents: 1000)>>
#Non-/sparse entries: 9948/2233052
#Sparsity           : 100% (it is rounded)
#Maximal term length: 34
#Weighting          : term frequency (tf)
#if you would like to look at this matrix, you have to convert this into matrix first
tdm <- as.matrix(tdm)
tdm[1:10, 1:20] #look at first 10 rows/terms and 20 tweets


#VISUALIZE TEXT DATA
#in the tdm if you sum rows, it will tell you how many times a term appears
#also there are many words/terms so we create a subset of w where row sum is >30
# Bar Plot
w <- rowSums(tdm)
w <- subset(w, w>=30) #run "w" to see which words appear how many times
barplot(w, las = 2, col=rainbow(40)) #words represented vertically using las=2, rainbow colors 
#find that words such as didnt, car, people's names also appears so go back and combine them into a clean data dictionary
#clean the dataset of these words using dictionary created and then redo term document matrix

##################
#######after creating bar plot, you can go back and combine words or clean up further if needed
#and recreate the term document matrix
cleanset <-tm_map(cleanset, removeWords, c('china', 'chinas'))
inspect(cleanset[1:5])
##################

# Word Cloud
library(wordcloud)
library(RColorBrewer)
w <- sort(rowSums(tdm), decreasing=TRUE) #sort words in decreasing order
set.seed(9999)
wordcloud(words = names(w), 
          freq=w, max.words = 300, 
          random.order =FALSE)  #words are specified in names in w dataframe, frequency is stored in w, random order=false

#specifying options in word cloud
#Specify that max words be no more than say, 200
#Freq for terms to be included in wordcloud (say they have to appear 5 times to be included)
#color words, specify scale (bigger words max size =3, smaller =0.2)
#rotate some words (rotation percentage = 30%)

wordcloud(words = names(w), 
          freq=w, 
          random.order =FALSE,
          max.words = 200, 
          min.freq = 5,
          colors = brewer.pal(8, 'Dark2'), 
          scale = c(3, 0.2), 
          rot.per = .3)  

#SENTIMENT ANALYSIS USING R
#load packages
install.packages("syuzhet")
library(syuzhet)
library(lubridate)
library(ggplot2)
library(scales)
library(dplyr)

#Reading Files
#take the initial apple tweet file (1000 obs and 16 vars for this)
#take the first column, text and put it into tweets dataframe
tweets <- iconv(houstonrockets$text, to="utf-8-mac")

#obtain sentiment scores for each 1000 tweets
#nrc_sentiment dictionary is called to calculate presence of 
#eight emotions & their corresponding valence in their text file

s <-get_nrc_sentiment(tweets)
head(s) 

#runs through each tweet and finds words corresponding to each sentiment
#and a score is given (last 2 cols are positive and negative tweet categories)
tail(s)
tweets[996] #look at tweet number 996

#you could also look at phrases or words in these tweets to see if they 
#lead to positive or negative') 
get_nrc_sentiment('obedience') 
get_nrc_sentiment('foreign businesses')


#plot the sentiment scores
#lets sum the column scores across tweets for the plot
#label y axis as total count, main title of plot label

barplot(colSums(s), 
        las = 2,
        ylab = 'Total Count', 
        main ='Sentiment Scores for houstonrockets Tweets')

#Try searching for multiple hashtags
hashtags <- '#houstonrockets + #morey'
tweets2 <-searchTwitter(hashtags, n=80, lang='en')
houstonrockets2<-twListToDF(tweets2)
write.csv(houstonrockets2, file = '~/Desktop/houstonrockets2.csv', row.names = F)
houstonrockets2<-read.csv(file.choose(), header=T) 
str(houstonrockets2) 
houstonrockets2$text <- sapply(houstonrockets2$text,function(row) iconv(row, "latin1", "ASCII", sub=""))
corpus2 <-iconv(houstonrockets2$text, to='utf-8-mac') 
corpus2 <- Corpus(VectorSource(corpus2)) 
corpus2 <-tm_map(corpus2, tolower) 
corpus2 <-tm_map(corpus2, removePunctuation)
corpus2 <-tm_map(corpus2, removeNumbers)
cleanset2 <-tm_map(corpus2, removeWords, stopwords('english'))
removeURL <- function(x) gsub("http[[:alnum:]]*", '', x)
cleanset2 <-tm_map(cleanset2, content_transformer(removeURL))
cleanset2 <-tm_map(cleanset2, removeWords, c('houstonrockets','houston','rockets','quickly','will','either','like','can','even','get','gets','knows','asked','begun'))
cleanset2 <- tm_map(cleanset2, stripWhitespace)
tdm2 <- TermDocumentMatrix(cleanset2)
tdm2
tdm2 <- as.matrix(tdm2)
tdm2[1:10, 1:20] 

w2<- rowSums(tdm2)
w2 <- subset(w2, w>=30) 
barplot(w2, las = 2, col=rainbow(40)) 
cleanset2 <-tm_map(cleanset2, removeWords, c('china', 'chinas'))

w2 <- sort(rowSums(tdm2), decreasing=TRUE) 
set.seed(9999)
wordcloud(words = names(w2), 
          freq=w2, max.words = 300, 
          random.order =FALSE)  

wordcloud(words = names(w2), 
          freq=w2, 
          random.order =FALSE,
          max.words = 200, 
          min.freq = 5,
          colors = brewer.pal(8, 'Dark2'), 
          scale = c(3, 0.2), 
          rot.per = .3)  

tweets2 <- iconv(houstonrockets2$text, to="utf-8-mac")

s2 <-get_nrc_sentiment(tweets2)
head(s2) 

barplot(colSums(s2), 
        las = 2,
        ylab = 'Total Count', 
        main ='Sentiment Scores for #Houstonrockets + #Morey Tweets')









####SOCIAL NETWORK ANALYSIS###
tdm[1:20, 1:20] #lets look at our term document matrix, 10 rows, 10 cols
library(igraph)
tdm[tdm>1] <-1 
#whenever our tdm value is more than 1 for a tweet we convert into 1 because we dont need the values 2, 3,
#we only need that the term appeared (freq of terms is not required in network analysis)
termM <-tdm %*% t(tdm) #transpose of tdm matrix; create tweet adjacency matrix using %*%
termM[1:10, 1:10] #term matrix

g <- graph.adjacency(termM, weighted=T, mode ='undirected') #convert it into graph, no direction for edges
g

#remove terms that have loops (going to self) 
g <- simplify(g)

#set labels and degrees of Vertices (V), each word is a vertices
V(g)$label <- V(g)$name #label is name
V(g)$label

V(g)$degree <- degree(g) #degree is the number of connections between terms
V(g)$degree

g[1,]

#Histogram of node degree, lets just use 100 bars (too many words), label of y and x axis
hist(V(g)$degree, 
     breaks=100, 
     col='green', 
     main ='histogram of node degree', 
     ylab ='frequency',
     xlab='degree of vertices') #right skewed 

#Network diagram
set.seed(9999)
plot(g) #interpretation is difficult so recreate more meaningful visuals

#Recreate this by looking at just the top terms/nodes by degree
tdm <- tdm[rowSums(tdm)>100,] #lets reduce the size and counts of total frequency (rowSum) 
#include only terms having frequency more than 100
#it will take out all very infrequent terms

#Rerun all other code
tdm[tdm>1] <-1 
termM <-tdm %*% t(tdm)
g <- graph.adjacency(termM, weighted=T, mode ='undirected')
g <- simplify(g)
V(g)$label <- V(g)$name 
V(g)$degree <- degree(g)
V(g)$label
V(g)$degree ####Find degree for nodes

V(g)$betweenness <- betweenness(g)
V(g)$betweenness ####Find betweenness for nodes

V(g)$closeness <- closeness(g)
V(g)$closeness #######Find closeness for nodes

V(g)$eigen <- eigen_centrality(g)
V(g)$eigen #####Find eigenvector for nodes

sort(igraph::degree(g))
### print the closeness for each node
sort(closeness(g))
### print the betweenness for each node
sort(round(betweenness(g),1))
### print the eigenvalue centrality for each node
sort(evcent(g)$vector)

V(g)$label.cex <- 1.1*V(g)$degree/max(V(g)$degree) + 0.3
V(g)$label.color <- rgb(0,0,.2,.8)
V(g)$frame.color <-NA
egam <- (log(E(g)$weight)+0.4)/max(log(E(g)$weight)+.4)
E(g)$color <-rgb(0.5,0.5,0,egam)
E(g)$width <- egam
plot(g, vertex.color = 'orange' ,vertex.size = V(g)$degree*0.5)

comm <-cluster_edge_betweenness(g)
plot(comm,g)

prop <-cluster_label_prop(g)
plot(prop,g)

greed <- cluster_fast_greedy(as.undirected((g)))
plot(greed,as.undirected(g))



