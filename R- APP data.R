library(gridExtra)
library(dslabs)
library(magrittr)
library(MASS)
library(tidyverse)
library(dplyr)
library(plyr)
library(ggplot2)
library(reshape2)
library(GGally)
library("tidyverse")
library("gridExtra")
library(corrplot)
library(RColorBrewer)

#Read data
rawData <- read.csv("~/Library/Mobile Documents/com~apple~CloudDocs/Duke/2019 Summer/R/Final Project/AppleStore.csv")
# Showing summary about the data structures


#------------------------------------------------------------------------------------------
#Data Cleaning
duplictate1 <- filter(rawData, rawData$track_name == "Mannequin Challenge")
duplictate1
duplictate2 <- filter(rawData, rawData$track_name == "VR Roller Coaster")
duplictate2
app<-rawData[!(rawData$track_name == "VR Roller Coaster" & rawData$ver == 0.81),]
app<-app[!(app$track_name== "Mannequin Challenge" & app$ver == "1.0.1"),]
#Sub dataset for apps without ratings for further investigation
app0<-filter(app,app$rating_count_tot == 0)
#Cleaned dataset
app <- app[app$rating_count_tot != 0,]
summary(app)
# Adding a new variable to include size by MB.
app$size_mb <- app$size_bytes/1024/1024

# Showing frequency of apps in our data set based on app content category.
qplot(x=fct_infreq(prime_genre), data = app) +
  labs (title="Frequncy of Apps of Different App Genre",
        x="Categories",y="Count") +
  theme(axis.text.x=element_text(angle=90,hjust=1))

# Showing summary about the price, also adding a new variable to seperate paid and free apps.
summary(app$price==0)
app$is_free <- app$price == 0
app$is_free <- ifelse(app$is_free == TRUE, "Free", "Paid")
app0$is_free <- app0$price == 0
app0$is_free <- ifelse(app0$is_free == TRUE, "Free", "Paid")

# Showing summary about the price, also adding a new variable to seperate the rating group
app$rating_three <- app$user_rating < 3
app$rating_three <- ifelse(app$rating_three == TRUE, "Rated Under 3", ifelse(app$user_rating ==3,"Rated 3","Rated above 3"))

#------------------------------------------------------------------------------------------
# Data Exploratory

# A histogram showung the summary of ipadSc_urls.num (MEAN) and user rating.
ggplot(aes(x=ipadSc_urls.num,y=rating_count_tot,fill=ipadSc_urls.num), data = app) +
  geom_histogram(stat = 'summary', fun.y = mean) + theme_light()+ facet_wrap(~rating_three)


# A histogram showing total number of user ratings and app category.
ggplot(aes(x=prime_genre,y=rating_count_tot,fill=prime_genre), data = app) +
  geom_bar(stat = 'summary', fun.y = mean) + coord_flip() +
  labs (x="App Genre", y="Average of total number of user rating",
        title="Genre-Number of Rating")  +facet_grid(rating_three ~ is_free)+ theme_light()


# A histogram showing Genre-Average Price-Rating Group.
ggplot(aes(x=prime_genre,y=price,fill=prime_genre), data = app) +
  geom_bar(stat = 'summary', fun.y = mean) + coord_flip() +
  labs (x="App Genre", y="Average of Price in Genre",
        title="Genre-Average Price-Rating Group")  +facet_wrap(~rating_three)+ theme_light()


# A histogram showing total number of apps and app category with 0 ratings.
ggplot(aes(prime_genre,fill=prime_genre), data=app0) +
  geom_bar() + coord_flip() +
  labs (x="App Genre", y="Count",
        title="App Genre with 0 Ratings-Count") + facet_wrap(~is_free)+ theme_light()
#------------------------------------------------------------------------------------------
#Model Building

#Draw the relationship matrix to have a general glance of the relationship between variables
pairs(app[,c('size_mb','price','rating_count_tot','user_rating','cont_rating',
             'prime_genre','sup_devices.num','ipadSc_urls.num','lang.num','vpp_lic')])
#cont_rating, prime_genre, and ipadSc_urls.num do not have obvious relationship with user_rating


lmmodel=lm(user_rating~I(log(size_mb))+price+I(log(rating_count_tot))+
             sup_devices.num+lang.num+as.factor(vpp_lic),data=app)

#size_mb log tranformation
plot(user_rating~size_mb,data=app)
lm1=lm(user_rating~size_mb,data=app)
lmnew1=lm(user_rating~I(log(size_mb)),data=app)
summary(lmnew1)
plot(user_rating~I(log(size_mb)),data=app)

#rating_count_tot log tranformation
plot(user_rating~rating_count_tot,data=app)
lm2=lm(user_rating~size_mb,data=app)
lmnew2=lm(user_rating~I(log(rating_count_tot)),data=app)
summary(lmnew2)
plot(user_rating~I(log(rating_count_tot)),data=app)

#final model and use stepwise function to find the best model
lmmodel=lm(user_rating~I(log(size_mb))+price+I(log(rating_count_tot))+
             sup_devices.num+lang.num+as.factor(vpp_lic),data=app)
summary(lmmodel)

#interaction plot using vpp_lic as interacting variable
ggplot(aes(x=I(log(size_mb)),y=user_rating,color=factor(vpp_lic)), data = app)+
  geom_point()+geom_quantile()
ggplot(aes(x=I(log(rating_count_tot)),y=user_rating,color=factor(vpp_lic)), data = app)+
  geom_point()+geom_quantile() 

#the model after adding interaction term
lmmodel1=lm(user_rating~I(log(size_mb))+price+I(log(rating_count_tot))+
             sup_devices.num+lang.num+as.factor(vpp_lic)+as.factor(vpp_lic) * 
             I(log(size_mb))+as.factor(vpp_lic) * 
             I(log(rating_count_tot)),data=app)
summary(lmmodel1)
step1<-step(lmmodel1)
summary(step1)
plot(step1)