library(readr) # import
library(dplyr) # data manipulation
library(ggplot2) # visualizations
library(gridExtra) # visualizations
library(tictoc) # timing models
library(caret) # tuning & cross-validation
library(date)
options=200
#backup for the full data
date<-df_full
#########################missing value###########################
missing_vars <- function(x) {
  var <- 0
  missing <- 0
  missing_prop <- 0
  for (i in 1:length(names(x))) {
    var[i] <- names(x)[i]
    missing[i] <- sum(is.na(x[, i]))
    missing_prop[i] <- missing[i] / nrow(x)
  }
  (missing_data <- data.frame(var = var, missing = missing, missing_prop = missing_prop) %>% 
      arrange(desc(missing_prop)))
}

#################date transfermation#################################
lct <- Sys.getlocale("LC_TIME"); Sys.setlocale("LC_TIME", "C")
#install.packages('date')
df_train<-read.csv('act_train.csv')
df_people<-read.csv('people.csv')
df_test<-read.csv('act_test.csv')

df_test$people_id %in% df_train$people_id
# so we can conclude is that we need to do the predection for different people 
df_test$date %in% df_train$date
# so we can see the time is same ,and we can draw the conclusion that the activity is happen in the same time 
#se we should look the activity type first in same day 
df_train$test_data<-0
df_test$test_data<-1
df_full<-bind_rows(df_train,df_test)
glimpse(df_full)
df_full$date
#let's select date
typeof(df_full$date) #so we need to convert 

df_people$date<-as.Date(df_people$date)
df_full$date<-as.Date(df_full$date)
df_full %>%
  filter(date=='2023-08-26' & test_data==1) %>%
  select(c('people_id','activity_category','test_data','outcome'))
glimpse(df_full)
df_full %>%
  group_by(date,activity_category) %>%
  count()

df_full %>%
  group_by(activity_category) %>%
  count()

df_people %>%
  group_by(group_1) %>%
  count() %>%
  arrange(desc(n))

glimpse(df_people)
df_full$date[df_full$date=='2023-08-26',]
date$activity_category


#######################################################################
df_full<-left_join(df_full,df_people,by="people_id")
glimpse(df_full)

type_1<-df_full %>%
  filter(activity_category=='type 1')
type_2<-df_full %>%
  filter(activity_category=='type 2')
type_3<-df_full %>%
  filter(activity_category=='type 3')
type_4<-df_full %>%
  filter(activity_category=='type 4')
type_5<-df_full %>%
  filter(activity_category=='type 5')
type_6<-df_full %>%
  filter(activity_category=='type 6')
type_7<-df_full %>%
  filter(activity_category=='type 7')

glimpse(type_1)
missing_vars(type_1)
type_1_train<-type_1[type_1$test_data==0,]
type_1_train<-type_1_train %>%
  select(-c('activity_id','char_10.x','test_data','date.y','activity_category'))
glimpse(type_1_train)


cols <- c("people_id", "char_1.x", "char_2.x", "char_5.x","outcome")
type_1_train[,cols]<-lapply(type_1_train[,cols], factor) 
options(max.print=200) 
type_1_train %>%
  group_by(date.x) %>%
  count() 

#so the date is from 2022-07-17 to 2013-8-31 we can devide 
type_1_train$date.x<-as.Date(type_1_train$date.x)

type_1_train$date1[type_1_train$date.x>='2022-07-17' & type_1_train$date.x<'2022-10-17']<-'1'
type_1_train$date1[type_1_train$date.x>='2022-10-17' & type_1_train$date.x<'2023-01-17']<-'2'
type_1_train$date1[type_1_train$date.x>='2023-01-17' & type_1_train$date.x<'2023-04-17']<-'3'
type_1_train$date1[type_1_train$date.x>='2023-04-17' & type_1_train$date.x<'2023-09-01']<-'4'



cols <- c("people_id", "char_1.x", "char_2.x", "char_5.x","outcome",'date1')
type_1_train[,cols]<-lapply(type_1_train[,cols], factor) 
type_1_train<-type_1_train %>%
  select(-c('people_id','group_1' ))
glimpse(type_1_train)




library(randomForest)
fit.forest <-randomForest(outcome~.,data=type_1_train,importance=TRUE, ntree=500)