library(readr) # import
library(dplyr) # data manipulation
library(ggplot2) # visualizations
library(gridExtra) # visualizations
library(tictoc) # timing models
library(caret) # tuning & cross-validation
library(date)
library(pryr)
rm()
gc()
pryr::mem_used()
#backup for the full data

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

#df_test$people_id %in% df_train$people_id
# so we can conclude is that we need to do the predection for different people 
#df_test$date %in% df_train$date
# so we can see the time is same ,and we can draw the conclusion that the activity is happen in the same time 
#se we should look the activity type first in same day 
df_train$test_data<-0
df_test$test_data<-1
df_full<-bind_rows(df_train,df_test)
df_full<-left_join(df_full,df_people,by="people_id")
df_full$date.x<-as.Date(df_full$date.x)
glimpse(df_full)
  write.csv(df_full,"dffull.csv",row.names = F)
df_full$date1[df_full$date.x>='2022-07-17' & df_full$date.x<'2022-10-17']<-'1'
df_full$date1[df_full$date.x>='2022-10-17' & df_full$date.x<'2023-01-17']<-'2'
df_full$date1[df_full$date.x>='2023-01-17' & df_full$date.x<'2023-04-17']<-'3'
df_full$date1[df_full$date.x>='2023-04-17' & df_full$date.x<'2023-09-01']<-'4'
df_full$date1<-as.factor(df_full$date1)

glimpse(df_full)
# df_full %>%
#   group_by(date,activity_category) %>%
#   count()
# 
# df_full %>%
#   group_by(activity_category) %>%
#   count()
# 
# df_people %>%
#   group_by(group_1) %>%
#   count() %>%
#   arrange(desc(n))
# 
# glimpse(df_people)
# glimpse(df_full)
# 
# #######################################################################
# #df_full<-left_join(df_full,df_people,by="people_id")
# glimpse(df_full)

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
glimpse(type_1_train)
glimpse(type_1)
type_1_train<-type_1_train %>%
  select(-c('activity_id','char_10.x','test_data','date.y','activity_category'))


cols <- c("people_id", "char_1.x", "char_2.x", "char_5.x","outcome")
type_1_train[,cols]<-lapply(type_1_train[,cols], factor) 
options(max.print=200) 
type_1_train %>%
  group_by(date.x) %>%
  count() 


cols <- c("people_id", "char_1.x", "char_2.x", "char_5.x","outcome",'date1')
type_1_train[,cols]<-lapply(type_1_train[,cols], factor) 
type_1_train<-type_1_train %>%
  select(-'people_id')
type_1_train<-type_1_train %>%
  select(-'date.x')
glimpse(type_1_train)
############################################################################
sapply(type_1_train, levels)
colnames(type_1_train)
options(tibble.print_max = Inf)
type_1_train %>%
  group_by(char_1.x) %>%
  count()

type_1_test %>%
  group_by(char_1.x) %>%
  count()
##############################################################
type_1_train[type_1_train$char_1.x=='type 46' | type_1_train$char_1.x=='type 47'|
               type_1_train$char_1.x=='type 48',] %>%
  select(char_1.x)

#######################################################
type_1_train[type_1_train$char_2.x=='type 32',] %>%
  select(char_2.x)

###########################################################
type_1_train[type_1_train$char_5.x=='type 7',] %>%
  select(char_5.x)

#####################################################
type_1_train_1<-type_1_train[-c(3155,14677,19782,30262,51517,157554,91026,100237,
                                144419,147765,74781,10694,36616,59763,152133,153864),]

type_1_train_1[type_1_train_1$char_1.x=='type 46'| type_1_train_1$char_1.x=='type 47'|
                 type_1_train_1$char_1.x=='type 48',] 

type_1_train_1$char_1.x<-factor(type_1_train_1$char_1.x)
type_1_train_1$char_2.x<-factor(type_1_train_1$char_2.x)
type_1_train_1$char_5.x<-factor(type_1_train_1$char_5.x)

# write.csv(type_1_train,file="type_1.csv", row.names=F)
# write.csv(type_1_train,file="type_1_train.csv", row.names=F)
# write.csv(type_1_test,file="type_1_test.csv", row.names=F)

library(randomForest)
fit.forest <-randomForest(outcome~.,data=type_1_train_1,importance=TRUE, ntree=500)
##################################################################################################
type_1_test<-type_1[type_1$test_data==1,]
glimpse(type_1_test)
cols <- c("people_id", "char_1.x", "char_2.x", "char_5.x","outcome",'date1')
type_1_test[,cols]<-lapply(type_1_test[,cols], factor) 
type_1_test<-type_1_test %>%
  select(-c('people_id','group_1','activity_id','char_10.x','test_data','date.y','activity_category','date.x','outcome'))

submission<-type_1[type_1$test_data==1,]
submission<-data.frame(activity_id=submission$activity_id)

glimpse(submission) 
submission
submission$outcome<-predict(fit.forest,type_1_test)
submission
# glimpse(type_1_test)
# glimpse(type_1_train)
# sapply(type_1_test, levels)
# sapply(type_1_train, levels)
# colnames(type_1_train)
# options(tibble.print_max = Inf)
#type_1_train %>%
#  group_by(char_1.x) %>%
#  count()

#type_1_test %>%
#  group_by(char_1.x) %>%
#  count()
# sapply(type_1_test, levels)
# sapply(type_1_train_1, levels)
# 
# options(tibble.print_max = Inf)
# colnames(type_1_train)
# 







#####################################################################
# type_x1<-df_full %>%
#   filter(activity_category!='type 1')
# type_x1$activity_category
# 
# glimpse(type_x1)
# type_x1_train<-type_x1[type_x1$test_data==0,]
# type_x1_train<-type_x1_train %>%
#   select(-c('people_id','activity_id','char_1.x','char_2.x','char_3.x','char_4.x','char_5.x',
#             'char_6.x','char_7.x','char_8.x','char_9.x','test_data','date.y','activity_category'))
# 
# glimpse(type_x1_train)
# type_x1_train$char_10.x<-as.factor(type_x1_train$char_10.x)
# levels(type_x1_train$char_10.x)
# type_x1_train %>%
#   group_by(char_10.x)

####################################type 2######################################
type_2$char_10.x<-as.factor(type_2$char_10.x)
glimpse(type_2) 
type_2_1<-type_2 %>%
  select(-c('people_id','activity_id','date.x','activity_category','char_1.x',
            'char_2.x','char_3.x','char_4.x','char_5.x','char_6.x','char_7.x','char_8.x',
            'char_9.x','char_10.x','group_1','date.y'))#test_data
type_2_train<-type_2_1[type_2$test_data==0,]
type_2_train<-type_2_train %>% select(-c('test_data'))
type_2_test<-type_2_1[type_2$test_data==1,]
type_2_test<-type_2_test %>% select(-c('test_data','outcome'))
glimpse(type_2_train)
glimpse(type_2_test)
# write.csv(type_2_train, file = "trainred.csv",row.names = FALSE)
# write.csv(submission2, file = "testred.csv",row.names = FALSE)

write.csv(type_2, file = "type_2.csv",row.names = FALSE)
glimpse(type_2)
library(randomForest)

type_2_train$outcome<-as.factor(as.character(type_2_train$outcome))
typeof(type_2_train$outcome)
fit.forest <-randomForest(outcome~.,data=type_2_train,importance=TRUE, ntree=50)
submission2<-type_2[type_2$test_data==1,]
submission2<-data.frame(activity_id=submission2$activity_id)
glimpse(submission2)
submission2$outcome<-predict(fit.forest,type_2_test)
write.csv(submission2, file = "submission.csv",row.names = FALSE)
glimpse(type_2_train)
levels(type_2_train)
levels(type_2_test$char_10.y)

###################################type 3###################################


count(type_3)
PER<-type_3 %>%
  group_by(char_10.x) %>%
  count() %>%
  arrange(desc(n))
PER$percentage<-PER$n/489339
##########################loop for char10.x#################
n=0
per=0
while (per<=0.95) {
  n=n+1
  per=per+PER[n,3]
  print(PER[n,])
  
} 
per

######################################################
type_3_1<-type_3
type_3_1$char_10.x <- ifelse(type_3_1$char_10.x %in% 
                                  c("type 23","type 2","type 61","type 52",
                                    "type 8","type 3","type 55"), 
                                type_3_1$char_10.x, 
                                "Other")
type_3_1$char_10.x<-as.factor(type_3_1$char_10.x )
###########################################################################
type_3_1<-type_3_1 %>%
  select(-c('people_id','activity_id','date.x','activity_category','char_1.x',
            'char_2.x','char_3.x','char_4.x','char_5.x','char_6.x','char_7.x','char_8.x',
            'char_9.x','group_1','date.y'))#test_data
type_3_train<-type_3_1[type_3$test_data==0,]
A<-type_3_1[type_3_1$test_data==0,]
type_3_train<-type_3_train %>% select(-c('test_data'))
type_3_test<-type_3_1[type_3$test_data==1,]
type_3_test<-type_3_test %>% select(-c('test_data','outcome'))
glimpse(type_3_train)
type_3_train$outcome<-as.factor(type_3_train$outcome)
glimpse(type_3_test)
# sapply(type_3_train,levels)
# sapply(type_3_test,levels)
fit.forest <-randomForest(outcome~.,data=type_3_train,importance=TRUE, ntree=50)
# write.csv(type_3_train, file = "trainred3.csv",row.names = FALSE)
# write.csv(type_3_test, file = "testred3.csv",row.names = FALSE)
submission3<-type_3[type_3$test_data==1,]
submission3<-data.frame(activity_id=submission3$activity_id)
# write.csv(submission3, file = "submission33.csv",row.names = FALSE)
submission3$outcome<-predict(fit.forest,type_3_test)
###################################type 4###################################
PER2
count(type_4)
PER2<-type_4 %>%
  group_by(char_10.x) %>%
  count() %>%
  arrange(desc(n))
options(tibble.print_max = 500)
PER2$percentage<-PER2$n/489339
##########################loop for char10.x#################
n=0
per=0
while (n<52) {
  n=n+1
  per=per+PER2[n,3]
  print(PER2[n,])
  
} 
per
#########################################################
type_4$char_11.x<-as.numeric(gsub("type", "", type_4$char_10.x))
type_4$char_11.x
max(type_4$char_11.x)
##########################
n=0
per=0
k=0
 while (n<52) {
  n=n+1
  k=k+1
  type_4$char_11.x[type_4$char_11.x<=(200*k) & type_4$char_11.x>=((k-1)*200)]<-k
  print(n)
  
 } 

max(type_4$char_11.x)
type_4$char_11.x
type_4$char_11.x<-as.factor(as.character(type_4$char_11.x))

####################################################
type_4_1<-type_4 %>%
  select(-c('people_id','activity_id','date.x','activity_category','char_1.x',
            'char_2.x','char_3.x','char_4.x','char_5.x','char_6.x','char_7.x','char_8.x',
            'char_9.x','char_10.x','group_1','date.y'))#test_data
type_4_train<-type_4_1[type_4$test_data==0,]
type_4_train<-type_4_train %>% select(-c('test_data'))
type_4_test<-type_4_1[type_4$test_data==1,]
type_4_test<-type_4_test %>% select(-c('test_data','outcome'))
glimpse(type_4_train)
type_4_train$outcome<-as.factor(type_4_train$outcome)
glimpse(type_4_test)
sapply(type_4_train,levels)
sapply(type_4_test,levels)
fit.forest <-randomForest(outcome~.,data=type_4_train,importance=TRUE, ntree=50)
submission4<-type_4[type_4$test_data==1,]
submission4<-data.frame(activity_id=submission4$activity_id)
submission4$outcome<-predict(fit.forest,type_4_test)
###################################type 5###################################

count(type_5)
PER3<-type_5 %>%
  group_by(char_10.x) %>%
  count() %>%
  arrange(desc(n))
PER3$percentage<-PER3$n/614173

n=0
per=0
while (n<52) {
  n=n+1
  per=per+PER3[n,3]
  print(PER3[n,])
  
} 
per
###################################################
select<-data.frame(PER3[1:50,1])
type_5$char_11.x <- ifelse(type_5$char_10.x %in% select$char_10.x, 
                             type_5$char_10.x, 
                             "Other")
type_5$char_11.x<-as.factor(type_5$char_11.x)
type_5$char_11.x 
########################################################################
type_5_1<-type_5 %>%
  select(-c('people_id','activity_id','date.x','activity_category','char_1.x',
            'char_2.x','char_3.x','char_4.x','char_5.x','char_6.x','char_7.x','char_8.x',
            'char_9.x','char_10.x','group_1','date.y'))#test_data
type_5_train<-type_5_1[type_5_1$test_data==0,]
type_5_train<-type_5_train %>% select(-c('test_data'))
type_5_test<-type_5_1[type_5_1$test_data==1,]
type_5_test<-type_5_test %>% select(-c('test_data','outcome'))
glimpse(type_5_train)
type_5_train$outcome<-as.factor(type_5_train$outcome)
glimpse(type_5_test)
sapply(type_5_train,levels)
sapply(type_5_test,levels)

fit.forest <-randomForest(outcome~.,data=type_5_train,importance=TRUE, ntree=50)
submission5<-type_5[type_5$test_data==1,]
submission5<-data.frame(activity_id=submission5$activity_id)
submission5$outcome<-predict(fit.forest,type_5_test)
###############################type 6#####################################
levels(as.factor(type_6$char_10.x))

type_6_1<-type_6 %>%
  select(-c('people_id','activity_id','date.x','activity_category','char_1.x',
            'char_2.x','char_3.x','char_4.x','char_5.x','char_6.x','char_7.x','char_8.x',
            'char_9.x','char_10.x','group_1','date.y'))#test_data
type_6_train<-type_6_1[type_6_1$test_data==0,]
type_6_train<-type_6_train %>% select(-c('test_data'))
type_6_test<-type_6_1[type_6_1$test_data==1,]
type_6_test<-type_6_test %>% select(-c('test_data','outcome'))
glimpse(type_6_train)
type_6_train$outcome<-as.factor(type_6_train$outcome)
glimpse(type_6_test)
sapply(type_6_train,levels)
sapply(type_6_test,levels)

fit.forest <-randomForest(outcome~.,data=type_6_train,importance=TRUE, ntree=500)
submission6<-type_6[type_6$test_data==1,]
submission6<-data.frame(activity_id=submission6$activity_id)
submission6$outcome<-predict(fit.forest,type_6_test)

###############################type 7######################################
levels(as.factor(type_7$char_10.x))

type_7_1<-type_7 %>%
  select(-c('people_id','activity_id','date.x','activity_category','char_1.x',
            'char_2.x','char_3.x','char_4.x','char_5.x','char_6.x','char_7.x','char_8.x',
            'char_9.x','char_10.x','group_1','date.y'))#test_data
type_7_train<-type_7_1[type_7_1$test_data==0,]
type_7_train<-type_7_train %>% select(-c('test_data'))
type_7_test<-type_7_1[type_7_1$test_data==1,]
type_7_test<-type_7_test %>% select(-c('test_data','outcome'))
glimpse(type_7_train)
type_7_train$outcome<-as.factor(type_7_train$outcome)
glimpse(type_7_test)
sapply(type_7_train,levels)
sapply(type_7_test,levels)

fit.forest <-randomForest(outcome~.,data=type_7_train,importance=TRUE, ntree=500)
submission7<-type_7[type_7$test_data==1,]
submission7<-data.frame(activity_id=submission7$activity_id)
submission7$outcome<-predict(fit.forest,type_7_test)
submission2<-read.csv('submission2.csv')
typeof(submission2$outcome)
submission2$outcome<-as.integer(submission2$outcome)
submissionall<-bind_rows(mutate_all(submission,as.character),
                         mutate_all(submission2,as.character),
                         mutate_all(submission3,as.character),
                         mutate_all(submission4,as.character),
                         mutate_all(submission5,as.character),
                         mutate_all(submission6,as.character),
                         mutate_all(submission7,as.character))
write.csv(submissionall, file = "submissionall.csv",row.names = FALSE)
# 
# 
# pro_sub<-read.csv('submissionall.csv')
# df_people
# 
# pro_sub$flag<-1
# pro_sub1<-left_join(pro_sub,test1233,by="activity_id")
# submissionall1<-pro_sub1 %>% 
#   select(c('group_1',"activity_id","outcome.x"))
# 
# names(submissionall1)[3] <- "outcome"
# submissionall1 %>%
#   filter(submissionall1$group_1=='group 17304' &submissionall1$outcome==1)
# pro_sub1
# test1233<-df_full[df_full$test_data==1,]
# glimpse(test1233)
