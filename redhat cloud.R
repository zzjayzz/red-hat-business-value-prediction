library(readr) # import
library(dplyr) # data manipulation
library(tictoc) # timing models
library(date)



#################date transfermation#################################
lct <- Sys.getlocale("LC_TIME"); Sys.setlocale("LC_TIME", "C")
#install.packages('date')
df_train<-read.csv('~/Dropbox/act_train.csv')
df_people<-read.csv('~/Dropbox/people.csv')
df_test<-read.csv('~/Dropbox/act_test.csv')

df_train$test_data<-0
df_test$test_data<-1
df_full<-bind_rows(df_train,df_test)
df_full<-left_join(df_full,df_people,by="people_id")
df_full$date.x<-as.Date(df_full$date.x)
glimpse(df_full)

df_full$date1[df_full$date.x>='2022-07-17' & df_full$date.x<'2022-10-17']<-'1'
df_full$date1[df_full$date.x>='2022-10-17' & df_full$date.x<'2023-01-17']<-'2'
df_full$date1[df_full$date.x>='2023-01-17' & df_full$date.x<'2023-04-17']<-'3'
df_full$date1[df_full$date.x>='2023-04-17' & df_full$date.x<'2023-09-01']<-'4'
df_full$date1<-as.factor(df_full$date1)

glimpse(df_full)
df_full1<-df_full
df_full<-df_full[df_full$group_1 != 'group 17304',]

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
type_1_train<-type_1[type_1$test_data==0,]
type_1_train<-type_1_train %>%
  select(-c('activity_id','char_10.x','test_data','date.y','activity_category','date.x','people_id','group_1'))

cols <- c( "char_1.x", "char_2.x", "char_5.x",
           "outcome",'date1')
type_1_train[,cols]<-lapply(type_1_train[,cols], factor) 
glimpse(type_1_train)

############################################################################
sapply(type_1_train, levels)
colnames(type_1_train)
##############################################################
type_1_train[type_1_train$char_1.x=='type 46' | type_1_train$char_1.x=='type 47'|
               type_1_train$char_1.x=='type 48'|type_1_train$char_1.x=='type 44',] %>%
  select(char_1.x)

#######################################################
type_1_train[type_1_train$char_2.x=='type 32',] %>%
  select(char_2.x)

###########################################################
type_1_train[type_1_train$char_5.x=='type 7',] %>%
  select(char_5.x)

#####################################################
type_1_train[type_1_train$char_1.x=='type 50',] %>%
  select(char_1.x)

type_1_train_1<-type_1_train[-c(1795,
                                8304,
                                29492,
                                84496,
                                90262,
                                88094,
                                15928,
                                12871,
                                48826,
                                82483),]

type_1_train_1$char_1.x<-factor(type_1_train_1$char_1.x)
type_1_train_1$char_2.x<-factor(type_1_train_1$char_2.x)
type_1_train_1$char_5.x<-factor(type_1_train_1$char_5.x)

library(randomForest)
fit.forest <-randomForest(outcome~.,data=type_1_train_1,importance=TRUE, ntree=5)
train<-sapply(type_1_train_1, levels)
test<-sapply(type_1_test, levels)



type_1_test<-type_1[type_1$test_data==1,]
cols <- c("people_id", "char_1.x", "char_2.x", "char_5.x","outcome",'date1')
type_1_test[,cols]<-lapply(type_1_test[,cols], factor) 
type_1_test<-type_1_test %>%
  select(-c('people_id','group_1','activity_id','char_10.x','test_data','date.y','activity_category','date.x','outcome'))
count(type_1_test)
type_1_test_1<-type_1_test[type_1_test$char_1.x !='type 45',]
type_1_test_1[type_1_test_1$char_1.x =='type 45',]
type_1_test_1$char_1.x<-factor(type_1_test_1$char_1.x)
glimpse(type_1_test)
submission<-type_1[type_1$test_data==1 & type_1$char_1.x !="type 45",]
submission<-data.frame(activity_id=submission$activity_id)
submission$outcome<-predict(fit.forest,type_1_test_1)





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
type_2_train$outcome<-as.factor(as.character(type_2_train$outcome))
typeof(type_2_train$outcome)
fit.forest <-randomForest(outcome~.,data=type_2_train,importance=TRUE, ntree=5)
submission2<-type_2[type_2$test_data==1,]
submission2<-data.frame(activity_id=submission2$activity_id)
glimpse(submission2)
submission2$outcome<-predict(fit.forest,type_2_test)
glimpse(type_2_train)
levels(type_2_train)
levels(type_2_test$char_10.y)



count(type_3)

PER<-type_3 %>%
  group_by(char_10.x) %>%
  count() %>%
  arrange(desc(n))
PER$percentage<-PER$n/231797
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
                                 "type 8","type 3","type 55","type 515"), 
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
train<-sapply(type_3_train,levels)
test<-sapply(type_3_test,levels)

fit.forest <-randomForest(outcome~.,data=type_3_train,importance=TRUE, ntree=5)
# write.csv(type_3_train, file = "trainred3.csv",row.names = FALSE)
# write.csv(type_3_test, file = "testred3.csv",row.names = FALSE)
submission3<-type_3[type_3$test_data==1,]
submission3<-data.frame(activity_id=submission3$activity_id)
# write.csv(submission3, file = "submission33.csv",row.names = FALSE)
submission3$outcome<-predict(fit.forest,type_3_test)



count(type_4)
PER2<-type_4 %>%
  group_by(char_10.x) %>%
  count() %>%
  arrange(desc(n))
options(tibble.print_max = 500)
PER2$percentage<-PER2$n/175516
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
train<-sapply(type_4_train,levels)
test<-sapply(type_4_test,levels)


fit.forest <-randomForest(outcome~.,data=type_4_train,importance=TRUE, ntree=5)
submission4<-type_4[type_4$test_data==1,]
submission4<-data.frame(activity_id=submission4$activity_id)
submission4$outcome<-predict(fit.forest,type_4_test)






count(type_5)
PER3<-type_5 %>%
  group_by(char_10.x) %>%
  count() %>%
  arrange(desc(n))
PER3$percentage<-PER3$n/395331

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
train<-sapply(type_5_train,levels)
test<-sapply(type_5_test,levels)

train %in% test
fit.forest <-randomForest(outcome~.,data=type_5_train,importance=TRUE, ntree=5)
submission5<-type_5[type_5$test_data==1,]
submission5<-data.frame(activity_id=submission5$activity_id)
submission5$outcome<-predict(fit.forest,type_5_test)


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

fit.forest <-randomForest(outcome~.,data=type_6_train,importance=TRUE, ntree=5)
submission6<-type_6[type_6$test_data==1,]
submission6<-data.frame(activity_id=submission6$activity_id)
submission6$outcome<-predict(fit.forest,type_6_test)



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

fit.forest <-randomForest(outcome~.,data=type_7_train,importance=TRUE, ntree=5)
submission7<-type_7[type_7$test_data==1,]
submission7<-data.frame(activity_id=submission7$activity_id)
submission7$outcome<-predict(fit.forest,type_7_test)
