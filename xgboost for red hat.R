
library(data.table)

library(FeatureHashing)

library(Matrix)

library(xgboost)

people <- fread("people.csv", showProgress = F)

p_logi <- names(people)[which(sapply(people, is.logical))]

for (col in p_logi) 
  
  set(people, j = col,value = as.integer(people[[col]]))

train  <- fread("act_train.csv", showProgress = F)
d1     <- merge(train, people, by = "people_id", all.x = T)
Y <- d1$outcome
d1[ , outcome := NULL]
colnames(d1)
typeof(Y)
b <- 2 ^ 22
f <- ~ . - people_id - activity_id - date.x - date.y - 1
X_train <- hashed.model.matrix(f, d1, hash.size = b)

sum(colSums(X_train) > 0)

set.seed(123456)
unique_p <- unique(d1$people_id)
valid_p  <- unique_p[sample(1:length(unique_p), 30000)]

valid<-which(d1$people_id %in% valid_p)
model <- (1:length(d1$people_id))[-valid]

param <- list(objective = "binary:logistic",
              eval_metric = "auc",
              booster = "gblinear",
              eta = 0.03)


dmodel  <- xgb.DMatrix(X_train[model, ], label = Y[model])
dvalid  <- xgb.DMatrix(X_train[valid, ], label = Y[valid])

m1 <- xgb.train(data = dmodel, param, nrounds = 200,
                watchlist = list(model = dmodel, valid = dvalid),
                print_every_n = 10)


dtrain  <- xgb.DMatrix(X_train, label = Y)

m2 <- xgb.train(data = dtrain, param, nrounds = 100,
                watchlist = list(train = dtrain),
                print_every_n = 10)
m2$



test <- fread("act_test.csv", showProgress = F)
d2   <- merge(test, people, by = "people_id", all.x = T)

X_test <- hashed.model.matrix(f, d2, hash.size = b)
dtest  <- xgb.DMatrix(X_test)

out <- predict(m2, dtest)
sub <- data.frame(activity_id = d2$activity_id, outcome = out)
write.csv(sub, file = "sub.csv", row.names = F)
summary(sub$outcome)


