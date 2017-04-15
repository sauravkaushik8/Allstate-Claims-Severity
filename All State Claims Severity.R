

library(data.table)
library(Matrix)
library(xgboost)
library(Metrics)
library(plyr)

ID = 'id'
TARGET = 'loss'
SEED = 8

TRAIN_FILE = "train.csv"
TEST_FILE = "test.csv"
SUBMISSION_FILE = "sample_submission.csv"


train = read.csv(TRAIN_FILE,stringsAsFactors = F)
test = read.csv(TEST_FILE,stringsAsFactors = F)





y_train=log(train[,TARGET]+200)


#for(i in 1:188318){
#  s<-0
#  for(j in 2:73){
#    if(train[i,j]=='A'){
#      s<-s+1}
#  }
#  train[i,"comb"]<-s
#}




#for(i in 1:125546){
#  s<-0
#  for(j in 2:73){
#    if(test[i,j]=='A'){
#      s<-s+1}
#  }
#  test[i,"comb"]<-s
#}

str(train)

#a<-train$comb
#b<-test$comb

#train$comb<-a
#test$comb<-b

train[, c("id","loss")] <- NULL
test[, c("id")] <- NULL


ntrain = nrow(train)
train_test = rbind(train, test)


for(i in 1:116)
{
  train_test[,i]<-as.numeric(as.factor(train_test[,i]))
}

library(ensembleR)

predictions<-train



cluster_cat<-kmeans(train_test[,1:116],10)
cluster_cont<-kmeans(train_test[,117:130],10)

train_test$cluster_cat<-(cluster_cat$cluster)
train_test$cluster_cont<-(cluster_cont$cluster)

#library(caret)

#dmy <- dummyVars(" ~ .", data = train_test,fullRank = T)
#train_test_Trsf <- data.frame(predict(dmy, newdata = train_test))


x_train = train_test[1:ntrain,]
x_test = train_test[(ntrain+1):nrow(train_test),]

x_train$loss<-y_train



outcomeName <- c('loss')
predictors <- names(x_train)[!names(x_train) %in% outcomeName]



library(xgboost)


require(caTools)
set.seed(101) 
sample = sample.split(x_train, SplitRatio = .75)
trainSet = subset(x_train, sample == TRUE)
testSet = subset(x_train, sample == FALSE)

smallestError <- 10000
for (depth in seq(1,10,1)) {
  for (rounds in seq(0,40,10)) {
    
    # train
    bst <- xgboost(data = as.matrix(trainSet[,pr]),
                   label = trainSet[,outcomeName],
                   max.depth=depth, nround=rounds,
                   objective = "reg:linear", verbose=0)
    gc()
    
    # predict
    predictions <- predict(bst, as.matrix(testSet[,predictors]), outputmargin=TRUE)
    err <- mae(as.numeric(exp(testSet[,outcomeName])), exp(as.numeric(predictions)))
    
    if (err < smallestError) {
      smallestError = err
      print(paste(depth,rounds,err))
    }     
  }
}  


xgb_params = list(
  seed = 0,
  colsample_bytree = 0.7,
  subsample = 0.7,
  eta = 0.075,
  objective = 'reg:linear',
  max_depth = 6,
  num_parallel_tree = 1,
  min_child_weight = 1,
  base_score = 7
)
set.seed(121)

pr<-c('cat80','comb','cat79','cont14','cat1','cont2')

bst <- xgboost(params = xgb_params,data = as.matrix(x_train[,predictors]),
               label = x_train[,outcomeName],
               objective = "reg:linear", verbose=1,nrounds=900)




library('caret')

model_xgb<-train(x_train[,predictors],x_train[,outcomeName],method='xgbLinear')

bst <- xgboost(data = as.matrix(x_train[,predictors]),
               label = trainSet[,outcomeName],
               max.depth=, nround=,
               objective = "reg:linear", verbose=0)



predictions <- predict(bst, as.matrix(x_test[,predictors]), outputmargin=TRUE)






dtrain = xgb.DMatrix(as.matrix(x_train), label=y_train)

dtrain = (as.matrix(trainSet))

dtest = xgb.DMatrix(as.matrix(x_test))





##

RANDOM_STATE = 2016
params = {
  'min_child_weight': 1,
  'eta': 0.01,
  'colsample_bytree': 0.5,
  'max_depth': 12,
  'subsample': 0.8,
  'alpha': 1,
  'gamma': 1,
  'silent': 1,
  'verbose_eval': True,
  'seed': RANDOM_STATE
}

##




xgb_params = list(
  seed = 0,
  colsample_bytree = 0.7,
  subsample = 0.7,
  eta = 0.075,
  objective = 'reg:linear',
  max_depth = 6,
  num_parallel_tree = 1,
  min_child_weight = 1,
  base_score = 7
)

xg_eval_mae <- function (yhat, dtrain) {
  y = getinfo(dtrain, "label")
  err= mae(exp(y),exp(yhat) )
  return (list(metric = "error", value = err))
}

res = xgb.cv(xgb_params,
             dtrain,
             nrounds=750,
             nfold=4,
             early_stopping_rounds=15,
             print_every_n = 10,
             verbose= 1,
             feval=xg_eval_mae,
             maximize=FALSE)

str(train)
best_nrounds = res$best_iteration
cv_mean = res$evaluation_log$test_error_mean[best_nrounds]
cv_std = res$evaluation_log$test_error_std[best_nrounds]
cat(paste0('CV-Mean: ',cv_mean,' ', cv_std))

gbdt = xgb.train(xgb_params, x_train, best_nrounds)

submission = fread(SUBMISSION_FILE, colClasses = c("integer", "numeric"))
submission$loss = exp(predict(gbdt,dtest))
write.csv(submission,'xgb_starter_v2.sub.csv',row.names = FALSE)



train<-read.csv("train.csv")
train<-read.csv("test.csv")

for(i in 1:188318){
  s<-0
  for(j in 2:73){
    if(train[i,j]=='A'){
      s<-s+1}
  }
  train[i,"comb"]<-s
}


for(i in 1:125546){
  s<-0
  for(j in 2:73){
    if(test[i,j]=='A'){
      s<-s+1}
  }
  test[i,"comb"]<-s
}

a<-train$comb
b<-test$comb


rm(test,train)


library(data.table)
library(Matrix)
library(xgboost)
library(Metrics)

ID = 'id'
TARGET = 'loss'
SEED = 0

TRAIN_FILE = "train.csv"
TEST_FILE = "test.csv"
SUBMISSION_FILE = "sample_submission.csv"


train = fread(TRAIN_FILE)
test = fread(TEST_FILE)




y_train = log(train[,TARGET, with = FALSE])[[TARGET]]


train[, c(ID, TARGET) := NULL]
test[, c(ID) := NULL]

ntrain = nrow(train)
train_test = rbind(train, test)



features = names(train)

for (f in features) {
  if (class(train_test[[f]])=="character") {
    #cat("VARIABLE : ",f,"\n")
    levels <- unique(train_test[[f]])
    train_test[[f]] <- as.integer(factor(train_test[[f]], levels=levels))
  }
}

train_test<-as.data.frame(train_test)


cluster<-kmeans(train_test[,101:130],10)

train_test$cluster<-cluster$cluster

x_train = train_test[1:ntrain,]
x_test = train_test[(ntrain+1):nrow(train_test),]

x_train$comb<-a
x_test$comb<-b


# set up the cross-validated hyper-parameter search
xgb_grid_1 = expand.grid(
  nrounds = 1000,
  eta = c(0.01, 0.001, 0.0001),
  max_depth = c(2, 4, 6, 8, 10),
  gamma = 1
)

# pack the training control parameters
xgb_trcontrol_1 = trainControl(
  method = "cv",
  number = 5,
  verboseIter = TRUE,
  returnData = FALSE,
  returnResamp = "all",                                                        # save losses across all models
  classProbs = TRUE,                                                           # set to TRUE for AUC to be computed
  summaryFunction = twoClassSummary,
  allowParallel = TRUE
)

# train the model for each parameter combination in the grid,
#   using CV to evaluate
xgb_train_1 = train(
  x = as.matrix(x_train),
  y = as.factor(y_train),
  trControl = xgb_trcontrol_1,
  tuneGrid = xgb_grid_1,
  method = "xgbLinear"
)

# scatter plot of the AUC against max_depth and eta
ggplot(xgb_train_1$results, aes(x = as.factor(eta), y = max_depth, size = ROC, color = ROC)) +
  geom_point() +
  theme_bw() +
  scale_size_continuous(guide = "none")</code>
  
  
  
  
  
  
  
  
  
  
  
dtrain = xgb.DMatrix(as.matrix(x_train), label=y_train)
dtest = xgb.DMatrix(as.matrix(x_test))


#x_train = train_test[1:ntrain,]
#x_test = train_test[(ntrain+1):nrow(train_test),]

#x_train$loss<-y_train




#x_train<-data.frame(x_train)
#x_test<-data.frame(x_test)


xgb_params = list(
  seed = 0,
  colsample_bytree = 0.7,
  subsample = 0.7,
  eta = 0.075,
  objective = 'reg:linear',
  max_depth = 6,
  num_parallel_tree = 1,
  min_child_weight = 1,
  base_score = 7
)

xg_eval_mae <- function (yhat, dtrain) {
  y = getinfo(dtrain, "label")
  err= mae(exp(y),exp(yhat) )
  return (list(metric = "error", value = err))
}

set.seed(101)
res = xgb.cv(xgb_params,
             dtrain,
             nrounds=750,
             nfold=4,
             early_stopping_rounds=15,
             print_every_n = 10,
             verbose= 1,
             feval=xg_eval_mae,
             maximize=FALSE)


x_train$loss<-y_train


outcomeName <- c('loss')
predictors <- names(x_train)[!names(x_train) %in% outcomeName]




best_nrounds = res$best_iteration
cv_mean = res$evaluation_log$test_error_mean[best_nrounds]
cv_std = res$evaluation_log$test_error_std[best_nrounds]
cat(paste0('CV-Mean: ',cv_mean,' ', cv_std))

gbdt = xgb.train(xgb_params, dtrain, nrounds=10)


xgb.plot.importance(xgb.importance(feature_names = predictors,model=bst)[1:20])


submission = read.csv(SUBMISSION_FILE, colClasses = c("integer", "numeric"))
submission$loss = exp(predict(bst,as.matrix(x_test)))
submission$loss<-submission$loss-200
write.csv(submission,'xgb_sk3.csv',row.names = FALSE)

#