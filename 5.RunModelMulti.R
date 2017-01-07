#########################
#This script creates an XGBoost model using the 'multi:softprob' objective
#input: Processed customer data and generated features from previous script
#output: Out of fold predictions for train data and predictions for test data.
#
#The best results were obtained by only training on June, 2015 data since
#it is the most similar to the month we are predicting (June 2016)
#Adding additional months decreased the model accuracy

library(xgboost)
library(data.table)
library(dplyr)
library(ggplot2)
library(Matrix)
library(caret)
library(corrplot)

#Change seed for different model runs
seed=1066
set.seed(seed)

#Load Processed Data
load("Dec18-XTrain_XTest-Data2.Rdata")

#only use June 2015 data
xtrain<-xtrain[xtrain$fecha_dato==6,]
y=y[which(xtrain$fecha_dato==6)] 
y=y-1

#only keep 1 month lag features and waffle features
xtrain_process<-xtrain[,grepl("lag_1_ind",names(xtrain)),with=FALSE]
xtrain_process=cbind(xtrain_process,xtrain[,grepl("waffle",names(xtrain)),with=FALSE])

xtest_process<-xtest[,grepl("lag_1_ind",names(xtest)),with=FALSE]
xtest_process=cbind(xtest_process,xtest[,grepl("waffle",names(xtest)),with=FALSE])

xtrain_use<-xtrain[,1:24,with=FALSE]
xtrain_use[,field_change:=xtrain$segmento_change]
xtrain_use[,zero_count:=xtrain$zero_count]
xtrain_use[,fecha_dato:=NULL]

xtest_use<-xtest[,1:24,with=FALSE]
xtest_use[,field_change:=xtest$segmento_change]
xtest_use[,zero_count:=xtest$zero_count]
xtest_use[,fecha_dato:=NULL]

##########################
#collapse product lags down to a single column
#ie instead of having a seperate feature for each previous month have a single feature
#This feature is 1 if the customer has ever owned the product in preceding months and 0 if the never owned it
for (category in category_names){
  train_sub<-as.integer(unlist(xtrain[,grepl(paste0("lag_",2,"_",category),names(xtrain)),with=FALSE]+xtrain[,grepl(paste0("lag_",3,"_",category),names(xtrain)),with=FALSE]+
                                 xtrain[,grepl(paste0("lag_",4,"_",category),names(xtrain)),with=FALSE] + xtrain[,grepl(paste0("lag_",5,"_",category),names(xtrain)),with=FALSE]))
  
  train_sub[train_sub>0]=1 
  xtrain_process[[paste0(category,"_owned")]]=train_sub
  
  
  test_sub<-as.integer(unlist(xtest[,grepl(paste0("lag_",2,"_",category),names(xtest)),with=FALSE]+xtest[,grepl(paste0("lag_",3,"_",category),names(xtest)),with=FALSE]+
                                xtest[,grepl(paste0("lag_",4,"_",category),names(xtest)),with=FALSE] + xtest[,grepl(paste0("lag_",5,"_",category),names(xtest)),with=FALSE]))
  
  test_sub[test_sub>0]=1 
  xtest_process[[paste0(category,"_owned")]]=test_sub
  
}

##########################
#collapse previouse ownerships and waffle features into one column to simplify model
#Place customers into one of 6 categories
# 0- Never ever used the product
# 1- Currently used this month, never used in the past
# 2- Don't use currently, have used in past
# 3- Used this month and have used in the past
# 4- Not Used
# 5- Not Used
# 6- Did not use this month but used in the past, added and removed multituple times
# 7- Used this month and have used in the past, added and removed multituple times

for (category in category_names){
  train_own<-as.integer(unlist((xtrain_process[,grepl(paste0("lag_",1,"_",category),names(xtrain_process)),with=FALSE])+(xtrain_process[,grepl(paste0(category,"_owned"),names(xtrain_process)),with=FALSE]*2)+
                                 (xtrain_process[,grepl(paste0(category,"_waffle"),names(xtrain_process)),with=FALSE]*4)))
  
  xtrain_use[[category]]=train_own
  
  test_own<-as.integer(unlist((xtest_process[,grepl(paste0("lag_",1,"_",category),names(xtest_process)),with=FALSE])+(xtest_process[,grepl(paste0(category,"_owned"),names(xtest_process)),with=FALSE]*2)+
                                (xtest_process[,grepl(paste0(category,"_waffle"),names(xtest_process)),with=FALSE]*4)))
  
  xtest_use[[category]]=test_own
}

xtrain<-xtrain_use
xtest<-xtest_use

    
##################
#Parameters for XGB

xgb_params = list(
  colsample_bytree= 0.9,
  subsample = 0.9,
  eta = 0.06,
  objective= 'multi:softprob',
  max_depth= 6,
  min_child_weight= 2,
  eval_metric= "mlogloss",
  num_class = length(category_names)
)

#convert xgbmatrix
dtest <- xgb.DMatrix(data.matrix(xtest))


#create folds
kfolds<-10
folds<-createFolds(y, k = kfolds, list = TRUE, returnTrain = FALSE)

allpredictions <- data.frame(matrix(ncol = length(category_names), nrow = nrow(xtest)))
allpredictions[is.na(allpredictions)]=0
out_of_fold <- data.frame(matrix(ncol = length(category_names), nrow = nrow(xtrain)))
names(out_of_fold)<-category_names
names(allpredictions)<-category_names

##Train Model
for (fold in folds){
  
  x_train<-xtrain[!fold,] #Train set
  x_val<-xtrain[fold,] #Out of fold validation set
  
  y_train<-y[-fold]
  y_val<-y[fold]
  
  
  #convert to xgbmatrix
  dtrain = xgb.DMatrix(as.matrix(x_train), label=y_train)
  dval = xgb.DMatrix(as.matrix(x_val), label=y_val)
  
  #perform training
  gbdt = xgb.train(params = xgb_params,
                   data = dtrain,
                   nrounds =1000,
                   watchlist = list(train = dtrain, val=dval),
                   print_every_n = 25,
                   early_stopping_rounds=25)
  
  #perform prediction
  allpredictions=allpredictions + as.data.frame(matrix(predict(gbdt,dtest), nrow=dim(xtest), byrow=TRUE))
  out_of_fold[fold,] <- as.data.frame(matrix(predict(gbdt,dval), nrow=dim(x_val), byrow=TRUE))
}

allpredictions = allpredictions/kfolds



save(allpredictions,out_of_fold,file="Dec18-MultiClass-10Fold-1066Seed.Rdata")
