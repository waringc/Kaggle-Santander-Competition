#########################
#This script cleans the raw data supplied by Santander
#input: Raw CSV supplied by Santander of customer info
#output: Processed RData of customer data

library(xgboost)
library(data.table)
library(dplyr)
library(ggplot2)
library(Matrix)
library(caret)

seed=1066
set.seed(seed)

#########################################
#Files to Open
TRAIN_FILE = "~/Desktop/kaggle/Santander/input/train_ver2.csv"
TEST_FILE = "~/Desktop/kaggle/Santander/input/test_ver2.csv"
SUBMISSION_FILE = "~/Desktop/kaggle/Santander/input/sample_submission.csv"



############################
##List of column names supplied by Santander, divided into categorical
##and numerical columns to allow appropriate processing.

cat_cols<-c("ind_empleado","pais_residencia","sexo","indresi", "indext","conyuemp","canal_entrada","indfall","nomprov","segmento","tiprel_1mes")
num_cols<-c("ind_nuevo","indrel","cod_prov",'ind_actividad_cliente')

#########################################
#Load files
train = fread(TRAIN_FILE, showProgress = TRUE,colClasses=list(character=c(11,12,16)))
test = fread(TEST_FILE, showProgress = TRUE,colClasses=list(character=c(11)))

#Ignore rare products added by customers to simplify model
#The following are rarely added.  A total of 45679 products were added
#in June 2015, and these are the counts of rate products
#"ind_ahor_fin_ult1" - 0/45679 Train
# ind_aval_fin_ult1" - 0/45679 Train
# "ind_cder_fin_ult1" - 9/45679 Train
#"ind_viv_fin_ult1" - 3/45679 Train
#"ind_ctju_fin_ult1" - 55/45679 Train
# "ind_deme_fin_ult1" - 33/45679 Train
#"ind_hip_fin_ult1" - 4/45679 Train
#"ind_plan_fin_ult1" - 21/45679 Train
#"ind_pres_fin_ult1" -8/45679 Train

product_ignore<-c("ind_ahor_fin_ult1","ind_aval_fin_ult1", "ind_deme_fin_ult1","ind_viv_fin_ult1","ind_deco_fin_ult1")
train[,which(names(train) %in% product_ignore, arr.ind = TRUE):=NULL]

category_names<-names(train)[(25:length(names(train)))]

#Load list of monthly new product add

#For training we are only interested in customers that added products
#load the new product data from previous script
load("MonthlyProductAddWithNewCust-Dec18.Rdata")
customers_added_products<-unique(c(products_added$ncodpers,test$ncodpers))

####Train Data
#Remove customers who don't added new data
#Data
train_data<-train[train$ncodpers %in% customers_added_products,]
train_data[,(25:dim(train_data)[2]):=NULL] # Remove product columns

#Products
train_products<-train[train$ncodpers %in% customers_added_products,]
train_products[,(3:24):=NULL]
train_products[is.na(train_products)]=0L


#combine traind and rest data for processing
ntrain<-nrow(train_data)
train_test<-rbind(train_data,test)


##############
#clean up character categorical data
#Set NA values to "" and encode categorical data to integers
for (feat in cat_cols){
  train_test[is.na(train_test[[feat]]),eval(feat):=""]
  train_test[[feat]]<-as.integer(factor(train_test[[feat]]))
}


##############
#tipdom-Address Type
#Remove NAs for tipodom column

train_test[is.na(train_test$tipodom),]$tipodom=0L


##############
#Customer end Date
#Encode customer end data as 1 (they have one), 0(don't have one)
train_test[train_test$ult_fec_cli_1t!="",]$ult_fec_cli_1t="1"
train_test[train_test$ult_fec_cli_1t=="",]$ult_fec_cli_1t="0"
train_test[,ult_fec_cli_1t:=as.integer(ult_fec_cli_1t)]

##############
#Factor customer start date
#create seperate features containing start date year and month
train_test[, fecha_alta := as.Date(fecha_alta)]
train_test[, fecha_alta_month  := as.integer(format(fecha_alta, format = '%m'))]
train_test[, fecha_alta  := as.integer(format(fecha_alta, format = '%y'))]

##############
#clean up customer type
train_test[train_test$indrel_1mes=="",]$indrel_1mes<-"99"
train_test[train_test$indrel_1mes=="P",]$indrel_1mes<-"5"
train_test[is.na(train_test$indrel_1mes),]$indrel_1mes="-1"
train_test[,indrel_1mes:=as.integer(indrel_1mes)]


##############
#clean up age

#Set unknown ages to mean
train_test[is.na(train_test$age),]$age = floor(mean(train_test$age,na.rm=TRUE))


##############
#clean up senority
#remove negative values
train_test[train_test$antiguedad < 0,]$antiguedad = 0

#Set unknown senority to mean
train_test[is.na(train_test$antiguedad),]$antiguedad = floor(mean(train_test$antiguedad,na.rm=TRUE))


##############
#clean up income

#Set unknown income to -1
train_test[is.na(train_test$renta),]$renta = -1

#Data is super skewed, try to get normal by taking log
train_test[,renta:=log(train_test$renta+500)]

##############
#clean up numeric columns

#Set NA values to 99
for (feat in num_cols){
  train_test[is.na(train_test[[feat]]),eval(feat):=99]
}

########
#Set unknown income to ind_nuevo and indrel_1mes to -1
train_test[is.na(train_test$ind_nuevo),]$ind_nuevo = -1
train_test[is.na(train_test$indrel_1mes),]$indrel_1mes = -1


##############
#With processing complete seperate the training and test data
train_processed<-train_test[(1:ntrain),]
test_processed<-train_test[((ntrain+1):dim(train_test)[1]),]


#Save Results as RData
save(train_processed,test_processed,train_products,category_names,file="Dec18-Test_TrainProcessedNewCust")
