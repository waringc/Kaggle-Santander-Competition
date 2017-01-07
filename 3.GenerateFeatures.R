#########################
#This script generates features on the customer data
#Features are generated for each instance a new product was added in a month
#input: Processed customer data from previous script
#output: Customer data with additional engineered features ready for modelling
#
#There was a lot of copy and pasting in this script!!
#It could benefit from some cleaning up.

library(xgboost)
library(data.table)
library(dplyr)
library(ggplot2)
library(Matrix)
library(caret)

seed=1066
set.seed(seed)

############################
##Function to add "lag" features
# Lag feature is adding customer properties to an instance of a new product for specified previous months
# Ie for a product added in june, creating features for customer income in May, April etc
# "train_lag"- specifies which months to lag back
# "lag_items"- specified which features to lag (ie income, province and so on)
# The function returns a data table with the lag features added
generate_lag<-function(xtrain,train_processed,train_products,train_lag,lag_items){
  i= 1
  for (lag in train_lag){
    lag_products<-train_products[train_products$fecha_dato %in% lag,]
    lag_data<-train_processed[train_processed$fecha_dato %in% lag,]
    lag_data<-lag_data[,names(lag_data) %in% c("ncodpers",lag_items),with=FALSE]
    
    lag_products<-lag_products[,names(lag_products) %in% c("ncodpers",category_names),with=FALSE]
    lag_products[is.na(lag_products)] = 0L
    
    names(lag_products)<-c("ncodpers",paste("lag",i,names(lag_products)[2:length(names(lag_products))],sep="_"))
    names(lag_data)<-c("ncodpers",paste("lag",i,names(lag_data)[2:(length(lag_items)+1)],sep="_"))
    
    xtrain<-merge(xtrain,lag_products,by="ncodpers",all.x=TRUE,sort=FALSE)
    xtrain<-merge(xtrain,lag_data,by="ncodpers",all.x=TRUE,sort=FALSE)
    xtrain[is.na(xtrain)] = 0L
    i=i+1
  }
  
  xtrain
}

#Monthly Product Adds
load("MonthlyProductAddWithNewCust-Dec18.Rdata")

#Load processed data
load("Dec18-Test_TrainProcessedNewCust")

##############
#perform processing on train data
#each month is processed seperate
xtrain_june<-train_processed[train_processed$fecha_dato=="2015-06-28",]
xtrain_june=xtrain_june[xtrain_june$ncodpers %in% products_added[products_added$month=="2015-06-28",]$ncodpers,]
xtrain_june<-merge(products_added[products_added$month=="2015-06-28",],xtrain_june,by="ncodpers",all.y=TRUE)

xtrain_july<-train_processed[train_processed$fecha_dato=="2015-07-28",]
xtrain_july=xtrain_july[xtrain_july$ncodpers %in% products_added[products_added$month=="2015-07-28",]$ncodpers,]
xtrain_july<-merge(products_added[products_added$month=="2015-07-28",],xtrain_july,by="ncodpers",all.y=TRUE)

xtrain_aug<-train_processed[train_processed$fecha_dato=="2015-08-28",]
xtrain_aug=xtrain_aug[xtrain_aug$ncodpers %in% products_added[products_added$month=="2015-08-28",]$ncodpers,]
xtrain_aug<-merge(products_added[products_added$month=="2015-08-28",],xtrain_aug,by="ncodpers",all.y=TRUE)

xtrain_sept<-train_processed[train_processed$fecha_dato=="2015-09-28",]
xtrain_sept=xtrain_sept[xtrain_sept$ncodpers %in% products_added[products_added$month=="2015-09-28",]$ncodpers,]
xtrain_sept<-merge(products_added[products_added$month=="2015-09-28",],xtrain_sept,by="ncodpers",all.y=TRUE)

xtrain_oct<-train_processed[train_processed$fecha_dato=="2015-10-28",]
xtrain_oct=xtrain_oct[xtrain_oct$ncodpers %in% products_added[products_added$month=="2015-10-28",]$ncodpers,]
xtrain_oct<-merge(products_added[products_added$month=="2015-10-28",],xtrain_oct,by="ncodpers",all.y=TRUE)

xtrain_nov<-train_processed[train_processed$fecha_dato=="2015-11-28",]
xtrain_nov=xtrain_nov[xtrain_nov$ncodpers %in% products_added[products_added$month=="2015-11-28",]$ncodpers,]
xtrain_nov<-merge(products_added[products_added$month=="2015-11-28",],xtrain_nov,by="ncodpers",all.y=TRUE)

xtrain_dec<-train_processed[train_processed$fecha_dato=="2015-12-28",]
xtrain_dec=xtrain_dec[xtrain_dec$ncodpers %in% products_added[products_added$month=="2015-12-28",]$ncodpers,]
xtrain_dec<-merge(products_added[products_added$month=="2015-12-28",],xtrain_dec,by="ncodpers",all.y=TRUE)

xtrain_jan2<-train_processed[train_processed$fecha_dato=="2016-01-28",]
xtrain_jan2=xtrain_jan2[xtrain_jan2$ncodpers %in% products_added[products_added$month=="2016-01-28",]$ncodpers,]
xtrain_jan2<-merge(products_added[products_added$month=="2016-01-28",],xtrain_jan2,by="ncodpers",all.y=TRUE)

xtrain_feb2<-train_processed[train_processed$fecha_dato=="2016-02-28",]
xtrain_feb2=xtrain_feb2[xtrain_feb2$ncodpers %in% products_added[products_added$month=="2016-02-28",]$ncodpers,]
xtrain_feb2<-merge(products_added[products_added$month=="2016-02-28",],xtrain_feb2,by="ncodpers",all.y=TRUE)

xtrain_march2<-train_processed[train_processed$fecha_dato=="2016-03-28",]
xtrain_march2=xtrain_march2[xtrain_march2$ncodpers %in% products_added[products_added$month=="2016-03-28",]$ncodpers,]
xtrain_march2<-merge(products_added[products_added$month=="2016-03-28",],xtrain_march2,by="ncodpers",all.y=TRUE)

xtrain_april2<-train_processed[train_processed$fecha_dato=="2016-04-28",]
xtrain_april2=xtrain_april2[xtrain_april2$ncodpers %in% products_added[products_added$month=="2016-04-28",]$ncodpers,]
xtrain_april2<-merge(products_added[products_added$month=="2016-04-28",],xtrain_april2,by="ncodpers",all.y=TRUE)

xtrain_may2<-train_processed[train_processed$fecha_dato=="2016-05-28",]
xtrain_may2=xtrain_may2[xtrain_may2$ncodpers %in% products_added[products_added$month=="2016-05-28",]$ncodpers,]
xtrain_may2<-merge(products_added[products_added$month=="2016-05-28",],xtrain_may2,by="ncodpers",all.y=TRUE)

##############
#perform processing on test data
xtest<-test_processed[test_processed$fecha_dato=="2016-06-28",]

##############
#Generate lag features, each month in training data is processed seperate
#for each month the lag features are generated for the 5 preceding months

#lag data features, the features to lag
lag_items<-c("age","ind_actividad_cliente","segmento","cod_prov",  "pais_residencia", 'tiprel_1mes', 'canal_entrada', 'renta', 'antiguedad')

#Create lag features for each month in train
xtrain_june<-generate_lag(xtrain_june,train_processed,train_products,c("2015-05-28","2015-04-28","2015-03-28","2015-02-28","2015-01-28"),lag_items)
xtrain_july<-generate_lag(xtrain_july,train_processed,train_products,c("2015-06-28","2015-05-28","2015-04-28","2015-03-28","2015-02-28"),lag_items)
xtrain_aug<-generate_lag(xtrain_aug,train_processed,train_products,c("2015-07-28","2015-06-28","2015-05-28","2015-04-28","2015-03-28"),lag_items)
xtrain_sept<-generate_lag(xtrain_sept,train_processed,train_products,c("2015-08-28","2015-07-28","2015-06-28","2015-05-28","2015-04-28"),lag_items)
xtrain_oct<-generate_lag(xtrain_oct,train_processed,train_products,c("2015-09-28","2015-08-28","2015-07-28","2015-06-28","2015-05-28"),lag_items)
xtrain_nov<-generate_lag(xtrain_nov,train_processed,train_products,c("2015-10-28","2015-09-28","2015-08-28","2015-07-28","2015-06-28"),lag_items)
xtrain_dec<-generate_lag(xtrain_dec,train_processed,train_products,c("2015-11-28","2015-10-28","2015-09-28","2015-08-28","2015-07-28"),lag_items)
xtrain_jan2<-generate_lag(xtrain_jan2,train_processed,train_products,c("2015-12-28","2015-11-28","2015-10-28","2015-09-28","2015-08-28"),lag_items)
xtrain_feb2<-generate_lag(xtrain_feb2,train_processed,train_products,c("2016-01-28","2015-12-28","2015-11-28","2015-10-28","2015-09-28"),lag_items)
xtrain_march2<-generate_lag(xtrain_march2,train_processed,train_products,c("2016-02-28","2016-01-28","2015-12-28","2015-11-28","2015-10-28"),lag_items)
xtrain_april2<-generate_lag(xtrain_april2,train_processed,train_products,c("2016-03-28","2016-02-28","2016-01-28","2015-12-28","2015-11-28"),lag_items)
xtrain_may2<-generate_lag(xtrain_may2,train_processed,train_products,c("2016-04-28","2016-03-28","2016-02-28","2016-01-28","2016-12-28"),lag_items)

#Create lag features for test
xtest<-generate_lag(xtest,train_processed,train_products,c("2016-05-28","2016-04-28","2016-03-28","2016-02-28","2016-01-28"),lag_items)


#####################
#Generate a list of products customers owned before the month we are predicting (June, 2016) 
#This is needed so that we don't end up predicting the customers adding a product they already have!
test_mayproducts<-train_products[train_products$fecha_dato=="2016-05-28"]
test_mayproducts=test_mayproducts[test_mayproducts$ncodpers %in% test_processed$ncodpers]
test_mayproducts=test_mayproducts[match(test_processed$ncodpers, test_mayproducts$ncodpers),]
test_mayproducts[is.na(test_mayproducts)] = 0L

###########################
###Combine train data for each month into a single data frame
xtrain<-rbind(xtrain_june,xtrain_july,xtrain_aug,xtrain_sept,xtrain_oct,xtrain_nov,xtrain_dec,xtrain_jan2,xtrain_feb2,xtrain_march2,xtrain_april2,xtrain_may2)
xtrain[,month:=NULL]


train_id<-xtrain$ncodpers
xtrain[,ncodpers:=NULL]

xtrain[, fecha_dato := as.Date(fecha_dato)]
xtrain[, fecha_dato  := as.integer(format(fecha_dato, format = '%m'))]

y<-xtrain$product_new
xtrain[,product_new := NULL]

#Clean up test data
test_id<-xtest$ncodpers
xtest[,ncodpers:=NULL]

xtest[, fecha_dato := as.Date(fecha_dato)]
xtest[, fecha_dato  := as.integer(format(fecha_dato, format = '%m'))]

###########################
###Add additional features 



#######
#Count how many times customers age is 0 (unknown is previous months)
ra_train<-xtrain[,grepl("age",names(xtrain)),with=FALSE]
ra_test<-xtest[,grepl("age",names(xtest)),with=FALSE]

xtrain$zero_count<-rowSums(ra_train[,grepl("age",names(ra_train)),with=FALSE]==0)
xtest$zero_count<-rowSums(ra_test[,grepl("age",names(ra_test)),with=FALSE]==0)


#############
#Add a flag if the customer status switched from inactive to active in a given month
xtrain$went_active = 0
xtest$went_active = 0

xtrain[ind_actividad_cliente==1 & lag_1_ind_actividad_cliente==0,]$went_active = 1
xtest[ind_actividad_cliente==1 & lag_1_ind_actividad_cliente==0,]$went_active = 1

#Remove Lag for status after this feature is added
xtrain[,grep("_ind_actividad_cliente",names(xtrain)):=NULL]
xtest[,grep("_ind_actividad_cliente",names(xtest)):=NULL]

#######
#Add feature to see if they user age changed in a month
xtrain$age_change = 0
xtest$age_change = 0


xtrain[age!=lag_1_age,]$age_change = 1
xtest[age!=lag_1_age,]$age_change = 1

#Remove Lag for age after this feature is added
xtrain[,grep("_age",names(xtrain)):=NULL]
xtest[,grep("_age",names(xtest)):=NULL]

########
#Add feature if the customer segment changed this month
xtrain$segmento_change = 0
xtest$segmento_change = 0

xtrain[segmento !=lag_1_segmento ,]$segmento_change = 1
xtest[segmento !=lag_1_segmento ,]$segmento_change = 1

#Remove segmento lags
xtrain[,grep("_segmento",names(xtrain)):=NULL]
xtest[,grep("_segmento",names(xtest)):=NULL]


######
#Add feature if the customer province changed this month
xtrain$codprov_change = 0
xtest$codprov_change = 0

xtrain[cod_prov !=lag_1_cod_prov ,]$codprov_change = 1
xtest[cod_prov !=lag_1_cod_prov ,]$codprov_change = 1

#Remove province lags
xtrain[,grep("_cod_prov",names(xtrain)):=NULL]
xtest[,grep("_cod_prov",names(xtest)):=NULL]

######
#Add feature if the customer country changed this month
xtrain$pais_change = 0
xtest$pais_change = 0

xtrain[pais_residencia !=lag_1_pais_residencia ,]$pais_change = 1
xtest[pais_residencia !=lag_1_pais_residencia ,]$pais_change = 1

#Remove country lags
xtrain[,grep("_pais_residencia",names(xtrain)):=NULL]
xtest[,grep("_pais_residencia",names(xtest)):=NULL]


######
#Add a did tiprel_1mes change feature?
xtrain$tip_change = 0
xtest$tip_change = 0

xtrain[tiprel_1mes !=lag_1_tiprel_1mes ,]$tip_change = 1
xtest[tiprel_1mes !=lag_1_tiprel_1mes ,]$tip_change = 1

#Remove tiprel_1mes lags
xtrain[,grep("_tip",names(xtrain)):=NULL]
xtest[,grep("_tip",names(xtest)):=NULL]

######
#Add a did canel (method customer was added) change feature
xtrain$canal_change = 0
xtest$canal_change = 0

xtrain[canal_entrada !=lag_1_canal_entrada ,]$canal_change = 1
xtest[canal_entrada !=lag_1_canal_entrada ,]$canal_change = 1

#Remove canel lags
xtrain[,grep("_canal_entrada",names(xtrain)):=NULL]
xtest[,grep("_canal_entrada",names(xtest)):=NULL]


######
#Add a did income change this month feature
xtrain$renta_change = 0
xtest$renta_change = 0

xtrain[renta !=lag_1_renta ,]$renta_change = 1
xtest[renta !=lag_1_renta ,]$renta_change = 1

#Remove income lags
xtrain[,grep("_renta",names(xtrain)):=NULL]
xtest[,grep("_renta",names(xtest)):=NULL]

######
#Add a did customer senority change this month
xtrain$antiguedad_change = 0
xtest$antiguedad_change = 0

xtrain[antiguedad !=lag_1_antiguedad ,]$antiguedad_change  = 1
xtest[antiguedad !=lag_1_antiguedad ,]$antiguedad_change  = 1

#Remove senority lags
xtrain[,grep("_antiguedad",names(xtrain)):=NULL]
xtest[,grep("_antiguedad",names(xtest)):=NULL]


########
#Add waffle features
#Waffle features are does the customer have a habit of adding and removing
#a given product mutiple times (ie they waffle)
train_waffles<-data.table(train_id)
names(train_waffles)<-c("ncodpers")

test_waffles<-data.table(test_id)
names(test_waffles)<-c("ncodpers")

#add feature to see if the person is a "waffler" for that product
for (product in category_names){
  train_waffle<-xtrain[,grep(product,names(xtrain)),with=FALSE]
  train_waffle$collapsed<-apply(train_waffle,1,paste,sep="",collapse="")
  train_waffle$collapsed=gsub('([[:digit:]])\\1+', '\\1', train_waffle$collapsed)
  train_waffle$collapsed=nchar(train_waffle$collapsed)
  new_names<-c(names(train_waffles), paste0(product,"_waffle"))
  train_waffles=cbind(train_waffles,train_waffle$collapsed)
  setnames(train_waffles,new_names)
  
  test_waffle<-xtest[,grep(product,names(xtest)),with=FALSE]
  test_waffle$collapsed<-apply(test_waffle,1,paste,sep="",collapse="")
  test_waffle$collapsed=gsub('([[:digit:]])\\1+', '\\1', test_waffle$collapsed)
  test_waffle$collapsed=nchar(test_waffle$collapsed)
  new_names<-c(names(test_waffles), paste0(product,"_waffle"))
  test_waffles<-cbind(test_waffles,test_waffle$collapsed)
  setnames(test_waffles,new_names)
}

#Null out codpers from waffles
train_waffles[,ncodpers:=NULL]
test_waffles[,ncodpers:=NULL]

train_waffles[train_waffles<3] = 0
train_waffles[train_waffles>2] = 1

test_waffles[test_waffles<3] = 0
test_waffles[test_waffles>2] = 1

xtrain<-cbind(xtrain,train_waffles)
xtest<-cbind(xtest,test_waffles)


##Save data with added features to RData
save(xtrain,xtest, y,train_id,test_id,test_mayproducts,category_names,file="Dec18-XTrain_XTest-Data2.Rdata")
