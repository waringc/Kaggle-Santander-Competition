#########################
#Combined predictions from various models and create submission CSV
#input: Model predictions and products previously owned by customers we are predicting
#output: Submission CSV
#
#The predictions from three models (two multi objective and one binary) were combined 
#to generate a single probability for each product that the customer added it in June, 2016
#The probabilities of any product the customer owned in May, 2016 is set to zero so
#that it is not recommended.  Since the evaluation metric is map@7, 7 product
#recommendations are included in the submission

library(data.table)
library(dplyr)



load("Dec18-XTrain_XTest-Data2.Rdata")

#First Multi Model
load("Dec18-MultiClass-10Fold-1066Seed.Rdata")
out_of_fold_multi1<-out_of_fold
allpredictionsmulti1<-allpredictions

#Second Multi Model
load("Dec18-MultiClass-10Fold-1985Seed.Rdata")
allpredictionsmulti2<-allpredictions
out_of_fold_multi2<-out_of_fold

#Single Binary Model
load("Dec18-BinaryClass-10Fold-1066Seed.Rdata")
allpredictionsbin<-allpredictions
out_of_fold_bin<-out_of_fold

#Combine the predictions from the three models
allpredictions2=.3*allpredictionsmulti1 + .3*allpredictionsmulti2 + .4*allpredictionsbin


#######################
###Create Predictions
cat("Predicting....")
pred_df<-allpredictions2
names(pred_df)<-category_names
pred_df$ncodpers<-test_id

############
##Remove any products that the customer already used/owned
##test_mayproducts contains the products owned by users in may before
#the month we are predicting of June 2016
#To avoid predicting this products set their probability to zero

#Create Mask
pred_mask<-as.data.frame(pred_df$ncodpers)
names(pred_mask)<-c("ncodpers")
pred_mask<-merge(pred_mask,test_mayproducts,by="ncodpers",all.x=TRUE,sort=FALSE)
pred_mask[pred_mask==1]=2
pred_mask[pred_mask==0]=1
pred_mask[pred_mask==2]=0
pred_mask[is.na(pred_mask)]=1

#apply mask
pred_mask$fecha_dato=NULL
pred_mask$ncodpers=NULL
pred_df$ncodpers=NULL
pred_df=pred_df*pred_mask

#create submission
sub<-as.data.frame(test_id)

#Generate submission of 7 highest probability products for each customer
sub$result<-apply(pred_df[1:(length(category_names))],1,function(x) paste(category_names[order(x,decreasing=TRUE)][1:7],sep= " ",collapse = " "))
names(sub)<-c("ncodpers","added_products")
sub = sub[,c(2,1)]

write.csv(sub,paste0(Sys.Date(),"submission.csv"),row.names = FALSE)

