#########################
#This script generates a list of new products added by customers for each
#month of the data supplied by Bosch
#input: Raw CSV supplied by Santander of customer info
#output: Processed RData of products added by customers each month

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
##Function to generate a list of products added by customers
# Requires input of the customer data.table and the desired month and
# its previous lag month
# Modified from: https://www.kaggle.com/alexeylive/santander-product-recommendation/june-2015-customers
new_products <- function(train, new_month, lag_month){
  
  newproducts<-subset(train,fecha_dato %in% c(new_month, lag_month))
  newproducts<-subset(newproducts,select=c(fecha_dato,ncodpers,25:length(names(train))))
  
  newproducts[, fecha_dato := as.Date(fecha_dato)]
  newproducts[, month_num  := as.integer(format(fecha_dato, format = '%m'))]
  newproducts[, fecha_dato := NULL]
  
  newproducts = melt(newproducts, measure = 2:(length(category_names)+1), variable.name = 'product_new')
  newproducts [, product_new := as.integer(product_new)]
  
  # pivot month (put month from columns to rows)
  newproducts = dcast(newproducts, ncodpers + product_new ~ month_num, value.var = c('value'))
  cols_in = c('ncodpers','product_new','month_05','month_06')
  setnames(newproducts, cols_in)
  newproducts[is.na(newproducts)] = 0L
  
  # added = present in current month and absent in prev
  month_added = newproducts[(month_06 == 1 & month_05 == 0)]
  
  #remove any "new" customers ie any customers with no previous history (ie atleast one month)
  # old_customers<-unique(train[train$fecha_dato %in% lag_month,]$ncodpers)
  # month_added = month_added[month_added$ncodpers %in% old_customers,]
  
  #june_added_customers = unique(june_added$ncodpers)
  month_added[,month_06:=NULL]
  month_added[,month_05:=NULL]
  month_added$month=new_month
  
  month_added
}


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

#Actual products ignored (found this combo thorugh iteration)
product_ignore<-c("ind_ahor_fin_ult1","ind_aval_fin_ult1", "ind_deme_fin_ult1","ind_viv_fin_ult1","ind_deco_fin_ult1")

#null out ignored columns
train[,which(names(train) %in% product_ignore, arr.ind = TRUE):=NULL]

category_names<-names(train)[(25:length(names(train)))]

#Generate new prodcuts for every month of data supplied.
products_added<-rbind(new_products(train, '2015-02-28', '2015-01-28'),
                      new_products(train, '2015-03-28', '2015-02-28'),
                      new_products(train, '2015-04-28', '2015-03-28'),
                      new_products(train, '2015-05-28', '2015-04-28'),
                      new_products(train, '2015-06-28', '2015-05-28'),
                      new_products(train, '2015-07-28', '2015-06-28'),
                      new_products(train, '2015-08-28', '2015-07-28'),
                      new_products(train, '2015-09-28', '2015-08-28'),
                      new_products(train, '2015-10-28', '2015-09-28'),
                      new_products(train, '2015-11-28', '2015-10-28'),
                      new_products(train, '2015-12-28', '2015-11-28'),
                      new_products(train, '2016-01-28', '2015-12-28'),
                      new_products(train, '2016-02-28', '2016-01-28'),
                      new_products(train, '2016-03-28', '2016-02-28'),
                      new_products(train, '2016-04-28', '2016-03-28'),
                      new_products(train, '2016-05-28', '2016-04-28'))

save(products_added,file="MonthlyProductAddWithNewCust-Dec18.Rdata")
