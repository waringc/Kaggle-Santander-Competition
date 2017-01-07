# Kaggle- Santander Product Recommendation
These are the scripts I created for the Kaggle [Santander Product Recommendation](https://www.kaggle.com/c/santander-product-recommendation).  

This model finished 99th out of 1785 teams.

## Data
The goal of the competition was to predict which products existing Santander customers would use in an upcoming month.  The data is publicly available [here](https://www.kaggle.com/c/santander-product-recommendation/data).

The recommendations generate for the competition were evaluated using [Mean Average Precision @ 7] (https://www.kaggle.com/wiki/MeanAveragePrecision) (MAP@7).

## Dependancies
R and RStudio were used to create the model.  The following R packages were used:

* dplyr
* data.table
* xgboost
* Matrix
* caret
* Metrics

## Content

The files used to build the model are summarized below:

##### Data Processing

  **1.GenerateProductsAddedList.R**- The goal of the competition is predict the products used each month for a customer.  To perform training we therefore need a list of which customers added new products, what that product was and when that product was added. This script generates the list of products added by customers.  Each line in the generated data table is one product added.  If a customer added 5 products in one month than the customer appears 5 times in the data table that month, once for each product.

  **2.DataProcessing.R**- This cleans and processes the train and test data to allow the model to be built.  NA and missing values are corrected.  Categorical features are encoded as integers.  Processing is only performed on customers known to have added a product based on results from script 1.  Only customers who add a product are used to build the model.   

##### Feature Engineering

  **3.GenerateFeatures.R**- Features are added/generated for the model.  The data is structured around one row or observation for each product added in a month.  Much of the feature generation is adding lag features to each of these rows.  The lag features include information such as what was the customer income or province 1, 2 or 3 months ago? The goal of these features is to identify if anything about the customer has recently changed which could cause them to use a new banking product (ie their income changed or they recently moved).  Some customers also do not use a product consistently and are constantly adding and removing it (this seemed to be very common with credit cards (the ind_tjcr_fin_ult1 category)).  Features were added to identify any of these products a customer had a history of removing and adding. This script is a little messy and could benefit from some cleaning up!

##### Model Building/Prediction

  **4.RunModelBinary.R/5.RunModelMulti.R**- These scripts generate the models using XGBoost.  Training is only performed on new products added in June 2015.  Adding additional months from other months in the training data only reduced accuracy.  Since, we are predicting product adds for June, 2016 this suggests each month has unique product add behavior.  The training data is folded 10 times. Out of fold predictions and predictions for the test data are predicted for every fold.  The test data predictions are averaged over the 10 folds.  The binary script uses the binary:logistic objective in XGBoost. The the multi script uses the multi:softprob objective in XGBoost.  The multi script was run twice with different seeds and the binary script was run once.

  **6.GenerateSubmission.R**-  Generates the final submission CSV file for Kaggle.  The predictions from the three previous models are blended together to generate a final probability of each customer adding each product.  The probability of products which the customer already owned are set to zero to avoid recommending a product they are already using.  The 7 highest probability products for each customer are recommended in the submission file since the competition evaluation metric is MAP@7.
