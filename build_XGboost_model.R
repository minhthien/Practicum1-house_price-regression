selected_feature_data <- read.csv("C:\\REGIS\\practium1\\data\\selectedFeatureData.csv",header = TRUE , stringsAsFactors = FALSE)
str(selected_feature_data)


# 10.let one hot encoding the cleandata for categorical feature
library("vtreat")
library("magrittr")
library(dplyr)
treatplan <- designTreatmentsZ(selected_feature_data, colnames(selected_feature_data), verbose = FALSE)
training.treat <- prepare(treatplan, selected_feature_data, varRestriction = newvars)

scoreFrame <- treatplan$scoreFrame %>% select(varName,origName,code)
newvars <- scoreFrame %>% filter(code %in% c("clean", "lev")) %>% use_series(varName)
encoding_full_selected_feautres_data <- prepare(treatplan, selected_feature_data, varRestriction = newvars)
str(encoding_full_selected_feautres_data)
train_index <- 1458

set.seed(8)
train_predictors <- encoding_full_selected_feautres_data %>% select(-SalePrice_clean) %>% 
                    filter(between(row_number(),1,train_index))
str(train_predictors)

# 2. Gradient Boosting model

# find the optimal number of tree to build the model
library(xgboost)
cv <- xgb.cv(data = as.matrix(train_predictors), 
             label = encoding_full_selected_feautres_data[1:train_index,"SalePrice_clean"],
             nrounds = 200,
             nfold = 10,
             objective = "reg:linear",
             eta = 0.1,
             max_depth = 20,
             early_stopping_rounds = 100,
             verbose = 1   # silent
)

elog <- cv$evaluation_log

ntrees <- which.min(elog$test_rmse_mean)
ntrees

# build the model
model_xgb <- xgboost(data = as.matrix(train_predictors), # training data as matrix
                          label = encoding_full_selected_feautres_data[1:train_index,"SalePrice_clean"],
                          nrounds = ntrees,       # number of trees to build
                          objective = "reg:linear", # objective
                          eta = 0.1,
                          depth = 20,
                          verbose = 0  # silent
)

model_xgb

saveRDS(model_xgb, "C:\\REGIS\\practium1\\XGboost_model.rds")

# make prediction and make data ready to submmit to kaggle
testData <- encoding_full_selected_feautres_data[c(1459:2917),]
prediction <- predict(model_xgb, as.matrix(testData))
prediction
str(prediction)
options(scipen=999)

antilogPrediction <- 10^prediction
antilogPrediction
antilogPrediction <- round(antilogPrediction, digits = 0)
result <- data.frame("Id"= testData$Id, "SalePrice" = antilogPrediction )
head(result)
write.csv(result, file = "C:\\REGIS\\practium1\\data\\result_xgb_boost_model.csv",row.names=FALSE)




