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

# 1 build a radom forest model after the data is ready
library(caret)
myControl <- trainControl(
  method = "repeatedcv",
  number = 10,
  repeats = 3,
  verboseIter = TRUE
)

set.seed(8)
rf_model <- train(SalePrice_clean ~ .,encoding_full_selected_feautres_data[1:train_index,],
                  method = "ranger",  tuneLength = 10,
                  trControl = myControl)

rf_model 

# save the model to disk
saveRDS(rf_model, "C:\\REGIS\\practium1\\final_rf_model.rds")

# make prediction on test data
testData <- encoding_full_selected_feautres_data[c(1459:2917),]
prediction <- predict(rf_model,testData)


# anti log the prediction and round it off and write to file ready to submmit to kaggle
prediction
options(scipen=999)
antilogPrediction <- 10^prediction
antilogPrediction
str(testData)
nrow(antilogPrediction)
antilogPrediction
antilogPrediction <- round(antilogPrediction, digits = 0)
result <- data.frame("Id"= testData$Id, "SalePrice" = antilogPrediction )
head(result)
write.csv(result, file = "C:\\REGIS\\practium1\\data\\result_rf_model.csv",row.names=FALSE)









