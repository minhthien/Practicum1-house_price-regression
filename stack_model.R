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




encoding_full_selected_feautres_data

# 3. build stack model
myControl <- trainControl(method="repeatedcv", number=10, repeats=3, savePredictions="final")
library(dplyr)
final_data <- encoding_full_selected_feautres_data
target_index <- grep("SalePrice_clean", colnames(final_data))
model_list <- caretList(
  x=final_data[1:1458,-target_index],
  y=final_data[1:1458,target_index],
  trControl=myControl,
  methodList=c("ranger", "xgbTree")
)

library(caretEnsemble)
stack_model <- caretStack(model_list,method = "glm")
print(stack_model)
saveRDS(stack_model, "C:\\REGIS\\practium1\\rf_xgboost_stack.rds")

testData <- encoding_full_selected_feautres_data[c(1459:2917),]
prediction <- predict(stack_model,testData)

str(prediction)


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
write.csv(result, file = "C:\\REGIS\\practium1\\data\\result_stack_model.csv",row.names=FALSE)




