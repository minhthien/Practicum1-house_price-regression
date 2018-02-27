source_file <-"C:\\REGIS\\practium1\\data\\testCleanData.csv"
testData <- read.csv(source_file,header = TRUE)
str(testData)
feature_source_file <-"C:\\REGIS\\practium1\\data\\selectedFeature.csv"

selected_feautres <- read.csv(feature_source_file,header = TRUE)
str(selected_feautres)
selected_feautres <- t(selected_feautres) 
str(selected_feautres)
class(selected_feautres)
selected_feautres

library(dplyr)

selected_feautres_data <- testData %>% select(selected_feautres[,1:50])

library("vtreat")
library("magrittr")
treatplan <- designTreatmentsZ(selected_feautres_data, selected_feautres[,1:50], verbose = FALSE)
training.treat <- prepare(treatplan, selected_feautres_data, varRestriction = newvars)

scoreFrame <- treatplan$scoreFrame %>% select(varName,origName,code)
newvars <- scoreFrame %>% filter(code %in% c("clean", "lev")) %>% use_series(varName)

selected_feautres_data_XGboost_ready <- prepare(treatplan, selected_feautres_data, varRestriction = newvars)

XGboost_model <- readRDS("C:\\REGIS\\practium1\\XGboost_model.rds")
library(caret)
prediction <- predict(XGboost_model, as.matrix(selected_feautres_data_XGboost_ready))


result <- data.frame("Id"= testData$Id, "SalePrice" = prediction )
head(result)

write.csv(result, file = "C:\\REGIS\\practium1\\data\\XGBoost_prediction_1.csv",row.names=FALSE)

