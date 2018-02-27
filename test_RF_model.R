final_model <- readRDS("C:\\REGIS\\practium1\\model_cv10_tune10.rds")
print(final_model)


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

str(selected_feautres_data)

library(caret)

prediction <- predict(final_model,selected_feautres_data)


result <- data.frame("Id"= testData$Id, "SalePrice" = prediction )
head(result)

write.csv(result, file = "C:\\REGIS\\practium1\\data\\result_prediction_1.csv",row.names=FALSE)
