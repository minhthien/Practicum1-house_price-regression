source_file <-"C:\\REGIS\\practium1\\data\\test.csv"
orgData <- read.csv(source_file,header = TRUE , stringsAsFactors = FALSE)

library("mice")
col_na_to_none <- c("Alley","BsmtQual","BsmtCond","BsmtExposure","BsmtFinType1","BsmtFinType2",
                    "FireplaceQu","GarageType","GarageFinish","GarageQual","GarageCond",
                    "PoolQC","Fence","MiscFeature")

col_na_to_none
str(orgData[col_na_to_none])

library(dplyr)
library(tidyr)
remove_na_data <- orgData[is.na(col_na_to_none)] <- "None"
str(remove_na_data)

orgData[col_na_to_none][is.na(orgData[col_na_to_none])] <- "None"

cleanData <- as.data.frame(unclass(orgData))

#imputing missing data
m <- mice(cleanData, method="rf")
cleanData <- mice::complete(m)
anyNA(cleanData)
write.csv(cleanData, file = "C:\\REGIS\\practium1\\data\\testCleanData.csv",row.names=FALSE)


library(Boruta)
set.seed(8)
feature.selection <- Boruta(SalePrice ~., data = cleanData, doTrace = 1)
table(feature.selection$finalDecision)
slelected_feature <- getSelectedAttributes(feature.selection)
write.csv(slelected_feature,file = "C:\\REGIS\\practium1\\data\\selectedFeature.csv",row.names=FALSE)



