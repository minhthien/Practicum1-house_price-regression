trainData <- read.csv("C:\\REGIS\\practium1\\data\\train.csv",header = TRUE , stringsAsFactors = FALSE)
testData <-   read.csv("C:\\REGIS\\practium1\\data\\test.csv",header = TRUE , stringsAsFactors = FALSE)
head(trainData$SalePrice)
train_index <- 1460

str(trainData)
str(testData)
testData$SalePrice <- 0

# 1.remove outliers GrLivArea > 4k but SalePRice < 200k
library(plyr)
library(dplyr)
trainData <- trainData %>% filter(!(GrLivArea > 4000 & SalePrice < 200000))
str(trainData)
train_index <- 1458

# 2.	Log transform "SalePrice" response variable to remove right skew
trainData$SalePrice <- log10(trainData$SalePrice)
library(ggplot2)
ggplot(trainData,aes(SalePrice)) + geom_histogram()
summary(trainData$SalePrice)




# 3.	Combine train.csv and test.csv into one dataset
full_data <- rbind(trainData,testData)
str(full_data)
summary(full_data$SalePrice)

# 4.these feature below have NA for ther value but it is not missing data
#   but simply that house does not have that feature so convert NA to NONE 
library("mice")
col_na_to_none <- c("Alley","BsmtQual","BsmtCond","BsmtExposure","BsmtFinType1","BsmtFinType2",
                    "FireplaceQu","GarageType","GarageFinish","GarageQual","GarageCond",
                    "PoolQC","Fence","MiscFeature")

full_data[col_na_to_none][is.na(full_data[col_na_to_none])] <- "None"
na_to_none_Data <- as.data.frame(unclass(full_data))
str(na_to_none_Data[col_na_to_none])
summary(na_to_none_Data)

# 5.these three feature is categorical feature but sea enterd as numberic
#   so convert them back to factor
na_to_none_Data$MSSubClass <- as.factor(na_to_none_Data$MSSubClass)
na_to_none_Data$MoSold <- as.factor(na_to_none_Data$MoSold)
na_to_none_Data$YrSold <- as.factor(na_to_none_Data$YrSold)
str(na_to_none_Data$MSSubClass)
str(na_to_none_Data$MoSold)
str(na_to_none_Data$YrSold)


# sepeate numberic and categorical feature so easy to clean it up
numberic_only_data <- na_to_none_Data %>% select(which(sapply(na_to_none_Data, is.numeric)))
factor_only_data <- na_to_none_Data %>% select(which(sapply(na_to_none_Data, is.factor)))
str(numberic_only_data)
str(factor_only_data)

# 6.imputing missing data for numberic data
m <- mice(numberic_only_data, method="rf")
no_missing_numberic_data <- mice::complete(m)
sum(is.na(no_missing_numberic_data))

# 7.scale numeric feature except id and SalePrice 
library(dplyr)
noId_sale_price_data <- no_missing_numberic_data %>% select(-Id,-SalePrice)
scale_center_num_data <- scale(noId_sale_price_data,center=TRUE,scale=TRUE)
scale_center_num_data <- cbind(Id = no_missing_numberic_data$Id,scale_center_num_data,SalePrice=no_missing_numberic_data$SalePrice)
summary(scale_center_num_data)

# 8.log transform numberic predictors that are have skewness > 1 and skewness > -1
tmp <- as.data.frame(scale_center_num_data)
library(e1071) 
df <- data.frame(name=character(),skewness=numeric())
for(i in colnames(tmp)){
   df = rbind(df, data.frame(name = i, skewness = skewness(tmp[[i]])))
}
sort_skewness_feature <- df %>% arrange(desc(skewness)) %>% filter(skewness >1 & skewness > -1)
sort_skewness_feature

for(i in sort_skewness_feature$name){
  tmp[[i]] <- log10((abs(tmp[[i]])))
}

dx <- data.frame(name=character(),skewness=numeric())
for(i in sort_skewness_feature$name){
  dx = rbind(dx, data.frame(name = i, skewness = skewness(tmp[[i]])))
}
sort_dx <- dx %>% arrange(desc(skewness))
sort_dx
scale_center_num_data <- tmp


# 9.replace Na for factor data with the mode of that feature
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}
for(i in 1:ncol(factor_only_data)){
  factor_only_data[is.na(factor_only_data[,i]), i] <- getmode(factor_only_data[,i])
}
sum(is.na(factor_only_data))
# let combine the numberic and categorical data togheter
clean_data <- cbind(scale_center_num_data,factor_only_data)
str(clean_data)

# 10.Sub select the features that have predictive power using Boruta package 
library(Boruta)
set.seed(8)
feature.selection <- Boruta(SalePrice ~., data = clean_data[1:train_index,], doTrace = 1)
table(feature.selection$finalDecision)
slelected_feature <- getSelectedAttributes(feature.selection)
selected_feautres_data <- clean_data %>% select(Id,selected_feautres,SalePrice)

colnames(selected_feautres_data)


write.csv(selected_feautres_data,file = "C:\\REGIS\\practium1\\data\\selectedFeatureData.csv",row.names=FALSE)
