source_file <-"C:\\REGIS\\practium1\\data\\train.csv"
orgData <- read.csv(source_file,header = TRUE)

dim(orgData)
str(orgData)
summary(orgData)

library(ggplot2)
ggplot(orgData,aes(SalePrice)) + geom_histogram()
ggplot(orgData,aes(x = 1 ,y = SalePrice)) + geom_boxplot()
ggplot(orgData,aes(x = YearBuilt,y = SalePrice)) + geom_point()
ggplot(orgData,aes(x = YearBuilt,y = SalePrice)) + geom_point()
ggplot(orgData,aes(x = factor(OverallCond)  ,y = SalePrice)) + geom_boxplot()
ggplot(orgData,aes(x = factor(OverallQual  )  ,y = SalePrice)) + geom_boxplot()