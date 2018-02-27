source_file <-"C:\\REGIS\\practium1\\data\\CleanData.csv"
data <- read.csv(source_file,header = TRUE)
str(data)
feature_source_file <-"C:\\REGIS\\practium1\\data\\selectedFeature.csv"

selected_feautres <- read.csv(feature_source_file,header = TRUE)
str(selected_feautres)
selected_feautres <- t(selected_feautres) 
str(selected_feautres)

library(dplyr)

selected_feautres_data <- data %>% select(selected_feautres[,1:50])
target_data <- data$SalePrice

library("vtreat")
library("magrittr")
treatplan <- designTreatmentsZ(selected_feautres_data, selected_feautres[,1:50], verbose = FALSE)
training.treat <- prepare(treatplan, selected_feautres_data, varRestriction = newvars)

scoreFrame <- treatplan$scoreFrame %>% select(varName,origName,code)
newvars <- scoreFrame %>% filter(code %in% c("clean", "lev")) %>% use_series(varName)

selected_feautres_data_XGboost_ready <- prepare(treatplan, selected_feautres_data, varRestriction = newvars)

library(xgboost)
cv <- xgb.cv(data = as.matrix(selected_feautres_data_XGboost_ready), 
             label = target_data,
             nrounds = 300,
             nfold = 5,
             objective = "reg:linear",
             eta = 0.1,
             max_depth = 20,
             early_stopping_rounds = 100,
             verbose = 0   # silent
)
elog <- cv$evaluation_log
result <- elog %>% summarize(ntrees.train = which.min(train_rmse_mean), ntrees.test  = which.min(test_rmse_mean)) 

ntrees <- result$ntrees.test

model_xgb <- xgboost(data = as.matrix(selected_feautres_data_XGboost_ready), # training data as matrix
                          label = target_data,  # column of outcomes
                          nrounds = ntrees,       # number of trees to build
                          objective = "reg:linear", # objective
                          eta = 0.1,
                          depth = 20,
                          verbose = 0  # silent
)

saveRDS(model_xgb, "C:\\REGIS\\practium1\\XGboost_model.rds")
