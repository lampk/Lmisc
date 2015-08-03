## packages need to be import
pkg <- c("RCurl", "car", "lubridate", "mice", "survival", "glmnet", "penalized",
         "ncvreg", "mgcv", "rpart", "randomForest", "gbm", "glmulti", "caret", "ROCR")
sapply(pkg, function(x) devtools::use_package(x))

## build documentation
devtools::document()