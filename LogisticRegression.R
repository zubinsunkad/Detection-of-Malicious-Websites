#Load Libraries

library(dplyr)
library(tidyverse)
require(purrr)
require(readr)
library(DataExplorer)
require(scales)
library(InformationValue)
library(rpart)
library(rpart.plot)
library(corrplot)
library(RColorBrewer)

##Read the Data
mal_model <- read_csv("dataset.csv")

#Dimentions of raw data
dim(mal_model)

#Snapshot of data
glimpse(mal_model)

#DataExplorer::create_report(mal_model)

#factors
mal_model$WHOIS_COUNTRY <- as_factor(mal_model$WHOIS_COUNTRY)
mal_model$CHARSET <- as_factor(mal_model$CHARSET)

###row_id
mal_model <- mal_model %>% mutate(id = row_number())

#Create training set
train <- mal_model %>% sample_frac(.70)

#Create test set
test  <- anti_join(mal_model, train, by = 'id')

#Training
mal_model_clean <- train %>%
  
  select(URL_LENGTH,
         
         NUMBER_SPECIAL_CHARACTERS,
         
         CONTENT_LENGTH,
         
         DIST_REMOTE_TCP_PORT,
         
         REMOTE_IPS,
         
         REMOTE_APP_PACKETS,
         
         SOURCE_APP_BYTES,
         
         APP_PACKETS,
         
         Type)

mal_model_clean[is.na(mal_model_clean)] <- 0
DataExplorer::create_report(mal_model_clean)

logitModel <- glm(Type~., family = "binomial" ,
                  
                  data=mal_model_clean, control = list(maxit = 50))

#Model Summary
summary(logitModel)

#testing
mal_model_test <- test %>%
  
  select(URL_LENGTH,
         
         NUMBER_SPECIAL_CHARACTERS,
         
         CONTENT_LENGTH,
         
         DIST_REMOTE_TCP_PORT,
         
         REMOTE_IPS,
         
         REMOTE_APP_PACKETS,
         
         SOURCE_APP_BYTES,
         
         APP_PACKETS)

mal_model_test[is.na(mal_model_test)] <- 0
test$model_prob <- predict(logitModel, mal_model_test, type = "response")

#Outcome
optCutOff <- optimalCutoff(test$Type, test$model_prob)[1]
optCutOff
misClassError(test$Type, test$model_prob, threshold = optCutOff)
Concordance(test$Type, test$model_prob)

# The columns are actuals, while rows are predicteds.
table_mat <-confusionMatrix(as.double(test$Type), as.double(test$model_prob), threshold = optCutOff)
table_mat

acc <- (table_mat$`1`[2]+table_mat$`0`[1])/sum(table_mat$`1` + table_mat$`0`)
acc