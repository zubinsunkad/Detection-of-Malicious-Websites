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

#DataExplorer::create_report(mal_model_clean)

fit <- rpart(Type~.,data=mal_model_clean, method = 'class')
rpart.plot(fit, extra = 106)


#Model Summary
summary(fit)


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
test$model_prob <- predict(fit, test, type = 'class')

table_mat <- table(test$Type, test$model_prob)
table_mat

accuracy_Test <- sum(diag(table_mat)) / sum(table_mat)
accuracy_Test