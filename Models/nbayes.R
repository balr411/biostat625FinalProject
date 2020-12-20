library(caret)
library(naivebayes)
library(e1071)
library(pROC)
library(tidyverse)
library(DMwR)
#create stratified 5-fold validation 
fitControl <- trainControl(
  method = "repeatedcv",
  number = 5,
  ## repeated five times
  repeats = 5,
  summaryFunction = twoClassSummary,
  classProbs = TRUE)

set.seed(1)
nbFit1 <- train(DIQ010 ~ .,
                data = train, 
                method = "naive_bayes",
                metric = "ROC",
                trControl = fitControl)


test_roc <- function(model, data) {
  
  roc(data$DIQ010,
      predict(model, data, type = "prob")[, "diabetes"], levels=c("no_diabetes", "diabetes"))
  
}

nbFit1 %>%
  test_roc(data = test) %>%
  auc()

# Use the same seed to ensure same cross-validation splits
fitControl$seeds <- nbFit1$control$seeds

# Create model weights (they sum to one)
model_weights <- ifelse(train$DIQ010 == "diabetes",
                        (1/table(train$DIQ010)[1]) * 0.5,
                        (1/table(train$DIQ010)[2]) * 0.5)

#build weighted model
weighted_fit <- train(DIQ010 ~ .,
                      data = train,
                      method = "naive_bayes",
                      weights = model_weights,
                      metric = "ROC",
                      trControl = fitControl)

#use down sampling to account for class imbalance
fitControl$sampling <- "down"

down_fit <- train(DIQ010 ~ .,
                  data = train,
                  method = "naive_bayes",
                  metric = "ROC",
                  trControl = fitControl)

#build up-sampled model
fitControl$sampling <- "up" 

up_fit <- train(DIQ010 ~ .,
                data = train,
                method = "naive_bayes",
                metric = "ROC",
                trControl = fitControl)

#build smote model
fitControl$sampling <- "smote"

smote_fit <- train(DIQ010 ~ .,
                   data = train,
                   method = "naive_bayes",
                   metric = "ROC",
                   trControl = fitControl)

# Examine results for test set
model_list <- list(original = nbFit1,
                   weighted = weighted_fit,
                   down = down_fit,
                   up = up_fit,
                   SMOTE = smote_fit)

model_list_roc <- model_list %>%
  map(test_roc, data = test)

model_list_roc %>%
  map(auc)

#original: 0.8311
#weighted: 0.8311
#down-sampled: 0.8316
#up-sampled: 0.8313
#smote:0.8296


#pred <- predict(nbFit1$finalModel,x_test)

#pred <- predict(nbFit1,x_test)

pred <- predict(nbFit1, newdata = test)
confusionMatrix(data = pred, reference = as.factor(test$DIQ010))

#get confusion matrix

pred_down_fit <- predict(down_fit, newdata = test)
confusionMatrix(data = pred, reference = as.factor(test$DIQ010))

pred_up_fit <- predict(up_fit, newdata = test)
confusionMatrix(data = pred, reference = as.factor(test$DIQ010))

pred_smote_fit <- predict(smote_fit, newdata = test)
confusionMatrix(data = pred, reference = as.factor(test$DIQ010))

#These models aren't valid. They predict no_diabetes for all observations except one







