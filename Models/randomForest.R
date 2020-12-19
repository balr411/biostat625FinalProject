train<-read.table("finalTables/train_cat.txt", header=TRUE)
test<-read.table("finalTables/test_cat.txt", header=TRUE)

ctrl <- trainControl(method = "repeatedcv",
                     number = 5,
                     repeats = 5,
                     summaryFunction = twoClassSummary,
                     classProbs = TRUE)

#set seed so cross validation done same each time we fit model 
set.seed(1)

fit_1 <- train(DIQ010 ~ .,
               data = train,
               method = "rf",
               metric = "ROC",
               trControl = ctrl)

fit_1 %>%
  test_roc(data = test) %>%
  auc()

#set seeds the same as original fit
ctrl$seeds <- fit_1$control$seeds

# Create model weights (they sum to one)
model_weights <- ifelse(train$DIQ010 == "diabetes",
                        (1/table(train$DIQ010)[1]) * 0.5,
                        (1/table(train$DIQ010)[2]) * 0.5)

#build weighted model
weighted_fit <- train(DIQ010 ~ .,
                      data = train,
                      method = "rf",
                      weights = model_weights,
                      metric = "ROC",
                      trControl = ctrl)

#use down sampling to account for class imbalance

ctrl$sampling <- "down"

down_fit <- train(DIQ010 ~ .,
                  data = train,
                  method = "rf",
                  metric = "ROC",
                  trControl = ctrl)

#build up-sampled model
ctrl$sampling <- "up" 

up_fit <- train(DIQ010 ~ .,
                data = train,
                method = "rf",
                metric = "ROC",
                trControl = ctrl)

#build smote model
ctrl$sampling <- "smote"

smote_fit <- train(DIQ010 ~ .,
                   data = train,
                   method = "rf",
                   metric = "ROC",
                   trControl = ctrl)

# Examine results for test set
model_list <- list(original = fit_1,
                   weighted = weighted_fit,
                   down = down_fit,
                   up = up_fit,
                   SMOTE = smote_fit)

model_list_roc <- model_list %>%
  map(test_roc, data = test)

model_list_roc %>%
  map(auc)

#downsampling works slightly better than all the rest
#original: 0.82
#weighted: 0.824
#down-sampled: 0.8279
#up-sampled: 0.8269
#SMOTE: 0.824