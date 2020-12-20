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
               method = "gbm",
               verbose = FALSE,
               metric = "ROC",
               trControl = ctrl)


#Random forest model
#set seeds the same as original fit
ctrl$seeds <- fit_1$control$seeds

#note that down sampling was most efficient
ctrl$sampling <- "down"

down_fit <- train(DIQ010 ~ .,
                  data = train,
                  method = "rf",
                  metric = "ROC",
                  trControl = ctrl)

#decision tree model
#note that down sampling was most efficient
ctrl$sampling <- "down"

fit_dt <- train(DIQ010 ~ .,
                data = train,
                method = "rpart",
                metric = "ROC",
                trControl = ctrl)

#compute confusion matrices
#random forest
to_predict<-as.factor(test$DIQ010)
pred_rf <- predict(down_fit,test)
cm_rf <- confusionMatrix(pred_rf,to_predict)

#gbm
pred_gbm <- predict(fit_1,test)
cm_gbm <- confusionMatrix(pred_gbm,to_predict)

#decision tree
pred_dt <- predict(fit_dt,test)
cm_dt <- confusionMatrix(pred_dt,to_predict)


#compare model accuracy
model_compare <- data.frame(Model = c('Random Forest',
                                      'Gradient Boosting',
                                      'Decision Tree'),
                            Accuracy = c(cm_rf$overall[1],
                                         cm_gbm$overall[1],
                                         cm_dt$overall[1]))

ggplot(aes(x=Model, y=Accuracy), data=model_compare) +
  geom_bar(stat='identity', fill = 'blue') +
  ggtitle('Comparative Accuracy of Models on Test Data') +
  xlab('Models') +
  ylab('Overall Accuracy')

cm_gbm
cm_rf
cm_dt

#gbm predicts 53% of diabetes outcomes whereas random forest predicts only 28.14% and decision tree predicts 25.6%
#gbm predicted 168 false positives, 877 false negatives compared to 2245, 188 for rf and 2429, 232 for dt
#check all this