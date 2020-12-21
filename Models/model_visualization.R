library("reshape2")
library("doParallel")

train<-read.table("finalTables/train_cat.txt", header=TRUE)
test<-read.table("finalTables/test_cat.txt", header=TRUE)

ctrl <- trainControl(method = "repeatedcv",
                     number = 5,
                     repeats = 5,
                     summaryFunction = twoClassSummary,
                     classProbs = TRUE)

#set seed so cross validation done same each time we fit model 
set.seed(1)

#GBM model
fit_gbm <- train(DIQ010 ~ .,
               data = train,
               method = "gbm",
               verbose = FALSE,
               metric = "ROC",
               trControl = ctrl)


#Random forest model
#set seeds the same as original fit
ctrl$seeds <- fit_gbm$control$seeds

#note that down sampling was most efficient
ctrl$sampling <- "down"

fit_rf <- train(DIQ010 ~ .,
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

#xgboost model
registerDoParallel(4,cores=4)
getDoParWorkers()
fit_xgb <- train(DIQ010 ~ .,
               data = train,
               method = "xgbTree",
               verbose = FALSE,
               metric = "ROC",
               trControl = ctrl)

#compute confusion matrices
#random forest
to_predict<-as.factor(test$DIQ010)
pred_rf <- predict(fit_rf,test)
cm_rf <- confusionMatrix(pred_rf,to_predict)

#gbm
pred_gbm <- predict(fit_gbm,test)
cm_gbm <- confusionMatrix(pred_cm,to_predict)

#decision tree
pred_dt <- predict(fit_dt,test)
cm_dt <- confusionMatrix(pred_dt,to_predict)

#xgboost
pred_xgb <- predict(fit_xgb, test)
cm_xgb <- confusionMatrix(pred_xgb, to_predict)


#compare model accuracy
model_compare <- data.frame(Model = c('Random Forest',
                                      'Gradient Boosting',
                                      'Decision Tree',
                                      'XGBoost'),
                            Accuracy = c(cm_rf$overall[1],
                                         cm_gbm$overall[1],
                                         cm_dt$overall[1],
                                         cm_xgb$overall[1]),
                            Sensitivity=c(cm_rf$byClass[1], 
                                          cm_gbm$byClass[1],
                                          cm_dt$byClass[1], 
                                          cm_xgb$byClass[1]), 
                            Specificity=c(cm_rf$byClass[2], 
                                          cm_gbm$byClass[2],
                                          cm_dt$byClass[2], 
                                          cm_xgb$byClass[2]))

model_compare_2<-data.frame(Model = c('Random Forest',
                                      'Gradient Boosting',
                                      'Decision Tree',
                                      'XGBoost'),
                            Sensitivity=c(cm_rf$byClass[1], 
                                         cm_gbm$byClass[1],
                                         cm_dt$byClass[1], 
                                         cm_xgb$byClass[1]), 
                            Specificity=c(cm_rf$byClass[2], 
                                          cm_gbm$byClass[2],
                                          cm_dt$byClass[2], 
                                          cm_xgb$byClass[2]))

df2 <- melt(model_compare_2, id.vars='Model')

ggplot(aes(x=Model, y=Accuracy), data=model_compare) +
  geom_bar(stat='identity', fill = 'blue') +
  ggtitle('Comparative Accuracy of Models on Test Data') +
  xlab('Models') +
  ylab('Overall Accuracy')

ggplot(aes(x=Model, y=Sensitivity), data=model_compare) +
  geom_bar(stat='identity', fill = 'blue') +
  ggtitle('Comparative Sensitivity of Models on Test Data') +
  xlab('Models') +
  ylab('Sensitivity')

ggplot(df2, aes(x=Model, y=value, fill=variable)) +
  geom_bar(stat='identity', position='dodge') +
  xlab('Models') + 
  ylab('Proportion') +
  guides(fill=guide_legend(title=NULL))+
  ggtitle("Sensitivity and Specificity of Models")

cm_gbm
cm_rf
cm_dt
cm_xgb

require("rlist")
list.save(cm_gbm, file="Models/confusionMatrixGBM.rdata")
list.save(cm_rf, file="Models/confusionMatrixRF.rdata")
list.save(cm_dt, file="Models/confusionMatrixDT.rdata")
list.save(cm_xgb, file="Models/confusionMatrixXGB.rdata")



