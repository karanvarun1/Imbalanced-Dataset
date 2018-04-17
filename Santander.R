library(AUC)
library(caret)
library(pROC)
library(DMwR)
library(mgcv)
setwd("C:/Users/Bansal/Desktop/Classes/Sem1/DataMining/Projects/Santander")
getwd()
df_train<-read.csv("train.csv",header = TRUE)
df_test<-read.csv("test.csv",header = TRUE)
print(table(df_train$TARGET))
set.seed(345)
splitIndex=createDataPartition(df_train$TARGET, p=.7, list = FALSE, times = 1)
trainSplit = df_train[splitIndex,]
testSplit = df_train[-splitIndex,]
prop.table(table(trainSplit$TARGET))
fmla<-as.formula(TARGET~var15+saldo_var30+saldo_var42+var38+num_var30+ind_var30+num_var4+num_var35)

ctrl <- trainControl(method = "cv", number = 5)

lm_model_Imp=train(fmla,data = trainSplit, method = "ranger")
predictSplitImp = predict(lm_model_Imp, testSplit[,names(df_test)], type = "response")

lm_model=train(TARGET~.,data = trainSplit, method = "glmnet", trControl = ctrl,preProcess = c("center","scale","zv"))
predictSplit = predict(lm_model, testSplit[,names(df_test)])
predictSplit<-ifelse(predictSplit>.5,1,0)
predictSplit<-as.factor(predictSplit)
trainSplit$TARGET <- as.factor(trainSplit$TARGET)


confusionMatrix(predictSplit,testSplit$TARGET)
auc<-roc(testSplit$TARGET,predictSplit)
plot(auc1)
auc(auc)


gam_model=train(TARGET~.,data = trainSplit, method = "gam", trControl = ctrl,preProcess = c("scale","zv","pca"))
predictSplitGam = predict(gam_model, testSplit[,names(df_test)])

aucGam<-roc(testSplit$TARGET,predictSplitGam)
plot(aucGam)
auc(aucGam)


trainSplit$TARGET <- as.factor(trainSplit$TARGET)
trainSplitSmote <- SMOTE(TARGET ~ ., trainSplit, perc.over = 200, perc.under=200)
trainSplit$TARGET <- as.numeric(trainSplit$TARGET)
trainSplitSmote$TARGET <- as.numeric(trainSplitSmote$TARGET)

lm_model_Smote=train(TARGET~.,data = trainSplitSmote, method = "glmnet", trControl = ctrl,family = "binomial")
predictSplitSmote = predict(lm_model_Smote, testSplit[,names(df_test)])

aucSM<-roc(testSplit$TARGET,predictSplitSmote)
plot(aucSM)
auc(aucSM)

m<-list(gam = gam_model, glmnet = lm_model_Smote,glmnet = lm_model)
resamples(m)

summary(gam_model)