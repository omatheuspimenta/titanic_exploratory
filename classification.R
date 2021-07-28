## "Exploratory Analysis - Titanic"
## Classification
## @ Matheus Pimenta
## github.com/omatheuspimenta/titanic_exploratory
###########################################################
#####
# Set path
setwd("/home/matheus/Dropbox/06_doutorado/2021_01/Bioestatistica/projeto/dataset/")
#####
# Libraries
library("class") # for KNN classifier
library("randomForest") # for RandomForest classifier
library("rpart") # for decision tree classifier
library("rpart.plot") # for plot decision tree classifier
library("infotheo") #for information theory
library("caTools") # for split data frame
library("scales") # for rescale
library("e1071") # for SVM classifier
#####
# Load file
load("titanic3.RData")
#####
# removing some columns
titanic3$name <- NULL
titanic3$ticket <- NULL
titanic3$cabin <- NULL
titanic3$boat <- NULL
titanic3$body <- NULL
titanic3$home.dest <- NULL
titanic3$lastname <- NULL
titanic3$title <- NULL
# Feature Selection - Information Theory
# in this part we don't remove any column, but if you want, one idea 
# is to use Information Theory
#####
# discretizing
# disc_df <- discretize(titanic3)
# mutual information among the columns
# mi <- mutinformation(disc_df)
# t<-mi[-2,2]
# t<-t[order(t, decreasing = TRUE)]
# ylim <- c(0, 1.1*max(t))
# xx <- barplot(t, 
#               col = rainbow(20),
#               main = "Barplot - Mutual Information",
#               xlab = "Variable",
#               ylab = "Frequency",
#               ylim = ylim)
# # remove temp variables
# remove(ylim, y, xx, mi, disc_df,t)
# after this, we don't use "sibsp", "parch" and "embarked" columns.
# REMEMBER, this is ONLY A EXAMPLE! If you use this, set a threshold before!!!!!
# the column age will be converted to "categorical dummy"
# Creating dummy variables for "pclass", "sex", "sibsp", "parch", "embarked", "age"
# and, "nfamily"
# using the base R
# pclass class
titanic3$class1 <- ifelse(titanic3$pclass=="1st",1,0)
titanic3$class2 <- ifelse(titanic3$pclass=="2nd",1,0)
titanic3$class3 <- ifelse(titanic3$pclass=="3rd",1,0)
# sex dummy
titanic3$sex <- ifelse(titanic3$sex=="female",1,0)
# sibsp dummy
titanic3$sibsp0 <- ifelse(titanic3$sibsp==0,1,0)
titanic3$sibsp1 <- ifelse(titanic3$sibsp==1,1,0)
titanic3$sibsp2 <- ifelse(titanic3$sibsp==2,1,0)
titanic3$sibsp3 <- ifelse(titanic3$sibsp==3,1,0)
titanic3$sibsp4 <- ifelse(titanic3$sibsp==4,1,0)
titanic3$sibsp5 <- ifelse(titanic3$sibsp==5,1,0)
titanic3$sibsp8 <- ifelse(titanic3$sibsp==8,1,0)
# parch dummy
titanic3$parch0 <- ifelse(titanic3$parch==0,1,0)
titanic3$parch1 <- ifelse(titanic3$parch==1,1,0)
titanic3$parch2 <- ifelse(titanic3$parch==2,1,0)
titanic3$parch3 <- ifelse(titanic3$parch==3,1,0)
titanic3$parch4 <- ifelse(titanic3$parch==4,1,0)
titanic3$parch5 <- ifelse(titanic3$parch==5,1,0)
titanic3$parch6 <- ifelse(titanic3$parch==6,1,0)
titanic3$parch9 <- ifelse(titanic3$parch==9,1,0)
# embarked dummy
titanic3$Cherbourg <- ifelse(titanic3$embarked == "Cherbourg",1,0)
titanic3$Queenstown <- ifelse(titanic3$embarked == "Queenstown",1,0)
titanic3$Southampton <- ifelse(titanic3$embarked == "Southampton",1,0)
# age dummy
titanic3$children <- ifelse(titanic3$age<=11, 1, 0)
titanic3$teenage <- ifelse((titanic3$age>11 & titanic3$age<20), 1, 0)
titanic3$young <- ifelse((titanic3$age>20 & titanic3$age<30), 1, 0)
titanic3$adult <- ifelse((titanic3$age>30 & titanic3$age<60), 1, 0)
titanic3$old <- ifelse(titanic3$age>60, 1, 0)
# # nfamily dummy 
titanic3$nfamily1 <- ifelse(titanic3$nfamily == 1,1,0)
titanic3$nfamily2 <- ifelse(titanic3$nfamily == 2,1,0)
titanic3$nfamily3 <- ifelse(titanic3$nfamily == 3,1,0)
titanic3$nfamily4 <- ifelse(titanic3$nfamily == 4,1,0)
titanic3$nfamily5 <- ifelse(titanic3$nfamily == 5,1,0)
titanic3$nfamily6 <- ifelse(titanic3$nfamily == 6,1,0)
titanic3$nfamily7 <- ifelse(titanic3$nfamily == 7,1,0)
titanic3$nfamily8 <- ifelse(titanic3$nfamily == 8,1,0)
titanic3$nfamily11 <- ifelse(titanic3$nfamily == 11,1,0)
#####
# Drop columns 
titanic3$pclass <- NULL
titanic3$embarked <- NULL
titanic3$sibsp <- NULL
titanic3$parch <- NULL
titanic3$age <- NULL
titanic3$nfamily <- NULL
#####
# Normalize "fare"
titanic3$fare <- rescale(as.numeric(titanic3$fare))
# Factorize
titanic3$survived <- as.factor(titanic3$survived)
#####
# Classification
# split dataframe
set.seed(7)
split <- sample.split(titanic3$survived, SplitRatio=0.8)
train_df <- subset(titanic3, split == "TRUE")
test_df <- subset(titanic3, split == "FALSE")
#####
# KNN - k = 3
# with you need improve the method, change the 'k'
# suggest: use a grid search to do this
knn3 <- knn(train = train_df[,-1],
            test = test_df[,-1],
            cl = train_df[,1],
            k = 3)
cm_knn3 <- table(test_df[,1], knn3)
# confusion matrix
confusionMatrix(cm_knn3)
# Confusion Matrix and Statistics
# knn3
#     0   1
# 0 135  27
# 1  32  68
#                Accuracy : 0.7748
#                  95% CI : (0.7194, 0.8239)
#     No Information Rate : 0.6374
#     P-Value [Acc > NIR] : 1.161e-06
#                   Kappa : 0.5183
#  Mcnemar's Test P-Value : 0.6025
#             Sensitivity : 0.8084
#             Specificity : 0.7158
#          Pos Pred Value : 0.8333
#          Neg Pred Value : 0.6800
#              Prevalence : 0.6374
#          Detection Rate : 0.5153
#    Detection Prevalence : 0.6183
#       Balanced Accuracy : 0.7621
#        'Positive' Class : 0
# if you need to do some cross-validation, please use the caret library
# train_control = trainControl(method = 'repeatedcv',
#                              number = 10,
#                              repeats = 10) # 10 times for each 10fold CrossValidation
# model = train(survived ~., 
#               data = titanic3, 
#               trControl = train_control, 
#               method = 'knn')
#####
# RandomForest
# with you need to improve the method, change the 'ntree'
# suggest: use a grid search to do this
rf100 <- randomForest(x = train_df[-1],
                     y = train_df$survived,
                     ntree = 100)
rf100_pred <- predict(rf100, 
                     newdata = test_df[-1])
rf100_cm <- table(test_df[,1],
                 rf100_pred)
# confusion matrix
confusionMatrix(rf100_cm)
# Confusion Matrix and Statistics
# rf100_pred
#     0   1
# 0 145  17
# 1  33  67
 #               Accuracy : 0.8092
 #                 95% CI : (0.7563, 0.8549)
 #    No Information Rate : 0.6794
 #    P-Value [Acc > NIR] : 1.809e-06
 #                  Kappa : 0.5829
 # Mcnemar's Test P-Value : 0.03389
 #            Sensitivity : 0.8146
 #            Specificity : 0.7976
 #         Pos Pred Value : 0.8951
 #         Neg Pred Value : 0.6700
 #             Prevalence : 0.6794
 #         Detection Rate : 0.5534
 #   Detection Prevalence : 0.6183
 #      Balanced Accuracy : 0.8061
 #       'Positive' Class : 0
# if you need to do some cross-validation, please use the caret library
# train_control = trainControl(method = 'repeatedcv',
#                              number = 10,
#                              repeats = 10) # 10 times for each 10fold CrossValidation
# model = train(survived ~., 
#               data = titanic3, 
#               trControl = train_control, 
#               method = 'rf')
#####
# Decision Tree
dt <- rpart(formula = survived ~ .,
           data = titanic3)
rpart.plot(dt)
dt_pred <- predict(dt, 
                  newdata = test_df[-1],
                  type = "class")
dt_cm <- table(test_df[,1],
              dt_pred)
confusionMatrix(dt_cm)
# Confusion Matrix and Statistics
# dt_pred
#     0   1
# 0 146  16
# 1  26  74
 #               Accuracy : 0.8397
 #                 95% CI : (0.7896, 0.882)
 #    No Information Rate : 0.6565
 #    P-Value [Acc > NIR] : 2.532e-11
 #                  Kappa : 0.6537
 # Mcnemar's Test P-Value : 0.1649
 #            Sensitivity : 0.8488
 #            Specificity : 0.8222
 #         Pos Pred Value : 0.9012
 #         Neg Pred Value : 0.7400
 #             Prevalence : 0.6565
 #         Detection Rate : 0.5573
 #   Detection Prevalence : 0.6183
 #      Balanced Accuracy : 0.8355
 #       'Positive' Class : 0
#####
# SVM
# with you need to improve the method, change the hyper parameters
# suggest: use a grid search to do this
svm.model <- svm(formula = survived ~ .,
                data = train_df,
                type = 'C-classification',
                kernel = 'radial',
                cost = 10.0)
svm.pred <- predict(svm.model,
                   newdata = test_df[-1])
svm_cm <- table(test_df[,1],
               svm.pred)
# confusion matrix
confusionMatrix(svm_cm)
# Confusion Matrix and Statistics
# svm.pred
#     0   1
# 0 142  20
# 1  32  68
 #               Accuracy : 0.8015
 #                 95% CI : (0.748, 0.8481)
 #    No Information Rate : 0.6641
 #    P-Value [Acc > NIR] : 6.34e-07
 #                  Kappa : 0.5696
 # Mcnemar's Test P-Value : 0.1272
 #            Sensitivity : 0.8161
 #            Specificity : 0.7727
 #         Pos Pred Value : 0.8765
 #         Neg Pred Value : 0.6800
 #             Prevalence : 0.6641
 #         Detection Rate : 0.5420
 #   Detection Prevalence : 0.6183
 #      Balanced Accuracy : 0.7944
 #       'Positive' Class : 0
#####
# remove temp variables
remove(cm_knn3, knn3, dt_cm, dt_pred, knn_3, rf100_cm, rf100_pred, split, svm_cm,
       svm.pred, svm.model, test_df, train_df, dt, rf100)
# You can tune the classifiers, plot graphs, heatmaps and, ROC curves.
# We don't do this at this time because this isn't the purpose o this work.
# Thank you so much for your attention