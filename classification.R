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
disc_df <- discretize(titanic3)
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
knn3 = knn(train = train_df[,-1],
            test = test_df[,-1],
            cl = train_df[,1],
            k = 3)
cm_knn3 = table(test_df[,1], knn3)
# confusion matrix
confusionMatrix(cm_knn3)
# Confusion Matrix and Statistics
# knn3
#     0   1
# 0 133  29
# 1  36  64
#                Accuracy : 0.7519        
#                  95% CI : (0.695, 0.803)
#     No Information Rate : 0.645         
#     P-Value [Acc > NIR] : 0.0001347     
#                   Kappa : 0.4672        
#  Mcnemar's Test P-Value : 0.4567504     
#             Sensitivity : 0.7870        
#             Specificity : 0.6882        
#          Pos Pred Value : 0.8210        
#          Neg Pred Value : 0.6400        
#              Prevalence : 0.6450        
#          Detection Rate : 0.5076        
#    Detection Prevalence : 0.6183        
#       Balanced Accuracy : 0.7376        
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
rf100 = randomForest(x = train_df[-1],
                     y = train_df$survived,
                     ntree = 100)
rf100_pred = predict(rf100, 
                     newdata = test_df[-1])
rf100_cm = table(test_df[,1],
                 rf100_pred)
# confusion matrix
confusionMatrix(rf100_cm)
# Confusion Matrix and Statistics
# rf100_pred
#     0   1
# 0 138  24
# 1  34  66
#                Accuracy : 0.7786          
#                  95% CI : (0.7234, 0.8274)
#     No Information Rate : 0.6565          
#     P-Value [Acc > NIR] : 1.121e-05       
#                   Kappa : 0.5218          
#  Mcnemar's Test P-Value : 0.2373          
#             Sensitivity : 0.8023          
#             Specificity : 0.7333          
#          Pos Pred Value : 0.8519          
#          Neg Pred Value : 0.6600          
#              Prevalence : 0.6565          
#          Detection Rate : 0.5267          
#    Detection Prevalence : 0.6183          
#       Balanced Accuracy : 0.7678          
#        'Positive' Class : 0 
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
dt = rpart(formula = survived ~ .,
           data = titanic3)
rpart.plot(dt)
dt_pred = predict(dt, 
                  newdata = test_df[-1],
                  type = "class")
dt_cm = table(test_df[,1],
              dt_pred)
confusionMatrix(dt_cm)
# Confusion Matrix and Statistics
# dt_pred
#     0   1
# 0 138  24
# 1  29  71
#                Accuracy : 0.7977          
#                  95% CI : (0.7439, 0.8446)
#     No Information Rate : 0.6374          
#     P-Value [Acc > NIR] : 1.275e-08       
#                   Kappa : 0.5673          
#  Mcnemar's Test P-Value : 0.5827          
#             Sensitivity : 0.8263          
#             Specificity : 0.7474          
#          Pos Pred Value : 0.8519          
#          Neg Pred Value : 0.7100          
#              Prevalence : 0.6374          
#          Detection Rate : 0.5267          
#    Detection Prevalence : 0.6183          
#       Balanced Accuracy : 0.7869          
#        'Positive' Class : 0
#####
# SVM
# with you need to improve the method, change the hyper parameters
# suggest: use a grid search to do this
svm.model = svm(formula = survived ~ .,
                data = train_df,
                type = 'C-classification',
                kernel = 'radial',
                cost = 10.0)
svm.pred = predict(svm.model,
                   newdata = test_df[-1])
svm_cm = table(test_df[,1],
               svm.pred)
# confusion matrix
confusionMatrix(svm_cm)
# Confusion Matrix and Statistics
# svm.pred
#     0   1
# 0 141  21
# 1  34  66
#                Accuracy : 0.7901          
#                  95% CI : (0.7357, 0.8378)
#     No Information Rate : 0.6679          
#     P-Value [Acc > NIR] : 9.038e-06       
#                   Kappa : 0.5439          
#  Mcnemar's Test P-Value : 0.1056          
#             Sensitivity : 0.8057          
#             Specificity : 0.7586          
#          Pos Pred Value : 0.8704          
#          Neg Pred Value : 0.6600          
#              Prevalence : 0.6679          
#          Detection Rate : 0.5382          
#    Detection Prevalence : 0.6183          
#       Balanced Accuracy : 0.7822          
#        'Positive' Class : 0  
#####
# remove temp variables
remove(cm_knn3, knn3, dt_cm, dt_pred, knn_3, rf100_cm, rf100_pred, split, svm_cm,
       svm.pred, svm.model, test_df, train_df, dt, rf100)
# You can tune the classifiers, plot graphs, heatmaps and, ROC curves.
# We don't do this at this time because this isn't the purpose o this work.
# Thank you so much for your attention