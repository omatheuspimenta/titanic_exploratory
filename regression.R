## "Exploratory Analysis - Titanic"
## Regression
## @ Matheus Pimenta
## github.com/omatheuspimenta/titanic_exploratory
###########################################################

#####
# Set path
setwd("/home/matheus/Dropbox/06_doutorado/2021_01/Bioestatistica/projeto/dataset/")

#####
# Libraries
library("ggplot2") #for graphics
library("dplyr") #for summary 
library("plyr") #for count
library("reshape2") #for melted matrix
library("infotheo") #for information theory
library("caTools") # for split dataframe
library("ROCR") # for ROC curve
library("caret") # for analyze
library("mfx") # for odds
#####
# Load file
load("titanic3.RData")

#####
# Feature Selection - Information Theory
# removing some columns
titanic3$name <- NULL
titanic3$ticket <- NULL
titanic3$cabin <- NULL
titanic3$boat <- NULL
titanic3$body <- NULL
titanic3$home.dest <- NULL
titanic3$lastname <- NULL
titanic3$title <- NULL
# discretizing
disc_df <- discretize(titanic3)
# mutual information among the columns
mi <- mutinformation(disc_df)
t<-mi[-2,2]
t<-t[order(t, decreasing = TRUE)]
ylim <- c(0, 1.1*max(t))
xx <- barplot(t, 
              col = rainbow(20),
              main = "Barplot - Mutual Information",
              xlab = "Variable",
              ylab = "Frequency",
              ylim = ylim)
# remove temp variables
remove(ylim, y, xx, mi, disc_df,t)
# after this, we don't use "sibsp", "parch" and "embarked" columns.
# REMEMBER, this is ONLY A EXAMPLE! If you use this, set a threshold before!!!!!
# the column age will be converted to "categorical dummy"
#####
# Creating dummy variables for "pclass", "sex", "sibsp", "parch", "embarked", "age"
# and, "nfamily"
# using the base R
# pclass class
titanic3$class <- ifelse(titanic3$pclass=="1st",1,ifelse(titanic3$pclass=="2nd",2,ifelse(titanic3$pclass=="3rd",3,0)))
# sex dummy
titanic3$sex <- ifelse(titanic3$sex=="female",1,0)
# sibsp dummy (not use)
# titanic3$sibsp0 <- ifelse(titanic3$sibsp==0,1,0)
# titanic3$sibsp1 <- ifelse(titanic3$sibsp==1,1,0)
# titanic3$sibsp2 <- ifelse(titanic3$sibsp==2,1,0)
# titanic3$sibsp3 <- ifelse(titanic3$sibsp==3,1,0)
# titanic3$sibsp4 <- ifelse(titanic3$sibsp==4,1,0)
# titanic3$sibsp5 <- ifelse(titanic3$sibsp==5,1,0)
# titanic3$sibsp8 <- ifelse(titanic3$sibsp==8,1,0)
# parch dummy (not use)
# titanic3$parch0 <- ifelse(titanic3$parch==0,1,0)
# titanic3$parch1 <- ifelse(titanic3$parch==1,1,0)
# titanic3$parch2 <- ifelse(titanic3$parch==2,1,0)
# titanic3$parch3 <- ifelse(titanic3$parch==3,1,0)
# titanic3$parch4 <- ifelse(titanic3$parch==4,1,0)
# titanic3$parch5 <- ifelse(titanic3$parch==5,1,0)
# titanic3$parch6 <- ifelse(titanic3$parch==6,1,0)
# titanic3$parch9 <- ifelse(titanic3$parch==9,1,0)
# embarked dummy (not use)
# titanic3$Cherbourg <- ifelse(titanic3$embarked == "Cherbourg",1,0)
# titanic3$Queenstown <- ifelse(titanic3$embarked == "Queenstown",1,0)
# titanic3$Southampton <- ifelse(titanic3$embarked == "Southampton",1,0)
# age dummy
titanic3$children <- ifelse(titanic3$age<=11, 1, 0)
titanic3$teenage <- ifelse((titanic3$age>11 & titanic3$age<20), 1, 0)
titanic3$young <- ifelse((titanic3$age>20 & titanic3$age<30), 1, 0)
titanic3$adult <- ifelse((titanic3$age>30 & titanic3$age<60), 1, 0)
titanic3$old <- ifelse(titanic3$age>60, 1, 0)
# # nfamily dummy (no dummy)
# titanic3$nfamily1 <- ifelse(titanic3$nfamily == 1,1,0)
# titanic3$nfamily2 <- ifelse(titanic3$nfamily == 2,1,0)
# titanic3$nfamily3 <- ifelse(titanic3$nfamily == 3,1,0)
# titanic3$nfamily4 <- ifelse(titanic3$nfamily == 4,1,0)
# titanic3$nfamily5 <- ifelse(titanic3$nfamily == 5,1,0)
# titanic3$nfamily6 <- ifelse(titanic3$nfamily == 6,1,0)
# titanic3$nfamily7 <- ifelse(titanic3$nfamily == 7,1,0)
# titanic3$nfamily8 <- ifelse(titanic3$nfamily == 8,1,0)
# titanic3$nfamily11 <- ifelse(titanic3$nfamily == 11,1,0)
#####
# Drop columns 
titanic3$pclass <- NULL
titanic3$embarked <- NULL
titanic3$sibsp <- NULL
titanic3$parch <- NULL
titanic3$age <- NULL
#####
# Logistic Regression
# split dataframe
set.seed(7)
split <- sample.split(titanic3$survived, SplitRatio=0.8)
train_df <- subset(titanic3, split == "TRUE")
test_df <- subset(titanic3, split == "FALSE")
# model to train/test
lr <- glm(survived~factor(sex)+
            fare+
            factor(mom)+
            class+
            children+
            teenage+
            young+
            adult+
            old+
            nfamily,
          family = binomial(link="logit"), 
          data = train_df)
summary(lr)
# Call:
#   glm(formula = survived ~ factor(sex) + fare + factor(mom) + class + 
#         children + teenage + young + adult + old + nfamily, family = binomial(link = "logit"), 
#       data = train_df)
# 
# Deviance Residuals: 
#   Min       1Q   Median       3Q      Max  
# -2.5934  -0.6568  -0.4610   0.6643   2.6405  
# 
# Coefficients:
#               Estimate Std. Error z value Pr(>|z|)    
# (Intercept)   0.469556   0.453035   1.036   0.3000    
# factor(sex)1  2.467526   0.184033  13.408  < 2e-16 ***
# fare          0.003214   0.002044   1.572   0.1159    
# factor(mom)1  0.891186   0.363252   2.453   0.0142 *  
# class        -0.901653   0.129169  -6.980 2.94e-12 ***
# children      2.278369   0.493785   4.614 3.95e-06 ***
# teenage       0.700966   0.410426   1.708   0.0877 .  
# young         0.551109   0.358620   1.537   0.1244    
# adult         0.389930   0.362156   1.077   0.2816    
# old          -0.495277   0.652925  -0.759   0.4481    
# nfamily      -0.368301   0.086174  -4.274 1.92e-05 ***
#   ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# (Dispersion parameter for binomial family taken to be 1)
# Null deviance: 1392.63  on 1046  degrees of freedom
# Residual deviance:  970.32  on 1036  degrees of freedom
# AIC: 992.32
# Number of Fisher Scoring iterations: 5
# anova
anova(lr, test = "Chisq")
# Analysis of Deviance Table
# Model: binomial, link: logit
# Response: survived
# Terms added sequentially (first to last)
#              Df Deviance Resid. Df Resid. Dev  Pr(>Chi)    
# NULL                         1046    1392.63              
# factor(sex)  1  287.752      1045    1104.87 < 2.2e-16 ***
# fare         1   35.544      1044    1069.33 2.494e-09 ***
# factor(mom)  1    0.370      1043    1068.96  0.542941    
# class        1   61.470      1042    1007.49 4.495e-15 ***
# children     1    8.570      1041     998.92  0.003418 ** 
# teenage      1    0.365      1040     998.56  0.545506    
# young        1    2.066      1039     996.49  0.150568    
# adult        1    2.967      1038     993.52  0.084978 .  
# old          1    0.473      1037     993.05  0.491666    
# nfamily      1   22.733      1036     970.32 1.862e-06 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# stepwise method
step(lr, direction = 'both')
# Start:  AIC=992.32
# survived ~ factor(sex) + fare + factor(mom) + class + children + 
#   teenage + young + adult + old + nfamily
#               Df Deviance     AIC
# - old          1   970.90  990.90
# - adult        1   971.50  991.50
# <none>             970.32  992.32
# - young        1   972.77  992.77
# - fare         1   973.02  993.02
# - teenage      1   973.30  993.30
# - factor(mom)  1   976.67  996.67
# - nfamily      1   993.05 1013.05
# - children     1   993.22 1013.22
# - class        1  1019.70 1039.70
# - factor(sex)  1  1182.51 1202.51
# Step:  AIC=990.9
# survived ~ factor(sex) + fare + factor(mom) + class + children + 
#   teenage + young + adult + nfamily
# 
#               Df Deviance     AIC
# <none>             970.90  990.90
# - fare         1   973.55  991.55
# - adult        1   973.77  991.77
# + old          1   970.32  992.32
# - young        1   975.64  993.64
# - teenage      1   975.85  993.85
# - factor(mom)  1   977.09  995.09
# - nfamily      1   993.52 1011.52
# - children     1   999.31 1017.31
# - class        1  1019.78 1037.78
# - factor(sex)  1  1184.42 1202.42
# Call:  glm(formula = survived ~ factor(sex) + fare + factor(mom) + class + 
#              children + teenage + young + adult + nfamily, family = binomial(link = "logit"), 
#            data = train_df)
# Coefficients:
# (Intercept)  factor(sex)1          fare  factor(mom)1         class      children       teenage  
#    0.315898      2.473012      0.003163      0.879230     -0.893326      2.403567      0.830554  
#       young         adult       nfamily  
#    0.681305      0.528959     -0.366909  
# Degrees of Freedom: 1046 Total (i.e. Null);  1037 Residual
# Null Deviance:	    1393 
# Residual Deviance: 970.9 	AIC: 990.9
# Well let's remove "old" and use the model
lr <- glm(survived~factor(sex)+
            fare+
            factor(mom)+
            class+
            children+
            teenage+
            young+
            adult+
            nfamily,
          family = binomial(link="logit"), 
          data = train_df)
# predict
predictTrain = predict(lr,type="response")
tapply(predictTrain, train_df$survived, mean)
#         0         1 
# 0.2401660 0.6115315
# ROC Curve
# plot 1
ROCRpred <- prediction(predictTrain,train_df$survived)
ROCRperf <- performance(ROCRpred, "tpr","fpr")
plot(ROCRperf, 
     colorize=TRUE, 
     print.cutoffs.at=seq(0,1,by=0.1), 
     text.adj=c(-0.2,1.7),
     main="ROC Curve")

# plot 2
roc1=plot.roc(train_df$survived,fitted(lr))
plot(roc1,
     print.auc=TRUE, 
     auc.polygon=TRUE, 
     grid.col=c("green","red"), 
     max.auc.polygon=TRUE, 
     auc.polygon.col="lightgreen", 
     print.thres=TRUE,
     main = "ROC Curve")

# Confusion Matrix with threshold 0.375
test_df$pred <- as.factor(
  ifelse(
    predict(lr,
            newdata = test_df,
            type = "response") > 0.375,
    1,0)
  )
confusionMatrix(test_df$pred, as.factor(test_df$survived))
# Confusion Matrix and Statistics
#             Reference
# Prediction   0   1
#          0 128  24
#          1  34  76
# 
#                Accuracy : 0.7786          
#                  95% CI : (0.7234, 0.8274)
#     No Information Rate : 0.6183          
#     P-Value [Acc > NIR] : 2.14e-08        
#                   Kappa : 0.5398          
#  Mcnemar's Test P-Value : 0.2373          
#             Sensitivity : 0.7901          
#             Specificity : 0.7600          
#          Pos Pred Value : 0.8421          
#          Neg Pred Value : 0.6909          
#              Prevalence : 0.6183          
#          Detection Rate : 0.4885          
#    Detection Prevalence : 0.5802          
#       Balanced Accuracy : 0.7751          
#        'Positive' Class : 0     
# odds in model
logitor(survived~factor(sex)+
          fare+
          factor(mom)+
          class+
          children+
          teenage+
          young+
          adult+
          nfamily,
        data = train_df)
# Call:
#   logitor(formula = survived ~ factor(sex) + fare + factor(mom) + 
#             class + children + teenage + young + adult + nfamily, data = train_df)
# 
# Odds Ratio:
#               OddsRatio  Std. Err.       z     P>|z|    
# factor(sex)1 11.8581114  2.1815598 13.4423 < 2.2e-16 ***
# fare          1.0031678  0.0020371  1.5575   0.11935    
# factor(mom)1  2.4090446  0.8746996  2.4215   0.01546 *  
# class         0.4092922  0.0525979 -6.9514 3.616e-12 ***
# children     11.0625619  5.1841949  5.1290 2.913e-07 ***
# teenage       2.2945888  0.8653321  2.2024   0.02764 *  
# young         1.9764548  0.6320890  2.1303   0.03314 *  
# adult         1.6971654  0.5382840  1.6678   0.09536 .  
# nfamily       0.6928725  0.0596033 -4.2652 1.997e-05 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# confident interval (95%)
exp(cbind(OR=coef(lr), confint(lr)))
#                      OR     2.5 %     97.5 %
# (Intercept)   1.3714910 0.6094841  3.0327349
# factor(sex)1 11.8581114 8.3171578 17.1188045
# fare          1.0031678 0.9993718  1.0074682
# factor(mom)1  2.4090446 1.2006990  5.0018492
# class         0.4092922 0.3175237  0.5259066
# children     11.0625619 4.4844576 28.2439003
# teenage       2.2945888 1.1026468  4.8493355
# young         1.9764548 1.0687351  3.7541068
# adult         1.6971654 0.9209081  3.2016328
# nfamily       0.6928725 0.5805972  0.8137376
# remove temp variables
remove(lr, roc1, ROCRperf, ROCRpred, test_df, train_df, predictTrain, split)