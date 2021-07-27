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
library("pROC") # for ROC curve
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
tapply(predictTrain, train_df$survived, mean) # because need the same length
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
#####
# trying to improve accuracy
# removing the fare outlier value and no dummy the "age" column
i <- which(titanic3$fare>500)
titanic3 <- titanic3[-i,]
remove(i)
# New logistic regression
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
            age+
            nfamily,
          family = binomial(link="logit"), 
          data = train_df)
summary(lr)
# Call:
#   glm(formula = survived ~ factor(sex) + fare + factor(mom) + class + 
#         age + nfamily, family = binomial(link = "logit"), data = train_df)
# Deviance Residuals: 
#     Min       1Q   Median       3Q      Max  
# -2.5147  -0.6489  -0.4507   0.6419   2.3916  
# Coefficients:
#                Estimate Std. Error z value Pr(>|z|)    
# (Intercept)   2.6066406  0.4870479   5.352 8.70e-08 ***
# factor(sex)1  2.5482888  0.1838893  13.858  < 2e-16 ***
# fare          0.0009649  0.0024633   0.392 0.695265    
# factor(mom)1  0.3899004  0.3405006   1.145 0.252176    
# class        -1.1239131  0.1359485  -8.267  < 2e-16 ***
# age          -0.0399763  0.0074106  -5.394 6.87e-08 ***
# nfamily      -0.2446959  0.0695098  -3.520 0.000431 ***
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# (Dispersion parameter for binomial family taken to be 1)
# Null deviance: 1386.8  on 1043  degrees of freedom
# Residual deviance:  966.7  on 1037  degrees of freedom
# AIC: 980.7
# Number of Fisher Scoring iterations: 5
# anova
anova(lr, test = "Chisq")
# Analysis of Deviance Table
# Model: binomial, link: logit
# Response: survived
# Terms added sequentially (first to last)
#             Df Deviance Resid. Df Resid. Dev  Pr(>Chi)    
# NULL                         1043    1386.84              
# factor(sex)  1  288.174      1042    1098.67 < 2.2e-16 ***
# fare         1   26.708      1041    1071.96 2.367e-07 ***
# factor(mom)  1    3.493      1040    1068.47 0.0616401 .  
# class        1   64.931      1039    1003.53 7.756e-16 ***
# age          1   22.909      1038     980.63 1.699e-06 ***
# nfamily      1   13.920      1037     966.70 0.0001907 ***
#  ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# stepwise method
step(lr, direction = 'both')
# Start:  AIC=980.7
# survived ~ factor(sex) + fare + factor(mom) + class + age + nfamily
#               Df Deviance     AIC
# - fare         1   966.86  978.86
# - factor(mom)  1   968.05  980.05
# <none>             966.70  980.70
# - nfamily      1   980.63  992.63
# - age          1   998.13 1010.13
# - class        1  1040.16 1052.16
# - factor(sex)  1  1196.10 1208.10
# Step:  AIC=978.86
# survived ~ factor(sex) + factor(mom) + class + age + nfamily
#               Df Deviance     AIC
# - factor(mom)  1   968.19  978.19
# <none>             966.86  978.86
# + fare         1   966.70  980.70
# - nfamily      1   981.55  991.55
# - age          1   998.31 1008.31
# - class        1  1086.22 1096.22
# - factor(sex)  1  1197.90 1207.90
# Step:  AIC=978.19
# survived ~ factor(sex) + class + age + nfamily
#               Df Deviance     AIC
# <none>             968.19  978.19
# + factor(mom)  1   966.86  978.86
# + fare         1   968.05  980.05
# - nfamily      1   982.15  990.15
# - age          1   998.49 1006.49
# - class        1  1086.38 1094.38
# - factor(sex)  1  1243.84 1251.84
# Call:  glm(formula = survived ~ factor(sex) + class + age + nfamily, 
#            family = binomial(link = "logit"), data = train_df)
# Coefficients:
#   (Intercept)  factor(sex)1         class           age       nfamily  
# 2.53775       2.61602      -1.14294      -0.03761      -0.20057  
# Degrees of Freedom: 1043 Total (i.e. Null);  1039 Residual
# Null Deviance:	    1387 
# Residual Deviance: 968.2 	AIC: 978.2
# Now, we remove the "mom" and "fare"
lr <- glm(survived~factor(sex)+
            class+
            age+
            nfamily,
          family = binomial(link="logit"), 
          data = train_df)
# predict
predictTrain = predict(lr,type="response")
tapply(predictTrain, train_df$survived, mean) # because need the same length
#         0         1 
# 0.2397009 0.6093539 
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
#           Reference
# Prediction   0   1
#          0 126  23
#          1  36  76
#                Accuracy : 0.7739          
#                  95% CI : (0.7183, 0.8232)
#     No Information Rate : 0.6207          
#     P-Value [Acc > NIR] : 8.849e-08       
#                   Kappa : 0.5319          
#  Mcnemar's Test P-Value : 0.1182          
#             Sensitivity : 0.7778          
#             Specificity : 0.7677          
#          Pos Pred Value : 0.8456          
#          Neg Pred Value : 0.6786          
#              Prevalence : 0.6207          
#          Detection Rate : 0.4828          
#    Detection Prevalence : 0.5709          
#       Balanced Accuracy : 0.7727          
#        'Positive' Class : 0  
logitor(survived~factor(sex)+
          class+
          age+
          nfamily,
        data = train_df)
# Call:
#   logitor(formula = survived ~ factor(sex) + class + age + nfamily, 
#           data = train_df)
# Odds Ratio:
#                 OddsRatio  Std. Err.        z     P>|z|    
#   factor(sex)1 13.6812152  2.4029737  14.8942 < 2.2e-16 ***
#   class         0.3188810  0.0358203 -10.1747 < 2.2e-16 ***
#   age           0.9630912  0.0068152  -5.3144  1.07e-07 ***
#   nfamily       0.8182609  0.0466694  -3.5167  0.000437 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# confident interval (95%)
exp(cbind(OR=coef(lr), confint(lr)))
#                      OR     2.5 %     97.5 %
# (Intercept)  12.6511989 5.5872517 29.3808183
# factor(sex)1 13.6812152 9.7568552 19.4363906
# class         0.3188810 0.2548360  0.3960097
# age           0.9630912 0.9496068  0.9763484
# nfamily       0.8182609 0.7285057  0.9115658
remove(lr, roc1, ROCRperf, ROCRpred, test_df, train_df, predictTrain, split)
#####
# trying to improve accuracy
# without fare outlier value and with dummy the "age" column as "adult",
#"children" and "old"
# age dummy
titanic3$children <- ifelse(titanic3$age<=11, 1, 0)
titanic3$adult <- ifelse((titanic3$age>11 & titanic3$age<50), 1, 0)
titanic3$old <- ifelse(titanic3$age>50, 1, 0)
# Drop columns 
titanic3$age <- NULL
# New logistic regression
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
            adult+
            old+
            nfamily,
          family = binomial(link="logit"), 
          data = train_df)
summary(lr)
# Call:
#   glm(formula = survived ~ factor(sex) + fare + factor(mom) + class + 
#         children + adult + old + nfamily, family = binomial(link = "logit"), 
#       data = train_df)
# Deviance Residuals: 
#     Min       1Q   Median       3Q      Max  
# -2.7269  -0.6241  -0.4836   0.6621   2.4319  
# Coefficients:
#               Estimate Std. Error z value Pr(>|z|)    
# (Intercept)   0.889031   0.809093   1.099  0.27186    
# factor(sex)1  2.561331   0.184446  13.887  < 2e-16 ***
# fare          0.002769   0.002561   1.081  0.27967    
# factor(mom)1  0.755487   0.368129   2.052  0.04015 *  
# class        -0.914258   0.126374  -7.235 4.67e-13 ***
# children      2.415086   0.849918   2.842  0.00449 ** 
# adult         0.165943   0.757315   0.219  0.82656    
# old          -0.451897   0.802447  -0.563  0.57333    
# nfamily      -0.419376   0.090634  -4.627 3.71e-06 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# (Dispersion parameter for binomial family taken to be 1)
# Null deviance: 1386.84  on 1043  degrees of freedom
# Residual deviance:  955.86  on 1035  degrees of freedom
# AIC: 973.86
# Number of Fisher Scoring iterations: 5
# anova
anova(lr, test = "Chisq")
# Analysis of Deviance Table
# Model: binomial, link: logit
# Response: survived
# Terms added sequentially (first to last)
#             Df Deviance Resid. Df Resid. Dev  Pr(>Chi)    
# NULL                         1043    1386.84              
# factor(sex)  1  288.174      1042    1098.67 < 2.2e-16 ***
# fare         1   26.708      1041    1071.96 2.367e-07 ***
# factor(mom)  1    3.493      1040    1068.47   0.06164 .  
# class        1   64.931      1039    1003.53 7.756e-16 ***
# children     1   16.257      1038     987.28 5.529e-05 ***
# adult        1    3.806      1037     983.47   0.05107 .  
# old          1    0.657      1036     982.81   0.41762    
# nfamily      1   26.949      1035     955.86 2.089e-07 ***
#   ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# stepwise method
step(lr, direction = 'both')
# Start:  AIC=973.86
# survived ~ factor(sex) + fare + factor(mom) + class + children + 
#   adult + old + nfamily
#               Df Deviance     AIC
# - adult        1   955.91  971.91
# - old          1   956.18  972.18
# - fare         1   957.06  973.06
# <none>             955.86  973.86
# - factor(mom)  1   960.27  976.27
# - children     1   964.13  980.13
# - nfamily      1   982.81  998.81
# - class        1  1010.17 1026.17
# - factor(sex)  1  1186.42 1202.42
# Step:  AIC=971.91
# survived ~ factor(sex) + fare + factor(mom) + class + children + 
#   old + nfamily
#               Df Deviance     AIC
# - fare         1   957.08  971.08
# <none>             955.91  971.91
# - old          1   959.83  973.83
# + adult        1   955.86  973.86
# - factor(mom)  1   960.30  974.30
# - nfamily      1   982.82  996.82
# - children     1   992.76 1006.76
# - class        1  1010.18 1024.18
# - factor(sex)  1  1186.65 1200.65
# Step:  AIC=971.08
# survived ~ factor(sex) + factor(mom) + class + children + old + 
#   nfamily
#               Df Deviance     AIC
# <none>             957.08  971.08
# + fare         1   955.91  971.91
# - old          1   961.03  973.03
# + adult        1   957.06  973.06
# - factor(mom)  1   961.22  973.22
# - nfamily      1   983.59  995.59
# - children     1   992.96 1004.96
# - class        1  1059.89 1071.89
# - factor(sex)  1  1191.11 1203.11
# Call:  glm(formula = survived ~ factor(sex) + factor(mom) + class + 
#              children + old + nfamily, family = binomial(link = "logit"), 
#            data = train_df)
# Coefficients:
#   (Intercept)  factor(sex)1  factor(mom)1         class      children           old       nfamily  
# 1.2542        2.5707        0.7297       -0.9928        2.2004       -0.6143       -0.3862  
# 
# Degrees of Freedom: 1043 Total (i.e. Null);  1037 Residual
# Null Deviance:	    1387 
# Residual Deviance: 957.1 	AIC: 971.1
# Now, we remove the "adult" and "fare"
lr <- glm(survived~factor(sex)+
            factor(mom)+
            class+
            children+
            old+
            nfamily,
          family = binomial(link="logit"), 
          data = train_df)
# predict
predictTrain = predict(lr,type="response")
tapply(predictTrain, train_df$survived, mean) # because need the same length
#         0         1 
# 0.2363644 0.6147915 
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
# Confusion Matrix with threshold 0.328
test_df$pred <- as.factor(
  ifelse(
    predict(lr,
            newdata = test_df,
            type = "response") > 0.328,
    1,0)
)
confusionMatrix(test_df$pred, as.factor(test_df$survived))
# Confusion Matrix and Statistics
#           Reference
# Prediction   0   1
#          0 120  19
#          1  42  80
#                Accuracy : 0.7663          
#                  95% CI : (0.7102, 0.8163)
#     No Information Rate : 0.6207          
#     P-Value [Acc > NIR] : 3.81e-07        
#                   Kappa : 0.5251          
#  Mcnemar's Test P-Value : 0.00485         
#             Sensitivity : 0.7407          
#             Specificity : 0.8081          
#          Pos Pred Value : 0.8633          
#          Neg Pred Value : 0.6557          
#              Prevalence : 0.6207          
#          Detection Rate : 0.4598          
#    Detection Prevalence : 0.5326          
#       Balanced Accuracy : 0.7744          
#        'Positive' Class : 0 
logitor(survived~factor(sex)+
          factor(mom)+
          class+
          children+
          old+
          nfamily,
        data = train_df)
# Call:
#   logitor(formula = survived ~ factor(sex) + factor(mom) + class + 
#             children + old + nfamily, data = train_df)
# Odds Ratio:
#              OddsRatio Std. Err.       z     P>|z|    
# factor(sex)1 13.074650  2.407140 13.9629 < 2.2e-16 ***
# factor(mom)1  2.074485  0.760266  1.9911   0.04647 *  
# class         0.370526  0.038085 -9.6591 < 2.2e-16 ***
# children      9.028415  3.503536  5.6702 1.426e-08 ***
# old           0.541013  0.169742 -1.9580   0.05023 .  
# nfamily       0.679631  0.057342 -4.5774 4.707e-06 ***
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# confident interval (95%)
exp(cbind(OR=coef(lr), confint(lr)))
#                      OR     2.5 %     97.5 %
# (Intercept)   3.5049869 2.0741533  5.9864514
# factor(sex)1 13.0746503 9.1702111 18.8846491
# factor(mom)1  2.0744851 1.0270434  4.3355849
# class         0.3705255 0.3019903  0.4520168
# children      9.0284153 4.2911833 19.7140224
# old           0.5410132 0.2892423  0.9913836
# nfamily       0.6796310 0.5718627  0.7959590
remove(lr, roc1, ROCRperf, ROCRpred, test_df, train_df, predictTrain, split)