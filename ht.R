## "Exploratory Analysis - Titanic"
## Analysis
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
#####
# Load file
load("titanic3.RData")
#####
# Extracting some information about the data
# comparing by classes
# survived by class
tapply(titanic3$survived, titanic3$pclass, sum)
# 1st 2nd 3rd 
# 200 119 181 
# sex by class
tapply(titanic3$sex, titanic3$pclass, table)
#`1st`            `2nd`             `3rd`
# female   male    female   male     female   male 
#    144    179       106    171        216    493 
# age by class (mean)
tapply(titanic3$age, titanic3$pclass, mean)
# 1st      2nd      3rd 
# 39.14061 29.51715 24.87024 
# fare by class (mean)
tapply(titanic3$fare, titanic3$pclass, mean)
#      1st      2nd      3rd 
# 87.50899 21.17920 13.30292
# embarked by class
tapply(titanic3$embarked, titanic3$pclass, table)
# `1st`
# Cherbourg  Queenstown Southampton 
#       142           3         178 
# `2nd`
# Cherbourg  Queenstown Southampton 
#        28           7         242 
#`3rd`
# Cherbourg  Queenstown Southampton 
#       101         113         495 
# embarked by survived
tapply(titanic3$embarked, titanic3$survived, table)
#`0`
# Cherbourg  Queenstown Southampton 
#       120          79         610 
#`1`
# Cherbourg  Queenstown Southampton 
#       151          44         305
# mom by class
tapply(titanic3$mom, titanic3$pclass, sum)
# 1st 2nd 3rd 
#  40  28  46 
# mom survived by class
tapply(titanic3[titanic3$pclass=="1st",]$mom, titanic3[titanic3$pclass=="1st",]$survived, sum)
# 0  1 
# 1 39
# finding the mom's name
titanic3[titanic3$pclass=="1st" & titanic3$survived==0 & titanic3$mom==1 ,]
# Allison, Mrs. Hudson J C (Bessi was two siblings on board.
# She is the only 1st class mom that died.
tapply(titanic3[titanic3$pclass=="2nd",]$mom, titanic3[titanic3$pclass=="2nd",]$survived, sum)
# 0  1 
# 1 27 
# finding mom's name
titanic3[titanic3$pclass=="2nd" & titanic3$survived==0 & titanic3$mom==1 ,]
# Lahtinen, Mrs. William (Anna Sy was one sibling on board.
# Lahtinen and her husband have died. 
tapply(titanic3[titanic3$pclass=="3rd",]$mom, titanic3[titanic3$pclass=="3rd",]$survived, sum)
# 0  1 
# 30 16 
# families with high number of survived
t<-count(titanic3[titanic3$survived==1,], 'lastname')
t<-t[order(t$freq, decreasing = TRUE),]
ylim <- c(0, 1.1*max(t$freq))
xx <- barplot(t$freq[1:10], 
              col = rainbow(20),
              main = "Barplot - Families/Survived",
              xlab = "Families",
              ylab = "Frequency",
              ylim = ylim)
text(x = xx, y = t$freq[1:10], label = t$freq[1:10], pos = 3, cex = 0.8, col = "black")
axis(1, at=xx, labels=t$lastname[1:10], tick=FALSE, las=2, line=-0.5, cex.axis=0.8)
# remove temp variables
remove(t,xx,ylim)
# select subset from dataframe
# pclass numeric
titanic3$class <- ifelse(titanic3$pclass=="1st",1,ifelse(titanic3$pclass=="2nd",2,ifelse(titanic3$pclass=="3rd",3,0)))
df_cor <- select(titanic3, class, survived, age, fare, nfamily, mom)
# correlation matrix
cormat <- round(cor(df_cor),2)
cormat_melted <- melt(cormat)
# Create a ggheatmap
ggheatmap <- ggplot(cormat_melted, aes(Var2, Var1, fill = value))+
  geom_tile(color = "white")+
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Pearson\nCorrelation")  +
  theme_minimal()+ # minimal theme
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   size = 12, hjust = 1))+
  coord_fixed()
print(ggheatmap)
# graph
ggheatmap + 
  geom_text(aes(Var2, Var1, label = value), color = "black", size = 4)
#remove temp variables
remove(ggheatmap, cormat, cormat_melted)
# correlation tests (if necessary)
cor.test(df_cor$class,df_cor$fare, method = "pearson")
#H0: no correlation
#H1: correlation
# Pearson's product-moment correlation
# data:  df_cor$class and df_cor$fare
# t = -24.353, df = 1307, p-value < 2.2e-16
# alternative hypothesis: true correlation is not equal to 0
# 95 percent confidence interval:
#  -0.5948682 -0.5202611
# sample estimates:
#        cor 
# -0.5586939 
# remove temp variables
remove(df_cor)
# scatterplot
# age/fare/class
p<-ggplot(titanic3, aes(x=age, y=fare, color=pclass)) + 
    geom_point(size=3)
p + labs(title = "Scatterplot - Age/Fare",
         x="Age (years)",
         y="Fare") +
  labs(colour = "Class")
# remove temp variables
remove(p)
#####
# HT - Wilcoxon Test
#H0: \mu1 = \mu2
#H1: \mu1 \neq \mu2
# split dataframe
class1 <- titanic3[titanic3$pclass == "1st",]
class2 <- titanic3[titanic3$pclass == "2nd",]
class3 <- titanic3[titanic3$pclass == "3rd",]
# wilcox.test -> age
wt_age12 <- wilcox.test(class1$age, class2$age)
# Wilcoxon rank sum test with continuity correction
# data:  class1$age and class2$age
# W = 62581, p-value < 2.2e-16
# alternative hypothesis: true location shift is not equal to 0
# Thus, \mu1 \neq \mu2, p-value < 0.05 (alpha)
wt_age23 <- wilcox.test(class2$age, class3$age)
# Wilcoxon rank sum test with continuity correction
# data:  class2$age and class3$age
# W = 122412, p-value = 1.291e-09
# alternative hypothesis: true location shift is not equal to 0
# Thus, \mu1 \neq \mu2, p-value < 0.05 (alpha)
wt_age31 <- wilcox.test(class3$age, class1$age)
# Wilcoxon rank sum test with continuity correction
# data:  class3$age and class1$age
# W = 45690, p-value < 2.2e-16
# alternative hypothesis: true location shift is not equal to 0
# Thus, \mu1 \neq \mu2, p-value < 0.05 (alpha)
# We have evidence that all positions measures for the age
# in the populations are different based  in the wilcox.test
remove(wt_age12, wt_age23, wt_age31)
# wilcox.test -> fare
wt_fare12 <- wilcox.test(class1$fare, class2$fare)
# Wilcoxon rank sum test with continuity correction
# data:  class1$fare and class2$fare
# W = 81640, p-value < 2.2e-16
# alternative hypothesis: true location shift is not equal to 0
# Thus, \mu1 \neq \mu2, p-value < 0.05 (alpha)
wt_fare23 <- wilcox.test(class2$fare, class3$fare)
# Wilcoxon rank sum test with continuity correction
# data:  class2$fare and class3$fare
# W = 153924, p-value < 2.2e-16
# alternative hypothesis: true location shift is not equal to 0
# Thus, \mu1 \neq \mu2, p-value < 0.05 (alpha)
wt_fare31 <- wilcox.test(class3$fare, class1$fare)
# Wilcoxon rank sum test with continuity correction
# data:  class3$fare and class1$fare
# W = 12184, p-value < 2.2e-16
# alternative hypothesis: true location shift is not equal to 0
# Thus, \mu1 \neq \mu2, p-value < 0.05 (alpha)
# We have evidence that all positions measures for the fare
# in the populations are different based  in the wilcox.test
remove(wt_fare12, wt_fare23, wt_fare31)
remove(class1, class2, class3)