## "Exploratory Analysis - Titanic"
## Preprocess
## @ Matheus Pimenta
## github.com/omatheuspimenta/titanic_exploratory
###########################################################

#####
# Set path
setwd("/home/matheus/Dropbox/06_doutorado/2021_01/Bioestatistica/projeto/dataset/")

#####
# Libraries
library("Hmisc") #for describe
library("stringr") #for string manipulation
library("ggplot2") #for graphics
library("plyr") #for count
library("dplyr") #for summary 
library("boot") #for bootstrap
#####
# Load file
load("titanic3.sav")

#####
# Sumary
head(titanic3)
summary(titanic3)
# > summary(titanic3)
# pclass       survived         name               sex           age              sibsp            parch          ticket         
# 1st:323   Min.   :0.000   Length:1309        female:466   Min.   : 0.1667   Min.   :0.0000   Min.   :0.000   Length:1309       
# 2nd:277   1st Qu.:0.000   Class :labelled    male  :843   1st Qu.:21.0000   1st Qu.:0.0000   1st Qu.:0.000   Class :labelled   
# 3rd:709   Median :0.000   Mode  :character                Median :28.0000   Median :0.0000   Median :0.000   Mode  :character  
# Mean   :0.382                                             Mean   :29.8811   Mean   :0.4989   Mean   :0.385                     
# 3rd Qu.:1.000                                             3rd Qu.:39.0000   3rd Qu.:1.0000   3rd Qu.:0.000                     
# Max.   :1.000                                             Max.   :80.0000   Max.   :8.0000   Max.   :9.000                     
# NA's   :263                                                          
#       fare                     cabin             embarked        boat          body        home.dest        
#  Min.   :  0.000                  :1014   Cherbourg  :270          :823   Min.   :  1.0   Length:1309       
#  1st Qu.:  7.896   C23 C25 C27    :   6   Queenstown :123   13     : 39   1st Qu.: 72.0   Class :labelled   
#  Median : 14.454   B57 B59 B63 B66:   5   Southampton:914   C      : 38   Median :155.0   Mode  :character  
#  Mean   : 33.295   G6             :   5   NA's       :  2   15     : 37   Mean   :160.8                     
#  3rd Qu.: 31.275   B96 B98        :   4                     14     : 33   3rd Qu.:256.0                     
#  Max.   :512.329   C22 C26        :   4                     4      : 31   Max.   :328.0                     
#  NA's   :1         (Other)        : 271                     (Other):308   NA's   :1188         

dim(titanic3)
# 1309   14

#####
# Identifying the types of the variables
print("Classes (class):")
print(sapply(titanic3, class))
print("Classes (typeof):")
print(sapply(titanic3, typeof))

# "Classes (class):"
# pclass   survived       name        sex        age      sibsp      parch     ticket       fare      cabin   embarked       boat       body  home.dest 
# "factor" "labelled" "labelled"   "factor" "labelled" "labelled" "labelled" "labelled" "labelled"   "factor"   "factor"   "factor" "labelled" "labelled" 
# "Classes (typeof):"
# pclass    survived        name         sex         age       sibsp       parch      ticket        fare       cabin    embarked        boat        body 
# "integer"   "integer" "character"   "integer"    "double"   "integer"   "integer" "character"    "double"   "integer"   "integer"   "integer"   "integer" 
# home.dest 
# "character" 

#####
# Describe dataset
describe(titanic3)

# titanic3 
# 14  Variables      1309  Observations
# --------------------------------------------------------------------------------------------------------
#   pclass 
#    n  missing distinct 
# 1309        0        3 
# Value        1st   2nd   3rd
# Frequency    323   277   709
# Proportion 0.247 0.212 0.542
# --------------------------------------------------------------------------------------------------------
#   survived : Survived 
#    n  missing distinct     Info      Sum     Mean      Gmd 
# 1309        0        2    0.708      500    0.382   0.4725 
# 
# --------------------------------------------------------------------------------------------------------
#   name : Name 
#    n  missing distinct 
# 1309        0     1307 
# 
# lowest : Abbing, Mr. Anthony             Abbott, Master. Eugene Joseph   Abbott, Mr. Rossmore Edward     Abbott, Mrs. Stanton (Rosa Hunt Abelseth, Miss. Karen Marie    
# highest: Zabour, Miss. Hileni            Zabour, Miss. Thamine           Zakarian, Mr. Mapriededer       Zakarian, Mr. Ortin             Zimmerman, Mr. Leo
# --------------------------------------------------------------------------------------------------------
#   sex 
#    n  missing distinct 
# 1309        0        2 
# 
# Value      female   male
# Frequency     466    843
# Proportion  0.356  0.644
# --------------------------------------------------------------------------------------------------------
#   age : Age [Year] 
#    n  missing distinct     Info     Mean      Gmd      .05      .10      .25      .50      .75 
# 1046      263       98    0.999    29.88    16.06        5       14       21       28       39 
# .90      .95 
#  50       57 
# 
# lowest :  0.1667  0.3333  0.4167  0.6667  0.7500, highest: 70.5000 71.0000 74.0000 76.0000 80.0000
# --------------------------------------------------------------------------------------------------------
#   sibsp : Number of Siblings/Spouses Aboard 
#    n  missing distinct     Info     Mean      Gmd 
# 1309        0        7     0.67   0.4989    0.777 
# 
# lowest : 0 1 2 3 4, highest: 2 3 4 5 8
# 
# Value          0     1     2     3     4     5     8
# Frequency    891   319    42    20    22     6     9
# Proportion 0.681 0.244 0.032 0.015 0.017 0.005 0.007
# --------------------------------------------------------------------------------------------------------
#   parch : Number of Parents/Children Aboard 
#    n  missing distinct     Info     Mean      Gmd 
# 1309        0        8    0.549    0.385   0.6375 
# 
# lowest : 0 1 2 3 4, highest: 3 4 5 6 9
# 
# Value          0     1     2     3     4     5     6     9
# Frequency   1002   170   113     8     6     6     2     2
# Proportion 0.765 0.130 0.086 0.006 0.005 0.005 0.002 0.002
# --------------------------------------------------------------------------------------------------------
#   ticket : Ticket Number 
#    n  missing distinct 
# 1309        0      929 
# 
# lowest : 110152      110413      110465      110469      110489     
# highest: W./C. 6608  W./C. 6609  W.E.P. 5734 W/C 14208   WE/P 5735  
# --------------------------------------------------------------------------------------------------------
#   fare : Passenger Fare [British Pound (\243)] 
#    n  missing distinct     Info     Mean      Gmd      .05      .10      .25      .50      .75 
# 1308        1      281        1     33.3    38.61    7.225    7.567    7.896   14.454   31.275 
#    .90      .95 
# 78.051  133.650 
# 
# lowest :   0.0000   3.1708   4.0125   5.0000   6.2375, highest: 227.5250 247.5208 262.3750 263.0000 512.3292
# --------------------------------------------------------------------------------------------------------
#   cabin 
#    n  missing distinct 
# 1309        0      187 
# 
# lowest :     A10 A11 A14 A16, highest: F33 F38 F4  G6  T  
# --------------------------------------------------------------------------------------------------------
#   embarked 
#    n  missing distinct 
# 1307        2        3 
# 
# Value        Cherbourg  Queenstown Southampton
# Frequency          270         123         914
# Proportion       0.207       0.094       0.699
# --------------------------------------------------------------------------------------------------------
#   boat 
#    n  missing distinct 
# 1309        0       28 
# 
# lowest :     1   10  11  12 , highest: A   B   C   C D D  
# --------------------------------------------------------------------------------------------------------
#   body : Body Identification Number 
#   n  missing distinct     Info     Mean      Gmd      .05      .10      .25      .50      .75 
# 121     1188      121        1    160.8      113       16       35       72      155      256 
# .90      .95 
# 297      307 
# 
# lowest :   1   4   7   9  14, highest: 312 314 322 327 328
# home.dest : Home/Destination 
#   n  missing distinct 
# 745      564      368 
# 
# lowest : ?Havana, Cuba                   Aberdeen / Portland, OR         Albany, NY                      Altdorf, Switzerland            Amenia, ND                     
# highest: Worcester, England              Worcester, MA                   Yoevil, England / Cottage Grove Youngstown, OH                  Zurich, Switzerland            
# --------------------------------------------------------------------------------------------------------
#####
# Now, we will see column by column to find "gold information"
# The columns "pclass" and "survived" do not have any "new" information until here.
# In the column "name" we can extract the "Mr", "Mrs",... of the observation
# and copy to a new column "title"
titanic3$title <- str_split_fixed(titanic3$name, " ",3)[,2]

# Extracting the last name for other analysis
titanic3$lastname <- str_split_fixed(titanic3$name, " ",3)[,1]

# Plot barplot of frequency - title
# temp variable for barplot - title
t<-count(titanic3, 'title')
t<-t[order(t$freq, decreasing = TRUE),]
ylim <- c(0, 1.1*max(t$freq))
xx <- barplot(t$freq[1:20], 
        col = rainbow(20),
        main = "Barplot - Title",
        xlab = "Title",
        ylab = "Frequency",
        ylim = ylim)
text(x = xx, y = t$freq[1:20], label = t$freq[1:20], pos = 3, cex = 0.8, col = "black")
axis(1, at=xx, labels=t$title[1:20], tick=FALSE, las=2, line=-0.5, cex.axis=0.8)
# remove temp variables
remove(t,xx,ylim)
# temp variable for barplot - lastname
t<-count(titanic3, 'lastname')
t<-t[order(t$freq, decreasing = TRUE),]
ylim <- c(0, 1.1*max(t$freq))
xx <- barplot(t$freq[1:20], 
              col = rainbow(20),
              main = "Barplot - Last name",
              xlab = "Last name",
              ylab = "Frequency",
              ylim = ylim)
text(x = xx, y = t$freq[1:20], label = t$freq[1:20], pos = 3, cex = 0.8, col = "black")
axis(1, at=xx, labels=t$lastname[1:20], tick=FALSE, las=2, line=-0.5, cex.axis=0.8)
# remove temp variables
remove(t,xx,ylim)

# The column "sex" does not have any "new" information until here.
# The column "age" have some NA values, in this case, we will replace these values
# in order to obtain an approximation and use this column in our analysis
# Let see some graphs about this column and some basics measures
# range
range(titanic3$age, na.rm = TRUE)
# 0.1667 80.0000
# boxplot
boxplot(titanic3$age~titanic3$pclass,
        main= "Age boxplot",
        xlab= "Age in years",
        horizontal = TRUE,
        col = "green")
# histogram and qqnorm 
age <- titanic3$age
mean_age <- mean(age,
                 na.rm = TRUE)
sd_age <- sd(age,
             na.rm = TRUE)
fteoricoN_age <- dnorm(seq(min(age, na.rm = TRUE),max(age, na.rm = TRUE),by=1),
                       mean=mean_age,
                       sd=sd_age)
hist(age,
     freq=F,
     xlab="Age",
     ylab="Relative Frequency",
     main="Histogram - Age",
     col = "green")
lines(seq(min(age, na.rm = TRUE),max(age, na.rm = TRUE),by=1)
      ,fteoricoN_age,
      col="red")
legend(x=60,y=0.04,legend=c("Normal"),lty=1,col=c("red"),bty="n")
qqnorm(age,
       xlab="Theoretical Quantiles ",
       ylab="Quantile Sample ",
       main="Age")
qqline(as.numeric(age),
       col="red")
# remove temp variables
remove(fteoricoN_age,mean_age,sd_age)
# Shapiro-Wilk test --------- disregard, this test does not have power 
#H0= Normal Dist.
#H1= Not Normal Dist.
shapiro.test(age)
remove(age)
# Input the new values in NA using the bootstrap strategy
f <- function(x,i){
        xx<-x[i]
        return(mean(xx, na.rm=TRUE))
}
R <- 1000 # use 10000 if need
pclass <- c("1st", "2nd", "3rd")
i_1 <- which(is.na(titanic3[titanic3$pclass=="1st",]$age))
i_2 <- 323 + which(is.na(titanic3[titanic3$pclass=="2nd",]$age))
i_3 <- 600 + which(is.na(titanic3[titanic3$pclass=="3rd",]$age))
for (c in pclass){
        if(c=="1st"){
                for(i in i_1){ # because the dataset is ordered
                        input <- titanic3[titanic3$pclass==c,]$age
                        if(is.na(titanic3$age[i])){
                                b <- boot(input, f, R=R)
                                if(mean(b$t) < 1){
                                        output <- mean(b$t)
                                }else{
                                        output <- round(mean(b$t))
                                }
                                titanic3$age[i] <- output
                        }
                }
        }
        if(c=="2nd"){
                for(i in i_2){ # because the dataset is ordered
                        input <- titanic3[titanic3$pclass==c,]$age
                        if(is.na(titanic3$age[i])){
                                b <- boot(input, f, R=R)
                                if(mean(b$t) < 1){
                                        output <- mean(b$t)
                                }else{
                                        output <- round(mean(b$t))
                                }
                                titanic3$age[i] <- output
                        }
                }
        }
        if(c=="3rd"){
                for(i in i_3){ # because the dataset is ordered
                        input <- titanic3[titanic3$pclass==c,]$age
                        if(is.na(titanic3$age[i])){
                                b <- boot(input, f, R=R)
                                if(mean(b$t) < 1){
                                        output <- mean(b$t)
                                }else{
                                        output <- round(mean(b$t))
                                }
                                titanic3$age[i] <- output
                        }
                }
        }
}
# remove temp variables
remove(c,i,input,output,pclass,R,f,b,i_1,i_2,i_3)
# replot the graphs
# boxplot
boxplot(titanic3$age~titanic3$pclass,
        main= "Age boxplot",
        xlab= "Age in years",
        ylab= "Class",
        horizontal = TRUE,
        col = "green")
# histogram and qqnorm 
age <- titanic3$age
mean_age <- mean(age,
                 na.rm = TRUE)
sd_age <- sd(age,
             na.rm = TRUE)
fteoricoN_age <- dnorm(seq(min(age, na.rm = TRUE),max(age, na.rm = TRUE),by=1),
                       mean=mean_age,
                       sd=sd_age)
hist(age,
     freq=F,
     xlab="Age",
     ylab="Relative Frequency",
     main="Histogram - Age",
     col = "green")
lines(seq(min(age, na.rm = TRUE),max(age, na.rm = TRUE),by=1)
      ,fteoricoN_age,
      col="red")
legend(x=60,y=0.04,legend=c("Normal"),lty=1,col=c("red"),bty="n")
qqnorm(age,
       xlab="Theoretical Quantiles ",
       ylab="Quantile Sample ",
       main="Age")
qqline(as.numeric(age),
       col="red")
# remove temp variables
remove(age, fteoricoN_age, mean_age, sd_age)

# The columns "sibsp", "parch" and "ticket" do not have any "new"
# information until here.
# The column "fare" has one NA value, let's see this.
# Finding 'NA' in fare
titanic3[which(is.na(titanic3$fare)),]
# Replacing the value using the bootstrap strategy. How the NA values  
# are in the "3rd" class, we assume the mean by class.
f <- function(x,i){
        xx<-x[i]
        return(mean(xx, na.rm=TRUE))
}
R <- 1000 # use 10000 if need
input <- titanic3[titanic3$pclass=="3rd",]$fare
b <- boot(input, f, R=R)
i <- which(is.na(titanic3$fare))
titanic3$fare[i] <- mean(b$t)
#remove temp variables
remove(f,R,input,b,i)
# boxplot, histgram and qqplot
boxplot(titanic3$fare~titanic3$pclass,
        main= "Fare boxplot",
        xlab= "Money",
        ylab= "Class",
        horizontal = TRUE,
        col = "pink")
fare <- titanic3$fare
mean_fare <- mean(fare)
sd_fare <- sd(fare)
fteoricoN_fare <- dnorm(seq(min(fare),max(fare),by=1),
                       mean=mean_fare,
                       sd=sd_fare)
fteoricoE_fare <- dexp(seq(min(fare),max(fare),by=10),
                       rate=1/mean_fare) 
hist(fare,
     freq=F,
     xlab="Age",
     ylab="Relative Frequency",
     main="Histogram - Fare",
     col = "pink")
lines(seq(min(fare),max(fare),by=1),
      fteoricoN_fare,
      col="red")
lines(seq(min(fare),max(fare),by=10),
      fteoricoE_fare,
      col="blue")
legend(x=400,
       y=0.015,
       legend=c("Normal","Exponential"),
       lty=1,
       col=c("red","blue"),
       bty="n")
qqnorm(fare,
       xlab="Theoretical Quantiles ",
       ylab="Quantile Sample ",
       main="Fare")
qqline(as.numeric(fare),
       col="red")
# remove temp variables
remove(fare, fteoricoN_fare, fteoricoE_fare, mean_fare, sd_fare)
# Mean fare/class
tapply(titanic3$fare,titanic3$pclass, mean)
#     1st      2nd      3rd 
#87.50899 21.17920 13.30289 

# In the column "cabin" we can't impute the NA information yet. 
# One way to do this is to associate the last name with the cabin, 
# but that information isn't available.
# The column "embarked" has 2 NA values. Let's see.
i_n<-which(is.na(titanic3$embarked))
titanic3[which(is.na(titanic3$embarked)),]
# The 2 values belong to the "1st" class.
t<-table(titanic3$embarked,titanic3$pclass)
c<-prop.table(t[,1])
x<-c("Cherbourg", "Queenstown", "Southampton")
# The frequency of "Cherbourg" and "Southamptom" is very close.
# Input new values in NA
for (i in i_n){
  titanic3$embarked[i]<-sample(x,1,prob = c)
}
remove(i_n,t,c,x,i)
# In the columns "boat", "body" and, "home.dest" we will not explore yet. 
# Two new columns will be creating: "mom" and "nfamily". 
# nfamily represents the number of relatives 
# number of siblings + number of parents + 1 (the observation)
titanic3$nfamily <- titanic3$sibsp + titanic3$parch + 1
# mom represents if the woman is mom or not, to create only if the age > 18 and
# parch > 1
titanic3$mom <- ifelse((titanic3$sex=="female" & titanic3$parch>=1 & titanic3$age > 18), 1, 0)
#####
describe(titanic3)

# titanic3 
# 
# 18  Variables      1309  Observations
# -----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# pclass 
#    n  missing distinct 
# 1309        0        3 
# 
# Value        1st   2nd   3rd
# Frequency    323   277   709
# Proportion 0.247 0.212 0.542
# -----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# survived : Survived 
#    n  missing distinct     Info      Sum     Mean      Gmd 
# 1309        0        2    0.708      500    0.382   0.4725 
# 
# -----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# name : Name 
#    n  missing distinct 
# 1309        0     1307 
# 
# lowest : Abbing, Mr. Anthony             Abbott, Master. Eugene Joseph   Abbott, Mr. Rossmore Edward     Abbott, Mrs. Stanton (Rosa Hunt Abelseth, Miss. Karen Marie    
# highest: Zabour, Miss. Hileni            Zabour, Miss. Thamine           Zakarian, Mr. Mapriededer       Zakarian, Mr. Ortin             Zimmerman, Mr. Leo             
# -----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# sex 
#    n  missing distinct 
# 1309        0        2 
# 
# Value      female   male
# Frequency     466    843
# Proportion  0.356  0.644
# -----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# age : Age [Year] 
#    n  missing distinct     Info     Mean      Gmd      .05      .10      .25      .50      .75      .90      .95 
# 1309        0       98    0.993    29.37    14.22      7.4     16.0     22.0     26.0     37.0     48.0     55.0 
# 
# lowest :  0.1667  0.3333  0.4167  0.6667  0.7500, highest: 70.5000 71.0000 74.0000 76.0000 80.0000
# -----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# sibsp : Number of Siblings/Spouses Aboard 
#    n  missing distinct     Info     Mean      Gmd 
# 1309        0        7     0.67   0.4989    0.777 
# 
# lowest : 0 1 2 3 4, highest: 2 3 4 5 8
# 
# Value          0     1     2     3     4     5     8
# Frequency    891   319    42    20    22     6     9
# Proportion 0.681 0.244 0.032 0.015 0.017 0.005 0.007
# -----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# parch : Number of Parents/Children Aboard 
#    n  missing distinct     Info     Mean      Gmd 
# 1309        0        8    0.549    0.385   0.6375 
# 
# lowest : 0 1 2 3 4, highest: 3 4 5 6 9
# 
# Value          0     1     2     3     4     5     6     9
# Frequency   1002   170   113     8     6     6     2     2
# Proportion 0.765 0.130 0.086 0.006 0.005 0.005 0.002 0.002
# -----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# ticket : Ticket Number 
#    n  missing distinct 
# 1309        0      929 
# 
# lowest : 110152      110413      110465      110469      110489     , highest: W./C. 6608  W./C. 6609  W.E.P. 5734 W/C 14208   WE/P 5735  
# -----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# fare : Passenger Fare [British Pound (\243)] 
#    n  missing distinct     Info     Mean      Gmd      .05      .10      .25      .50      .75      .90      .95 
# 1309        0      282        1    33.28    38.59    7.225    7.570    7.896   14.454   31.275   78.020  133.650 
# 
# lowest :   0.0000   3.1708   4.0125   5.0000   6.2375, highest: 227.5250 247.5208 262.3750 263.0000 512.3292
# -----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# cabin 
#    n  missing distinct 
# 1309        0      187 
# 
# lowest :     A10 A11 A14 A16, highest: F33 F38 F4  G6  T  
# -----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# embarked 
#    n  missing distinct 
# 1309        0        3 
# 
# Value        Cherbourg  Queenstown Southampton
# Frequency          270         123         916
# Proportion       0.206       0.094       0.700
# -----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# boat 
#    n  missing distinct 
# 1309        0       28 
# 
# lowest :     1   10  11  12 , highest: A   B   C   C D D  
# -----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# body : Body Identification Number 
#   n  missing distinct     Info     Mean      Gmd      .05      .10      .25      .50      .75      .90      .95 
# 121     1188      121        1    160.8      113       16       35       72      155      256      297      307 
# 
# lowest :   1   4   7   9  14, highest: 312 314 322 327 328
# -----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# home.dest : Home/Destination 
#   n  missing distinct 
# 745      564      368 
# 
# lowest : ?Havana, Cuba                   Aberdeen / Portland, OR         Albany, NY                      Altdorf, Switzerland            Amenia, ND                     
# highest: Worcester, England              Worcester, MA                   Yoevil, England / Cottage Grove Youngstown, OH                  Zurich, Switzerland            
# -----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# title 
#    n  missing distinct 
# 1309        0       34 
# 
# lowest : Billiard, Brito,    Capt.     Carlo,    Col.     , highest: Steen,    the       Velde,    Walle,    y        
# -----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# lastname 
#    n  missing distinct 
# 1309        0      868 
# 
# lowest : Abbing,      Abbott,      Abelseth,    Abelson,     Abrahamsson,, highest: Yousseff,    Yrois,       Zabour,      Zakarian,    Zimmerman,  
# -----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# nfamily : Number of Siblings/Spouses Aboard 
#    n  missing distinct     Info     Mean      Gmd 
# 1309        0        9    0.773    1.884    1.328 
# 
# lowest :  1  2  3  4  5, highest:  5  6  7  8 11
# 
# Value          1     2     3     4     5     6     7     8    11
# Frequency    790   235   159    43    22    25    16     8    11
# Proportion 0.604 0.180 0.121 0.033 0.017 0.019 0.012 0.006 0.008
# -----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# mom 
#    n  missing distinct     Info      Sum     Mean      Gmd 
# 1309        0        2    0.239      114  0.08709   0.1591 
# 
# -----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------                                                                                                                             -----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Creating dummy variables for "pclass", "sex", "sibsp", "parch", "embarked" and, "nfamily"
# using the base R
# pclass dummy
titanic3$pclass1st <- ifelse(titanic3$pclass=="1st",1,0)
titanic3$pclass2nd <- ifelse(titanic3$pclass=="2nd",1,0)
titanic3$pclass3rd <- ifelse(titanic3$pclass=="3rd",1,0)
# sex dummy
titanic3$female <- ifelse(titanic3$sex=="female",1,0)
titanic3$male <- ifelse(titanic3$sex=="male",1,0)
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
# nfamily dummy
titanic3$nfamily1 <- ifelse(titanic3$nfamily == 1,1,0)
titanic3$nfamily2 <- ifelse(titanic3$nfamily == 2,1,0)
titanic3$nfamily3 <- ifelse(titanic3$nfamily == 3,1,0)
titanic3$nfamily4 <- ifelse(titanic3$nfamily == 4,1,0)
titanic3$nfamily5 <- ifelse(titanic3$nfamily == 5,1,0)
titanic3$nfamily6 <- ifelse(titanic3$nfamily == 6,1,0)
titanic3$nfamily7 <- ifelse(titanic3$nfamily == 7,1,0)
titanic3$nfamily8 <- ifelse(titanic3$nfamily == 8,1,0)
titanic3$nfamily11 <- ifelse(titanic3$nfamily == 11,1,0)

# save the file 
save.image("titanic3.RData")