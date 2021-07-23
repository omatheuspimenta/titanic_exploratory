# [Análise Exploratória](github.com/omatheuspimenta/titanic_exploratory)
### Análises e Testes de Hipótese
## Bioestatística - 2021
[Matheus Pimenta](https://github.com/omatheuspimenta)
### Dataset: [titanic3.RData](dataset/titanic3.RData)
###### [detalhes sobre a geração dos dados](preprocess.md)
---
## Extraindo algumas informações

## Criação das variáveis _dummy_

Para as colunas acima indicadas foram criadas variáveis do tipo _dummy_ utilizando a library _base_ do **R**.
```r
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
# age dummy
titanic3$children <- ifelse(titanic3$age<=11, 1, 0)
titanic3$teenage <- ifelse((titanic3$age>11 & titanic3$age<20), 1, 0)
titanic3$young <- ifelse((titanic3$age>20 & titanic3$age<30), 1, 0)
titanic3$adult <- ifelse((titanic3$age>30 & titanic3$age<60), 1, 0)
titanic3$old <- ifelse(titanic3$age>60, 1, 0)
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
```