# [Análise Exploratória](github.com/omatheuspimenta/titanic_exploratory)
### Regressão
## Bioestatística - 2021
[Matheus Pimenta](https://github.com/omatheuspimenta)
### Dataset: [titanic3.RData](dataset/titanic3.RData)
###### [detalhes sobre a geração dos dados](preprocess.md)
---
## Load libraries
```r
library("ggplot2")  #for graphics
library("dplyr")    #for summary 
library("plyr")     #for count
library("reshape2") #for melted matrix
library("infotheo") #for information theory
library("caTools")  #for split dataframe
library("ROCR")     #for ROC curve
library("pROC")     #for ROC curve
library("caret")    #for analyze
library("mfx")      #for odds
```

## Preparando o dataset

O primeiro passo é remover as colunas que não iremos utilizar neste momento (mas que podem ser incluídas caso necessário). Optamos por não utilizar as colunas apresentadas abaixo pois este é uma apresentação do método, em ambientes de produção é recomendado a exploração do maior número de variáveis que acrescente informação ao modelo.
```r
titanic3$name <- NULL
titanic3$ticket <- NULL
titanic3$cabin <- NULL
titanic3$boat <- NULL
titanic3$body <- NULL
titanic3$home.dest <- NULL
titanic3$lastname <- NULL
titanic3$title <- NULL
```

## Feature Selection - Teoria da Informação

Utilizamos a estratégia de seleção de _features_ para o moddelo através da informação mútua entre as colunas, isto é, o quanto de informação é "compartilhada" entre as variáveis. Essa etapa é necessária para remover variáveis que não fornecem informação ao modelo, sendo uma etapa de redução de dimensionalidade ao modelo proposto.

Para isso foram comparadas todas as variáveis contra a nossa variável resposta _survived_ e selecionadas as que possuem o maior valor de informação mútua.

**ATENÇÃO: Na prática o ideal é fixar um valor de _threshold_, em nossa apresentação foram selecionadas 5 variáveis como demonstração.**

```r
# discretizing
disc_df <- discretize(titanic3)
# mutual information among the columns
mi <- mutinformation(disc_df)
t<-mi[-2,2]
t<-t[order(t, decreasing = TRUE)]
```

## Criando variáveis _dummy_

Após a seleção das características que irão compor o modelo, as variáveis categóricas serão transformadas em variáveis _dummy_.

```r
# pclass class
titanic3$class <- ifelse(titanic3$pclass=="1st",1,ifelse(titanic3$pclass=="2nd",2,ifelse(titanic3$pclass=="3rd",3,0)))
# sex dummy
titanic3$sex <- ifelse(titanic3$sex=="female",1,0)
# age dummy
titanic3$children <- ifelse(titanic3$age<=11, 1, 0)
titanic3$teenage <- ifelse((titanic3$age>11 & titanic3$age<20), 1, 0)
titanic3$young <- ifelse((titanic3$age>20 & titanic3$age<30), 1, 0)
titanic3$adult <- ifelse((titanic3$age>30 & titanic3$age<60), 1, 0)
titanic3$old <- ifelse(titanic3$age>60, 1, 0)
```
E por fim, as colunas que não serão utilizadas serão removidas, obtendo o dataset preparado para a regressão.

```r
titanic3$pclass <- NULL
titanic3$embarked <- NULL
titanic3$sibsp <- NULL
titanic3$parch <- NULL
titanic3$age <- NULL
```

## Regressão Logística

>A regressão logística é um recurso que nos permite estimar
a probabilidade associada à ocorrência de determinado
evento em face de um conjunto de variáveis explanatórias.

Algumas plicações da [regressão logística](https://en.wikipedia.org/wiki/Logistic_regression):

> Em medicina, permite por exemplo determinar os factores que caracterizam um grupo de indivíduos doentes em relação a indivíduos sãos;  
> No domínio dos seguros, permite encontrar fracções da clientela que sejam sensíveis a determinada política securitária em relação a um dado risco particular;  
> Em instituições financeiras, pode detectar os grupos de risco para a subscrição de um crédito;  
> Em econometria, permite explicar uma variável discreta, como por exemplo as intenções de voto em actos eleitorais.

O dataset será dividido em treinamento e teste, na proporção 80/20, isto é, 80% dos dados serão utilizados para o treinamento do modelo e 20% dos dados serão usados para o testar o modelo.

```r
set.seed(7)
split <- sample.split(titanic3$survived, SplitRatio=0.8)
train_df <- subset(titanic3, split == "TRUE")
test_df <- subset(titanic3, split == "FALSE")
```
O primeiro modelo de regressão logística proposto utiliza todas as variáveis do dataset preparado anteriormente.

```r
lr <- glm(factor(survived)~factor(sex)+
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
```
O sumário deste primeiro modelo gerou um valor de AIC (Akaike Information Criterion) de 992.32, buscamos valores baixos para AIC, por outro lado, a comparação dos resíduos de _deviance_ entre o modelo Null (NULL) contra o modelo de regressão logística proposto apresenta um valor elevado, isso significa que nosso modelo está acrescentando informações de maneira não aleatória. Também é possível verificar quais variáveis são consideradas significativas ao modelo, isto é, que são diferentes de 0 através da análise de cada um de seus valores _p-valor_.

Realizando um teste _ANOVA_ em nosso modelo é possível verificar quais variáveis acrescentam informação, já que através deste teste as variáveis são incluídas de maneira sequencial ao modelo, gerando valores a cada iteração.

```r
anova(lr, test = "Chisq")
```
O resultado do teste _ANOVA_ mostra que a variável _adult_ acrescenta baixa informação ao modelo, podemos tratá-la, isto é, considerar uma outra forma de divisão da coluna que a gerou (_age_). Contudo, antes, faremos um outro teste para a análise de nosso modelo.

O teste _stepwise_ realiza diversa iterações incluíndo e removendo as variáveis ao modelo, de maneira que um novo modelo seja fornecido com um menor valor de AIC. 

```r
step(lr, direction = 'both')
```

Analisando o resultado do _stepwise_ em nosso modelo de regressão logística quando a variável _old_ é removida há um ganho ao modelo, dessa forma consideraremos essa modificação ao modelo de regressão logística.

```r
lr <- glm(factor(survived)~factor(sex)+
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
```

## Predição e Análises

Para avaliarmos as métricas do modelo proposto, uma curva ROC foi plotada e através dos valores de _tchresholds_ consideramos as predições como binária.  
[Figure - roccurve1_logregression.pdf](figures/roccurve1_logregression.pdf)   
[Figure - roccurve2_logregression.pdf](figures/roccurve2_logregression.pdf) 

```r
predictTrain = predict(lr,type="response")
tapply(predictTrain, train_df$survived, mean) # because need the same length
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
```

Utilizndo o valor máximo obtido pelos _thresholds_ na curva ROC, utilizamos o valor de 0.375 para a geração da matriz de confusão e dessa forma avaliarmos as demais métricas do modelo de regressão logística proposto.

```r
# Confusion Matrix with threshold 0.375
test_df$pred <- as.factor(
  ifelse(
    predict(lr,
            newdata = test_df,
            type = "response") > 0.375,
    1,0)
  )
confusionMatrix(test_df$pred, as.factor(test_df$survived))
```

Os seguintes valores foram obtidos para a matriz de confusão e métricas:

|            	| Reference 	|    	|
|:----------:	|:---------:	|:--:	|
| Prediction 	|     0     	|  1 	|
|      0     	|    128    	| 24 	|
|      1     	|     34    	| 76 	|


|  Accuracy                	|  0.7786           	|
|--------------------------	|-------------------	|
| 95% CI                   	|  (0.7234, 0.8274) 	|
|     No Information Rate  	|  0.6183           	|
|     P-Value [Acc > NIR]  	|  2.14e-08         	|
| Kappa                    	|  0.5398           	|
|  Mcnemar's Test P-Value  	|  0.2373           	|
|             Sensitivity  	|  0.7901           	|
|             Specificity  	|  0.7600           	|
|          Pos Pred Value  	|  0.8421           	|
|          Neg Pred Value  	|  0.6909           	|
|              Prevalence  	|  0.6183           	|
|          Detection Rate  	|  0.4885           	|
|    Detection Prevalence  	|  0.5802           	|
|       Balanced Accuracy  	|  0.7751           	|
|        'Positive' Class  	| 0                 	|

Além das métricas acima, o modelo de regressão logística também fornece os valores da [razão de chances](https://www.ncbi.nlm.nih.gov/pmc/articles/PMC2938757/) (_odds ratio_), que significa quais as chances do evento ocorrer para aquela variável.

```r
logitor(factor(survived)~factor(sex)+
          fare+
          factor(mom)+
          class+
          children+
          teenage+
          young+
          adult+
          nfamily,
        data = train_df)
exp(cbind(OR=coef(lr), confint(lr)))
```
|              	|     OR     	|    2.5%   	|    97.5%   	|
|--------------	|:----------:	|:---------:	|:----------:	|
| (Intercept)  	|  1.3714910 	| 0.6094841 	|  3.0327349 	|
| factor(sex)1 	| 11.8581114 	| 8.3171578 	| 17.1188045 	|
| fare         	|  1.0031678 	| 0.9993718 	|  1.0074682 	|
| factor(mom)1 	|  2.4090446 	| 1.2006990 	|  5.0018492 	|
| class        	|  0.4092922 	| 0.3175237 	|  0.5259066 	|
| children     	| 11.0625619 	| 4.4844576 	| 28.2439003 	|
| teenage      	|  2.2945888 	| 1.1026468 	|  4.8493355 	|
| young        	|  1.9764548 	| 1.0687351 	|  3.7541068 	|
| adult        	|  1.6971654 	| 0.9209081 	|  3.2016328 	|
| nfamily      	|  0.6928725 	| 0.5805972 	|  0.8137376 	|

Analisando os valores de _odds ratio (OR)_ pode-se verificar que mulheres (_sex_ = 1) e crianças (_children_) possuem a maior chance de sobreviver. Essa análise converge com a história do Titanic, onde as primeiras pessoas a serem colocadas nos botes salva-vidas eram mulheres e crianças.

Nós podemos refazer as análises considerando, por exemplo, _class_ como um fator e dessa maneira analisar as chances de sobrevivência para da uma das classes.

## Tentando melhorar o modelo

Podemos realizar algumas análises com o objetivo de obter melhores valores nas métricas de avaliação do modelo. As abordagens ocorrem na manipulação do dataset. Algumas ideias que podem ser aplicadas são:  
* Considerar grupos diferentes na criação das variáveis _dummy_;
* Considerar variáveis como fatores e analisar o comportamento de cada um de seus fatores;
* Retirar os valores _outliers_ das colunas;
* Acrescentar novas colunas que possam estar relacionadas com o estudo.

Nesta etapa optamos por considerar duas abordagens para demonstrar a modificação dos valores de avaliação do modelo:

### Considerando o dataset sem _outliers_ superiores para _fare_ e com a coluna _age_

```r
# removing the fare outlier value and no dummy the "age" column
i <- which(titanic3$fare>500)
titanic3 <- titanic3[-i,]
```
O novo modelo de regressão é composto pelas variáveis _sex, fare, mom, class, age_ e _nfamily_.

```r
lr <- glm(factor(survived)~factor(sex)+
            fare+
            factor(mom)+
            class+
            age+
            nfamily,
          family = binomial(link="logit"), 
          data = train_df)
summary(lr)
```

A análise para este novo modelo é análoga à análise realizada anteriormente, e dessa forma um novo modelo de regressão logística é considerado após a remoção de algumas variáveis e execução do _stepwise_.

```r
lr <- glm(survived~factor(sex)+
            class+
            age+
            nfamily,
          family = binomial(link="logit"), 
          data = train_df)
```

O valor de _threshold_ considerado para a classificação binária foi igual ao do modelo anterior (verificar curva ROC).  
[Figure - roccurve1_logregression2.pdf](figures/roccurve1_logregression2.pdf)   
[Figure - roccurve2_logregression2.pdf](figures/roccurve2_logregression2.pdf)  
A matriz de confusão e métricas são apresentadas abaixo:

|            	| Reference 	|    	|
|------------	|-----------	|----	|
| Prediction 	| 0         	| 1  	|
| 0          	| 126       	| 23 	|
| 1          	| 36        	| 76 	|

| Accuracy               	| 0.7739           	|
|------------------------	|------------------	|
| 95% CI                 	| (0.7183, 0.8232) 	|
| No Information Rate    	| 0.6207           	|
| P-Value [Acc > NIR]    	| 8,85E-05         	|
| Kappa                  	| 0.5319           	|
| Mcnemar's Test P-Value 	| 0.1182           	|
| Sensitivity            	| 0.7778           	|
| Specificity            	| 0.7677           	|
| Pos Pred Value         	| 0.8456           	|
| Neg Pred Value         	| 0.6786           	|
| Prevalence             	| 0.6207           	|
| Detection Rate         	| 0.4828           	|
| Detection Prevalence   	| 0.5709           	|
| Balanced Accuracy      	| 0.7727           	|
| 'Positive' Class       	| 0                	|

As análises em relação à _odds ratio_ são análogas às já executadas.

|              	|     OR     	|    2.5%   	|    97.5%   	|
|--------------	|:----------:	|:---------:	|:----------:	|
| (Intercept)  	| 12.6511989 	| 5.5872517 	| 29.3808183 	|
| factor(sex)1 	| 13.6812152 	| 9.7568552 	| 19.4363906 	|
| class        	|  0.3188810 	| 0.2548360 	|  0.3960097 	|
| age          	|  0.9630912 	| 0.9496068 	|  0.9763484 	|
| nfamily      	|  0.8182609 	| 0.7285057 	|  0.9115658 	|

Dessa forma, neste novo modelo de regressão logística, mulheres possuem grande chance de sobreviverem.

### Considerando o dataset sem _outliers_ superiores para _fare_ e com a coluna _age_ em formato _dummy_  

A variável _age_ foi transformada em formato _dummy_ utilizando intervalos diferentes do primeiro modelo considerado.

```r
# age dummy
titanic3$children <- ifelse(titanic3$age<=11, 1, 0)
titanic3$adult <- ifelse((titanic3$age>11 & titanic3$age<50), 1, 0)
titanic3$old <- ifelse(titanic3$age>50, 1, 0)
```

E o novo modelo de regressão logística considerado é o seguinte:

```r
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
```
Após a _ANOVA_ e _stepwise_, o novo modelo de regressão logística é considerado:

```r
lr <- glm(survived~factor(sex)+
            factor(mom)+
            class+
            children+
            old+
            nfamily,
          family = binomial(link="logit"), 
          data = train_df)
```
E após a análise das curvas ROC, o valor de threshold considerado para a geração da matriz de confusão é de 0.328.   
[Figure - roccurve1_logregression3.pdf](figures/roccurve1_logregression3.pdf)   
[Figure - roccurve2_logregression3.pdf](figures/roccurve2_logregression3.pdf)  
A matriz de confusão e métricas são apresentadas abaixo:

|            	| Reference 	|    	|
|------------	|-----------	|----	|
| Prediction 	| 0         	| 1  	|
| 0          	| 120       	| 19 	|
| 1          	| 42        	| 80 	|

| Accuracy               	| 0.7663           	|
|------------------------	|------------------	|
| 95% CI                 	| (0.7102, 0.8163) 	|
| No Information Rate    	| 0.6207           	|
| P-Value [Acc > NIR]    	| 3.81e-07         	|
| Kappa                  	| 0.5251           	|
| Mcnemar's Test P-Value 	| 0.00485          	|
| Sensitivity            	| 0.7407           	|
| Specificity            	| 0.8081           	|
| Pos Pred Value         	| 0.8633           	|
| Neg Pred Value         	| 0.6557           	|
| Prevalence             	| 0.6207           	|
| Detection Rate         	| 0.4598           	|
| Detection Prevalence   	| 0.5326           	|
| Balanced Accuracy      	| 0.7744           	|
| 'Positive' Class       	| 0                	|

E por fim, a análise dos valores de _odds ratio_ obtidos para este modelo é a seguinte:

|              	|     OR     	|    2.5%   	|    97.5%   	|
|--------------	|:----------:	|:---------:	|:----------:	|
| (Intercept)  	|  3.5049869 	| 2.0741533 	|  5.9864514 	|
| factor(sex)1 	| 13.0746503 	| 9.1702111 	| 18.8846491 	|
| factor(mom)1 	|  2.0744851 	| 1.0270434 	|  4.3355849 	|
| class        	|  0.3705255 	| 0.3019903 	|  0.4520168 	|
| children     	|  9.0284153 	| 4.2911833 	| 19.7140224 	|
| old          	|  0.5410132 	| 0.2892423 	|  0.9913836 	|
| nfamily      	|  0.6796310 	| 0.5718627 	|  0.7959590 	|

Novamente, mulheres e crianças possuem uma chance maior de sobrevida quando comparados com os outros grupos.

---

**ADVERTÊNCIA:** Esse é uma apresentação do método aplicado em um dataset modelo. As abordagens são exemplos de aplicação. Há diversas outras formas de considerar as análises, como resíduos, normalidade, conhecimento e tratamento a priori dos dados, entre outras.