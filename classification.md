# [Análise Exploratória](github.com/omatheuspimenta/titanic_exploratory)
### Classificação
## Bioestatística - 2021
[Matheus Pimenta](https://github.com/omatheuspimenta)
### Dataset: [titanic3.RData](dataset/titanic3.RData)
###### [detalhes sobre a geração dos dados](preprocess.md)
---
## Load libraries
```r
library("class")        #for KNN classifier
library("randomForest") #for RandomForest classifier
library("rpart")        #for decision tree classifier
library("rpart.plot")   #for plot decision tree classifier
library("infotheo")     #for information theory
library("caTools")      #for split data frame
library("scales")       #for rescale
library("e1071")        #for SVM classifier
```

## Preparando o dataset

A preparação do dataset para a execução dos classificadores consistiu na remoção das colunas abaixo:

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
Optamos por não remover nenhuma outra coluna para avaliar o comportamento dos classificadores com as demais colunas, contudo reforçamos que é recomendável a execução da etapa de redução da dimensionalidade.

A variáveis _dummy_ foram criadas para todas as colunas restantes.

```r
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
```

E por fim, foram removidas as colunas que não serão utilizadas.

```r
titanic3$pclass <- NULL
titanic3$embarked <- NULL
titanic3$sibsp <- NULL
titanic3$parch <- NULL
titanic3$age <- NULL
titanic3$nfamily <- NULL
```

O dataset final contem 39 variáveis e 1309 observações. A única coluna com dados contínuos é a coluna _fare_, que foi normalizada através _MinMax_, dessa forma todos os valores estão contidos no intervalo [0,1].

O dataset foi dividido em subconjunto de treinamento e teste, na proporção 80/20, isto é, 80% dos dados foram utilizados para treinamento e 20% dos dados foram utilizados para teste dos classificadores.

**ADVERTÊNCIA:** Não foi realizado nenhum tipo de ajuste fino nos hiper parâmetros dos classificadores, como também não foi realizado validação cruzada. A apresentação das métricas obtidas é um referêncial. Análises dos hiper parâmetros e validação cruzada são necessárias para a confirmação dos resultados.

## KNN

O classificador k-nearest neighbors foi proposto inicialmente na década de [60](https://apps.dtic.mil/dtic/tr/fulltext/u2/a800276.pdf), e expandido na década de [90](https://www.tandfonline.com/doi/abs/10.1080/00031305.1992.10475879). O algoritmo realiza a comparação da distância da i-ésima amostra com _k_ vizinhos mais próximos e através de votação classifica a amostra como pertencente a classe de maior votos.

```r
knn3 <-> knn(train = train_df[,-1],
            test = test_df[,-1],
            cl = train_df[,1],
            k = 3)
```
A matriz de confusão e métricas utilizando o KNN são apresentadas abaixo:

|   	| 0   	| 1  	|
|---	|-----	|----	|
| 0 	| 135 	| 27 	|
| 1 	| 32  	| 68 	|

| Accuracy               	| 0.7748           	|
|------------------------	|------------------	|
| 95% CI                 	| (0.7194, 0.8239) 	|
| No Information Rate    	| 0.6374           	|
| P-Value [Acc > NIR]    	| 1,16E-03         	|
| Kappa                  	| 0.5183           	|
| Mcnemar's Test P-Value 	| 0.6025           	|
| Sensitivity            	| 0.8084           	|
| Specificity            	| 0.7158           	|
| Pos Pred Value         	| 0.8333           	|
| Neg Pred Value         	| 0.6800           	|
| Prevalence             	| 0.6374           	|
| Detection Rate         	| 0.5153           	|
| Detection Prevalence   	| 0.6183           	|
| Balanced Accuracy      	| 0.7621           	|
| 'Positive' Class       	| 0                	|


## Random Forest

O algoritmo Random Forest é um método _ensemble_ que combina a execução de diversas árvores de decisão de maneira que relacione os atributos (características) dos elementos (observações) com seus respectivos rótulos (classes) com uma maior assertividade, diminuindo o efeito de _overfitting_ do método árvore de decisão.

```r
rf100 <- randomForest(x = train_df[-1],
                     y = train_df$survived,
                     ntree = 100)
```

A matriz de confusão e métricas utilizando o KNN são apresentadas abaixo:

|   	| 0   	| 1  	|
|---	|-----	|----	|
| 0 	| 145 	| 17 	|
| 1 	| 33  	| 67 	|

| Accuracy               	| 0.8092           	|
|------------------------	|------------------	|
| 95% CI                 	| (0.7563, 0.8549) 	|
| No Information Rate    	| 0.6794           	|
| P-Value [Acc > NIR]    	| 1,81E-03         	|
| Kappa                  	| 0.5829           	|
| Mcnemar's Test P-Value 	| 0.03389          	|
| Sensitivity            	| 0.8146           	|
| Specificity            	| 0.7976           	|
| Pos Pred Value         	| 0.8951           	|
| Neg Pred Value         	| 0.6700           	|
| Prevalence             	| 0.6794           	|
| Detection Rate         	| 0.5534           	|
| Detection Prevalence   	| 0.6183           	|
| Balanced Accuracy      	| 0.8061           	|
| 'Positive' Class       	| 0                	|


## Árvore de decisão

O classificador [árvore de decisão](https://link.springer.com/article/10.1007/BF00116251) busca relacionar os atributos (características) dos elementos (observações) com seus respectivos rótulos (classes) utilizando uma estrutura de grafos em topologia de árvore. O objetivo é identificar quais as melhores características para realizar a "divisão" dos dados nas classes corretas.

Para isso são considerados algumas métricas que buscam mensurar quão nítida pode ser essa divisão, como por exemplo a métrica de Gini ou ainda o quanto de informação é recebido com as divisões, através do cálculo de entropia.

É um método "simples", não exigindo conhecimentos prévios a respeito dos dados, contudo altamente propenso a _overfitting_ dos dados de treinamento. Alternativas a este comportamento são propostas nos classificadores seguintes.

```r
dt <- rpart(formula = survived ~ .,
           data = titanic3)
```
A matriz de confusão e métricas utilizando o KNN são apresentadas abaixo:

|   	| 0   	| 1  	|
|---	|-----	|----	|
| 0 	| 146 	| 16 	|
| 1 	| 26  	| 74 	|

| Accuracy               	| 0.8397          	|
|------------------------	|-----------------	|
| 95% CI                 	| (0.7896, 0.882) 	|
| No Information Rate    	| 0.6565          	|
| P-Value [Acc > NIR]    	| 2,53E-08        	|
| Kappa                  	| 0.6537          	|
| Mcnemar's Test P-Value 	| 0.1649          	|
| Sensitivity            	| 0.8488          	|
| Specificity            	| 0.8222          	|
| Pos Pred Value         	| 0.9012          	|
| Neg Pred Value         	| 0.7400          	|
| Prevalence             	| 0.6565          	|
| Detection Rate         	| 0.5573          	|
| Detection Prevalence   	| 0.6183          	|
| Balanced Accuracy      	| 0.8355          	|
| 'Positive' Class       	| 0               	|

## Support Vector Machine (SVM)

O objetivo de uma máquina de vetor suporte (do inglês [Support Vector Machine](https://link.springer.com/article/10.1023/B:STCO.0000035301.49549.88) (SVM)) é definir um hiperplano em um espaço n-dimensional de maneira que divida os pontos em classes.

```r
svm.model <- svm(formula = survived ~ .,
                data = train_df,
                type = 'C-classification',
                kernel = 'radial',
                cost = 10.0)
```

A matriz de confusão e métricas utilizando o KNN são apresentadas abaixo:

|   	| 0   	| 1  	|
|---	|-----	|----	|
| 0 	| 142 	| 20 	|
| 1 	| 32  	| 68 	|

| Accuracy               	| 0.8015          	|
|------------------------	|-----------------	|
| 95% CI                 	| (0.748, 0.8481) 	|
| No Information Rate    	| 0.6641          	|
| P-Value [Acc > NIR]    	| 6.34e-07        	|
| Kappa                  	| 0.5696          	|
| Mcnemar's Test P-Value 	| 0.1272          	|
| Sensitivity            	| 0.8161          	|
| Specificity            	| 0.7727          	|
| Pos Pred Value         	| 0.8765          	|
| Neg Pred Value         	| 0.6800          	|
| Prevalence             	| 0.6641          	|
| Detection Rate         	| 0.5420          	|
| Detection Prevalence   	| 0.6183          	|
| Balanced Accuracy      	| 0.7944          	|
| 'Positive' Class       	| 0               	|

---
## Resumo das acurácias

|Método|Acurácia|
|------|--------|
|KNN|77.48%|
|RF |80.92%|
|DT |83.97%|
|SVM|80.15%|