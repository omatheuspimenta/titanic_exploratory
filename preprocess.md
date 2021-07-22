# [Análise Exploratória](github.com/omatheuspimenta/titanic_exploratory)
### Pré-processamento
## Bioestatística - 2021
[Matheus Pimenta](omatheuspimenta.github.io)
### Dataset: [titanic3.sav](dataset/titanic3.sav)
###### [detalhes sobre a geração dos dados](https://hbiostat.org/data/repo/titanic.html)
---
## Load libraries
```r
library("Hmisc")     #for describe
library("stringr")   #for string manipulation
library("ggplot2")   #for graphics
library("plyr")      #for count
library("dplyr")     #for summary 
library("boot")      #for bootstrap
```
## Load file
```r
load("titanic3.sav")
```
---
## Summary
```r
head(titanic3)
summary(titanic3)
dim(titanic3)
```
Dataset contém 1309 observações e 14 colunas.  
As colunas do dataset [titanic3](https://hbiostat.org/data/repo/titanic.html) representam:  
* **pclass**: Tipo de classe que o passageiro estava. 1 é a primeira classe e 3 é a pior classe.  
* **survived**: Informa se o passageiro sobreviveu (1) ou não (0).  
* **name**: Nome do passageiro.  
* **sex**: Sexo do passageiro, masculino ou feminino.  
* **age**: Idade do passageiro.  
* **sibsp**: Número de irmãos / cônjuges a bordo.  
* **parch**: Número de pais / filhos a bordo.  
* **ticket**: Código da passagem.  
* **fare**: Valor da passagem.  
* **cabin**: Identificação da cabine.  
* **embarked**: Local de embarque.  
* **boat**: Bote.  
* **body**: Identificação do Corpo.  
* **home.dest**: Destino ou Partida.

## Identifying the types of the variables
```r
print("Classes (class):")
print(sapply(titanic3, class))
print("Classes (typeof):")
print(sapply(titanic3, typeof))
```
## Describe dataset

```r
describe(titanic3)
```
Após analisar a descrição das variáveis do dataset é possível verificar que:  
* **pclass** (inteiro/fator): Possui três valores que estão relacionados a classe de cada passageiro, sendo 1 classe mais rica e 3 classe mais pobre. A distribuição é seguinte: 323 passageiros na primeira classe, 277 na segunda classe e 709 passageiros viajaram de terceira classe.
* **survived** (binária): Informa se o passageiro sobreviveu (1) ou não (0). Ao todo, neste dataset, 500 passageiros sobreviveram.
* **name** (string): Nome do passageiro, sendo 1307 nomes distintos.  
* **sex** (string/fator): Sexo do passageiro, masculino ou feminino. São 466 pessoas do sexo feminino e 843 pessoas do sexo masculino.  
* **age** (double): Idade do passageiro. Temos 263 dados faltantes que iremos tratar ao decorrer dessa análise, os outros dados refere-se a 1046 observações com um valor mínimo de 0.1667 e um valor máximo de 80 anos.  
* **sibsp** (inteiro): Número de irmãos / cônjuges a bordo. O valor máximo para essa variável é de 8 familiares, sendo distribuídos da seguinte maneira:  

|Valor|          0|    1|     2|     3|     4|     5|     8|  
|:---|:---------:|:---:|:----:|:----:|:----:|:----:|:----:|    
|Frequência|    891|   319|    42|    20|    22|     6|     9|

* **parch** (inteiro): Número de pais / filhos a bordo. A seguinte distribuição é apresentada:  

|Valor|          0|     1|     2|     3|     4|     5|     6|     9|
|:---|:---------:|:---:|:----:|:----:|:----:|:----:|:----:|:----| 
|Frequência|   1002|   170|   113|     8|     6|     6|     2|     2|

* **ticket** (string): Código da passagem, ao todo são 929 passagens diferentes. 
* **fare** (double): Valor da passagem, há uma observação faltante. O valor da passagem necessita de algumas análises particulares devido a existência de outliers, já que a média dos valores foi de 33.3 e a mediana é de apenas 14.4, ao observar os extremos dos valores é possível verificar a existência de algumas observações nulas, isto é, não ocorreu o pagamento de nenhum valor pela passagem e no outro extremo há a existência de um valor de 512.32. Ao decorrer da análise serão detalhados esses valores e se necessário, corrigidos.
* **cabin** (fator): Identificação da cabine, totalizando 187 cabines distintas. 
* **embarked** (fator): Local de embarque, há dois valores faltantes nesta coluna, o restante é distribuído da seguinte maneira:

|Local|        Cherbourg|  Queenstown| Southampton|
|:----|:--------------:|:----------:|:---------|
|Frequência|          270|         123|         914|
* **boat** (fator): São apresentados 28 identificações de botes distintas. 
* **body** (inteiro): Identificação do Corpo. Esta variável apresenta 1188 observações ausentes.
* **home.dest** (string): Destino ou Partida, há 564 observações faltantes nesta coluna.
---
## Analisando cada coluna em busca de informações

### Coluna _pclass_
A coluna _pclass_ não possui nenhuma observação faltante e seus dados são categoricos. Dessa forma esta apta ao uso sem maiores manipulações.

**Dica:** Transformar em variável _dummy_ ao final da análise exploratória.

### Coluna _survived_

A coluna _survived_ não possui observação faltante. É uma coluna binária, podendo ser utilizada como observação-resposta em métodos de regressão.

### Coluna _name_

A coluna _name_ refere-se ao nome do passageiro e também inclui um nome de tratamento a cada observação, esta informação não está explícita no dataset. 
Para isso iremos criar uma nova coluna e analisar esse título também.

```r
# Using the stringr library
titanic3$title <- str_split_fixed(titanic3$name, " ",3)[,2]
```

Também será criada uma coluna com o sobrenome de cada um dos passageiros.
```r
# Using the stringr library
titanic3$lastname <- str_split_fixed(titanic3$name, " ",3)[,1]
```
A distribuição dos títulos e sobrenomes através de um gráfico de barras representando a frequência absoluta é a seguinte:  
[Figure - barplot_title.pdf](figures/barplot_title.pdf)  
[Figure - barplot_lastname.pdf](figures/barplot_lastname.pdf)

Observando os valores obtidos nas 20 observações de maior frequência é possível verificar que existe muito "ruído" no nome de tratamento adotado, caso seja o objetivo utilizar essa variável em uma regressão uma solução é a inclusão de valores com baixa frequência em um novo grupo para evitar ruídos nas predições. Essa abordagem pode ser considerada já que são nomes de tratamento como "Capt", "Major" e afins com baixíssimas observações.

Já sobre as 20 observações de maior frequência em relação aos sobrenomes é possível verificar que algumas famílias eram compostas por diversos membros a bordo. Ao decorrer das análises poderemos verificar se há uma ligação entre estes valores, classes e sobreviventes.

**Dica:** Unir valores de baixa frequência da nova coluna _title_ e verificar as correlações entre as maiores famílias a bordo presentes na nova coluna _lastname_.

## Coluna _sex_

A coluna _sex_ não possui nenhuma observação faltante e seus dados são categoricos. Dessa forma esta apta ao uso sem maiores manipulações.

**Dica:** Transformar em variável _dummy_ ao final da análise exploratória.

## Coluna _age_

A coluna _age_ apresenta diversas observações faltantes e dessa forma necessita uma análise para a imputação dos valores faltantes de maneira menos enviesada.

Inicialmente verificamos algumas medidas de posição e graficamente analisamos a distribuição dos dados.
```r
range(titanic3$age, na.rm = TRUE)
```
Considerando apenas as amostras existentes, é possível confirmar os extremos dos dados, inicialmente não iremos considerá-los como _outliers_, pois mesmo com uma baixa probabilidade ainda era possível pessoas terem idades próximas a 80 anos.

O [boxplot](figures/boxplot_age.pdf), [histograma](hist_age.pdf) e [qqplot](figures/qqplot_age.pdf) da coluna _sex_ são considerados também.
```r
boxplot(titanic3$age,
        main= "Age boxplot",
        xlab= "Age in years",
        horizontal = TRUE,
        col = "green")
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
```

Visualmente há evidências que a coluna _age_ segue uma distribuição próxima a Normal, ~~para obtermos a confirmação ou não, realizamos o teste de normalidade de [Shapiro-Wilk](https://doi.org/10.1093/biomet/52.3-4.591)~~.
```r
shapiro.test(age)
```
~~Através do teste de normalidade de Shapiro-Wilk não assumimos a normalidade dos dados e~~  dessa forma a inclusão de novas informações ao dataset será realizada via _Imputação Múltipla_ utilizando a estratégia _bootstrap_.

_Não foi considerado o teste de normalidade de Shapiro-Wilk devido o baixo poder de teste apresentado por este teste. Está em desuso._ 

Optamos por selecionar apenas um conjunto de dados para a distribuição dos dados, neste caso foi selecionado a coluna _pclass_. Para cada conjunto _pclass_ são consideradas **R** reamostragens _bootstrap_ e selecionada a média entre esses conjuntos e substituida a observação faltante.

Novos [boxplot](figures/boxplot_age2.pdf), [histograma](hist_age2.pdf) e [qqplot](figures/qqplot_age2.pdf) da coluna _age_.

## Coluna _sibsp_

A coluna _sibsp_ não possui observação faltante. Não trabalharemos com ela neste momento.

**Dica:** Transformar em variável _dummy_ ao final da análise exploratória.

## Coluna _parch_

A coluna _parch_ não possui observação faltante. Não trabalharemos com ela neste momento.

**Dica:** Transformar em variável _dummy_ ao final da análise exploratória.

## Coluna _ticket_

A coluna _ticket_ possui dados mistos, isto é, inteiros e strings. Todas as observações possuem inteiros, dessa forma podemos tentar buscar alguma relação entre os valores e as outras variáveis, contudo até o momento não utilizaremos, caso seja necessário voltaremos para extrair novas informações.

## Coluna _fare_

A coluna _fare_ possui uma única observação faltante, iremos utilizar a mesma abordagem utilizada para a inclusão de valores na coluna _age_ para a inclusão deste valor e depois trataremos os valores outliers, se necessário.

```r
titanic3[which(is.na(titanic3$fare)),]
```
A saída indica que é um passageiro da terceira classe. 
Iremos substituir através da média resultante da abordagem _bootstrap_.

Analisando os [boxplot](figures/boxplot_fare1.pdf), [histograma](hist_fare1.pdf) e [qqplot](figures/qqplot_fare1.pdf) da coluna _fare_, é possível afirmar que há _outliers_ e que essa coluna não segue distribuição Normal. 

Sobre os valores _outliers_ da coluna _fare_ podemos verificar que são valores iguais a zero e o valor extremo maior que 500. Estes valores serão considerados em uma outra análise.

## Coluna _cabin_

A coluna _cabin_ possui alguns valores faltantes, contudo não é possível a imputação dos valores faltantes neste momento. Uma alternativa seria associar famílias com a cabine, contudo essa associação não é possível por ausência de informações no dataset.

## Coluna _embarked_

A coluna _embarked_ possui 2 observações faltantes. Iremos preencher baseado na frequência de embarque por classe.
```r
i_n<-which(is.na(titanic3$embarked))
titanic3[which(is.na(titanic3$embarked)),]
# The 2 values belong to "1st" class.
t<-table(titanic3$embarked,titanic3$pclass)
c<-prop.table(t)
x<-c("Cherbourg", "Queenstown", "Southampton")
# Input new values in NA
for (i in i_n){
  titanic3$embarked[i]<-sample(x,1,prob = c)
}
remove(i_n,t,c,x,i)
```

**Dica:** Transformar em variável _dummy_ ao final da análise exploratória.

## Coluna _boat_

A coluna _boat_ possui observações faltantes, contudo não é possível realizar a imputação dos dados faltantes neste momento.

## Coluna _body_

A coluna _body_ possui diversas observações faltantes que não são possíveis recuperar no momento.

## Coluna _home.dest_

A coluna _home.dest_ possui alguns valores faltantes, contudo não é possível a imputação dos valores faltantes neste momento. Uma alternativa seria associar famílias com a observação, contudo essa associação não é possível por ausência de informações no dataset no momento.

## Inclusão de novas colunas: _mother_ e _nfamily_

Nova colunas podem ser incluídas ao analisar os dados. Podemos inferir se há mães ou não a bordo, já que temos informações de filhos e parentes a bordo e também verificar o tamanho da família a bordo.

```r
titanic3$nfamily <- titanic3$sibsp + titanic3$parch + 1
titanic3$mom <- ifelse((titanic3$sex=="female" & titanic3$parch>=1 & titanic3$age > 18), 1, 0)
```

**Dica:** Transformar a coluna _nfamily_ em variável _dummy_ ao final da análise exploratória.

---

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

---
## Save file
```r
save.image("titanic3.RData")
```