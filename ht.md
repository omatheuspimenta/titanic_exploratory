# [Análise Exploratória](github.com/omatheuspimenta/titanic_exploratory)
### Análises e Testes de Hipótese
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
```

## Extraindo algumas informações

Inicialmente serão apresentadas algumas informações que podem ser extraídas dos dados.  
* Comparando o número de passageiros que sobreviveram por classe, é possível confirmar que o maior número de passageiros que foram salvos estavam lotados na primeira classe.
```r
tapply(titanic3$survived, titanic3$pclass, sum)
```

|1st| 2nd| 3rd|
|--|----|----|
|200| 119| 181|

* Em número absolutos pode parecer que os passageiros da primeira e terceira classe estão próximo, contudo quando comparamos a % dos passageiros que sobreviveram por classe temos que a maioria dos passageiros lotados na primeira classe sobreviveram:
```r
tapply(titanic3$survived, titanic3$pclass, sum) / table(titanic3$pclass)
```
|1st|       2nd|       3rd|
|---|---------|-----------| 
|0.619| 0.429| 0.255|

* Comparando o número de passageiros do sexo masculino e feminino por classe é possível verificar que há uma maior presença de indivíduos do sexo masculino em todas as classes, quando levamos em consideração a idade média em cada uma das classes, a primeira classe possuí a maior idade média, seguida pela segunda classe e por último aparece a terceira classe.
```r
tapply(titanic3$sex, titanic3$pclass, table)
```
|`1st`| `1st`|           `2nd`| `2nd`|   `3rd`|`3rd`|
|-----|------|----------------|------|--------|-----|
|female|   male|    female|   male|     female|   male| 
|144|    179|       106|    171|        216|    493| 
```r
tapply(titanic3$age, titanic3$pclass, mean)
```
| 1st|      2nd|      3rd| 
|----|---------|---------|
|39.14061| 29.51715| 24.87024| 
* Já em relação ao valor médio pago nas passagens para cada uma das classes, a primeira classe tem o maior valor médio, seguindo da segunda e terceira classe, conforme o esperado.
```r
tapply(titanic3$fare, titanic3$pclass, mean)
```
|      1st|      2nd|      3rd|
|---------|---------|---------|
|87.50899| 21.17920| 13.30292|
* Quando analisado o número de pessoas que embarcaram em cada uma das cidades possíveis, a primeira classe majoritariamente adentrou ao Titanic em Cherbourg ou Southampton, tanto na primeira e segunda classe o local de Queenstown teve poucos embarques. Já a terceira classe teve um grande números de passageiros que embarcaram em Southampton. Essas informações acabam refletindo em pessoas que se salvaram ou não, devido a qual classe que recebeu estes passageiros.
```r
tapply(titanic3$embarked, titanic3$pclass, table)
```
* Utilizando a coluna que criamos para representar as mães a bordo, podemos extrair informações de quantas mães estão lotadas em cada classe e após isso verificar se todas as mães sobreviveram ou não no desastre ocorrido com o Titanic.
```r
tapply(titanic3$mom, titanic3$pclass, sum)
```
|1st| 2nd| 3rd|
|---|----|----| 
|40|  28|  46|
* Analisando mães sobreviventes por cada uma das classes, na primeira e segunda classe apenas uma mãe não conseguiu salvar-se, vamos verificar quem são essas pessoas:
```r
titanic3[titanic3$pclass=="1st" & titanic3$survived==0 & titanic3$mom==1 ,]
titanic3[titanic3$pclass=="2nd" & titanic3$survived==0 & titanic3$mom==1 ,]
```
* A mãe que, infelizmente, não sobreviveu na primeira classe é a Sra. Hudson J. C. Allison, ela estava com dois filhos a bordo. Na segunda classe, a Sra. William (Anna Sy Lahtinen não sobreviveu juntamente com seu marido e seu único filho a bordo.
* A família _Brown_ é a família que teve o maior número de indivíduos sobreviventes, contabilizando **5** pessoas sobreviventes.   
[Figure - barplot_families.pdf](figures/barplot_families.pdf)  

## Correlações 

Buscamos identificar correlações entre as variáveis presentes no dataset.  
Visualmente obtemos a seguinte matriz de correlação (juntamente com um _heatmap_):
```r
# correlation matrix
cormat <- round(cor(df_cor),2)
cormat_melted <- melt(cormat)
# Create a ggheatmap
ggheatmap <- ggplot(cormat_melted, aes(Var2, Var1, fill = value))+
  geom_tile(color = "white")+
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Pearson\nCorrelation")  +
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   size = 12, hjust = 1))+
  coord_fixed()
print(ggheatmap)
# graph
ggheatmap + 
  geom_text(aes(Var2, Var1, label = value), color = "black", size = 4)
```
[Figure - heatmap_correlation.pdf](figures/heatmap_correlation.pdf)  
Utilizamos a correlação de Pearson no momento, podemos apresentar outras medidas de correlação se necessário.
A matriz de correlação apresenta resultados plausíveis com o esperado, temos uma correlação negativa entre _class_ e _fare_, o que é esperado, pois quanto "menor" a identificação da classe "maior" o valor da passagem. Também é possível observar essa relação entre _age_ e _class_. As análises seguintes em relação a matriz de correlação podem serem tomadas de maneira análoga a essa.

_Se necessário pode-se utilizar teste de hipótese para verificar a correlação entre as variáveis._

```r
cor.test(df_cor$class,df_cor$fare, method = "pearson")
```

## Scatter Plot
Para verificar a distribuição entre as observações das variáveis um _scatter plot_ foi realizado relacionando _fare_ e _age_, separados por _class_. Através dessa representação é possível verificar que o valor médio entre segunda e terceira classe são bem próximos, por observação. Já os valores para a primeira classe são em sua maioria "distantes" dos valores da segunda e terceira classe. Essa observação é interessante já que através dela é possível verificar que mesmo na primeira classe alguns passageiros pagaram valores inferiores a passageiros de classes inferiores.  
[Figure - scatterplot_agefare.pdf](figures/scatterplot_agefare.pdf)  

## Teste de Hipótese

Alguns testes de hipótese (não paramétrico, já que não assumimos a normalidade das variáveis) foram tomados com o objetivo de verificar se a média das observações das variáveis _age_ e _fare_ realmente eram diferentes para cada uma das três classes.
```r
# HT - Wilcoxon Test
#H0: \mu1 = \mu2
#H1: \mu1 \neq \mu2
# split dataframe
class1 <- titanic3[titanic3$pclass == "1st",]
class2 <- titanic3[titanic3$pclass == "2nd",]
class3 <- titanic3[titanic3$pclass == "3rd",]
# wilcox.test -> age
wt_age12 <- wilcox.test(class1$age, class2$age)
wt_age23 <- wilcox.test(class2$age, class3$age)
wt_age31 <- wilcox.test(class3$age, class1$age)
# wilcox.test -> fare
wt_fare12 <- wilcox.test(class1$fare, class2$fare)
wt_fare23 <- wilcox.test(class2$fare, class3$fare)
wt_fare31 <- wilcox.test(class3$fare, class1$fare)
```
Em todos os testes de hipótese realizados os valores retornados indicaram que as medidas _age_ e _fare_ são diferentes para todas as classes. Isso é, há uma diferença entre as médias das variáveis _age_ e _fare_ entre as classes.