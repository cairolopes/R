# Name-> Cairo da Silva Lopes 
# 
# Install
# install.packages("datasets.load")

# Call library
library(datasets)

# Call the mtcars dataset
data(mtcars)
mtcars


# Mpg [Miles/(US) gallon]: Continuous
# Cyl [Number of cylinders]: Discrete
# Disp [Displacement (cu.in.)]:  Continuous
# Hp [ Gross horsepower]: Continuous or ordinal
# Drat [ Rear axle ratio]: Continuous
# Wt [Weight (1000 lbs)]: Continuous
# Qsec [1/4-mile time]: Continuous
# Vs [Engine, (0 = V-shaped, 1 = straight)]: Nominal
# Am [Transmission (0 = automatic, 1 = manual)]: Nominal
# Gear [ Number of forward gears]: Discrete 
# Carb [Number of carburetors]: Discrete

library(expss)
mtcars = apply_labels(mtcars,
                      mpg = "Miles/(US) gallon",
                      cyl = "Number of cylinders",
                      disp = "Displacement (cu.in.)",
                      hp = "Gross horsepower",
                      drat = "Rear axle ratio",
                      wt = "Weight (1000 lbs)",
                      qsec = "1/4 mile time",
                      vs = "Engine",
                      vs = c("V-engine" = 0, #Here I managed to put what each
                             "Straight engine" = 1), # rating of each nominal rod
                      am = "Transmission",
                      am = c("Automatic" = 0,
                             "Manual"=1),
                      gear = "Number of forward gears",
                      carb = "Number of carburetors"
)

# 1- Escolha uma variável numérica e faça a análise univariada com as 
# técnicas aprendidas até o momento. Interprete os resultados.
library(ggplot2)
# 1.1 Histograma
hist(mtcars$gear)
# pelo formato do histograma podemos classificar como assimetrico a direita
# mode


# 1.2 Boxplot

summary(mtcars$gear)
# com essa funçao ja podemos recolher as medidas que envolvem um boxplot
# (min, q1,q2,q3, mediana e max). Tambem temos a media, mas nao entra nos parametros
# do bloxplot
ggplot(mtcars, aes(y=gear)) +
  geom_boxplot() +
  labs( x="", y="number of gears", title = "mtcars's gears")

# apresenta esse formato curioso devido que o q1 é igual ao minino
# a nossa mediana esta com o mesmo valor que o q3.
# entao tras esse formato parecendo uma vela

# 1.3 Medidas de posiçao

#   1.3.1 Moda
mode_gear <- table(mtcars$gear); names(mode_gear)[mode_gear ==max(mode_gear)]
# podemos observar que o numero de marchas que mais aparece sao os com 3 marchas

#   1.3.2 Mediana
median(mtcars$gear)

#   1.3.3 Media
mean_gear <- mean(mtcars$gear)
# todos essa valores podemos observar com a funçao summary, feita anteriormente

# 1.4 Medidas de dispersao

#   1.4.1 Desvio padrão e Variância
sd_gear <- sd(mtcars$gear)
sd(mtcars$gear)**2
# pode utilizar tambem a funçao var()

#   1.4.2 Quartis
quantile(mtcars$gear, probs = c(0.25,0.5,0.75))
#   1.4.3 Max
max(mtcars$gear)
#   1.4.4 Min
min(mtcars$gear)
#   1.4.5 Amplitude
amplitude_total <- max(mtcars$gear) - min(mtcars$gear)
amplitude_total
#   1.4.6 Coeficiente de variaçao

cv <- (sd_gear/mean_gear) * 100
cv

# 2- Escolha uma (ou mais) abordagem adequada e avalie a simetria das variáveis 
# numéricas.
library(Hmisc)
hist.data.frame(mtcars)
# utilizando a funçao hist.data.frame() da bibilioteca Hmisc, conseguimos
# observar os os histogramas do dataset
# percebemos que a maioria dos casos sao assimetricos a direita e algums ate um
# pouco simetricos, porem para mais detalhes vamos observar o boxplot
boxplot(mtcars)
# a visualizaçao estao bem dificil de enxergar devido a escala, logo as variaveis com
# escalas menores ficam praticamentes invisiveis...
mtcars_scale <- scale(mtcars)
# agora conseguimos observar melhor nosso boxplot pois colocamos todos na mesma
# escala

## 3- Caso alguma variável quantitativa apresente assimetria, teste algumas 
# opções de transformação e indique qual a mais adequada.

# vamos escolher a primeira coluna que ela é bem interessante

hist(mtcars$carb, main="orig", ylab="", xlab="", col="darkgrey", border="white")
# observando o histograma identificamos logo de cara que ela tem as caracteristicas
# assimetrica a direita
summary(mtcars$carb)
# aqui observamos algumas medidas de dispersao e de variaçao
boxplot(mtcars$carb)
# é interesante colocar essa medidas em forma de grafico para podermos ter uma 
# melhor visualizaçao, a minha preferencia é o boxplot 

# Vamos agora utilizar algumas tecnicas de tranformaçao
par(mfrow=c(2,2))

hist(log(mtcars$carb), main="log", ylab="", xlab="", col="darkgrey", border="white")
hist(mtcars$carb ** (1/4), main="p=1/4", ylab="", xlab="", col="darkgrey", border="white")
hist(mtcars$carb ** (1/2), main="p=1/2", ylab="", xlab="", col="darkgrey", border="white")
hist(mtcars$carb ** (1/3), main="p=1/3", ylab="", xlab="", col="darkgrey", border="white")

# Apos a aplicaçao dessas 4 transformaçoes percebemos que utilizando  
# nao foram muito eficientes 

## 4- Escolha duas variáveis qualitativas e faça a análise bivariada. 
# Interprete os resultados.

# quando fazemos analise de duas variaveis qualitativas utilizamos a tabela 
# de cruamento de dados para entender melhor a situação
tableCars <- table(mtcars$vs, mtcars$am)
sunResultRow <- margin.table(tableCars,1)
sunResultCol <- margin.table(tableCars,2)

finalResulTable <- rbind(cbind(tableCars,sunResultRow),
                         c(sunResultCol, sum(sunResultRow)))
dimnames(finalResulTable)[[1]][3] <- "sunResultCol"

finalResulTable

library(scales)
library(reshape2)
# minha tabela esta como data.frame logo irei passar so as colunas que vou 
# utilizar como data.table
dataTableMtcars <- as.data.table(mtcars[ , c("vs","am")])
# logo apois vou tornar as colunas factor para termos os leveis e ficar melhor
# a nossa visualizaçao 
dataTableMtcars$vs <- factor(dataTableMtcars$vs)
dataTableMtcars$am <- factor(dataTableMtcars$am)

dataTableN <- dataTableMtcars[ , .N, by=list(vs, am)]
colnames(dataTableN) <- c("Engine","Transmission","Total")

ggplot(dataTableN , aes(x =Engine , y = Total, fill = Transmission))+
  geom_bar(stat="identity", position = "stack") +
  geom_text(aes(label=Total),position = position_stack(vjust = 1),
            vjust=-0.25)

# Analisando a tabela podemos verificar que os carros que sao automaticos 
# possuem o mesmo numero de Stranight engine (tipo do motor) que os manuais
# ja os V-engine estao mais presentes nos Aumaticos

# 5- Escolha duas variáveis quantitativas e faça a análise bivariada.
# Interprete os resultados.

ggplot(mtcars, aes(x=mpg, y=hp)) +
  geom_point(colour = "orange")+
  theme_dark()

# podemos observar claramente uma correlaçao linear negativa
# devido que: quanto menor hp (praticamente torque) maior a capacidade de fazer
# 1 milha por galao 
# e colocando o qsec podemos observar que em alguns pontos 
cor(mtcars$mpg, mtcars$hp)
# podemos observar que a correlaçao ficou no valor de  cor ~ -0.78, que indica
# uma correlaçao negativa Forte

# 6- Escolha uma variável qualitativa e uma quantitativa e 
# faça a análise bivariada. Interprete os resultados.
# mpg e vs
library(knitr)
mtcarsTable <- as.data.table(mtcars)
mtcarsTable$vs <- factor(mtcarsTable$vs)
kable(mtcarsTable[, list(mean=mean(mpg),
                         median = median(mpg),
                         sd = sd(mpg),
                         min = min(mpg),
                         max = max(mpg),
                         q1 = quantile(mpg, 0.25),
                         q2 = quantile(mpg, 0.50),
                         q3 = quantile(mpg, 0.75),
                         Total=.N),
                  by=vs])



# somente observando a tabela ja podemos perceber que os motores Straight sao mais 
# economicos do que os moteres V-shape ( V-engine)
# os valores dos motores Straight é bem maior do que os V-engine

ggplot(mtcarsTable , 
       aes(x = mpg, y = vs)) +
  geom_boxplot()+
  theme_dark()

# o boxplot só comprova o que vimos na tabela, perceba que todos os motores 
# straight engine estao bem mais presentes na direita, onde fazem mais milhas
# por galao
# temos ali um caso isolado que é o Porsche 914-2 que é o unico entre os outros
# que possuem motores V-engine que possui somente 4 cilindros
mtcars[ mtcars$mpg == max(mtcars[vs == 0,"mpg"]),]

# quantificar o grau de dependencia 
globalVar <- var(mtcars$mpg)
vEngineVar <- var(mtcars[vs %in% 0,"mpg"])
sEngineVar <- var(mtcars[vs %in% 1,"mpg"])

mean(varVEngine, varSEngine)
weightedVar <- (vEngineVar*nrow(mtcars[vs == 0,]) +
                  sEngineVar*nrow(mtcars[vs == 1,]))/nrow(mtcars[,vs])

r2 <- (globalVar-weightedVar)/globalVar

tableVar<-data.table(Var=c("Global","V-Engine","Straight-Engine"),
                 values=c(globalVar,vEngineVar,sEngineVar))
# percebemos que os valores das variancias dentro de cada categoria é menor
# do que a global, logo sabemos que: a variavel qualitativa melhora a capacidade 
# de previsao da quantitativa
# R2 = 42.07%  da variaçao total do mpg (milhas/galao) é explicado pela variavel
# cv (enginer).

# Extra
ggplot(mtcars, aes(x=mpg, y=hp)) +
  geom_point(alpha = 0.7,aes(size=disp,shape=factor(vs)),colour = "orange")+
  theme_dark()

# brincando com as variaveis e com as correlaçoes podemos observar no grafico
# que a medida que diminui o HP, aumenta o mpg e a presença dos motores Straight
# aumentam
# Podemos observar tambem que os motores que tem um maior HP sao os V-engine
# e tambem sao os que tem maior despolamento (disp) que sao as cilindradas.

