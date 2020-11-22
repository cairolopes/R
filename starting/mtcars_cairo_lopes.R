# Name-> Cairo da Silva Lopes 
# 
# Install
# install.packages("datasets.load")

# Call library
library(datasets)

# Call the mtcars dataset
data(mtcars)
mtcars
# 1-Identifique o tipo de cada variável presente no dataset.
str(df_mtcars)

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

# 2-Escolha uma variável qualitativa, construa a tabela de frequências
# e interprete os valores.
 
# When analyzing our dataset and following its guidelines
# I immediately looked for nominal variables and found
# "vs -> Engine (0 = V-shaped, 1 = straight)" and
# "am -> Transmission (0 = automatic, 1 = manual)
# I quickly built the frequency table and realized 
# that it was between 0 and 1 (a binary classification)

freq_am <- table(mtcars$am)

# I looked for some way to replace at least in the table header 
# instead of 0 having automatic and 1 having manual
# I ended up finding the library expss
#install.packages('expss')
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

freq_am <- table(mtcars$am)

prop_am <- round(prop.table(freq_am)*100,2) #created variable prop
# now we can understand that 19 are automatic (59.38%) and 13 manuals (40.62%)

# 2.1 Frequency distributions
library(knitr)
kable(cbind(freq_am,prop_am),col.names = c("Freq","Prop"))


# 3-Construa um gráfico de barras para uma variável qualitativa utilizando 
# a biblioteca ggplot2.
# 3.1 - Transforming the variable into a factor and renaming the factor levels
mtcars$vs <- factor(mtcars$vs,levels= names(table(mtcars$vs))) 
# the "names" function returns variable names

# 3.2 - Preparing the data set
freq_vs <- table(mtcars$vs)
prop_vs <- round(prop.table(table(mtcars$vs))*100,2)
df_vs <- data.table(cbind(freq_vs, prop_vs))
df_vs[,classification := levels(mtcars$vs)]
#3.3 - Ploting...
ploting <- function(df,col,xLabel, yLabel, titleName){
  ggplot(df, aes(x=col)) + 
    geom_bar() + 
    labs(x = xLabel, y= yLabel, title= titleName)
  
}
ploting(mtcars,mtcars$vs, "Engine","Frequency", "MTcars dataset")
# 
# ggplot(mtcars, aes(x=vs)) +
#   geom_bar() +
#   labs(x="Engine",y="Freq",title="MTcars dataset")

# 4-Escolha uma variável quantitativa e construa um gráfico de barras utilizando
# a biblioteca ggplot2

# 4.1 - As we know, we have to group the quantitative variables 
# before we do the frequency directions
# Using the function made by the teacher in class ...

group <- function(df, group = NULL, sep = "-", text = ""){
  # Input: Vetor com valores numéricos.
  # Output: Fator com nomes dos grupos da variável numérica.
  if(length(group) < 2) {
    stop("Número de grupos deve ser maior que 2.")		
  }
  # (We don't need this because all variables are already numeric)
  #x <- as.numeric(x)
  
  brackets <- paste(group, group[2:length(group)]-1, sep = sep)
  brackets[length(group)] <- paste0(group[length(group)], " +")
  df <- factor(findInterval(df, group), levels = c(1:length(brackets)))
  levels(df) <- paste0(brackets, text)
  df
}
mtcars$qsec <- group(mtcars$qsec,group=c(trunc(min(mtcars$qsec)), #trunc -> decimal removed
                                         round(mean(mtcars$qsec)), 
                                         floor(max(mtcars$qsec))))#floor -> round down 

# 4.2 - Preparing the data set:
mtcars$vs <- factor(mtcars$vs,levels= levels(mtcars$qsec)) 
freq_qsec <- table(mtcars$qsec)
prop_qsec <- round(prop.table(table(mtcars$qsec))*100,2)
df_qsec <- data.table(cbind(freq_qsec, prop_qsec))
df_qsec[, classification :=levels(mtcars$qsec)]
# 4.3 - Ploting...
# ggplot(df_qsec, aes(x='', y=prop_qsec, fill= classification)) + 
#   geom_bar(width=1, stat = "identity") + 
#   coord_polar("y",start=0)
ploting(mtcars,mtcars$qsec,"1/4 mile time","Frequency","Speed ??????ranges")
