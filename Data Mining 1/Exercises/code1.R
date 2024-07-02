#https://r4ds.had.co.nz/explore-intro.html
library(tidyverse)

#dataframe relacionado com carros
mpg 

#gráfico de dispersão que relaciona 
#displ (tamanho) e hwwy (eficiência do gasóleo)
#demonstra uma relação negativa entre as variáveis "\"
ggplot(data = mpg) + geom_point(mapping = aes(x = displ, y = hwy)) 

#1. Run ggplot(data = mpg). What do you see?
ggplot(data = mpg)
#não faz nada 

#2. How many rows are in mpg? How many columns?
#tem 234 colunas e 11 atributos

#3. What does the drv variable describe? 
#descreve o tip de condução rodas da frente (f), rodas de trás (r),
#4 rodas (4)

#4. Make a scatterplot of hwy vs cyl.
ggplot(data = mpg) + geom_point(mapping = aes(x = hwy, y = cyl)) 

#5. What happens if you make a scatterplot of class vs drv?
ggplot(data = mpg) + geom_point(mapping = aes(x = class, y = drv))
eleventh_column <- mpg[[11]]
print(eleventh_column)
#este gráfico não nos diz nada de relevante

#gráfico de dispersão onde a cor dos pontos são definidos consuante
#a classe, pode se fazer o mesmo com o "size", "alpha", "shape"
ggplot(data = mpg) + geom_point(mapping = aes(x = displ, y = hwy, color = class))


