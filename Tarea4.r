install.packages("arules")
library(arules)
install.packages("genero")
library(genero)
install.packages("ggplot2")
library(ggplot2)

data <- read.csv('MIGRACION_BDP.csv', sep=",")

reglas <- apriori(data, parameter = list(support=0.2, confidence=0.5))

inspect(reglas[0:100])

colnames(data)[colnames(data) == "PEI3"] <- "Sexo"
colnames(data)[colnames(data) == "PEI4"] <- "Edad"
colnames(data)[colnames(data) == "PEI5"] <- "Anio"

data_apriori <- data

reglas <- apriori(data_apriori, parameter = list(support=0.2, confidence = 0.5))

cluster <- kmeans(data_apriori, centers=4)

# Visualización de 'Edad vs Zona'
ggplot(data_apriori, aes(x = Edad, y = ZONA, color = as.factor(cluster$cluster))) +
  geom_point() +
  geom_point(data = as.data.frame(cluster$centers), aes(x = Edad, y = ZONA), color = "black", size = 4, shape = 17) +
  labs(title = "Edad vs Zona") +
  theme_minimal()

# Visualización de 'Departamento vs Zona'
ggplot(data_apriori, aes(x = DEPARTAMENTO, y = ZONA, color = as.factor(cluster$cluster))) +
  geom_point() +
  geom_point(data = as.data.frame(cluster$centers), aes(x = DEPARTAMENTO, y = ZONA), color = "black", size = 4, shape = 17) +
  labs(title = "Departamento vs Zona") +
  theme_minimal()

# Filtrar datos para el mujeres
dataf <- subset(data_apriori, Sexo == 2)
clusterf <- kmeans(dataf, centers = 3)

# Visualización de 'Edad vs Zona Mujeres'
ggplot(dataf, aes(x = Edad, y = ZONA, color = as.factor(clusterf$cluster))) +
  geom_point() +
  geom_point(data = as.data.frame(clusterf$centers), aes(x = Edad, y = ZONA), color = "black", size = 4, shape = 17) +
  labs(title = "Edad vs Zona para mujeres") +
  theme_minimal()

# Filtrar datos para el hombres
datam <- subset(data_apriori, Sexo == 1)
clusterm <- kmeans(datam, centers = 4)

# Visualización de 'Edad vs Zona Hombres'
ggplot(datam, aes(x = Edad, y = ZONA, color = as.factor(clusterm$cluster))) +
  geom_point() +
  geom_point(data = as.data.frame(clusterm$centers), aes(x = Edad, y = ZONA), color = "black", size = 4, shape = 17) +
  labs(title = "Edad vs Zona para hombres") +
  theme_minimal()
