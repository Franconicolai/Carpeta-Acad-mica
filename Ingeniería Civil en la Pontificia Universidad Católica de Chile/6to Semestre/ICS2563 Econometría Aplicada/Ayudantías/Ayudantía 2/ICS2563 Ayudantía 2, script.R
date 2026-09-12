#install.packages("haven")
#install.packages("readr")  
library(haven)
library(readr)

#Abrimos los archivos
simce <- read_dta("simce8b2019_rbd.dta")
names(simce) <- toupper(names(simce)) #pasar de minusculas a mayusculas en los titulos de bdd
SEP <- read_csv2("Preferentes_Prioritarios_y_Beneficiarios_SEP_2019.csv")
matricula <- read_csv2("Resumen_Matricula_EE_Oficial_2019.csv")

#Combinamos las dos bases de datos por RBD
datos_0 <- merge(simce, SEP, by="RBD")
datos <- merge(datos_0, matricula, by= "RBD")

#Construimos un índice de vulnerabilidad
indice_vulnerabilidad <- (datos$N_PRIO / datos$MAT_TOTAL)

#Lo agregamos a nuestra base de datos con la que trabajaremos
datos <- cbind(datos, indice_vulnerabilidad)
datos

# Pregunta 1 --------------------------------------------------------------

#Calculamos la mediana y realizamos un histograma del índice de vulnerabilidad 
m <- median(indice_vulnerabilidad, na.rm = TRUE)
hist(x = indice_vulnerabilidad, main = "Proporción de estudiantes prioritarios
     en establecimientos", xlab = 'Proporción', ylab = "Cantidad de colegios")

#Valores estandarizados de puntajes SIMCE-matemáticas
ptje_mate <- (datos$PROM_MATE8B_RBD - mean(datos$PROM_MATE8B_RBD, na.rm = TRUE))/ 
  sd(datos$PROM_MATE8B_RBD, na.rm = TRUE)


# Pregunta 2 ---------------------------------------------------------------

#Función que retorna los coeficiente de la regresión MCO
calcular_coef_MCO <- function(x,y){
  cov_xy <- cov(na.omit(cbind(x,y)))[2]
  var_x <- var(na.omit(x))
  beta_1 <- cov_xy / var_x
  beta_0 <- mean(y, na.rm = TRUE) - mean(x, na.rm = TRUE)*beta_1
  coef <- cbind(beta_0, beta_1)
  return(coef)
}

#Calculamos los coeficientes
coef <- calcular_coef_MCO(indice_vulnerabilidad, ptje_mate)
beta_0 <- coef[1, 1]
beta_1 <- coef[1, 2]

#Realizamos el scatterplot junto a su recta
y <- ptje_mate
x <- indice_vulnerabilidad

plot(x, y, pch=19, col="black", main = "Relación estudiantes prioritarios y puntaje SIMCE - Matemáticas")
abline(a = beta_0, b = beta_1, col = "red" )



#Pregunta 3 --------------------------------------------------------------
#Seleccione una muestra aleatoria de 10 establecimientos y 
#estime los nuevos valores de beta, haga 10000 simulaciones

relacion <- na.omit(cbind(indice_vulnerabilidad, ptje_mate))

#Utilizamos una semilla vacía y generamos un vector vacío
set.seed(2563)
conjunto_beta <- vector()

#Iteramos 10000 veces para obtener distintos betas y unirlos al vector
for (i in 1:10000)
{
  #Utilizamos una muestra aleatoria de 10 establecimientos y calculamos beta
  muestra_aleatoria <- relacion[sample(nrow(relacion), size = 10),]
  
  covarianza_aleatoria <- cov(muestra_aleatoria)[2]
  varianza_aleatoria <- var(muestra_aleatoria)[1]
  
  beta_aleatoria <- covarianza_aleatoria/varianza_aleatoria
  
  conjunto_beta <- append(conjunto_beta, beta_aleatoria, after = length(conjunto_beta))
  
}

#Realizamos el histograma de los valores de beta obtenidos
hist(x = conjunto_beta, main = "Histograma de coeficientes de 10 muestras aleatorias", xlab = "x: Valores de beta", 
     ylab = "y: Cantidad de beta")


#Seleccione una muestra aleatoria de 100 establecimientos y 
#estime los nuevos valores de beta, haga 10000 simulaciones

#Utilizamos una semilla vacía y generamos un vector vacío
set.seed(2563)
conjunto_beta = vector()

#Iteramos 10000 veces para obtener distintos betas y unirlos al vector
for (i in 1:10000)
{
  #Utilizamos una muestra aleatoria de 100 establecimientos y calculamos beta
  muestra_aleatoria = relacion[sample(nrow(relacion), size = 100),]
  
  covarianza_aleatoria = cov(muestra_aleatoria)[2]
  varianza_aleatoria = var(muestra_aleatoria)[1]
  
  beta_aleatoria = covarianza_aleatoria/varianza_aleatoria
  
  conjunto_beta = append(conjunto_beta, beta_aleatoria, after = length(conjunto_beta))
  
}

#Realizamos el histograma de los valores de beta obtenidos
hist(x = conjunto_beta, main = "Histograma de coeficientes de 100 muestras aleatorias", xlab = "x: Valores de beta", 
     ylab = "y: Cantidad de beta")

