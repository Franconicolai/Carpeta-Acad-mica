############################
##  AYUDANTÍA 1 - Script  ##
############################

#################
##  Librería   ##
#################

library(rio)
library(dplyr)

##################
##  Preliminar  ##
##################

Casen_2022 <- import("Casen_2022_muestra.dta")

# Arreglamos la variable Sexo para que sea binaria, ya que se encuentra definida como
# 1 Hombre - 2 Mujer
Casen_2022$sexo <- ifelse(Casen_2022$sexo == 2, 1, 0)

# Filtrar trabajadores entre 25 y 65 años
Casen_trabajadores <- Casen_2022[Casen_2022$o1 == 1 & 
                                   Casen_2022$edad >= 25 &
                                   Casen_2022$edad <= 65 &
                                   Casen_2022$ypc > 0, ]

# Filtramos para obtener las variables que nos interesan 
Casen_trabajadores <- Casen_trabajadores[, c("ypc", "esc", "sexo")]

# Eliminamos los valores NA
Casen_trabajadores <- Casen_trabajadores[!is.na(Casen_trabajadores$ypc) &
                                           !is.na(Casen_trabajadores$esc), ]

# Creamos la variable log(ypc)
Casen_trabajadores$log_ypc <- log(Casen_trabajadores$ypc)

########
## a) ##
########


plot(Casen_trabajadores$esc, Casen_trabajadores$log_ypc,
     xlab = "Años de escolaridad",
     ylab = "Ingreso per cápita",
     main = "Relación entre educación e ingreso",
     pch = 19, col = rgb(0, 0, 1, 0.4))


########
## c) ##
########

# Se crea una función que se encarga de calcular beta
beta = function(x, y){
  if (var(x) == 0) {
    return(0)
  } else {
    return(cov(x, y) / var(x))
  }
}


# Se crea una función que se encarga de calcular alpha
alpha = function(x, y){
  a = mean(y) - beta(x, y) * mean(x)
  return(a)
}


########
## d) ##
########

beta_1 <- beta(Casen_trabajadores$esc, Casen_trabajadores$log_ypc)
alpha_1 <- alpha(Casen_trabajadores$esc, Casen_trabajadores$log_ypc)

beta_0 <- beta(Casen_trabajadores$esc, Casen_trabajadores$ypc)
alpha_0 <- alpha(Casen_trabajadores$esc, Casen_trabajadores$ypc)

########
## e) ##
########

## Hombres
beta_hombres <- beta(Casen_trabajadores$esc[Casen_trabajadores$sexo == 0],
                     Casen_trabajadores$log_ypc[Casen_trabajadores$sexo == 0])

alpha_hombres <- alpha(Casen_trabajadores$esc[Casen_trabajadores$sexo == 0],
                       Casen_trabajadores$log_ypc[Casen_trabajadores$sexo == 0])

## Mujeres
beta_mujeres <- beta(Casen_trabajadores$esc[Casen_trabajadores$sexo == 1],
                     Casen_trabajadores$log_ypc[Casen_trabajadores$sexo == 1])

alpha_mujeres <- alpha(Casen_trabajadores$esc[Casen_trabajadores$sexo == 1],
                       Casen_trabajadores$log_ypc[Casen_trabajadores$sexo == 1])

## Resultados
beta_hombres
alpha_hombres
beta_mujeres
alpha_mujeres

## Brecha en intercepto (log-ingreso a igual escolaridad)
alpha_gap <- alpha_mujeres - alpha_hombres

## Brecha en pendiente (retorno a la escolaridad)
beta_gap <- beta_mujeres - beta_hombres

alpha_gap
beta_gap


########
## f) ##
########

# Creamos las bdd de hombres
Casen_th <- Casen_trabajadores[Casen_trabajadores$sexo == 0, ]

# Agrupar por escolaridad y calcular el log del salario promedio para hombres
Casen_promedio_h <- aggregate(log(ypc) ~ esc, data = Casen_th, FUN = mean)
colnames(Casen_promedio_h) <- c("esc", "log_salario_prom")

# Cálculo de la regresión lineal para hombres
beta_log_h1 <- beta(Casen_promedio_h$esc, Casen_promedio_h$log_salario_prom)
alpha_log_h1 <- alpha(Casen_promedio_h$esc, Casen_promedio_h$log_salario_prom)

# Gráfico para Hombres
plot(Casen_promedio_h$esc, Casen_promedio_h$log_salario_prom, 
     pch = 16, col = "skyblue",
     xlab = "Años de Escolaridad", 
     ylab = "Log(Salario Promedio)",
     main = "Relación entre Escolaridad y Log(Salario Promedio) - Hombres")
abline(alpha_log_h1, beta_log_h1, col = "red", lwd = 2)


########
## g) ##
########

set.seed(2563)
N <- 1000
n <- 100
betas_boot <- numeric(N)

for (i in 1:N) {
  muestra <- Casen_trabajadores[sample(1:nrow(Casen_trabajadores), n, replace = TRUE), ]
  betas_boot[i] <- beta(muestra$esc, muestra$log_ypc)
}

# Coeficiente con toda la bdd
beta_full <- beta(Casen_trabajadores$esc, Casen_trabajadores$log_ypc)
beta_boot_mean <- mean(betas_boot)

# Evolución del promedio acumulado
prom_acum <- cumsum(betas_boot) / seq_along(betas_boot)

plot(prom_acum, type = "l", lwd = 2, col = "darkgreen",
     xlab = "Número de simulaciones",
     ylab = "Promedio acumulado",
     main = "Evolución del estimador Beta (bootstrap)")
abline(h = beta_full, col = "red", lwd = 2, lty = 2)
abline(h = beta_boot_mean, col = "blue", lwd = 2, lty = 3)
legend("bottomright",
       legend = c("Promedio acumulado", "Beta (toda la base)", "Media bootstrap"),
       col = c("darkgreen","red","blue"), lwd = 2, lty = c(1,2,3), bty = "n")


# Histograma en escala de densidad
hist(betas_boot, freq = FALSE,
     main = "Distribución del coeficiente estimado (Bootstrap)",
     xlab = "Beta estimado", col = "lightblue", border = "white")

# Densidad kernel
dens_beta <- density(betas_boot)
lines(dens_beta, lwd = 2, col = "darkblue")

# Líneas de referencia
abline(v = beta_full, col = "red", lwd = 2, lty = 2)
abline(v = beta_boot_mean, col = "blue", lwd = 2, lty = 3)
abline(v = median(betas_boot), col = "darkorange", lwd = 2, lty = 4)

legend("topright",
       legend = c("Densidad kernel", "Beta (original)", "Media bootstrap", "Mediana bootstrap"),
       col = c("darkblue","red","blue","darkorange"),
       lwd = 2, lty = c(1,2,3,4), bty = "n")

