########################
##  TAREA 1 - Script  ##
########################

#################
##  Librería   ##
#################

library(rio)
library(dplyr)
library(ggplot2)
library(stargazer)


################
## Preliminar ##
################

# Importamos los datos
data <- import("salarios_transparencia.dta")

# Creamos variable binaria para sexo
# 1 -> Hombre
# 0 -> Mujer
data$male <- ifelse(data$sexo == "masculino", 1, 0)

# Vemos el nombre de las variables
colnames(data)

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

################
## Pregunta 1 ##
################

# Filtramos para el mes de Mayo
data_mayo <- data[data$mes == 5, ]


## Contar número de funcionarios por dependencia
conteo <- table(data_mayo$padre_org)
conteo_ordenado <- sort(conteo, decreasing = TRUE)

## Seleccionar top 10 dependencias
top10_dependencias <- names(conteo_ordenado[1:10])

## Filtrar data_mayo para top 10
data_top10 <- data_mayo[data_mayo$padre_org %in% top10_dependencias, ]

## Calcular número de funcionarios y salario promedio
n_func <- tapply(data_top10$rem_mes_inf, data_top10$padre_org, length)
salario_prom <- tapply(data_top10$rem_mes_inf, data_top10$padre_org, mean, na.rm = TRUE)

## Combinar en un data.frame
res_p1 <- data.frame(
  padre_org = names(n_func),
  n_func = as.vector(n_func),
  salario_prom = as.vector(salario_prom),
  stringsAsFactors = FALSE
)

## Ordenar por número de funcionarios
res_p1 <- res_p1[order(res_p1$n_func, decreasing = TRUE), ]
res_p1

################
## Pregunta 2 ##
################

#########
## 2.a ##
#########

prom_h <- mean(data_mayo$rem_mes_inf[data_mayo$male == 1], na.rm = TRUE)
prom_m <- mean(data_mayo$rem_mes_inf[data_mayo$male == 0], na.rm = TRUE)

# Diferencia absoluta H - M
dif_abs <- prom_h - prom_m

# Mostrar resultados
cat("Promedio salarial Hombres:", prom_h, "\n")
cat("Promedio salarial Mujeres:", prom_m, "\n")
cat("Diferencia salarial entre hombres y mujeres en mayo:", dif_abs, "\n")

# Esta pregunta se podría hacer con una regresión lineal también

#########
## 2.b ##
#########

# Filtrar mayo, salario > 0 para poder usar log y que 
# ambos modelos sean comparables en relación al número de obs
data_mayo <- data_mayo[data_mayo$rem_mes_inf > 0, ]


# Modelo lineal-lineal brecha salarial
x_2.a <- data_mayo$male
y_2.a <- data_mayo$rem_mes_inf

b_2.a <- beta(x_2.a, y_2.a)
a_2.a <- alpha(x_2.a, y_2.a)
brecha_salarial_a <- b_2.a / a_2.a

cat("Intercepto (α):", a_2.a, "\n")
cat("Coeficiente (β):", b_2.a, "\n")
cat("Brecha porcentual (β/α) modelo lineal-lineal:", brecha_salarial_a, "\n")


#########
## 2.c ##
#########

# Modelo log-lineal brecha salarial

x_2.b <- data_mayo$male
y_2.b <- log(data_mayo$rem_mes_inf)

b_2.b <- beta(x_2.b, y_2.b)

cat("Coeficiente (β) modelo log-lineal:", b_2.b, "\n")

################
## Pregunta 3 ##
################

# Separar salarios por sexo
sal_h <- data_mayo$rem_mes_inf[data_mayo$male == 1]
sal_m <- data_mayo$rem_mes_inf[data_mayo$male == 0]

# Calcular densidades
dens_h <- density(sal_h, na.rm = TRUE)
dens_m <- density(sal_m, na.rm = TRUE)

# Determinar límites
xmax <- max(dens_h$x, dens_m$x, na.rm = TRUE) * 1.1  # 10% más
ymax <- max(dens_h$y, dens_m$y, na.rm = TRUE) * 1.1

# Graficar
plot(dens_h, col = "blue", lwd = 2,
     main = "Distribución de salarios - Hombres vs Mujeres (Mayo)",
     xlab = "Salario mensual informado",
     ylab = "Densidad", xlim = c(0, 10^7),
     ylim = c(0, ymax))
lines(dens_m, col = "red", lwd = 2)
legend("topright", legend = c("Hombres", "Mujeres"),
       col = c("blue", "red"), lwd = 2)

# Limité el eje x porque sino la figura se ve muy mal dados los outliers

################
## Pregunta 4 ##
################


#########
## 4.a ##
#########

# Filtramos para mes de mayo y sectores Salud / Educación
data_mayo_SE <- data %>%
  filter(
    mes == 5,
    rem_mes_inf > 0,
    padre_org %in% c("Salud", "Educación")
  )

# Creamos variable binaria para sector Salud / Educación
# 1 -> Salud
# 0 -> Educación
data_mayo_SE$salud <- ifelse(data_mayo_SE$padre_org == "Salud", 1, 0)

# Modelo log-lineal diferencia porcentual de salarios 
# para sector Salud / Educación

x_4.a <- data_mayo_SE$salud
y_4.a <- log(data_mayo_SE$rem_mes_inf)

a_4.a <- alpha(x_4.a, y_4.a)
b_4.a <- beta(x_4.a, y_4.a)

cat("Beta (brecha porcentual Salud - Educación en pesos):", b_4.a, "\n")


#########
## 4.b ##
#########

# Filtramos para personas con un ingreso menor a 5MM
data_mayo_SE_filtro <- data_mayo_SE[data_mayo_SE$rem_mes_inf<5000000, ]

# Modelo log-lineal diferencia porcentual de salarios 
# para sector Salud / Educación

x_4.b <- data_mayo_SE_filtro$salud
y_4.b <- log(data_mayo_SE_filtro$rem_mes_inf)

a_4.b <- alpha(x_4.b, y_4.b)
b_4.b <- beta(x_4.b, y_4.b)

cat("Beta (brecha porcentual Salud - Educación en pesos con ingresos menores a 5000000):", b_4.b, "\n")

#########
## 4.c ##
#########

# Separar salarios por sector
salud_sal <- data_mayo_SE$rem_mes_inf[data_mayo_SE$padre_org == "Salud"]
educ_sal  <- data_mayo_SE$rem_mes_inf[data_mayo_SE$padre_org == "Educación"]

# Calcular densidades
dens_salud <- density(salud_sal, na.rm = TRUE)
dens_educ  <- density(educ_sal, na.rm = TRUE)

# Determinar límites más amplios
xmax_se <- max(dens_salud$x, dens_educ$x, na.rm = TRUE) * 1.1
ymax_se <- max(dens_salud$y, dens_educ$y, na.rm = TRUE) * 1.1

# Graficar
plot(dens_salud, col = "blue", lwd = 2,
     main = "Distribución de salarios - Salud vs Educación (Mayo)",
     xlab = "Salario mensual informado",
     ylab = "Densidad",
     xlim = c(0, 1e7),
     ylim = c(0, ymax_se))
lines(dens_educ, col = "red", lwd = 2)
legend("topright", legend = c("Salud", "Educación"),
       col = c("blue", "red"), lwd = 2)

# Limite el eje x porque sino la figura se ve muy mal dados los outliers


#########
## 4.d ##
#########

# Definir secuencia de topes salariales (desde un valor bajo hasta el máximo)
topes <- seq(300000, 25*10^6, length.out = 50)

# Calcular brecha porcentual para cada tope
resultados <- data.frame()

for (cap in topes) {
  datos_cap <- subset(data_mayo_SE, rem_mes_inf <= cap)
  
  x_4.d <- datos_cap$salud
  y_4.d <- log(datos_cap$rem_mes_inf)
  b_4.d <- beta(x_4.d, y_4.d)
  n_obs <- nrow(datos_cap)
  
  resultados <- rbind(resultados,
                      data.frame(Tope = cap,
                                 Brecha = b_4.d,
                                 N = n_obs))
}

min_brecha <- min(resultados$Brecha)
max_brecha <- max(resultados$Brecha)

min_N <- min(resultados$N)
max_N <- max(resultados$N)
# Factor para normalizar y dejar eje y derecho con el número de observaciones
factor <- (max_brecha - min_brecha) / (max_N - min_N)

ggplot(resultados, aes(x = Tope)) +
  geom_line(aes(y = Brecha), color = "blue", size = 1) +
  geom_point(aes(y = Brecha), color = "blue", size = 2) +
  geom_line(aes(y = (N - min_N) * factor + min_brecha), 
            color = "red", linetype = "dashed", size = 1) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray40") +
  scale_y_continuous(
    name = "Brecha porcentual (%)",
    sec.axis = sec_axis(~ (. - min_brecha) / factor + min_N,
                        name = "Número de observaciones")
  ) +
  labs(
    title = "Brecha porcentual y número de observaciones según tope salarial",
    x = "Tope máximo de salario incluido (CLP)"
  ) +
  scale_x_continuous(labels = scales::label_number(big.mark = ".", decimal.mark = ",")) +
  theme_minimal(base_size = 14)

# Rojo: Número de observaciones
# Azul: Brecha porcentual


################
## Pregunta 5 ##
################

#########
## 5.a ##
#########

data_f <- data[data$rem_mes_inf > 0, ]

x_5.a <- data_f$male
y_5.a <- data_f$n_hrs_ext_tot

a_5.a <- alpha(x_5.a, y_5.a)
b_5.a <- beta(x_5.a, y_5.a)

a_5.a
b_5.a

### Con outliers
# Mujeres -> 97.63 hrs extra totales promedio
# Hombres -> 97.63 + 103.97 = 201.6 hrs extra totales promedio


#########
## 5.b ##
#########

# Percentil 99
p99 <- quantile(data$n_hrs_ext_tot, 0.99, na.rm = TRUE) 

# El límite escogido para que no sea un outlier un dato, 
# es que las horas extras totales sean menores a 120 hrs

# Filtrar bdd
data_sin_outliers <- subset(data, n_hrs_ext_tot <= p99)
data_outliers <- subset(data, n_hrs_ext_tot > p99)

# Considerando la bdd sin outliers

x_5.a.1 <- data_sin_outliers$male
y_5.a.1 <- data_sin_outliers$n_hrs_ext_tot

a_5.a.1 <- alpha(x_5.a.1, y_5.a.1)
b_5.a.1 <- beta(x_5.a.1, y_5.a.1)

a_5.a.1
b_5.a.1

# Considerando valores mayores o iguales al percentil

x_5.a.2 <- data_outliers$male
y_5.a.2 <- data_outliers$n_hrs_ext_tot

a_5.a.2 <- alpha(x_5.a.2, y_5.a.2)
b_5.a.2 <- beta(x_5.a.2, y_5.a.2)

a_5.a.2
b_5.a.2


# En esta pregunta es se pueden identificar los outliers de diferentes formas.
# lo importante es que se aprecie este filtro una vez usado un modelo 
# de regresión lineal. Se aprecia en el orden de magnitud que se ve en las 
# horas extra totales promedio de las mujeres y en la brecha con los hombres.

### Sin outliers
# Mujeres -> 6.63596 hrs extra totales promedio
# Hombres -> 6.63596 + 1.22438 = 7.86034 hrs extra totales promedio

### Bbd con valores superiores al percentil 99:
# Mujeres -> 10578 hrs extra totales promedio
# Hombres -> 10578 + 5406 = 15984 hrs extra totales promedio


#########
## 5.c ##
#########

# lista con los resultados
resultados <- data.frame()

# Iterar sobre los meses (1 a 12)
for (m in 1:12) {
  datos_mes <- subset(data, mes == m)
  
  x <- datos_mes$male
  y <- datos_mes$n_hrs_ext_tot
  
  # estimar coeficiente
  b <- beta(x, y)
  
  resultados <- rbind(resultados,
                      data.frame(
                        mes = m,
                        estimate = b
                      ))
}

ggplot(resultados, aes(x = mes, y = estimate)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray40") +
  geom_point(color = "red", size = 2) +
  labs(
    title = "Brecha de género en horas extra solicitadas por mes",
    subtitle = "Coeficiente de male en regresiones mensuales",
    x = "Mes",
    y = "Coeficiente de male"
  ) +
  scale_x_continuous(breaks = 1:12,
                     labels = c("Ene","Feb","Mar","Abr","May","Jun",
                                "Jul","Ago","Sep","Oct","Nov","Dic")) +
  theme_minimal()



#########
## 5.d ##
#########


### Con outliers

set.seed(2563) # reproducibilidad

data <- data %>%
  select(n_hrs_ext_tot, male, mes)

# lista con los resultados
resultados <- data.frame()

# Iterar 1000 veces
for (i in 1:1000) {
  for (m in 1:12) {
    datos_mes <- filter(data, mes == m)
    muestra <- datos_mes %>% sample_n(500)
    x <- muestra$male
    y <- muestra$n_hrs_ext_tot
    # calculo coeficiente
    beta_male <- beta(x, y)
    resultados <- rbind(resultados,
                        data.frame(
                          iter = i,
                          mes = m,
                          beta = beta_male
                        ))
  }
}

# Calcular estadísticos por mes
resumen <- resultados %>%
  group_by(mes) %>%
  summarise(
    beta_mean = mean(beta, na.rm = TRUE),
    beta_p2.5 = quantile(beta, 0.025, na.rm = TRUE),
    beta_p97.5 = quantile(beta, 0.975, na.rm = TRUE),
    .groups = "drop"
  )

# Graficar
ggplot(resumen, aes(x = mes, y = beta_mean)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray40") +
  geom_point(color = "red", size = 2) +
  geom_errorbar(aes(ymin = beta_p2.5, ymax = beta_p97.5), width = 0.2, color = "blue") +
  labs(
    title = "Brecha de género en horas extra solicitadas por mes",
    subtitle = "Promedio de coeficientes con IC 95% (1000 iteraciones, n=500 por mes)",
    x = "Mes",
    y = "Coeficiente de male"
  ) +
  scale_x_continuous(breaks = 1:12,
                     labels = c("Ene","Feb","Mar","Abr","May","Jun",
                                "Jul","Ago","Sep","Oct","Nov","Dic")) +
  theme_minimal()

#########
## 5.e ##
#########

### Sin outliers

set.seed(2563) # reproducibilidad

data_sin_outliers <- data_sin_outliers %>%
  select(n_hrs_ext_tot, male, mes)

# lista con los resultados
resultados <- data.frame()

# Iterar 1000 veces
for (i in 1:1000) {
  for (m in 1:12) {
    datos_mes <- filter(data_sin_outliers, mes == m)
    muestra <- datos_mes %>% sample_n(500)
    x <- muestra$male
    y <- muestra$n_hrs_ext_tot
    # calculo coeficiente
    beta_male <- beta(x, y)
    resultados <- rbind(resultados,
                        data.frame(
                          iter = i,
                          mes = m,
                          beta = beta_male
                        ))
  }
}


# Calcular estadísticos por mes
resumen <- resultados %>%
  group_by(mes) %>%
  summarise(
    beta_mean = mean(beta, na.rm = TRUE),
    beta_p2.5 = quantile(beta, 0.025, na.rm = TRUE),
    beta_p97.5 = quantile(beta, 0.975, na.rm = TRUE),
    .groups = "drop"
  )


# Graficar
ggplot(resumen, aes(x = mes, y = beta_mean)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray40") +
  geom_point(color = "red", size = 2) +
  geom_errorbar(aes(ymin = beta_p2.5, ymax = beta_p97.5), width = 0.2, color = "blue") +
  labs(
    title = "Brecha de género en horas extra solicitadas por mes",
    subtitle = "Promedio de coeficientes con IC 95% (1000 iteraciones, n=500 por mes)",
    x = "Mes",
    y = "Coeficiente de male"
  ) +
  scale_x_continuous(breaks = 1:12,
                     labels = c("Ene","Feb","Mar","Abr","May","Jun",
                                "Jul","Ago","Sep","Oct","Nov","Dic")) +
  theme_minimal()
