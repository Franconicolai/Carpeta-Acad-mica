########################
##  TAREA 2 - Script  ##
########################

#################
##  Librería   ##
#################

library(rio)
library(dplyr)
library(ggplot2)
library(stargazer)
library(reshape2)
library(dplyr)
library(tidyr)
library(haven)
library(purrr)


#################
##  Funciones  ##
#################

estimador_beta = function(y, X1){
  beta1 = solve(t(X1)%*%X1)%*%t(X1)%*%y
  return(beta1)
}



R2 = function(log_ypc, X, beta_0) {
  
  e_0 <- log_ypc - X %*% beta_0
  y_mean <- mean(log_ypc)
  R2 <- 1 - ((t(e_0) %*% e_0) / (t(log_ypc - y_mean) %*% (log_ypc - y_mean)))
  return(R2)
}



##################
##  Preliminar  ##
##################

data <- import("salarios_transparencia.dta")

# creamos variables

data$rm = ifelse(data$reg_metro == "RM", 1, 0)
data$mujer = ifelse(data$sexo == "femenino", 1, 0)
data$hombre = ifelse(data$sexo != "femenino", 1, 0)
data$salud = ifelse(data$institucion_salud == "Salud", 1, 0)
data$profesional = ifelse(data$estamento == "Profesional", 1, 0)
data$planta = ifelse(data$calidad_juridica == "planta", 1, 0)
data_completa <- data
# filtramos para agosto 2024
data = data[data$year == 2024 & data$mes == 8,]



# horas extras  siempre es informado

### PREGUNTA 1 ###


### a ###
#mujeres vs hombres
ggplot(data, aes(x = pago_hrs_ext_tot_inf)) +
  geom_histogram(data = subset(data, mujer == 1),
                 aes(fill = "Mujeres"),
                 binwidth = 5000, alpha = 1, position = "identity", color = "black") +
  geom_histogram(data = subset(data, mujer == 0),
                 aes(fill = "Hombres"),
                 binwidth = 5000, alpha = 0.8, position = "identity") +
  scale_fill_manual(values = c("Hombres" = "lightblue", "Mujeres" = "pink")) +
  scale_x_continuous(limits = c(0, 1000000)) +
  scale_y_continuous(limits = c(0, 200)) +
  labs(x = "Pago horas extra", y = "Frecuencia", fill = "Sexo") +
  theme_minimal()

# salud vs resto
ggplot(data, aes(x = pago_hrs_ext_tot_inf)) +
  geom_histogram(data = subset(data, salud == 1),
                 aes(fill = "Salud"),
                 binwidth = 5000, alpha = 1, position = "identity", color = "black") +
  geom_histogram(data = subset(data, salud == 0),
                 aes(fill = "Resto"),
                 binwidth = 5000, alpha = 0.8, position = "identity") +
  scale_fill_manual(values = c("Salud" = "lightblue", "Resto" = "pink")) +
  scale_x_continuous(limits = c(0, 1000000)) +
  scale_y_continuous(limits = c(0, 200)) +
  labs(x = "Pago horas extra", y = "Frecuencia", fill = "Institucion") +
  theme_minimal()


# Profesional vs resto
ggplot(data, aes(x = pago_hrs_ext_tot_inf)) +
  geom_histogram(data = subset(data, profesional == 0),
                 aes(fill = "Resto"),
                 binwidth = 5000, alpha = 0.8, position = "identity") +
  geom_histogram(data = subset(data, profesional == 1),
                 aes(fill = "profesional"),
                 binwidth = 5000, alpha = 1, position = "identity", color = "black") +
  scale_fill_manual(values = c("profesional" = "lightblue", "Resto" = "pink")) +
  scale_x_continuous(limits = c(0, 1000000)) +
  scale_y_continuous(limits = c(0, 200)) +
  labs(x = "Pago horas extra", y = "Frecuencia", fill = "Institucion") +
  theme_minimal()


# planta vs resto
ggplot(data, aes(x = pago_hrs_ext_tot_inf)) +
  geom_histogram(data = subset(data, planta == 0),
                 aes(fill = "Resto"),
                 binwidth = 5000, alpha = 0.8, position = "identity") +
  geom_histogram(data = subset(data, planta == 1),
                 aes(fill = "planta"),
                 binwidth = 5000, alpha = 1, position = "identity", color = "black") +
  scale_fill_manual(values = c("planta" = "lightblue", "Resto" = "pink")) +
  scale_x_continuous(limits = c(0, 1000000)) +
  scale_y_continuous(limits = c(0, 200)) +
  labs(x = "Pago horas extra", y = "Frecuencia", fill = "Institucion") +
  theme_minimal()




## B ##

#Calculamos con álgebra-matricial

#creamos la interacción entre los del área de la salud y mujeres
salud_mujer = data$salud*data$mujer

#generamos la matriz de los X, considerando el intercepto y los Y
X = cbind(1, salud_mujer, data$salud, data$mujer)
Y = data$pago_hrs_ext_tot_inf

#utilizamos la función que está al inicio del script
estimador_beta(Y,X)

X = 0
# también se podría haber hecho para hombres
salud_hombre = data$salud*data$hombre
X = cbind(1, salud_hombre, data$salud, data$hombre)
Y = data$pago_hrs_ext_tot_inf

estimador_beta(Y,X)

## C ##
# para el caso del modelo con mujer corresponde al coeficiente asociado a salud
# para el caso del modelo con hombre corresponde a salud + salud*hombre
## D ##

# importante notar que en está pregunta para evitar colinealidad perfecta y que la matriz sea no singular
# se eliminan dos columnas, Salud y otra al azar, en este caso Vivienda y organismo




# Asegurar que la variable sea factor
padre_org <- factor(data$padre_org)

# Crear matriz de dummies
dummies_df <- model.matrix(~ padre_org-1 )

# Renombrar columnas con prefijo padre_org_
colnames(dummies_df) <- paste0("padre_org_", levels(padre_org))

# Actualizar base de datos
data <- cbind(data, dummies_df)

matriz_padre_org <- as.matrix( select(data, starts_with("padre_org_")) )
# Eliminamos Salud por ya estar en la bdd y también Vivienda y urbanismo para evitar colinealidad perfecta
matriz_padre_org <- matriz_padre_org[, !colnames(matriz_padre_org) %in% c("padre_org_Salud", "padre_org_Vivienda y Urbanismo")]


# estimamos
modelo_1D = estimador_beta(data$pago_hrs_ext_tot_inf, cbind(1,salud_mujer, data$salud, data$mujer, matriz_padre_org))
modelo_1D

### PREGUNTA 2 ###



## A ##
#Se generam las matrices X e Y
X = cbind(1, data$rem_mes_inf)
Y = data$pago_hrs_ext_tot_inf

estimador_beta(Y,X)

## B ##
#Se generam las matrices X e Y, sin el vector de 1
X = cbind( data$rem_mes_inf)
Y = data$pago_hrs_ext_tot_inf

estimador_beta(Y,X)



## C ##
#Generamos la interacción entre mujuer y rem_mes_inf

rem_mes_inf_mujer = data$rem_mes_inf*data$mujer

#ahora se generan las matrices

X = cbind(1, data$rem_mes_inf, data$mujer, rem_mes_inf_mujer)
Y= data$pago_hrs_ext_tot_inf

estimador_beta(Y,X)

## D ##

#ahora se generan las matrices

#Modelo 1
X = cbind(1, data$rem_mes_inf, data$mujer,  rem_mes_inf_mujer, data$profesional)
Y= data$pago_hrs_ext_tot_inf
estimador_beta(Y,X)
coef = estimador_beta(Y,X)
R2(Y, X, coef)


#Modelo 2
X = cbind(1, data$rem_mes_inf, data$mujer, rem_mes_inf_mujer, data$profesional,data$planta)
Y = data$pago_hrs_ext_tot_inf
estimador_beta(Y,X)
coef = estimador_beta(Y,X)
R2(Y, X, coef)


#Modelo 3


summary(lm(pago_hrs_ext_tot_inf ~ mujer + mujer*rem_mes_inf + rem_mes_inf + planta + factor(padre_org), data = data))

## E ##


### PREGUNTA 3 ###

## A ##

#Generamos las matrices X e Y
X = cbind(1, data$rem_mes_inf, data$mujer, data$planta, data$profesional, data$salud)
Y= data$pago_hrs_ext_tot_inf
coef = estimador_beta(Y,X)

## B ##


# usamos FWL
X = cbind(1, data$rem_mes_inf, data$mujer, data$salud, data$profesional, data$planta)
Y = data$pago_hrs_ext_tot_inf
modelo_3b = estimador_beta(Y,X)
modelo_3b

# modelo restringido 

# usamos FWL
X_rest = cbind(1, data$rem_mes_inf, data$mujer, data$salud)
Y_rest = data$pago_hrs_ext_tot_inf
coef_rest = estimador_beta(Y_rest,X_rest)
predict_restringido = X_rest%*%coef_rest
e_restringido =  data$pago_hrs_ext_tot_inf - predict_restringido


#modelo_profesional = lm(profesional ~ rem_mes_inf + mujer + salud, data = data)
X_prof = cbind(1, data$rem_mes_inf, data$mujer, data$salud)
Y_prof = data$profesional
coef_prof = estimador_beta(Y_prof,X_prof)
predict_prof = X_prof%*%coef_prof
e_prof =  data$profesional - predict_prof


#modelo_planta = lm(planta ~ rem_mes_inf + mujer + salud, data = data)
X_planta = cbind(1, data$rem_mes_inf, data$mujer, data$salud)
Y_planta = data$planta
coef_planta = estimador_beta(Y_planta,X_planta)
predict_planta = X_planta%*%coef_planta
e_planta =  data$planta - predict_planta

#modelo_fwl = lm(e_restringido ~ e_profesional + e_planta)
X_fwl = cbind(1, e_prof, e_planta)
Y_fwl = e_restringido
coef_fwl = estimador_beta(Y_fwl,X_fwl)
coef_fwl


## C ##


#R^2 original
R2(Y, X, coef)

sum((Y - X%*%coef)^2)
(Y - X%*%coef)


#R^2 particionado
R2(Y_rest, X_rest, coef_rest) + R2(Y_fwl, X_fwl, coef_fwl)
R2(Y_fwl, X_fwl, coef_fwl)





## D ##
X_d = cbind(1, data$rem_mes_inf, data$mujer, data$salud)
Y_d= data$pago_hrs_ext_tot_inf
coef_d = estimador_beta(Y,X)
coef_d

#Calculo del R^2 sin profesional ni planta

R2(Y_d,X_d,coef_d)


### PREGUNTA 4 ###

#j=50
data_coeficiente_agosto_50 <- data_frame(N_iteracion = numeric(), beta_media = numeric(), rcuadrado_media = numeric(), tamano_muestra = numeric())
i = 200
n = 1
while(i < nrow(data) ) {
  
  data_temporal <- data_frame(N_iteracion = numeric(), beta = numeric(), rcuadrado = numeric())
  
  for (j in 1:50){
    data_sample <- sample_n(data, i)
    
    data_temporal[j,1] <- j
    data_temporal[j,2] <- estimador_beta(
      data_sample$pago_hrs_ext_tot_inf, 
      cbind(1, data_sample$rem_mes_inf, data_sample$mujer, data_sample$planta, data_sample$profesional, data_sample$salud)
    )[2]
    
    data_temporal[j,3] <- R2(
      data_sample$pago_hrs_ext_tot_inf, 
      cbind(1, data_sample$rem_mes_inf, data_sample$mujer, data_sample$planta, data_sample$profesional, data_sample$salud),
      estimador_beta(
        data_sample$pago_hrs_ext_tot_inf, 
        cbind(1, data_sample$rem_mes_inf, data_sample$mujer, data_sample$planta, data_sample$profesional, data_sample$salud)
      )
    )[1]
  }
  data_coeficiente_agosto_50[n,1] <- n
  data_coeficiente_agosto_50[n,2] <- mean(data_temporal$beta)
  data_coeficiente_agosto_50[n,3] <- mean(data_temporal$rcuadrado)
  data_coeficiente_agosto_50[n,4] <- i
  
  n = n + 1
  i = i + 200
}




#j=100
data_coeficiente_agosto_100 <- data_frame(N_iteracion = numeric(), beta_media = numeric(), rcuadrado_media = numeric(), tamano_muestra = numeric())
i = 200
n = 1
while(i < nrow(data) ) {
  
  data_temporal <- data_frame(N_iteracion = numeric(), beta = numeric(), rcuadrado = numeric())
  
  for (j in 1:100){
    data_sample <- sample_n(data, i)
    
    data_temporal[j,1] <- j
    data_temporal[j,2] <- estimador_beta(
      data_sample$pago_hrs_ext_tot_inf, 
      cbind(1, data_sample$rem_mes_inf, data_sample$mujer, data_sample$planta, data_sample$profesional, data_sample$salud)
    )[2]
    
    data_temporal[j,3] <- R2(
      data_sample$pago_hrs_ext_tot_inf, 
      cbind(1, data_sample$rem_mes_inf, data_sample$mujer, data_sample$planta, data_sample$profesional, data_sample$salud),
      estimador_beta(
        data_sample$pago_hrs_ext_tot_inf, 
        cbind(1, data_sample$rem_mes_inf, data_sample$mujer, data_sample$planta, data_sample$profesional, data_sample$salud)
      )
    )[1]
  }
  data_coeficiente_agosto_100[n,1] <- n
  data_coeficiente_agosto_100[n,2] <- mean(data_temporal$beta)
  data_coeficiente_agosto_100[n,3] <- mean(data_temporal$rcuadrado)
  data_coeficiente_agosto_100[n,4] <- i
  
  n = n + 1
  i = i + 200
}



#j=300
data_coeficiente_agosto_300 <- data_frame(N_iteracion = numeric(), beta_media = numeric(), rcuadrado_media = numeric(), tamano_muestra = numeric())
i = 200
n = 1
while(i < nrow(data) ) {
  
  data_temporal <- data_frame(N_iteracion = numeric(), beta = numeric(), rcuadrado = numeric())
  
  for (j in 1:300){
    data_sample <- sample_n(data, i)
    
    data_temporal[j,1] <- j
    data_temporal[j,2] <- estimador_beta(
      data_sample$pago_hrs_ext_tot_inf, 
      cbind(1, data_sample$rem_mes_inf, data_sample$mujer, data_sample$planta, data_sample$profesional, data_sample$salud)
    )[2]
    
    data_temporal[j,3] <- R2(
      data_sample$pago_hrs_ext_tot_inf, 
      cbind(1, data_sample$rem_mes_inf, data_sample$mujer, data_sample$planta, data_sample$profesional, data_sample$salud),
      estimador_beta(
        data_sample$pago_hrs_ext_tot_inf, 
        cbind(1, data_sample$rem_mes_inf, data_sample$mujer, data_sample$planta, data_sample$profesional, data_sample$salud)
      )
    )[1]
  }
  data_coeficiente_agosto_300[n,1] <- n
  data_coeficiente_agosto_300[n,2] <- mean(data_temporal$beta)
  data_coeficiente_agosto_300[n,3] <- mean(data_temporal$rcuadrado)
  data_coeficiente_agosto_300[n,4] <- i
  
  n = n + 1
  i = i + 200
}



# Ahora se busca los coeficientes para toda la base de datos.

data_coeficiente_50 <- data_frame(N_iteracion = numeric(), beta_media = numeric(), rcuadrado_media = numeric(), tamano_muestra = numeric())
i = 200
n = 1
while(i < nrow(data)) {
  data_temporal <- data_frame(N_iteracion = numeric(), beta = numeric(), rcuadrado = numeric())
  for (j in 1:50){
    data_sample <- sample_n(data_completa, i)
    data_temporal[j,1] <- j
    data_temporal[j,2] <- estimador_beta(
      data_sample$pago_hrs_ext_tot_inf, 
      cbind(1, data_sample$rem_mes_inf, data_sample$mujer, data_sample$planta, data_sample$profesional, data_sample$salud)
    )[2]
    
    data_temporal[j,3] <- R2(
      data_sample$pago_hrs_ext_tot_inf, 
      cbind(1, data_sample$rem_mes_inf, data_sample$mujer, data_sample$planta, data_sample$profesional, data_sample$salud),
      estimador_beta(
        data_sample$pago_hrs_ext_tot_inf, 
        cbind(1, data_sample$rem_mes_inf, data_sample$mujer, data_sample$planta, data_sample$profesional, data_sample$salud)
      )
    )[1]
  }
  data_coeficiente_50[n,1] <- n
  data_coeficiente_50[n,2] <- mean(data_temporal$beta)
  data_coeficiente_50[n,3] <- mean(data_temporal$rcuadrado)
  data_coeficiente_50[n,4] <- i
  n = n + 1
  i = i + 200
}


data_coeficiente_100 <- data_frame(N_iteracion = numeric(), beta_media = numeric(), rcuadrado_media = numeric(), tamano_muestra = numeric())
i = 200
n = 1
while(i < nrow(data)) {
  data_temporal <- data_frame(N_iteracion = numeric(), beta = numeric(), rcuadrado = numeric())
  for (j in 1:100){
    data_sample <- sample_n(data_completa, i)
    data_temporal[j,1] <- j
    data_temporal[j,2] <- estimador_beta(
      data_sample$pago_hrs_ext_tot_inf, 
      cbind(1, data_sample$rem_mes_inf, data_sample$mujer, data_sample$planta, data_sample$profesional, data_sample$salud)
    )[2]
    
    data_temporal[j,3] <- R2(
      data_sample$pago_hrs_ext_tot_inf, 
      cbind(1, data_sample$rem_mes_inf, data_sample$mujer, data_sample$planta, data_sample$profesional, data_sample$salud),
      estimador_beta(
        data_sample$pago_hrs_ext_tot_inf, 
        cbind(1, data_sample$rem_mes_inf, data_sample$mujer, data_sample$planta, data_sample$profesional, data_sample$salud)
      )
    )[1]
  }
  data_coeficiente_100[n,1] <- n
  data_coeficiente_100[n,2] <- mean(data_temporal$beta)
  data_coeficiente_100[n,3] <- mean(data_temporal$rcuadrado)
  data_coeficiente_100[n,4] <- i
  n = n + 1
  i = i + 200
}
lm(pago_hrs_ext_tot_inf ~ rem_mes_inf + mujer+ planta +profesional +salud, data = data )
lm(pago_hrs_ext_tot_inf ~ rem_mes_inf + mujer+ planta +profesional +salud, data = data_completa )

data_coeficiente_300 <- data_frame(N_iteracion = numeric(), beta_media = numeric(), rcuadrado_media = numeric(), tamano_muestra = numeric())
i = 200
n = 1
while(i < nrow(data)) {
  data_temporal <- data_frame(N_iteracion = numeric(), beta = numeric(), rcuadrado = numeric())
  for (j in 1:300){
    data_sample <- sample_n(data_completa, i)
    data_temporal[j,1] <- j
    data_temporal[j,2] <- estimador_beta(
      data_sample$pago_hrs_ext_tot_inf, 
      cbind(1, data_sample$rem_mes_inf, data_sample$mujer, data_sample$planta, data_sample$profesional, data_sample$salud)
    )[2]
    
    data_temporal[j,3] <- R2(
      data_sample$pago_hrs_ext_tot_inf, 
      cbind(1, data_sample$rem_mes_inf, data_sample$mujer, data_sample$planta, data_sample$profesional, data_sample$salud),
      estimador_beta(
        data_sample$pago_hrs_ext_tot_inf, 
        cbind(1, data_sample$rem_mes_inf, data_sample$mujer, data_sample$planta, data_sample$profesional, data_sample$salud)
      )
    )[1]
  }
  data_coeficiente_300[n,1] <- n
  data_coeficiente_300[n,2] <- mean(data_temporal$beta)
  data_coeficiente_300[n,3] <- mean(data_temporal$rcuadrado)
  data_coeficiente_300[n,4] <- i
  n = n + 1
  i = i + 200
}


library(ggplot2)

# Lista de tamaños
tamanos <- c(50, 100, 300)

# --- Calcular rangos comunes ---
# Extraemos min y max de beta y r2 en todas las bases
all_data <- do.call(rbind, lapply(tamanos, function(n) {
  df_name <- paste0("data_coeficiente_agosto_", n)
  df <- get(df_name)
  df
}))

ylim_beta <- range(all_data$beta_media, na.rm = TRUE)
ylim_r2   <- range(all_data$rcuadrado_media, na.rm = TRUE)

# --- Loop de gráficos ---
for (n in tamanos) {
  
  # Construir el nombre del dataset
  df_name <- paste0("data_coeficiente_agosto_", n)
  df <- get(df_name)
  
  # ---- Gráfico beta_media ----
  p_beta <- ggplot(df, aes(x = tamano_muestra, y = beta_media)) +
    geom_line(color = "blue", size = 1) +
    labs(
      title = paste("Evolución de beta_media en Agosto (muestra =", n, ")"),
      x = "Tamaño de muestra",
      y = "Beta media"
    ) +
    ylim(ylim_beta) +                 # <<< mismo eje Y
    theme_bw(base_size = 14) +
    theme(
      plot.title = element_text(size = 16, hjust = 0.5)
    )
  
  ggsave(paste0("beta_media_agosto_", n, ".png"), p_beta, width = 8, height = 5, bg = "white")
  
  # ---- Gráfico rcuadrado_media ----
  p_r2 <- ggplot(df, aes(x = tamano_muestra, y = rcuadrado_media)) +
    geom_line(color = "red", size = 1) +
    labs(
      title = paste("Evolución de R-cuadrado medio en Agosto (muestra =", n, ")"),
      x = "Tamaño de muestra",
      y = "R² medio"
    ) +
    ylim(ylim_r2) +                   # <<< mismo eje Y
    theme_bw(base_size = 14) +
    theme(
      plot.title = element_text(size = 16, hjust = 0.5)
    )
  
  ggsave(paste0("rcuadrado_media_agosto_", n, ".png"), p_r2, width = 8, height = 5, bg = "white")
}




# Casos a recorrer
tamanos <- c(50, 100, 300)

# ---- 1) Calcular un rango Y común (intervalos fijos) para beta_media en TODOS los casos ----
all_beta <- map_dfr(tamanos, function(n) {
  g <- get(paste0("data_coeficiente_", n)) %>% select(beta_media)
  a <- get(paste0("data_coeficiente_agosto_", n)) %>% select(beta_media)
  bind_rows(g, a)
})
ylim_beta <- range(all_beta$beta_media, na.rm = TRUE)

# ---- 2) Loop: combinar General vs. Agosto y graficar beta_media ----
for (n in tamanos) {
  df_g <- get(paste0("data_coeficiente_", n)) %>% mutate(periodo = "General")
  df_a <- get(paste0("data_coeficiente_agosto_", n)) %>% mutate(periodo = "Agosto")
  
  data_long <- bind_rows(df_g, df_a)
  
  # Número de iteraciones (según N_iteracion presentes)
  n_iter <- length(unique(data_long$N_iteracion))
  
  p <- ggplot(data_long, aes(x = N_iteracion, y = beta_media, color = periodo)) +
    geom_line(size = 1) +
    labs(
      title = paste0("Comparación de coeficientes (beta_media) — Muestra = ", n),
      x = "N_iteracion",
      y = "Beta media"
    ) +
    coord_cartesian(ylim = ylim_beta) +   # mismo eje Y en todos
    theme_bw(base_size = 14) +            # fondo blanco + cuadrilla
    theme(plot.title = element_text(hjust = 0.5))
  
  ggsave(paste0("comparacion_", n, ".png"),
         p, width = 8, height = 5, bg = "white")
}


