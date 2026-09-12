# --- Importación de módulos y base de datos -----------------------------------
library(haven)
install.packages("dplyr")
library(dplyr)
set.seed(123)
salarios_transparencia <- read_dta("salarios_transparencia_v2.0.dta") 
salarios_transparencia <- salarios_transparencia_v2_0
salarios_transparencia=salarios_transparencia[salarios_transparencia$mes==4,]
# --- Pregunta 1 ---------------------------------------------------------------
# --- Pregunta 1, ítem i -------------------------------------------------------
limpieza_seccion = ls()
objetos_a_limpiar = setdiff(limpieza_seccion, "salarios_transparencia")
rm(list = objetos_a_limpiar)
rm(limpieza_seccion)

gamma1 = 20000
gamma2 = 100
sigma_error = sqrt(10^5)

IAa = salarios_transparencia$IA
hrsa = salarios_transparencia$n_hrs_ext_tot

error = rnorm(length(IAa), mean = 0, sd = sigma_error)
salario = gamma1 * IAa + gamma2 * hrsa + error

f_estimador = function(numero_iteraciones, numero_muestra){
  
  beta1_estimados = numeric(numero_iteraciones)
  alpha1_estimados = numeric(numero_iteraciones)
  
  for (i in 1:numero_iteraciones) {
    idx = sample(1:length(IAa), size = numero_muestra, replace = FALSE)
    
    modelo_1 = lm(salario[idx] ~ IAa[idx])
    beta1_estimados[i] = coef(modelo_1)["IAa[idx]"]
    
    modelo_2 = lm(salario[idx] ~ IAa[idx] + hrsa[idx])
    alpha1_estimados[i] = coef(modelo_2)["IAa[idx]"]
    
  }
  return(list(beta1 = beta1_estimados, alpha1 = alpha1_estimados))
}

# --- Pregunta 1, ítem ii -------------------------------------------------------
numero_iteraciones = 1000
numero_muestra = 1000

resultados  = f_estimador(numero_iteraciones, numero_muestra)
beta1_estimados = resultados$beta1
alpha1_estimados = resultados$alpha1

# --- Pregunta 1, ítem iii -------------------------------------------------------
densidad_beta1 = density(beta1_estimados)
densidad_alpha1 = density(alpha1_estimados)

media_beta1 = mean(beta1_estimados)
media_alpha1 = mean(alpha1_estimados)

rango_x = range(c(densidad_beta1$x, densidad_alpha1$x))
rango_y = range(c(densidad_beta1$y, densidad_alpha1$y))

plot(
  densidad_beta1,
  main = "Distribución empírica de estimadores",
  xlab = "Valor del Coeficiente",
  ylab = "Densidad",
  xlim = rango_x,
  ylim = rango_y,
  col = "red",
  lwd = 2
)

lines(
  densidad_alpha1,
  col = "lightblue",
  lwd = 2
)

abline(v = media_beta1, col = "red", lty = 2, lwd = 2)
abline(v = media_alpha1, col = "lightblue", lty = 2, lwd = 2)

legend(
  "topright",
  legend = c("Beta 1", "Alpha 1", "Media beta","Media alpha"),
  col = c("red", "lightblue", "red", "lightblue"),
  lty = c(1, 1, 1, 2, 2),
  lwd = 2
)

# --- Pregunta 2 ---------------------------------------------------------------
# --- Pregunta 2, ítem i -------------------------------------------------------
limpieza_seccion = ls()
objetos_a_limpiar = setdiff(limpieza_seccion, "salarios_transparencia")
rm(list = objetos_a_limpiar)
rm(limpieza_seccion)

gamma1 = 20000
gamma2 = 100
sigma_error = sqrt(10^5)

IAa = salarios_transparencia$IA
hrsa = salarios_transparencia$n_hrs_ext_tot
x3i = salarios_transparencia$años_servicio

error = rnorm(length(IAa), mean = 0, sd = sigma_error)
salario = gamma1 * IAa + gamma2 * hrsa + error

f_estimador = function(numero_iteraciones, numero_muestra){
  
  beta1_estimados = numeric(numero_iteraciones)
  alpha1_estimados = numeric(numero_iteraciones)
  
  for (i in 1:numero_iteraciones) {
    idx = sample(1:length(IAa), size = numero_muestra, replace = FALSE)
    
    modelo_1 = lm(salario[idx] ~ IAa[idx] + hrsa[idx])
    beta1_estimados[i] = coef(modelo_1)["IAa[idx]"]
    
    modelo_2 = lm(salario[idx] ~ IAa[idx] + hrsa[idx] + x3i[idx])
    alpha1_estimados[i] = coef(modelo_2)["IAa[idx]"]
  }
  return(list(beta1 = beta1_estimados, alpha1 = alpha1_estimados))
}

# --- Pregunta 2, ítem ii -------------------------------------------------------
numero_iteraciones = 1000
numero_muestra = 1000

resultados  = f_estimador(numero_iteraciones, numero_muestra)
beta1_estimados = resultados$beta1
alpha1_estimados = resultados$alpha1

# --- Pregunta 2, ítem iii -------------------------------------------------------
densidad_beta1 = density(beta1_estimados)
densidad_alpha1 = density(alpha1_estimados)

media_beta1 = mean(beta1_estimados)
media_alpha1 = mean(alpha1_estimados)

rango_x = range(c(densidad_beta1$x, densidad_alpha1$x))
rango_y = range(c(densidad_beta1$y, densidad_alpha1$y))

plot(
  densidad_beta1,
  main = "Distribución empírica de estimadores",
  xlab = "Valor del Coeficiente",
  ylab = "Densidad",
  xlim = rango_x,
  ylim = rango_y,
  col = "red",
  lwd = 2
)

lines(
  densidad_alpha1,
  col = "lightblue",
  lwd = 2
)

abline(v = media_beta1, col = "red", lty = 2, lwd = 2)
abline(v = media_alpha1, col = "lightblue", lty = 2, lwd = 2)

legend(
  "topright",
  legend = c("Beta 1", "Alpha 1", "Media beta","Media alpha"),
  col = c("red", "lightblue", "red", "lightblue"),
  lty = c(1, 1, 1, 2, 2),
  lwd = 2
)

# --- Pregunta 3 ---------------------------------------------------------------
# --- Pregunta 3, ítem i -------------------------------------------------------
limpieza_seccion = ls()
objetos_a_limpiar = setdiff(limpieza_seccion, "salarios_transparencia")
rm(list = objetos_a_limpiar)
rm(limpieza_seccion)
gamma1 = 20000
gamma2 = 100
sigma_error = sqrt(10^5)

datos <- salarios_transparencia %>%
  rename(
    IAa = IA,
    hrsa = n_hrs_ext_tot
  ) %>%
  na.omit() %>%
  mutate(
    error = rnorm(n(), mean = 0, sd = sigma_error),
    salario = (gamma1 * IAa) + (gamma2 * hrsa) + error
  )
numero_submuestra = 1000
submuestra_a<-datos[sample(nrow(datos),numero_submuestra,replace=FALSE),]
IA_scale_a<-scale(submuestra_a$IAa)
hrs_scale_a<-scale(submuestra_a$hrsa)
Z_a<-matrix(NA,nrow=numero_submuestra,ncol = 15)
for (j in 1:15){
  residuo_a<-rnorm(numero_submuestra,0,1)
  zji_a<-0.3*IA_scale_a+0.3*hrs_scale_a+residuo_a
  #volver a estandarizar
  Z_a[,j]<-scale(zji_a)
}
z_dataframe_a<-as.data.frame(Z_a)
colnames(z_dataframe_a)<-paste0("z",1:15)
submuestra_conz_a<-cbind(submuestra_a,z_dataframe_a)
modelo1_a<-lm(salario~IAa+hrsa,data= submuestra_conz_a)
beta_1_p3_a<-coef(modelo1_a)["IAa"] #19999.8 
modelo2_a<-lm(salario~IAa+hrsa+Z_a,data = submuestra_conz_a)
alpha_1_z_p3_a<-coef(modelo2_a)["IAa"]#19999.58  
# --- Pregunta 3, ítem ii -------------------------------------------------------
limpieza_seccion = ls()
objetos_a_limpiar = setdiff(limpieza_seccion, "salarios_transparencia")
rm(list = objetos_a_limpiar)
rm(limpieza_seccion)

gamma1 = 20000
gamma2 = 100
sigma_error = sqrt(10^5)

datos <- salarios_transparencia %>%
  rename(
    IAa = IA,
    hrsa = n_hrs_ext_tot
  ) %>%
  na.omit() %>%
  mutate(
    error = rnorm(n(), mean = 0, sd = sigma_error),
    salario = (gamma1 * IAa) + (gamma2 * hrsa) + error
  )

numero_iteraciones=500
numero_submuestra = 1000

beta_1_p3<-numeric(numero_iteraciones)
alpha_1_z_p3<-numeric(numero_iteraciones)

#simulación:
for (i in 1:numero_iteraciones){
  submuestra<-datos[sample(nrow(datos),numero_submuestra,replace=FALSE),]
  #estandarizamos variables relevantes
  IA_scale<-scale(submuestra$IAa)
  hrs_scale<-scale(submuestra$hrsa)
  #15 variables z:
  Z<-matrix(NA,nrow=numero_submuestra,ncol = 15)
  for (j in 1:15){
    residuo<-rnorm(numero_submuestra,0,1)
    zji<-0.3*IA_scale+0.3*hrs_scale+residuo
    #volver a estandarizar
    Z[,j]<-scale(zji)
  }
  #convertimos a dataframe y combinamos con muestra:
  z_dataframe<-as.data.frame(Z)
  colnames(z_dataframe)<-paste0("z",1:15)
  submuestra_conz<-cbind(submuestra,z_dataframe)
  #estimación modelo sin z:
  modelo1<-lm(salario~IAa+hrsa,data= submuestra_conz)
  beta_1_p3[i]<-coef(modelo1)["IAa"]
  #estimación modelo con z:
  modelo2<-lm(salario~IAa+hrsa+Z,data = submuestra_conz)
  alpha_1_z_p3[i]<-coef(modelo2)["IAa"]
}

beta_1_p3
alpha_1_z_p3
# --- Pregunta 3, ítem iii -------------------------------------------------------
densidad_beta1 = density(beta_1_p3)
densidad_alpha1 = density(alpha_1_z_p3)

media_beta1 = mean(beta_1_p3) #19999.761
media_alpha1 = mean(alpha_1_z_p3) #19999.764

rango_x = range(c(densidad_beta1$x, densidad_alpha1$x))
rango_y = range(c(densidad_beta1$y, densidad_alpha1$y))

plot(
  densidad_beta1,
  main = "Distribución empírica de estimadores",
  xlab = "Valor del Coeficiente",
  ylab = "Densidad",
  xlim = rango_x,
  ylim = rango_y,
  col = "red",
  lwd = 2
)

lines(
  densidad_alpha1,
  col = "lightblue",
  lwd = 2
)

abline(v = media_beta1, col = "red", lty = 3, lwd =2 )
abline(v = media_alpha1, col = "lightblue", lty = 2, lwd = 2)

legend(
  "topright",
  legend = c("Beta 1", "Alpha 1", "Media beta","Media alpha"),
  col = c("red", "lightblue", "red", "lightblue"),
  lty = c(1, 1, 1, 3, 2),
  lwd = 2
)

# --- Pregunta 4 ---------------------------------------------------------------
# --- Pregunta 4, ítem i,ii y iii-------------------------------------------------------
limpieza_seccion = ls()
objetos_a_limpiar = setdiff(limpieza_seccion, "salarios_transparencia")
rm(list = objetos_a_limpiar)
rm(limpieza_seccion)
set.seed(1234)
#DGP original:
gamma1 = 20000
gamma2 = 100
sigma_error = sqrt(10^5)
datos <- salarios_transparencia %>%
  rename(
    IAa = IA,
    hrsa = n_hrs_ext_tot
  ) %>%
  na.omit() %>%
  mutate(
    error = rnorm(n(), mean = 0, sd = sigma_error),
    salario = (gamma1 * IAa) + (gamma2 * hrsa) + error
  )

S<-50000
n<-1000
N<-200
grilla_prophombres<-seq(0,1,by=0.05)
almacenar_beta1_prom<-numeric(length(grilla_prophombres))

#iteraciones sobre cada proporción p:
for (p in 1:length(grilla_prophombres)){
  prop<-grilla_prophombres[p]
  betahat_1<-numeric(N)
  #N repeticiones para p fijo:
  for (i in 1:N){
    #submuestra de tamaño 1000 del DGP original
    submuestra_it<-datos[sample(nrow(datos),n,replace=FALSE),]
    #salario base:
    #controlar aleatoriedad usando semilla diferente para cada iteración
    set.seed(i+p*N)
    submuestra_it<-submuestra_it%>%
      #queremos crear columnas de error, salario base y asignación del tratamiento D aleatorio
      mutate(error_orig=rnorm(n(),mean=0,sd=sigma_error),salario_base=(gamma1 * IAa) + (gamma2 * hrsa) + error_orig,D=rbinom(n(),1,0.5))
    #aplicar tratamiento D_i y definir S para esto, para obtener salario ajustado:
    submuestra_it<-submuestra_it%>%
      mutate(efecto_Di=ifelse(male==1,S*D,-S*D),salario_ajustado=salario_base+efecto_Di)
    m_hombres<-submuestra_it %>% filter(male == 1)
    m_mujeres<-submuestra_it %>% filter(male == 0)
    #seleccionamos aleatoriamnete hombres y mujeres para submuestra:
    subm_hombres<-round(prop*n)
    subm_mujeres<-(1-prop)*n
    subm_final_hombres<-m_hombres[sample(nrow(m_hombres),subm_hombres,replace=TRUE),] #replace=TRUE por si propo=0 o prop=1 y no hay suficiente muestra
    subm_final_mujeres<-m_mujeres[sample(nrow(m_mujeres),subm_mujeres,replace=TRUE),]
    
    #muestra para regresión:
    submuestra_regr<-rbind(subm_final_hombres,subm_final_mujeres)
    #estimamos modelo con salario ajustado:
    modelo_nuevo<-lm(salario_ajustado~D,data=submuestra_regr)
    #agregar valor estimado betahat_1:
    betahat_1[i]<-coef(modelo_nuevo)["D"]
  }
  #media de las estimaciones de betahat_1 al iterar:
  almacenar_beta1_prom[p]<-mean(betahat_1,na.rm=TRUE)
}

mean(almacenar_beta1_prom) #90.54968
# --- Pregunta 4, ítem a-------------------------------------------------------
plot(grilla_prophombres,almacenar_beta1_prom,type = "b",pch=16,col="red",
     xlab="Proporción de hombres (p)",ylab="Valor promedio del estimador β_hat_1",
     main="Estimador β_hat_1 vs. Proporción de Hombres en la Muestra (p)")
# --- Pregunta 4, ítem b-------------------------------------------------------
lines(grilla_prophombres, (2 * grilla_prophombres - 1) * S, lty = 2)
legend("topleft", legend = c("empírico", "teórico (2p-1)S"), col=c("red","black"),lty = c(1,2), bty = "n")






