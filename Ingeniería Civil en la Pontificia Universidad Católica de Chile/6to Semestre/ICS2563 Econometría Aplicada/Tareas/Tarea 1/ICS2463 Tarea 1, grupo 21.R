rm(list=ls())
install.packages("haven")
install.packages("data.table")
install.packages("dplyr")
library(dplyr)
library(haven)
library(data.table)

salarios_transparencia <- read_dta("Tareas/Tarea 1/salarios_transparencia.dta")
DT<-salarios_transparencia
head((DT))
names(DT)
#filtrar solo datos de mayo
DT_mayo<-subset(DT, mes==5)

##PREGUNTA 1:##
#frecuencias para las dependencias principales:
as.data.frame(table(DT_mayo$padre_org))
#desde los datos que se ven por el comando anterior, las 10 dependencias principales con mayor número de funcionarios:
#1: Salud (16959)
#2: Educación (5033)
#3: Obras Públicas (882)
#4: Justicia (858)
#5: Trabajo y Previsión Social (768)
#6: Hacienda (654)
#7: Defensa Nacional (648)
#8: Agricultura (623)
#9: Desarrollo Social (596)
#10: Vivienda y Urbanismo (586)

#filtrar los datos de antes, para que sólo incluya las 10 dependencias principales con mayor número de funcionarios
top_10<-DT_mayo %>% filter(padre_org=="Salud"|padre_org=="Educación"|padre_org=="Obras Públicas"|padre_org=="Justicia"|padre_org=="Trabajo y Previsión Social"|padre_org=="Hacienda"|padre_org=="Defensa Nacional"|padre_org=="Agricultura"|padre_org=="Desarrollo Social"|padre_org=="Vivienda y Urbanismo")

#calculamos el promedio de la variable de salarios para cada categoría (dependencias)
promedios_salario<-aggregate(rem_mes_inf ~ padre_org, data = top_10, FUN =mean)
#si se apreta promedios_salario en data se muestra la tabla ordenada de los resultados pedidos:
colnames(promedios_salario)<-c("Dependencia","Salario Promedio")

##PREGUNTA 2:##
DT<-salarios_transparencia
DT_mayo<-subset(DT, mes==5)

#inciso a#:
DT_mayo=DT_mayo[, c("sexo", "rem_mes_inf")]
DT_mayo_hombres<-subset(DT_mayo,sexo=="masculino")
DT_mayo_mujeres<-subset(DT_mayo,sexo=="femenino")

salarios_promedio_hombres=mean(DT_mayo_hombres$rem_mes_inf)
salarios_promedio_mujeres=mean(DT_mayo_mujeres$rem_mes_inf)

#diferencia salarial absoluta entre hombres y mujeres:
abs(salarios_promedio_hombres-salarios_promedio_mujeres)
#642753.1

#inciso b#:
#cambiaremos la forma de diferenciar hombres y mujeres con variables binarias--> 1 (hombres) y 0 (mujeres)
DT_mayo$sexo = ifelse(DT_mayo$sexo == "masculino", 1, 0)
DT_mayo <- DT_mayo[DT_mayo$rem_mes_inf != 0.00, ] # quitamos datos inconsistentes, o sea, rem_mes_inf igual a cero.
#queremos escoger los coeficientes (llamemos a estos beta) que minimicen la suma de cuadrados de los errores 
#considerando el modelo de regresión lineal: y= X*beta_1 + beta_0
#beta_0: promedio salarial mujeres
#beta_1: diferencia de promedio salarial entre hombres y mujeres
#una vez calculamos beta_0 y beta_1 se puede calcular la diferencia porcentual. Primero debemos calcular beta_hat

DT_mayo_hombres<-subset(DT_mayo, sexo=="1")
DT_mayo_mujeres<-subset(DT_mayo, sexo=="0")

b1 = mean(DT_mayo_mujeres$rem_mes_inf) -mean(DT_mayo_hombres$rem_mes_inf)
# b1 = -650045.3
b0 = mean(DT_mayo_hombres$rem_mes_inf)
# b0 = 2495889

#Así calculamos la diferencia porcentual respecto a las mujeres (con dos coeficientes): beta_1/beta_0*100
diferencia_porcentual_hym_dosc<- (b1/b0)*100 
# -26.044%

#inciso c#:
#usaremos un modelo log-lineal, ya que el log hace más "proporcional" la comparación (comprime valores altos)
#y considerando que ahora se debe plantear un modelo que estime la diferencia porcentual pedida en UN solo coeficiente, usaremos un modelo de regresion con la variable dependiente en logaritmos, así el cálculo final de la brecha se derive de una única operación entre coeficientes.
#este modelo no tiene intercepto, y cada cada coeficiente representa el salario logarítmico promedio para su respectivo grupo.
#La diferencia entre las medias logarítmicas es la resta de los coeficientes estimados: ln(salarioH)-ln(salarioM)=beta_H - beta_M --> por propedades: ln(salarioH/salarioM)=beta_H - beta_M --> salarioH/salarioM = e^(beta_H - beta_M).
#entonces e^(beta_H - beta_M) dice cúantas veces es el salario promedio de los hombres mayor al de las mujeres y así para que se pueda expresar como porcentaje de cambio al calcular la diferencia porcentual usamos la expresión: (e^(beta_H - beta_M) - 1)*100.

DT_mayo$log_rem_inf <- log(DT_mayo$rem_mes_inf)

DT_mayo_hombres<-subset(DT_mayo, sexo=="1")
DT_mayo_mujeres<-subset(DT_mayo, sexo=="0")

b1 = mean(DT_mayo_mujeres$log_rem_inf) -mean(DT_mayo_hombres$log_rem_inf)
# b1 = -0.2844106

diferencia_porcentual_hym_unc<- (exp(b1)-1)*100 #32.9%
# -24.75424%

#inciso d#:
#como se mencione antes el segundo modelo usado en el inciso c, es más preciso y menos sensible a sueldos muy altos, a diferencia de el primer modelo dle inciso b.
#Para el primer modelo la diferencia es de 35.2% y para el segundo es de 32.9%, ambas diferencias son similares y demuestran que la brecha salarial es relevante y a favor de los hombres, y no hay salarios tan asimétricos considerando que la diferencia entre los modelos no es muy alta.

##PREGUNTA 3:##
densidad_mujeres= density(DT_mayo_mujeres$rem_mes_inf)
densidad_hombres= density(DT_mayo_hombres$rem_mes_inf)
plot(densidad_mujeres, main= "Distribucion salarial en mayo: Hombre vs Mujeres", xlab = "Remuneración mensual (CLP)", ylab = "Densidad", lwd=3, col="purple")
lines(densidad_hombres, lwd=3, col="darkgreen")
abline(v = median(DT_mayo_mujeres$rem_mes_inf), col = "purple", lty = 2)
abline(v = median(DT_mayo_hombres$rem_mes_inf), col = "lightgreen", lty = 2)
legend("topright",
       legend=c("Mujeres","Hombres","Mediana mujeres","Mediana hombres"),
       col=c("purple","darkgreen","purple","lightgreen"),
       lty=c(1,1,2,2,3,3),
       lwd=2, bty="n")  
# La curva del gráfico de hombres es más ancha que la de mujeres por lo que hay mayor variabilidad en los salarios. 
# La mediana en los salarios de hombres es mayor a la de mujeres, significa que el trabajdor típico recibe un salario más alto si es hombre.
# La distribución de mujeres está más concentrada en torno a salarios más bajos, mientras que la de hombres está más “aplanada” y extendida hacia la derecha. En otras palabras, las mujeres tienden a estar más agrupadas en un rango más estrecho de salarios, mientras que entre hombres hay más dispersión y más casos en la parte alta.

##PREGUNTA 4:##
DT<-salarios_transparencia
DT_mayo<-subset(DT, mes==5)

f_beta = function(x, y){
  if (var(x) == 0) {
    return(0)
  } else {
    return(cov(x, y) / var(x))
  }
}

f_alpha = function(x, y, beta){
  a = mean(y) - beta * mean(x)
  return(a)
}

#inciso a#:
# Se repite el proceso de 2c, pero esta vez con la variable padre_org como variable explicativa y filtrando solo los sectores de salud y educación.
DT_mayo=DT_mayo[, c("padre_org", "rem_mes_inf")]
DT_mayo_saludyeducación<- DT_mayo %>% filter(padre_org=="Salud"|padre_org=="Educación")
DT_mayo_saludyeducación<-subset(DT_mayo_saludyeducación, rem_mes_inf>0)
DT_mayo_saludyeducación$padre_org = ifelse(DT_mayo_saludyeducación$padre_org == "Salud", 1, 0)
DT_mayo_saludyeducación$log_rem_inf <- log(DT_mayo_saludyeducación$rem_mes_inf)

b_1 = f_beta(DT_mayo_saludyeducación$padre_org, DT_mayo_saludyeducación$log_rem_inf)
b_0 = f_alpha(DT_mayo_saludyeducación$padre_org, DT_mayo_saludyeducación$log_rem_inf, b_1)

diferencia_porcentual = (exp(b_1)-1)*100

#inciso b#:
DT_mayo=DT_mayo[, c("padre_org", "rem_mes_inf")]
DT_mayo_saludyeducación<- DT_mayo %>% filter(padre_org=="Salud"|padre_org=="Educación")
DT_mayo_saludyeducación<-subset(DT_mayo_saludyeducación, rem_mes_inf>0)
DT_mayo_saludyeducación<- DT_mayo_saludyeducación %>% filter(rem_mes_inf<5000000)
DT_mayo_saludyeducación$padre_org = ifelse(DT_mayo_saludyeducación$padre_org == "Salud", 1, 0)

DT_mayo_saludyeducación$log_rem_inf <- log(DT_mayo_saludyeducación$rem_mes_inf)

b_1 = f_beta(DT_mayo_saludyeducación$padre_org, DT_mayo_saludyeducación$log_rem_inf)
b_0 = f_alpha(DT_mayo_saludyeducación$padre_org, DT_mayo_saludyeducación$log_rem_inf, b_1)

diferencia_porcentual = (exp(b_1)-1)*100

#inciso c#:
DT_mayo_salud=DT_mayo_saludyeducación %>% filter(padre_org==1)
DT_mayo_educación=DT_mayo_saludyeducación %>% filter(padre_org==0)
densidad_educación= density(DT_mayo_educación$rem_mes_inf)
densidad_salud= density(DT_mayo_salud$rem_mes_inf)
plot(densidad_educación, main= "Distribucion salarial en mayo: Salud vs Educación", xlab = "Remuneración mensual (CLP)", ylab = "Densidad", lwd=3, col="pink", ylim = c(0, 0.000001))
lines(densidad_salud, lwd=3, col="blue")
abline(v = median(DT_mayo_educación$rem_mes_inf), col = "pink", lty = 2)
abline(v = median(DT_mayo_salud$rem_mes_inf), col = "lightblue", lty = 2)
legend("topright",
       legend=c("Educación","Salud","Mediana educación","Mediana salud"),
       col=c("pink","blue","pink","lightblue"),
       lty=c(1,1,2,2,3,3),
       lwd=2, bty="n")  
#en la parte baja–media de la distribución. En educación como en salud la mayoría de trabajadores se concentra en sueldos relativamente modestos.
#Las dos distribuciones son bastante parecidas: se concentran fuerte en los salarios bajos, luego caen, y presentan colas largas hacia la derecha.

#inciso d#:
#Se eliminaron observaciones con salarios no positivos (porque el modelo es log-lineal)
#Se estimó manualmente, para cada tope salarial, un modelo MCO simple:ln(salarios_i)= alpha+beta*salud_i+ erara
#La brecha porcentual se calculó como (e^beta-1)*100
#Para ver cómo cambia la brecha al ampliar la muestra, se incrementó progresivamente el tope máximo de salario incluido en pasos de 100.000 CLP.
#sobre resultados gráfico: en los tramos bajos de salario la brecha es bastante negativa (sectores de educación ganan más que los de salud).
#al aumentar el tope de salario la brecha se reduce significativamente incluso llegando a cero y numeros positivos (cerca del tope de 1000000), indica que en ese rango algunos salarios de salud elevan el promedio relativo del sector.
#pasado el tope de un millón y si se sigue aumentando, la brecha cae a cero, e incluso baja a numeros negativos pero se mantiene en ese rango cercano a cero.
#el gráfico muestra que la estimación de la brecha depende de cómo se traten los outliers--> Con salarios bajos, educación aparece con ventaja, y con salarios muy altos, salud logra compensar y en algunos tramos supera a educación, pero este efecto desaparece cuando se considera la masa principal de trabajadores.

DT_mayo=DT_mayo[, c("padre_org", "rem_mes_inf")]
DT_mayo_saludyeducación<- DT_mayo %>% filter(padre_org=="Salud"|padre_org=="Educación")
DT_mayo_saludyeducación<-subset(DT_mayo_saludyeducación, rem_mes_inf>0)
DT_mayo_saludyeducación$padre_org = ifelse(DT_mayo_saludyeducación$padre_org == "Salud", 1, 0)
DT_mayo_saludyeducación$log_rem_mes_inf <- log(DT_mayo_saludyeducación$rem_mes_inf)

saltos = 100000 
cota_inferior = min(DT_mayo_saludyeducación$rem_mes_inf) + saltos
cota_superior = max(DT_mayo_saludyeducación$rem_mes_inf)
vector_cota = seq(from = cota_inferior, to = cota_superior, by = saltos)

brechas_porcentuales = numeric(length(vector_cota))

for (i in 1:length(vector_cota)) {
  cota_actual = vector_cota[i]
  
  data_temporal = DT_mayo_saludyeducación[DT_mayo_saludyeducación$rem_mes_inf < cota_actual, ]
  
  if (nrow(data_temporal) > 10) { 
    beta_temporal = f_beta(data_temporal$padre_org, data_temporal$log_rem_mes_inf)
    brecha_porcentual = (exp(beta_temporal) - 1) * 100
    brechas_porcentuales[i] = brecha_porcentual
  } else {
    brechas_porcentuales[i] = NA
  }
}

datos_grafico = data.frame(cota = vector_cota, brecha = brechas_porcentuales)
datos_grafico = na.omit(datos_grafico)

plot(datos_grafico$cota, datos_grafico$brecha, 
     type = "l",
     main = "Brecha salarial porcentual por tope de salario",
     xlab = "Tope máximo de salario (CLP)",
     ylab = "Brecha salarial porcentual (%)",
     col = "blue",
     lwd = 3,
     xlim = c(0, 15000000))


##PREGUNTA 5:##
DT<-salarios_transparencia
DT$sexo = ifelse(DT$sexo == "femenino", 1, 0)
DT_p5<- DT[, c("sexo", "n_hrs_ext_tot")]
DT_p5 = DT_p5[!is.na(DT_p5$n_hrs_ext_tot) & !is.na(DT_p5$sexo), ]
DT_p5$log_n_hrs_ext_tot = log(DT_p5$n_hrs_ext_tot)

#inciso a#:
#el modelo de regresión es: n_hrs_ext_tot= beta_0+beta_1*sexo_mujer + vector de errores.
#encontraremos la solución con esto: beta_hat= (X'*X)^-1*X'*y
#la matriz de diseño, con una columna de unos para el intercepto y otra de la variable DT$sexo (1 para mujer y 0 para hombre)
X_p5<- cbind(1,DT_p5$sexo)
#el vector de horas extra es la variable dependiente (y)
y_p5<- DT_p5$n_hrs_ext_tot
#beta_0 (intercepto): representa el número promedio de horas extra de la mujeres
#beta_1: representa la diferencia promedio en horas extra entre hombres y mujeres
XapX_p5<-t(X_p5) %*% X_p5 #matriz X'X
Xapy_p5<- t(X_p5) %*% y_p5 #producto X'y 
beta_hat_p5<- solve(XapX_p5,Xapy_p5)

beta_0_p5<- as.numeric(beta_hat_p5[1]) #promedio horas extra hombres
beta_1_p5<- as.numeric(beta_hat_p5[2]) #diferencia promedio en horas extra entre hombres y mujeres
#Así calculamos la diferencia porcentual respecto a las mujeres:
dif_prom_num_extra_tot_hym<- (beta_1_p5/beta_0_p5)*100 #-47.6%
colnames(beta_hat_p5) <- c("estimate")
beta_hat_p5[2] #-90.72695--> esta variable da cuántas horas extra (en promedio) difieren las mujeres respecto a los hombres, como es negativo las mujeres en promedio tiene menos horas extras.

#inciso b#:
#se puede usar el rango intercuartil para eliminar outliers en los datos:
#Los valores que caen por debajo del límite inferior (Q1 - 1.5*IQR) y por encima del limite superior (Q3 + 1.5*IQR) serían los outliers.
Q1<-quantile(DT_p5$n_hrs_ext_tot, 0.25)
Q3<-quantile(DT_p5$n_hrs_ext_tot, 0.75)
iqr_p5<-IQR(DT_p5$n_hrs_ext_tot)
limite_inferior<- Q1-(1.5*iqr_p5)
limite_superior<- Q3+(1.5*iqr_p5)
DT_p5_sin_outliers <- subset(DT_p5, n_hrs_ext_tot>= limite_inferior & n_hrs_ext_tot<= limite_superior)
#ahora se repite el proceso de estimación realizado en el inciso anterior, pero con los datos sin outliers:
X_p5_sin_outliers<- cbind(1,DT_p5_sin_outliers$sexo)
y_p5_sin_outliers<- DT_p5_sin_outliers$n_hrs_ext_tot
XapX_p5_sin_outliers<-t(X_p5_sin_outliers) %*% X_p5_sin_outliers
Xapy_p5_sin_outliers<- t(X_p5_sin_outliers) %*% y_p5_sin_outliers  
beta_hat_p5_sin_outliers<- solve(XapX_p5_sin_outliers,Xapy_p5_sin_outliers)

beta_0_p5_sin_outliers<- as.numeric(beta_hat_p5_sin_outliers[1])
beta_1_p5_sin_outliers<- as.numeric(beta_hat_p5_sin_outliers[2]) 
dif_prom_num_extra_tot_hym_sin_outliers<- (beta_1_p5_sin_outliers/beta_0_p5_sin_outliers)*100 #NaN
beta_hat_p5_sin_outliers[1]
beta_hat_p5_sin_outliers[2] #0%
#En la muestra completa (inciso a) el estimador beta_1 fue aprox -90.73, que indica que los hombres en promedio hacen 90.73 horas más que las mujeres.
#Tras eliminar outliers beta_1 cambió a 0, y el porcentaje de diferencia respecto al promedio de mujeres cambió de -47.6% a indefinido.
#Esta diferencia se explica porque, en la base original, existen pocos individuos con valores extremadamente altos de horas extra (outliers) que arrastran fuertemente el promedio y generan una brecha de género negativa muy grande. 
#Al eliminarlos, la diferencia promedio desaparece. Esto muestra que la conclusión acerca de la existencia de una brecha depende críticamente del tratamiento de los outliers: sin ellos, los hombres y las mujeres no presentan diferencias relevantes en el número de horas extra reportadas.

#inciso c#:
DT<-salarios_transparencia
DT$sexo = ifelse(DT$sexo == "femenino", 1, 0)
DT_p5<- DT[, c("sexo", "mes", "n_hrs_ext_tot")]
DT_p5 = DT_p5[!is.na(DT_p5$n_hrs_ext_tot) & !is.na(DT_p5$mes), ]
DT_p5$log_n_hrs_ext_tot = log(DT_p5$n_hrs_ext_tot)
#se ordenan los meses en orden
meses <- sort(unique(DT_p5$mes))
coef_mes <- data.frame(mes = meses, beta = NA, n = NA)
#inicio de secuencia
for (i in seq_along(meses)){
  m <- meses[i]
  sub <- DT_p5[DT_p5$mes == m, ]
  coef_mes$n[i] <- nrow(sub)
  if (nrow(sub) >= 2){ #asegura que el subconjunto de datos para un mes tenga mas de dos observaciones
    Xt <- cbind(1, as.matrix(sub$sexo))
    yt <- as.matrix(sub$n_hrs_ext_tot)
    beta_t <- solve(t(Xt)%*%Xt) %*% t(Xt) %*% yt
    coef_mes$beta[i] <- beta_t[2]
  } else {
    coef_mes$beta[i] <- NA
  }
}

#gráfico que muestre la brecha de género en horas extra para cada mes:
plot(coef_mes$mes, coef_mes$beta, type = "o",col="blue", pch = 19,
     xlab = "Mes", ylab = "Brecha género",
     main = "Brecha de género en horas extra por mes")
abline(h = 0, lty = 2)

#inciso d#:

set.seed(1234)
N <- 1000
muestra <- 500

#variable que almacena iteraciones:
iteraciones<-list()
#iteraciones:
for (m in meses){
  sub <- DT_p5[DT_p5$mes == m, ]
  num <- nrow(sub)
  if(num == 0) next
  coef_mes <- numeric(N)
  for (r in 1:N){
    if (num >= muestra){
      samp <- sample(1:num, muestra, replace = FALSE)
    } else {
      samp <- sample(1:num, muestra, replace = TRUE)
    }
    sampl <- sub[samp, ]
    Xr <- cbind(1, as.matrix(sampl$sexo))
    yr <- as.matrix(sampl$n_hrs_ext_tot)
    beta_r <- solve(t(Xr)%*%Xr) %*% t(Xr) %*% yr
    coef_mes[r] <- beta_r[2]
  }
  iteraciones[[as.character(m)]] <- coef_mes
}

#calculo de percentiles y medias por mes:
#data.frame resumen que guarda, para cada mes, las estadísticas de los 1000 coeficientes obtenidos antes
resumen_coef<- do.call(rbind, lapply(names(iteraciones), function(m){
  it<-iteraciones[[m]]
  c(mes=as.numeric(m),
    mean=mean(it,na.rm=TRUE),
    per2=quantile(it,0.02,na.rm=TRUE),
    per50=quantile(it,0.5,na.rm=TRUE),
    per98=quantile(it,0.98,na.rm=TRUE))
}))
#transformar a data.frame y s emuestar resultados de coeficientes obtenidos con sus respectivos percentiles por mes:
resumen_coef<-as.data.frame(resumen_coef)

#graficamos datos para  mostrar cómo cambia la brecha de género en horas extras para cada mes, identificando el promedio y los intervalos de confianza:
plot(resumen_coef$mes, resumen_coef$mean, type="o",pch=19,
     ylim= range(c(resumen_coef$per2, resumen_coef$per98), na.rm=TRUE),
     xlab= "Mes", ylab= "Brecha de genero en horas extra", main= "Evolución brecha de género en horas extras para cada mes")
lines(x = resumen_coef$mes, y=resumen_coef$mean, col="black",lty=1,pch=45)
lines(x = resumen_coef$mes, y=resumen_coef$per50, col="blue",lty=1)
lines(x = resumen_coef$mes, y = resumen_coef$per2, col = "red", lty = 2) # Percentil 2
lines(x = resumen_coef$mes, y = resumen_coef$per98, col = "green", lty = 2) # Percentil 98

# Anadir una leyenda para clarificar las lineas
legend("bottomleft", 
       legend = c("promedio acumulado","Mediana (P50)", "P2", "P98"), 
       col = c("black","blue", "red", "green"), 
       lty = c(1, 1, 2,2), 
       pch = c(NA, NA, NA))

#inciso e#:
#usaremos el mismo método para excluir outliers que en la b
#Asíque se hará el mismo procedimiento anterior, pero sin los outliers en la data
Q1_e<-quantile(DT_p5$n_hrs_ext_tot, 0.25)
Q3_e<-quantile(DT_p5$n_hrs_ext_tot, 0.75)
iqr_p5_e<-IQR(DT_p5$n_hrs_ext_tot)
limite_inferior_e<- Q1_e-(1.5*iqr_p5_e)
limite_superior_e<- Q3_e+(1.5*iqr_p5_e)
DT_p5_sin_outliers_e <- subset(DT_p5, n_hrs_ext_tot>= limite_inferior_e & n_hrs_ext_tot<= limite_superior_e)

#ahora repetir procedimiento anterior pero con la data sin los outliers:

set.seed(1234)
N_e <- 1000
muestra_e <- 500
meses_e <- sort(unique(DT_p5_sin_outliers_e$mes))
coef_mes_e <- data.frame(mes = meses, beta = NA, n = NA)

#variable que almacena iteraciones:
iteraciones_e<-list()
#iteraciones:
for (m in meses_e){
  sub_e <- DT_p5_sin_outliers_e[DT_p5_sin_outliers_e$mes == m, ]
  num_e <- nrow(sub_e)
  if(num_e == 0) next
  coef_mes_e <- numeric(N_e)
  for (r in 1:N_e){
    if (num_e >= muestra_e){
      samp_e <- sample(1:num_e, muestra_e, replace = FALSE)
    } else {
      samp_e <- sample(1:num_e, muestra_e, replace = TRUE)
    }
    sampl_e <- sub_e[samp_e, ]
    Xr_e <- cbind(1, as.matrix(sampl_e$sexo))
    yr_e <- as.matrix(sampl_e$n_hrs_ext_tot)
    beta_r_e <- solve(t(Xr_e)%*%Xr_e) %*% t(Xr_e) %*% yr_e
    coef_mes_e[r] <- beta_r_e[2]
  }
  iteraciones_e[[as.character(m)]] <- coef_mes_e
}

#calculo de percentiles y medias por mes:
#data.frame resumen que guarda, para cada mes, las estadísticas de los 1000 coeficientes obtenidos antes
resumen_coef_e<- do.call(rbind, lapply(names(iteraciones_e), function(m){
  it_e<-iteraciones_e[[m]]
  c(mes=as.numeric(m),
    mean=mean(it_e,na.rm=TRUE),
    per2_e=quantile(it_e,0.02,na.rm=TRUE),
    per50_e=quantile(it_e,0.5,na.rm=TRUE),
    per98_e=quantile(it_e,0.98,na.rm=TRUE))
}))
#transformar a data.frame y s emuestar resultados de coeficientes obtenidos con sus respectivos percentiles por mes:
resumen_coef_e<-as.data.frame(resumen_coef_e) 


plot(resumen_coef_e$mes, resumen_coef_e$mean, type="o",pch=19,
     ylim= range(c(resumen_coef_e$per2_e, resumen_coef_e$per98_e), na.rm=TRUE),
     xlab= "Mes", ylab= "Brecha de genero en horas extra", main= "Evolución brecha de género en horas extras para cada mes")
lines(x = resumen_coef_e$mes, y=resumen_coef_e$mean, col="black",lty=1,pch=45)
lines(x = resumen_coef_e$mes, y=resumen_coef_e$per50_e, col="blue",lty=1)
#innecesario poner estas dos:
lines(x = resumen_coef_e$mes, y = resumen_coef_e$per2_e, col = "red", lty = 2) # Percentil 2
lines(x = resumen_coef_e$mes, y = resumen_coef_e$per98_e, col = "green", lty = 2) # Percentil 98

# Anadir una leyenda para clarificar las lineas
legend("bottomleft", 
       legend = c("promedio acumulado","Mediana (P50)"), 
       col = c("black","blue"), 
       lty = c(1, 1), 
       pch = c(NA, NA))

#como todos los datos quedan en cero, no hay un cambio a lo largo de los meses

#inciso f#:
#grafico comparativo:
plot(resumen_coef$mes, resumen_coef$mean, type='b', pch=19, ylim = range(c(resumen_coef$per2, resumen_coef$per98, resumen_coef_e$per2_e, resumen_coef_e$per98_e), na.rm=TRUE),
     xlab = "Mes", ylab = "Brecha estimada", main = "Comparación: con outliers vs sin outliers")
points(resumen_coef_e$mes, resumen_coef_e$mean, type='b', pch=17, col = "blue")
lines(x = resumen_coef$mes, y=resumen_coef$per50, col="grey",lty=1)
lines(x = resumen_coef_e$mes, y=resumen_coef_e$per50_e, col="lightblue",lty=1)
legend("bottomleft", legend = c("Original","Mediana original","Sin outliers","Mediana sin outliers"), pch = c(19,17), col = c("black","grey","blue","lightblue"))
abline(h=0, lty=2)
#comparación se hizo en el informe latex





