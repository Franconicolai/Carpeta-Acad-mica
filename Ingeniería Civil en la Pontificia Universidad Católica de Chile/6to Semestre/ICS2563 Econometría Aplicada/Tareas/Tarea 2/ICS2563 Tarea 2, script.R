# --- Importación de módulos y base de datos -----------------------------------

library(haven)
salarios_transparencia <- read_dta("Tareas/salarios_transparencia.dta") 

# --- Pregunta 1 ---------------------------------------------------------------
limpieza_seccion = ls()
objetos_a_limpiar = setdiff(limpieza_seccion, "salarios_transparencia")
rm(list = objetos_a_limpiar)
rm(limpieza_seccion)
# --- Pregunta 1, ítem a -------------------------------------------------------

bd = salarios_transparencia
bd = bd[bd$mes == 8,]
bd = bd[bd$pago_hrs_ext < 1000000,]
# bd = bd[bd$pago_hrs_ext > 0,]
bd$sexo_dummy <- ifelse(bd$sexo == "masculino", 0, 1)

f_grafico = function(data_1, data_2, titulo, x_lab_texto, legend_texto){
  
  dens_1 = density(data_1, na.rm = TRUE)
  dens_2 = density(data_2, na.rm = TRUE)
  xmin = min(dens_1$x, dens_2$x, na.rm = TRUE) * 1
  xmax = max(dens_1$x, dens_2$x, na.rm = TRUE) * 10**-1
  ymax = max(dens_1$y, dens_2$y, na.rm = TRUE) * 1.1
  
  plot(dens_1, col = "blue", lwd = 2,
       main = titulo,
       xlab = x_lab_texto,
       ylab = "Densidad", 
       xlim = c(0, xmax),
       ylim = c(0, ymax))
  lines(dens_2, col = "red", lwd = 2)
  legend("topright", legend = legend_texto,
         col = c("blue", "red"), lwd = 2)
}

f_grafico(
  data_1 = bd[bd$sexo_dummy == 0,]$pago_hrs_ext,
  data_2 = bd[bd$sexo_dummy == 1,]$pago_hrs_ext,
  titulo = "Pago de horas extraordinarias",
  x_lab_texto = "Monto (CLP)",
  legend_texto = c("Distribución hombres", "Distribución mujeres")
)

f_grafico(
  data_1 = bd[bd$padre_org == "Salud",]$pago_hrs_ext,
  data_2 = bd[bd$padre_org != "Salud",]$pago_hrs_ext,
  titulo = "Pago de horas extraordinarias",
  x_lab_texto = "Monto (CLP)",
  legend_texto = c("Distribución salud", "Distribución resto de sectores")
)


f_grafico(
  data_1 = bd[bd$estamento == "Profesional",]$pago_hrs_ext,
  data_2 = bd[bd$estamento != "Profesional",]$pago_hrs_ext,
  titulo = "Pago de horas extraordinarias",
  x_lab_texto = "Monto (CLP)",
  legend_texto = c("Distribución profesional", "Distribución resto de estamentos")
)

f_grafico(
  data_1 = bd[bd$calidad_juridica == "planta",]$pago_hrs_ext,
  data_2 = bd[bd$calidad_juridica == "contrata",]$pago_hrs_ext,
  titulo = "Pago de horas extraordinarias",
  x_lab_texto = "Monto (CLP)",
  legend_texto = c("Distribución de planta", "Distribución de contrata")
)

# --- Pregunta 1, ítem b-c -------------------------------------------------------

limpieza_seccion = ls()
objetos_a_limpiar = setdiff(limpieza_seccion, "salarios_transparencia")
rm(list = objetos_a_limpiar)
rm(limpieza_seccion)

bd = salarios_transparencia
bd = bd[bd$mes == 8,]
# bd = bd[bd$pago_hrs_ext > 0,]
bd = bd[bd$padre_org == "Salud",]
bd$sexo_dummy <- ifelse(bd$sexo == "masculino", 0, 1)

f_MCO = function(X, y){
  
  X_con_intercepto <- cbind(1, X)
  XtX <- t(X_con_intercepto) %*% X_con_intercepto
  XtX_inv <- solve(XtX)
  Xty <- t(X_con_intercepto) %*% y
  beta <- XtX_inv %*% Xty
  
  return(beta)
}

f_MCO(
  bd$sexo_dummy,
  bd$pago_hrs_ext
)

# --- Pregunta 1, ítem b-c -------------------------------------------------------

limpieza_seccion = ls()
objetos_a_limpiar = setdiff(limpieza_seccion, "salarios_transparencia")
rm(list = objetos_a_limpiar)
rm(limpieza_seccion)

bd = salarios_transparencia
bd = bd[bd$mes == 8,]
bd = bd[bd$pago_hrs_ext > 0,]
bd$sexo_dummy <- ifelse(bd$sexo == "masculino", 0, 1)

bd_hombre_salud = bd[bd$sexo_dummy == 0 & bd$padre_org == "Salud",]
bd_hombre_no_salud = bd[bd$sexo_dummy == 0 & bd$padre_org != "Salud",]

diferencia = mean(bd_hombre_salud$pago_hrs_ext) - mean(bd_hombre_no_salud$pago_hrs_ext)

# --- Pregunta 1, ítem d -------------------------------------------------------

limpieza_seccion = ls()
objetos_a_limpiar = setdiff(limpieza_seccion, "salarios_transparencia")
rm(list = objetos_a_limpiar)
rm(limpieza_seccion)

bd = salarios_transparencia
bd = bd[bd$mes == 8,]
bd = bd[bd$pago_hrs_ext > 0,]
bd$sexo_dummy <- ifelse(bd$sexo == "masculino", 0, 1)

X = model.matrix(~ sexo_dummy + padre_org, data = bd)

f_MCO = function(X, y){
  
  XtX <- t(X) %*% X
  XtX_inv <- solve(XtX)
  Xty <- t(X) %*% y
  beta <- XtX_inv %*% Xty
  
  return(beta)
}

f_MCO(
    X = X,
    y = bd$pago_hrs_ext
  )


# --- Pregunta 2 ---------------------------------------------------------------

limpieza_seccion = ls()
objetos_a_limpiar = setdiff(limpieza_seccion, "salarios_transparencia")
rm(list = objetos_a_limpiar)
rm(limpieza_seccion)

library(readr)
Datos<-salarios_transparencia
Datos_agosto<-subset(Datos,mes==8)

# --- Pregunta 2, ítem a -------------------------------------------------------

#para estimar los coeficientes del modelo bivariado, usando forma manual-matricial:
#queremos estimar el modelo y = β0 + β1x + u
#donde y=pago_mensual_horas_extr (variable que queremos predecir), y x=rem_men_inf (variable que se usa para explicar comportamiento de y)
#β0 es el intercepto, que representa el valor esperado de Y cuando X es cero.
#β1 es la pendiente, que mide el cambio en Y por cada unidad de cambio en X
# el estimador se obtiene resolviendo: β = (X′X)−1X′y

#filtro de datos:
DT<-subset(Datos_agosto,pago_hrs_ext_tot_inf<1000000) #alfinal no saque los que valen cero, nse si esta bien pero hay una diferencia
DT_2a<-DT[, c("pago_hrs_ext_tot_inf", "rem_mes_inf")]
DT_2a <- DT_2a[!is.na(DT_2a$pago_hrs_ext_tot_inf) & !is.na(DT_2a$rem_mes_inf), ]

#definimos variable dependiente (y):
y_2a<-as.matrix(DT_2a$pago_hrs_ext_tot_inf)
#definimos variable matriz X, que tendrá en su primera columna un vectro de unos para el intercepto y en la segunda columna estará la variable indepondiente:
x_2a<-as.matrix(cbind(1,DT_2a$rem_mes_inf))
#calculamos X'X:
XtX_2a<- t(x_2a) %*% x_2a
#calculamos (X'X)^-1 (inversa):
inv_XtX_2a<- solve(XtX_2a)
#caclulamos X'y:
Xty_2a <- t(x_2a) %*% y_2a
beta_hat_2a<-inv_XtX_2a %*% Xty_2a
as.numeric(beta_hat_2a[1])
as.numeric(beta_hat_2a[2])
#β0=48215.4
#β1=-0.002090202
#el estimador del parámetro de la variable rem_mes_inf, es la variación esperada en el pago mensual de horas extra (en pesos) cuando la remuneración bruta mensualizada aumenta en 1 peso.
#osea sería los pesos de horas extra por peso de remuneración
# significa que por cada peso extra en remuneración bruta, el pago promedio de horas extra disminuye en aprox 0.002 CLP

# --- Pregunta 2, ítem b -------------------------------------------------------
#El modelo sería el mismo pero sin el término β0, así que la regresión lineal pasa a ser: y = β1x + u
#Es decir, ahora la matriz de diseño X sólo tendrá la columan de la variable independiente (rem_mes_inf)
#entonces usamos la misma lógica que antes pero ahora lo distitno sería nuestra matriz X:
y_2b<-as.matrix(DT_2a$pago_hrs_ext_tot_inf)
x_2b<-as.matrix(DT_2a$rem_mes_inf)
XtX_2b<- t(x_2b) %*% x_2b
inv_XtX_2b<- solve(XtX_2b)
Xty_2b <- t(x_2b) %*% y_2b
beta_hat_2b<-inv_XtX_2b %*% Xty_2b
as.numeric((beta_hat_2b))
#β1= 0.01316271
#con intercepto la relación aparece ligeramente negativa; sin intercepto, claramente positiva y mayor en magnitud
#La razón de esta discrepancia es que el modelo sin intercepto obliga la recta a pasar por el origen (0,0), una restricción que no parece adecuada en nuestra muestra (los salarios son grandes y la relación real no está centrada en el origen). Por estas razones, considero que el modelo con intercepto representa mejor la muestra; el modelo sin intercepto es bueno como contraste.
#Es decir, forzar el origen (usar el modelo sin intercepto) produce un estimador que no representa la relación típica observada en la muestra.

# --- Pregunta 2, ítem c -------------------------------------------------------
#Ahora se introduce la variable sexo en el análisis para ver si la pendiente de la remuneración cambia para hombres y mujeres.
#Entonces la variable dependiente (y) es pago_mensual_horas_extr, y las variables independientes serían: variable x=rem_men_inf, una variable binaria dummy para sexo llamemosla D=1 si es mujer y D=0 si es hombre, y una término de interacción entre ambas variables anteriories (x*D) 
#nuestro modelo sería: y= β0+β1x+β2D+β3(x*D)+ϵ
#β0= Es el pago esperado por horas extras para un hombre con remuneración cero.
#β1= efecto de rem_mes sobre pago_hrs para hombres.
#β2= Es la diferencia en el pago de horas extras entre mujeres y hombres cuando la remuneración es cero.
#β3= Es la diferencia en el efecto de la remuneración en el pago de horas extras entre mujeres y hombres.
#β1+β3=efecto marginal en mujeres->diferencia en la pendiente de rem_mes para mujeres respecto a hombres (es lo que queremos saber)
#efecto marginal=efecto remuneracion (x) en pago de horas extras (y)--> derivada parcial de y respecto con x

DT_2c<-DT[, c("sexo","pago_hrs_ext_tot_inf", "rem_mes_inf")]
DT_2c$sexo<-ifelse(DT_2c$sexo=="femenino",1,0)

#definimos matrices:
y_2c<-as.matrix(DT_2c$pago_hrs_ext_tot_inf)
#para la X, debe incluir el vector de unos para el intercepto, la remuneracion mensual, la dummy de sexo y el término de interacción:
INT<-DT_2c$rem_mes_inf*DT_2c$sexo
X_2c<-as.matrix(cbind(1, DT_2c$rem_mes_inf, DT_2c$sexo, INT))
XtX_2c<- t(X_2c) %*% X_2c
inv_XtX_2c <- solve(XtX_2c)
XtY_2c<- t(X_2c) %*% y_2c
beta_hat_2c<- inv_XtX_2c %*% XtY_2c
as.numeric(beta_hat_2c[4])
#β0= 65563.71
#β1= -0.005455916
#β2= -25508.98
#β3= 0.004975111
#considernado nuestro sistema para la variable dummy (m=0 y f=1), entonces:
#el efecto marginal de la remuneración en el pago de horas extras para los hombres es: -0.005455916
as.numeric(beta_hat_2c[2])+as.numeric(beta_hat_2c[4])
#β1+β3= -0.0004808053
#el efecto marginal de la remuneración en el pago de horas extras par las mujeres es: -0.0004808053

# --- Pregunta 2, ítem d -------------------------------------------------------
install.packages("MASS")
library(MASS)

DT<-subset(Datos_agosto,pago_hrs_ext_tot_inf<1000000)
DT_2d <- DT[!is.na(DT$pago_hrs_ext_tot_inf) & !is.na(DT$rem_mes_inf), ]

DT_2d$sexo<-ifelse(DT_2d$sexo=="femenino",1,0)
#Entonces ahora encima del modelo del inciso c se agrega lo siguiente:
#variable binaria que si estamento es profesional entonces toma el valor de 1:
DT_2d$estamento_dummy <- ifelse(DT_2d$estamento == "Profesional", 1, 0)
#otra variable binaria que si calidad juridica es planta toma el valor de 1:
DT_2d$planta_dummy <- ifelse(DT_2d$calidad_juridica == "planta", 1, 0)
#varible dummies por institución publica (padre_org), se usara la función model.matrix para esto
#definimos la variable dependiente:
y_2d<- as.matrix(DT_2d$pago_hrs_ext_tot_inf)
#matriz de diseño de forma acumulativa:
X_acumulativo_2d<- model.matrix(pago_hrs_ext_tot_inf ~ rem_mes_inf * sexo + 
                                  estamento_dummy + planta_dummy +
                                  factor(padre_org), data = DT_2d)
#calculamos los coeficientes beta usando la pseudoinversa--> usaremos ginv() en vez de solve porque hubo gente que tuvo problemas por ser matriz singular
XtX_2d<- t(X_acumulativo_2d) %*% X_acumulativo_2d
ginv_XtX_2d<- ginv(XtX_2d)
XtY_2d <- t(X_acumulativo_2d) %*% y_2d
beta_hat_2d <- ginv_XtX_2d %*% XtY_2d
#β0= 3.497105e-09
#β1=0,0123 aprox
#el modelo simple con solo remuneración, sexo y término de interacción dio un coeficiente negativo para beta1.
#Cuando se agrego todas las variables de control (dummies para padre_org), el coeficiente de rem_mes_inf (beta1) se volvió positivo. Este nuevo resultado (0,0123) es más lógico, ya que un salario más alto generalmente se asocia con una tasa de pago más alta por las horas extras. Las variables de control del modelo 2d han "limpiado" la relación entre remuneración y pago de horas extras. 
#asignamos nombres a coeficientes:
rownames(beta_hat_2d) <- colnames(X_acumulativo_2d)
#seleccionaremos lo prinicpal para tabular:
coeficientes_principales <- beta_hat_2d[c("(Intercept)", "rem_mes_inf", 
                                          "sexo", "rem_mes_inf:sexo",
                                          "estamento_dummy", "planta_dummy"),]
#tabla de coeficientes:
print("Tabla de Coeficientes de las variables principales:")
print(coeficientes_principales)
tabla_resultados <- as.data.frame(coeficientes_principales) #esta es la tabla 
colnames(tabla_resultados) <- "Coeficiente"
print(tabla_resultados)

#el efecto de la remuneración es ligeramente mayor para las mujeres (0.0139) que para los hombres (0.0123), lo que indica una diferencia de aproximadamente 13% en la pendiente entre ambos grupos.
#El hecho de que estamento_dummy y planta_dummy tengan coeficientes de cero es una consecuencia directa al problema de la colinealidad, que es común en modelos con tantas variables explicativas. Si bien ginv() permite que el modelo corra, la interpretación de estos coeficientes individuales se vuelve imposible.

# --- Pregunta 2, ítem e -------------------------------------------------------
#La variabilidad explicada en un modelo de regresión se refiere a qué tan bien las variables independientes explican la variabilidad de la variable dependiente. Esta se mide por el coeficiente de determinación, o R^2.
#Como regla general, el R^2 siempre aumenta o se mantiene igual al añadir una nueva variable a un modelo de regresión
#al pasar de un modelo simple (2c) a un modelo completo con más de 300 dummies (2d), es de esperar que el R^2 aumente significativamente. Esto se debe a que las variables dummy de padre_org y organismo capturan una gran cantidad de la variabilidad del pago de horas extras que está relacionada con la institución específica de cada individuo.
#A pesar del aumento del R ^2, el modelo no es robusto. La evidencia de esto es la colinealidad perfecta que resultó en coeficientes de cero para estamento_dummy y el término de interacción rem_mes_inf:sexo. Este hallazgo muestra que la relación entre las variables de interés se ve comprometida al añadir las dummies, lo que hace que los resultados sean inestables e imposibles de interpretar de forma aislada.
#En conclusión, mientras que el aumento del R ^2 indica que el modelo se ajusta mejor a los datos, la aparición de la colinealidad perfecta y la consecuente pérdida de interpretabilidad de los coeficientes de variables importantes demuestran que el modelo de la pregunta 2d no es robusto 


# --- Pregunta 3 ---------------------------------------------------------------
limpieza_seccion = ls()
objetos_a_limpiar = setdiff(limpieza_seccion, "salarios_transparencia")
rm(list = objetos_a_limpiar)
rm(limpieza_seccion)
# --- Pregunta 3, ítem a -------------------------------------------------------
DT<-subset(Datos_agosto,pago_hrs_ext_tot_inf<1000000)
DT_3a <- DT[!is.na(DT$pago_hrs_ext_tot_inf) & !is.na(DT$rem_mes_inf), ]
_________
#Usaremos un modelo lineal con múltiples variables 
#Variable Dependiente (y): pago_hrs_ext_tot_inf.
#Variables Independientes (X): rem_mes_inf, sexo, una dummy para "profesional", una dummy para "planta" y una dummy para el sector salud.
#MODELO GENERAL: Y=β0+β1X_1+β2X_2+β3X_3+β4X_4+β5X_5+ϵ, donde cada X es una de las variables independientes, queremos interpretar los coeficientes obtenidos por planta y salud
#usaremos metodo de algebra matricial denuevo para estimar por MCO
y_3a<- as.matrix(DT_3a$pago_hrs_ext_tot_inf)
DT_3a$sexo<-ifelse(DT_3a$sexo=="femenino",1,0)
DT_3a$estamento_dummy <- ifelse(DT_3a$estamento == "Profesional", 1, 0)
DT_3a$planta_dummy <- ifelse(DT_3a$calidad_juridica == "planta", 1, 0)
DT_3a$salud_dummy <- ifelse(DT_3a$institucion_salud == "Salud", 1, 0)
#matriz diseño:
X_3a <- as.matrix(cbind(1, 
                        DT_3a$rem_mes_inf, 
                        DT_3a$sexo, 
                        DT_3a$estamento_dummy,
                        DT_3a$planta_dummy,
                        DT_3a$salud_dummy))

XtX_3a<- t(X_3a) %*% X_3a
inv_XtX_3a <- solve(XtX_3a)
XtY_3a <- t(X_3a) %*% y_3a
beta_hat_3a <- inv_XtX_3a %*% XtY_3a
#le pondremos los nombres a los coeficientes para mejor visualización:
rownames(beta_hat_3a) <- c("Intercepto", 
                           "rem_mes_inf", 
                           "sexo_dummy", 
                           "estamento_dummy", 
                           "planta_dummy", 
                           "salud_dummy")
as.numeric(beta_hat_3a[6])
#coeficiente intercepto: 34905.51 
#coeficiente rem_mes_inf: -0.002835037 (Indica cuánto aumenta el pago de horas extra cuando la remuneración mensual sube en 1 peso (manteniendo lo demás fijo)).
#coeficiente sexo: -17402.11 (Refleja la diferencia promedio entre mujeres y hombres)
#coeficiente dummy profesional: 9212.001 (significa que los trabajadores del estamento profesional reciben, en promedio 9212 por horas extraordinarias que quienes no lo son)
#coeficiente dummy planta: 4405.163 (Captura la diferencia en el pago de horas extraordinarias entre trabajadores de planta y a contrata, manteniendo constante la remuneración, sexo, profesional y salud.Como es positivo, implica que los contratos planta pagan más en horas extra)
#coeficiente dummmy salud: 37835.18 (Permite ver si pertenecer al sector salud se asocia con más o menos pago por horas extra, en comparación con otros sectores.)

#Profesional: Los trabajadores de este estamento reciben en promedio $9,212 más en horas extra que quienes no lo son, lo que sugiere que sus cargos tienen mayor acceso o pago a horas adicionales.
#Planta: Los trabajadores con contrato planta reciben en promedio $4,405 más que los de contrata, lo que indica una diferencia favorable por tipo de contrato en horas extra.
#Pertenecer al sector salud implica un pago de horas extraordinarias en promedio $37,835 mayor que en otros sectores, controlando por sexo, remuneración y estamento.Esto refleja que el sector salud concentra gran parte del gasto en horas extra.

# --- Pregunta 3, ítem b -------------------------------------------------------
#Usaremos el teorema de las regresiones particionadas--> teorema FWL
_____
DT<-subset(Datos_agosto,pago_hrs_ext_tot_inf<1000000)
DT_3b <- DT[!is.na(DT$pago_hrs_ext_tot_inf) & !is.na(DT$rem_mes_inf), ]
DT_3b$sexo<-ifelse(DT_3b$sexo=="femenino",1,0)
DT_3b$estamento_dummy <- ifelse(DT_3b$estamento == "Profesional", 1, 0)
DT_3b$planta_dummy <- ifelse(DT_3b$calidad_juridica == "planta", 1, 0)
DT_3b$salud_dummy <- ifelse(DT_3b$institucion_salud == "Salud", 1, 0)
library(dplyr)
_____
#definimos las variables del modelo:
y_3b<-as.numeric(DT_3b$pago_hrs_ext_tot_inf) 
#matriz con variables de control:
X1_3b<- cbind(1, DT_3b$rem_mes_inf, DT_3b$sexo, DT_3b$salud_dummy)  
#matriz con variables de interés
X2_3b<- cbind(DT_3b$estamento_dummy, DT_3b$planta_dummy)

#Ahora se buscan los coeficientes usando FWL:
#Modelo 1:
#primera regresión de y sobre X1:
beta_hat_y_X1_3b <- solve(t(X1_3b) %*% X1_3b) %*% t(X1_3b) %*% y_3b
u1_3b <- y_3b - X1_3b %*% beta_hat_y_X1_3b #residuos de y-X1--> representan la parte del pago_hrs_ext_tot_inf que no es explicada por las variables de control (rem_bruta_mensual, sexo_dummy, salud_dummy

#Modelo 2:
#segunda regresión de X2 sobre X1:
beta_hat_X2_X1_3b <- solve(t(X1_3b) %*% X1_3b) %*% t(X1_3b) %*% X2_3b
u2_3b <- X2_3b - X1_3b %*% beta_hat_X2_X1_3b #residuos X2-X1--> representan la parte de las variables estamento_dummy y planta_dummy que no está correlacionada con las variables de control.

## Modelo 3, con los residuos del modelo 1 y 2:
beta_fwl <- solve(t(u2_3b) %*% u2_3b) %*% t(u2_3b) %*% u1_3b
beta_fwl 
rownames(beta_fwl) <- c("estamento_dummy", "planta_dummy")
#esto nos dió:
#coeficiente estamento dummy= 9212.001
#coeficiente  planta dummy= 4405.163
#al regresionar u1 sobre u2, estamos esencialmente estimando el efecto de la parte de las variables de interés (profesional y planta) que no está relacionada con las variables de control, sobre la parte del pago de horas extras que tampoco está relacionada con ellas
#Mismos resultados que nos dió en inciso a
# --- Pregunta 3, ítem c -------------------------------------------------------
#se pide comparar la suma de los cuadrados de los residuos netre el modelo completo del inciso a y el estimado mediante FWL.
#ya tenemos los modelos de antes, así que sólo calcualremos los residuos para cada uno, y así calcular la suma de sus cuadrados.

#suma residuos cuadrados modelo inciso a:
residuos_modelo_inciso_a <- y_3a - X_3a %*% beta_hat_3a
SSR_modelo_inciso_a<-sum(residuos_modelo_inciso_a^2)

#ahora calculamos la suma de residuos del modelo de el inciso b:
residuos_modelo_inciso_b <- u1_3b - u2_3b %*% beta_fwl
SSR_modelo_inciso_b <- sum(residuos_modelo_inciso_b^2)

#y calculamos la diferencia para comparar los SSR de los dos modelos:
diferencia <- SSR_modelo_inciso_a - SSR_modelo_inciso_b #-0.3125
as.numeric(SSR_modelo_inciso_a)
SSR_modelo_inciso_b
#SSR modelo a: 388771039448710 
#SSR modelo b: 388771039448710
#la suma de los residuos cuadrados de ambos modelos son idénticos, pero la diferncia que se meustra es un error de precisión computacional.

# --- Pregunta 3, ítem d -------------------------------------------------------
#modelo sin variables profesional y planta:
DT<-subset(Datos_agosto,pago_hrs_ext_tot_inf<1000000)
DT_3d <- DT[!is.na(DT$pago_hrs_ext_tot_inf) & !is.na(DT$rem_mes_inf), ]
DT_3d$sexo<-ifelse(DT_3d$sexo=="femenino",1,0)
DT_3d$salud_dummy <- ifelse(DT_3d$institucion_salud == "Salud", 1, 0)

y_3d<-as.matrix(DT_3d$pago_hrs_ext_tot_inf)
SST <- sum((y_3d - mean(y_3d))^2)

#R^2 para modelo inciso a:
R2_modelo_inciso_a<-1-(SSR_modelo_inciso_a/SST) #0.0310764751939835
#R^2 para modelo inciso b:
R2_modelo_inciso_b<-1-(SSR_modelo_inciso_b/SST) #0.0310764751939827

X_simple_3d <- as.matrix(cbind(1, 
                               DT_3d$rem_mes_inf, 
                               DT_3d$sexo, 
                               DT_3d$salud_dummy))
#coeficientes modelo sin planta ni profesional:
beta_simple_3d <- solve(t(X_simple_3d) %*% X_simple_3d) %*% t(X_simple_3d) %*% y_3d
residuos_simple_3d <- y_3d - X_simple_3d %*% beta_simple_3d
SSR_simple_3d <- sum(residuos_simple_3d^2)
R2_simple_3d <- 1 - (SSR_simple_3d/SST) #0.0296601490625337
#Para lso modelo de a y b R^2 es práctimanete idéntico, esto es resultado del Teorema de Frisch-Waugh-Lovell. Confirma que, aunque uses un método alternativo para calcular un subconjunto de los coeficientes, el poder explicativo total del modelo se mantiene.
#Para el modelo sin las varoiables planta y profesional el avlor de R^2 es un poco menor.
#el aumento en el R^2 al incluir las variables profesional y planta indica que son factores relevantes para explicar la variabilidad en el pago por horas extras, mejorando el ajuste general del modelo.
#La diferencia en el R ^2 (aproximadamente 0.0014) entre las de a y b con la simple, representa la contribución de estas dos variables a la capacidad explicativa del modelo.


# --- Pregunta 4 ---------------------------------------------------------------
limpieza_seccion = ls()
objetos_a_limpiar = setdiff(limpieza_seccion, "salarios_transparencia")
rm(list = objetos_a_limpiar)
rm(limpieza_seccion)
# --- Pregunta 4, ítem a -------------------------------------------------------
bd = salarios_transparencia
bd = bd[bd$mes == 8,]
#bd = bd[bd$pago_hrs_ext < 1000000,]
#bd = bd[bd$pago_hrs_ext > 0,]
bd$sexo_dummy = ifelse(bd$sexo == "masculino", 0, 1)
bd$profesional_dummy = ifelse(bd$estamento == "Profesional", 1, 0)
bd$planta_dummy = ifelse(bd$calidad_juridica   == "planta", 1, 0)
bd$salud_dummy = ifelse(bd$padre_org == "Salud", 1, 0)

columnas_a_seleccionar = c("rem_mes_inf", "pago_hrs_ext", "sexo_dummy", 
                           "profesional_dummy", "planta_dummy", "salud_dummy")
bd = bd[, columnas_a_seleccionar]

malla = seq(200, nrow(bd), by = 200)
resultados <- data.frame(
  tipo_data = character(),
  iteraciones = integer(),
  N = integer(),
  media_coef = numeric(),
  media_r2 = numeric(),
  stringsAsFactors = FALSE
)

f_grafico = function(data_1, data_2, titulo, x_lab_texto, legend_texto){
  
  dens_1 = density(data_1, na.rm = TRUE)
  dens_2 = density(data_2, na.rm = TRUE)
  
  xmin = min(dens_1$x, dens_2$x, na.rm = TRUE)
  xmax = max(dens_1$x, dens_2$x, na.rm = TRUE)
  ymax = max(dens_1$y, dens_2$y, na.rm = TRUE) * 1.1
  
  plot(dens_1, col = "blue", lwd = 2,
       main = titulo,
       xlab = x_lab_texto,
       ylab = "Densidad", 
       xlim = c(xmin, xmax),
       ylim = c(0, ymax))
  lines(dens_2, col = "red", lwd = 2)
  legend("topright", legend = legend_texto,
         col = c("blue", "red"), lwd = 2)
}

f_MCO = function(X, y){
  
  XtX = t(X) %*% X
  XtX_inv = solve(XtX)
  Xty = t(X) %*% y
  
  beta = XtX_inv %*% Xty
  coef_remuneracion = beta[2, 1]
  
  y_hat = X %*% beta
  SST = sum((y - mean(y))^2)
  SSR = sum((y - y_hat)^2)
  R2 = 1 - (SSR / SST)
  
  return(list(coef_remuneracion = coef_remuneracion, r2 = R2))
}

f_bootstraping = function(data, k, malla, bd_temporal){
  
  resultados <- list()

  for (N in malla) {
    
    coeficientes_remuneracion = numeric(k)
    r2_valores = numeric(k)
    
    for (i in 1:k) {
      
      muestra = bd_temporal[sample(nrow(bd_temporal), N, replace = FALSE), ]
      y_muestra = muestra$pago_hrs_ext
      X_muestra = model.matrix(~ rem_mes_inf + sexo_dummy + profesional_dummy + planta_dummy + salud_dummy, 
                               data = muestra)
      resultados_iter = f_MCO(X_muestra, y_muestra)
      coeficientes_remuneracion[i] = resultados_iter$coef_remuneracion
      r2_valores[i] = resultados_iter$r2
      
    }
    
    resultados <- rbind(
      resultados,
      data.frame(
        tipo_data = data, 
        iteraciones = k,
        N = N,
        media_coef = mean(coeficientes_remuneracion),
        media_r2 = mean(r2_valores)
      )
    )
  }
  return(resultados)
}

resultados_bootstrap <- rbind(
  f_bootstraping("Base de datos de Agosto", 50, malla, bd),
  f_bootstraping("Base de datos de Agosto", 100, malla, bd),
  f_bootstraping("Base de datos de Agosto", 300, malla, bd)
)

plot(
  x = resultados_bootstrap$N[resultados_bootstrap$tipo_data == "Base de datos de Agosto" & resultados_bootstrap$iteraciones == 50],
  y = resultados_bootstrap$media_coef[resultados_bootstrap$tipo_data == "Base de datos de Agosto" & resultados_bootstrap$iteraciones == 50],
  type = "b",
  col = "blue",
  pch = 19,
  lwd = 2,
  main = "Evolución de Coeficientes de Remuneración en contraste a N, con 50 iteraciones",
  xlab = "Tamaño de N",
  ylab = "Valor de Coeficientes de Remuneración"
)

plot(
  x = resultados_bootstrap$N[resultados_bootstrap$tipo_data == "Base de datos de Agosto" & resultados_bootstrap$iteraciones == 300],
  y = resultados_bootstrap$media_coef[resultados_bootstrap$tipo_data == "Base de datos de Agosto" & resultados_bootstrap$iteraciones == 300],
  type = "b",
  col = "red", # Usamos un color diferente para distinguirlo
  pch = 19,
  lwd = 2,
  main = "Evolución de Coeficientes de Remuneración en contraste a N, con 300 iteraciones",
  xlab = "Tamaño de N",
  ylab = "Valor de Coeficientes de Remuneración"
)

plot(
  x = resultados_bootstrap$N[resultados_bootstrap$tipo_data == "Base de datos de Agosto" & resultados_bootstrap$iteraciones == 50],
  y = resultados_bootstrap$media_r2[resultados_bootstrap$tipo_data == "Base de datos de Agosto" & resultados_bootstrap$iteraciones == 50],
  type = "b",
  col = "blue",
  pch = 19,
  lwd = 2,
  main = "Evolución de R² en contraste a N, con 50 iteraciones",
  xlab = "Tamaño de N",
  ylab = "Valor de R²"
)

plot(
  x = resultados_bootstrap$N[resultados_bootstrap$tipo_data == "Base de datos de Agosto" & resultados_bootstrap$iteraciones == 300],
  y = resultados_bootstrap$media_r2[resultados_bootstrap$tipo_data == "Base de datos de Agosto" & resultados_bootstrap$iteraciones == 300],
  type = "b",
  col = "red", # Usamos un color diferente para distinguirlo
  pch = 19,
  lwd = 2,
  main = "Evolución de R² en contraste a N, con 300 iteraciones",
  xlab = "Tamaño de N",
  ylab = "Valor de R²"
)

f_grafico(
  data_1 = resultados_bootstrap$media_coef[resultados_bootstrap$tipo_data == "Base de datos de Agosto" & resultados_bootstrap$iteraciones  == 50],
  data_2 = resultados_bootstrap$media_coef[resultados_bootstrap$tipo_data == "Base de datos de Agosto" & resultados_bootstrap$iteraciones  == 300],
  titulo = "Comparación de Coeficientes de Remuneración",
  x_lab_texto = "Coeficiente",
  legend_texto = c("50 iteraciones", "300 iteraciones")
)

f_grafico(
  data_1 = resultados_bootstrap$media_r2[resultados_bootstrap$tipo_data == "Base de datos de Agosto" & resultados_bootstrap$iteraciones  == 50],
  data_2 = resultados_bootstrap$media_r2[resultados_bootstrap$tipo_data == "Base de datos de Agosto" & resultados_bootstrap$iteraciones  == 300],
  titulo = "Comparación de R²",
  x_lab_texto = "R²",
  legend_texto = c("50 iteraciones", "300 iteraciones")
)

# --- Pregunta 4, ítem b -------------------------------------------------------

bd = salarios_transparencia
# bd = bd[bd$pago_hrs_ext < 1000000 & bd$pago_hrs_ext > 0,]
bd$sexo_dummy = ifelse(bd$sexo == "masculino", 0, 1)
bd$profesional_dummy = ifelse(bd$estamento == "Profesional", 1, 0)
bd$planta_dummy = ifelse(bd$calidad_juridica   == "planta", 1, 0)
bd$salud_dummy = ifelse(bd$padre_org == "Salud", 1, 0)

bd_agosto <- bd[bd$mes == 8, ]

columnas_a_seleccionar = c("rem_mes_inf", "pago_hrs_ext", "sexo_dummy", 
                           "profesional_dummy", "planta_dummy", "salud_dummy")
bd = bd[, columnas_a_seleccionar]
bd_agosto = bd_agosto[, columnas_a_seleccionar]

resultados_bootstrap <- rbind(
  f_bootstraping("Base de datos de Agosto", 300, malla, bd_agosto),
  f_bootstraping("Base de datosd e todos los meses", 300, malla, bd)
)

f_grafico(
  data_1 = resultados_bootstrap$media_coef[resultados_bootstrap$tipo_data == "Base de datos de Agosto"],
  data_2 = resultados_bootstrap$media_coef[resultados_bootstrap$tipo_data == "Base de datosd e todos los meses"],
  titulo = "Comparación de Coeficientes de Remuneración",
  x_lab_texto = "Coeficiente",
  legend_texto = c("Agosto", "Todos los meses")
)

m_1 = mean(resultados_bootstrap$media_coef[resultados_bootstrap$tipo_data == "Base de datos de Agosto"])
m_2 = mean(resultados_bootstrap$media_coef[resultados_bootstrap$tipo_data == "Base de datosd e todos los meses"])
abs(m_1 - m_2)

f_grafico(
  data_1 = resultados_bootstrap$media_r2[resultados_bootstrap$tipo_data == "Base de datos de Agosto"],
  data_2 = resultados_bootstrap$media_r2[resultados_bootstrap$tipo_data == "Base de datosd e todos los meses"],
  titulo = "Comparación de R²",
  x_lab_texto = "R²",
  legend_texto = c("Agosto", "Todos los meses")
)

m_1 = mean(resultados_bootstrap$media_r2[resultados_bootstrap$tipo_data == "Base de datos de Agosto"])
m_2 = mean(resultados_bootstrap$media_r2[resultados_bootstrap$tipo_data == "Base de datosd e todos los meses"])
abs(m_1 - m_2)

