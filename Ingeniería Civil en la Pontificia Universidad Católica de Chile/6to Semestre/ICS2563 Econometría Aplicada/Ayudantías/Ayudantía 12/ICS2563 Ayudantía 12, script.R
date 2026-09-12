# Ayudantía 12

# Importamos librerías
library(ggplot2)
library(AER)

# Importamos el dataset
df <- read.csv("student_data.csv")

# (a) Visualización de datos

# Primero realizamos un scatter plot de risk_index vs age

ggplot(df, aes(x = age, y = risk_index)) +
  geom_point(alpha = 0.2)

# No se ve mucho...
# Intentemos agregar una línea de tendencia

ggplot(df, aes(x = age, y = risk_index)) +
  geom_point(alpha = 0.2) +
  stat_smooth(method = "lm", se = FALSE, color = "red")

# Se ve que la línea tiene pendiente levemente positiva, pero no genera un
# aumento tan notorio

# Probemos hacer bins de edad y graficar el promedio de risk_index en cada bin
# Recordemos que los bins agrupan los datos en intervalos, y luego
# calculamos el promedio de risk_index en cada intervalo de edad

ggplot(df, aes(x = age, y = risk_index)) +
  stat_summary_bin(fun = mean, bins = 15, geom = "point", size = 2) +
  stat_smooth(method = "lm", se = FALSE, color = "blue") +
  labs(title = "Conductas de riesgo por edad (con bins)")

# Ahora se ve más claro como las conductas de riesgo aumentan con la edad, pero
# específicamente al cumplir 21 años hay un salto más notorio

# (b) Estimación del efecto causal usando MCO

# Estimamos el modelo RMCO
rmco_model <- lm(gpa ~ risk_index, data = df)
summary(rmco_model)

# Según este modelo, el efecto de cada unidad adicional del índice de factores
# de riesgo es de -0.07 en el GPA

# Es válido este modelo? La verdad es que es dudoso, ya que el risk_index puede
# estar correlacionado con variables omitidas que afectan el GPA, como la
# motivación del estudiante, su entorno familiar, etc.

# (c) Estimación del efecto causal usando IV

# Antes de calcular, veamos si se cumplen los supuestos de RIV

# 1. Relevancia: Podemos probarla realizando la primera etapa de RIV
# 2. Exogeneidad: La edad de los alumnos puede considerarse aleatoria
# 3. Exclusión: La edad no afecta directamente el GPA, solo a través del aumento
# de conductas de riesgo


# Primero hacemos la first stage para ver si el instrumento es relevante

first_stage <- lm(risk_index ~ over21, data = df)
summary(first_stage)

# El instrumento es relevante, ya que el coeficiente de over21 es significativo

# Ahora obtenemos la forma reducida

reduced_form <- lm(gpa ~ over21, data = df)
summary(reduced_form)

# Obtenemos el estimador de RIV manualmente

beta_riv <- coef(reduced_form)["over21"] / coef(first_stage)["over21"]
beta_riv

# Como nuestro instrumento es binario, es lo mismo que obtener el estimador de
# Wald
# Wald = (E[Y|Z=1] - E[Y|Z=0]) / (E[D|Z=1] - E[D|Z=0])
# donde Z es el instrumento (over21), D es la variable endógena (risk_index)
# y Y es la variable dependiente (gpa)
wald_estimator <- (mean(df$gpa[df$over21 == 1]) - mean(df$gpa[df$over21 == 0])) /
  (mean(df$risk_index[df$over21 == 1]) - mean(df$risk_index[df$over21 == 0]))
wald_estimator

# Para ambos se obtiene un coeficiente de -0.23, lo cual es considerablemente
# mas negativo que el estimador RMCO. Esto muestra que el RMCO estaba sesgado,
# probablemente debido a variables omitidas que afectan tanto el risk_index como
# el gpa.

# (d) Ahora estimamos el modelo RIV usando 2SLS

# Estimamos el modelo RIV usando la función ivreg

riv_model <- ivreg(gpa ~ risk_index | over21, data = df)
summary(riv_model)

# También podemos obtener el coeficiente utilizando 2sls a mano
# Primero obtenemos los valores ajustados de risk_index desde la first stage

df$risk_index_hat <- -0.08476 + 0.64230*df$over21

# Luego estimamos la second stage usando los valores ajustados

second_stage <- lm(gpa ~ risk_index_hat, data = df)
summary(second_stage)

# El coeficiente de risk_index_hat es el mismo que el obtenido con ivreg

# La principal diferencia de estos métodos con el cálculo manual es que
# entregan información sobre la significancia estadística del estimador.
# (Ya que en los métodos anteriores solo se dividen coeficientes, por lo que esa
# información se pierde)

# (e) Comparación de resultados e interpretación

# Creamos una tabla con los 5 estimadores
estimators <- data.frame(
  Method = c("RMCO", "RIV (manual)", "Wald", "RIV (ivreg)", "RIV (2SLS manual)"),
  Estimate = c(coef(rmco_model)["risk_index"],
               beta_riv,
               wald_estimator,
               coef(riv_model)["risk_index"],
               coef(second_stage)["risk_index_hat"]),
  Comentario = c("Sesgado", "No entrega información de significancia", "Solo sirve si es binaria, tampoco entrega significancia", "Sí entrega significancia del estimado", "Sí entrega significancia del estimado")
)
print(estimators)
# Interpretación:
# El estimador RMCO sugiere que un aumento en las conductas de riesgo reduce el
# GPA en 0.07 puntos por punto adicional del índice. Sin embargo, este estimador 
# puede estar sesgado debido a la endogeneidad de las conductas de riesgo.
# Los estimadores RIV (manual, Wald, ivreg y 2SLS manual) sugieren un efecto más
# negativo, aproximadamente 0.23 puntos por unidad adicional.
# Esto indica que, al utilizar un instrumento válido (over21), el efecto
# negativo delas conductas de riesgo en el GPA es más pronunciado que lo
# estimado por el RMCO.
# Esto resalta la importancia de abordar la endogeneidad para obtener
# estimaciones causales más precisas.
# Conclusión: El uso de RIV proporciona una estimación más confiable del efecto
# causal de las conductas de riesgo en el GPA en comparación con el RMCO.

