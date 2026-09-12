###########################
## Ayudantía 13 - Script ##
###########################


#################
## Pregunta 1  ##
#################

df_beca <- data.frame(
  paes   = c(590, 595, 597, 599, 602, 605, 607, 610, 613, 615),
  p_beca = c(0.12, 0.15, 0.18, 0.20, 0.45, 0.50, 0.52, 0.55, 0.57, 0.60)
)

# Colores iguales al estilo que estás usando
col_left  <- "#1F497D"
col_right <- "#8B0000"

######
# a) #
######

x <- df_beca$paes
y <- df_beca$p_beca

# Lados del umbral 600
left  <- x < 600
right <- x > 600

plot(x[left], y[left],
     xlim = c(585, 620), ylim = c(0, 0.8),
     xlab = "Puntaje PAES (Running Variable)",
     ylab = "Pr(Beca = 1)",
     main = "Probabilidad de Beca vs Puntaje PAES",
     pch = 16, col = col_left,
     cex = 1.5)

# puntos de la derecha
points(x[right], y[right],
       pch = 16, col = col_right,
       cex = 1.5)

# línea del umbral
abline(v = 600, lty = 2, lwd = 2)

# Grid igual al estilo
grid(col = "grey85", lty = "dotted")


#################
## Pregunta 3  ##
#################

library(haven)
# Leemos la bdd
bdd <- read_dta("PlanPreferente_RDD.dta")

######
# a) #
######

breaks <- seq(500, 750, by = 5)
bins   <- cut(bdd$riskscore, breaks = breaks)

riskscore_bin <- tapply(bdd$riskscore, bins, mean)
mean_plan     <- tapply(bdd$plan,      bins, mean)

# Color por lado del umbral
colores <- ifelse(riskscore_bin < 620, col_left, col_right)

plot(riskscore_bin, mean_plan,
     xlab = "RiskScore",
     ylab = "Pr(Plan = 1)",
     pch  = 16,
     col  = colores,
     main = "Probabilidad de adherirse al Plan vs Riskscore")

abline(v = 620, lty = 2)
legend("topright",
       legend = c("Lado izquierdo (RS < 620)", "Lado derecho (RS ≥ 620)"),
       col = c(col_left, col_right), pch = 16)

######
# b) #
######

# Creamos este subconjunto de datos para poder medir con mayor precisión
bdd_local <- subset(bdd, riskscore >= 570 & riskscore <= 670)

# Distancia al umbral e indicador Z
# c = 620
bdd_local$dist <- bdd_local$riskscore - 620
bdd_local$Z    <- ifelse(bdd_local$riskscore < 620, 1, 0)

# First stage local lineal
fs <- lm(plan ~ Z + dist + Z:dist, data = bdd_local)
summary(fs)


######
# c) #
######

breaks <- seq(500, 750, by = 5)
bins   <- cut(bdd$riskscore, breaks = breaks)

# Promedios por bin
riskscore_bin <- tapply(bdd$riskscore, bins, mean)
ingresos_bin  <- tapply(bdd$ingresos,  bins, mean)

# Colores por lado del umbral
colores <- ifelse(riskscore_bin < 620, col_left, col_right)

# Gráfico
plot(riskscore_bin, ingresos_bin,
     xlab = "RiskScore",
     ylab = "Ingresos promedio",
     pch  = 16,
     col  = colores,
     main = "Ingresos vs Riskscore")

abline(v = 620, lty = 2)
legend("topright",
       legend = c("RS < 620", "RS ≥ 620"),
       col    = c(col_left, col_right),
       pch    = 16)
