############################
##  AYUDANTÍA 5 - Script  ##
############################

##################
##  Pregunta 3  ##
##################

# Parámetros del DGP
set.seed(2563)
alpha <- 1
beta  <- 2
n     <- 1000

# a) DGP sin error de medida
X_star <- rnorm(n, mean = 0, sd = 1)
eps    <- rnorm(n, mean = 0, sd = 1)
Y_star <- alpha + beta*X_star + eps

# Estimación MCO sin error
model_true <- lm(Y_star ~ X_star)
summary(model_true)

# b) Error clásico en Y ----------------------------
sigma_v2 <- 4
v <- rnorm(n, mean = 0, sd = sqrt(sigma_v2))
Y_errY <- Y_star + v
model_errY <- lm(Y_errY ~ X_star)
summary(model_errY)

# c) Error clásico en X ----------------------------
sigma_u2 <- 4
u <- rnorm(n, mean = 0, sd = sqrt(sigma_u2))
X_errX <- X_star + u
model_errX <- lm(Y_star ~ X_errX)
summary(model_errX)

# d) Repeticiones para distribuciones --------------
R <- 1000
betas_true <- numeric(R)
betas_errY <- numeric(R)
betas_errX <- numeric(R)

for (r in 1:R) {
  idx <- sample(1:n, n, replace = TRUE)  # submuestreo
  
  # Caso sin error
  betas_true[r] <- coef(lm(Y_star[idx] ~ X_star[idx]))[2]
  
  # Caso error en Y
  betas_errY[r] <- coef(lm(Y_errY[idx] ~ X_star[idx]))[2]
  
  # Caso error en X
  betas_errX[r] <- coef(lm(Y_star[idx] ~ X_errX[idx]))[2]
}

dens_true <- density(betas_true)
dens_errY <- density(betas_errY)
dens_errX <- density(betas_errX)

# Calcular medias
m_true  <- mean(betas_true)
m_errY  <- mean(betas_errY)
m_errX  <- mean(betas_errX)

# Ajustar rangos de gráfico
x_min <- min(dens_true$x, dens_errY$x, dens_errX$x)
x_max <- max(dens_true$x, dens_errY$x, dens_errX$x)
y_max <- max(dens_true$y, dens_errY$y, dens_errX$y)

# Gráfico con densidades y líneas de medias
plot(dens_true, col = "blue", lwd = 2,
     main = "Distribución de β bajo distintos escenarios",
     xlab = "Estimador β", ylim = c(0, y_max),
     xlim = c(x_min, x_max))
lines(dens_errY, col = "green", lwd = 2)
lines(dens_errX, col = "red", lwd = 2)

# Línea del valor verdadero
abline(v = beta, lty = 2, lwd = 2, col = "black")

# Líneas en las medias de cada distribución
abline(v = m_true, col = "blue", lwd = 2, lty = 3)
abline(v = m_errY, col = "green", lwd = 2, lty = 3)
abline(v = m_errX, col = "red", lwd = 2, lty = 3)

# Leyenda
legend("top", 
       legend = c("Sin error", "Error en Y", "Error en X", "β verdadero"),
       col = c("blue", "green", "red", "black"), 
       lwd = 2, lty = c(1,1,1,2))
