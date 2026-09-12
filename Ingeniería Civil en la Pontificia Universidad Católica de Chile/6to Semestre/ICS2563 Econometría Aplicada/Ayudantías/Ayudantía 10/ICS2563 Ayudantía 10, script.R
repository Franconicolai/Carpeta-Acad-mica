
# --- 0. Crear los datos ---

# Creamos los vectores de datos
distrito <- 1:10
Y_VShare_Dem <- c(39, 28, 34, 31, 27, 31, 45, 27, 23, 32)
X_Turnout <- c(63, 49, 59, 42, 58, 54, 65, 44, 52, 49)
Z_Lluvia <- c(0, 5, 1, 10, 0, 3, 0, 8, 2, 6)

# (Opcional) Combinarlos en un data.frame, que es más ordenado
datos <- data.frame(distrito, Y_VShare_Dem, X_Turnout, Z_Lluvia)

# --- 1. Análisis MCO (Naïve) ---
# Modelo: Y = beta_0 + beta_1*X + u

# Usamos la función lm() para Linear Model
modelo_mco <- lm(Y_VShare_Dem ~ X_Turnout, data = datos)

# Ver los resultados
print("--- 1. Modelo MCO (Naïve) ---")
summary(modelo_mco)

# Extraer el coeficiente beta_1 de MCO
beta_1_mco <- coef(modelo_mco)["X_Turnout"]
print(paste("Beta 1 MCO:", beta_1_mco)) # Debería ser aprox. +0.54


# --- 2. Análisis del Instrumento ---
# (Esta parte es conceptual, no requiere código)


# --- 3. Estimación por Variables Instrumentales (VI) ---

# a) Primera Etapa
# Modelo: X = gamma_0 + gamma_1*Z + v
primera_etapa <- lm(X_Turnout ~ Z_Lluvia, data = datos)

print("--- 3a. Primera Etapa (X ~ Z) ---")
summary(primera_etapa)

# Extraer el coeficiente gamma_1
gamma_1 <- coef(primera_etapa)["Z_Lluvia"]
print(paste("Gamma 1 (Efecto de Z en X):", gamma_1))

# b) Forma Reducida
# Modelo: Y = pi_0 + pi_1*Z + w
forma_reducida <- lm(Y_VShare_Dem ~ Z_Lluvia, data = datos)

print("--- 3b. Forma Reducida (Y ~ Z) ---")
summary(forma_reducida)

# Extraer el coeficiente pi_1
pi_1 <- coef(forma_reducida)["Z_Lluvia"]
print(paste("Pi 1 (Efecto de Z en Y):", pi_1)) 
s
# c) Estimador VI (Cálculo manual)
# beta_1_IV = pi_1 / gamma_1
beta_1_iv <- pi_1 / gamma_1

print("--- 3c. Estimador VI (Manual) ---")
print(paste("Beta 1 IV (pi_1 / gamma_1):", beta_1_iv))


# --- 4. Conclusión ---
print("--- 4. Comparación Final ---")
print(paste("Beta 1 MCO (Sesgado):", beta_1_mco))
print(paste("Beta 1 IV (Causal):  ", beta_1_iv))


#Desarrollo alternativo
num <- cov(Y_VShare_Dem, Z_Lluvia)
den <- cov(X_Turnout, Z_Lluvia)

estimador_iv <- num/den
print(estimador_iv)
