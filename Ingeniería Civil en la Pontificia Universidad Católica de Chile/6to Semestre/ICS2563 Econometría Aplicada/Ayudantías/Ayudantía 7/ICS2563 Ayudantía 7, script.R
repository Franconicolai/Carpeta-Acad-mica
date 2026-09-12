##########################
## AYUDANTÍA 7 - SCRIPT ##
##########################

################
## Preliminar ##
################

bdd <- rio::import("empleo_beca.csv")

# Creamos una función para simular
simular <- function(formula, data, N_iter=500, n=80){
  betas <- numeric(N_iter)
  for (i in 1:N_iter){
    sub <- data[sample(1:nrow(data), n, replace=FALSE), ]
    mod <- lm(formula, data=sub)
    betas[i] <- coef(mod)["treat"]
  }
  return(betas)
}


################
## PREGUNTA 2 ##  
################

#######################
## Preguntas a, b, c ##
#######################

betas_a <- simular(salario_base ~ treat, bdd)
betas_b <- simular(salario_base ~ treat + educ_padres, bdd)
betas_c <- simular(salario_base ~ treat + educ_padres + distancia, bdd)


# Gráficos
plot(density(betas_a), col="red", lwd=2,
     main="Densidad de kernel de coeficientes",
     xlab="Coeficiente", ylim=c(0, max(density(betas_a)$y,
                                       density(betas_b)$y,
                                       density(betas_c)$y)))
lines(density(betas_b), col="darkgreen", lwd=2)
lines(density(betas_c), col="blue", lwd=2)

abline(v=mean(betas_a), col="red", lty=2, lwd=2)
abline(v=mean(betas_b), col="darkgreen", lty=2, lwd=2)
abline(v=mean(betas_c), col="blue", lty=2, lwd=2)

legend("topright", legend=c("Sin covariables",
                            "Con educ_padres",
                            "Con educ_padres + distancia"),
       col=c("red","darkgreen","blue"), lwd=2)



#################
## Pregunta e) ##
#################

# Creamos una función para simular treat_sesgado
simular_sesgado <- function(formula, data, N_iter=500, n=80){
  betas <- numeric(N_iter)
  for (i in 1:N_iter){
    sub <- data[sample(1:nrow(data), n, replace=FALSE), ]
    mod <- lm(formula, data=sub)
    betas[i] <- coef(mod)["treat_sesgado"] # --> Solo cambia esto
  }
  return(betas)
}

# Simulaciones
betas_e_simple <- simular_sesgado(salario_sesgado ~ treat_sesgado, bdd, N_iter=500, n=80)
betas_e_covs   <- simular_sesgado(salario_sesgado ~ treat_sesgado + educ_padres + distancia, 
                          bdd, N_iter=500, n=80)

# Gráfico comparativo
plot(density(betas_e_covs), col="red", lwd=2,
     main="Escenario sesgado: densidad de coeficientes",
     xlab="Coeficiente de Treat (sesgado)")
lines(density(betas_e_simple), col="blue", lwd=2)

abline(v=mean(betas_e_covs), col="red", lty=2, lwd=2)
abline(v=mean(betas_e_simple), col="blue", lty=2, lwd=2)

legend("topright", legend=c("Regresión simple", "Regresión con covariables"),
       col=c("blue","red"), lwd=2)




################
## PREGUNTA 3 ##  
################

# Librería útil
library(tableone)

# Variables de balance
vars <- c("edad", "educ_padres", "distancia")

# (a) Balance con asignación aleatoria
tab_base <- CreateTableOne(vars = vars, strata = "treat", data = bdd)
print(tab_base, smd = TRUE)

# (b) Balance con asignación sesgada
tab_sesgado <- CreateTableOne(vars = vars, strata = "treat_sesgado", data = bdd)
print(tab_sesgado, smd = TRUE)

