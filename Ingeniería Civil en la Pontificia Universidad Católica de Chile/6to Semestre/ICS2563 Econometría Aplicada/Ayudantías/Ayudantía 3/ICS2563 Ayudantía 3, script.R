install.packages("wooldridge")
library(wooldridge)
library(dplyr)

datos1 <- datos4

attach(datos1)
Invest=as.numeric(Invest)
CPI=as.numeric(CPI)
GNP=as.numeric(GNP)
Interest=as.numeric(Interest)
rInvest = Invest/CPI
rInvest = rInvest[2:15]
rGNP = GNP/CPI
rGNP = rGNP[2:15]
inflation = (CPI[2:15]-CPI[1:14])/CPI[1:14]
interest = Interest[2:15]
Trend = c(2:15)
View(datos1)
X1 = cbind(1,Trend)
X2 = cbind(rGNP, interest, inflation)
X = cbind(X1,X2)
X

#Buscamos los coeficientes con una sola regresión

solve(t(X)%*%X)%*%t(X)%*%rInvest

#Ahora buscamos coeficientes ocupando FWL

#Modelo 1

coef_1 = solve(t(X1)%*%X1)%*%t(X1)%*%rInvest
u1 = rInvest - X1%*%coef_1 
u1 #residuos Y ~ X1
coef_1

#Modelo 2

coef_2 = solve(t(X1)%*%X1)%*%t(X1)%*%X2
u2 = X2 - X1%*%coef_2
u2 #residuos X2 ~ X1
coef_2

# Modelo 3, con los residuos del modelo 1 y 2

beta2 = solve(t(u2)%*%u2)%*%t(u2)%*%u1 
beta2 #u1 ~ u2

#Buscamos beta_1 (Trend)
u_3 = rInvest - X2 %*% beta2
u_3

beta1 = solve(t(X1)%*%X1)%*%t(X1)%*%u_3
beta1


#Juntamos los coeficientes, como si fuera una sola regresión
beta = c(beta1, beta2)
beta

