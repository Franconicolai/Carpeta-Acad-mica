############################
##  AYUDANTÍA 6 - Script  ##
############################

library(haven)
library(tableone)

##################
##  Pregunta 3  ##
##################

salud_programa <- read_dta("salud_programa.dta")

########
## a) ##
########

# Media de salud en tratados y controles
media_tratados_Treat1   <- mean(salud_programa$Salud[salud_programa$Treat1 == 1])
media_controles_Treat1  <- mean(salud_programa$Salud[salud_programa$Treat1 == 0])

# Diferencia
diff_Treat1 <- media_tratados_Treat1 - media_controles_Treat1

cat("Treat1 - Media tratados:", media_tratados_Treat1, "\n")
cat("Treat1 - Media controles:", media_controles_Treat1, "\n")
cat("Treat1 - Diferencia de medias:", diff_Treat1, "\n\n")
# 5.7722

########
## b) ##
########

summary(lm(Salud ~ Treat1, data = salud_programa))
# 5.7722

########
## c) ##
########

summary(lm(Salud ~ Treat1 + Edad + Ingreso + Fuma, data = salud_programa))
# 5.141

########
## d) ##
########

# a)
# Media de salud en tratados y controles
media_tratados_Treat2   <- mean(salud_programa$Salud[salud_programa$Treat2 == 1])
media_controles_Treat2  <- mean(salud_programa$Salud[salud_programa$Treat2 == 0])

# Diferencia
diff_Treat2 <- media_tratados_Treat2 - media_controles_Treat2

cat("Treat2 - Media tratados:", media_tratados_Treat2, "\n")
cat("Treat2 - Media controles:", media_controles_Treat2, "\n")
cat("Treat2 - Diferencia de medias:", diff_Treat2, "\n")
# 17.4508

# b)
summary(lm(Salud ~ Treat2, data = salud_programa))
# 17.4508

# c)
summary(lm(Salud ~ Treat2 + Edad + Ingreso + Fuma, data = salud_programa))
# -0.07861
# No es significativo

########
## e) ##
########

# Tablas de balance
vars_balance <- c("Edad", "Ingreso", "Fuma")

CreateTableOne(vars = vars_balance, strata = "Treat1", data = salud_programa)
CreateTableOne(vars = vars_balance, strata = "Treat2", data = salud_programa)

