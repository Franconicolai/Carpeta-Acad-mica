##########################
## AYUDANTÍA 11 - SCRIPT ##
##########################

################
## Preliminar ##
################

library(AER)
library(dplyr)
library(fixest)

# Leemos los datos
ak <- rio::import("iv_angrist_krueger.csv")

################
## PREGUNTA 3 ##  
################

# ---- (a) Wald con controles por cohorte (versión "ILS") ----
# Reduced form: y ~ z + cohort
dat <- ak
rf   <- lm(y ~ z + cohort, data = dat)
# First stage: s ~ z + cohort
fs   <- lm(s ~ z + cohort, data = dat)
wald <- coef(rf)["z"] / coef(fs)["z"]

# ---- (b) 2SLS (cohort FE) ----
iv1  <- ivreg(y ~ s + cohort | z + cohort, data = dat)     # AER
iv2  <- feols(y ~ 1 | cohort | s ~ z, data = dat)          # fixest (equivalente con FE)

# ---- Reporte breve ----
list(
  OLS      = coef(lm(y ~ s + cohort, data = dat))["s"],
  FirstStage_pi = coef(fs)["z"],
  RF_delta = coef(rf)["z"],
  Wald     = wald,
  `2SLS (AER)` = coef(iv1)["s"],
  `2SLS (fixest)` = coef(iv2)["fit_s"]
)



