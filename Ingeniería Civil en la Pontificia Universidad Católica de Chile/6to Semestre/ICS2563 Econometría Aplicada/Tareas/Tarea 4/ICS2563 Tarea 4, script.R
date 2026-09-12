# --- Importación de módulos y base de datos -----------------------------------
install.packages("haven")
library(haven)
install.packages("dplyr")
library(dplyr)
install.packages("knitr")
library(knitr)
set.seed(123)

linea_base <- read_dta("linea_base_t4.dta") 
linea_salida <- read_dta("linea_salida_t4.dta") 
tratamiento <- read_dta("tratamiento_t4.dta") 

# --- Pregunta 1 ---------------------------------------------------------------
limpieza_seccion = ls()
objetos_a_mantener <- c("linea_base", "linea_salida", "tratamiento")
rm(list = setdiff(ls(), objetos_a_mantener))

# --- Pregunta 1, ítem a
modelo_a <- lm(disrupcion ~ n_disrup, data = linea_base)
print(summary(modelo_a))

# --- Pregunta 1, ítem b)
modelo_b <- lm(disrupcion ~ n_disrup + n_curso + prof_mujer, data = linea_base)
print(summary(modelo_b))

# --- Pregunta 1, ítem c)
mediana_disruptivos <- median(linea_base$n_disrup, na.rm = TRUE)

estadisticas_descriptivas <- linea_base %>%
  mutate(
    grupo = if_else(n_disrup > mediana_disruptivos,
                    "Alta proporción de estudiantes disruptivos",
                    "Baja proporción de estudiantes disruptivos")
  ) %>%
  group_by(grupo) %>%
  summarise(
    N_Colegios = n(),
    Disrupcion_Promedio = mean(disrupcion, na.rm = TRUE),
    Atraso_Promedio_Minutos = mean(atraso, na.rm = TRUE)
  )

print(
  kable(estadisticas_descriptivas,
        format = "pipe",
        caption = "Resultados")
)

# --- Pregunta 3 ---------------------------------------------------------------
objetos_a_mantener <- c("linea_base", "linea_salida", "tratamiento")
rm(list = setdiff(ls(), objetos_a_mantener))

linea_base <- linea_base %>% mutate(periodo = 0) 
linea_salida <- linea_salida %>% mutate(periodo = 1)

panel_data <- bind_rows(linea_base, linea_salida) %>% 
  left_join(tratamiento, by = "rbd_curso")

# --- Pregunta 3, ítem a) y b)
modelo_did <- lm(atraso ~ tratamiento * periodo, data = panel_data)
print(summary(modelo_did))

# --- Pregunta 3, ítem c)
modelo_ddd <- lm(atraso ~ tratamiento * periodo * prof_mujer, data = panel_data)
print(summary(modelo_ddd))

# --- Pregunta 3, ítem d
data_hombres <- panel_data %>%
  filter(prof_mujer == 0)

modelo_hombres <- lm(atraso ~ tratamiento * periodo, data = data_hombres)
print(summary(modelo_hombres))

data_mujeres <- panel_data %>%
  filter(prof_mujer == 1)

modelo_mujeres <- lm(atraso ~ tratamiento * periodo, data = data_mujeres)
print(summary(modelo_mujeres))
