# ===============================
# 1. Librerías necesarias
# ===============================
library(readr)
library(dplyr)
library(lme4)
library(tidyr)

# ===============================
# 2. Leer y preparar datos
# ===============================

datos <- read_csv("C:/Users/hamga/Documents/repo/ModeloDeNegociosActividades/MLR/dataset/life.csv")


# Limpiar nombres de columnas
colnames(datos) <- gsub(" ", "_", colnames(datos))
colnames(datos) <- gsub("-", "_", colnames(datos))
colnames(datos) <- gsub("/", "_", colnames(datos))

# Convertir variables categóricas en factores
datos <- datos %>%
  mutate(
    Country = as.factor(Country),
    Year = as.factor(Year)
  ) %>%
  drop_na()

# ===============================
# 3. Escalar las variables numéricas
# ===============================
cols_to_scale <- c(
  "Adult_Mortality", "infant_deaths", "Alcohol", "percentage_expenditure",
  "Hepatitis_B", "Measles", "BMI", "under_five_deaths",
  "Polio", "Total_expenditure", "Diphtheria", "HIV_AIDS", "GDP",
  "Population", "thinness__1_19_years", "thinness_5_9_years",
  "Income_composition_of_resources", "Schooling"
)

datos_scaled <- datos %>%
  mutate(across(all_of(cols_to_scale), ~ as.numeric(scale(.))))

# ===============================
# 4. Modelo lineal múltiple
# ===============================
modelo_lm <- lm(
  Life_expectancy ~ Adult_Mortality + infant_deaths + Alcohol +
    percentage_expenditure + Hepatitis_B + Measles + BMI + under_five_deaths +
    Polio + Total_expenditure + Diphtheria + HIV_AIDS + GDP + Population +
    thinness__1_19_years + thinness_5_9_years +
    Income_composition_of_resources + Schooling,
  data = datos_scaled
)

pred_lineal <- predict(modelo_lm, newdata = datos_scaled)

# ===============================
# 5. Modelo mixto — solo intercepto aleatorio
# ===============================
modelo_mlm_intercepto <- lmer(
  Life_expectancy ~ Adult_Mortality + infant_deaths + Alcohol +
    percentage_expenditure + Hepatitis_B + Measles + BMI + under_five_deaths +
    Polio + Total_expenditure + Diphtheria + HIV_AIDS + GDP + Population +
    thinness__1_19_years + thinness_5_9_years +
    Income_composition_of_resources + Schooling +
    (1 | Country) + (1 | Year),
  data = datos_scaled
)

pred_mlm_intercepto <- predict(modelo_mlm_intercepto, newdata = datos_scaled)

# ===============================
# 6. Modelo mixto — intercepto + pendiente aleatoria
# (para Income_composition_of_resources)
# ===============================
modelo_mlm_intercepto_pendiente <- lmer(
  Life_expectancy ~ Adult_Mortality + infant_deaths + Alcohol +
    percentage_expenditure + Hepatitis_B + Measles + BMI + under_five_deaths +
    Polio + Total_expenditure + Diphtheria + HIV_AIDS + GDP + Population +
    thinness__1_19_years + thinness_5_9_years +
    Income_composition_of_resources + Schooling +
    (1 + Income_composition_of_resources | Country) + (1 | Year),
  data = datos_scaled
)

pred_mlm_intercepto_pendiente <- predict(modelo_mlm_intercepto_pendiente, newdata = datos_scaled)

# ===============================
# 7. Exportar predicciones para Power BI
# ===============================
resultados <- datos_scaled %>%
  mutate(
    pred_lineal = pred_lineal,
    pred_mixto_intercepto = pred_mlm_intercepto,
    pred_mixto_intercepto_pendiente = pred_mlm_intercepto_pendiente,
    error_lineal = Life_expectancy - pred_lineal,
    error_mixto_intercepto = Life_expectancy - pred_mixto_intercepto,
    error_mixto_intercepto_pendiente = Life_expectancy - pred_mixto_intercepto_pendiente
  )

write_csv(resultados, "predicciones_modelos_lineal_vs_mixtos.csv")
