suppressPackageStartupMessages({
  library(tidyverse)
  library(forecast)
  library(zoo)   # as.yearmon
})


setwd("C:/Users/hamga/Documents/repo/ModeloDeNegociosActividades/hierarchy/dataset")

archivos <- c("banamex.csv", "banorte.csv", "BBVA.csv", "hsbc.csv", "santander.csv")
bancos   <- c("Banamex", "Banorte", "BBVA", "HSBC", "Santander")

esperadas <- c("Periodo",
               "credito_comercial",
               "credito_vivienda",
               "cartera_de_creditos",
               "caja",
               "banco",
               "efectivo")

leer_archivo <- function(path, nombre_banco){
  df <- read.csv(path, check.names = FALSE, stringsAsFactors = FALSE)
  faltan <- setdiff(esperadas, names(df))
  if (length(faltan) > 0) {
    stop(sprintf("En '%s' faltan columnas: %s", path, paste(faltan, collapse=", ")))
  }
  df <- df[, esperadas]
  df$Periodo <- as.Date(paste0(as.character(df$Periodo), "01"), format = "%Y%m%d")
  df$Banco <- nombre_banco
  df
}

lista <- lapply(seq_along(archivos), function(i){
  leer_archivo(archivos[i], bancos[i])
})

df <- bind_rows(lista) %>%
  arrange(Periodo, Banco)

df <- df %>%
  rename(banco_monto = banco)


variables_hoja <- c("caja", "banco_monto", "credito_comercial", "credito_vivienda")
variables_intermedio <- c("efectivo", "cartera_de_creditos")
variables_total <- c("Total")

df <- df %>%
  mutate(Total = efectivo + cartera_de_creditos)


names(df)


##### HOJAS #####
resultados_hojas <- list()

for (banco in unique(df$Banco)) {
  for (var in variables_hoja) {
    subserie <- df %>% filter(Banco == banco) %>% pull(var)
    fechas   <- df %>% filter(Banco == banco) %>% pull(Periodo)
    
    if (length(subserie) == 0 || all(is.na(subserie))) {
      message("⚠️ Serie vacía (hojas) para Banco: ", banco, " Variable: ", var)
      next
    }
    
    start_year  <- as.numeric(format(min(fechas), "%Y"))
    start_month <- as.numeric(format(min(fechas), "%m"))
    
    ts_data <- ts(subserie, frequency = 12, start = c(start_year, start_month))
    modelo <- auto.arima(ts_data, D = 1, seasonal = TRUE)
    fc <- forecast(modelo, h = 30)
    
    pronos <- tibble(
      Periodo = as.Date(c(
        as.yearmon(time(ts_data)),
        as.yearmon(time(fc$mean))
      )),
      Valor = c(as.numeric(ts_data), as.numeric(fc$mean)),
      Tipo = c(rep("Historico", length(ts_data)), rep("Pronostico", 30)),
      Banco = banco,
      Variable = var,
      Nivel = "Hoja"
    )
    
    resultados_hojas[[paste(banco, var, sep = "_")]] <- pronos
  }
}

##### INTERMEDIOS #####
resultados_intermedios <- list()

for (banco in unique(df$Banco)) {
  for (var in variables_intermedio) {
    subserie <- df %>% filter(Banco == banco) %>% pull(var)
    fechas   <- df %>% filter(Banco == banco) %>% pull(Periodo)
    
    if (length(subserie) == 0 || all(is.na(subserie))) {
      message("⚠️ Serie vacía (intermedios) para Banco: ", banco, " Variable: ", var)
      next
    }
    
    start_year  <- as.numeric(format(min(fechas), "%Y"))
    start_month <- as.numeric(format(min(fechas), "%m"))
    
    ts_data <- ts(subserie, frequency = 12, start = c(start_year, start_month))
    modelo <- auto.arima(ts_data, D = 1, seasonal = TRUE)
    fc <- forecast(modelo, h = 30)
    
    pronos <- tibble(
      Periodo = as.Date(c(
        as.yearmon(time(ts_data)),
        as.yearmon(time(fc$mean))
      )),
      Valor = c(as.numeric(ts_data), as.numeric(fc$mean)),
      Tipo = c(rep("Historico", length(ts_data)), rep("Pronostico", 30)),
      Banco = banco,
      Variable = var,
      Nivel = "Intermedio"
    )
    
    resultados_intermedios[[paste(banco, var, sep = "_")]] <- pronos
  }
}

##### TOTALES #####
resultados_total <- list()

for (banco in unique(df$Banco)) {
  for (var in variables_total) {
    subserie <- df %>% filter(Banco == banco) %>% pull(var)
    fechas   <- df %>% filter(Banco == banco) %>% pull(Periodo)
    
    if (length(subserie) == 0 || all(is.na(subserie))) {
      message("⚠️ Serie vacía (total) para Banco: ", banco, " Variable: ", var)
      next
    }
    
    start_year  <- as.numeric(format(min(fechas), "%Y"))
    start_month <- as.numeric(format(min(fechas), "%m"))
    
    ts_data <- ts(subserie, frequency = 12, start = c(start_year, start_month))
    modelo <- auto.arima(ts_data, D = 1, seasonal = TRUE)
    fc <- forecast(modelo, h = 30)
    
    pronos <- tibble(
      Periodo = as.Date(c(
        as.yearmon(time(ts_data)),
        as.yearmon(time(fc$mean))
      )),
      Valor = c(as.numeric(ts_data), as.numeric(fc$mean)),
      Tipo = c(rep("Historico", length(ts_data)), rep("Pronostico", 30)),
      Banco = banco,
      Variable = var,
      Nivel = "Total"
    )
    
    resultados_total[[paste(banco, var, sep = "_")]] <- pronos
  }
}


### UNIR

df_forecast <- bind_rows(
  bind_rows(resultados_hojas),
  bind_rows(resultados_intermedios),
  bind_rows(resultados_total)
) %>%
  arrange(Nivel, Banco, Variable, Periodo)

write.csv(
  df_forecast,
  "C:/Users/hamga/Documents/repo/ModeloDeNegociosActividades/hierarchy/forecast_sarima2.csv",
  row.names = FALSE
)




############################################################
# PRONÓSTICO JERÁRQUICO — RECONCILIACIÓN OLS
# Héctor — versión completa lista para pegar
############################################################

suppressPackageStartupMessages({
  library(tidyverse)
  library(forecast)
  library(zoo)
})

# --- 1. Configuración base ---
setwd("C:/Users/hamga/Documents/repo/ModeloDeNegociosActividades/hierarchy/dataset")

archivos <- c("banamex.csv", "banorte.csv", "BBVA.csv", "hsbc.csv", "santander.csv")
bancos   <- c("Banamex", "Banorte", "BBVA", "HSBC", "Santander")

esperadas <- c(
  "Periodo",
  "credito_comercial",
  "credito_vivienda",
  "cartera_de_creditos",
  "caja",
  "banco",
  "efectivo"
)

# --- 2. Leer archivos ---
leer_archivo <- function(path, nombre_banco){
  df <- read.csv(path, check.names = FALSE, stringsAsFactors = FALSE)
  faltan <- setdiff(esperadas, names(df))
  if (length(faltan) > 0) {
    stop(sprintf("En '%s' faltan columnas: %s", path, paste(faltan, collapse=", ")))
  }
  df <- df[, esperadas]
  df$Periodo <- as.Date(paste0(as.character(df$Periodo), "01"), format = "%Y%m%d")
  df$Banco <- nombre_banco
  df
}

lista <- lapply(seq_along(archivos), function(i){
  leer_archivo(archivos[i], bancos[i])
})

df <- bind_rows(lista) %>%
  arrange(Periodo, Banco) %>%
  rename(banco_monto = banco) %>%
  mutate(Total = efectivo + cartera_de_creditos)

# --- 3. Definir jerarquía ---
variables_hoja <- c("caja", "banco_monto", "credito_comercial", "credito_vivienda")
variables_intermedio <- c("efectivo", "cartera_de_creditos")
variables_total <- c("Total")

orden_vars <- c(
  variables_hoja,
  variables_intermedio,
  variables_total
)

# --- 4. Matriz S ---
S <- matrix(c(
  1,0,0,0,  # caja
  0,1,0,0,  # banco_monto
  0,0,1,0,  # credito_comercial
  0,0,0,1,  # credito_vivienda
  1,1,0,0,  # efectivo
  0,0,1,1,  # cartera
  1,1,1,1   # total
), nrow = 7, byrow = TRUE)

# --- 5. Reconciliación OLS ---
resultados_ols <- list()

for (banco in unique(df$Banco)) {
  
  df_banco <- df %>% filter(Banco == banco)
  
  # Fechas iniciales para SARIMA en hojas
  start_year  <- as.numeric(format(min(df_banco$Periodo), "%Y"))
  start_month <- as.numeric(format(min(df_banco$Periodo), "%m"))
  
  pronos_hojas <- list()
  ts_length <- NULL
  fc_length <- NULL
  
  # --- 5.1 Pronóstico SARIMA en hojas ---
  for (var in variables_hoja) {
    subserie <- df_banco %>% pull(var)
    ts_data <- ts(subserie, frequency = 12, start = c(start_year, start_month))
    modelo <- auto.arima(ts_data, D = 1, seasonal = TRUE)
    fc <- forecast(modelo, h = 30)
    pronos_hojas[[var]] <- c(as.numeric(ts_data), as.numeric(fc$mean))
    if (is.null(ts_length)) ts_length <- length(ts_data)
    if (is.null(fc_length)) fc_length <- length(fc$mean)
  }
  
  # --- 5.2 Construir base temporal ---
  base_df <- tibble(
    Periodo = as.Date(c(
      as.yearmon(time(ts_data)),
      as.yearmon(time(fc$mean))
    )),
    Tipo = c(
      rep("Historico", ts_length),
      rep("Pronostico_OLS", fc_length)
    )
  )
  
  for (var in variables_hoja) {
    base_df[[var]] <- pronos_hojas[[var]]
  }
  
  # --- 5.3 Reconciliar jerarquía ---
  pronos_reconciliados <- base_df %>%
    rowwise() %>%
    mutate(
      reconc = list(as.numeric(S %*% matrix(c_across(all_of(variables_hoja)), ncol = 1)))
    ) %>%
    ungroup() %>%
    unnest_wider(reconc, names_sep = "_")
  
  colnames(pronos_reconciliados)[grep("reconc_", names(pronos_reconciliados))] <- orden_vars
  
  # --- 5.4 Pasar a formato largo ---
  df_long <- pronos_reconciliados %>%
    pivot_longer(
      cols = all_of(orden_vars),
      names_to = "Variable",
      values_to = "Valor"
    ) %>%
    mutate(
      Banco = banco,
      Nivel = case_when(
        Variable %in% variables_hoja ~ "Hoja",
        Variable %in% variables_intermedio ~ "Intermedio",
        Variable == "Total" ~ "Total"
      )
    ) %>%
    select(Periodo, Banco, Variable, Nivel, Tipo, Valor)
  
  resultados_ols[[banco]] <- df_long
}

# --- 6. Unir todos los bancos ---
df_forecast_ols <- bind_rows(resultados_ols) %>%
  arrange(Periodo, Banco, Nivel, Variable)

# --- 7. Exportar a CSV final ---
write.csv(
  df_forecast_ols,
  "C:/Users/hamga/Documents/repo/ModeloDeNegociosActividades/hierarchy/forecast_OLS.csv",
  row.names = FALSE
)

############################################################
# FIN DEL SCRIPT OLS ✅
############################################################

df_forecast_ols <- df_forecast_ols %>% distinct()
