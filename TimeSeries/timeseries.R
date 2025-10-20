rm(list = ls())

# --- LIBRERÍAS NECESARIAS ---
library(readr)
library(forecast)
library(stringr)

# --- PARÁMETROS ---
archivo <- "C:/Users/hamga/Downloads/competidorescarros/caso4/Competidor10.csv"
carpeta_base <- "C:/Users/hamga/Documents/repo/ModeloDeNegociosActividades/TimeSeries"
competidor_id <- "Ford"

# Crear carpeta de salida
carpeta_salida <- file.path(carpeta_base, competidor_id)
if (!dir.exists(carpeta_salida)) dir.create(carpeta_salida, recursive = TRUE)

# --- LEER Y LIMPIAR DATOS ---
df <- read_csv(archivo, show_col_types = FALSE)
df <- df[, !grepl("^\\.\\.\\.", names(df))]  # quitar columnas basura
df$YMD <- as.Date(as.character(df$YMD), format = "%Y%m%d")
if (all(is.na(df$YMD))) stop("Error en conversión de fechas")

productos <- setdiff(names(df), c("YMD", "time"))

# --- PRONÓSTICO ---
for (producto in productos) {
  serie <- as.numeric(df[[producto]])
  
  if (all(is.na(serie)) || all(serie == 0)) {
    cat(" Serie vacía o inválida:", producto, "\n")
    next
  }
  
  ultima_fecha <- max(df$YMD, na.rm = TRUE)
  if (!is.finite(ultima_fecha)) {
    cat("echa inválida para:", producto, "\n")
    next
  }
  
  ts_prod <- ts(serie, frequency = 52)
  modelo <- tryCatch(auto.arima(ts_prod, D = 1), error = function(e) NULL)
  if (is.null(modelo)) {
    cat("o se pudo ajustar ARIMA(D=1) para:", producto, "\n")
    next
  }
  
  fc <- forecast(modelo, h = 143)  # ~1000 días
  fechas_fc <- seq(ultima_fecha + 7, by = 7, length.out = 143)
  
  # --- UNIR DATOS HISTÓRICOS Y PRONOSTICADOS ---
  df_hist <- data.frame(
    Fecha = df$YMD,
    Ventas = serie,
    Tipo = "Historico",
    Competidor = competidor_id
  )
  
  df_forecast <- data.frame(
    Fecha = fechas_fc,
    Ventas = round(as.numeric(fc$mean), 2),
    Tipo = "Pronosticado",
    Competidor = competidor_id
  )
  
  df_completo <- rbind(df_hist, df_forecast)
  
  # --- GUARDAR CSV UNIFICADO ---
  nombre_archivo <- paste0("serie_", producto, "_Toyota.csv")
  write.csv(df_completo, file.path(carpeta_salida, nombre_archivo), row.names = FALSE)
  
  cat("SV generado para:", producto, "\n")
}
