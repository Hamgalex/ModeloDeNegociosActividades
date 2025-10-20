library(forecast)
library(ggplot2)

# --- Ruta de salida ---
archivo <- "C:/Users/hamga/Documents/repo/ModeloDeNegociosActividades/TimeSeries/series_unidas.csv"

# --- 1. Convertir fecha y ordenar ---
df$Fecha <- as.Date(df$Fecha, format = "%d/%m/%Y")
df <- df[order(df$Fecha), ]

# --- 2. Crear serie de tiempo ---
ts_prod <- ts(df$Ventas, frequency = 52)

# --- 3. Ajustar modelo ARIMA ---
modelo <- auto.arima(ts_prod, D = 1)

# --- 4. Pronóstico a 143 semanas ---
fc <- forecast(modelo, h = 143)

# --- 5. Fechas futuras ---
ultima_fecha <- max(df$Fecha)
fechas_fc <- seq(from = ultima_fecha + 7, by = 7, length.out = 143)

# --- 6. Crear data frames ---
df_hist <- data.frame(
  Fecha = df$Fecha,
  Ventas = df$Ventas,
  Lower = NA,
  Upper = NA,
  Tipo = "Historico"
)

df_pred <- data.frame(
  Fecha = fechas_fc,
  Ventas = as.numeric(fc$mean),
  Lower = as.numeric(fc$lower[, 2]),
  Upper = as.numeric(fc$upper[, 2]),
  Tipo = "Pronostico"
)

df_final <- rbind(df_hist, df_pred)

# --- 7. Guardar CSV en la ruta especificada ---
write.csv(df_final, archivo, row.names = FALSE)
cat("✅ Archivo guardado en:", archivo, "\n")

# --- 8. 📊 Gráfica ---
autoplot(fc) +
  autolayer(ts_prod, series = "Histórico") +
  ggtitle("Ventas - Histórico y Pronóstico") +
  xlab("Tiempo") + ylab("Ventas") +
  theme_minimal()

