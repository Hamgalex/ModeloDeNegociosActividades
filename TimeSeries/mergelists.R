rm(list = ls())

##############################################################
# PASO 1. LIBRERÍAS
library(readr)
library(dplyr)
library(stringr)

##############################################################
# PASO 2. DEFINIR CARPETA Y ARCHIVO DE SALIDA

carpeta_series <- "C:/Users/hamga/Documents/repo/ModeloDeNegociosActividades/TimeSeries/fulllist"
archivo_salida <- "C:/Users/hamga/Documents/repo/ModeloDeNegociosActividades/TimeSeries/series_unidas.csv"

##############################################################
# PASO 3. LEER TODOS LOS CSV Y UNIRLOS

archivos <- list.files(carpeta_series, pattern = "\\.csv$", full.names = TRUE)

if (length(archivos) == 0) {
  stop("No se encontraron archivos CSV en la carpeta especificada.")
}

num_archivos <- length(archivos)

lista_df <- lapply(seq_along(archivos), function(i) {
  df <- read_csv(archivos[i], show_col_types = FALSE)
  
  # Asignar 1 o 2 según el orden o solo 1 si hay un archivo
  if (num_archivos == 1) {
    df$Producto <- 1
  } else {
    df$Producto <- i
  }
  
  # Obtener nombre de archivo sin extensión
  nombre_archivo <- basename(archivos[i])
  nombre_sin_csv <- str_remove(nombre_archivo, "\\.csv$")
  df$NombreArchivo <- nombre_sin_csv
  
  return(df)
})

df_total <- bind_rows(lista_df)

##############################################################
# PASO 4. GUARDAR CSV UNIFICADO

write.csv(df_total, archivo_salida, row.names = FALSE)
cat("Archivos combinados guardados en:", archivo_salida, "\n")

# Vista previa
print(head(df_total))
