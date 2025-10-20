# ============================
# 1) Librerías
# ============================
suppressPackageStartupMessages({
  library(tidyverse)
  library(forecast)
  library(zoo)
})

# ============================
# 2) Lectura de archivos
# ============================
setwd("C:/Users/hamga/Documents/repo/ModeloDeNegociosActividades/hierarchy/dataset")

archivos <- c("banamex.csv", "banorte.csv", "BBVA.csv", "hsbc.csv", "santander.csv")
bancos   <- c("Banamex", "Banorte", "BBVA", "HSBC", "Santander")

# Columnas esperadas
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

# ============================
# 3) Transformar a formato hoja
# ============================
df_long <- df %>%
  pivot_longer(
    cols = c(credito_comercial, credito_vivienda, caja, banco),
    names_to = "Categoria",
    values_to = "Valor"
  ) %>%
  unite("Leaf", Banco, Categoria, sep = "_") %>%
  pivot_wider(names_from = Leaf, values_from = Valor)

leaf_order <- as.vector(t(outer(bancos,
                                c("credito_comercial","credito_vivienda","caja","banco"),
                                paste, sep="_")))
leaf_cols <- intersect(leaf_order, names(df_long))

df_long <- df_long %>%
  mutate(PeriodoYM = as.yearmon(Periodo)) %>%
  select(PeriodoYM, all_of(leaf_cols)) %>%
  arrange(PeriodoYM)

# ============================
# 4) Imputar valores faltantes
# ============================
ym_seq <- seq(from = min(df_long$PeriodoYM),
              to   = max(df_long$PeriodoYM),
              by   = 1/12)

df_wide_full <- tibble(PeriodoYM = ym_seq) %>%
  left_join(df_long, by = "PeriodoYM") %>%
  arrange(PeriodoYM)

X <- as.matrix(df_wide_full %>% select(all_of(leaf_cols)))

X_imp <- apply(X, 2, function(col) {
  xt <- ts(col, frequency = 12,
           start = c(as.integer(format(as.Date(ym_seq[1]), "%Y")),
                     as.integer(format(as.Date(ym_seq[1]), "%m"))))
  as.numeric(na.interp(xt))
})
df_wide_full[, leaf_cols] <- X_imp

ts_base <- ts(X_imp,
              frequency = 12,
              start = c(as.integer(format(as.Date(ym_seq[1]), "%Y")),
                        as.integer(format(as.Date(ym_seq[1]), "%m"))))

# ============================
# 5) Construir matriz S jerárquica
# ============================
n_bancos <- length(bancos)
n_hojas_por_banco <- 4
n_bottom <- n_bancos * n_hojas_por_banco

idx_bank <- function(i){
  start <- (i-1)*n_hojas_por_banco + 1
  end   <- i*n_hojas_por_banco
  start:end
}

m <- 1 + n_bancos + 2*n_bancos + n_bottom
n <- n_bottom
S <- matrix(0, nrow = m, ncol = n)

row <- 1
# Total Global
S[row, ] <- 1; row <- row + 1

# Total por banco
for(i in 1:n_bancos){
  S[row, idx_bank(i)] <- 1; row <- row + 1
}

# Cartera = comercial + vivienda
for(i in 1:n_bancos){
  idx <- idx_bank(i)
  S[row, idx[1]] <- 1
  S[row, idx[2]] <- 1
  row <- row + 1
}

# Efectivo = caja + banco
for(i in 1:n_bancos){
  idx <- idx_bank(i)
  S[row, idx[3]] <- 1
  S[row, idx[4]] <- 1
  row <- row + 1
}

# Hojas
S[row:(row+n_bottom-1), ] <- diag(n_bottom)

row_names <- c(
  "Total",
  paste0(bancos, "_Total"),
  paste0(bancos, "_cartera_de_creditos"),
  paste0(bancos, "_efectivo"),
  leaf_cols
)
col_names <- leaf_cols
dimnames(S) <- list(row_names, col_names)

# ============================
# 6) Pronósticos SARIMA
# ============================
h <- 30
fit_arima <- function(x){
  auto.arima(x, D = 1)
}

# Hojas
fc_leaf <- lapply(1:ncol(ts_base), function(j){
  fit <- fit_arima(ts_base[, j])
  forecast(fit, h = h)
})
F_leaf <- t(sapply(fc_leaf, function(x) as.numeric(x$mean)))
rownames(F_leaf) <- col_names

# Total
Ymat <- ts_base %*% t(S)
fc_all <- lapply(1:ncol(Ymat), function(j){
  fit <- fit_arima(Ymat[, j])
  forecast(fit, h = h)
})
F_all <- t(sapply(fc_all, function(x) as.numeric(x$mean)))
rownames(F_all) <- row_names

# ============================
# 7) Reconciliación OLS
# ============================
Pinv <- solve(t(S) %*% S)
P    <- S %*% Pinv %*% t(S)
Yhat_OLS <- P %*% F_all

future_ym <- seq(from = tail(ym_seq, 1) + 1/12, by = 1/12, length.out = h)
future_cols <- format(as.Date(future_ym), "%Y-%m")

colnames(Yhat_OLS) <- future_cols
colnames(F_all)    <- future_cols
colnames(F_leaf)   <- future_cols

# ============================
# 8) CSV final para Power BI
# ============================
trad <- data.frame(Serie = rownames(F_leaf),  F_leaf, check.names = FALSE) %>%
  mutate(Metodo = "Tradicional")

ols  <- data.frame(Serie = rownames(Yhat_OLS), Yhat_OLS, check.names = FALSE) %>%
  mutate(Metodo = "OLS")

df_all_metodos <- bind_rows(trad, ols)

df_long <- df_all_metodos %>%
  pivot_longer(
    cols = matches("^[0-9]{4}-[0-9]{2}$"),
    names_to = "Periodo",
    values_to = "Valor"
  ) %>%
  mutate(Periodo = as.Date(paste0(Periodo, "-01")))

df_long <- df_long %>%
  separate(Serie, into = c("Banco", "Categoria"), sep = "_", fill = "right", remove = FALSE) %>%
  mutate(
    Banco = ifelse(Serie == "Total", NA_character_, Banco),
    Categoria = ifelse(Serie == "Total", "Total", Categoria),
    Nivel = case_when(
      Serie == "Total" ~ "Total",
      grepl("_Total$", Serie) ~ "Intermedio",
      Categoria %in% c("credito_comercial","credito_vivienda","caja","banco") ~ "Hoja",
      Categoria %in% c("cartera_de_creditos","efectivo") ~ "Intermedio",
      TRUE ~ "Otro"
    )
  )

write.csv(df_long, "pronosticos_todos_metodos.csv", row.names = FALSE)

# ============================
# 9) Prueba de consistencia OLS
# ============================
sum_bancos_ols <- sum(Yhat_OLS[paste0(bancos, "_Total"), ])
total_ols <- sum(Yhat_OLS["Total", ])
cat("DIFERENCIA TOTAL OLS:", sum_bancos_ols - total_ols, "\n")
