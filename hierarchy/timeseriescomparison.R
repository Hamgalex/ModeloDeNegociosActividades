# ============================
# 1) Librerías
# ============================
suppressPackageStartupMessages({
  library(tidyverse)
  library(forecast)
  library(zoo)   # as.yearmon
})

# ============================
# 2) Lectura de datos
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
# 3) Hojas (bottom level)
# ============================
df_long <- df %>%
  pivot_longer(
    cols = c(credito_comercial, credito_vivienda, caja, banco),
    names_to = "Categoria", values_to = "Valor"
  )

df_wide <- df_long %>%
  unite("Leaf", Banco, Categoria, sep = "_") %>%
  pivot_wider(names_from = Leaf, values_from = Valor)

leaf_order <- as.vector(t(outer(bancos,
                                c("credito_comercial","credito_vivienda","caja","banco"),
                                paste, sep="_")))
leaf_cols <- intersect(leaf_order, names(df_wide))
if (length(leaf_cols) != length(leaf_order)) {
  faltan_leaf <- setdiff(leaf_order, leaf_cols)
  warning(sprintf("Faltan hojas esperadas: %s", paste(faltan_leaf, collapse = ", ")))
}

# ============================
# 3.1) Calendario mensual completo
# ============================
df_wide <- df_wide %>%
  mutate(PeriodoYM = as.yearmon(Periodo)) %>%
  select(PeriodoYM, all_of(leaf_cols)) %>%
  arrange(PeriodoYM)

ym_seq <- seq(from = min(df_wide$PeriodoYM),
              to   = max(df_wide$PeriodoYM),
              by   = 1/12)

df_wide_full <- tibble(PeriodoYM = ym_seq) %>%
  left_join(df_wide, by = "PeriodoYM") %>%
  arrange(PeriodoYM)

X <- df_wide_full %>% select(all_of(leaf_cols)) %>% as.matrix()
if (ncol(X) == 0) stop("No hay columnas de hojas para modelar.")

start_year  <- as.integer(format(as.Date(ym_seq[1]), "%Y"))
start_month <- as.integer(format(as.Date(ym_seq[1]), "%m"))

X_imp <- apply(X, 2, function(col) {
  xt <- ts(col, frequency = 12, start = c(start_year, start_month))
  as.numeric(na.interp(xt))
})
df_wide_full[, leaf_cols] <- X_imp

# ============================
# 4) ts base
# ============================
ts_base <- ts(df_wide_full %>% select(all_of(leaf_cols)) %>% as.matrix(),
              frequency = 12, start = c(start_year, start_month))

# ============================
# 5) Matriz S
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
# (1) Total
S[row, ] <- 1; row <- row + 1

# (2) Total por banco
for(i in 1:n_bancos){
  S[row, idx_bank(i)] <- 1; row <- row + 1
}

# (3) Cartera = comercial + vivienda
for(i in 1:n_bancos){
  idx <- idx_bank(i)
  S[row, idx[1]] <- 1
  S[row, idx[2]] <- 1
  row <- row + 1
}

# (4) Efectivo = caja + banco
for(i in 1:n_bancos){
  idx <- idx_bank(i)
  S[row, idx[3]] <- 1
  S[row, idx[4]] <- 1
  row <- row + 1
}

# (5) Hojas
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

if (qr(t(S) %*% S)$rank < n) stop("S'S no es invertible (faltan hojas independientes).")

# ============================
# 6) Series agregadas
# ============================
Bmat <- as.matrix(ts_base)
Ymat <- Bmat %*% t(S)
colnames(Ymat) <- row_names
make_ts <- function(x) ts(x, start = c(start_year, start_month), frequency = 12)

# ============================
# 7) Pronósticos SARIMA (h = 30)
# ============================
h <- 30
fit_arima <- function(x){
  auto.arima(x,
             D=1)
}

# Hojas
fc_leaf <- lapply(1:ncol(ts_base), function(j){
  fit <- fit_arima(ts_base[, j])
  forecast(fit, h = h)
})

F_leaf <- sapply(fc_leaf, function(x) as.numeric(x$mean))
F_leaf <- t(F_leaf)
rownames(F_leaf) <- col_names

# Todas las series
fc_all <- lapply(1:ncol(Ymat), function(j){
  fit <- fit_arima(make_ts(Ymat[, j]))
  forecast(fit, h = h)
})
F_all <- sapply(fc_all, function(x) as.numeric(x$mean))
F_all <- t(F_all)
rownames(F_all) <- row_names

# ============================
# 8) Reconciliación BU y OLS
# ============================
Yhat_BU  <- S %*% F_leaf
Pinv <- solve(t(S) %*% S)
P    <- S %*% Pinv %*% t(S)
Yhat_OLS <- P %*% F_all

# ============================
# 9) Periodos futuros
# ============================
last_hist_ym <- tail(ym_seq, 1)
future_ym <- seq(from = last_hist_ym + 1/12, by = 1/12, length.out = h)
future_cols <- format(as.Date(future_ym), "%Y-%m")

colnames(F_leaf)   <- future_cols
colnames(F_all)    <- future_cols
colnames(Yhat_BU)  <- future_cols
colnames(Yhat_OLS) <- future_cols

# ============================
# 10) Exportables
# ============================
df_S    <- data.frame(Serie = row_names, S, check.names = FALSE)
df_BU   <- data.frame(Serie = rownames(Yhat_BU),  as.data.frame(Yhat_BU),  check.names = FALSE)
df_OLS  <- data.frame(Serie = rownames(Yhat_OLS), as.data.frame(Yhat_OLS), check.names = FALSE)
df_leaf <- data.frame(Serie = rownames(F_leaf),   as.data.frame(F_leaf),   check.names = FALSE)
df_allf <- data.frame(Serie = rownames(F_all),    as.data.frame(F_all),    check.names = FALSE)

write.csv(df_S,    "matriz_S.csv",                      row.names = FALSE)
write.csv(df_BU,   "pronostico_reconciliado_BU.csv",    row.names = FALSE)
write.csv(df_OLS,  "pronostico_reconciliado_OLS.csv",   row.names = FALSE)
write.csv(df_leaf, "pronostico_tradicional_hojas.csv",  row.names = FALSE)
write.csv(df_allf, "pronostico_tradicional_todos.csv",  row.names = FALSE)

# ============================
# 11) Comparación gráfica
# ============================
plot(as.numeric(F_all["Total",]), type="l", lwd=2, col="darkgreen",
     main="Total: Tradicional vs Bottom-Up vs OLS (30 meses)",
     ylab="Monto", xlab="Horizonte", xaxt="n")
axis(1, at = 1:h, labels = future_cols, cex.axis=0.7, las=2)
lines(as.numeric(Yhat_BU["Total",]),  lwd=2, col="steelblue")
lines(as.numeric(Yhat_OLS["Total",]), lwd=2, col="tomato")
legend("topleft",
       c("Tradicional (SARIMA Total)", "Bottom-Up", "OLS (W=I)"),
       col=c("darkgreen","steelblue","tomato"), lwd=2, bty="n")

# ============================
# 12) CSV para Power BI
# ============================
trad <- read.csv("pronostico_tradicional_hojas.csv", check.names = FALSE)
bu   <- read.csv("pronostico_reconciliado_BU.csv",   check.names = FALSE)
ols  <- read.csv("pronostico_reconciliado_OLS.csv",  check.names = FALSE)

trad$Metodo <- "Tradicional"
bu$Metodo   <- "Bottom-Up"
ols$Metodo  <- "OLS"

df_all_metodos <- bind_rows(trad, bu, ols)

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
      !is.na(Banco) & Categoria %in% c("cartera_de_creditos","efectivo") ~ "Intermedio",
      !is.na(Banco) & Categoria %in% c("credito_comercial","credito_vivienda","caja","banco") ~ "Hoja",
      grepl("_Total$", Serie) ~ "Intermedio",
      TRUE ~ "Otro"
    )
  )

write.csv(df_long, "pronosticos_todos_metodos.csv", row.names = FALSE)
