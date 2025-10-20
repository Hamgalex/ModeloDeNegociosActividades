rm(list=ls())

library(openxlsx); 
library(Hmisc); 
library(scales); 
library(officer)
library(flextable); 
library(Distance); 
library(cluster); 
library(ClusterR)
library(fpc); 
library(plotly); 
library(scatterplot3d); 
library(kohonen); 
library(cclust)

BaseAumentada <- read.csv("C:/Users/hamga/Documents/BasePFLD.csv",
                          header = TRUE, sep = ",")

PCA <- prcomp(x = BaseAumentada, center = TRUE, scale. = TRUE)
summary(PCA)

PCs <- data.frame(
  TransactionID = seq_len(nrow(BaseAumentada)),
  PC1  = PCA$x[,1],
  PC2  = PCA$x[,2],
  PC3  = PCA$x[,3],
  PC29 = PCA$x[, max(1, ncol(PCA$x)-1)],
  PC30 = PCA$x[, ncol(PCA$x)]
)

# ayudantes: Mahalanobis 2D + flag
mah2_flag <- function(df, c1, c2, alpha = 0.99){
  M  <- as.matrix(df[, c(c1, c2)])
  mu <- colMeans(M); S <- cov(M)
  d2 <- mahalanobis(M, center = mu, cov = S)
  thr <- qchisq(alpha, df = 2)
  list(d2 = as.numeric(d2), thr = thr, flag = d2 > thr)
}

o12 <- mah2_flag(PCs, "PC1","PC2")     # varianza
o13 <- mah2_flag(PCs, "PC1","PC3")     # varianza extra
oLL <- mah2_flag(PCs, "PC29","PC30")   # correlación

PCs$md2_PC1_PC2  <- o12$d2
PCs$md2_PC1_PC3  <- o13$d2
PCs$md2_PC29_PC30<- oLL$d2

PCs$Outlier_PC1_PC2   <- ifelse(o12$flag, "Outlier", "Normal")
PCs$Outlier_PC1_PC3   <- ifelse(o13$flag, "Outlier", "Normal")
PCs$Outlier_PC29_PC30 <- ifelse(oLL$flag, "Outlier", "Normal")

# Score compuesto (máximo de distancias normalizadas)
score_var12  <- o12$d2 / o12$thr
score_var13  <- o13$d2 / o13$thr
score_corrLL <- oLL$d2 / oLL$thr
PCs$AnomalyScore <- pmax(score_var12, score_var13, score_corrLL)

# Banderas agregadas y nivel de riesgo
PCs$Outlier_Any <- ifelse(PCs$Outlier_PC1_PC2=="Outlier" |
                            PCs$Outlier_PC1_PC3=="Outlier" |
                            PCs$Outlier_PC29_PC30=="Outlier", "Outlier", "Normal")

PCs$RiskLevel <- dplyr::case_when(
  PCs$Outlier_PC29_PC30 == "Outlier"                            ~ "Riesgo Alto",      # rompe correlación (últimas PCs)
  PCs$Outlier_PC1_PC2 == "Outlier" | PCs$Outlier_PC1_PC3=="Outlier" ~ "Riesgo Moderado", # infla varianza
  TRUE                                                          ~ "Riesgo Bajo"
)


out_dir <- getwd()
write.csv(PCs,           file.path(out_dir, "PCs_for_PowerBI.csv"), row.names = FALSE)
write.csv(pca_evr,       file.path(out_dir, "pca_evr.csv"),          row.names = FALSE)
write.csv(pca_loadings,  file.path(out_dir, "pca_loadings.csv"),     row.names = FALSE)

message("Exportado:\n - PCs_for_PowerBI.csv (TransactionID, PCs, md2, Outlier_*, Outlier_Any, RiskLevel, AnomalyScore)\n",
        " - pca_evr.csv\n - pca_loadings.csv\n")

