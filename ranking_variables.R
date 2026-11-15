install.packages("tidyverse")
install.packages(c("ggplot2", "dplyr"))
install.packages("rlang")

install.packages("patchwork")   # solo la primera vez

library(patchwork)
library(tidyverse)
library(ggplot2)
library(dplyr)

data_pi0 <- read.csv("C:/Users/ntouv/OneDrive/Escritorio/6DGFYM/TFG/data_pi0.csv")

data_pi0 <- data_pi0 %>% mutate(clase = factor(ifelse(nPi0 == 1, "Señal (nPi0 = 1)", "Background"),
                                               levels = c("Background", "Señal (nPi0 = 1)")))

data_min_trackScore <- data_pi0 %>% 
  filter(!is.na(trackScore)) %>% filter(trackScore>0) %>% group_by (eventID) %>%
  slice_min (trackScore, n=1, with_ties = FALSE) %>% ungroup()

#Primer estadístico: (media S - media B) / √(desv. S^2 + desv. B ^2)

poder_separacion <- function(datos, var) {
  x <- datos[[var]]
  S <- x[datos$nPi0 == 1]
  B <- x[datos$nPi0 != 1]
  mS <- mean(S, na.rm = TRUE);  mB <- mean(B, na.rm = TRUE)
  sS <- sd(S, na.rm = TRUE);    sB <- sd(B, na.rm = TRUE)
  data.frame(variable = var,
             media_S = mS, media_B = mB,
             sigma_S = sS, sigma_B = sB,
             test1 = (mS - mB) / sqrt(sS^2 + sB^2))
}

# Variables numéricas a comparar (ajusta los nombres a tus columnas)
vars <- c("trackScore", "elScore", "phoScore", "showerOpAngle", "pScore", "showerdEdx")
vars <- intersect(vars, names(data_min_trackScore))   # por si alguna no existe

resultados1 <- bind_rows(lapply(vars, poder_separacion, datos = data_min_trackScore)) %>%
  mutate(abs_test1 = abs(test1)) %>%
  arrange(desc(abs_test1))

resultados1

producto_escalar <- function(datos, var, nbins = 50) {
  x <- datos[[var]]
  S <- x[datos$nPi0 == 1]; S <- S[!is.na(S)]
  B <- x[datos$nPi0 != 1]; B <- B[!is.na(B)]
  
  # mismos bins para S y B
  breaks <- seq(min(x, na.rm = TRUE), max(x, na.rm = TRUE), length.out = nbins + 1)
  dx <- diff(breaks)
  
  fS <- hist(S, breaks = breaks, plot = FALSE)$density
  fB <- hist(B, breaks = breaks, plot = FALSE)$density
  
  int_SB <- sum(fS * fB * dx)   # Integral(S * B)
  int_S  <- sum(fS * dx)        # Integral(S)
  int_B  <- sum(fB * dx)        # Integral(B)
  
  data.frame(variable = var, prod_escalar = int_SB / (int_S * int_B))
}


res_pe <- bind_rows(lapply(vars, producto_escalar, datos = data_min_trackScore)) %>%
  arrange(prod_escalar)    # de menor a mayor solapamiento
res_pe

#Chi^2

table(data_min_trackScore$clase)

signal <- data_min_trackScore[data_min_trackScore$clase == "Signal", ]
background <- data_min_trackScore[data_min_trackScore$clase == "Background", ]

# Crear 10 intervalos
breaks <- seq(
  min(data_min_trackScore$trackScore, na.rm = TRUE),
  max(data_min_trackScore$trackScore, na.rm = TRUE),
  length.out = 11
)

data_min_trackScore$bin <- cut(data_min_trackScore$trackScore, breaks = breaks,
                 include.lowest = TRUE)

# Tabla de contingencia
tabla <- table(data_min_trackScore$bin, data_min_trackScore$clase)

# Test chi-cuadrado
chi <- chisq.test(tabla)

chi$statistic
chi$p.value
