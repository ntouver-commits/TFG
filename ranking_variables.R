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

# Variables numéricas a comparar
vars <- c("trackScore", "elScore", "phoScore", "showerOpAngle", "pScore", "showerdEdx")

resultados1 <- bind_rows(lapply(vars, poder_separacion, datos = data_min_trackScore)) %>%
  mutate(abs_test1 = abs(test1)) %>%
  arrange(desc(abs_test1))

resultados1

#Producto escalar

producto_escalar <- function(datos, var, nbins = 50) {
  x <- datos[[var]]
  S <- x[datos$nPi0 == 1]; S <- S[!is.na(S)]
  B <- x[datos$nPi0 != 1]; B <- B[!is.na(B)]
  
  # mismos bins para S y B
  breaks <- seq(min(x, na.rm = TRUE), max(x, na.rm = TRUE), length.out = nbins + 1)
  dx <- diff(breaks)
  
  fS <- hist(S, breaks = breaks, plot = FALSE)$density
  fB <- hist(B, breaks = breaks, plot = FALSE)$density

  int_SB <- sum(fS * fB * dx)
  int_SS <- sum(fS^2 * dx)
  int_BB <- sum(fB^2 * dx)
  data.frame(variable = var, prod_escalar = int_SB / sqrt(int_SS * int_BB))
}


res_pe <- bind_rows(lapply(vars, producto_escalar, datos = data_min_trackScore)) %>%
  arrange(prod_escalar)    # de menor a mayor solapamiento
res_pe

#Test Kolmogorov-Smirnov

ks_D <- function(datos, var) {
  x <- datos[[var]]
  ok <- !is.na(x)
  x <- x[ok]
  es_senal <- datos$nPi0[ok] == 1
  
  S <- x[es_senal]       # valores de la variable en señal
  B <- x[!es_senal]      # valores de la variable en background
  
  data.frame(variable = var,
             D_ks_test = unname(suppressWarnings(ks.test(S, B)$statistic)))
}


vars <- c("trackScore", "phoScore", "elScore", "showerdEdx", "showerOpAngle", "pScore")
vars <- intersect(vars, names(data_min_trackScore))

res_ks <- bind_rows(lapply(vars, ks_D, datos = data_min_trackScore))

res_ks


largo <- data_min_trackScore %>%
  select(all_of(vars), nPi0) %>%
  mutate(clase = ifelse(nPi0 == 1, "Señal", "Background")) %>%
  pivot_longer(all_of(vars), names_to = "variable", values_to = "valor") %>%
  filter(!is.na(valor))

ggplot(largo, aes(valor, colour = clase)) +
  stat_ecdf(geom = "step") +
  facet_wrap(~ variable, scales = "free_x") +
  labs(x = NULL, y = "Fracción acumulada", colour = NULL,
       title = "Acumuladas señal vs background") +
  theme_bw()


#Test chi cuadrado

chi2_R <- function(datos, var, nbins = 40) {
  x <- datos[[var]]
  ok <- !is.na(x)
  x <- x[ok]
  es_senal <- datos$nPi0[ok] == 1
  
  breaks <- seq(min(x), max(x), length.out = nbins + 1)
  bin <- cut(x, breaks = breaks, include.lowest = TRUE)
  
  S_i <- as.numeric(table(bin[es_senal]))     # señal por bin   -> p
  B_i <- as.numeric(table(bin[!es_senal]))    # background por bin -> x
  
  usar <- S_i > 0      # un bin con S_i = 0 tendría esperado 0 y daría chi2 infinito
  
  test <- suppressWarnings(chisq.test(x = B_i[usar], p = S_i[usar], rescale.p = TRUE))
  
  data.frame(variable = var,
             chi2 = unname(test$statistic),
             gl = unname(test$parameter),
             p_valor = test$p.value,
             chi2_norm = unname(test$statistic) / sum(B_i),    # para comparar entre variables
             frac_B_descartado = sum(B_i[!usar]) / sum(B_i))
}

res_chi2_R <- bind_rows(lapply(vars, chi2_R, datos = data_min_trackScore, nbins = 20)) %>%
  arrange(desc(chi2_norm))
res_chi2_R
