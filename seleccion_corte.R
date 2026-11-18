#Ahora vamos a estudiar cuál es el corte óptimo. Como no ha quedado muy claro 
#qué variable es mejor, si trackScore o phoScore, lo haremos para las 2.

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

#Empezamos para trackScore:

trackScore_senal <- data_min_trackScore$trackScore[data_min_trackScore$nPi0 == 1]
trackScore_bckg <- data_min_trackScore$trackScore[data_min_trackScore$nPi0 != 1]

cortes <- seq(min(data_min_trackScore$trackScore), max(data_min_trackScore$trackScore), length.out = 200)

funcion_cortes <- function(corte) {
  S_sel <- sum(trackScore_senal < corte)
  B_sel <- sum(trackScore_bckg  < corte)
  eff <- S_sel / length(trackScore_senal)
  pur <- if (S_sel + B_sel > 0) S_sel / (S_sel + B_sel) else NA
  data.frame(corte = corte, eff = eff, pur = pur, prod = eff * pur)
}

res_cortes <- bind_rows(lapply(cortes, funcion_cortes))
res_cortes[which.max(res_cortes$prod), ]   # corte óptimo


library(ggplot2); library(tidyr)

mejor <- res_cortes[which.max(res_cortes$prod), ]

res_cortes %>%
  pivot_longer(c(eff, pur, prod), names_to = "magnitud", values_to = "valor") %>%
  ggplot(aes(corte, valor, colour = magnitud)) +
  geom_line() +
  geom_vline(xintercept = mejor$corte, linetype = "dashed") +
  labs(x = "Corte en trackScore", y = NULL, colour = NULL,
       title = paste0("Corte óptimo = ", round(mejor$corte, 3))) +
  theme_bw()


