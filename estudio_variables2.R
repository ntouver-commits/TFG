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
#Aquí lo grafico sin aplicar ningún filtro
data_pi0 %>%
  filter(!is.na(trackScore), trackScore >= 0) %>%
  ggplot(aes(trackScore, fill = clase, colour = clase)) +
  geom_density(alpha = 0.4) +
  labs(x = "trackScore", y = "Densidad") +
  theme_minimal()

#Vamos a trabajar con el cluster que tenga menor trackScore para cada evento

data_min_trackScore <- data_pi0 %>% 
  filter(!is.na(trackScore)) %>% filter(trackScore>0) %>% group_by (eventID) %>%
  slice_min (trackScore, n=1, with_ties = FALSE) %>% ungroup()

#Ahora tenemos un dataset con una fila por evento, y ya podemos construir nuestras
#gráficas a partir de él

data_min_trackScore %>%
  filter(trackScore >= 0) %>%
  ggplot(aes(trackScore, fill = clase, colour = clase)) +
  geom_histogram(aes(y = after_stat(density)), bins = 40,
                 alpha = 0.4, position = "identity") +
  labs(x = "trackScore mínimo por evento", y = "Densidad") +
  theme_classic()

data_min_trackScore %>%
  filter(trackScore >= 0) %>%
  ggplot(aes(trackScore, fill = clase, colour = clase)) +
  geom_histogram(aes(y = after_stat(density)), bins = 40,
                 alpha = 0.4, position = "identity") +
  labs(x = "trackScore mínimo por evento", y = "Densidad") +
  theme_minimal()

data_min_trackScore %>%
  filter(elScore >= 0) %>%
  ggplot(aes(elScore, fill = clase, colour = clase)) +
  geom_histogram(aes(y = after_stat(density)), bins = 40,
                 alpha = 0.4, position = "identity") +
  labs(x = "elScore", y = "Densidad") +
  theme_minimal()

#Hacemos un estudio en escala logarítmica para ver bien qué pasa entre 0.001 y 0.1
data_min_trackScore %>%
  filter(elScore > 0) %>%
  ggplot(aes(elScore, fill = clase, colour = clase)) +
  geom_histogram(aes(y = after_stat(density)), bins = 40,
                 alpha = 0.4, position = "identity") +
  scale_x_log10() +
  labs(x = "elScore (escala log)", y = "Densidad") +
  theme_minimal()

#Ahora muScore, phiScore,...

data_min_trackScore %>%
  filter(muScore >= 0) %>%
  ggplot(aes(muScore, fill = clase, colour = clase)) +
  geom_histogram(aes(y = after_stat(density)), bins = 40,
                 alpha = 0.4, position = "identity") +
  scale_x_log10() +
  labs(x = "muScore", y = "Densidad") +
  theme_minimal()

data_min_trackScore %>%
  filter(phoScore >= 0) %>%
  ggplot(aes(phoScore, fill = clase, colour = clase)) +
  geom_histogram(aes(y = after_stat(density)), bins = 40,
                 alpha = 0.4, position = "identity") +
  labs(x = "phoScore", y = "Densidad") +
  theme_minimal()

#piScore con y sin escala logarítmica

data_min_trackScore %>%
  filter(piScore >= 0) %>%
  ggplot(aes(piScore, fill = clase, colour = clase)) +
  geom_histogram(aes(y = after_stat(density)), bins = 40,
                 alpha = 0.4, position = "identity") +
  labs(x = "piScore", y = "Densidad") +
  theme_minimal()

data_min_trackScore %>%
  filter(piScore >= 0) %>%
  ggplot(aes(piScore, fill = clase, colour = clase)) +
  geom_histogram(aes(y = after_stat(density)), bins = 40,
                 alpha = 0.4, position = "identity") +
  scale_x_log10() +
  labs(x = "piScore", y = "Densidad") +
  theme_minimal()

#pScore

data_min_trackScore %>%
  filter(pScore >= 0) %>%
  ggplot(aes(pScore, fill = clase, colour = clase)) +
  geom_histogram(aes(y = after_stat(density)), bins = 40,
                 alpha = 0.4, position = "identity") +
  labs(x = "pScore", y = "Densidad") +
  theme_minimal()

data_min_trackScore %>%
  filter(pScore >= 0) %>%
  ggplot(aes(pScore, fill = clase, colour = clase)) +
  geom_histogram(aes(y = after_stat(density)), bins = 40,
                 alpha = 0.4, position = "identity") +
  scale_x_log10() +
  labs(x = "pScore", y = "Densidad") +
  theme_minimal()

#ENERGÍA
data_min_trackScore %>%
  filter(showerE >= 0) %>%
  ggplot(aes(showerE, fill = clase, colour = clase)) +
  geom_histogram(aes(y = after_stat(density)), bins = 40,
                 alpha = 0.4, position = "identity") +
  labs(x = "Energía", y = "Densidad") +
  theme_minimal()

data_min_trackScore %>%
  filter(showerE >= 0) %>%
  ggplot(aes(showerE, fill = clase, colour = clase)) +
  geom_histogram(aes(y = after_stat(density)), bins = 40,
                 alpha = 0.4, position = "identity") +
  scale_x_log10() +
  labs(x = "Energía", y = "Densidad") +
  theme_minimal()

#Deposición de energía



data_min_trackScore %>%
  filter(showerdEdx >= 0) %>%
  ggplot(aes(showerdEdx, fill = clase, colour = clase)) +
  geom_histogram(aes(y = after_stat(density)), bins = 40,
                 alpha = 0.4, position = "identity") +

  labs(x = "Deposición de nergía", y = "Densidad") +
  theme_minimal()


data_min_trackScore %>%
  filter(!is.na(showerdEdx), showerdEdx > 0, showerdEdx <= 10) %>%
  ggplot(aes(showerdEdx, fill = clase, colour = clase)) +
  geom_histogram(aes(y = after_stat(density)), binwidth = 0.1,
                 alpha = 0.4, position = "identity") +
  scale_x_continuous(breaks = 0:10) +
  labs(x = "dE/dx (MeV/cm)", y = "Densidad") +
  theme_minimal()



hist_var <- function(data, var, xlab, xmax = NULL, log = FALSE, bins = 50) {
  d <- data %>% filter(!is.na({{ var }}))
  if (log)  d <- d %>% filter({{ var }} > 0)
  if (!is.null(xmax)) d <- d %>% filter({{ var }} <= xmax)
  
  p <- ggplot(d, aes({{ var }}, fill = clase, colour = clase)) +
    geom_histogram(aes(y = after_stat(density)), bins = bins,
                   alpha = 0.4, position = "identity") +
    labs(x = xlab, y = "Densidad") +
    theme_classic()
  
  if (log) p <- p + scale_x_log10()
  p
}

data_min_trackScore %>%
  filter(!is.na(trackKE), trackKE >= 0, trackKE <= 5000) %>%   # ajusta el 5000
  ggplot(aes(trackKE, fill = clase, colour = clase)) +
  geom_histogram(aes(y = after_stat(density)), bins = 50,
                 alpha = 0.4, position = "identity") +
  labs(x = "trackKE", y = "Densidad") +
  theme_classic()

hist_var(data_min_trackScore, trackKE,       "trackKE")

hist_var(data_min_trackScore, trackTheta,    "trackTheta") #No es discriminatoria

hist_var(data_min_trackScore, trackPhi,      "trackPhi") #No es discriminatoria

data_min_trackScore %>%
  filter(!is.na(showerLength), showerLength > 0) %>%
  ggplot(aes(showerLength, fill = clase, colour = clase)) +
  geom_histogram(aes(y = after_stat(density)), bins = 50,
                 alpha = 0.4, position = "identity") +
  labs(x = "showerLength", y = "Densidad") +
  theme_classic()

data_min_trackScore %>%
  filter(showerLength > 0) %>%
  ggplot(aes(showerLength, fill = clase, colour = clase)) +
  geom_density(alpha = 0.4) +
  scale_x_log10() +
  labs(x = "showerLength (escala log)", y = "Densidad") +
  theme_classic()

#showerOpAngle
data_min_trackScore %>%
  filter(!is.na(showerOpAngle), showerOpAngle > 0) %>%
  ggplot(aes(showerOpAngle, fill = clase, colour = clase)) +
  geom_histogram(aes(y = after_stat(density)), bins = 50,
                 alpha = 0.4, position = "identity") +
  labs(x = "showerOpAngle", y = "Densidad") +
  theme_classic()
data_min_trackScore %>%
  filter(showerOpAngle > 0) %>%
  ggplot(aes(showerOpAngle, fill = clase, colour = clase)) +
  geom_density(alpha = 0.4) +
  scale_x_log10() +
  labs(x = "showerOpAngle (escala log)", y = "Densidad") +
  theme_classic()

#explicación: un pión neutro se desintegra en 2 fotones, creando 2 cascadas EM
#Si el detector las reconstruye como una sola cascada, el cono es más abierto porque
#contiene 2 cascadas con una separación angular entre ellas

#Ahora vamos a trabajar con el cluster más energético:

data_max_E <- data_pi0 %>% 
  filter(!is.na(showerE)) %>% filter(showerE>0) %>% group_by (eventID) %>%
  slice_max (showerE, n=1, with_ties = FALSE) %>% ungroup()

#Repetimos todo el proceso

data_max_E %>%
  filter(trackScore >= 0) %>%
  ggplot(aes(trackScore, fill = clase, colour = clase)) +
  geom_histogram(aes(y = after_stat(density)), bins = 40,
                 alpha = 0.4, position = "identity") +
  labs(x = "trackScore", y = "Densidad") +
  theme_minimal()

data_max_E %>%
  filter(elScore >= 0) %>%
  ggplot(aes(elScore, fill = clase, colour = clase)) +
  geom_histogram(aes(y = after_stat(density)), bins = 40,
                 alpha = 0.4, position = "identity") +
  labs(x = "elScore", y = "Densidad") +
  theme_minimal()

#Hacemos un estudio en escala logarítmica para ver bien qué pasa entre 0.001 y 0.1
data_max_E %>%
  filter(elScore > 0) %>%
  ggplot(aes(elScore, fill = clase, colour = clase)) +
  geom_histogram(aes(y = after_stat(density)), bins = 40,
                 alpha = 0.4, position = "identity") +
  scale_x_log10() +
  labs(x = "elScore (escala log)", y = "Densidad") +
  theme_minimal()

#Ahora muScore, phiScore,...

data_max_E %>%
  filter(muScore >= 0) %>%
  ggplot(aes(muScore, fill = clase, colour = clase)) +
  geom_histogram(aes(y = after_stat(density)), bins = 40,
                 alpha = 0.4, position = "identity") +
  scale_x_log10() +
  labs(x = "muScore", y = "Densidad") +
  theme_minimal()

data_max_E %>%
  filter(phoScore >= 0) %>%
  ggplot(aes(phoScore, fill = clase, colour = clase)) +
  geom_histogram(aes(y = after_stat(density)), bins = 40,
                 alpha = 0.4, position = "identity") +
  labs(x = "phoScore", y = "Densidad") +
  theme_minimal()

#piScore con y sin escala logarítmica

data_max_E %>%
  filter(piScore >= 0) %>%
  ggplot(aes(piScore, fill = clase, colour = clase)) +
  geom_histogram(aes(y = after_stat(density)), bins = 40,
                 alpha = 0.4, position = "identity") +
  labs(x = "piScore", y = "Densidad") +
  theme_minimal()

data_max_E %>%
  filter(piScore >= 0) %>%
  ggplot(aes(piScore, fill = clase, colour = clase)) +
  geom_histogram(aes(y = after_stat(density)), bins = 40,
                 alpha = 0.4, position = "identity") +
  scale_x_log10() +
  labs(x = "piScore", y = "Densidad") +
  theme_minimal()

#pScore

data_max_E %>%
  filter(pScore >= 0) %>%
  ggplot(aes(pScore, fill = clase, colour = clase)) +
  geom_histogram(aes(y = after_stat(density)), bins = 40,
                 alpha = 0.4, position = "identity") +
  labs(x = "pScore", y = "Densidad") +
  theme_minimal()

data_max_E %>%
  filter(pScore >= 0) %>%
  ggplot(aes(pScore, fill = clase, colour = clase)) +
  geom_histogram(aes(y = after_stat(density)), bins = 40,
                 alpha = 0.4, position = "identity") +
  scale_x_log10() +
  labs(x = "pScore", y = "Densidad") +
  theme_minimal()

#ENERGÍA
data_max_E %>%
  filter(showerE >= 0) %>%
  ggplot(aes(showerE, fill = clase, colour = clase)) +
  geom_histogram(aes(y = after_stat(density)), bins = 40,
                 alpha = 0.4, position = "identity") +
  labs(x = "Energía", y = "Densidad") +
  theme_minimal()

data_max_E %>%
  filter(showerE >= 0) %>%
  ggplot(aes(showerE, fill = clase, colour = clase)) +
  geom_histogram(aes(y = after_stat(density)), bins = 40,
                 alpha = 0.4, position = "identity") +
  scale_x_log10() +
  labs(x = "Energía", y = "Densidad") +
  theme_minimal()

#Deposición de energía



data_max_E %>%
  filter(showerdEdx >= 0) %>%
  ggplot(aes(showerdEdx, fill = clase, colour = clase)) +
  geom_histogram(aes(y = after_stat(density)), bins = 40,
                 alpha = 0.4, position = "identity") +
  
  labs(x = "Deposición de nergía", y = "Densidad") +
  theme_minimal()


data_max_E %>%
  filter(!is.na(showerdEdx), showerdEdx > 0, showerdEdx <= 10) %>%
  ggplot(aes(showerdEdx, fill = clase, colour = clase)) +
  geom_histogram(aes(y = after_stat(density)), binwidth = 0.1,
                 alpha = 0.4, position = "identity") +
  scale_x_continuous(breaks = 0:10) +
  labs(x = "dE/dx (MeV/cm)", y = "Densidad") +
  theme_minimal()



hist_var <- function(data, var, xlab, xmax = NULL, log = FALSE, bins = 50) {
  d <- data %>% filter(!is.na({{ var }}))
  if (log)  d <- d %>% filter({{ var }} > 0)
  if (!is.null(xmax)) d <- d %>% filter({{ var }} <= xmax)
  
  p <- ggplot(d, aes({{ var }}, fill = clase, colour = clase)) +
    geom_histogram(aes(y = after_stat(density)), bins = bins,
                   alpha = 0.4, position = "identity") +
    labs(x = xlab, y = "Densidad") +
    theme_classic()
  
  if (log) p <- p + scale_x_log10()
  p
}

data_max_E %>%
  filter(!is.na(trackKE), trackKE >= 0, trackKE <= 5000) %>%   # ajusta el 5000
  ggplot(aes(trackKE, fill = clase, colour = clase)) +
  geom_histogram(aes(y = after_stat(density)), bins = 50,
                 alpha = 0.4, position = "identity") +
  labs(x = "trackKE", y = "Densidad") +
  theme_classic()

hist_var(data_max_E, trackKE,       "trackKE")

hist_var(data_max_E, trackTheta,    "trackTheta") #No es discriminatoria

hist_var(data_max_E, trackPhi,      "trackPhi") #No es discriminatoria

data_max_E %>%
  filter(!is.na(showerLength), showerLength > 0) %>%
  ggplot(aes(showerLength, fill = clase, colour = clase)) +
  geom_histogram(aes(y = after_stat(density)), bins = 50,
                 alpha = 0.4, position = "identity") +
  labs(x = "showerLength", y = "Densidad") +
  theme_classic()

#showerOpAngle
data_max_E %>%
  filter(!is.na(showerOpAngle), showerOpAngle > 0) %>%
  ggplot(aes(showerOpAngle, fill = clase, colour = clase)) +
  geom_histogram(aes(y = after_stat(density)), bins = 50,
                 alpha = 0.4, position = "identity") +
  labs(x = "showerOpAngle", y = "Densidad") +
  theme_classic()
data_max_E %>%
  filter(showerOpAngle > 0) %>%
  ggplot(aes(showerOpAngle, fill = clase, colour = clase)) +
  geom_density(alpha = 0.4) +
  scale_x_log10() +
  labs(x = "showerOpAngle (escala log)", y = "Densidad") +
  theme_classic()


#Ahora vamos a estudiar las variables categóricas, para las que no tiene sentido
#trabajar con histogramas

#Primero con data_min_trackScore

data_min_trackScore <- data_min_trackScore %>%
  mutate(isNC_f = factor(isNC, levels = c(0, 1), labels = c("CC", "NC")),
         int_mode_f = factor(int_mode))

# conteos
table(data_min_trackScore$int_mode_f, data_min_trackScore$clase)

# proporciones dentro de cada clase (cada columna suma 1)
round(prop.table(table(data_min_trackScore$int_mode_f, data_min_trackScore$clase), margin = 2), 3)

# ¿de qué modos viene la señal y el fondo?
ggplot(data_min_trackScore, aes(int_mode_f, fill = clase)) +
  geom_bar(aes(y = after_stat(prop), group = clase),
           position = "dodge", alpha = 0.7) +
  labs(x = "int_mode", y = "Proporción dentro de la clase") +
  theme_minimal()

# conteos
table(data_min_trackScore$isNC_f, data_min_trackScore$clase)
# proporciones dentro de cada clase (cada columna suma 1)
round(prop.table(table(data_min_trackScore$isNC_f, data_min_trackScore$clase), margin = 2), 3)

# ¿de qué modos viene la señal y el fondo?
ggplot(data_min_trackScore, aes(isNC_f, fill = clase)) +
  geom_bar(aes(y = after_stat(prop), group = clase),
           position = "dodge", alpha = 0.7) +
  labs(x = "is_NC", y = "Proporción dentro de la clase") +
  theme_minimal()

#Trabajamos ahora con data_max_E

data_max_E <- data_max_E %>%
  mutate(isNC_f = factor(isNC, levels = c(0, 1), labels = c("CC", "NC")),
         int_mode_f = factor(int_mode))


# conteos
table(data_max_E$int_mode_f, data_max_E$clase)

# proporciones dentro de cada clase (cada columna suma 1)
round(prop.table(table(data_max_E$int_mode_f, data_max_E$clase), margin = 2), 3)

# ¿de qué modos viene la señal y el fondo?
ggplot(data_max_E, aes(int_mode_f, fill = clase)) +
  geom_bar(aes(y = after_stat(prop), group = clase),
           position = "dodge", alpha = 0.7) +
  labs(x = "int_mode", y = "Proporción dentro de la clase") +
  theme_minimal()

# conteos
table(data_max_E$isNC_f, data_max_E$clase)
# proporciones dentro de cada clase (cada columna suma 1)
round(prop.table(table(data_max_E$isNC_f, data_max_E$clase), margin = 2), 3)

# ¿de qué modos viene la señal y el fondo?
ggplot(data_max_E, aes(isNC_f, fill = clase)) +
  geom_bar(aes(y = after_stat(prop), group = clase),
           position = "dodge", alpha = 0.7) +
  labs(x = "is_NC", y = "Proporción dentro de la clase") +
  theme_minimal()

library(patchwork)

dos_graficos <- function(var, xlab, log = FALSE, bins = 50, xmax = NULL) {
  hacer <- function(data, titulo) {
    d <- data %>% filter(!is.na({{ var }}), {{ var }} > 0)
    if (!is.null(xmax)) d <- d %>% filter({{ var }} <= xmax)
    
    p <- ggplot(d, aes({{ var }}, fill = clase, colour = clase)) +
      geom_histogram(aes(y = after_stat(density)), bins = bins,
                     alpha = 0.4, position = "identity") +
      labs(title = titulo, x = xlab, y = "Densidad") +
      theme_classic()
    
    if (log) p <- p + scale_x_log10()
    p
  }
  
  (hacer(data_min_trackScore, "Menor trackScore") +
      hacer(data_max_E, "Menor showerE")) +
    plot_layout(guides = "collect")
}
g1 <- data_min_trackScore %>%
  filter(!is.na(showerdEdx), showerdEdx > 0, showerdEdx <= 10) %>%
  ggplot(aes(showerdEdx, fill = clase, colour = clase)) +
  geom_histogram(aes(y = after_stat(density)), binwidth = 0.1,
                 alpha = 0.4, position = "identity") +
  scale_x_continuous(breaks = 0:10) +
  labs(x = "dE/dx (MeV/cm)", y = "Densidad") +
  theme_minimal()
g2 <- data_max_E %>%
  filter(!is.na(showerdEdx), showerdEdx > 0, showerdEdx <= 10) %>%
  ggplot(aes(showerdEdx, fill = clase, colour = clase)) +
  geom_histogram(aes(y = after_stat(density)), binwidth = 0.1,
                 alpha = 0.4, position = "identity") +
  scale_x_continuous(breaks = 0:10) +
  labs(x = "dE/dx (MeV/cm)", y = "Densidad") +
  theme_minimal()
g <- g1+g2

graficos1 <- c(
dos_graficos(trackScore, "trackScore"),
dos_graficos(elScore, "elScore"),
dos_graficos(muScore, "muScore"),
dos_graficos(phoScore, "phoScore"),
dos_graficos(piScore, "piScore"),
dos_graficos(pScore, "pScore")
)

wrap_plots(graficos1, ncol = 2, guides = "collect")

graficos2 <- c(
  dos_graficos(showerLength, "showerLength"),
  dos_graficos(showerOpAngle, "showerOpAngle"),
  dos_graficos(showerE, "showerE", log = TRUE),
  g
)
wrap_plots(graficos2, ncol = 2, guides = "collect")

# Ahora analizamos combinaciones de los datos
#Vamos a sacar ahora unas matrices de correlación:

vars <- data_min_trackScore %>%
  select(where(is.numeric)) %>%
  select(-any_of(c("eventID", "nPi0"))) %>%
  names()

datos_cor <- data_min_trackScore %>%
  select(clase, all_of(vars)) %>%
  mutate(across(all_of(vars), ~ ifelse(.x %in% c(-1, -999), NA, .x)))

cor_clase <- function(k, metodo = "spearman") {
  datos_cor %>%
    filter(clase == k) %>%
    select(-clase) %>%
    cor(use = "pairwise.complete.obs", method = metodo)
}
cor_sig <- cor_clase("Señal (nPi0 = 1)")
cor_bkg <- cor_clase("Background")
round(cor_sig, 2)

plot_cor <- function(m, titulo = "") {
  as.data.frame(as.table(m)) %>%
    ggplot(aes(Var1, Var2, fill = Freq)) +
    geom_tile() +
    geom_text(aes(label = round(Freq, 2)), size = 2.5) +
    scale_fill_gradient2(low = "#d6604d", mid = "white", high = "#4393c3",
                         midpoint = 0, limits = c(-1, 1), name = "r") +
    labs(x = NULL, y = NULL, title = titulo) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
}

p1 <- plot_cor(cor_sig, "Señal (nPi0 = 1)")
p2 <- plot_cor(cor_bkg, "Background")

p1 + p2 + plot_layout(guides = "collect")

#Ahora vamos a aplicar más filtros (no sé si esto tiene tanto sentido)

data_señal_1 <- data_min_trackScore %>%
  filter(trackScore < 0.5, phoScore > 0.25, elScore > 0.01, showerOpAngle > 0.01)

data_señal_1 %>%
  filter(!is.na(showerdEdx), showerdEdx > 0, showerdEdx <= 10) %>%
  ggplot(aes(showerdEdx, fill = clase, colour = clase)) +
  geom_histogram(aes(y = after_stat(density)), binwidth = 0.1,
                 alpha = 0.4, position = "identity") +
  scale_x_continuous(breaks = 0:10) +
  labs(x = "dE/dx (MeV/cm)", y = "Densidad") +
  theme_minimal()

data_señal_1 %>%
  filter(trackScore >= 0) %>%
  ggplot(aes(trackScore, fill = clase, colour = clase)) +
  geom_histogram(aes(y = after_stat(density)), bins = 40,
                 alpha = 0.4, position = "identity") +
  labs(x = "trackScore mínimo por evento", y = "Densidad") +
  theme_classic()

data_señal_1 %>% count(clase) %>% mutate(frac = n / sum(n))
data_min_trackScore %>% count(clase) %>% mutate(frac = n / sum(n))   # referencia

n_foton <- data_pi0 %>%
  mutate(es_foton = trackScore >= 0 & trackScore < 0.6 & phoScore > 0.25) %>%
  group_by(eventID, clase) %>%
  summarise(n_fotones = sum(es_foton, na.rm = TRUE), .groups = "drop")

n_foton %>%
  count(clase, n_fotones) %>%
  group_by(clase) %>%
  mutate(frac = n / sum(n)) %>%
  ggplot(aes(n_fotones, frac, fill = clase)) +
  geom_col(position = "dodge", alpha = 0.8) +
  scale_x_continuous(breaks = 0:10) +
  labs(x = "Número de fotones por evento", y = "Fracción de eventos") +
  theme_classic()

#oleoleole