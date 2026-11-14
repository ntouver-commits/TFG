install.packages("tidyverse")
install.packages(c("ggplot2", "dplyr"))
install.packages("rlang")

install.packages("patchwork")   # solo la primera vez

library(patchwork)
library(tidyverse)
library(ggplot2)
library(dplyr)

data_pi0 <- read.csv("C:/Users/ntouv/OneDrive/Escritorio/6DGFYM/TFG/data_pi0.csv")

data_pi0 <- data_pi0 %>%
  mutate(clase = factor(ifelse(nPi0 == 1, "Señal (nPi0 = 1)", "Background"),
                        levels = c("Background", "Señal (nPi0 = 1)")))

cols_track <- grep("trackScore", names(data_pi0), value = TRUE)
cols_el    <- grep("elScore",    names(data_pi0), value = TRUE)

data_pi0 %>%
  select(clase, all_of(cols_track)) %>% #quitamos todas las columnas que no vamos a usar
  pivot_longer(-clase, names_to = "objeto", values_to = "trackScore") %>%
  filter(!is.na(trackScore), trackScore >= 0) %>%   
  ggplot(aes(trackScore, fill = clase, colour = clase)) +
  geom_histogram(aes(y = after_stat(density)), bins = 40,
                 alpha = 0.4, position = "identity") +
  facet_wrap(~objeto) +
  labs(x = "trackScore", y = "Densidad") +
  theme_minimal()

data_pi0 %>%
  select(clase, all_of(cols_el)) %>%
  pivot_longer(-clase, names_to = "objeto", values_to = "elScore") %>%
  filter(!is.na(elScore), elScore >= 0) %>%   
  ggplot(aes(elScore, fill = clase, colour = clase)) +
  geom_histogram(aes(y = after_stat(density)), bins = 40,
                 alpha = 0.4, position = "identity") +
  facet_wrap(~objeto) +
  labs(x = "elScore", y = "Densidad") +
  theme_minimal()
#Esta discrimación no nos da mucha información

#Vamos a usar ahora una función que nos haga ya directamente los histogramas
#sin necesidad de estar escribiendo todo el rato el mismo código

hist_var <- function(v, datos = data_pi0, bins = 40, quitar = c(-1, -999), tipo = "todos") {
  
  d <- datos %>% filter(!is.na(.data[[v]]), !(.data[[v]] %in% quitar))
  
  # Aplicamos el filtro según el tipo de objeto
  if (tipo == "cascada") {
    d <- d %>% filter(!is.na(trackScore), trackScore >= 0, trackScore < 0.5)
  } else if (tipo == "traza") {
    d <- d %>% filter(!is.na(trackScore), trackScore >= 0.5)
  }
  
  ggplot(d, aes(x = .data[[v]], fill = clase, colour = clase)) +
    geom_histogram(aes(y = after_stat(density)), bins = bins,
                   alpha = 0.4, position = "identity") +
    labs(x = v, y = "Densidad", title = paste(v, "-", tipo)) +
    theme_minimal()
}

hist_var2 <- function(v, datos = data_pi0, bins = 40, quitar = c(-1, -999),
                      cuantiles = c(0.01, 0.99), log_x = FALSE, tipo = "todos") {
  
  d <- datos %>% filter(!is.na(.data[[v]]), !(.data[[v]] %in% quitar))
  
  # Aplicamos el filtro según el tipo de objeto
  if (tipo == "cascada") {
    d <- d %>% filter(!is.na(trackScore), trackScore >= 0, trackScore < 0.5)
  } else if (tipo == "traza") {
    d <- d %>% filter(!is.na(trackScore), trackScore >= 0.5)
  }
  
  if (!is.null(cuantiles)) {
    lim <- quantile(d[[v]], cuantiles, na.rm = TRUE)
    d <- d %>% filter(.data[[v]] >= lim[1], .data[[v]] <= lim[2])
  }
  
  p <- ggplot(d, aes(x = .data[[v]], fill = clase, colour = clase)) +
    geom_histogram(aes(y = after_stat(density)), bins = bins,
                   alpha = 0.4, position = "identity") +
    labs(x = v, y = "Densidad", title = paste(v, "-", tipo)) +
    theme_minimal()
  
  if (log_x) p <- p + scale_x_log10()
  
  return(p)
}

hist_var("trackScore")

hist_var("phoScore", tipo = "cascada")
hist_var("elScore", tipo = "cascada")
hist_var2("showerdEdx", tipo = "cascada")
hist_var2("showerOpAngle", tipo = "cascada")

hist_var2("showerE") #poco discriminante
hist_var2("showerdEdx")
hist_var("trueE")

#Ahora vamos a estudiar las variables categóricas, para las que no tiene sentido
#trabajar con histogramas

data_pi0 <- data_pi0 %>%
  mutate(isNC_f = factor(isNC, levels = c(0, 1), labels = c("CC", "NC")),
         int_mode_f = factor(int_mode))

# conteos
table(data_pi0$int_mode_f, data_pi0$clase)

# proporciones dentro de cada clase (cada columna suma 1)
round(prop.table(table(data_pi0$int_mode_f, data_pi0$clase), margin = 2), 3)

# ¿de qué modos viene la señal y el fondo?
ggplot(data_pi0, aes(int_mode_f, fill = clase)) +
  geom_bar(aes(y = after_stat(prop), group = clase),
           position = "dodge", alpha = 0.7) +
  labs(x = "int_mode", y = "Proporción dentro de la clase") +
  theme_minimal()

# conteos
table(data_pi0$isNC_f, data_pi0$clase)
# proporciones dentro de cada clase (cada columna suma 1)
round(prop.table(table(data_pi0$isNC_f, data_pi0$clase), margin = 2), 3)

# ¿de qué modos viene la señal y el fondo?
ggplot(data_pi0, aes(isNC_f, fill = clase)) +
  geom_bar(aes(y = after_stat(prop), group = clase),
           position = "dodge", alpha = 0.7) +
  labs(x = "is_NC", y = "Proporción dentro de la clase") +
  theme_minimal()

# Ahora analizamos combinaciones de los datos
#Vamos a sacar ahora unas matrices de correlación:

vars <- data_pi0 %>%
  select(where(is.numeric)) %>%
  select(-any_of(c("eventID", "nPi0"))) %>%
  names()

datos_cor <- data_pi0 %>%
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



data_pi0 %>%
  filter(!is.na(showerE), !(showerE %in% c(-1, -999)),
         !is.na(showerdEdx), !(showerdEdx %in% c(-1, -999))) %>%
  # Acotamos los ejes para centrar el gráfico en la zona de interés
  filter(showerdEdx > 0, showerdEdx < 15, showerE < 600) %>% 
  ggplot(aes(x = showerE, y = showerdEdx)) +
  # Usamos bin2d que agrupa los datos en píxeles de color según su concentración
  geom_bin2d(bins = 60) + 
  # Escala de colores optimizada para mapas de calor
  scale_fill_viridis_c(option = "plasma") + 
  # Separa la señal y el fondo en dos gráficos paralelos
  facet_wrap(~ clase) + 
  labs(
    title = "Mapa de calor 2D: showerE vs showerdEdx",
    x = "Energía de la cascada (showerE) [MeV]",
    y = "Deposición de energía (showerdEdx) [MeV/cm]",
    fill = "Sucesos"
  ) +
  theme_minimal()
