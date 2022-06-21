library(tidyverse)
library(lubridate)
library(janitor)
library(openxlsx)

## Data obtained from DEIS Cognos Chile

## Data 2022

data2022_1_4yr <- read.xlsx("data_raw/AtencionesUrgencia_1_4yr2022.xlsx")

data2022_1_4yr[45, 1] <- "Atenciones"

attentions2022_1_4yr <- data2022_1_4yr[45:83, 1:length(data2022_1_4yr) - 1] |>
  row_to_names(1) |>
  filter(Atenciones == "TOTAL CAUSAS SISTEMA RESPIRATORIO") |>
  dplyr::select(-2) |>
  pivot_longer(
    cols = -Atenciones,
    names_to = "week",
    values_to = "attentions"
  ) |>
  mutate(
    attentions = as.numeric(attentions),
    week = as.numeric(week),
    year = 2022
  )

## Data 2021

data2021_1_4yr <- read.xlsx("data_raw/AtencionesUrgencia_1_4yr2021.xlsx")

data2021_1_4yr[45, 1] <- "Atenciones"

attentions2021_1_4yr <- data2021_1_4yr[45:83, 1:length(data2021_1_4yr)] |>
  row_to_names(1) |>
  filter(Atenciones == "TOTAL CAUSAS SISTEMA RESPIRATORIO") |>
  dplyr::select(-2) |>
  pivot_longer(
    cols = -Atenciones,
    names_to = "week",
    values_to = "attentions"
  ) |>
  mutate(
    attentions = as.numeric(attentions),
    week = as.numeric(week),
    year = 2021
  )


## Data 2019

data2019_1_4yr <- read.xlsx("data_raw/AtencionesUrgencia_1_4yr2019.xlsx")

data2019_1_4yr[45, 1] <- "Atenciones"

attentions2019_1_4yr <- data2019_1_4yr[45:83, 1:length(data2019_1_4yr)] |>
  row_to_names(1) |>
  filter(Atenciones == "TOTAL CAUSAS SISTEMA RESPIRATORIO") |>
  dplyr::select(-2) |>
  pivot_longer(
    cols = -Atenciones,
    names_to = "week",
    values_to = "attentions"
  ) |>
  mutate(
    attentions = as.numeric(attentions),
    week = as.numeric(week),
    year = 2019
  )


attentions_1_4yr <- bind_rows(attentions2019_1_4yr, attentions2021_1_4yr, attentions2022_1_4yr)

# Plot

plot_attentions_1_4yr <- attentions_1_4yr |>
  ggplot(aes(week, attentions, color = factor(year))) +
  geom_path(size = 2) +
  scale_color_manual(values = c("#ef476f", "#06d6a0", "#118ab2")) +
  scale_y_continuous(labels = scales::comma) +
  theme_bw() +
  labs(
    title = "Atenciones de urgencia por causas respiratorias (entre 1 y 4 años). Chile",
    subtitle = "Se incluyen todos los servicios de urgencia hospitalarios del país",
    x = "Semana estadística",
    y = "N° de atenciones",
    caption = "Elaborado por Paulo Villarroel | Fuente: DEIS",
    color = "Año"
  ) +
  theme(plot.title = element_text(size = 20))

plot_attentions_1_4yr

ggsave("plots/attentions2021_2022_1_4yr.png", height = 15, width = 26, units = "cm")
