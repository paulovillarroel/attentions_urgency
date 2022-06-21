library(tidyverse)
library(lubridate)
library(janitor)
library(openxlsx)

## Data obtained from DEIS Cognos Chile

## Data 2022

data2022_1yr <- read.xlsx("data_raw/AtencionesUrgencia_1yr2022.xlsx")

data2022_1yr[45, 1] <- "Atenciones"

attentions2022_1yr <- data2022_1yr[45:83, 1:length(data2022_1yr) - 1] |>
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

data2021_1yr <- read.xlsx("data_raw/AtencionesUrgencia_1yr2021.xlsx")

data2021_1yr[45, 1] <- "Atenciones"

attentions2021_1yr <- data2021_1yr[45:83, 1:length(data2021_1yr)] |>
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

data2019_1yr <- read.xlsx("data_raw/AtencionesUrgencia_1yr2019.xlsx")

data2019_1yr[45, 1] <- "Atenciones"

attentions2019_1yr <- data2019_1yr[45:83, 1:length(data2019_1yr)] |>
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


attentions_1yr <- bind_rows(attentions2019_1yr, attentions2021_1yr, attentions2022_1yr)

# Plot

plot_attentions_1yr <- attentions_1yr |>
  ggplot(aes(week, attentions, color = factor(year))) +
  geom_path(size = 2) +
  scale_color_manual(values = c("#ef476f", "#06d6a0", "#118ab2")) +
  scale_y_continuous(labels = scales::comma) +
  theme_bw() +
  labs(
    title = "Atenciones de urgencia por causas respiratorias (menores de 1 año). Chile",
    subtitle = "Se incluyen todos los servicios de urgencia hospitalarios del país",
    x = "Semana estadística",
    y = "N° de atenciones",
    caption = "Elaborado por Paulo Villarroel | Fuente: DEIS",
    color = "Año"
  ) +
  theme(plot.title = element_text(size = 20))

plot_attentions_1yr

ggsave("plots/attentions2019_2022_1yr.png", height = 15, width = 26, units = "cm")
