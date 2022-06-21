library(tidyverse)
library(lubridate)
library(janitor)
library(openxlsx)

## Data obtained from DEIS Cognos Chile

## Data 2022

data2022 <- read.xlsx("data_raw/AtencionesUrgencia2022.xlsx")

data2022[4, 1] <- "Atenciones"

attentions2022 <- data2022[4:42, 1:length(data2022) - 1] |>
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

data2021 <- read.xlsx("data_raw/AtencionesUrgencia2021.xlsx")

data2021[4, 1] <- "Atenciones"

attentions2021 <- data2021[4:42, 1:length(data2021) - 1] |>
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

data2019 <- read.xlsx("data_raw/AtencionesUrgencia2019.xlsx")

data2019[4, 1] <- "Atenciones"

attentions2019 <- data2019[4:42, 1:length(data2019) - 1] |>
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


attentions <- bind_rows(attentions2019, attentions2021, attentions2022)

# Plot

plot_attentions <- attentions |>
  ggplot(aes(week, attentions, color = factor(year))) +
  geom_path(size = 2) +
  scale_color_manual(values = c("#ef476f", "#06d6a0", "#118ab2")) +
  scale_y_continuous(labels = scales::comma) +
  theme_bw() +
  labs(
    title = "Atenciones de urgencia por causas respiratorias. Chile",
    subtitle = "Se incluyen todos los servicios de urgencia hospitalarios del país\nTodas las edades",
    x = "Semana estadística",
    y = "N° de atenciones",
    caption = "Elaborado por Paulo Villarroel | Fuente: DEIS",
    color = "Año"
  ) +
  theme(plot.title = element_text(size = 20))

plot_attentions

ggsave("plots/attentions2019_2022_total.png", height = 15, width = 26, units = "cm")
