library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(patchwork)
library(scales)
library(tibble)
# expenditure in billions current USD, in the US
# from: https://ncses.nsf.gov/pubs/nsb20246/trends-in-u-s-r-d-performance
f1 <- read_excel("data/nsb2024_fig1.xlsx") |>
  mutate(
    `Nonfederal government` = ifelse(
      `Nonfederal government` == "NA", "0", `Nonfederal government`
    )
  ) |>
  mutate(across(everything(), as.numeric)) 

f2 <- read_excel("data/nsb2024_fig2.xlsx") |>
  mutate(across(everything(), as.numeric))


f1_prop_long <- f1 |>
  pivot_longer(-c(1:2), names_to = "sector", values_to = "expenditure") |>
  mutate(sector = ifelse(
    !sector %in% c("Business", "Federal government", "Higher education"),
    "Other",
    sector
  )) |>
  group_by(sector, Year, `U.S. total R&D`) |>
  summarize(expenditure = sum(expenditure)) |>
  ungroup() |> 
  mutate(
    proportion = expenditure / `U.S. total R&D`
  ) 

f2_prop_long <- f2 |>
  pivot_longer(-c(1:2), names_to = "sector", values_to = "expenditure") |>
  mutate(
    proportion = expenditure / `U.S. total R&D`
  ) 

p1 <- f1_prop_long |>
  ggplot() +
  aes(x = Year, y = proportion, linetype = sector) +
  geom_line() +
  theme_bw() +
  theme(
    legend.position = "top"
  ) +
  scale_y_continuous(labels = scales::label_percent()) +
  scale_x_continuous(breaks = seq(1960, 2020, 20)) +
  labs(y = "Proportion of R&D performed (by cost)")

p2 <- f2_prop_long |>
  ggplot() +
  aes(x = Year, y = proportion, linetype = sector) +
  geom_line() +
  theme_bw() +
  theme(
    legend.position = "top"
  ) +
  scale_y_continuous(labels = scales::label_percent()) +
  scale_x_continuous(breaks = seq(1960, 2020, 20)) +
  labs(y = "Proportion of R&D funding")

p3 <- f2_prop_long |>
  ggplot() +
  aes(x = Year, y = expenditure, linetype = sector) +
  geom_line() +
  theme_bw() +
  theme(
    legend.position = "top"
  ) +
  labs(y = "R&D funding, billions of 2024 USD") +
  scale_x_continuous(breaks = seq(1960, 2020, 20))

(p1 + p2 + p3) + 
  plot_layout(guides = "collect")  & 
  theme(legend.position = "bottom") & 
  guides(color = guide_legend(nrow = 2))

ggsave(
  "figures/expenditures_by_funder_performer.png", 
  width = 7, height = 4
)
