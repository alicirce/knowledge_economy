library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(patchwork)
library(scales)
library(tibble)

# data from Table RD-6
# https://ncses.nsf.gov/pubs/nsb20246/u-s-business-r-d

benchmarks <- tribble(
  ~benchmark,      ~value,
  "all industry",   0.046,
  "manufacturing",  0.050
)

# labels have been abbreviated for clarity
rd_to_sales <- tribble(
  ~sector,             ~rnd_to_sales,
  "Pharmaceuticals",        0.161,
  "Machinery",              0.042,
  "Semiconductors",         0.204,
  "Motor vehicles",         0.042,
  "Aerospace",              0.042,
  "Finance & insurance",    0.014,
  "Software",               0.129,
  "Computer engineering",   0.102
)
p_expenditure <- rd_to_sales |>
  ggplot() +
  aes(x = rnd_to_sales, y = sector) +
  geom_bar(
    stat = "identity",
    width = 0.5
  ) +
  geom_vline(
    data = benchmarks,
    aes(xintercept = value, linetype = benchmark)
  ) +
  theme_bw() +
  theme(
    axis.title.y = element_blank(),
    axis.ticks.y = element_blank(),
    panel.grid.major.y = element_blank()
  ) +
  scale_linetype_manual(values = c("dashed", "dotted")) +
  scale_x_continuous(
    expand = c(0, 0, 0.01, 0.01),
    labels = scales::label_percent()
  ) +
  labs(
    x = "R&D expenditure relative to sales",
  )
p_expenditure
ggsave(
  "figures/expenditures_sales_ratio_by_sector.png", 
  width = 6, height = 2.5, dpi = 1200
)
ggsave(
  "figures/expenditures_sales_ratio_by_sector.svg", 
  width = 6, height = 2.5
)

p_expenditure +
  theme(
    legend.position = "bottom",
    legend.justification = "right"
  )

ggsave(
  "figures/expenditures_sales_ratio_by_sector_2.png", 
  width = 4, height = 2.5, dpi = 1200
)
ggsave(
  "figures/expenditures_sales_ratio_by_sector_2.svg", 
  width = 4, height = 2.5
)
