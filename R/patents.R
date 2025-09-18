library(rvest)
library(ggplot2)
library(dplyr)
library(tidyr)

url <- "https://www.uspto.gov/web/offices/ac/ido/oeip/taf/h_counts.htm"
page <- read_html(url) 

df <- page |>
  html_element("center table") |>
  html_table(fill = TRUE)

colnames(df) <- colnames(df) |>
  gsub(pattern = "\\(.*", replacement = "") |>
  gsub(pattern = " $", replacement = "") |>
  gsub(pattern = " +", replacement = "_") |>
  tolower()
df[[""]] <- NULL

df |>
  mutate(
    across(
      everything(),
      \(x) as.numeric(gsub("\\D", "", x))
    ),
    across(everything(), ~ replace_na(.x, 0))
  ) |>
  ggplot() +
  aes(x = calendar_year, y = utilitypatents + plantpatents) +
  geom_line() +
  theme_bw() +
  theme(
    plot.caption = element_text(colour = "grey30")
  ) +
  scale_y_continuous(labels = scales::label_number(big.mark = ",")) +
  labs(
    x = "Year",
    y = "US patents per year",
    caption = "Patent counts include utility patents (inventions) 
    plus plant patents, and exclude design patents"
  )
ggsave("figures/patents_per_year.png", width = 3, height = 3)
