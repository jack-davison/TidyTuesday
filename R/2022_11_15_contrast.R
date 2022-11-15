
library(tidyverse)
library(showtext)

quick_annotate <- function(lab, y, color, size) {
  annotate(
    x = lubridate::ymd("2021-01-01"),
    family = "source",
    y = y,
    label = lab,
    color = color,
    geom = "text",
    size = size
  )
}

# Add fonts ---------------------------------------------------------------

font_add_google(name = "Source Sans Pro", "source")
showtext_auto()

# Prep data ---------------------------------------------------------------

color_contrast <-
  readr::read_csv(
    'https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-11-15/color_contrast.csv'
  )

yaxis <- tibble(y = pretty(color_contrast$percent))

plot_dat <-
  color_contrast |>
  janitor::remove_constant() |>
  mutate(date = lubridate::as_date(date)) |>
  filter(client == "mobile")


# Make plot (low res) -----------------------------------------------------

plt <-
  plot_dat |>
  ggplot(aes(x = date, y = percent)) +
  ggforce::stat_link2(n = 300, aes(color = percent)) +
  geom_text(data = yaxis, aes(
    y = y,
    label = paste(y, "%"),
    color = y,
    x = lubridate::ymd("2017-01-01")
  )) +
  scale_y_continuous(labels = scales::label_percent(scale = 1)) +
  theme(
    text = element_text(family = "source"),
    plot.caption = element_text(color = "grey25"),
    plot.background = element_rect(fill = "black", color = NA),
    panel.background = element_blank(),
    panel.grid = element_blank(),
    legend.position = "none",
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank()
  ) +
  scale_color_gradient(low = "black", high = "grey50") +
  quick_annotate("By the end of October 2022,",
                 y = 27, "grey20", 4) +
  quick_annotate("only 23.6% of smartphone",
                 y = 26.5, "grey30", 4) +
  quick_annotate("websites had an appropriate",
                 y = 26, "grey40", 4) +
  quick_annotate("contrast ratio, which could",
                 y = 25.5, "grey50", 4) +
  quick_annotate("be leaving many users",
                 y = 25, "grey60", 4) +
  quick_annotate("IN THE DARK", 24.25, "white", 8) +
  labs(caption = "Data from httparchive.org | Visualisation by Jack Davison (@JDavison_)",
       x = NULL, y = NULL)

ggsave(
  plot = plt, 
  filename = "plots/contrast_bad.png",
  dpi = 150,
  width = 5,
  height = 3,
  device = "png"
)


# Lights on! --------------------------------------------------------------

plt_good <- 
  plot_dat |>
  ggplot(aes(x = date, y = percent)) +
  ggforce::stat_link2(aes(color = percent)) +
  geom_text(data = yaxis,
            color = "black",
            aes(
    y = y,
    label = paste(y, "%"),
    x = lubridate::ymd("2017-01-01")
  )) +
  scale_y_continuous(labels = scales::label_percent(scale = 1)) +
  theme(
    text = element_text(family = "source"),
    plot.caption = element_text(color = "black"),
    plot.background = element_rect(fill = "white"),
    panel.background = element_blank(),
    panel.grid = element_blank(),
    legend.position = "none",
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank()
  ) +
  scale_color_gradient(low = "lightgreen", high = "hotpink") +
  quick_annotate("By the end of October 2022,",
                 y = 27, "black", 4) +
  quick_annotate("only 23.6% of smartphone",
                 y = 26.5, "black", 4) +
  quick_annotate("websites had an appropriate",
                 y = 26, "black", 4) +
  quick_annotate("contrast ratio, which could",
                 y = 25.5, "black", 4) +
  quick_annotate("be leaving many users",
                 y = 25, "black", 4) +
  quick_annotate("IN THE DARK", 24.25, "black", 8) +
  labs(caption = "Data from httparchive.org | Visualisation by Jack Davison (@JDavison_)",
       x = NULL, y = NULL)

ggsave(
  plot = plt_good, 
  filename = "plots/contrast_good.png",
  dpi = 150,
  width = 5,
  height = 3,
  device = "png"
)
