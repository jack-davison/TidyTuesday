library(tidyverse)
library(tidytuesdayR)
library(tvthemes)

tuesdata <- tidytuesdayR::tt_load("2020-08-11")

avatar <- tuesdata$avatar
scenes <- tuesdata$scene_description

import_avatar()

avatar_data <- avatar %>%
  mutate(
    book_chapt = case_when(
      book == "Water" ~ chapter_num,
      book == "Earth" ~ chapter_num + 21,
      book == "Fire" ~ chapter_num + 42
    ),
    true_chapt = case_when(
      book == "Water" ~ chapter_num - 1,
      book == "Earth" ~ chapter_num + 20,
      book == "Fire" ~ chapter_num + 41,
    ),
    true_chapt = if_else(true_chapt == max(true_chapt), max(book_chapt) + 1, true_chapt),
    imdb_rating = if_else(book_chapt == 20, 9.7, imdb_rating)
  ) %>%
  select(book, director, imdb_rating, book_chapt, true_chapt) %>%
  group_by(book) %>%
  mutate(series_rating = mean(imdb_rating)) %>%
  unique()

series_names <- avatar_data %>%
  group_by(book) %>%
  summarise(series_rating = min(series_rating),
            halfway = (min(true_chapt) + max(book_chapt) + 1) / 2)

avatar_data %>%
  ggplot(aes(x = book_chapt, y = imdb_rating)) +
  geom_step(aes(y = series_rating, x = true_chapt), size = 1) +
  geom_segment(aes(yend = series_rating, xend = book_chapt), linetype = 2) +
  geom_segment(
    data = avatar_data %>% filter(book == "Water"),
    aes(
      x = min(true_chapt),
      xend = max(book_chapt) + 1,
      y = series_rating,
      yend = series_rating
    ),
    size = 2,
    color = "#006992"
  ) +
  geom_segment(
    data = avatar_data %>% filter(book == "Earth"),
    aes(
      x = min(true_chapt),
      xend = max(book_chapt) + 1,
      y = series_rating,
      yend = series_rating
    ),
    size = 2,
    color = "#00916E"
  ) +
  geom_segment(
    data = avatar_data %>% filter(book == "Fire"),
    aes(
      x = min(true_chapt),
      xend = max(book_chapt) + 1,
      y = series_rating,
      yend = series_rating
    ),
    size = 2,
    color = "#FC440F"
  ) +
  geom_point(aes(color = book)) +
  geom_text(
    data = series_names,
    aes(
      y = series_rating + .025,
      x = halfway,
      label = book,
      color = book
    ),
    family = "Slayer",
    vjust = 0,
    hjust = .5,
    size = 12,
    alpha = 1 / 4
  ) +
  annotate(
    geom = "text",
    x = 10,
    y = 9.8,
    label = "Each point is\nan episode",
    family = "Slayer",
    size = 3, fontface = 2 
  ) +
  annotate(
    "curve",
    xend = 19,
    yend = 9.7,
    x = 15.5,
    y = 9.8,
    curvature = -.2,
    size = 1,
    arrow = arrow(type = "closed", length = unit(.5, "lines")),
    colour = "black"
  ) +
  annotate(
    geom = "text",
    x = 18,
    y = 7.5,
    label = "Even the lowest\nrated episode\nstill got 7.1/10",
    family = "Slayer",
    size = 2
  ) +
  annotate(
    "curve",
    xend = 12,
    yend = 7.1,
    x = 18,
    y = 7.3,
    curvature = -.3,
    arrow = arrow(type = "closed", length = unit(.5, "lines")),
    colour = "black"
  ) +
  annotate(
    geom = "text",
    x = 51.5,
    y = 9.775,
    label = "The final two episodes\nwere the highest rated\n with 9.8/10 each",
    family = "Slayer",
    size = 2
  ) +
  annotate(
    "curve",
    xend = 61.5,
    yend = 9.8,
    x = 57.5,
    y = 9.70,
    curvature = .4,
    arrow = arrow(type = "closed", length = unit(.5, "lines")),
    colour = "black"
  ) +
  annotate(
    "curve",
    xend = 63,
    yend = 9.85,
    x = 57.5,
    y = 9.85,
    curvature = -.4,
    arrow = arrow(type = "closed", length = unit(.5, "lines")),
    colour = "black"
  ) +
  labs(
    x = "",
    y = "IMDb Rating",
    title = "Living in Harmony",
    subtitle = "IMDb Ratings for Avatar: The Last Airbender tended to improve as the series went on!\n\n\n",
    caption = "Data from {appa} (https://github.com/averyrobbins1/appa)\n Visualisation by Jack Davison (Twitter @JDavison_)\nCode found at github.com/jack-davison"
  ) +
  theme_avatar(text.font = "Slayer") +
  theme(
    axis.line.x = element_blank(),
    axis.text.x = element_blank(),
    legend.position = "none",
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(
      color = "#d9cfb5",
      linetype = 3,
      size = 1
    ),
    plot.title = element_text(hjust = .5, size = 25, face = "bold"),
    plot.subtitle = element_text(hjust = .5, size = 10),
    plot.margin = unit(c(1, 1, 1, 1), "cm"),
    plot.caption = element_text(
      hjust = .5,
      size = 8,
      family = "sans",
      color = "#a89567"
    ),
    text = element_text(family = "Slayer")
  ) +
  scale_y_continuous(
    limits = c(7, 10),
    breaks = seq(7, 10, 0.5),
    sec.axis = dup_axis(name = NULL)
  ) +
  scale_color_manual(values = c(
    "Water" = "#006992",
    "Earth" = "#00916E",
    "Fire" = "#FC440F"
  )) +
  scale_fill_manual(values = c(
    "Water" = "#006992",
    "Earth" = "#00916E",
    "Fire" = "#FC440F"
  ))
