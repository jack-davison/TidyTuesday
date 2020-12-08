
library(tidyverse)
library(tidytuesdayR)
library(tidytext)
library(rnaturalearth)
library(rnaturalearthdata)
library(cowplot)
library(extrafont)


world <-
  ne_countries(scale = "medium", returnclass = "sf") %>% filter(!continent %in% c("Antarctica", "Seven seas (open ocean)"))

women <-
  readr::read_csv(
    'https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-12-08/women.csv'
  ) %>%
  rename(id = name) %>%
  mutate(
    name = case_when(
      country == "UAE" ~ "United Arab Emirates",
      country == "UK" ~ "United Kingdom",
      country == "US" ~ "United States",
      country == "South Korea" ~ "Korea",
      country == "Republic of Ireland" ~ "Ireland",
      country == "Northern Ireland" ~ "United Kingdom",
      country == "DR Congo" ~ "Congo",
      country == "Wales, UK" ~ "United Kingdom",
      country == "Iraq/UK" ~ "United Kingdom",
      str_detect(country, "Exiled") ~ "China",
      TRUE ~ country
    )
  ) %>%
  filter(!is.na(category),
         category != "All")


plot_women_map = function(cat, col, nn, pos) {
  women_map = women %>%
    filter(category == cat) %>%
    left_join(world, .) %>%
    filter(!is.na(id))
  
  
  map_plot = ggplot() +
    geom_sf(data = world,
            color = NA,
            alpha = .3) +
    geom_sf(
      data = women_map,
      color = NA,
      fill = col,
      alpha = .3,
    ) +
    theme_void() +
    theme(legend.position = "none")
  
  word_cloud = women %>%
    filter(category == cat) %>%
    mutate(word = str_split(description, " ")) %>%
    unnest(word) %>%
    mutate(word = str_remove_all(word, ",|'s|'s|\\."),
           word = tolower(word),
           word = case_when(word == "activists" ~ "activist",
                            TRUE ~ word)) %>%
    filter(!word %in% c("-","woman","women")) %>%
    anti_join(get_stopwords()) %>%
    count(word, sort = T) %>%
    filter(n > nn) %>%
    ggplot() +
    ggwordcloud::geom_text_wordcloud(aes(
      label = word,
      size = n
    ), color = "grey30", family = "Bodoni MT") +
    theme_void() +
    scale_size(range = c(4, 10)) +
    scale_alpha(range = c(.5,1))
    # scale_color_gradient(low = "black", high = col, trans = "log")
  
  ggdraw() +
    draw_plot(map_plot) +
    draw_plot(word_cloud) +
    draw_text(y = pos, size = 20, text = cat, family = "Bodoni MT Black", color = col)
  
}

combo_plot = list(
  plot_women_map("Leadership", "#ee741cff", nn = 2, pos = 0.8),
  plot_women_map("Creativity", "#d04592ff", nn = 2, pos = 0.8),
  plot_women_map("Knowledge",  "#5ac2deff", nn = 2, pos = 0.2),
  plot_women_map("Identity",   "#34aa4dff", nn = 2, pos = 0.2)
) %>% 
  plot_grid(plotlist = .)

random_plot = (world %>%
  mutate(color = sample(x = c("#ee741cff", "#d04592ff", "#5ac2deff", "#34aa4dff"), 232, replace = T)) %>%
  ungroup() %>%
  ggplot() + 
  geom_sf(aes(fill = color, alpha = .1), color = NA) +
  geom_sf(fill = "white", color = NA, alpha = .5) +
  scale_fill_identity() +
  theme_void() +
  theme(legend.position = "none")) %>%
  ggdraw(plot = .) +
  draw_text("In an extraordinary year, when countless women\naround the world have made a sacrifice to help others,\nthe first place on the list is left open to acknowledge\ntheir work and to remember those who have lost their\nlives while making a difference.", family = "Bodoni MT", color = "grey20",size = 9)

combo_plot +
  draw_text(
    y = .57,
    x = .1,
    hjust = 0,
    "The BBC's 100 Women of 2020",
    family = "Bodoni MT Black",
    color = "grey20",
    size = 30
  ) +
  draw_text(
    y = .48,
    x = .1,
    hjust = 0,
    "The BBC has revealed its list of 100 inspiring and influential women from around\nthe world for 2020. This year, '100 Women' is highlighting those who are leading\nchange and making a difference during these turbulent times. These word-clouds\nexplore how the ninety-nine named women's accomplishments are described.",
    family = "Bodoni MT",
    color = "grey40"
  ) +
  draw_plot(random_plot, x=0.3,y=0, scale = 0.3)
  

