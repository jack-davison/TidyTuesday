
library(tidytuesdayR)
library(tidyverse)
library(rnaturalearth)
library(cowplot)

tidytuesdayR::tt_available()

tuesdata <- tidytuesdayR::tt_load("2020-09-01")

world <- ne_countries(scale = "medium", returnclass = "sf")

key_crops <- tuesdata$key_crop_yields %>%
  janitor::clean_names() %>%
  filter(year == 2018) %>%
  select(-code,-year) %>%
  rename(name = entity) %>%
  mutate(
    name = case_when(
      name == "Bosnia and Herzegovina" ~ "Bosnia and Herz.",
      name == "Central African Republic" ~ "Central African Rep.",
      name == "Cote d'Ivoire" ~ "Côte d'Ivoire",
      name == "Democratic Republic of Congo" ~ "Dem. Rep. Congo",
      name == "Czech Republic" ~ "Czech Rep.",
      name == "Dominican Republic" ~ "Dominican Rep.",
      name == "Equatorial Guinea" ~ "Eq. Guinea",
      name == "Laos" ~ "Lao PDR",
      name == "North Korea" ~ "Dem. Rep. Korea",
      name == "Western Sahara" ~ "W. Sahara",
      name == "South Sudan" ~ "S. Sudan",
      name == "Solomon Islands" ~ "Solomon Is.",
      name == "Timor" ~ "Timor-Leste",
      TRUE ~ name
    )
  ) %>%
  pivot_longer(c(-name), names_to = "crop", values_to = "val") %>%
  filter(!is.na(crop)) %>%
  mutate(
    val = if_else(is.na(val), 0, val),
    crop = str_remove(crop, "_tonnes_per_hectare"),
    crop = str_remove(crop, "_"),
    crop = str_to_title(crop)
  ) %>%
  group_by(crop) %>%
  mutate(val = scales::rescale(val)) %>%
  left_join(world, ., by = "name") %>%
  filter(!is.na(val),!is.na(crop))

max <- key_crops %>%
  group_by(crop) %>%
  filter(val == max(val))

plot <- ggplot(key_crops) +
  geom_sf(aes(fill = crop, alpha = val), color = NA) +
  geom_sf(data = max, color = NA, fill = "white") +
  geom_text(
    data = key_crops %>% select(crop) %>% unique(),
    x = -140,
    y = -55,
    aes(label = toupper(crop), color = crop),
    check_overlap = T,
    angle = 90,
    size = 5,
    hjust = 0,
    vjust = .5
  ) +
  geom_text(
    data = max %>% select(name, crop) %>% unique(),
    x = -125,
    y = -55,
    aes(label = name),
    check_overlap = T,
    angle = 90,
    size = 3,
    hjust = 0,
    vjust = .5,
    color = "white"
  ) +
  facet_wrap( ~ crop, ncol = 3) +
  theme_void() +
  theme(
    strip.background = element_blank(),
    strip.text = element_blank(),
    legend.position = "none",
    plot.background = element_rect(fill = "grey10", color = NA),
    plot.margin = unit(c(1, 1, 1, 1), "cm"),
    plot.title = element_text(
      color = "white",
      hjust = .5,
      size = 40
    ),
    plot.subtitle = element_text(
      color = "grey80",
      hjust = .5,
      face = "italic"
    )
  ) +
  scale_alpha(range = c(0.05, 1)) +
  scale_fill_manual(
    values = c(
      "#33A8C7",
      "#52E3E1",
      "#A0E426",
      "#FDF148",
      "#FFAB00",
      "#F77976",
      "#A63446",
      "#F050AE",
      "#D883FF",
      "#9336FD",
      "#52489C"
    )
  ) +
  scale_color_manual(
    values = c(
      "#33A8C7",
      "#52E3E1",
      "#A0E426",
      "#FDF148",
      "#FFAB00",
      "#F77976",
      "#A63446",
      "#F050AE",
      "#D883FF",
      "#9336FD",
      "#52489C"
    )
  ) +
  labs(title = "2018 World Crop Capitals",
       subtitle = "\"Which countries have the greatest crop yeilds per hectare?\"\n\n")

ggdraw(plot = plot) +
  draw_text(
    text = "Guide",
    fontface = "bold",
    color = "white",
    hjust = 0,
    vjust = .5,
    x = .68,
    y = .18,
    size = 15
  ) +
  draw_text(
    text = "\nFill colors are normalised per crop type. The more\nsaturated the color, the greater the crop yeild. The\n'crop capital' with the greatest yeild per hectare is\nhighlighted in white.",
    color = "grey80",
    hjust = 0,
    vjust = .5,
    x = .68,
    y = .12,
    size = 10
  )
