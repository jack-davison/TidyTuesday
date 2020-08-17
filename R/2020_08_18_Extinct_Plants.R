
library(tidytuesdayR)
library(tidyverse)
library(cowplot)
library(rnaturalearth)
library(ggforce)
library(sf)

tuesdata <- tidytuesdayR::tt_load("2020-08-18")
plants <- tuesdata$plants
actions <- tuesdata$actions

# Dot plot ----------------------------------------------------------------

a <- actions %>%
  filter(
    !action_type %in% c("Unknown", "Education & Awareness"),
    continent %in% c("Africa", "Oceania")
  ) %>%
  group_by(binomial_name) %>% mutate(tot = sum(action_taken)) %>% filter(tot != 0) %>%
  ungroup() %>%
  ggplot(aes(x = binomial_name, y = action_type)) +
  geom_point(aes(
    alpha = action_taken,
    size = action_taken,
    color = action_type
  )) +
  labs(y = "", x = "") +
  theme_minimal() +
  facet_row( ~ continent, scales = "free_x", space = "free") +
  theme(
    legend.position = "none",
    axis.text.x = element_blank(),
    #element_text(angle = 45, hjust = 1),
    panel.background = element_rect(fill = "#E8EDEB", color = NA),
    panel.grid.major.x = element_line(color = "white"),
    strip.text = element_text(face = "bold"),
    axis.text.y = element_blank()
  ) + #element_text(face = "bold")) +
  scale_color_manual(values = c("#4B7F52", "#7B5E7B", "#6D72C3", "#DA4167")) +
  scale_alpha(range = c(0.1, 1)) +
  scale_size(range = c(1, 2))

b <- actions %>%
  mutate(continent = str_replace(continent, "South", "S.")) %>%
  filter(
    !action_type %in% c("Unknown", "Education & Awareness"),!continent %in% c("Africa", "Oceania")
  ) %>%
  group_by(binomial_name) %>% mutate(tot = sum(action_taken)) %>% filter(tot != 0) %>%
  ungroup() %>%
  ggplot(aes(x = binomial_name, y = action_type)) +
  geom_point(aes(
    alpha = action_taken,
    size = action_taken,
    color = action_type
  )) +
  labs(y = "", x = "") +
  theme_minimal() +
  facet_row( ~ continent, scales = "free_x", space = "free") +
  theme(
    legend.position = "none",
    axis.text.x = element_blank(),
    #element_text(angle = 45, hjust = 1),
    panel.background = element_rect(fill = "#E8EDEB", color = NA),
    panel.grid.major.x = element_line(color = "white"),
    strip.text = element_text(face = "bold"),
    axis.text.y = element_blank()
  ) + #element_text(face = "bold")) +
  scale_color_manual(values = c("#4B7F52", "#7B5E7B", "#6D72C3", "#DA4167")) +
  scale_alpha(range = c(0.1, 1), guide = "none") +
  scale_size(range = c(1, 2), guide = "none")

dots <- plot_grid(a, b, ncol = 1)

# Map --------------------------------------------------------------------

world <- ne_countries(scale = "medium", returnclass = "sf")

country_actions <- actions %>% filter(action_taken == 1,!action_type %in% c("Unknown", "Education & Awareness")) %>%
  count(country, action_type, sort = T) %>%
  group_by(country) %>%
  filter(n == max(n)) %>%
  mutate(n_dist = n_distinct(action_type)) %>%
  ungroup() %>%
  mutate(action = if_else(n_dist > 1, "Mixed Strategy", action_type)) %>% 
select(country, action) %>%
  unique() %>%
  rename(name = country) %>%
  mutate(
    name = case_when(
      name == "Pitcairn" ~ "Pitcairn Is.",
      name == "Cook Island" ~ "Cook Is.",
      name == "Viet Nam" ~ "Vietnam",
      name == "Cabo Verde" ~ "Cape Verde",
      name == "Sao Tome and Principe" ~ "São Tomé and Principe",
      name == "French Polynesia" ~ "Fr. Polynesia",
      TRUE ~ name
    )
  )

map <- left_join(world, country_actions) %>%
  mutate(action = if_else(is.na(action), "No Data Available", action)) %>%
  mutate(action = factor(
    action,
    levels = c(
      "Species Management",
      "Research & Monitoring",
      "Law & Policy",
      "Land & Water Protection",
      "Mixed Strategy",
      "No Data Available"
    )
  )) %>%
  filter(continent != "Antarctica") %>%
  ggplot(aes(fill = action)) + geom_sf(color = NA) +
  theme_void() +
  scale_fill_manual(
    values = c(
      "Species Management" = "#DA4167",
      "Research & Monitoring" = "#6D72C3",
      "Law & Policy" = "#7B5E7B",
      "Land & Water Protection" = "#4B7F52",
      "No Data Available" = "#DDE3E0",
      "Mixed Strategy" = "#0B3948"
    )
  ) +
  labs(fill = "Most Common Known Strategy") +
  theme(
    legend.position = "top",
    legend.text = element_text(size = 7, vjust = .5),
    legend.key.size = unit(0.4, "cm"),
    legend.title.align = 0.5,
    legend.title = element_text(
      family = "Bodoni MT",
      face = "bold",
      size = 20
    ),
    plot.background = element_rect(fill = "#F4F6F5", color = NA),
    plot.margin = unit(c(1, 1, 1, 1), "cm")
  ) +
  guides(fill = guide_legend(title.position = "top"))

# Assemble Plot -----------------------------------------------------------

titles <- ggdraw() +
  labs(title = "\nMISSION: BIODIVERSITY",
       subtitle = "The Global Effort to Save Our Plant Species\n\n") +
  theme(
    plot.title = element_text(
      hjust = .5,
      vjust = .5,
      family = "Bodoni MT Black",
      size = 30
    ),
    plot.subtitle = element_text(
      hjust = .5,
      vjust = .5,
      family = "Bodoni MT",
      size = 15
    )
  )

bigplot <- plot_grid(
  titles,
  map,
  dots + theme(plot.margin = unit(c(.3, .5, .3, .1), "cm")),
  ncol = 1,
  rel_heights = c(0.2, .8, .5)
)

ggdraw(bigplot) +
  draw_label(
    label = "Each vertical band represents a different plant species. A dot indicates that the corresponding strategy is being employed",
    size = 7,
    y = 0.03,
    fontface = "italic"
  ) +
  draw_label(
    label = "The USA is the only\ncountry to focus on research\nand monitoring above\nother strategies",
    size = 7,
    y = 0.59,
    x = .125,
    color = "#6D72C3"
  ) +
  draw_label(
    label = "Much of South America\nemploys many different\nstrategies equally",
    size = 7,
    y = 0.475,
    x = .25,
    color = "#0B3948"
  ) +
  draw_label(
    label = "Madagascar is home to\nnearly 20% of the extinct\nspecies. 50% of its strategies\nfocus on land & water management",
    size = 7,
    y = 0.43,
    x = .67,
    color = "#4B7F52"
  ) +
  draw_label(
    label = "Species management is\nemployed all around the\nworld, but commonly only most\npopular in countries with fewer\nextinct species",
    size = 7,
    y = 0.6,
    x = .9,
    color = "#DA4167"
  )
