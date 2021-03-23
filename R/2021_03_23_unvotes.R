
library(tidyverse)
library(tidytuesdayR)
library(widyr)
library(tidygraph)
library(ggraph)
library(rnaturalearth)
library(cowplot)
library(extrafont)

ggthemr::ggthemr("light")

# Read in Data ------------------------------------------------------------

tuesdata = tt_load("2021-03-23") 

unvotes = tuesdata$unvotes %>%
  left_join(countrycode::codelist %>% select(country_code = iso2c, continent))

euro = unvotes %>% filter(continent == "Europe")

europe = rnaturalearth::ne_countries(continent = "Europe", scale = "medium") %>% sf::st_as_sf()

# Correlations ------------------------------------------------------------

euro_corrs = euro %>%
  mutate(vote = if_else(vote == "yes", TRUE, FALSE)) %>%
  pairwise_cor(tbl = ., item = country_code, feature = rcid, value = vote)

tidytab = euro_corrs %>%
  filter(correlation > .5) %>%
  as_tbl_graph(directed = F) %>%
  mutate(block = as.factor(group_edge_betweenness(weights = correlation)))

set.seed(123)

graph = ggraph(tidytab, layout = "kk") +
  geom_edge_link(aes(alpha = correlation)) +
  geom_node_point(shape = 21, aes(fill = block), color = "white", size = 12, stroke = 1.5) +
  geom_node_text(aes(label = name), color = "black") +
  theme_graph(base_family = "sans") +
  ggthemr::scale_colour_ggthemr_d() +
  theme(legend.position = "none")

# Map ---------------------------------------------------------------------

blocks = tidytab %>% 
  as.data.frame() %>% 
  tibble() %>%
  rename(iso_a2 = name) %>%
  left_join(select(unvotes, iso_a2 = country_code)) %>%
  distinct()

euro_blocks = europe %>%
  left_join(blocks)

map = ggplot(euro_blocks) +
  geom_sf(aes(fill = block)) +
  # geom_sf_text(aes(label = iso_a2), color = "grey25") +
  scale_x_continuous(limits = c(-40, 85)) +
  scale_y_continuous(limits = c(35, 80)) +
  theme_void() +
  theme(legend.position = "none", panel.border = element_rect(fill = NA)) +
  scale_fill_manual(values = ggthemr::swatch()[-1], na.value = "grey90")

map

# Title -------------------------------------------------------------------

text = ggplot() +
  coord_cartesian(ylim = c(0,1), xlim = c(0,1)) +
  theme_void() +
  annotate(geom = "text", family = "Bodoni MT Black", x = .1, y = .5, hjust = 0,
           label = "UN Voting in Europe", color = "black", face = "bold", size = 5) +
  annotate(geom = "text", family = "Bodoni MT", x = .1, y = .375, hjust = 0,
           label = "This correlation graph shows the European countries which tend to vote together in UN votes (R > 0.5).\nCountries are clustered based on edge betweenness. There is a relatively clear west-east split.", 
           color = "black", face = "bold", size = 2)


# Combine -----------------------------------------------------------------

ggdraw(graph) +
  draw_plot(map, x = .75, y = .75, scale = .4, hjust = .5, vjust = .5) +
  draw_text(family = "Bodoni MT Black", x = .1, y = .25, hjust = 0,
            text = "UN Voting in Europe", color = "black", face = "bold", size = 40) +
  draw_text(family = "Bodoni MT", x = .1, y = .15, hjust = 0,
            text = "This correlation graph shows the European countries which tend to vote together\nin UN votes (R > 0.5). Countries are clustered based on edge betweenness. There\nis a relatively clear east-west split.", color = "black", face = "bold", size = 12)
