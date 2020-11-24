
library(tidytuesdayR)
library(tidyverse)
library(stringr)

tuesdata <- tidytuesdayR::tt_load('2020-11-24')

data = tuesdata$hike_data %>%
  mutate(across(gain:rating, as.numeric),
         length = parse_number(length)) %>%
  unnest(cols = c(features)) %>%
  filter(features != "Dogs not allowed")

inits = data.frame(name = (data %>% pull(features) %>% unique())) %>%
  mutate(init = case_when(name == "Dogs allowed on leash" ~ "DA",
                          name == "Wildlife" ~ "Wl",
                          name == "Good for kids" ~ "GK",
                          name == "Lakes" ~ "Lk",
                          name == "Fall foliage" ~ "FF",
                          name == "Ridges/passes" ~ "RP",
                          name == "Established campsites" ~ "EC",
                          name == "Mountain views" ~ "MV",
                          name == "Old growth" ~ "OG",
                          name == "Waterfalls" ~ "Wf",
                          name == "Wildflowers/Meadows" ~ "WM",
                          name == "Rivers" ~ "Ri",
                          name == "Coast" ~ "Co",
                          name == "Summits" ~ "Su"), 
         type = if_else(init %in% c("DA","DN","GK"), "Companion", "Feature"))

correlation = data %>% 
  pairwise_cor(item = features, feature = name) %>%
  filter(abs(correlation) > 0.1) %>%
  as_tbl_graph() %>%
  activate(nodes) %>%
  left_join(inits) %>%
  arrange(name)

ggraph(correlation) +
  geom_edge_link(aes(color = correlation, width = abs(correlation))) +
  geom_node_point(aes(color = type), size = 10) +
  geom_node_text(aes(label = init), color = "#2a2a2aff") +
  scale_edge_color_gradient2(high = "#8aab37ff", low = "#00557bff", name = "CORR.") +
  scale_edge_width(range = c(1,3), guide = F) +
  theme_graph() +
  theme(plot.background = element_rect(fill = "#f3f3f3ff"),
        text = element_text(color = "#2a2a2aff", family = "Berlin Sans FB"), 
        legend.title = element_text(face = "bold", family = "Berlin Sans FB Demi"),
        plot.title = element_text(face = "bold", family = "Berlin Sans FB Demi", size = 30),
        plot.subtitle = element_text(family = "Berlin Sans FB"),
        legend.box.background = element_rect(fill = "white", color = NA),
        legend.box.margin = margin(.5,.5,.5,.5,"cm")) +
  scale_discrete_identity(aesthetics = "label",
                          name = "KEY",
                          breaks = activate(correlation, "nodes") %>% pull(init),
                          labels = paste0("—  ", activate(correlation, "nodes") %>% pull(name)),
                          guide = "legend") +
  scale_color_manual(values = c("#8a8a8a","#bdbdbdff"), name = "CATEGORY") +
  labs(title = "HIKING IN WASHINGTON:\nWHAT YOU'LL SEE AND WHO YOU'LL SEE IT WITH",
       subtitle = "The Washington Trails Association helpfully provides a hiking guide written by local experts. Each trail is flagged with different features one will encounter\n- rivers, mountains, waterfalls - and the companions you can bring - children and/or dogs.\n\nThis graph explores the underlying relationship between these two categories. It is seen that child-friendly routes are also commonly dog-friendly, and that\nthe Trails Association discourages bringing children to mountainous routes with features like summits, ridges and passes. Kids are more than welcome to\nenjoy wildlife and coastal routes, however, and your dog will happy to romp in the fall foliage!") +
  guides(edge_color = guide_edge_colorbar(direction = "horizontal"))

