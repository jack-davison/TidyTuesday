
library(tidytuesdayR)
library(tidyverse)

tuesdata = tt_load("2020-11-10")

ggthemr::ggthemr(palette = "earth")

mob  = tuesdata$mobile
land = tuesdata$landline

data = left_join(mob, select(land, -total_pop, -gdp_per_cap))
  
earliest = data %>%
  mutate(check = mobile_subs > landline_subs) %>%
  group_by(entity, check) %>%
  summarise(min = min(year)) %>%
  filter(check)

plot_data = data %>% 
  left_join(earliest) %>%
  filter(year == min) %>%
  select(-min, -check) %>%
  group_by(continent) %>%
  mutate(max = max(year), min = min(year))

early_and_late = plot_data %>%
  group_by(continent) %>%
  filter(year == max | year == min)

late = early_and_late %>%
  filter(year == max) %>%
  group_by(continent) %>%
  summarise(late_entity = paste(entity, collapse = "\n"))

early = early_and_late %>%
  filter(year == min) %>%
  group_by(continent) %>%
  summarise(early_entity = paste(entity, collapse = "\n")) %>%
  mutate(early_entity = str_replace_all(early_entity, "Democratic Republic of Congo", "Dem. Rep. Congo"))

plot_data %>% 
  left_join(early, by = "continent") %>%
  left_join(late, by = "continent") %>%
  ggplot(aes(year, color = continent)) +
  geom_histogram(aes(fill = continent), color = NA, position = position_identity(), binwidth = 1) +
  facet_wrap(~continent, ncol = 1, strip.position = "right") + 
  gghighlight::gghighlight(use_direct_label = F) +
  theme_minimal() +
  geom_segment(aes(x = min, xend = min, y = 12, yend = 1), color = "grey20", arrow = arrow(type = "closed", length = unit(.2,"cm"))) +
  geom_segment(aes(x = max, xend = max, y = 12, yend = 1), color = "grey20", arrow = arrow(type = "closed", length = unit(.2,"cm"))) +
  geom_label(aes(x = min, y = 12, label = early_entity), size = 3, alpha = .5) +
  geom_label(aes(x = max, y = 12, label = late_entity), size = 3, alpha = .5) +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        legend.position = "none",
        strip.text = element_text(size = 10), 
        plot.title = element_text(face = "bold", size = 20, family = "serif"), 
        plot.title.position = "plot", plot.caption.position = "plot", 
        plot.margin = unit(rep(1,4), "cm")) +
  labs(y = "", x = "", title = "THE COMMUNICATION REVOLUTION",
       subtitle = "The years in which countries had more fixed mobile subscriptions than landline subscriptions.\nThe earliest and latest adopters of mobile phones are labelled for each continent\n",
       caption = "Data from OurWorldInData.org\nVisualisation by Jack Davison (Twitter @JDavison_)\nCode found at github.com/jack-davison")

