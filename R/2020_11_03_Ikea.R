
library(tidyverse)
library(tidytuesdayR)
library(extrafont)
font_import()

loadfonts(device = "win")

tuesdata = tt_load("2020-11-03") %>% 
  .$ikea %>%
  select(-X1) %>%
  mutate(price = price * .2)

tuesdata %>% glimpse()

tuesdata %>% count(category, sort = T)

wardrobes = tuesdata %>% filter(category == "Wardrobes")

tuesdata

tuesdata %>% 
  na.omit() %>%
  filter(fct_lump_n(category, n = 9) != "Other") %>%
  add_count(category) %>%
  mutate(category = paste0(str_to_title(category), "\nn = ",n),
         category = fct_reorder(.f = category, .x = price),
         dimension = height * width * depth) %>%
  group_by(category) %>%
  mutate(dimension = dimension / max(dimension)) %>%
  ggplot(aes(y = category, x = price)) +
  geom_point(aes(size = dimension, color = category), alpha = .2,
             position = position_jitter(seed = 20201103)) +
  geom_boxplot(outlier.color = NA, fill = "white", alpha = .5, size = .5) +
  scale_x_continuous(labels = scales::dollar_format(prefix = "£")) +
  theme_light() +
  theme(aspect.ratio = .6,
        legend.position = "none", 
        plot.title.position = "plot", 
        plot.caption.position = "plot",
        plot.title = element_text(face = "bold", size = 30),
        plot.subtitle = element_text(size = 12.5),
        plot.caption = element_text(size = 10), 
        plot.margin = unit(c(1,1,1,1), "cm")) +
  labs(y = "", x = "",
       title = '"How Much?!?"',
       subtitle = "The price distribution of nine different types of IKEA furniture. The size of each point corresponds to the dimensions\nof the item of furniture (Depth x Width x Height), normalised within each furniture type.\n\nBigger items typically cost more - no great surprise there!\n",
       caption = "Visualisation by Jack Davison (@JDavison_)\nData from www.kaggle.com/ahmedkallam/ikea-sa-furniture-web-scraping") +
  scale_color_manual(values = c("#ffda1a","#ecc745","#d9b45e","#c5a271","#af9082",
                                "#977f91","#7b6f9f","#575fad","#0051ba"))


tuesdata %>% 
  na.omit() %>%
  filter(fct_lump_n(category, n = 9) != "Other") %>%
  mutate(category = str_to_title(category),
         category = fct_reorder(.f = category, .x = price),
         dimension = height * width * depth) %>%
  group_by(category) %>%
  mutate(dimension = dimension / max(dimension),
         price = price / max(price)) %>%
  ggplot(aes(x = dimension, y = price, fill = category, color = category)) + 
  geom_smooth() +
  facet_wrap(~category) +
  gghighlight::gghighlight() +
  theme_light() + 
  theme(aspect.ratio = 1, legend.position = "none") +
  scale_color_manual(values = c("#ffda1a","#ecc745","#d9b45e","#c5a271","#af9082",
                                "#977f91","#7b6f9f","#575fad","#0051ba")) +
  scale_fill_manual(values = c("#ffda1a","#ecc745","#d9b45e","#c5a271","#af9082",
                                "#977f91","#7b6f9f","#575fad","#0051ba"))

# tuesdata %>% 
#   filter(fct_lump_n(category, n = 9) != "Other") %>%
#   mutate(category = fct_reorder(.f = category, .x = price, .fun = max)) %>%
#   ggplot(aes(price, group = category)) +
#   stat_ecdf() +
#   gghighlight::gghighlight() +
#   facet_wrap(~category) +
#   theme(aspect.ratio = 1)

tuesdata$short_description

geom_
