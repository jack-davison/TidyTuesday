
library(tidyverse)
library(tidytuesdayR)
library(cowplot)

beer_awards <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-10-20/beer_awards.csv')

counts = beer_awards %>% 
  count(state) %>% 
  mutate(nn = n / 15) %>% 
  left_join(data.frame(state = datasets::state.abb,
                       name = datasets::state.name)) %>%
  mutate(name = if_else(state == "DC", "Washington DC", name)) %>%
  filter(!is.na(name))

plot = counts %>%
  filter(n > 25) %>%
  ggplot(aes(x = 1, y = n, size = n)) +
  scale_size(range = c(2,10)) +
  geom_polygon(inherit.aes = F, aes(x = x, y = n),
               data = data.frame(x = c(.65, .65, .6, .6, 1.5, 1.5, 1.45, 1.45),
                                 n = c(-100, 450, 650, 1100, 1100, 650, 450, -100)),
               fill = "#171219", color = NA, alpha = .5) +
  geom_polygon(inherit.aes = F, aes(x = x, y = n),
               data = data.frame(x = c(.6, .6, .55, .55, 1.45, 1.45, 1.4, 1.4),
                                 n = c(-50, 500, 700, 1000, 1000, 700, 500, -50)),
               fill = "#F6C101", color = NA) +
  geom_polygon(inherit.aes = F, aes(x=x,y=n),
               data = data.frame(x = c(.55, .55, 1.45, 1.45),
                                 n = c(1000, 1150, 1150, 1000)),
               fill = "white", color = NA) +
  geom_hline(yintercept = c(0,250,500,750,1000), lty = 2, color = "#171219") +
  geom_point(shape = 21, position = position_jitter(seed = 28, width = .35),
             color = "#C96E12", fill = "#EC9D00") +
  geom_text(
    aes(label = if_else(n > 225, name, state), size = nn),
    hjust = .5,
    vjust = .5, 
    position = position_jitter(seed = 28, width = .35)
  ) +
  theme_void() +
  theme(aspect.ratio = 1.5,
        legend.position = "none",
        plot.background = element_rect(color = NA, fill = "#225560"),
        plot.margin = unit(c(1,12,1,2), "cm"), 
        axis.text.y = element_text(color = "white")) +
  scale_y_continuous(breaks = seq(0,1000,250))

ggdraw(plot) +
  cowplot::draw_text("Great American\nBeer Festival", x = .555, y = .705, size = 30, hjust = 0, color = "#171219") +
  cowplot::draw_text("Great American\nBeer Festival", x = .55, y = .71, size = 30, hjust = 0, color = "#F6C101") +
  cowplot::draw_text("The Professional Judge Panel awards gold, silver or bronze\nmedals that are recognized around the world as symbols of\nbrewing excellence. These awards are among the most\ncoveted in the industry and heralded by the winning brewers\nin their national advertising. ", x = .55, y = .58, size = 8, hjust = 0, vjust = 1, color = "white") +
  cowplot::draw_text("In this beer, each US State with greater than 25 medals since\n1987 is represented by a bubble. The higher and larger the\nbubble in the pint the greater the number of medals recieved.\nCalifornia is way in front, with 962 medals!", 
                     x = .55, y = .40, size = 8, hjust = 0, vjust = 1, color = "white") +
  cowplot::draw_text("Data from greatamericanbeerfestival.com/the-competition/winners\nVisualisation by Jack Davison (Twitter @JDavison_)\nCode found at github.com/jack-davison", 
                     x = .55, y = .25, size = 7, hjust = 0, vjust = 1, color = "#171219")

