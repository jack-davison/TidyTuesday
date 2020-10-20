
library(tidyverse)
library(tidytuesdayR)
library(here)

beer_awards <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-10-20/beer_awards.csv')

sf::read_sf()

beer_awards %>% 
  filter(medal == "Gold") %>%
  count(state, sort = T)
