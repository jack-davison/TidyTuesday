
library(tidytuesdayR)
library(tidyverse)
library(ggraph)
library(tidygraph)
library(rsvg)
library(cowplot)
library(ggtext)

# Read in Data -------------------------------------------------------------

setwd("~/TidyTuesday")

chopped <- tt_load('2020-08-25') %>%
  .$chopped %>%
  mutate(air_date = lubridate::mdy(air_date))

#img <- rsvg("drawing.svg")


# Find out the average rating for an episode with a given judge -----------

judge_ratings <- chopped %>%
  pivot_longer(contains("judge"), names_to = "x", values_to = "name") %>%
  mutate(name = stringi::stri_trans_general(name, "Latin-ASCII")) %>% # remove accented characters - caused issues later
  group_by(name) %>%
  summarise(rating = median(episode_rating, na.rm = T),
            n = n()) %>%
  mutate(rating = plyr::round_any(rating,.5),
         rating = factor(rating)) # rounding to nearest .5


# Look at combos of judges ------------------------------------------------

# Amanda F. and Chris S. are misspelled a handful of times - could have used case_when() here perhaps
judges <- chopped %>% select(contains("judge")) %>% 
  mutate(across(everything(), stringi::stri_trans_general, "Latin-ASCII")) %>%
  mutate(judge1 = if_else(judge1 == "Amanda Freita", "Amanda Freitag", judge1),
         judge2 = if_else(judge2 == "Amanda Freita", "Amanda Freitag", judge2),
         judge3 = if_else(judge3 == "Amanda Freita", "Amanda Freitag", judge3),
         judge1 = if_else(judge1 == "Chris Santo", "Chris Santos", judge1),
         judge2 = if_else(judge2 == "Chris Santo", "Chris Santos", judge2),
         judge3 = if_else(judge3 == "Chris Santo", "Chris Santos", judge3))

# Looking for combos between judges 1 and 2, 2 and 3, and 1 and 3. We then bind them together.

a <- judges %>% select(judge1, judge2) %>%
  group_by(judge_1 = pmin(judge1, judge2), judge_2 = pmax(judge1, judge2)) %>%
  summarise(n = n())

b <- judges %>% select(judge2, judge3) %>%
  group_by(judge_1 = pmin(judge2, judge3), judge_2 = pmax(judge2, judge3)) %>%
  summarise(n = n())

c <- judges %>% select(judge1, judge3) %>%
  group_by(judge_1 = pmin(judge1, judge3), judge_2 = pmax(judge1, judge3)) %>%
  summarise(n = n())


# Create graph ------------------------------------------------------------

graph <- 
  full_join(a, b, by = c("judge_1","judge_2")) %>%
  full_join(c, by = c("judge_1","judge_2")) %>%
  rowwise() %>%
  mutate(ntot = sum(n.x, n.y, n, na.rm = T)) %>% # add up each of these judge combos
  ungroup() %>%
  select(-n.x, -n.y, -n) %>%
  as_tbl_graph()%>%
  left_join(judge_ratings, by = "name") %>% # combine with judge ratings
  filter(n > 2) %>%
  mutate(name = str_replace(name, " ", "\n")) %>% # put the names on two lines
  activate(nodes) %>% 
  arrange(name) # arrange names alphabetically


# Create plot -------------------------------------------------------------

graphed <- ggraph(graph, layout = 'linear', circular = TRUE) +
  ggraph::geom_edge_arc(
    aes(alpha = ntot),
    width = 1,
    show.legend = F,
    color = "white"
  ) +
  ggraph::geom_node_point(aes(color = rating)) +
  ggraph::geom_node_label(
    aes(label = name, color = rating),
    alpha = .75,
    label.size = 0,
    fill = "#291720",
    size = 3
  ) +
  theme_void() +
  theme(
    legend.position = "none",
    plot.background = element_rect(color = NA, fill = NA),
    text = element_text(color = "white"),
    plot.margin = unit(c(1, 1, 1, 1), "cm"),
    plot.subtitle = element_markdown(lineheight = 1.5, hjust = .5),
    plot.title = element_text(size = 45, hjust = .5),
    plot.caption = element_markdown(size = 7, hjust = .5)
  ) +
  scale_color_manual(values = c("#820263", "#D90368", "#F75C03", "#5AB1BB", "blue")) +
  scale_alpha(range = c(0, 1)) +
  labs(
    title = "Chopped Judge Network",
    subtitle = "Over its 44 seasons, <i>Chopped</i> has had over a hundred judges in various combinations. This graph<br>shows how those who have judged more than once have worked together. A <b>bolder line</b> indicates<br>more time together on the judging panel. Colors indicate the average rating for episodes in which<br>the judge features according to IMDb; <b style='color:#820263'>7.5</b>/10, <b style='color:#D90368'>8.0</b>/10, <b style='color:#F75C03'>8.5</b>/10 and <b style='color:#5AB1BB'>9</b>/10.<br>",
    caption = "Data from <b>'Chopped: 10+ Years of Episode Data' </b>(kaggle.com/jeffreybraun/chopped-10-years-of-episode-data)<br>
       Visualisation by <b>Jack Davison</b> (Twitter @JDavison_)<br>
       Code found at <b>github.com/jack-davison</b>"
  ) +
  coord_equal()

# Add logo in background - not really needed - could change the above plot.background to what is below and end there.
ggdraw() + 
  draw_plot(ggplot()+theme_void()+ theme(plot.background = element_rect(color = NA, fill = "#291720"))) +
  draw_image(interpolate = F, img, clip = "off", scale = .4, x = 0, y = -0.05) +
  draw_plot(graphed)

