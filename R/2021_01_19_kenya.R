
library(tidyverse)
library(tidytuesdayR)
library(rKenyaCensus)
library(biscale)
library(cowplot)
library(extrafont)

windowsFonts()

map = rKenyaCensus::KenyaCounties_SHP %>%
  sf::st_as_sf()

tuesdata = tt_load("2021-01-19")

data = tuesdata$gender %>%
  janitor::clean_names() %>%
  filter(county != "Total") %>%
  select(-intersex) %>%
  mutate(`Ratio Male/Female` = male/female,
         `Total Population` = total,
         County = toupper(county), 
         .keep = "unused") %>%
  bi_class(x = `Ratio Male/Female`, y = `Total Population`, dim = 3)

leg = biscale::bi_legend(
  pal = "GrPink",
  dim = 3,
  xlab = "Total Population",
  ylab = "Ratio Male/Female", 
  size = 10
)

map %>%
  left_join(data) %>%
  ggplot() + 
  geom_sf(aes(fill = if_else(between(Area, 1000, 2000), "red", "white"))) +
  geom_sf_label(aes(label = if_else(between(Area, 1000, 2000), County, NULL), alpha = .5))

bi_map = map %>%
  left_join(data) %>%
  ggplot() + 
  geom_sf(aes(fill = bi_class), color = "white", show.legend = F) +
  bi_theme() +
  bi_scale_fill("GrPink", 3) +
  theme(plot.title.position = "plot", 
        plot.caption.position = "plot",
        text          = element_text(family = "Eras Light ITC"),
        plot.title    = element_text(size = 30, hjust = .5, family = "Eras Bold ITC"),
        plot.subtitle = element_text(size = 12.5, hjust = .5, color = "grey50"),
        plot.caption  = element_text(size =  8, hjust = .5)) +
  labs(title = "Kenya Population Census",
       subtitle = "Gender and Population Differences in the Counties of Kenya",
       caption = "Data from {rKenyaCensus}\nVisualisation by Jack Davison (Twitter @JDavison_ | Github jack-davison)")

mombasa = map %>%
  left_join(data) %>%
  filter(County == "MOMBASA") %>%
  ggplot() + 
  geom_sf(aes(fill = bi_class), color = "white", show.legend = F) +
  facet_wrap(~"Mombasa") +
  bi_scale_fill("GrPink", 3) +
  theme_void() +
  theme(text = element_text(size = 10))

ggdraw() +
  draw_plot(bi_map, 0, 0, 1, 1) +
  draw_plot(leg, 0.15, 0.09, 0.2, 0.2) +
  draw_plot(mombasa, 0.7, 0.1, .1, .1) +
  draw_line(
    x = c(0.715, 0.615),
    y = c(0.125, 0.15),
    arrow = arrow(type = "closed", length = unit(.3, "cm")),
    curvature = 0.5
    )


