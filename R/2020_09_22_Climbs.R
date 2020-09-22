

library(tidytuesdayR)
library(tidyverse)
library(cowplot)

# Read in data

tuesdata = tidytuesdayR::tt_load("2020-09-22")

members = tuesdata$members
peaks = tuesdata$peaks

# Prepare to plot

ggthemr::ggthemr("greyscale", layout = "scientific")

perc = function(x) {
  paste0(x, "%")
}

combined = left_join(members, peaks) %>%
  filter(died | injured) %>%
  mutate(
    Injury = 100 * injury_height_metres / height_metres,
    Death = 100 * death_height_metres / height_metres
  ) %>%
  pivot_longer(cols = c(Injury, Death))

num_died = members %>% count(died) %>% filter(died) %>% .$n

num_injured = members %>% count(injured) %>% filter(injured) %>% .$n

# Useful info:

combined %>% count(death_cause, sort = T)

combined %>% filter(value < 25, name == "Death", climbing_status == "Unclimbed") %>% glimpse()

# Plot:

plot = ggplot(combined, aes(x = value, fill = name, color = climbing_status)) +
  geom_density(alpha = .9, color = NA) +
  labs(
    x = "",
    y = "",
    fill = "",
    title = "The Dangers of the Himalayas",
    subtitle = paste0(
      "There have sadly been ",
      num_injured,
      " recorded injuries and ",
      num_died,
      " deaths of climbers in the Himalayas. These distributions\nshow how far to the peak these climbers were before their injury or demise. Unsurprisingly, climbs grow more\ndangerous the closer one gets to the peak. The most common causes of death in the Himalayas are related to\navalanches, followed by falls and altitude sickness.\n"
    ),
    caption = "Data from the Himalayan Database (https://www.himalayandatabase.com/)\n
       Visualisation by Jack Davison (Twitter @JDavison_)\n
       Code found at github.com/jack-davison"
  ) +
  scale_x_continuous(limits = c(0, 100), labels = perc) +
  scale_y_continuous(expand = c(0, 0)) +
  facet_wrap( ~ climbing_status, strip.position = "top") +
  theme(
    axis.line.y = element_blank(),
    axis.text.y = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    axis.ticks.y = element_blank(),
    panel.spacing = unit(1, "cm"),
    aspect.ratio = 1,
    strip.placement = "outside",
    legend.position = c(.1, .95),
    legend.direction = "horizontal",
    plot.margin = unit(rep(.5, 4), "cm"),
    strip.text = element_text(face = "bold"),
    plot.title = element_text(size = 20),
    plot.caption = element_text(size = 8, lineheight = .5)
  )

ggdraw(plot) +
  draw_text(x = 0.595,
            y = 0.44,
            text = "This early death\nwas a 1992 attempt to\nclimb Jannu East",
            size = 8)
