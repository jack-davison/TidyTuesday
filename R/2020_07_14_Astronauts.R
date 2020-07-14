library(tidytuesdayR)
library(tidyverse)
library(ggtext)

tuesdata <- tidytuesdayR::tt_load('2020-07-14')

astro <- tuesdata$astronauts %>%
  mutate(age_selected = year_of_selection - year_of_birth,
         age_mission = year_of_mission - year_of_birth)

# Work out means
astro_means <- astro %>%
  group_by(name) %>%
  filter(mission_number == max(mission_number)) %>%
  pivot_longer(cols = c(age_mission, age_selected),
               names_to = "agetype") %>%
  group_by(agetype) %>%
  summarise(mean = as.integer(mean(value))) %>%
  mutate(
    agetype = str_remove(agetype, "age_"),
    agetype = ifelse(agetype == "selected", "selection", "mission")
  )

# Pick out the most recent mission
astro %>%
  group_by(name) %>%
  filter(mission_number == max(mission_number)) %>%
  pivot_longer(cols = c(age_mission, age_selected),
               names_to = "agetype") %>%
  
  # the plot!
  ggplot(aes(value, fill = agetype)) +
  
  # geoms & annotations
  geom_histogram(position = position_identity(),
                 alpha = .9,
                 binwidth = 1) +
  geom_vline(
    data = astro_means,
    aes(xintercept = mean),
    lty = 2,
    colour = "white"
  ) +
  annotate(
    "text",
    x = 72,
    y = 38,
    label = "John Glenn was 77\non his last mission -\nthe oldest person to\ntravel in space!",
    colour = "white",
    vjust = 0.5
  ) +
  annotate(
    "curve",
    xend = 77,
    yend = 4,
    x = 75,
    y = 26,
    curvature = -.2,
    arrow = arrow(type = "closed", length = unit(1, "lines")),
    colour = "white"
  ) +
  geom_text(
    data = astro_means,
    inherit.aes = F,
    y = 60,
    hjust = 0,
    aes(
      x = mean + 1,
      colour = agetype,
      label = paste0("Mean age at\n", agetype, " = ", mean)
    )
  ) +
  
  # defining colours
  scale_fill_manual(values = c("#eeff00", "#1CCAD8")) +
  scale_color_manual(values = c("#eeff00", "#1CCAD8")) +
  
  # labels, captions, axes, etc.
  labs(
    y = "",
    x = "Age of Astronaut / Years",
    caption = "Data from the Astronaut Database (https://data.mendeley.com/datasets/86tsnnbv2w/1)",
    title = '"How old were astronauts on their most recent mission?"',
    subtitle = "Age of astronauts when they were <b style='color:#1CCAD8'>selected</b>
         and when they were sent on their <b style='color:#eeff00'>mission</b>.<br>"
  ) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 67)) +
  
  # themes
  theme(
    rect = element_rect(fill = "#373F51"),
    text = element_text(colour = "white"),
    line = element_line(colour = "white"),
    title = element_text(colour = "white"),
    axis.text = element_text(colour = "white"),
    axis.ticks = element_blank(),
    panel.background = element_rect(fill = "#373F51"),
    axis.line = element_blank(),
    legend.position = "none",
    axis.title.x = element_text(hjust = 1),
    panel.grid.major = element_line(colour = "#3e4c6d"),
    panel.grid.minor = element_blank(),
    plot.title = element_text(face = "bold"),
    plot.subtitle = element_markdown(),
    plot.caption = element_text(hjust = 0, colour = "#7383a8"),
    plot.margin = unit(c(1, 1, 1, 1), "cm")
  )
