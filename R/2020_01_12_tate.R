
library(tidyverse)
library(tidytuesdayR)
library(ggtext)
library(extrafont)

windowsFonts()

tuesdata = tt_load(x = "2021", week = 3)

art = tuesdata$artwork %>%
  filter(year > 1700) %>%
  mutate(artist = fct_lump_n(artist, n = 1, other_level = "Everybody Else")) %>%
  group_by(artist) %>%
  arrange(year) %>%
  mutate(id = row_number(),
         id = cumsum(id))

counts = art %>%
  count(artist, sort = T) %>%
  mutate(artist = if_else(str_detect(artist, "Joseph"),
                          "Joseph Mallord William Turner",
                          "any other artist"))

artist = "Joseph Mallord\nWilliam Turner"

art %>%
  ungroup() %>%
  # filter(str_detect(artist, "Mallord")) %>%
  summarise(range(year))

ggplot(art, aes(year, fill = artist)) +
  geom_hline(yintercept = c(1000,2000,3000), color = "grey80") +
  geom_histogram(position = "identity", alpha = .8, binwidth = 1) +
  theme_minimal() +
  theme(plot.background = element_rect(fill = "grey95", color = NA),
        legend.position = "none", 
        plot.caption = element_text(size = 6),
        axis.line.x = element_line(),
        panel.grid.major.x = element_blank(), 
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(), plot.margin = unit(rep(1,4), "cm")) + 
  scale_y_continuous(label = scales::comma, expand = expansion()) +
  scale_fill_manual(values = c("#7166aa", "#cc0066")) +
  labs(x = NULL, y = NULL, 
       caption = "Data from the Tate Art Museum (https://github.com/tategallery/collection)\nVisualisation by Jack Davison (Twitter @JDavison_ | Github jack-davison)") +
  annotate(
    geom = "text",
    x = 2014,
    y = 2950,
    hjust = 1,
    vjust = 1,
    family = "Rockwell Extra Bold",
    label = "Tate Art Museum",
    size = 8
  ) +
  annotate(
    geom = "text",
    x = 2014,
    y = 2750,
    hjust = 1,
    vjust = 1,
    family = "Rockwell Extra Bold",
    label = "Acquisitions",
    size = 10.7
  ) +
  annotate(
    geom = "richtext",
    x = 2014,
    y = 2450,
    hjust = 1,
    vjust = 1,
    family = "Rockwell",
    label = glue::glue("Between  1701  and  2014,  the  Tate  Art  Museum  acquired  {sum(counts$n)}  pieces.<br>Of  these,  an  amazing  {counts$n[1]}  were  from  <b  style='color:#7166aa'>{counts$artist[1]}</b>,<br>with  the  other  {counts$n[2]}  coming  from  <b  style='color:#cc0066'>{counts$artist[2]}</b>.  The  Tate  therefore  not<br>only  acquired  more  pieces  from  Turner  than  any  other  individual  artist,<br>they  acquired  more  pieces  from  Turner  than  all  other  artists  put  together!"),
    size = 2.5,
    label.colour = NA, fill = NA
  )

geom_rich
