library(tidytuesdayR)
library(tidyverse)
library(ggtext)

tuesdata <- tidytuesdayR::tt_load('2020-07-21')

brisbane <- tuesdata$brisbane_complaints %>% select(-nature, -city)

brisbane %>%
  
  # Pull out the years/months from the file extensions
  mutate(year = stringi::stri_extract(date_range, regex = "\\d{4}"),
         date_range = str_remove(date_range, "cars-srsa-open-data-animal-related-complaints-"),
         month = str_extract(date_range, c("1st|jan|january|april|apr|july|jul|oct|october")),
         month = case_when(month %in% c("1st","jan","january") ~ 1,
                           month %in% c("april","apr") ~ 4,
                           month %in% c("july", "jul") ~ 7,
                           month %in% c("oct","october") ~ 10),
         monthyear = paste0(month,"/",year),
         monthyear = zoo::as.yearmon(monthyear, "%m/%Y")) %>%
  
  # We're looking at animal attacks only
  filter(animal_type == "Attack", category %in% c("Attack On A Person","Attack On An Animal")) %>% 
  
  # Work out relative percentages
  count(category, monthyear) %>%
  group_by(monthyear) %>%
  mutate(tot = sum(n),
         perc = 100*n/tot, 
         totperc = sum(perc),
         perc = round(perc)) %>%
  
  # Begin plotting!
  ggplot() +
  aes(
    x = monthyear,
    y = perc,
    colour = category,
    shape = category
  ) +
  
  # geoms (geom_textbox from ggtext)
  geom_line(size = 1) + 
  geom_point(size = 5) +
  geom_textbox(
    inherit.aes = F,
    data = tibble(x = 2017,
                  y = 50.35,
                  label = "<b style='font-size:20pt;'>Animal Attacks in Brisbane</b><br><br> Every year, more <b style ='color:#E4002B'> animals are reported to attack other animals </b> than <b style ='color:#012169'> animals are reported to attack humans</b>."),
    aes(x = x, y = y, label = label),
    fill = NA,
    box.color = NA,
    size = 3,
    width = unit(3, "inch")
  ) +
  
  # scales
  scale_shape_manual(values = c("\U1F9CD", "\U1F415")) +
  scale_color_manual(values = c("#012169", "#E4002B")) +
  scale_y_continuous(
    breaks = c(30, 50, 70),
    limits = c(30, 70),
    labels = function(x)
      paste0(x, "%")
  ) +
  
  # themes and labels
  theme_minimal() +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = "none",
    axis.text.y = element_text(color = "grey50", face = "bold"),
    aspect.ratio = 0.7
  ) +
  labs(x = "", y = "")
