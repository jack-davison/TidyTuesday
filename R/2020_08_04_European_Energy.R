
library(tidyverse)
library(tidytuesdayR)
library(geofacet)
library(grid)
library(png)

## Load in data
tuesdata <- tidytuesdayR::tt_load('2020-08-04')

## Examine the countries and compare with the geofacet grid
tuesdata$energy_types$country_name %>% unique()
europe_countries_grid2$name %>% unique()

## Match countries in data w/ grid, do some pivoting
energy_raw <- tuesdata$energy_types %>%
  mutate(country_name = case_when(country_name == "North Macedonia" ~ "N. Macedonia",
                                  country_name == "Bosnia & Herzegovina" ~ "Bosnia & H.",
                                  country == "UK" ~ "UK",
                                  country == "EL" ~ "Greece",
                                  TRUE ~ country_name)) %>%
  filter(country_name %in% europe_countries_grid2$name) %>%
  pivot_longer(cols = c(`2016`,`2017`,`2018`), names_to = "year") %>%
  pivot_wider(names_from = type, values_from = value) %>%
  select(-country)

## Work out percentages of the total and tidy the data
energy_plot <- left_join(energy_raw %>% filter(level == "Level 1") %>% select(-level) %>% janitor::remove_empty("cols"),
                    energy_raw %>% filter(level == "Level 2") %>% select(-level) %>% janitor::remove_empty("cols")) %>% 
  janitor::clean_names() %>%
  rowwise() %>%
  mutate(tot = sum(conventional_thermal, nuclear, hydro, wind, solar, geothermal, other, pumped_hydro_power),
         across(where(is.numeric), ~ 100 * . / tot)) %>%
  select(-tot) %>%
  ungroup() %>%
  pivot_longer(names_to = "energy", values_to = "value", 
               c(conventional_thermal, nuclear, hydro, wind, solar, geothermal, other, pumped_hydro_power)) %>%
  rbind(expand.grid(country_name = setdiff(europe_countries_grid2$name, energy_raw$country_name),
                    year = 2016:2018, energy = "No Data Available", value = 100)) %>%
  mutate(energy = case_when(energy == "conventional_thermal" ~ "Conventional Thermal",
                            energy == "nuclear" ~ "Nuclear",
                            energy == "No Data Available" ~ energy,
                            TRUE ~ "Renewable"),
         energy = factor(energy, 
                         levels = c("Conventional Thermal", "Nuclear", 
                                    "Renewable", "No Data Available"))) %>%
  group_by(energy, year, country_name) %>%
  summarise(value = sum(value)) %>% mutate(country_name = str_trunc(country_name, 11))

## Plot!

ggplot(energy_plot) +
  
  # The actual proper plotting stuff here isn't actually that complicated!
  aes(x = year, y = value, fill = energy) +
  geom_col(color = "#30332E") +
  coord_flip() +
  scale_y_reverse() +
  
  # facet_geo from the "geofacet" package - I truncate the names here just to be sure
  facet_geo( ~ country_name,
             grid = europe_countries_grid2 %>% mutate(name = str_trunc(name, 11))) +
  
  # colours and themes
  scale_fill_manual(values = c("#EF476F", "#FFD166", "#06D6A0", "#4B5446")) +
  theme_minimal() +
  theme(
    plot.background = element_rect(fill = "#30332E"),
    text = element_text(color = "white"),
    strip.text = element_text(color = "white"),
    panel.grid = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_text(color = "white"),
    plot.margin = unit(c(1, 1, 1.5, 1.2), "cm"),
    legend.position = c(0.075, .125),
    legend.title = element_text(size = 15),
    plot.title = element_text(size = 40, face = "bold"),
    plot.subtitle = ggtext::element_markdown(size = 13),
    plot.caption = ggtext::element_markdown(color = "#5E7054", size = 10)
  ) +
  labs(
    x = "",
    y = "",
    fill = "Type of Energy",
    title = "EUROPEAN ENERGY\nGENERATION",
    subtitle = "Each bar represents the <b>total energy generation</b> for each country per year.<br>The colours represent the proportion of energy generated <b>a)</b> using <b style='color:#EF476F'>conventional<br> thermal power plants</b>, which is to say those that use coal, oil or natural gas,<br><b>b)</b> using <b style='color:#FFD166'>nuclear power stations</b>, and <b>c)</b> using other <b style='color:#06D6A0'>renewable sources</b>.<br><br>",
    caption = "Data from <b>'Electricity generation statistics - First Results'</b> (ec.europa.eu/eurostat/statistics-explained) <br> Visualisation by <b>Jack Davison</b> (Twitter @JDavison_)<br>Code found at <b>github.com/jack-davison</b>"
  )

  
