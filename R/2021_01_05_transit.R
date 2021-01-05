
library(tidyverse)
library(tidytuesdayR)
library(countrycode)
library(rnaturalearth)
library(leaflet)
library(htmltools)
library(ggpubr)
library(extrafont)

# Read in data ------------------------------------------------------------

world = ne_countries(scale = "medium", returnclass = "sf") %>%
  filter(!continent %in% c("Antarctica", "Seven seas (open ocean)"))

continents = countrycode::codelist %>%
  select(ecb, country.name.en, continent) %>%
  rename(country = country.name.en)

city_locs = readr::read_csv("GitHub/TidyTuesday/extra_data/worldcities.csv") %>%
  select(city, country, lat, lng) %>%
  mutate(country = if_else(country == "Korea, South", "South Korea", country))

data = tt_load("2021-01-05")$transit_cost %>%
  filter(!is.na(city)) %>%
  rename(ecb = country) %>%
  mutate(ecb = if_else(ecb == "UK", "GB", ecb)) %>%
  left_join(continents, by = "ecb") %>%
  left_join(city_locs, by = c("city", "country")) %>%
  filter(!is.na(lat)) %>%
  mutate(start_year = as.numeric(start_year)) %>%
  filter(start_year > 2000)

rm(continents, city_locs)

# plots --------------------------------------------------------

all = data %>%
  filter(!is.na(continent), rr == 0) %>%
  group_by(city, continent, lat, lng) %>%
  summarise(avg = median(cost_km_millions, na.rm = T)) %>%
  ggplot() +
  geom_sf(data = world, color = "white", fill = "grey20") +
  geom_point(aes(
    group = city,
    x = lng,
    y = lat,
    size = avg,
    color = continent
  ),
  alpha = .5) +
  theme_void() +
  scale_color_manual(values = c("Oceania" = "#4D9DE0", 
                                "Americas" = "#E15554", 
                                "Africa" = "#E1BC29", 
                                "Asia" = "#3BB273", 
                                "Europe" = "#7768AE")) +
  scale_size(range = c(1,10), limits = c(60,1650)) +
  theme(legend.position = "top", 
        text = element_text(family = "Segoe UI"),
        plot.title = element_text(family = "Segoe UI Black", hjust = .5, size = 50),
        plot.subtitle = element_text(family = "Segoe UI Light", hjust = .5, size = 20), 
        legend.text = element_text(hjust = 0),
        legend.spacing.x = unit(0.5, 'cm')) +
  guides(color = guide_none()) +
  labs(size = NULL,
       title = '"From A to B in USD"',
       subtitle = "The price of road transit in millions of USD per kilometer around the globe.")

plot_cont = function(world_cont, data_cont){
  
  df = data %>%
    filter(!is.na(continent), rr == 0) %>%
    filter(continent == data_cont) %>%
    group_by(city, continent, lat, lng) %>%
    summarise(avg = median(cost_km_millions, na.rm = T)) %>%
    ungroup()
  
  max_df = filter(df, avg == max(avg))
  
  min_df = filter(df, avg == min(avg))
  
  ggplot(df) +
    geom_sf(data = world %>% filter(continent %in% world_cont), color = "white", fill = "grey20") +
    geom_point(aes(
      group = city,
      x = lng,
      y = lat,
      size = avg,
      color = if_else(city %in% c(max_df$city, min_df$city), continent, "white"),
    ),
    alpha = .75) +
    theme_bw() +
    scale_color_manual(values = c("Oceania" = "#4D9DE0", 
                                  "Americas" = "#E15554", 
                                  "Africa" = "#E1BC29", 
                                  "Asia" = "#3BB273", 
                                  "Europe" = "#7768AE",
                                  "white" = "grey75")) +
    scale_size(range = c(1,10), limits = c(60,1650)) +
    theme(
      legend.position = "none",
     axis.title = element_blank(),
     axis.ticks = element_blank(),
      axis.text = element_blank(),
     plot.title = element_text(family = "mono")
    ) +
    guides(color = guide_none()) +
    labs(size = "Million $ / km",
         title = paste0("Max: ", max_df$city, " ($", round(max_df$avg), "/km)\n",
                        "Min: ", min_df$city, " ($", round(min_df$avg), "/km)"))
  
}

na = plot_cont(world_cont = c("North America"), data_cont = "Americas") +
  coord_sf(xlim = c(-140, -50), ylim = c(10,60))

eu = plot_cont(world_cont = c("Europe"), data_cont = "Europe") +
  coord_sf(xlim = c(-10, 45), ylim = c(38, 60))

as = plot_cont(world_cont = c("Asia"), data_cont = "Asia") +
  coord_sf(xlim = c(30, 140), ylim = c(-10,50))

conts = ggarrange(na, eu, as, nrow = 1)

ggarrange(all, conts, ncol = 1, heights = c(2,1), align = "hv")

# leaflet -----------------------------------------------------------------

avgs = data %>% 
  select(city, country, continent, lat, lng, cost_km_millions) %>%
  group_by(city, country, continent, lat, lng) %>%
  summarise(avg = median(cost_km_millions)) %>%
  ungroup() %>%
  as.data.frame()

leaflet(avgs) %>%
  addTiles() %>%
  addCircles(data = avgs, lng = ~lng, lat = ~lat,
             radius = ~round(avg,0) * 200) %>%
  addMarkers(lng = ~lng, lat = ~lat, 
             clusterOptions = markerClusterOptions(),
             popup = ~paste0("<b>", city, "</b>, ",country,"<br>",
                             "$",round(avg,0)," million / km")
  )
