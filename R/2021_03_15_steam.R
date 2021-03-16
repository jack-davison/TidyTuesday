
library(tidyverse)
library(tidytuesdayR)
library(rvest)
library(ggthemr)

ggthemr(palette = "chalk", text_size = 14, layout = "minimal", type = "outer")

# Read in data ------------------------------------------------------------

tuesdata = tt_load("2021-03-16")

games = tuesdata$games %>%
  mutate(avg_peak_perc = parse_number(avg_peak_perc)/100,
         yearmon = lubridate::my(glue::glue("{month}/{year}")))

# web scraping ------------------------------------------------------------

recent_updates = html_nodes(read_html("https://dota2.gamepedia.com/Game_Versions"),
                            "table")[1] %>%
  html_table() %>%
  .[[1]] %>%
  janitor::clean_names()

older_updates = html_nodes(read_html("https://dota2.gamepedia.com/Game_Versions/6.70_to_6.88f"),
           "table") %>%
  html_table() %>%
  .[[1]] %>%
  janitor::clean_names() %>%
  .[-1,] %>%
  select(-patch_date_2)
  
updates = bind_rows(recent_updates, older_updates) %>%
  mutate(patch_date = lubridate::ymd(patch_date)) %>%
  mutate(version = as.numeric(version)) %>%
  drop_na(patch_date, version) %>% 
  mutate(yearmon = lubridate::floor_date(patch_date, unit = "month"))

# combine data ------------------------------------------------------------

unique_updates = updates %>%
  select(yearmon) %>%
  distinct() %>%
  mutate(flag = "!") %>%
  left_join()

unique_updates = updates %>%
  select(version, yearmon) %>%
  group_by(yearmon) %>%
  arrange(version) %>%
  filter(version == version[1])

dota = games %>%
  filter(gamename == "Dota 2") %>%
  left_join(unique_updates, by = "yearmon") %>%
  arrange(yearmon) %>%
  mutate(popular = if_else(!is.na(version), lead(gain) > 0, NA),
         bulk_ver = round(version, 1)) %>%
  fill(bulk_ver, .direction = "updown")

dota %>%
  ggplot(aes(x = yearmon, y = avg)) +
  geom_point(aes(color = factor(bulk_ver))) +
  scale_color_viridis_d()

# visualisation -----------------------------------------------------------

dota %>%
  mutate(bulk_ver = if_else(bulk_ver == 7,
                            glue::glue("v{bulk_ver}.0"),
                            glue::glue("v{bulk_ver}"))) %>%
  ggplot(aes(x = yearmon, y = avg)) +
  geom_line() +
  geom_point(aes(shape = popular), fill = "#3c3c3c", color = "#3c3c3c", size = 5) +
  geom_point(aes(color = popular, fill = popular, shape = popular), size = 3) +
  scale_color_manual(values = c("#DB5461", "#2AF5FF"), na.value = "#D8D5DB") +
  scale_fill_manual(values = c("#DB5461", "#2AF5FF"), na.value = "#D8D5DB") +
  scale_shape_manual(values = c(25, 24), na.value = 21) +
  scale_y_continuous(labels = scales::comma, expand = expansion(mult = c(0,.1)), sec.axis = sec_axis(~., labels = scales::comma)) +
  expand_limits(y = 0) +
  labs(x = NULL, y = NULL, 
       title = "Monthly Average Concurrent Dota 2 players",
       subtitle = "Months with updates are marked using triangles.\nA red downwards triangle indicates an unpopular update, where the player numbers decreased.\nA blue upwards triangle indicates a popular update, where the player numbers increased.") +
  theme(panel.grid.major.y = element_line(linetype = 2, color = "grey40"),
        axis.line.y = element_blank(),
        legend.position = "none")
