
library(tidyverse)
library(tidytuesdayR)

# Read in Data ------------------------------------------------------------

df = tt_load("2020-12-22") %>% .$`big-mac`
codes = read_csv("~/GitHub/TidyTuesday/extra_data/country_codes.txt") %>%
  janitor::clean_names() %>%
  rename(iso_a3 = three_letter_country_code)

# Filter ------------------------------------------------------------------

data = df %>% filter(date == max(date))

us_burger = data %>% filter(name == "United States") %>% pull(dollar_price)

codes

data %>%
  # filter(name != "United States") %>%
  mutate(burger_ex = local_price / us_burger) %>%
  select(iso_a3, name, burger_ex, dollar_ex) %>%
  mutate(burger_ex = burger_ex / dollar_ex, 
         burger_ex = burger_ex - 1,
         name = fct_reorder(name, burger_ex),
         .keep = "unused") %>%
  left_join(codes) %>%
  ggplot(aes(y = name)) +
  geom_point(aes(x = burger_ex)) +
  geom_vline(xintercept = 0) +
  geom_segment(aes(x = 0, xend = burger_ex, y = name, yend = name)) +
  scale_x_continuous(labels = scales::label_percent()) +
  facet_grid(continent_name~., scales = "free_y", space = "free")

df %>%
  filter(name %in% c("United States", "Britain")) %>%
  select(date, name, local_price, dollar_ex) %>%
  pivot_wider(names_from = name, values_from = local_price) %>%
  janitor::clean_names() %>%
  fill(united_states, .direction = "up") %>%
  filter(!is.na(britain)) %>%
  mutate(burger_ex = britain/united_states, .keep = "unused") %>%
  ggplot(aes(x = date)) +
  geom_ribbon(aes(ymax = burger_ex, ymin = dollar_ex),
              fill = "grey90", alpha = .5) +
  geom_line(aes(y = dollar_ex, color = "Economics"), size = 1) +
  geom_line(aes(y = burger_ex, color = "Burgernomics"), size = 1) +
  scale_color_manual(values = c("#DA291C", "#FFC72C")) +
  theme_minimal() +
  theme(panel.grid = element_blank(),
        panel.grid.major.y = element_line(color = "grey90")) +
  labs()



data %>%
  filter(name != "United States") %>%
  mutate(burger_exchange = local_price / us_burger) %>%
  ggplot(aes(y = reorder(name, dollar_ex))) +
  geom_col(width = .3, aes(x = burger_exchange, fill = "#FFC72C")) +
  geom_point(aes(x = dollar_ex, color = "#DA291C")) +
  scale_x_log10() +
  scale_fill_identity() +
  theme_minimal()

n_distinct(df$name)
