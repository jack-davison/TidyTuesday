
library(tidyverse)
library(tidymodels)
library(tidytuesdayR)
library(ggtext)
library(extrafont)

extrafont::font_import()

tt = tt_load("2021-07-13")

sd = tt$scoobydoo |> 
  mutate(across(where(is.character), ~if_else(.x == "NULL", NA_character_, .x)))

labels = c(
  "daphnie" = glue::glue("<img src='{here::here('extra_data', 'scooby', 'daphne.png')}'width='56'/>"),
  "shaggy" = glue::glue("<img src='{here::here('extra_data', 'scooby', 'shaggy.png')}'width='62'/>"),
  "velma" = glue::glue("<img src='{here::here('extra_data', 'scooby', 'velma.png')}'width='44'/>"),
  "fred" = glue::glue("<img src='{here::here('extra_data', 'scooby', 'fred.png')}'width='66'/>"),
  "scooby" = glue::glue("<img src='{here::here('extra_data', 'scooby', 'scoob.png')}'width='90'/>")
  )

colors = tribble(
  ~name, ~color,
  "daphnie", "#7C6AA8",
  "shaggy", "#B2BE34",
  "scooby", "#B1752C",
  "velma", "#FA9C39", 
  "fred", "#01A0DA"
)

uc = sd |> 
  select(index, contains("unmask"), contains("captured"), contains("snack")) |> 
  mutate(across(!contains("index"), as.logical)) |> 
  pivot_longer(where(is.logical)) |> 
  mutate(activity = str_extract(name, "unmask|captured|snack"),
         name = str_remove(name, "unmask_|captured_|snack_")) |> 
  count(name, value, activity) |> 
  filter(value, name != "other") |> 
  left_join(colors, by = "name") |> 
  mutate(name = fct_reorder(name, n, max))

uc |>
  pivot_wider(names_from = activity, values_from = n) |> 
  ggplot(aes(x = name)) +
  geom_segment(aes(yend = unmask, y = captured, xend = name), color = "grey80", size = 3) +
  geom_segment(aes(yend = snack, y = captured, xend = name), color = "grey80", size = 3) +
  geom_point(size = 8, aes(y = captured, color = color, shape = "Times Captured by Monster")) +
  geom_text(aes(label = captured, y = captured), color = "white", family = "Berlin Sans FB Demi") +
  geom_point(size = 8, aes(y = unmask, color = color, shape = "Monsters Unmasked")) +
  geom_text(aes(label = unmask, y = unmask), color = "white", family = "Berlin Sans FB Demi") +
  geom_point(size = 8, aes(y = snack, color = color, shape = "Snacks Eaten")) +
  geom_text(aes(label = snack, y = snack), color = "white", family = "Berlin Sans FB Demi") +
  scale_x_discrete(name = NULL, labels = labels) +
  scale_color_identity() +
  theme_minimal() +
  theme(axis.text.x = element_markdown(color = "black", size = 11, vjust = 0),
        axis.title.x = element_text(), 
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        legend.position = "top",
        legend.direction = "horizontal",
        legend.box = "horizontal",
        legend.title = element_text(family = "Berlin Sans FB Demi", size = 25),
        plot.caption = element_text(family = "Berlin Sans FB", hjust = .5, color = "grey50"),
        plot.margin = unit(rep(.5,4), "cm")) +
  expand_limits(y = c(0, 110)) +
  scale_y_continuous(breaks = seq(0, 100, 25), sec.axis = sec_axis(~.*1,breaks = seq(0, 100, 25))) +
  labs(x = NULL, y = NULL, shape = "Mystery Incorporated\nPerformance Review", 
       caption = "\nVisualisation by Jack Davison (@JDavison_) | Data from ScoobyPedia") +
  guides(shape = guide_legend(title.position = "top", title.hjust = .5, label.hjust = .5, override.aes = aes(size =5)))

