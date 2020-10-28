

library(tidyverse)
library(tidytuesdayR)
library(rnaturalearth)
library(gt)

tt_load("2020-10-27")

tuesdata <-
  readr::read_csv(
    'https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-10-27/wind-turbine.csv'
  )


# Canada Avg --------------------------------------------------------------

canada_avg = tuesdata %>%
  mutate(commissioning_date = as.numeric(substr(
    commissioning_date, start = 1, stop = 4
  ))) %>%
  summarise(
    Turbines = n(),
    Projects = n_distinct(project_name),
    `Total` = sum(total_project_capacity_mw),
    `Mean` = mean(total_project_capacity_mw),
    `Diameter` = mean(rotor_diameter_m),
    `Height` = mean(hub_height_m),
    `Earliest Turbine` = min(commissioning_date)
  ) %>%
  cbind(
    tuesdata %>% count(`Top Manufacturer` = manufacturer) %>% filter(n == max(n)) %>% select(-n)
  ) %>%
  mutate(`Province/Territory` = "Canada")

# Manufacturers -----------------------------------------------------------

manu = tuesdata %>%
  count(province_territory, manufacturer) %>%
  group_by(province_territory) %>%
  filter(n == max(n)) %>%
  mutate(
    `Top Manufacturer` = if_else(
      province_territory == "Yukon",
      true = "Vestas/Bonus",
      false = manufacturer
    )
  ) %>%
  select(-n,-manufacturer) %>%
  unique()


# Table -------------------------------------------------------------------

tuesdata %>%
  mutate(commissioning_date = as.numeric(substr(
    commissioning_date, start = 1, stop = 4
  ))) %>%
  group_by(province_territory) %>%
  summarise(
    Turbines = n(),
    Projects = n_distinct(project_name),
    `Total` = sum(total_project_capacity_mw),
    `Mean` = mean(total_project_capacity_mw),
    `Diameter` = mean(rotor_diameter_m),
    `Height` = mean(hub_height_m),
    `Earliest Turbine` = min(commissioning_date)
  ) %>%
  left_join(manu) %>%
  relocate(`Top Manufacturer`, .after = province_territory) %>%
  rename(`Province/Territory` = province_territory) %>%
  arrange(-Turbines) %>%
  rbind(canada_avg) %>%
  
  gt() %>%
  tab_style(style = list(cell_text(weight = "bold")),
            locations = cells_column_labels(everything())) %>%
  tab_options(
    table_body.hlines.color = "white",
    table.border.bottom.color = "white",
    table.border.top.color = "white",
    table.border.bottom.width = px(3)
  ) %>%
  cols_label(
    `Province/Territory` = md('<div style="text-align: left">Province/Territory</div>'),
    `Top Manufacturer` = md('<div style="text-align: left">Top<br>Manufacturer</div>'),
    `Earliest Turbine` = md('<div style="text-align: right">First<br>Turbine</div>'),
    `Total` = md('<div style="text-align: right">Total</div>'),
    `Mean` = md('<div style="text-align: right">Mean</div>'),
    `Diameter` = md('<div style="text-align: right">Diameter</div>'),
    `Height` = md('<div style="text-align: right">Height</div>'),
    `Turbines` = md('<div style="text-align: right">Turbines</div>'),
    `Projects` = md('<div style="text-align: right">Projects</div>')
  ) %>%
  tab_spanner(columns = c("Turbines", "Projects"),
              label = md("**Number**")) %>%
  tab_spanner(columns = c("Total", "Mean"),
              label = md("**Project Capacity (kW)**")) %>%
  tab_spanner(
    columns = c("Diameter", "Height"),
    label = md("**Mean Turbine Spec (m)**")
  ) %>%
  fmt_number(columns = c("Total", "Mean"), decimals = 0) %>%
  fmt_number(columns = c("Diameter", "Height"),
             decimals = 1) %>%
  tab_style(
    style = list(
      cell_text(weight = "bold"),
      cell_borders(
        sides = "top",
        color = "lightgrey",
        weight = px(2)
      )
    ),
    locations = cells_body(columns = everything(),
                           rows = `Province/Territory` == "Canada")
  ) %>%
  tab_source_note(
    md(
      "**Table**: @JDavison_ | **Data:** open.canada.ca | **Inspiration:** @thomas_mock"
    )
  ) %>%
  cols_align(align = "right", columns = c("Turbines", "Projects")) %>%
  cols_align(align = "right", columns = c("Total", "Mean")) %>%
  cols_align(align = "right", columns = c("Diameter", "Height")) %>%
  tab_options(data_row.padding = px(10))
