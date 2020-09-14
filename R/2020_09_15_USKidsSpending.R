library(tidytuesdayR)
library(tidyverse)

tuesdata <- tidytuesdayR::tt_load("2020-09-15") %>% .$kids

plot_data = tuesdata %>%
  filter(year == max(year), variable == "PK12ed") %>%
  group_by(state) %>%
  summarise(val = sum(inf_adj),
            ch = sum(inf_adj_perchild)) %>%
  left_join(geofacet::us_state_grid1 %>% rename(state = name))

avg = plot_data %>%
  summarise(val = mean(val),
            ch = mean(ch))

ggplot(plot_data) +
  aes(val, ch, color = ch * val) +
  geom_hline(data = avg,
             aes(yintercept = ch),
             color = "#f07167",
             size = 1) +
  geom_vline(data = avg,
             aes(xintercept = val),
             color = "#0081a7",
             size = 1) +
  geom_point(size = 2) +
  
  ggrepel::geom_text_repel(data = filter(
    plot_data,
    ch != max(ch) & ch != min(ch) & val != max(val) & val != min(val)
  ),
  aes(label = code)) +
  ggrepel::geom_label_repel(data = filter(
    plot_data,
    ch == max(ch) | ch == min(ch) | val == max(val) | val == min(val)
  ),
  aes(label = state)) +
  annotate(
    na.rm = T,
    geom = "text",
    x = 70000000,
    y = avg$ch + .3,
    color = "grey20",
    hjust = 1,
    label = paste0("Mean Spend per Child = $", round(avg$ch, 2))
  ) +
  annotate(
    na.rm = T,
    geom = "text",
    x = avg$val - 600000,
    y = 20,
    color = "grey20",
    hjust = 1,
    angle = 90,
    label = paste0("Mean Total Spend = $", round(avg$val, -4))
  ) +
  scale_x_log10(labels = scales::label_dollar()) +
  scale_y_log10(limits = c(4, 20), labels = scales::label_dollar()) +
  scale_color_gradient(trans = scales::log10_trans(),
                       low = "#f07167",
                       high = "#0081a7") +
  theme_minimal() +
  theme(
    legend.position = "none",
    axis.line = element_line(),
    aspect.ratio = .6,
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "grey98", color = NA),
    plot.margin = unit(c(1, 1, 1, 1), "cm"),
    plot.title = element_text(face = "bold", size = 20),
    plot.caption = ggtext::element_markdown(
      hjust = 1,
      vjust = 1,
      color = "grey20"
    )
  ) +
  labs(
    y = "Public Spending on Children per Child\n",
    x = "\nTotal Public Spending on Children",
    title = "2016 US Spending on Kids",
    subtitle = "Each dot corresponds to one state. The state with the greatest and lowest total and per-child spending are labelled.\n",
    caption = "<br>Data from <b>{tidykids}</b> (jrosen48.github.io/tidykids)<br>Visualisation by <b>Jack Davison</b> (Twitter @JDavison_) | Code found at <b>github.com/jack-davison</b>"
  )
