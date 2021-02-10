
library(tidytuesdayR)
library(tidyverse)
library(gggibbous)
library(ggtext)
library(extrafont)

ggthemr::ggthemr("flat",
                 text_size = 14,
                 layout = "scientific",
                 spacing = 1)

tuesdata = tt_load("2021-02-09")

tuesdata$student_debt %>%
  mutate(tot = 1,
         race = factor(race, levels = c("Black", "White", "Hispanic"))) %>%
  
  ggplot(aes(x = year, y = loan_debt)) +
  geom_line(aes(color = race), size = 3) +
  geom_moon(aes(ratio = tot),
            size = 11,
            fill = "grey25",
            color = "grey25") +
  geom_moon(aes(fill = race, ratio = loan_debt_pct),
            right = F,
            color = NA) +
  geom_moon(
    aes(fill = race, ratio = 1 - loan_debt_pct),
    right = T,
    color = NA,
    fill = "white"
  ) +
  
  ggforce::facet_col(race ~ .) +
  gghighlight::gghighlight(
    use_direct_label = F,
    unhighlighted_params = list(fill = "white", color = "white")
  ) +
  ggthemr::scale_colour_ggthemr_d() +
  theme(
    legend.position = "none",
    text = element_text(family = "Bahnschrift"),
    plot.title = element_textbox_simple(lineheight = 1, padding = margin(0, 0, 5, 0)),
    plot.title.position = "plot",
    plot.caption = element_text(
      size = 8,
      color = "grey66",
      hjust = .5
    ),
    plot.caption.position = "plot"
  ) +
  scale_y_continuous(
    sec.axis = sec_axis( ~ ., labels = scales::dollar),
    labels = scales::dollar,
    breaks = seq(0, 15000, 5000),
    limits = c(0, 15000)
  ) +
  labs(
    x = NULL,
    y = NULL,
    title = "<b style = 'font-size:25pt;'>Since the mid-2000s, <span style = 'color:#3498db;'>black families</span>, on average, have carried more student loan debt than <span style = 'color:#2ecc71;'>white families</span>.</b><br>
<span style = 'font-size:10pt;color:grey66'>This is driven in large part by the growing share of black families that take on student debt. In 2016, 42% of families headed by black adults ages 25 to 55 had student loan debt, compared with 34% of similar white families. This is visualised as the extent to which each point is filled with colour. It is worth remembering that black students also have lower graduation rates than white students - student loan debt doesn’t always translate into a degree that promotes economic mobility, income and wealth in the long run.
</span>",
caption = "Data and Text from apps.urban.org/features/wealth-inequality-charts/\nVisualisation by Jack Davison (Twitter @JDavison_ | Github jack-davison)"
  )
