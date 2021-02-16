
library(tidyverse)
library(tidytuesdayR)
library(cowplot)


# Read in Data ------------------------------------------------------------

tuesdata = tt_load("2021-02-16")

# Wrangle Data ------------------------------------------------------------

# We're effectively wanting the actual data to occupy 61% of the pie chart
# The other 39% is empty
# We also need to do some creative arranging to get the values for the label positions

dat = tuesdata$occupation %>%
  janitor::clean_names() %>%
  mutate(plot_perc = percentage / sum(percentage)) %>%
  mutate(plot_perc = (plot_perc * 61)/100) %>%
  bind_rows(tibble(group = c("Negroes", "Whites"),
                   occupation = c("empty", "empty"),
                   percentage = c(0,0),
                   plot_perc = c(0.195, 0.195))) %>%
  mutate(occupation = factor(occupation,
                             levels = c("Agriculture, Fisheries and Mining",
                                        "Domestic and Personal Service",
                                        "Manufacturing and Mechanical Industries",
                                        "Trade and Transportation",
                                        "Professions",
                                        "Empty"))) %>%
  arrange(desc(group), occupation) %>%
  mutate(ypos = cumsum(plot_perc)- 0.5*plot_perc) %>%
  arrange(occupation)

# This data is for the legend, which we'll plot separately

lab_dat = dat %>%
  select(occupation) %>%
  distinct() %>%
  drop_na() %>%
  mutate(x = c(-1.5, 1.5, -1.5, 1.5, 1.5),
         y = c(4, 5, 2, 1, 3),
         occupation = case_when(
           occupation == "Agriculture, Fisheries and Mining" ~ "AGRICULTURE, FISHERIES\nAND MINING.",
           occupation == "Domestic and Personal Service" ~ "DOMESTIC AND\nPERSONAL SERVICE.",
           occupation == "Manufacturing and Mechanical Industries" ~ "MANUFACTURING AND\nMECHANICAL INDUSTRIES.",
           occupation == "Trade and Transportation" ~ "TRADE AND\nTRANSPORTATION.",
           occupation == "Professions" ~ "PROFESSIONS."
         ))

# Visualisations ------------------------------------------------------------

# First the pie.
# We need to use group = group so the two racial groups are considered separately
# We use color = occupation == "empty" so that only the "true" wedges have borders, not the invisible ones
# na.translate gets rid of the empty wedges

pie = ggplot(dat, aes(y = "", x = plot_perc, fill = occupation, group = group)) +
  geom_col(aes(color = occupation == "empty")) +
  coord_polar(start = pi - (55*pi)/180) +
  scale_fill_manual(values = c("#b8243c", "#f2c50a", "#5a6796", "#d5c8b7", "#ab927a"), na.translate = F) +
  theme_void() +
  theme(legend.position = "none",
        plot.background = element_rect(color = NA, fill = "#ece5d5"),
        text = element_text(family = "mono"),
        plot.title = element_text(hjust = .5, face = "bold"), plot.margin = unit(rep(.5,4), "cm")) +
  geom_text(aes(x = ypos, 
                label = ifelse(occupation == "empty", NA, glue::glue("{round(percentage,1)}%"))),
            nudge_y = .35, family = "mono") +
  annotate(geom = "text", x = 0.65, y = 1.5, label = "NEGROES.", family = "mono") +
  annotate(geom = "text", x = 0.15, y = 1.5, label = "WHITES.", family = "mono") +
  scale_color_manual(values = c("#5f4e3b", NA)) +
  guides(color = guide_none()) +
  labs(title = "OCCUPATIONS OF NEGROES AND WHITES IN GEORGIA.")

# The legend is plotted separately - it's so different from a ggplot2 legend its just easier to cowplot them together
  
legend = ggplot(lab_dat, aes(x,y, fill = occupation)) +
  geom_point(shape = 21, size = 10) +
  geom_text(data = lab_dat %>% filter(x == -1.5),
            aes(label = occupation),
            nudge_x = .55, family = "mono", size = 3) +
  geom_text(data = lab_dat %>% filter(x == 1.5),
            aes(label = occupation),
            nudge_x = -.4, family = "mono", size = 3) +
  scale_fill_manual(values = c("#b8243c", "#f2c50a", "#5a6796", "#d5c8b7", "#ab927a"), na.translate = F) +
  theme_void() +
  theme(text = element_text(family = "mono"),
        legend.position = "none", aspect.ratio = .3) +
  coord_cartesian(clip = "off")

# Cowplot!
# If I didn't care about having a reproducible workflow, I'd just do this in e.g. inkscape

ggdraw(plot = pie) +
  draw_plot(legend, 
            scale = .7)
  
