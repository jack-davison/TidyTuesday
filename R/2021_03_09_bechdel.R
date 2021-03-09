
library(tidytuesdayR)
library(tidyverse)
library(tidytext)
library(rsample)
library(extrafont)

set.seed(123)

ggthemr::ggthemr(palette = "earth", type = "outer")

tuesdata = tt_load("2021-03-09")

# Read/Manipulate in Data ------------------------------------------------------------

movies = tuesdata$movies

unnested_genres = movies %>%
  unnest_tokens(input = genre, output = "genre", "words") %>%
  mutate(genre = if_else(genre == "sci", "sci-fi", genre)) %>%
  filter(genre != "fi")

genre_counts = unnested_genres %>%
  count(year, genre, binary) %>%
  group_by(year, genre) %>%
  mutate(tot_n = sum(n),
         perc = n / tot_n) %>%
  filter(binary == "PASS", n > 5) %>%
  select(year, genre, perc)

genres_wide = unnested_genres %>%
  filter(genre != "documentary") %>%
  mutate(check = TRUE) %>%
  pivot_wider(names_from = genre, values_from = check, values_fill = FALSE)

genre_binary = genres_wide %>%
  select(binary, where(is.logical)) %>%
  mutate(binary = if_else(binary == "FAIL", F, T)) %>%
  janitor::remove_constant()


# Rsample -----------------------------------------------------------------

lm(binary ~ ., genre_binary) %>% 
  broom::tidy() %>%
  arrange(estimate) %>%
  mutate(sig = if_else(p.value <= .05, "***", ""))

binary_intervals = reg_intervals(binary ~ .,
                                 data = genre_binary, 
                                 type = "percentile", 
                                 keep_reps = T)


# Plot --------------------------------------------------------------------

binary_intervals %>%
  mutate(
    term = str_remove_all(toupper(term), "TRUE|`"),
    term = fct_reorder(term, .estimate)
  ) %>%
  unnest(.replicates) %>%
  ggplot(aes(estimate)) +
  geom_vline(xintercept = 0, lty = 1, color = ggthemr::swatch()[1]) +
  geom_text(aes(label = term, x = 0, y = 100), check_overlap = T, family = "Tw Cen MT Condensed Extra Bold", size = 8) +
  geom_histogram(aes(fill = .estimate), alpha = 0.8, show.legend = FALSE, bins = 40) +
  facet_wrap(vars(term), scales = "free_x") +
  scale_x_continuous(breaks = seq(-.5,.5,.25), limits = c(-.5,.5), labels = scales::comma) +
  scale_y_continuous(expand = expansion(mult = c(0,.1))) +
  theme(text = element_text(family = "Tw Cen MT"),
        plot.title = element_text(size = 35, face = "bold", family = "Tw Cen MT Condensed Extra Bold"),
        plot.margin = unit(rep(2,4),"cm"),
        plot.caption = element_text(size = 10.5, hjust = .5, family = "Tw Cen MT Condensed"),
        strip.text = element_blank(),
        panel.spacing = unit(1, "cm"),
        axis.line.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        panel.grid.major.y = element_blank()) +
  scale_fill_gradient(low = ggthemr::swatch()[2], high = ggthemr::swatch()[3]) +
  labs(x = NULL, y = NULL,
       title = "Film Genres & the Bechdel Test",
       subtitle = glue::glue("Using a dataset of {nrow(movies)} films from {min(movies$year)} to {max(movies$year)}, statistical modelling was used to determine which film genres are more likely to pass the Bechdel\ntest (which asks whether a work features at least two women who talk to each other about something other than a man). Visualised below are the\nbootstrap confidence intervals of the model estimates for the 20 genres in the data set.\n\nWar and western films are (perhaps unsurprisingly) less likely to pass the Bechdel test. Curiously, the animated films were unlikely to pass - around\n70% of the animated films in the data set failed. On the other end of the spectrum, music, family and - interestingly - horror films are more likely to\npass the Bechdel test. Perhaps in horror films there are more pressing things for the characters to talk about!\n\n"),
       caption = "\nVisualisation by Jack Davison (@JDavison_) | Data from FiveThirtyEight | Analysis inspired by Julia Silge (juliasilge.com/blog/superbowl-conf-int)")
