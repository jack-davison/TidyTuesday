library(tidytuesdayR)
library(tidyverse)
library(tidytext)
library(showtext)

tuesdata = tidytuesdayR::tt_load("2020-09-08")

friends = tuesdata$friends

showtext_auto()
font_add(family = "friends", regular = "~/GABRWFFR.TTF")

main_friends <- friends %>%
  filter(
    speaker %in% c(
      "Rachel Green",
      "Ross Geller",
      "Chandler Bing",
      "Monica Geller",
      "Joey Tribbiani",
      "Phoebe Buffay"
    )
  )

unique_sentiments <- main_friends %>%
  unnest_tokens(word, text) %>%
  anti_join(get_stopwords()) %>%
  group_by(speaker, word) %>%
  count() %>%
  inner_join(get_sentiments()) %>%
  group_by(word) %>%
  filter(n_distinct(speaker) != 6,
         n > 3) %>%
  group_by(speaker) %>%
  mutate(n = scales::rescale(n)) %>%
  arrange(-n)

ggplot(unique_sentiments,
       aes(
         label = word,
         size = n,
         color = sentiment,
         alpha = n
       )) +
  ggwordcloud::geom_text_wordcloud_area(area_corr_power = 1) +
  facet_wrap(~ speaker) +
  scale_radius(range = c(3, 20)) +
  theme_void() +
  scale_color_manual(values = c("#f14c38ff", "#01b0f1ff")) +
  scale_alpha(range = c(.5, 1)) +
  theme(
    plot.background = element_rect(fill = "#393536ff", color = NA),
    strip.text = element_text(
      family = "friends",
      size = 20,
      color = "white"
    ),
    plot.margin = unit(rep(1, 4), "cm"),
    panel.spacing = unit(.5, "cm"),
    plot.title = element_text(
      family = "friends",
      size = 40,
      color = "#f4c93cff",
      hjust = .5,
      vjust = .5
    ),
    plot.subtitle = ggtext::element_markdown(
      hjust = .5,
      color = "white",
      size = 15
    ),
    plot.caption = ggtext::element_markdown(
      hjust = .5,
      vjust = .5,
      color = "#f4c93cff"
    )
  ) +
  labs(title = "the one with the sentiment analysis",
       subtitle = "(<span style='color:#01b0f1ff'>Positive</span> and <span style='color:#f14c38ff'>Negative</span>)<br><br>",
       caption = "<br>Data from <b>{friends}</b> (github.com/EmilHvitfeldt/friends)<br> Visualisation by <b>Jack Davison</b> (Twitter @JDavison_)<br>Code found at <b>github.com/jack-davison</b>")
