
library(tidytuesdayR)
library(tidyverse)
library(gganimate)
library(magick)


# Load Data ---------------------------------------------------------------

data = tt_load("2020-10-13")$datasaurus

stats = data %>%
  group_by(dataset) %>%
  summarise(mean_x = mean(x, na.rm = T),
            mean_y = mean(y, na.rm = T),
            sd_x   =   sd(x, na.rm = T),
            sd_y   =   sd(y, na.rm = T),
            coeff  = cor(y, x))


# Make Plots --------------------------------------------------------------

scatter = data %>%
  left_join(stats) %>%
  ggplot(aes(x = x, y = y)) + 
  geom_point() +
  theme_minimal() +
  theme(aspect.ratio = 1,
        text = element_text(family = "mono", size = 20)) +
  transition_states(dataset, wrap = T,
                    transition_length = 1,
                    state_length = 2)

statistics = data %>%
  left_join(stats) %>%
  mutate(dataset = str_replace_all(dataset, "_", " "),
         dataset = str_to_title(dataset)) %>%
  ggplot() + 
  geom_text(x = 50, y = 50, hjust = 1, vjust = 0.5, size = 10, family = "mono",
             aes(label = paste0(dataset,
                                "\nMean x = ",
                                "\nMean y = ",
                                "\nSD x = ",
                                "\nSD y = ",
                                "\nCorr. = "
                                )), check_overlap = T, color = "black") +
  geom_text(x = 50, y = 50, hjust = 0, vjust = 0.5, size = 10, family = "mono",
            aes(label = paste0(" Data Set",
                               "\n", mean_x,
                               "\n", mean_y,
                               "\n", sd_x,
                               "\n", sd_y,
                               "\n", coeff
            )), check_overlap = T, color = "grey50") +
  geom_text(x = 50, y = 50, hjust = 0, vjust = 0.5, size = 10, family = "mono",
            aes(label = paste0(" Data Set",
                               "\n", trunc(mean_x * 10)/10,
                               "\n", trunc(mean_y * 10)/10,
                               "\n", trunc(sd_x * 10)/10,
                               "\n", trunc(sd_y * 10)/10,
                               "\n", trunc(coeff * 100)/100
            )), check_overlap = T, color = "black") +
  theme_void() + 
  theme(aspect.ratio = 1) +
  transition_states(dataset, wrap = T,
                    transition_length = 1,
                    state_length = 2) +
  xlim(0,100) + ylim(0,100)


# Image Nonsense ----------------------------------------------------------

a_gif = animate(scatter)
b_gif = animate(statistics)

a_mgif <- image_read(a_gif)
b_mgif <- image_read(b_gif)

new_gif <- image_append(c(a_mgif[1], b_mgif[1]))
for(i in 2:100){
  combined <- image_append(c(a_mgif[i], b_mgif[i]))
  new_gif <- c(new_gif, combined)
}

magick::image_write(image = new_gif, path = "dino.gif")

