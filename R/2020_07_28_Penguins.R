library(tidytuesdayR)
library(tidyverse)
library(ggtext)
library(rpart)
library(rpart.plot)
library(cowplot)
library(ggendro)
library(grid)

tuesdata <- tidytuesdayR::tt_load('2020-07-28')

# Read in my penguin image
img <- readPNG(source = "penguin.png")
img <- grid::rasterGrob(img, interpolate = TRUE)

# Fit a decision tree
rpart(data = penguins,
      formula = species ~ bill_depth_mm + bill_length_mm + flipper_length_mm + body_mass_g) %>%
  printcp()

# Extract data from tree to plot
fitr <- rpart(data = penguins,
      formula = species ~ bill_depth_mm + bill_length_mm + flipper_length_mm + body_mass_g) %>%
  ggdendro::dendro_data()

# Faff about with the coordinates to draw it as I want
fitr$segments$y[7] <- 0.08900524
fitr$segments$y[fitr$segments$y == 1] <- 0.625
fitr$segments$yend[fitr$segments$yend == 1] <- 0.625
fitr$segments$yend[9] <- 0.08900524
fitr$segments$x[fitr$segments$x == 3.5] <- 2.5
fitr$segments$xend[fitr$segments$xend == 3.5] <- 2.5
fitr$segments$x[fitr$segments$x == 3] <- 2 
fitr$segments$x[fitr$segments$x == 4] <- 3 
fitr$segments$xend[fitr$segments$xend == 3] <- 2 
fitr$segments$xend[fitr$segments$xend == 4] <- 3 
fitr$leaf_labels$y <- 0.08900524
fitr$labels$y[fitr$labels$y == 1] <- 0.625 
fitr$labels$x <- c(2, 1.5, 2.5)
fitr$leaf_labels$x <- c(1,2,2,3)
fitr$labels$label <- c("Flipper Length < 206.5 mm", "Bill Length < 43.35 mm", "Bill Depth >= 17.65 mm")

# Plot the decision tree
a <- ggplot() +
  geom_segment(data = fitr$segments, aes(
    x = x,
    y = y,
    xend = xend,
    yend = yend
  )) +
  geom_label(
    data = fitr$labels,
    aes(x = x, y = y, label = label),
    label.padding = unit(.5, "lines"),
    hjust = "center",
    vjust = "center"
  ) +
  geom_label(
    data = fitr$leaf_labels,
    aes(
      x = x,
      y = y,
      label = label,
      fill = label
    ),
    hjust = "center",
    vjust = "center",
    color = "white",
    label.padding = unit(1, "lines"),
    show.legend = F
  ) +
  ggdendro::theme_dendro() +
  scale_fill_manual(values = c("#03B5AA", "#020887", "#A663CC")) +
  xlim(0.9, 3.1) + ylim(0, 0.7) +
  annotate(
    geom = "text",
    x = 1.5,
    y = 0.7,
    label = "TRUE",
    fontface = "bold"
  ) +
  annotate(
    geom = "curve",
    x = 1.6,
    xend = 1.4,
    y = 0.65,
    yend = 0.65,
    curvature = 0,
    arrow = arrow(length = unit(2, "mm")),
    size = 1
  ) +
  annotate(
    geom = "text",
    x = 2.5,
    y = 0.7,
    label = "FALSE",
    fontface = "bold"
  ) +
  annotate(
    geom = "curve",
    x = 2.4,
    xend = 2.6,
    y = 0.65,
    yend = 0.65,
    curvature = 0,
    arrow = arrow(length = unit(2, "mm")),
    size = 1
  ) +
  annotation_custom(
    img,
    x = 1,
    xmax = 1.25,
    ymin = 0.35,
    ymax = 0.7
  ) +
  NULL 

# Plot the histograms
b <- penguins %>%
  pivot_longer(c(bill_length_mm, bill_depth_mm, flipper_length_mm)) %>%
  mutate(
    name = case_when(
      name == "bill_length_mm" ~ "Bill Length (mm)",
      name == "bill_depth_mm"  ~ "Bill Depth (mm)",
      name == "flipper_length_mm" ~ "Flipper Length (mm)"
    )
  ) %>%
  ggplot(aes(x = value, fill = species)) +
  geom_histogram(position = position_identity(),
                 alpha = .5,
                 bins = 25) +
  facet_wrap( ~ name, scales = "free_x") +
  theme_minimal() +
  scale_fill_manual(values = c("#03B5AA", "#020887", "#A663CC")) +
  theme(
    axis.line.y = element_blank(),
    axis.text.y = element_blank(),
    strip.text = element_text(face = "bold"),
    plot.title = element_text(size = 20),
    plot.subtitle = element_markdown(size = 10, margin = margin(0, 0, 30, 0)),
    legend.position = "none"
  ) +
  labs(
    y = "",
    x = "",
    title = "Modelling Penguin Species",
    subtitle = "Assigning penguin species (
       <b style='color:#03B5AA'>Adelie</b>,
       <b style='color:#020887'>Chinstrap</b>,
       <b style='color:#A663CC'>Gentoo</b>) using a categorisation tree from the {rpart} package."
  )

# Combine the plots!
cowplot::plot_grid(b, a, ncol = 1, rel_heights = c(1.4,1))
                                                                                
