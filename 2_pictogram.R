
library(extrafont)
library(tidyverse)
library(waffle)
library(ggtext)
library(cowplot)

loreal_food <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-03-30/allCategories.csv') %>% 
  filter(!is.na(name) & brand %in% c("L'Oréal")) %>% 
  filter(str_detect(categories, "drink") | str_detect(categories, "food")) %>% 
  distinct(name, .keep_all = TRUE) %>% 
  arrange(desc(lightness)) %>% 
  top_n(-24) %>% 
  mutate(num = 1)

loreal_gem <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-03-30/allCategories.csv') %>% 
  filter(!is.na(name) & brand %in% c("L'Oréal")) %>% 
  filter(str_detect(categories, "gem")) %>% 
  distinct(name, .keep_all = TRUE) %>% 
  arrange(desc(lightness)) %>% 
  top_n(-24) %>% 
  mutate(num = 1)

food_plot <- loreal_food %>%
  ggplot(aes(fill = hex, values = num)) +
  geom_waffle(color = "white", n_rows = 4, size = 0.33,  flip = TRUE) +
  scale_fill_manual(
    name = NULL,
    values = loreal_food$hex,
    labels = loreal_food$name
  ) +
  coord_equal() +
  theme_enhance_waffle() +
  labs(
    x = "",
    y = "",
    title = "Hues named after food/drink"
  ) +
  theme(legend.title = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        panel.background = element_blank(),
        legend.position = "None", 
        plot.title = element_text(size = 12, family = "Cambria")) 

gem_plot <- loreal_gem %>%
  ggplot(aes(fill = hex, values = num)) +
  geom_waffle(color = "white", n_rows = 4, size = 0.33,  flip = TRUE) +
  scale_fill_manual(
    name = NULL,
    values = loreal_gem$hex,
    labels = loreal_gem$name
  ) +
  coord_equal() +
  theme_enhance_waffle() +
  labs(
    x = "",
    y = "",
    title = "Hues named after gems"
  ) +
  theme(legend.title = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        panel.background = element_blank(),
        legend.position = "None", 
        plot.title = element_text(size = 12, family = "Cambria")) 

title <- ggplot() + 
  labs(title = "What do L'Oréal hues say about microaggressions in the beauty industry?",
       subtitle = "Data: TidyTuesday Makeup Shades") +
  theme(panel.background = element_blank(),
        plot.title = element_text(size = 12, family = "Cambria", face = "bold"),
        plot.subtitle = element_text(size = 8, family = "Cambria")) 
  
hue_plots <- plot_grid(gem_plot, food_plot)
 
plot_grid(title, hue_plots, ncol = 1, rel_heights = c(0.15, 1))
