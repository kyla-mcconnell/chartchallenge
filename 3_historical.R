library(tidyverse)
library(ggtext)
library(cowplot)
library(extrafont)

#Data source: Google 
debt <- tibble(country = c("USA", "USA", "DE", "DE"),
               year = c(2020, 2021, 2020, 2021),
               debt_bil = c(23300, 27900, 1305, 1800),
               gdp = c(20930, 21430, 3332, 3861)) %>% 
  mutate(debt_gdp = debt_bil/gdp)

debt_plot <- ggplot(data = debt) +
  geom_line(aes(x = year, y = debt_gdp, color = country), 
            lineend = "butt", linejoin = "round", size = 1,
            arrow = arrow(length = unit(0.1, "inches"))) +
  geom_label(aes(x=year, y = .8, label = year, color = country), size = 3) +
  scale_y_continuous(labels = scales::percent, limits = c(0, 1.5)) +
  xlim(2019.8, 2021.2) +
  scale_color_manual(values = c("#B6270D", "#080A86")) +
  labs(
    x = "",
    y = "",
    title = "National debt pre-Covid vs. mid-Covid",
    subtitle = "as % of GDP \n"
  ) +
  facet_wrap(~country) +
  theme(legend.title = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.ticks.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.x = element_blank(),
        text = element_text(size = 8),
        panel.background =  element_rect(fill = "#FCFCFC", color = "#CCCCCC"),
        legend.position = "None", 
        strip.text.x = element_blank(),
        plot.title = element_text(size = 15, family = "Perpetua", face = "bold", hjust = 0.5),
        plot.subtitle = element_text(size = 13, family = "Perpetua", face = "bold", hjust = 0.5)) 

ggdraw() +
  draw_plot(debt_plot) +
  draw_image("https://emojigraph.org/media/lg/flag-germany_1f1e9-1f1ea.png", scale = .13, x = -0.18, y = 0.31) +
  draw_image("https://cdn-0.emojis.wiki/emoji-pics/facebook/united-states-facebook.png", scale = -.14, x = 0.28, y = 0.31)


            