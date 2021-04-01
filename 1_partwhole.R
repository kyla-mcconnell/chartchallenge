
library(tidyverse)
library(gggibbous)
library(ggtext)
library(extrafont)

bechdel <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-03-09/raw_bechdel.csv')

bechdel <- bechdel %>% 
  mutate(bechdel = ifelse(rating == 3, "pass", "fail")) %>% 
  count(year, bechdel) %>% 
  filter(year >= 2000 &  year < 2020) %>% 
  group_by(year) %>% 
  mutate(total = sum(n),
         ratio = n / total,
         right = ifelse(bechdel == "pass", TRUE, FALSE), 
         pass_ratio = ifelse(bechdel == "pass", ratio, NA)) %>% 
  fill(pass_ratio, .direction = "up") %>% 
  ungroup() 
  
ggplot(data = bechdel) +
  geom_line(aes(x= year, y = pass_ratio), color = "#686868") +
  geom_moon(aes(x = year, y = pass_ratio, ratio = ratio, right = right, fill = bechdel), color = "#686868") +
  geom_text(aes(x = 2017, y = .69, label = "67%: 2017"), size = 2, color = "#686868") +
  geom_text(aes(x = 2002, y = .53, label = "55%: 2002"), size = 2, color = "#686868") +
  ylim(.51, .72) +
  scale_x_continuous(breaks=seq(2000, 2019, 2)) +
  scale_fill_manual(values = c("#F5CBA7", "#45B39D")) +
  labs(
    x = "",
    y = "",
    title = "What percentage of movies <b style = 'color:#45B39D;'>pass the Bechdel test</b>?",
    subtitle = "<i style = 'color:#686868'> To pass, a film must contain at least 2 named women that talk to each other about anything other than men! </i>",
    caption = "Data: TidyTuesday GH / Bechdel API"
  ) +
  theme(legend.title = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        panel.background = element_blank(),
        plot.title = element_markdown(size = 11),
        plot.subtitle = element_markdown(size = 8),
        plot.caption = element_markdown(size = 6),
        legend.position = "None", 
        text = element_text(size = 12, family = "Verdana")) 
