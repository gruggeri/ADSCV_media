###########
library(tidyverse)


artwork <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-01-12/artwork.csv')


artwork %>% 
  filter(year > 1990) %>% 
  mutate(medium = fct_rev(fct_infreq(medium))) %>% 
  count(medium, name = "artworks", sort = TRUE) %>% 
  filter(!is.na(medium)) %>% 
  slice_max(artworks, n= 10) %>% 
  ggplot(aes(x = artworks, 
             y = medium)) +
  geom_col(fill = "#70284a") +
  scale_x_continuous(expand = c(0,0), lim = c(0, 650)) +
  geom_text(aes(label = artworks), nudge_x = 40, size = 3)+
  theme_minimal() +
  labs(title = "Most of Tate Gallery art pieces aquired after 1990 are etching on paper",
       subtitle = "Only 179 art pieces aquired after 1990 are digital prints",
       y = "",
       x = "Number of Artworks", 
       caption = "Source: Tate Art Museum") +
  theme(axis.text = element_text(size = 10),
        plot.title.position = "plot")


ggsave("ordered_bars.pdf", width = 6, height = 3.5)



artwork %>% 
  filter(year > 1990) %>% 
  count(medium, name = "artworks", sort = TRUE) %>% 
  filter(!is.na(medium)) %>% 
  slice_max(artworks, n= 10) %>% 
  ggplot(aes(x = artworks, 
             y = medium)) +
  geom_col(fill = "#70284a") +
  scale_x_continuous(expand = c(0,0), lim = c(0, 650)) +
  geom_text(aes(label = artworks), nudge_x = 40, size = 3)+
  theme_minimal() +
  labs(title = "Most of Tate Gallery art pieces aquired after 1990 are etching on paper",
       subtitle = "Only 179 art pieces aquired after 1990 are digital prints",
       y = "",
       x = "Number of Artworks", 
       caption = "Source: Tate Art Museum") +
  theme(axis.text = element_text(size = 10),
        plot.title.position = "plot")

ggsave("non_ordered_bars.pdf", width = 6, height = 3.5)
