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
  labs(title = "Most of Tate Gallery art pieces created after 1990 are etching on paper",
       subtitle = "Only 179 art pieces created after 1990 are digital prints",
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
  labs(title = "Most of Tate Gallery art pieces created after 1990 are etching on paper",
       subtitle = "Only 179 art pieces created after 1990 are digital prints",
       y = "",
       x = "Number of Artworks", 
       caption = "Source: Tate Art Museum") +
  theme(axis.text = element_text(size = 10),
        plot.title.position = "plot")

ggsave("non_ordered_bars.pdf", width = 6, height = 3.5)

#### using colors to highlight

artwork %>% 
  filter(year > 1990) %>% 
  mutate(medium = fct_rev(fct_infreq(medium))) %>% 
  count(medium, name = "artworks", sort = TRUE) %>% 
  mutate(medium_col = ifelse(medium == "Digital print on paper",
                             "#70284a" ,
                             "#bdbdbd")) %>% 
  filter(!is.na(medium)) %>% 
  slice_max(artworks, n= 10) %>% 
  ggplot(aes(x = artworks, 
             y = medium)) +
  geom_col(aes(fill = medium_col)) +
  scale_x_continuous(expand = c(0,0), lim = c(0, 650)) +
  scale_fill_identity()+
  geom_text(aes(label = artworks), nudge_x = 40, size = 3)+
  theme_minimal() +
  labs(title = "Most of Tate Gallery art pieces created after 1990 are etching on paper",
       subtitle = "Only 179 art pieces created after 1990 are digital prints",
       y = "",
       x = "Number of Artworks", 
       caption = "Source: Tate Art Museum") +
  theme(axis.text = element_text(size = 10),
        plot.title.position = "plot")

ggsave("scales.pdf", width = 6, height = 3.5)




# Using ggtext




artwork %>% 
  filter(year > 1990) %>% 
  mutate(medium = fct_rev(fct_infreq(medium))) %>% 
  count(medium, name = "artworks", sort = TRUE) %>% 
  mutate(medium_col = ifelse(medium == "Digital print on paper",
                             "#70284a" ,
                             "#bdbdbd")) %>% 
  filter(!is.na(medium)) %>% 
  slice_max(artworks, n= 10) %>% 
  ggplot(aes(x = artworks, 
             y = medium)) +
  geom_col(aes(fill = medium_col)) +
  scale_x_continuous(expand = c(0,0), lim = c(0, 650)) +
  scale_fill_identity()+
  geom_text(aes(label = artworks), nudge_x = 40, size = 3)+
  theme_minimal() +
  labs(title = "Most of Tate Gallery art pieces created after 1990 are etching on paper",
       subtitle = paste0("Only <b style='color:#70284a'>179 </b style='color:#70284a'>",
                         "art pieces created after 1990 are" ,
                         "<b style='color:#70284a'>digital prints</b style='color:#70284a'>"),
       y = "",
       x = "Number of Artworks", 
       caption = "Source: Tate Art Museum") +
  theme(axis.text = element_text(size = 10),
        plot.title.position = "plot",
        plot.subtitle = ggtext::element_markdown())

ggsave("ggtext_scales.pdf", width = 6, height = 3.5)
## adding color to y axis

artwork %>% 
  filter(year > 1990) %>% 
  count(medium, name = "artworks", sort = TRUE) %>% 
  mutate(medium_col = if_else(medium == "Digital print on paper",
                             "#70284a" ,
                             "#bdbdbd")) %>% 
  mutate(medium_styled = if_else(medium == "Digital print on paper",
                                glue::glue("<b style='color:#70284a'>{medium}</b>"),
                                as.character(medium))) %>% 
  mutate(medium_styled = fct_reorder(.f = medium_styled, .x = artworks)) %>% 
  filter(!is.na(medium)) %>% 
  slice_max(artworks, n= 10) %>% 
  mutate(artworks_styled = if_else(medium == "Digital print on paper",
                                glue::glue("<b style='color:#70284a'>{artworks}</b>"),
                                as.character(artworks))) %>% 
  ggplot(aes(x = artworks, 
             y = medium_styled)) +
  geom_col(aes(fill = medium_col)) +
  scale_x_continuous(expand = c(0,0), lim = c(0, 650)) +
  scale_fill_identity()+
  ggtext::geom_richtext(aes(label = artworks_styled),
                        nudge_x = 40, 
                        size = 3,
                        fill = NA, label.color = NA)+
  theme_minimal() +
  labs(title = "Most of Tate Gallery art pieces created after 1990 are etching on paper",
       subtitle = "Only <b style='color:#70284a'>179 </b style='color:#70284a'> art pieces created after 1990 are <b style='color:#70284a'>digital prints</b style='color:#70284a'>",
       y = "",
       x = "Number of Artworks", 
       caption = "Source: Tate Art Museum") +
  theme(axis.text = element_text(size = 10),
        plot.title.position = "plot",
        plot.subtitle = ggtext::element_markdown(),
        axis.text.y = ggtext::element_markdown(),
        )
ggsave("ggtext_scales_final.pdf", width = 6, height = 3.5)



