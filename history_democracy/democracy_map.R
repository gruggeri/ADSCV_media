####################################
## 2021 January 7
## Author: Giulia Ruggeri
## legend customisation inspired by
## https://timogrossenbacher.ch
####################################


library(sf)
library(tidyverse)


age_dem <- read_csv("history_democracy/age-of-democracies.csv") %>% 
  janitor::clean_names() %>% 
  mutate(across(contains("age_"), as.numeric)) %>% 
  rename(age = age_of_democracies_at_the_end_of_2015_boix_miller_and_rosato_2013_2018)



world <- st_read("history_democracy/naturalearth/ne_110m_admin_0_countries.shp") %>% 
  left_join(age_dem, c("ADM0_A3" = "code")) %>% 
  filter(SOV_A3 != "ATA") %>% 
  st_transform(crs = 'ESRI:54013')

# quantile_vec <- world %>%
#   pull(age) %>%
#   quantile(probs = c(0, 0.25, 0.5, 0.75, 1), na.rm = TRUE)

myvec <- c(0, 25, 50, 75, 100, 200, 220 )

labels <- tibble(
  lab1 = myvec,
  lab2 = c(myvec[2:length(myvec)], NA)
) %>%
  slice(1:n() - 1) %>% 
  mutate(labs = paste(lab1, lab2, sep = " - "))


world <- world %>%
  mutate(quantiles = cut(age,
                         breaks = myvec,
                         labels = labels$labs,
                         include.lowest = TRUE, 
                         ordered_result = TRUE,
  ))


ggplot() +
  geom_sf(
    data = world,
    mapping = aes(fill = quantiles),
    size = 0.3
  ) +
  rcartocolor::scale_fill_carto_d(palette = "BurgYl", na.translate=FALSE) +
  guides(fill = guide_legend(
    direction = "horizontal",
    keyheight = unit(2, units = "mm"),
    keywidth = unit(50 / length(labels), units = "mm"),
    title.position = 'top',
    title.hjust = 0,
    label.hjust = 0.5,
    nrow = 1,
    byrow = T,
    label.position = "bottom"
  )) +
  theme_void() +
  labs(
    subtitle = "US is the most ancient democracy",
    title = "Age of the democracies in 2015 ",
    caption = "Source: Our world in Data \n
    Boix, Miller, and Rosato, 2013, 2018",
    fill = "Age of democracy (Years)"
  ) +
  theme(
    plot.title = element_text(size = 18, hjust = 0.5),
    plot.subtitle = element_text(size = 10, hjust = 0.5,
                                 color = "#70284A",
                                 face = "bold",
                                 margin = margin(b = -0.1,
                                                 t = -0.1,
                                                 l = 2,
                                                 unit = "cm"),
                                 debug = F),
    legend.title = element_text(size = 8),
    legend.position = "bottom",
    plot.caption = element_text(size = 7,
                                hjust = .5,
                                margin = margin(t = 0.2,
                                                b = 0,
                                                unit = "cm"),
                                color = "#939184")
    
  )
  
ggsave("history_democracy/history_of_democracy.png")


# 
# 
# pol_reg <- read_csv("history_democracy/political-regime-updated2016.csv") %>% 
#   janitor::clean_names() %>% 
#   mutate(across(contains("age_"), as.numeric))
