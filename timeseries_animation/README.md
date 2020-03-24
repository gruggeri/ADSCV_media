
# Animating timeseries

``` r
library(tidyverse)
library(lubridate)
library(janitor)
library(ggtext)
library(gganimate)
```

``` r
cases_raw <- read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Confirmed.csv")
```

The following visualisation is inspired by John Burn-Murdoch work.

``` r
cases_data <- cases_raw %>%
  pivot_longer(cols= matches("\\d+/\\d+/\\d+"), 
               names_to = "date", 
               values_to = "cases") %>% 
  clean_names() %>% 
  transmute(country_region, date = mdy(date), cases) %>% 
  filter(!country_region %in% c("Others", "China", "Cruise Ship")) %>%
  group_by(country_region, date) %>% 
  summarise(cases = sum(cases)) %>% 
  filter(cases >= 100) %>%
  filter(n() >= 5) %>%
  arrange(date, .by_group = TRUE) %>%
  mutate(days_since_100 = (date-first(date)) / ddays(1), 
         new_cases = cases - first(cases)) %>%
  ungroup() %>% 
  bind_rows(
    tibble(country_region = "33% daily rise", days_since_100 = 0:20) %>%
      mutate(cases = 100*1.33^days_since_100)
  ) %>%
  mutate(color_label = forcats::fct_collapse(country_region,
    "#D63D32" = "Switzerland",
    "#888888" = "Italy",
    "#6699CC" = "Iran",
    "#661100" = "Germany",
    "#882255" = "France",
    "#999933" = "United Kingdom",
    "#44AA99" = "US",
    "#332288" = "Spain",
    "#117733" = "Korea, South",
    "#DDCC77" = "Netherlands",
    "#CC503E" = "Hong Kong",
    "#1D6996" = "Singapore",
    "#855C75" = "Japan",
    "black"   = "33% daily rise",
    other_level = "grey90"),
    color_label = fct_relevel(color_label, "grey90")) %>%
  arrange(color_label) %>%
  mutate(country_region = fct_inorder(country_region),
         line_type = ifelse(country_region == "33% daily rise", "2", "1")) %>% 
  mutate(country_label = ifelse(color_label == "grey90", "", as.character(country_region)))
```

``` r
ggplot(data = cases_data, 
       mapping = aes(x = days_since_100, 
                     y = cases, 
                     color = color_label,
                     group = country_region)) +
  geom_point(size = 0.8, alpha = 0.9, pch = 21)+
  geom_line(mapping = aes(linetype = line_type), 
            size = 0.7, alpha =0.9) +
  scale_y_log10(expand = expansion(add = c(0,0.1)), 
                breaks=c(100, 500, 2000,  10000, 60000)) +
  shadowtext::geom_shadowtext(aes(label = paste0(" ",country_label)),
                              hjust=0, vjust = 0, bg.color = "white") +
  scale_color_identity()+
  theme_minimal() +
  theme(
    panel.grid.minor = element_blank(),
    legend.position = "none",
    plot.margin = margin(3,15,3,3,"mm")
  )+
  coord_cartesian(clip = "off") +
  labs(x = "Number of days since 100th case", 
       y = "Total Number of Cases",
       title = "Total number of COVID-19 cases",
       subtitle =  "Outside of China",
       caption = "Data Source: John Hopkins University") +
  transition_reveal(days_since_100)
```

![](README_files/figure-gfm/unnamed-chunk-4-1.gif)<!-- -->

``` r
#anim_save("covid.gif")
```
