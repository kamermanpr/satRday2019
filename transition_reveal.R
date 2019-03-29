# Load packages
library(tidyverse)
library(magrittr)
library(gganimate)
library(ggrepel)

# Define Palette
pal <- c('#E69F00', '#56B4E9', '#009E73')

# Import data
data <- read_csv('data/IHME-GBD-cleaned.csv')

# Process data
## Rank data by year
data %<>% 
    # Select females
    filter(sex == 'Female') %>% 
    # Select global
    filter(location == 'Global') %>% 
    # Trim years (for reveal effect)
    filter(year %in% c(1990, 1995, 2000, 2005, 2010, 2015, 2017)) %>% 
    # Nest by year
    group_by(year) %>% 
    nest() %>% 
    # Rank causes within each year and then select the top ten / year
    mutate(rank = map(.x = data,
                      ~ .x %>% 
                          arrange(desc(val)) %>% 
                          mutate(rank = row_number()) %>% 
                          filter(rank <= 10))) %>% 
    # Unnest
    select(-data) %>% 
    unnest()

## Generate cause filter 
cause_filter <- data %>% 
    filter(year == 2017) %>% 
    filter(rank <= 10) %>% 
    .$cause

## Filter
data %<>% 
    filter(cause %in% cause_filter)

p2 <- ggplot(data = data) +
    aes(x = year, 
        y = val, 
        group = cause) +
    geom_line(aes(colour = colour),
              size = 1) +
    geom_point(aes(fill = colour,
                   colour = colour),
               size = 6) +
    geom_text_repel(aes(label = cause, 
                        colour = colour),
                    size = 8,
                    hjust = 0, 
                    direction = 'y',
                    nudge_x = 2) +
    scale_colour_manual(values = pal) +
    scale_x_continuous(limits = c(1990, 2045),
                       expand = c(0, 0),
                       breaks = c(1990, 2000, 2010, 2020)) +
    labs(subtitle = 'Trends for the leading causes of female global DALY per 100k population',
         x = 'Year', 
         y = 'DALY per 100k population') +
    theme_minimal(base_size = 20) +
    theme(legend.position = 'none',
          panel.grid = element_blank())

p2_treveal <- p2 + transition_reveal(along = year)

animate(p2_treveal,
        res = 72,
        width = 800,
        height = 700,
        nframe = 200,
        end_pause = 50,
        duration = 20)
