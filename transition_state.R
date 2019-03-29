############################################################
#                                                          #
#                 Use of transition_state                  #
#                                                          #
############################################################

# Load packages
library(tidyverse)
library(gganimate)

# Define palette
pal <- c('#E69F00', '#56B4E9', '#009E73')

# Import data
data <- read_csv('data/IHME-GBD-cleaned.csv')

# Process data
global_female <- data %>% 
    # Select females
    filter(sex == 'Female') %>% 
    # Select global
    filter(location == 'Global') %>% 
    # Trim years (for shadow_mark effect)
    filter(year %in% c(1990, 2000, 2010, 2017)) %>% 
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
    unnest() %>% 
    # Add padding to text labels
    mutate(cause = str_pad(cause,
                           width = 28,
                           side = 'both'))

p1 <- ggplot(data = global_female) +
    aes(x = year, 
        y = rev(rank), 
        colour = colour, 
        fill = colour) +
    geom_label(aes(label = cause), 
               size = 6, 
               alpha = 0.2,
               label.padding = unit(1.2, 'lines'),
               label.size = 0.5) +
    scale_fill_manual(values = pal) +
    scale_colour_manual(values = pal) +
    scale_x_continuous(breaks = c(1990, 2000, 2010, 2017),
                       limits = c(1988, 2019)) +
    scale_y_continuous(breaks = 1:10,
                       labels = 10:1) +
    labs(subtitle = 'Rank order of leading causes of female global DALY per 100k population',
         x = 'Year', 
         y = 'Rank') +
    theme_minimal(base_size = 20) +
    theme(legend.position = 'none',
          panel.grid = element_blank())

p1_tstate <- p1 +
    transition_states(states = year, 
                      wrap = FALSE) +
    ease_aes('linear') +
    shadow_mark() +
    exit_fade()

animate(p1_tstate,  
        res = 72,
        width = 1300,
        height = 700,
        fps = 10,     # default
        nframe = 100, # default
        end_pause = 30,
        start_pause = 10)