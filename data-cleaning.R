############################################################
#                                                          #
#           Basic clean-up of the original data            #
#                                                          #
############################################################

# Load packages
library(tidyverse)
library(magrittr)

# Import data
data <- read_csv('data/IHME-GBD-original.csv')

# Shorten cause names (where possible)
data %<>%
    mutate(cause = case_when(
        cause == 'Diarrheal diseases' ~  'Diarrhoeal diseases',
        cause == 'Chronic obstructive pulmonary disease' ~ 'COPD',
        cause == 'Congenital birth defects' ~ 'Congenital defects',
        cause == 'Ischemic heart disease' ~ 'Ischaemic heart disease',
        cause == 'Diabetes mellitus' ~ 'Diabetes',
        cause == 'Age-related and other hearing loss' ~ 'Hearing loss',
        cause == 'Dietary iron deficiency' ~ 'Iron deficiency',
        cause == 'Other musculoskeletal disorders' ~ 'Musculoskeletal disorders',
        cause == 'Protein-energy malnutrition' ~ 'Malnutrition',
        TRUE ~ as.character(cause)
    ))

# Add colour coding
## A: Communicable, maternal, nutritional, injury causes
## B: Non-communicable causes
## C: Low back pain
data %<>%
    mutate(colour = case_when(
        cause == 'Lower respiratory infections' | cause == 'Diarrhoeal diseases' |
            cause == 'Measles' | cause == 'Tuberculosis' | cause == 'Malaria' | 
            cause == 'HIV/AIDS' | cause == 'Meningitis' ~ 'A',
        cause == 'Neonatal disorders' | cause == 'Iron deficiency' | 
            cause == 'Malnutrition' | cause == 'Self-harm' | 
            cause == 'Road injuries' ~ 'A',
        cause == 'Low back pain' ~ 'C',
        TRUE ~ 'B'
    ))

# Write to csv
write_csv(data, 'data/IHME-GBD-cleaned.csv')
