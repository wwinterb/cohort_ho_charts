### BHPS Homeownership by cohort charts ### 


# clear environment
rm(list=ls())
setwd("Q:/ADD Directorate/CASA/Homeownership/BHPS Data")

# install pacman to load packages
if(!require(pacman))install.packages("pacman")  

# load packages
pacman::p_load( 
  'tidyverse', # data cleaning / piping 
  'ggthemes' # ggplot themes
)

# import cleaned file
BHPS <- readRDS("combined_BHPS_file.RDS") 

# filter tenure and age 
cohort_df <- BHPS %>% 
  select(doby, tenure_dv, intyear) %>% 
  drop_na() %>% 
  mutate(age_at_int =  as.numeric(intyear) - as.numeric(doby), 
         homeowner = case_when( 
           tenure_dv == "Owned with Mortgage" ~ 1, 
           tenure_dv == "Owned Outright" ~ 1  ,
           TRUE ~ 0) )
  
# put birth into cohorts 
coh_chart <- cohort_df %>% 
  mutate(intyear = as.integer(intyear), 
         age_group = cut(doby, 
                         breaks = seq(1960,1990, by = 5), 
                         labels = c("1960-65","1965-70", "1970-75","1975-80", "1980-85", "1985-1990"))
           ) %>% 
  select(age_group, homeowner, intyear, age_at_int) %>% 
  drop_na() %>% 
  group_by(age_group, age_at_int) %>% 
  summarise(ho_share = mean(homeowner), n = n()) %>% 
  filter(age_at_int > 20, age_at_int < 55)

# plot chart
ggplot(coh_chart, aes(x = age_at_int, y = ho_share, color = age_group)) + 
  geom_line(size = 1.2) + 
  theme_clean() + 
  scale_color_brewer() + 
  labs(x = "Age", 
       y = "Share in Homeownership", 
       title = "Homeownership by cohort")




  

