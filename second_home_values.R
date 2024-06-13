# exploring BHPS data 

# clear environment
rm(list=ls())

# set paths 
root <- "Q:/ADD Directorate/CASA/Homeownership/BHPS Data"
ukhls <- paste0(root, "/stata/stata13_se/ukhls")

# setwd
setwd(root)

# install pacman to load packages
if(!require(pacman))install.packages("pacman")  

# load packages
pacman::p_load( 
  'tidyverse',     # data cleaning / piping  
  'haven',         # importing dta files 
  'lubridate'      # working with dates
)

# collect ukhls files 

ukhls_files <- list.files(ukhls) 
hh_files <- ukhls_files[grep("hhresp",ukhls_files)] # filter on hhresp files


# set vars for loop

vars <- c("hs2valo","hscost","xpmg")
df <- NA
ukhls_df <- data.frame(matrix(nrow = 1, ncol = length(vars))) 
colnames(ukhls_df) <- vars

# begin loop

for(ii in hh_files){ 

  wave <- str_sub(ii,1,2) # extract wave name
  
  df <- read_dta(file = paste0(ukhls,"/",ii)) |> 
    rename_all(funs(str_replace_all(.,wave, ""))) |> # remove wave prefix
    select(any_of(vars)) # select only vars
  
  ukhls_df <- bind_rows(ukhls_df,df)
  
  }

# remove first row (which is NA) 
ukhls_df <- ukhls_df[-1,]

# distribution of second home value 
ukhls_df %>% 
  drop_na() %>% 
  select(hs2valo) %>% 
  filter(!hs2valo < 0) %>% 
  ggplot(aes(x = hs2valo)) + 
  geom_density(fill = "steelblue", alpha = 0.8) + 
  xlim(c(0,1000000)) + 
  theme_ipsum() + 
  labs(subtitle = "Distribution of Second Homes Values", 
       x = "Second Home Value, £")

# averages 
ukhls_df %>% 
  drop_na() %>% 
  select(hs2valo) %>% 
  filter(!hs2valo < 0) %>% 
  summarise(median = median(hs2valo), 
            count = n())
  
  
  
  
  



