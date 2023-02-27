options(scipen = 999)

#Load in packages
if(!require("pacman")) install.package("pacman")  #If pacman package doesnt exist, install it
#p_load looks to see if packages exists, and if they do loads them and if they dont, installs and loads them
pacman::p_load(odin,       #This is the package that contains the language odin which solves the ODEs
               ggplot2,    #used for plotting
               scales,     #used for plotting nice scales
               rio,        #used to import data
               here,       #used to simplify file paths
               data.table, #used to manipulate data.frames
               lubridate,  #used to clean dates
               janitor,    #used to clean data
               devtools,   #used to install packages 
               rgdal,      #used for shapefules
               ggpubr,   
               maptools,
               viridis,
               grid,
               raster,
               tsibble,    #used for yearweek
               tidyverse  #a variety of packages for manipulating data 
)  

#Load functions
invisible(sapply(list.files("R/functions/", full.names = T), function(x) source(x)))

#Load age data
age_data_raw <- import_list(here("data", "raw", "demographic", "ofm_pop_sade_state_2010_to_2020.xlsx"))[[2]]

#Load and clean Symptom prevalence
symptom_prevalence_raw <- import(here("data", "processed", "demographic", "long_covid_prevalence_by_group_data_processed.csv"))
symptom_prevalence <- symptom_prevalence_raw %>%
  subset(indicator == "Currently experiencing long COVID, as a percentage of all adults" &
           subgroup == "Washington") %>%
  mutate(across(contains("date"),
                .fns = mdy)) %>%
  group_by(time_period) %>%
  mutate(mean_date = mean(c(time_period_start_date,
                            time_period_end_date)))

#Load in model estimates
model_estimates <- import(max(list.files(here("data", "processed", "model_predictions"), pattern = "county", full.names = T)))

#Load in raw data
model_data <- prepare_data_for_model()

#Plot incidence and prevalence
case_hosp_long_data <- incidence_prevalence_plot(model_run = model_estimates, 
                                                 model_data = model_data,
                                                 adult_population = 5994805)

#Plot breakdown by age_group/sex/race-ethnicity


