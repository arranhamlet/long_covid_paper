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

#Load in shapefile for Washington
WA_adm2 <- readOGR(here("data", "shapefile", "WA_County_Bndys.shp"))

#Load age data - using import list because we want the 2nd page of a xlsx file
age_data_raw <- import_list(here("data", "raw", "demographic", "ofm_pop_sade_state_2010_to_2020.xlsx"))[[2]]

#Load in state demography data
demog_data <- import(here("data", "processed", "demographic", "demographic_data_processed.csv"))

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

#Aggregate data for subgroup plots
aggregate_runs <- prepare_subgroup_data(state_demography_data = demog_data,
                                        age_data = age_data_raw,
                                        model_estimates = model_estimates)

#Plot incidence and prevalence
case_hosp_long_data <- incidence_prevalence_plot(model_run = model_estimates, 
                                                 model_data = model_data,
                                                 adult_population = 5994805)

#Plot breakdown by age_group/sex/race-ethnicity
subgroup_prevalence <- plot_prevalence_by_subgroup(subgroup_data = aggregate_runs)

#Plot map
map_prevalence <- plot_map_results(model_estimates = model_estimates,
                 state_demography_data = state_demography_data,
                 shapefile = WA_adm2)

#Prevalence by subgroup - table and summary text
prevalence_text_table <- pretty_format_model_breakdown(model_estimates = model_estimates,
                                        pulse_data = symptom_prevalence_raw,
                                        subgroup_prevalence = aggregate_runs,
                                        state_population = 7656200,
                                        adult_population = 5994805,
                                        cost_per_disabled = 15068)

#Export plots and table
ggsave("figs/case_hosp_long_data.jpg",
       case_hosp_long_data,
       height = 5, width = 8)

ggsave("figs/subgroup_prevalence.jpg",
       subgroup_prevalence,
       height = 7, width = 11)

ggsave("figs/map_prevalence.jpg",
       map_prevalence,
       height = 6, width = 11)

write.csv(prevalence_text_table[[2]],
          "figs/end_prevalence_table.csv",
          row.names = FALSE)

