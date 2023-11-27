#Load in packages
if(!require("pacman")) install.package("pacman")  #If pacman package doesnt exist, install it

#p_load looks to see if packages exists, and if they do loads them and if they dont, installs and loads them
pacman::p_load(ggplot2,    #used for plotting
               scales,     #used for plotting nice scales
               rio,        #used to import data
               here,       #used to simplify file paths
               data.table, #used to manipulate data.frames
               lubridate,  #used to clean dates
               janitor,    #used to clean data
               devtools,   #used to install packages 
               rgdal,      #used for shapefules
               ggpubr,     #used for arranging plots
               maptools,   #used for plotting maps
               viridis,    #colorscheme for plots
               grid,       #used for arranging plots
               raster,     #used for plotting maps
               tsibble,    #used for yearweek
               tidyverse   #a variety of packages for manipulating data 
)  

#Load functions
invisible(sapply(list.files("R/functions/", full.names = T, recursive = T), function(x) source(x)))

#Load in shapefile for Washington
WA_adm2 <- readOGR(here("data", "shapefile", "WA_County_Bndys.shp"))

#Load age data - using import list because we want the 2nd page of a xlsx file
age_data_raw <- import_list(here("data", "raw", "demographic", "ofm_pop_sade_state_2010_to_2020.xlsx"))[[2]]

#Load in state demography data
demog_data <- import(here("data", "processed", "demographic", "demographic_data_processed.csv"))

#Load and clean Symptom prevalence
symptom_prevalence_raw <- import(here("data", "processed", "demographic", "long_covid_prevalence_by_group_data_processed.csv"))

#Load in model estimates - this will load the file with the latest date
model_estimates <- fread(max(list.files(here("data", "processed", "model_predictions"), 
                                         pattern = "county", 
                                         full.names = T)))

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
                 state_demography_data = demog_data,
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

