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
               abind,      #used for array manipulation
               lubridate,  #used to clean dates
               janitor,    #used to clean data
               devtools,   #used to install packages 
               tsibble,    #used for yearweek
               lhs,         #used to carry out latin hypercube sampling
               tidyverse  #a variety of packages for manipulating data 
)  

#Load functions
invisible(sapply(list.files("R/functions/", full.names = T), function(x) source(x)))

#Import fit parameters
load_in_fit_parameters <- import(max(list.files(here("data", "processed", "fit_parameters"), full.names = T)))

#Set up data
model_data <- prepare_data_for_model(
  #Parameters for LHC and the values
  LHC_param_names = c("permanent_non_hosp_prop",
                      "permanent_hosp_prop",
                      "recovery_rate_non_hosp",
                      "recovery_rate_hosp",
                      "omicron_long_covid_multiplier",
                      "vaccination_impact",
                      "hospitalization_long_covid_multiplier"),
  LHC_param_values = c(subset(load_in_fit_parameters, grepl("permanent_non_hosp_prop", parameter))$fitted_value,
                       subset(load_in_fit_parameters, grepl("permanent_hosp_prop", parameter))$fitted_value,
                       1/subset(load_in_fit_parameters, grepl("recovery_rate_non_hosp", parameter))$fitted_value,
                       1/subset(load_in_fit_parameters, grepl("recovery_rate_hosp", parameter))$fitted_value,
                       subset(load_in_fit_parameters, grepl("omicron", parameter))$fitted_value,
                       0.353,
                       4/3),
  #Specify the number of LHC samples
  number = 10,
  #How to assign unreported cases either the same shape or unknown (into NA compartment)
  unreported_assignment = "unknown",
  #Prepare the data as a state total or by individual counties
  county_or_total = "county")

#Run model
model <- odin("odin/long_covid_model_stochastic_county.R")

model_results <- run_odin_model(model_data = model_data,
                                adult_population = 5994805)

#Average results across runs - this may take a while (>20mins)
year_week_average <- model_results %>%
  dplyr::select(county, age_group, sex, race, vaccination, timestep, into_long_covid, all_long_inc_perm) %>%
  dplyr::group_by(county, age_group, sex, race, vaccination, timestep) %>%
  dplyr::summarise(across(.cols = into_long_covid:all_long_inc_perm, 
                          .fns = list(mid = ~median(., na.rm = T),
                                      low = ~quantile(., 0.025, na.rm = T),
                                      high = ~quantile(., 0.975, na.rm = T)),
                          .names = "{.fn}_{.col}"))

export(year_week_average,
       here("data", "processed", "model_predictions", paste0(if(model_data$counties == 1) "total" else "county", "_model_run_", gsub("-", "", Sys.Date()), ".csv")))
