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
               tsibble,    #used for yearweek
               lhs,        #used to carry out latin hypercube sampling
               gridExtra,  #used for displaying tables in plots
               tidyverse   #a variety of packages for manipulating data 
               
)  

#Load functions
invisible(sapply(list.files(here("R", "functions"), full.names = T), function(x) source(x)))

#Set up data
model_data <- prepare_data_for_model(
  #How to assign unreported cases either the same shape or unknown (into NA compartment)
  unreported_assignment = "unknown",
  #Prepare the data as a state total or by individual counties
  county_or_total = "total")

#Load in the model - ignore installing devtools
model <- odin(here("odin", "long_covid_model_stochastic_county.R"))

#Set up benchmark data
benchmark_data <- model_data$full_pulse_data %>%
  subset(state == "Washington" & 
           indicator == "Currently experiencing long COVID, as a percentage of all adults") %>%
  group_by(time_period) %>%
  mutate(pulse = paste0("Survey ", time_period),
         middle_date = mean(c(mdy(time_period_end_date), mdy(time_period_start_date))),
         yearmonth = yearmonth(middle_date))

#Set up dataframe of what we want to fit
#these parameters and values are chosen based on available data from literature
#the more you include the longer it will take
param_fit_df <- data.frame(
  parameters_to_fit = c("recovery_rate_non_hosp", 
                        "recovery_rate_hosp",
                        "permanent_non_hosp_prop",
                        "permanent_hosp_prop",
                        "omicron_long_covid_multiplier",
                        "infection_longprob_multiplier"),
  starting_values = c(1/4, 1/9, 0.06, 0.16, 4.5/10.8, 0.5),
  lower_limits = c(1/12, 1/24, 0.02, 0.08, 0.2, 0.25),
  upper_limits = c(1/2, 1/6, 0.12, 0.24, 0.6, 3)
  )

#Run fitting
fitting_results <- fit_odin_model(
  odin_model = model, 
  prepare_data_for_model_object = model_data, 
  benchmark_data = benchmark_data,
  parameters_to_fit = param_fit_df$parameters_to_fit,
  starting_values = param_fit_df$starting_values,
  lower_limits = param_fit_df$lower_limits,
  upper_limits = param_fit_df$upper_limits
)

#Save outputs
export(fitting_results,
          here("data", "processed", "fit_parameters", paste0("fit_parameters_", gsub("-", "", Sys.Date()), ".csv")))
