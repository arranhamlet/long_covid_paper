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
               stringi,    #for random strings
               devtools,   #used to install packages 
               tsibble,    #used for yearweek
               optimr,     #for fitting
               lhs,         #used to carry out latin hypercube sampling
               gridExtra,
               tidyverse  #a variety of packages for manipulating data 
               
)  

#Load functions
invisible(sapply(list.files("R/functions/", full.names = T, recursive = T), function(x) source(x)))

#Data process
model_setup <- prepare_data_for_model(
  #How to assign unreported cases
  unreported_assignment = "unknown",
  #Prepare the data as a state total or by individual counties
  county_or_total = "total")

#Set up system
model <- odin("odin/long_covid_model_stochastic_county.R")

#Set up benchmark data
benchmark_data <- model_setup$full_pulse_data %>%
  subset(state == "Washington" & 
           indicator == "Currently experiencing long COVID, as a percentage of all adults") %>%
  group_by(time_period) %>%
  mutate(pulse = paste0("Survey ", time_period),
         middle_date = mean(c(mdy(time_period_end_date), mdy(time_period_start_date))),
         yearmonth = yearmonth(middle_date))

#Plot of benchmark data
ggplot(data = benchmark_data, 
       aes(x = middle_date, 
           y = value, 
           ymin = low_ci, 
           ymax = high_ci)) +
  geom_point() +
  geom_errorbar() +
  theme_bw() +
  labs(x = "",
       y = "Population prevalence (%)",
       title = "Population prevalence of long COVID in the total WA adult population")

benchmark_weights <- c(1, 1, 1, 1, 1, 1, 0, 1, 1)

#Set up dataframe of what we want to fit
default_values <- import(here("data", "raw", "parameter_values", "default_values.csv"))
fit_these <- c("recovery_rate_non_hosp", 
               "recovery_rate_hosp",
               "permanent_non_hosp_prop",
               "permanent_hosp_prop",
               "omicron_long_covid_multiplier")

param_fit_df <- data.frame(
  parameters_to_fit = fit_these,
  starting_values = as.numeric(default_values[fit_these]),
  lower_limits = c(1/12, 1/24, 0.02, 0.08, 0.2),
  upper_limits = c(1/2, 1/6, 0.12, 0.24, 0.6)
)

#Run fitting
#This will create a folder named with a random combination of letters and numbers here data\processed\fit_parameters\individual_fits
#these are the model fits which contain information on the fit parameters and the sum of least squares
#you can load in the best parameter set by using the function load_best_fit(id = ) where the id is the name of the folder you want to load
#these are then fed into the function prepare_data_for_model at the next step

fitting_results <- fit_odin_model(
  odin_model = model, 
  parameters_to_fit = param_fit_df$parameters_to_fit,
  starting_values = param_fit_df$starting_values,
  lower_limits = param_fit_df$lower_limits,
  upper_limits = param_fit_df$upper_limits,
  weights = benchmark_weights
)
