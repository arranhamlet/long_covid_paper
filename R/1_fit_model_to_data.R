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
               lhs,        #used to carry out latin hypercube sampling
               gridExtra,  #used for displaying tables on figures
               tidyverse   #a variety of packages for manipulating data 
               
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

#Set up dataframe of what we want to fit
default_values <- import(here("data", "raw", "parameter_values", "default_values.csv"))
fit_these <- c("recovery_rate_non_hosp", 
               "recovery_rate_hosp",
               "permanent_non_hosp_prop",
               "permanent_hosp_prop",
               "omicron_long_covid_multiplier")

#This specifies the names, starting values and lower and upper limits of the values
#the fitting process will only choose values between the lower and upper limit
param_fit_df <- data.frame(
  parameters_to_fit = fit_these,                            #The parameters we want to fit
  starting_values = as.numeric(default_values[fit_these]),  #The starting values
  lower_limits = c(1/12, 1/24, 0.02, 0.08, 0.2),            #The lower limit the value could be
  upper_limits = c(1/2, 1/6, 0.12, 0.24, 0.6)               #The upper limit 
)

#Run fitting
#This will create a folder named with a random combination of letters and numbers here data\processed\fit_parameters\individual_fits
#these are the model fits which contain information on the fit parameters and the sum of least squares
#you can load in the best parameter set by using the function load_best_fit(id = ) where the id is the name of the folder you want to load
#these are then fed into the function prepare_data_for_model at the next step
fitting_results <- fit_odin_model(
  odin_model = model,                       #The odin model
  parameter_fit_dataframe = param_fit_df,   #The dataframe of what to fit and the range of values to try
  weights = 1                               #You can either specify 1 (all the same) or specify different weights for each household pulse survey for the fitting process
)
