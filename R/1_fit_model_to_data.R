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

#Set up system
model <- odin("odin/long_covid_model_stochastic_county.R")

#Specify what parameters we want to fit
fit_these <- c("recovery_rate_non_hosp", 
               "recovery_rate_hosp_multiplier",
               "permanent_non_hosp_prop",
               "permanent_hosp_prop")

#This specifies the names, starting values and lower and upper limits of the values
#the fitting process will only choose values between the lower and upper limit
param_fit_df <- data.frame(
  parameters_to_fit = fit_these,                            #The parameters we want to fit
  starting_values = c(1/3, 1.5, 0.06, 0.16),                #The starting values
  lower_limits =    c(1/6, 1, 0.04, 0.14),                  #The lower value limit of the variable you are fitting
  upper_limits =    c(2, 2, 0.08, 0.20)                     #The upper value limit of the variable you are fitting
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
