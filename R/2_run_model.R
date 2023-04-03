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
               lhs,        #used to carry out latin hypercube sampling
               tidyverse   #a variety of packages for manipulating data 
               
)  

#Load functions
invisible(sapply(list.files("R/functions/", full.names = T, recursive = T), function(x) source(x)))

#Load in best fitting parameters - This loads in the parameter set with the best (lowest) Sum of Least Squares from the fitting process
#specifiy id =  as the folder of fits you want to use
load_in_fit_parameters <- load_best_fit(id = "VAc9834l")

#We are subsetting to only the values we fit, becasue these are what we want to explore in the LHC process
LHC_these <- load_in_fit_parameters %>%
  subset(which_fit == T)

#Set up data
model_data <- prepare_data_for_model(
  #Parameters for LHC and the values - we are putting the parameters fit and also additional parameters into the LHC
  LHC_param_names = c(LHC_these$parameter,
                      "vaccination_impact_partial",
                      "vaccination_impact_full", 
                      "hosp_longprob_multiplier"),
  LHC_param_values = c(LHC_these$fitted_value,
                       1 - 0.353,
                       1 - 0.353,
                       4/3),
  #Specify the number of LHC samples and the variation of sampling
  sd_variation = .25,  #How much variation you want from the starting value, .25 would indicate values could be 25% higher or lower than the value specified in LHC_param_values                                            
  number = 25,          #How many samples you want to run for - the larger the number the more certainty
  #How we want to assign the "missing" cases we are calculating from the case ascertainment data
  unreported_assignment = "unknown",
  #Prepare the data as a state total or by individual counties
  county_or_total = "county")

#Load model
model <- odin("odin/long_covid_model_stochastic_county.R")

#Run model
model_results <- run_odin_model(model_data = model_data,
                           adult_population = ((1 - 0.217) * 7656200))

#Average results across runs - this may take a while (>20mins)
year_week_average <- model_results %>%
  dplyr::select(county, age_group, sex, race, vaccination, timestep, into_long_covid, all_long_inc_perm) %>%
  dplyr::group_by(county, age_group, sex, race, vaccination, timestep) %>%
  dplyr::summarise(across(.cols = into_long_covid:all_long_inc_perm, 
                          .fns = list(mid = ~median(., na.rm = T),
                                      low = ~quantile(., 0.025, na.rm = T),
                                      high = ~quantile(., 0.975, na.rm = T)),
                          .names = "{.fn}_{.col}"))

#Export results
write.csv(year_week_average,
       here("data", "processed", "model_estimates", paste0(if(model_data$num_counties == 1) "total" else "county", "_model_run_", gsub("-", "", Sys.Date()), ".csv")),
       row.names = FALSE)
