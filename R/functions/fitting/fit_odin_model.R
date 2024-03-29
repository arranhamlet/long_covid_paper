
fit_odin_model <- function(odin_model,
                           parameter_fit_dataframe,
                           weights = 1){
  
  #Set up inputs
  fit_these <- c(parameter_fit_dataframe$starting_values)
  names(fit_these) <- parameter_fit_dataframe$parameters_to_fit
  
  #Setting a random seed and then setting the seed as 1 to generate a random string and then start fitting from the same place
  set.seed(as.numeric(Sys.time()))
  
  random_string <- paste(c(sample(c(LETTERS, letters), 4, replace = T), 
                           sample(0:9, 4, replace = T))[sample(1:8)], 
                         collapse = "")
  set.seed(1)
  
  message(paste0("Fitting ID: ", random_string))
  
  #Create folder
  fit_folder <- here("data", "processed", "fit_parameters", "individual_fits", random_string)
  if(!dir.exists(fit_folder)) dir.create(fit_folder)
  
  #Fit model
  optimr(
    par = fit_these,                                  #These are the parameters we want to vary in order to fit the model
    fn = least_squares_fit,                           #This is the function used to fit the model
    method  = "L-BFGS-B",
    lower = parameter_fit_dataframe$lower_limits,
    upper = parameter_fit_dataframe$upper_limits,
    odin_model = odin_model,
    benchmark_weights = weights,
    random_string = random_string
  )
  
}