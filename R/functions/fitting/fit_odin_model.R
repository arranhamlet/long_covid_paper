
fit_odin_model <- function(odin_model, 
                           prepare_data_for_model_object, 
                           parameters_to_fit,
                           starting_values,
                           lower_limits,
                           upper_limits,
                           weights = 1){
  
  #Set up inputs
  fit_these <- c(starting_values)
  names(fit_these) <- parameters_to_fit
  
  random_string <- stri_rand_strings(1, length = 6)
  
  #Create folder
  fit_folder <- here("data", "processed", "fit_parameters", "individual_fits", random_string)
  if(!dir.exists(fit_folder)) dir.create(fit_folder)
  
  #Fit model
  model_go <- optim(
    par = fit_these,
    fn = least_squares_fit,
    method  = "L-BFGS-B",
    control = list(factr = 1e12),
    lower = lower_limits,
    upper = upper_limits,
    odin_model = odin_model,
    prepare_data_for_model_object = prepare_data_for_model_object,
    benchmark_weights = weights,
    random_string = random_string
  )
  
}