
fit_odin_model <- function(odin_model, 
                           prepare_data_for_model_object, 
                           benchmark_data,
                           parameters_to_fit,
                           starting_values,
                           lower_limits,
                           upper_limits,
                           weights = 1){
  
  #Set up inputs
  fit_these <- c(starting_values)
  names(fit_these) <- parameters_to_fit
  
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
    benchmark_data = benchmark_data,
    benchmark_weights = weights
  )
  
  #Fitting results
  fitting_results <- data.frame(parameter = names(fit_these),
                                starting_value = fit_these,
                                lower_limit = lower_limits,
                                upper_limit = upper_limits,
                                fitted_value = as.numeric(model_go$par),
                                weight = paste(weights, collapse = ";"),
                                measure_of_fit = model_go$value,
  row.names = NULL)
  
  fitting_results
  
}