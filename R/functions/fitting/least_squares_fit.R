
least_squares_fit <- function(par, 
                              odin_model,
                              random_string,
                              benchmark_weights = 1, 
                              plot = T, 
                              ...){
  
  #Prepare data for model setting number to 1 and sd_variation to 0 as we are not using LHC sampling during fitting
  prepare_data_for_model_object <- prepare_data_for_model(
    LHC_param_names = names(par),
    LHC_param_values = as.numeric(par),
    number = 1,
    sd_variation = 0
  )
  
  #Run the odin model
  over_18_pop <- 7656200 * (1 - 0.217)
  
  cleaned_model_results <- run_odin_model(
    model_data = prepare_data_for_model_object,
    adult_population = over_18_pop,
    return = "18+",
    plot = F,
    print_progress = F) %>%
    mutate(prev = 100 * all_long_inc_perm/over_18_pop)
  
  #Set up dataframe
  benchmark_data <- subset(prepare_data_for_model_object$full_pulse_data,
                           state == "Washington" &
                           indicator == "Currently experiencing long COVID, as a percentage of all adults") %>%
    mutate(across(
      .cols = ends_with("_date"),
           .fns = mdy)) %>%
    mutate(yearmonth = yearmonth(median(c(time_period_start_date,
                                          time_period_end_date))))
  
  data_prediction <- data.frame(data = benchmark_data$value,
                                prediction = subset(cleaned_model_results,
                                                    as.character(timestep) %in% as.character(benchmark_data$yearmonth))$prev) %>%
    mutate(difference = data - prediction)
  
  sum_least_squares <- sum(data_prediction$difference^2 * benchmark_weights)
  
  parameter_table <- data.frame(Parameter = names(par),
                                Value = round(par, 5))
  
  #Plot
  print(plot_prevalence(model_output = cleaned_model_results,
                        benchmark = T,
                        benchmark_data = benchmark_data,
                        population = over_18_pop) +
          labs(subtitle = paste0("Sum of Least Squares: ", round(sum_least_squares, 2))) +
          theme(legend.position = "none") +
          annotation_custom(tableGrob(parameter_table, rows = NULL),
                            xmin = ymd("2020/06/01"),
                            xmax = ymd("2021/03/01"),
                            ymin = 5, 
                            ymax = 6))
  
  #Save data
  save_fit <- gather(prepare_data_for_model_object$latin_hypercube) %>% 
    mutate(date_time = Sys.time(),
           sum_least_squares = sum_least_squares,
           weights = paste(benchmark_weights, collapse = ";"),
           final_longcovid_number = subset(cleaned_model_results, timestep == max(timestep))$all_long_inc_perm) %>%
    left_join(gather(default_values, value = "default_values"), by = "key") %>%
    select(date_time, weights, key, default_values, value, sum_least_squares, final_longcovid_number)
  
  #Export the fit 
  write.csv(save_fit,
            here("data", "processed", "fit_parameters", "individual_fits", random_string,
                 paste0("individual_fit_",
                        sum_least_squares,
                        "_",
                        stri_rand_strings(1, length = 6),
                        ".csv")),
            row.names = FALSE)
  
  #Report least squares rounded - improving by more than 3 decimal places has no real effect on the output
  round(sum_least_squares, 3)
  
}
