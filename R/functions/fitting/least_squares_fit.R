
least_squares_fit <- function(par, odin_model, prepare_data_for_model_object,
                              benchmark_weights = 1, plot = T, random_string, ...){
  
  #Parameters that can be possibly fit - including default values
  prepare_data_for_model_object$number <- 1
  
  default_values <- data.frame(
    vaccination_impact_full = 0.647,
    vaccination_impact_partial = 0.647,
    non_hosp_additional_mortality_long = 1,
    hosp_additional_mortality_long = 1,
    infection_longprob_multiplier = 2/3,
    hosp_longprob_multiplier = 4/3,
    permanent_non_hosp_prop = 0.6,
    permanent_hosp_prop = 0.16,
    recovery_rate_non_hosp = 0.25,
    recovery_rate_hosp = 1/9,
    omicron_long_covid_multiplier = 4.5/10.8
  )
  
  prepare_data_for_model_object$latin_hypercube <- default_values
  
  #Replace values that we want to fit
  for(i in names(par)){
    prepare_data_for_model_object$latin_hypercube[i] <- par[i]
  }
  
  #Set up vaccination
  prepare_data_for_model_object$age_sex_race_vaccination_array <- sweep(prepare_data_for_model_object$age_sex_race_vaccination_array, 
                                                                        4, 
                                                                        c(prepare_data_for_model_object$latin_hypercube$vaccination_impact_full, 
                                                                          median(c(prepare_data_for_model_object$latin_hypercube$vaccination_impact_full, 1)), 
                                                                          prepare_data_for_model_object$latin_hypercube$vaccination_impact_partial, 1), "*")
  
  #Run the odin model
  over_18_pop <- 7656200 * (1 - 0.217)
  
  cleaned_model_results <- run_odin_model(
    model_data = prepare_data_for_model_object,
    adult_population = over_18_pop,
    return = "18+",
    plot = F) %>%
    mutate(prev = 100 * all_long_inc_perm/over_18_pop)
  
  #Set up dataframe
  data_prediction <- data.frame(data = benchmark_data$value,
                                prediction = subset(cleaned_model_results,
                                                    as.character(timestep) %in% as.character(benchmark_data$yearmonth))$prev) %>%
    mutate(difference = data - prediction)
  
  sum_least_squares <- sum(data_prediction$difference^2 * benchmark_weights)
  
  parameter_table <- data.frame(Parameter = names(par),
                                Value = round(par, 5))
  
  #Plot
  print(plot_prevalence(cleaned_model_results,
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
  
  write.csv(save_fit,
            here("data", "processed", "fit_parameters", "individual_fits", random_string,
                 paste0("individual_fit_",
                        sum_least_squares,
                        ".csv")),
            row.names = FALSE)
  
  #Report least squares
  sum_least_squares
  
}
