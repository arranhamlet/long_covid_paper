
least_squares_fit <- function(par, odin_model, prepare_data_for_model_object, benchmark_data,
                           benchmark_weights = 1, ...){
  
  #Parameters that can be possibly fit - including default values
  parameters_that_can_fit <- data.frame(
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
  
  #Replace values that we want to fit
  for(i in names(par)){
    parameters_that_can_fit[i] <- par[i]
  }
  
  #Set up vaccination
  prepare_data_for_model_object$age_sex_race_vaccination_array <- sweep(prepare_data_for_model_object$age_sex_race_vaccination_array, 4, c(parameters_that_can_fit$vaccination_impact_full, median(c(parameters_that_can_fit$vaccination_impact_full, 1)), parameters_that_can_fit$vaccination_impact_partial, 1), "*")
  
  #Run model with our inputs
  model_generator <- odin_model$new(time_length = prepare_data_for_model_object$num_timepoints,
                               bd = prepare_data_for_model_object$bd,
                               age_group_number = prepare_data_for_model_object$num_age_groups,
                               sex_number = prepare_data_for_model_object$num_sex,
                               race_ethnicity_number = prepare_data_for_model_object$num_race_groups,
                               vaccination_status = prepare_data_for_model_object$num_vaccine_groups,
                               num_counties = prepare_data_for_model_object$num_counties,
                               
                               infection = prepare_data_for_model_object$infections,
                               cases = prepare_data_for_model_object$cases,
                               hospitalization = prepare_data_for_model_object$hospitalizations,
                               
                               non_hosp_additional_mortality_long = parameters_that_can_fit$non_hosp_additional_mortality_long,
                               hosp_additional_mortality_long = parameters_that_can_fit$hosp_additional_mortality_long,
                               
                               infection_long_non_hosp = prepare_data_for_model_object$age_sex_race_vaccination_array * parameters_that_can_fit$non_hosp_additional_mortality_long,
                               cases_long_non_hosp = prepare_data_for_model_object$age_sex_race_vaccination_array,
                               hosp_long_hosp = prepare_data_for_model_object$age_sex_race_vaccination_array * parameters_that_can_fit$hosp_additional_mortality_long,
                               
                               time_omicron_switch = prepare_data_for_model_object$time_omicron_switch,
                               case_delay = 3,
                               
                               #Fit these
                               permanent_non_hosp_prop = parameters_that_can_fit$permanent_non_hosp_prop,
                               permanent_hosp_prop = parameters_that_can_fit$permanent_hosp_prop,
                               recovery_rate_non_hosp = parameters_that_can_fit$recovery_rate_non_hosp,
                               recovery_rate_hosp = parameters_that_can_fit$recovery_rate_hosp,
                               omicron_long_covid_multiplier = parameters_that_can_fit$omicron_long_covid_multiplier
                               )
  
  model_ran <- as.data.frame(model_generator$run(1:prepare_data_for_model_object$num_timepoints))
  
  #Clean model output
  over_18_pop <- ((1 - 0.217) * 7656200)
  
  cleaned_model_results <- clean_model_results(raw_case_data = prepare_data_for_model_object$cases,
                                               model_results = model_ran,
                                               time_unit = paste0("year", "month"),
                                               county_or_total = if(prepare_data_for_model_object$num_counties != 1) "county" else "total") %>%
    subset(age_group == "18+") %>%
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
  
  sum_least_squares
  
}
