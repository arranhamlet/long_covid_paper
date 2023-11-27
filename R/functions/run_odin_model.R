
run_odin_model <- function(model_data, adult_population, return = "all", plot = T, print_progress = T){
  
  #Loop through all runs of the latin hypercube
  first_time <- Sys.time()
  
  rbindlist(sapply(1:model_data$number, function(a){
    set.seed(a)
    
    #Set up vaccination
    model_data$age_sex_race_vaccination_array <- sweep(model_data$age_sex_race_vaccination_array, 
                                                       4, 
                                                       c(model_data$latin_hypercube$vaccination_impact_full[a], 
                                                         median(c(model_data$latin_hypercube$vaccination_impact_full[a], 1)), 
                                                         model_data$latin_hypercube$vaccination_impact_partial[a], 1), "*")
    
    #Run model
    model_generator <- model$new(
      #Set up the dimensions of the model
      time_length = model_data$num_timepoints,
      bd = model_data$bd,
      age_group_number = model_data$num_age_groups,
      sex_number = model_data$num_sex,
      race_ethnicity_number = model_data$num_race_groups,
      vaccination_status = model_data$num_vaccine_groups,
      num_counties = model_data$num_counties,
      #Feed in the infection/cases/hospitalizations
      infection = model_data$infections,
      cases = model_data$cases,
      hospitalization = model_data$hospitalizations,
      #Additional mortality from experiencing long covid
      non_hosp_additional_mortality_long = model_data$latin_hypercube$non_hosp_additional_mortality_long[a],
      hosp_additional_mortality_long = model_data$latin_hypercube$hosp_additional_mortality_long[a],
      #The probability of developing long COVID, taken from the household pulse survey
      infection_long_non_hosp = model_data$age_sex_race_vaccination_array * model_data$latin_hypercube$infection_longprob_multiplier[a],
      cases_long_non_hosp = model_data$age_sex_race_vaccination_array,
      hosp_long_hosp = model_data$age_sex_race_vaccination_array * model_data$latin_hypercube$hosp_longprob_multiplier[a],
      #The probability of developing "permanent" long COVID
      permanent_non_hosp_prop = model_data$latin_hypercube$permanent_non_hosp_prop[a],
      permanent_hosp_prop = model_data$latin_hypercube$permanent_hosp_prop[a],
      #The rate at which individuals recover from long COVID 
      recovery_rate_non_hosp = model_data$latin_hypercube$recovery_rate_non_hosp[a],
      recovery_rate_hosp_multiplier = model_data$latin_hypercube$recovery_rate_hosp_multiplier[a],
      #The time when omicron lineages became dominant
      time_omicron_switch = model_data$time_omicron_switch,
      #The change in the probability of developing long COVID with omicron dominance
      omicron_long_covid_multiplier = model_data$latin_hypercube$omicron_long_covid_multiplier[a],
      #Delay of infections/cases/hospitalizations to long COVID - 3 months as the household pulse survey
      case_delay = 3)
    
    #Run the model
    model_ran <- as.data.frame(model_generator$run(1:model_data$num_timepoints))
    
    #Clean model output
    cleaned_model_results <- clean_model_results(raw_case_data = model_data$cases,
                                                 model_results = model_ran,
                                                 time_unit = "yearmonth",
                                                 county_or_total = if(model_data$num_counties == 1) "total" else "county",
                                                 return = return)
    
    #Plot results
    if(plot == T){
      print(plot_prevalence(model_output = cleaned_model_results,
                            population = adult_population,
                            benchmark = T,
                            benchmark_data = model_data$full_pulse_data))
    }
    
    time_inside_2 <- Sys.time()
    if(print_progress == T) message(paste0("Run ", a, " of ", model_data$number, " time elapsed ", round(difftime(time_inside_2, first_time, units = "mins"), 2)))
    
    #Output
    cleaned_model_results
    
  }, simplify = FALSE))
  
}