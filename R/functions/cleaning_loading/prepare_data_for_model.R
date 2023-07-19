

prepare_data_for_model <- function(LHC_param_names = NA, 
                                   LHC_param_values = NA,
                                   number = 1, #the number of runs you want to do
                                   county_or_total = "total", #Specify if you want the county level data to be loaded in
                                   sd_variation = 0.25, #The sd variation in the LHC
                                   unreported_assignment = "unknown", #Assign the unreported cases to the unknown column or keep the same shape "same_shape"
                                   remove_heterogeneity_probability = F,
                                   remove_heterogeneity_underascertainment = F
                                   ){
  
  #Import data
  #Life expectancy
  life_expectancy <- import(here("data", "processed", "demographic", "life_expectancy_by_age_sex.csv"))
  life_expectancy_matrix <- matrix(c(subset(life_expectancy, sex == "female")$weighted_life,
                                     subset(life_expectancy, sex == "male")$weighted_life),
                                   ncol = 2)
  
  life_expectancy_matrix <- rbind(life_expectancy_matrix, 
                                  t(colMeans(life_expectancy_matrix)))
  life_expectancy_matrix <- cbind(life_expectancy_matrix,
                                  matrix(rowMeans(life_expectancy_matrix)))
  
  if(county_or_total == "single_dimension"){
    life_expectancy_matrix <- matrix(sum(life_expectancy$weighted_life * life_expectancy$number/sum(life_expectancy$number)))
  }
  
  #Symptom prevalence
  symptom_prevalence_raw <- import(here("data", "processed", "demographic", "long_covid_prevalence_by_group_data_processed.csv"))  %>% 
    group_by(time_period_label) %>%
    mutate(middle_date = mean(c(mdy(time_period_start_date), mdy(time_period_end_date)))) %>%
    ungroup()
  
  symptom_prevalence <- symptom_prevalence_raw %>%
    subset(indicator == "Ever experienced long COVID, as a percentage of adults who ever had COVID")    
  
  #Case ascertainment to multiply cases by
  case_ascertainment <- readRDS(max(list.files(full.names = T,
                                               here("data", "processed", "case_hospitalization_data"), pattern = "case_ascertainment")))
  
  #Load in case and hospitalization data
  all_files <- list.files(here("data", "processed", "case_hospitalization_data"), full.names = TRUE)
  all_files <- all_files[which(grepl("all_counties", all_files))]
  actual_case_data <- readRDS(max(all_files[grepl("data/nonhosp_case", all_files)]))
  actual_hosp_data <- readRDS(max(all_files[grepl("data/hosp_case", all_files)]))
  
  #Load in actual case data
  if(county_or_total == "total"){
    
    #We want to retain the county dimenson, but with 1 entry, so we need to reconfigure the array
    remake_dim <- c(dim(actual_case_data)[1:4], 1, dim(actual_case_data)[6])
    actual_case_data_new <- array(NA, dim = remake_dim)
    dimnames(actual_case_data_new) <- c(dimnames(actual_case_data)[1:4], "total", dimnames(actual_case_data)[6])
    
    #Here we re-arrange and then sum the case data to collapse the county dimension in the array
    actual_case_data <- rowSums(aperm(actual_case_data, c(1, 2, 3, 4, 6, 5)), na.rm = T, dims = 5)
    actual_hosp_data <- rowSums(aperm(actual_hosp_data, c(1, 2, 3, 4, 6, 5)), na.rm = T, dims = 5)
    
    dim(actual_case_data) <- remake_dim
    dim(actual_hosp_data) <- remake_dim
    
    dimnames(actual_case_data) <- dimnames(actual_case_data_new)
    dimnames(actual_hosp_data) <- dimnames(actual_case_data_new)
    
  } else if(county_or_total == "single_dimension"){
    
    #We want to retain the county dimenson, but with 1 entry, so we need to reconfigure the array
    remake_dim <- c(rep(1, 5), dim(actual_case_data)[6])
    actual_case_data_new <- array(NA, dim = remake_dim)
    dimnames(actual_case_data_new) <- c(rep(list("total"), 5), dimnames(actual_case_data)[6])
    
    #Here we re-arrange and then sum the case data to collapse the county dimension in the array
    actual_case_data <- apply(actual_case_data, 6, sum)
    actual_hosp_data <- apply(actual_hosp_data, 6, sum)
    
    dim(actual_case_data) <- remake_dim
    dim(actual_hosp_data) <- remake_dim
    
    dimnames(actual_case_data) <- dimnames(actual_case_data_new)
    dimnames(actual_hosp_data) <- dimnames(actual_case_data_new)
    
  }
  
  #Set up blank infections
  test_COVID_infections <- actual_hosp_data
  test_COVID_infections[] <- 0
  
  raw_case_data <- actual_case_data
  
  if(remove_heterogeneity_underascertainment == T){
    mean_ascertainment <- weighted.mean(x = pmin(case_ascertainment$cases/case_ascertainment$infections_symptomatic, 1),
                                        w = case_ascertainment$cases/sum(case_ascertainment$cases))
    
    case_ascertainment$symptomatic_missed <- case_ascertainment$cases/mean_ascertainment
    
  }
  
  #Account for unreported infections
  for(i in 1:last(dim(actual_case_data))){
    increase_by <- case_ascertainment$symptomatic_missed[i]
    if(is.na(increase_by)) increase_by <- 0
    if(unreported_assignment == "unknown"){
      actual_case_data[which(dimnames(actual_case_data)[[1]] %in% c("NA", "all")), which(dimnames(actual_case_data)[[2]] %in% c("NA", "all")), which(dimnames(actual_case_data)[[3]] %in% c("NA", "all")), which(dimnames(actual_case_data)[[4]] %in% c("NA", "all")), length(dimnames(actual_case_data)[[5]]), i] <- actual_case_data[which(dimnames(actual_case_data)[[1]] %in% c("NA", "all")), which(dimnames(actual_case_data)[[2]] %in% c("NA", "all")), which(dimnames(actual_case_data)[[3]] %in% c("NA", "all")), which(dimnames(actual_case_data)[[4]] %in% c("NA", "all")), length(dimnames(actual_case_data)[[5]]), i] + increase_by
    } else if(unreported_assignment == "same_shape"){
      actual_case_data[, , , , , i] + actual_case_data[, , , , , i]/sum(actual_case_data[, , , , , i]) * increase_by
    }
  }

  counties <- dim(actual_case_data)[5]

  #Set up system
  num_timepoints <- last(dim(actual_case_data))
  num_age_groups <- dim(actual_case_data)[1]
  num_sex <- dim(actual_case_data)[2]
  num_race_groups <- dim(actual_case_data)[3]
  num_vaccine_groups <- dim(actual_case_data)[4]
  
  #Set up age and race matrix
  age_long_data <- filter(symptom_prevalence, time_period_end_date == max(time_period_end_date) & group == "By Age")$value/100
  sex_long_data <- rev(filter(symptom_prevalence, time_period_end_date == max(time_period_end_date) & group == "By Sex")$value/100) #Reversing to match the case data
  race_long_data <- filter(symptom_prevalence, time_period_end_date == max(time_period_end_date) & group == "By Race/Hispanic ethnicity")$value/100
  race_long_data <- c(race_long_data[4], race_long_data[3], race_long_data[1], race_long_data[5], mean(race_long_data), race_long_data[2])#add in NA data
  
  age_sex_mat <- matrix(rep(age_long_data, 
                            length(sex_long_data)),
                        ncol = length(sex_long_data))
  
  age_sex_matrix <- sweep(age_sex_mat, 2, sex_long_data/mean(sex_long_data), "*")
  
  #Add in the below 18 and unknown columns
  age_sex_matrix <- rbind(t(colMeans(age_sex_matrix)),
                          age_sex_matrix,
                          t(colMeans(age_sex_matrix)))
  
  age_sex_matrix <- cbind(age_sex_matrix,
                          matrix(rowMeans(age_sex_matrix)))
  
  #Now set it up for the dimension of race
  age_sex_race_array <- array(rep(age_sex_matrix, 5), dim = c(nrow(age_sex_matrix), 
                                                              ncol(age_sex_matrix), 
                                                              length(race_long_data), 
                                                              num_vaccine_groups))
  age_sex_race_array <- sweep(age_sex_race_array, 3, race_long_data/mean(race_long_data), "*")
  
  #Now set up for the vaccination dimension
  age_sex_race_vaccination_array <- sweep(age_sex_race_array, 4, 1, "*")
  
  #Change all parameters to the mean value - remove heterogeneity from age/sex/race/vaccination
  if(remove_heterogeneity_probability == T) age_sex_race_vaccination_array[] <-  subset(symptom_prevalence, group == "National Estimate" & time_period_start_date == max(time_period_start_date))$value/100
  
  
  if(county_or_total == "single_dimension"){
    median_value <- filter(symptom_prevalence, time_period_end_date == max(time_period_end_date) & state == "Washington") %>%
      select(value) %>%
      pull(value)/100
    
    age_sex_race_vaccination_array <- array(data = rep(median_value, 4),
          dim = c(1, 1, 1, 1))
    
    dimnames(age_sex_race_vaccination_array) <- rep(list("all"), 4)
  }
  
  #Repeat a bunch of times for differing values of probabilities
  #We are setting up a Latin Hypercube to sample from in order to efficiently 
  #sample a range of potential parameter combinations
  set.seed(1)
  
  time <- "month"
  time_adjust <- if(time == "month") 30 else if(time == "week") 7 else if(time == "day") 1
  
  #Set up LHC values for sampling
  if(!all(is.na(LHC_param_values))){
    
    sd_variation_updated <- if(length(sd_variation) == length(LHC_param_values) & length(sd_variation) != 1) sd_variation[i] else sd_variation
    
    potential_LHC_values <- sapply(1:length(LHC_param_values), function(x){
      rnorm(n = number,
            mean = LHC_param_values[x],
            sd = LHC_param_values[x] * sd_variation_updated)
    }, simplify = FALSE)
    
    if(!all(is.na(LHC_param_names))) names(potential_LHC_values) <- LHC_param_names
    
    latin_hypercube <- as.data.frame(randomLHS(length(potential_LHC_values[[1]]), length(potential_LHC_values)))
    
    for(i in 1:length(potential_LHC_values)){
      latin_hypercube[, i] <- qunif(latin_hypercube[, i], min(potential_LHC_values[[i]]), max(potential_LHC_values[[i]]))
    }
    
    colnames(latin_hypercube) <- names(potential_LHC_values)
  } else {
    latin_hypercube <- NULL
  }

  #Add in default values
  default_values <- import(here("data", "raw", "parameter_values", "default_values.csv"))
  
  if(is.null(latin_hypercube)){
    latin_hypercube <- default_values
  } else {
    latin_hypercube <- cbind(latin_hypercube,
                             default_values %>% 
                               select(which(!colnames(default_values) %in% colnames(latin_hypercube))),
                             row.names = NULL)
  }
  
  list(  num_timepoints = num_timepoints,
         num_age_groups = num_age_groups,
         num_sex = num_sex,
         num_race_groups = num_race_groups,
         num_vaccine_groups = num_vaccine_groups,
         num_counties = counties,
         raw_case_data = raw_case_data,
         infections = test_COVID_infections,
         cases = actual_case_data,
         hospitalizations = actual_hosp_data,
         age_sex_race_vaccination_array = age_sex_race_vaccination_array,
         bd = 1/(life_expectancy_matrix * if(time == "month") 12 else if(time == "week") 52 else if(time == "day") 365),
         latin_hypercube = latin_hypercube,
         time_omicron_switch = min(which(grepl("2022", last(dimnames(actual_case_data))))),
         full_pulse_data = symptom_prevalence_raw,
         case_ascertainment = case_ascertainment,
         number = number)
  
}
