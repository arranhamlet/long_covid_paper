
prepare_subgroup_data <- function(state_demography_data,
                                    age_data,
                                    model_estimates){
  
  #Process state demog data
  state_demography_data <- subset(state_demography_data, year == 2020 & geography != "State Total" &
                          data_type == "Max. Sub-Population") %>%
    mutate(category = case_when(
      category == "AIAN Only, NH" ~ "Two or more races + Other races, not Hispanic",
      category == "Asian Only, NH" ~ "Asian alone, not Hispanic",
      category == "Black Only, NH" ~ "Black alone, not Hispanic",
      category == "Hispanic" ~ "Hispanic or Latino (may be of any race)",
      category == "Multi-Race, NH" ~ "Two or more races + Other races, not Hispanic",
      category == "NHOPI Only, NH" ~ "Two or more races + Other races, not Hispanic",
      category == "White Only, NH" ~ "White alone, not Hispanic",
      TRUE ~ category
    ))
  
  #Aggregate by sex
  sex_pop_agg <- state_demography_data %>% 
    subset(category %in% c("Male", "Female")) %>%
    group_by(category) %>%
    summarise(value = sum(value))
  
  #Aggregate by race/ethnicity
  race_pop_agg <- state_demography_data %>% 
    subset(category %in% c("Two or more races + Other races, not Hispanic", "Asian alone, not Hispanic",                    
                           "Black alone, not Hispanic", "Hispanic or Latino (may be of any race)",    
                           "White alone, not Hispanic")) %>%
    group_by(category) %>%
    summarise(value = sum(value))
  
  #Work out population data by our age groups
  pop_data <- age_data %>%
    clean_names() %>%
    subset(age_group != "Total" & 
             area_name == "Washington" & 
             year == 2020) %>%
    dplyr::select(age_group, total) %>%
    mutate(age_group = case_when(
      age_group %in% c("30-34", "35-39") ~ "30-39",
      age_group %in% c("40-44", "45-49") ~ "40-49",
      age_group %in% c("50-54", "55-59") ~ "50-59",
      age_group %in% c("60-64", "65-69") ~ "60-69",
      age_group %in% c("70-74", "75-79") ~ "70-79",
      age_group %in% c("80-84", "85+") ~ "80+",
      TRUE ~ age_group),
      total = as.numeric(total)) %>%
    group_by(age_group) %>%
    summarise(total = sum(total))
  
  one_seventeen <- sum(subset(pop_data, age_group %in% c("0-4", "5-9", "10-14"))$total, rep(subset(pop_data, age_group %in% c("15-19"))$total/5, 3))
  eighteen_twenty_nine <- sum(subset(pop_data, age_group %in% c("20-24", "25-29"))$total, rep(subset(pop_data, age_group %in% c("15-19"))$total/5, 2))
  
  pop_data <- rbind(data.frame(age_group = c("0-17",
                                             "18-29"),
                               total = c(one_seventeen, eighteen_twenty_nine)),
                    pop_data %>%
                      subset(!age_group %in% c("0-4", "5-9", "10-14", "15-19",
                                               "20-24", "25-29")))
  
  #Aggregate model estimates by subgroups
  model_estimates_sex <- model_estimates %>%
    group_by(sex, timestep) %>%
    summarise(across(c(mid_into_long_covid:high_all_long_inc_perm), ~sum(., na.rm = T))) %>%
    left_join(sex_pop_agg, by = c("sex" = "category")) %>%
    group_by(sex) %>%
    mutate(aggregate_type = "sex")
  
  model_run_age <- model_estimates %>%
    filter(age_group %in% c("0-17", "18-29", "30-39", "40-49", "50-59", "60-69", "70-79", "80+")) %>%
    group_by(age_group, timestep) %>%
    summarise(across(c(mid_into_long_covid:high_all_long_inc_perm), ~sum(., na.rm = T))) %>%
    left_join(pop_data, by = "age_group") %>%
    mutate(aggregate_type = "age_group")
  
  model_run_race <- model_estimates %>%
    filter(race %in% c("Two or more races + Other races, not Hispanic", "Asian alone, not Hispanic",                    
                       "Black alone, not Hispanic", "Hispanic or Latino (may be of any race)",    
                       "White alone, not Hispanic")) %>%
    group_by(race, timestep) %>%
    summarise(across(c(mid_into_long_covid:high_all_long_inc_perm), ~sum(., na.rm = T))) %>%
    left_join(race_pop_agg, by = c("race" = "category")) %>%
    mutate(aggregate_type = "race_ethnicity")
  
  #Export
  rbind(model_estimates_sex,
        model_run_age,
        model_run_race)
  
}
