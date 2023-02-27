
pretty_format_model_breakdown <- function(model_estimates,
                                          pulse_data,
                                          subgroup_prevalence,
                                          state_population,
                                          adult_population,
                                          cost_per_disabled = 15068){
  
  #Subset to final timepoint all groups
  final_timepoint_all <- subset(model_estimates, county == "all" & timestep == max(timestep))
  
  #Aggregate to age only
  model_run_age <- subgroup_prevalence %>%
    filter(age_group %in% c("0-17", "18-29", "30-39", "40-49", "50-59", "60-69", "70-79", "80+")) %>%
    group_by(age_group, timestep) %>%
    summarise(across(c(mid_into_long_covid:high_all_long_inc_perm), ~sum(., na.rm = T)))
  
  #Aggregate to sex only
  model_run_sex <- subgroup_prevalence %>%
    group_by(sex, timestep) %>%
    summarise(across(c(mid_into_long_covid:high_all_long_inc_perm), ~sum(., na.rm = T)))
  
  #Aggregate to race/ethnicity only
  model_run_race <- subgroup_prevalence %>%
    filter(race %in% c("Two or more races + Other races, not Hispanic", "Asian alone, not Hispanic",                    
                       "Black alone, not Hispanic", "Hispanic or Latino (may be of any race)",    
                       "White alone, not Hispanic")) %>%
    group_by(race, timestep) %>%
    summarise(across(c(mid_into_long_covid:high_all_long_inc_perm), ~sum(., na.rm = T)))
  
  #Subset pulse data to disability
  disability_long_COVID <- pulse_data %>%
    subset(!is.na(value) &
             time_period_start_date == max(time_period_start_date) &
             state == "Washington" &
             indicator == "Significant activity limitations from long COVID, as a percentage of adults who currently have long COVID")
  
  #Calculate the number disabled
  disabled_number <- paste0(formatC(plyr::round_any(disability_long_COVID$value/100 * subset(final_timepoint_all, age_group == "all")$mid_all_long_inc_perm, 10000), big.mark = ",", format = "d"),
                            " (95% CI ",
                            formatC(plyr::round_any(disability_long_COVID$low_ci/100 * subset(final_timepoint_all, age_group == "all")$low_all_long_inc_perm, 10000), big.mark = ",", format = "d"),
                            " - ",
                            formatC(plyr::round_any(disability_long_COVID$high_ci/100 * subset(final_timepoint_all, age_group == "all")$high_all_long_inc_perm, 10000), big.mark = ",", format = "d"),
                            ")")
  
  #Calculate the cost of disability
  disabled_cost <- paste0("$", 
                          formatC(cost_per_disabled * plyr::round_any(disability_long_COVID$value/100 * subset(final_timepoint_all, age_group == "all")$mid_all_long_inc_perm, 10000), big.mark = ",", format = "fg"),
                          " (95% CI ",
                          formatC(cost_per_disabled * plyr::round_any(disability_long_COVID$low_ci/100 * subset(final_timepoint_all, age_group == "all")$low_all_long_inc_perm, 10000), big.mark = ",", format = "fg"),
                          " - ",
                          formatC(cost_per_disabled * plyr::round_any(disability_long_COVID$high_ci/100 * subset(final_timepoint_all, age_group == "all")$high_all_long_inc_perm, 10000), big.mark = ",", format = "fg"),
                          ")")
  
  #Disabled df
  disabled_df <- data.frame(category = "Disability",
                            key = c("Number", "Cost"),
                            value = c(disabled_number, disabled_cost))
  
  #Text for end prevalences
  end_df <- data.frame(category = "All",
                       key = c("All", "18+"),
                       value = c(paste0(format(round(median(subset(final_timepoint_all, age_group == "all")$mid_all_long_inc_perm/state_population * 100), 1), nsmall = 1),
                                        "% (95% CI ",
                                        format(round(median(subset(final_timepoint_all, age_group == "all")$low_all_long_inc_perm/state_population * 100), 1), nsmall = 1),
                                        " - ",
                                        format(round(median(subset(final_timepoint_all, age_group == "all")$high_all_long_inc_perm/state_population * 100), 1), nsmall = 1),
                                        ")"),
                                 paste0(format(round(median(subset(final_timepoint_all, age_group == "18+")$mid_all_long_inc_perm/adult_population *  100), 1), nsmall = 1),
                                        "% (95% CI ",
                                        format(round(median(subset(final_timepoint_all, age_group == "18+")$low_all_long_inc_perm/adult_population * 100), 1), nsmall = 1),
                                        " - ",
                                        format(round(median(subset(final_timepoint_all, age_group == "18+")$high_all_long_inc_perm/adult_population *  100), 1), nsmall = 1),
                                        ")"))
  )
  
  #Sex breakdown end prevalence
  sex_end <- as.data.frame(subset(subgroup_prevalence, timestep == max(timestep))) %>%
    subset(sex %in% c("Female", "Male"))
  
  sex_df <- data.frame(
    category = "Sex",
    key = sex_end$sex,
    value = paste0(gsub(" ", "", format(round(sex_end$mid_all_long_inc_perm/sex_end$value/median(sex_end$mid_all_long_inc_perm/sex_end$value, na.rm = T) * 100, 1), nsmall = 1)),
         "% (95% CI ",
         gsub(" ", "", format(round(sex_end$low_all_long_inc_perm/sex_end$value/median(sex_end$mid_all_long_inc_perm/sex_end$value, na.rm = T) * 100, 1), nsmall = 1)),
         " - ",
         gsub(" ", "", format(round(sex_end$high_all_long_inc_perm/sex_end$value/median(sex_end$mid_all_long_inc_perm/sex_end$value, na.rm = T) * 100, 1), nsmall = 1)),
         ")")
  )
  
  age_end <- as.data.frame(subset(subgroup_prevalence, aggregate_type == "age_group" & timestep == max(timestep)))
  
  age_df <- data.frame(
    category = "Age",
    key = age_end$age_group,
    value = paste0(gsub(" ", "", format(round(age_end$mid_all_long_inc_perm/age_end$total/median(age_end$mid_all_long_inc_perm/age_end$total) * 100, 1), nsmall = 1)),
                   "% (95% CI ",
                   gsub(" ", "", format(round(age_end$low_all_long_inc_perm/age_end$total/median(age_end$mid_all_long_inc_perm/age_end$total) * 100, 1), nsmall = 1)),
                   " - ",
                   gsub(" ", "", format(round(age_end$high_all_long_inc_perm/age_end$total/median(age_end$mid_all_long_inc_perm/age_end$total) * 100, 1), nsmall = 1)),
                   ")")
  )
  
  race_end <- as.data.frame(subset(subgroup_prevalence, aggregate_type == "race_ethnicity" & timestep == max(timestep)))
  
  race_df <- data.frame(
    category = "Race",
    key = race_end$race,
    value = paste0(gsub(" ", "", format(round(race_end$mid_all_long_inc_perm/race_end$value/median(race_end$mid_all_long_inc_perm/race_end$value) * 100, 1), nsmall = 1)),
         "% (95% CI ",
         gsub(" ", "", format(round(race_end$low_all_long_inc_perm/race_end$value/median(race_end$mid_all_long_inc_perm/race_end$value) * 100, 1), nsmall = 1)),
         " - ",
         gsub(" ", "", format(round(race_end$high_all_long_inc_perm/race_end$value/median(race_end$mid_all_long_inc_perm/race_end$value) * 100, 1), nsmall = 1)),
         ")", sep = "")
  )
  
  df <- rbind(end_df, age_df, sex_df, race_df, disabled_df) %>%
    mutate(last_timepoint = max(subgroup_prevalence$timestep)) %>%
    dplyr::select(last_timepoint, category, key, value)
  
  text_results <- data.frame(
    section = c("18+ prevalence and disabeled", "Sex, age, race"),
    text = c(paste0("The prevalence of long COVID rises with cumulative infections. While most of those with long COVID will recover, a substantial proportion develop longer term sequalae. Over time this has built up and currently ", subset(df, key == "18+")$value, " of adults in Washington state are estimated to be afflicted by long COVID. There is a wide spectrum of impact to an individual’s daily life with long COVID, with a minority experiencing severe disruption or disability. However due to the substantial burden of long COVID on the overall population, we estimate ", subset(df, key == "Number")$value, " Washingtonians are living with “Significant activity limitations from long COVID, as a percentage of adults who currently have long COVID”. ")),
  paste0("Estimates of long COVID prevalence show substantial heterogeneity across sub-categories. Females are estimated to be substantially more likely to have long COVID compared to males, with relative prevalence estimates of ", subset(df, key == "Female")$value, " and ", subset(df, key == "Male")$value, " at the latest time point respectively (Figure 3A). The highest prevalence of long COVID is found in those aged 18-29, 30-39 and 40-49 at ", subset(df, key == "18-29")$value, ", ", subset(df, key == "30-39")$value, " and ", subset(df, key == "40-49")$value, " respectively. Conversely, those 60-69, 70-79 and 80+ have the lowest estimated relative prevalence of long COVID with ", subset(df, key == "60-69")$value, ", ", subset(df, key == "70-79")$value, ", ", subset(df, key == "80+")$value, ". Race/ethnicity differences in estimated relative long COVID prevalence are pronounced (Figure 3C). The lowest relative prevalence is found in those identifying as Asian, ", subset(df, key == "Asian alone, not Hispanic")$value, ", and the highest in those identifying as Black, and Hispanic or Latino, ", subset(df, key == "Black alone, not Hispanic")$value, " and ", subset(df, key == "Hispanic or Latino (may be of any race)")$value, " respectively."))
  
  list(text_results,
       df)
}
