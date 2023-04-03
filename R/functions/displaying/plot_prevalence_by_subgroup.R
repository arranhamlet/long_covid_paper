
plot_prevalence_by_subgroup <- function(subgroup_data){
  
  #Select the final prevalence of the end point
  final_median_sex_prev <- subset(subgroup_data, aggregate_type == "sex" & timestep == max(timestep) & sex %in% c("Female", "Male")) %>%
    ungroup() %>%
    mutate(final_prev = median(mid_all_long_inc_perm/value))
  
  #Set up figure for relative prevalence by sex
  sex_prevalence <- ggplot(data = subset(subgroup_data, aggregate_type == "sex" & timestep == max(timestep) & sex %in% c("Female", "Male")), 
                           aes(x = sex,
                               y = mid_all_long_inc_perm/value/unique(final_median_sex_prev$final_prev) * 100,
                               ymin = low_all_long_inc_perm/value/unique(final_median_sex_prev$final_prev) * 100,
                               ymax = high_all_long_inc_perm/value/unique(final_median_sex_prev$final_prev) * 100,
                               fill = sex,
                               group = sex)) +
    geom_point(size = 3) +
    geom_errorbar() + 
    theme_bw() +
    labs(x = "",
         y = "Relative prevalence (%)",
         fill = "",
         color = "") +
    theme(legend.position = "none",
          axis.text = element_text(size = 12),
          axis.title = element_text(size = 16)) +
    geom_hline(yintercept = 100,
               linetype = "dashed",
               color = "red")
  
  #Set up the final median age prevalence 
  final_median_age_prev <- subset(subgroup_data, aggregate_type == "age_group" & timestep == max(timestep)) %>%
    ungroup() %>%
    mutate(final_prev = median(mid_all_long_inc_perm/total))
  
  #Set up figure for relative prevalence by age
  age_prevalence <- ggplot(data = subset(subgroup_data, aggregate_type == "age_group" & timestep == max(timestep)), 
                           aes(x = age_group,
                               y = mid_all_long_inc_perm/total/unique(final_median_age_prev$final_prev) * 100,
                               ymin = low_all_long_inc_perm/total/unique(final_median_age_prev$final_prev) * 100,
                               ymax = high_all_long_inc_perm/total/unique(final_median_age_prev$final_prev) * 100,
                               group = age_group)) +
    geom_point(size = 3) +
    geom_errorbar() +
    theme_bw() +
    labs(x = "",
         y = "Relative prevalence (%)",
         fill = "",
         color = "") +
    theme(legend.position = "none",
          axis.text = element_text(size = 12),
          axis.title = element_text(size = 16)) +
    geom_hline(yintercept = 100,
               linetype = "dashed",
               color = "red")
  
  #By race/ethnicity
  end_race_prevalence <- subset(subgroup_data, aggregate_type == "race_ethnicity" & timestep == max(timestep)) %>%
    mutate(end_prev = mid_all_long_inc_perm/value) %>%
    pull(end_prev) %>%
    median()
  
  #Set up figure for relative prevalence by race/ethnicity
  race_prevalence <- ggplot(data = subset(subgroup_data, aggregate_type == "race_ethnicity" & timestep == max(timestep)), 
                            aes(x = gsub(" alone, not Hispanic| \\(may be of any race)|, not Hispanic",
                                         "",
                                         gsub(" \\+", "\n+", race)),
                                y = mid_all_long_inc_perm/value/end_race_prevalence * 100,
                                ymin = low_all_long_inc_perm/value/end_race_prevalence * 100,
                                ymax = high_all_long_inc_perm/value/end_race_prevalence * 100,
                                # color = race,
                                # fill = race,
                                group = race)) +
    geom_errorbar() +
    geom_point(size = 3) +
    theme_bw() +
    labs(x = "",
         y = "Relative prevalence (%)",
         fill = "",
         color = "") +
    geom_hline(yintercept = 100,
               linetype = "dashed",
               color = "red") +
    theme(legend.position = "none",
          axis.text = element_text(size = 12),
          axis.title = element_text(size = 16))
  
  #Combine all the plots
  combo_sex_age_race <- ggarrange(ggarrange(sex_prevalence, age_prevalence, labels = c("A", "B")),
                                  race_prevalence, labels = c("", "C"), ncol = 1, heights = c(1, 1))
  
  print(combo_sex_age_race)
  
  combo_sex_age_race
  
}