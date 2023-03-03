plot_prevalence <- function(model_output, population, benchmark = F, benchmark_data = F){
  
  overall_data <- subset(model_output,
                         age_group == "18+" &
                           sex == "all" &
                           race == "all" &
                           vaccination == "all")
  
  #Benchmark section
  plot_output <- ggplot() +
    geom_line(data = overall_data,
              aes(group = 1,
                  x = as.Date(timestep),
                  y = 100 * all_long_inc_perm/population),
              color = "#2c7fb8", linewidth = 1) +
    theme_bw() +
    labs(x = "Case creation date",
         y = "Population prevalence (% of 18+ population)",
         color = "") +
    scale_y_continuous(label = comma) +
    coord_cartesian(xlim = c(ymd("2020/01/01"), Sys.Date())) +
    theme(legend.position = c(0.15, 0.6),
          legend.background = element_rect(fill = NA))
  
  if(benchmark == T){
    
    #Subset data and add pulse column
    this_benchmark <- subset(benchmark_data,
                             state == "Washington" & 
                               indicator == "Currently experiencing long COVID, as a percentage of all adults") %>%
      group_by(time_period) %>%
      mutate(across(
        .cols = ends_with("_date"),
        .fns = ymd
      )) %>%
      mutate(pulse = paste0("Survey ", time_period),
             middle_date = mean(c(time_period_end_date, time_period_start_date)))
    
    plot_output <- plot_output +
      geom_errorbar(data = this_benchmark,
                    aes(x = middle_date,
                        y = value,
                        ymin = low_ci,
                        ymax = high_ci,
                        color = "Household pulse\nsurvey data")) +
      geom_point(data = this_benchmark,
                    aes(x = middle_date,
                        y = value,
                        color = "Household pulse\nsurvey data"),
                 size = 2) +
      scale_fill_manual(name = "",
                        values = c("Household pulse\nsurvey data" = "black"),
                        aesthetics = c("color", "fill"))
      
  } 
  
  plot_output
  
}