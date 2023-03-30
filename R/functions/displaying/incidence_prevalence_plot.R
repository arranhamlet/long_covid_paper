
incidence_prevalence_plot <- function(model_run, model_data, adult_population){
  
  date_till <- yearmonth(Sys.Date()) - 1
  these_dates <- yearmonth(ym(dimnames(model_data$raw_case_data)[[6]]))
  here_date <- which(these_dates == date_till)
  
  #Create a dataframe
  case_hosp <- data.frame(
    yearmonth = rep(these_dates[1:which(these_dates == date_till)], 4),
    type = rep(c("Symptomatic Infections", "Reported Cases", "Hospitalizations", "Long COVID"), 
               each = length(these_dates[1:here_date])),
    value = c(model_data$case_ascertainment$infections_symptomatic,
              apply(model_data$raw_case_data, 6, sum)[1:here_date],
              apply(model_data$hospitalizations, 6, sum)[1:here_date],
              head(subset(model_run, county == "all" & age_group == "all")$mid_into_long_covid, here_date)),
    low = c(c(model_data$case_ascertainment$infections_symptomatic,
              apply(model_data$raw_case_data, 6, sum)[1:here_date],
              apply(model_data$hospitalizations, 6, sum)[1:here_date]), head(subset(model_run, county == "all" & age_group == "all")$low_into_long_covid, here_date)),
    high = c(c(model_data$case_ascertainment$infections_symptomatic,
               apply(model_data$raw_case_data, 6, sum)[1:here_date],
               apply(model_data$hospitalizations, 6, sum)[1:here_date]), head(subset(model_run, county == "all" & age_group == "all")$high_into_long_covid, here_date))
  ) %>%
    mutate(type = factor(type, levels = c("Symptomatic Infections", "Reported Cases", "Hospitalizations", "Long COVID")))
  
  
  #Cases and hospitalizations
  case_hosp_graph <- ggplot(data = case_hosp,
                            aes(x = as.Date(yearmonth) + 30,
                                y = value/10000,
                                group = type,
                                color = type)) +
    geom_line(alpha = 0.75,
              linewidth = .75) +
    geom_ribbon(aes(fill = type,
                    ymin = low/10000,
                    ymax = high/10000), alpha = 0.25) +
    theme_bw() +
    labs(x = "",
         y = "Monthly incidence (per 10,000)",
         color = "") +
    scale_y_continuous(labels = comma) +
    theme(legend.background = element_rect(fill = NA),
          legend.position = "none",
          plot.margin = unit(c(.25, .25, -.5, .25), "cm")) +
    scale_fill_manual(name = "",
                      values = c("Symptomatic Infections" = "#fee090",
                                 "Reported Cases" = "#fc8d59",
                                 "Hospitalizations" = "#d73027",
                                 "Long COVID" = "#1f78b4"),
                      aesthetics = c("color", "fill")) +
    facet_wrap(~type, nrow = 1, scales = "free_y") +
    scale_x_date(limits = c(ymd("2020/01/01"),
                            ceiling_date(Sys.Date(), "month")))
  
  #State 
  state_total_long_COVID <- ggplot() +
    #18+
    geom_line(data = subset(subset(model_run, age_group == "18+"), 
                            timestep %in% as.character(these_dates[1:here_date])),
              aes(x = as.Date(ym(timestep)) + 30,
                  y = 100 * mid_all_long_inc_perm/adult_population,
                  color = "Model estimates (18+)")) +
    geom_ribbon(data = subset(subset(model_run, age_group == "18+"), 
                              timestep %in% as.character(these_dates[1:here_date])),
                aes(x = as.Date(ym(timestep)) + 30,
                    ymin = 100 * low_all_long_inc_perm/adult_population,
                    ymax = 100 * high_all_long_inc_perm/adult_population, 
                    fill = "Model estimates (18+)"),
                alpha = 0.45) +
    theme_bw() +
    labs(x = "",
         y = "Prevalence (%)",
         title = "",
         color = "") +
    scale_y_continuous(label = comma) +
    coord_cartesian(xlim = c(ymd("2020/01/01"),
                             Sys.Date()-30)) +
    theme(legend.position = c(.25, 0.6),
          legend.background = element_rect(fill = NA),
          plot.margin = unit(c(-.5, .25, 0, .25), "cm")) + 
    geom_point(data = symptom_prevalence,
               aes(x = mean_date,
                   y = value,
                   color = "Household pulse survey estimates (18+)")) +
    geom_errorbar(data = symptom_prevalence,
                  aes(x = mean_date,
                      y = value,
                      ymin = low_ci,
                      ymax = high_ci,
                      color = "Household pulse survey estimates (18+)")) +
    scale_fill_manual(name = "",
                      values = c("Model estimates (18+)" = "#1f78b4",
                                 "Household pulse survey estimates (18+)" = "black",
                                 "Model estimates (all)" = "#a6cee3"),
                      aesthetics = c("color", "fill")) +
    scale_x_date(limits = c(ymd("2020/01/01"),
                            ceiling_date(Sys.Date(), "month")))
  
  input_prevalence_plot <- ggarrange(case_hosp_graph, 
                                     state_total_long_COVID, 
                                     ncol = 1, 
                                     heights = c(1, 1.25),
                                     labels = c("A", "B"))
  
  print(input_prevalence_plot)
  
  input_prevalence_plot
  
}