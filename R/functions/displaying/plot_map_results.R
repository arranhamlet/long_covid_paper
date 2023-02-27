
plot_map_results <- function(model_estimates,
                             state_demography_data,
                             shapefile){
  
  #Process dataframe
  agg_last_timepoint <- subset(model_estimates, county != "NA" & county != "all" & county != "NA" & timestep == max(timestep)) %>%
    group_by(county) %>%
    summarise(long_covid = sum(mid_all_long_inc_perm))
  
  last_timestep <-  agg_last_timepoint %>%
    mutate(county = gsub(" County", "", county)) %>%
    left_join(subset(state_demography_data, 
                     data_type == "Max. Total Population" & 
                       geography != "State Total" & 
                       year == 2020 & 
                       category == "0-4"),
              by = c("county" = "geography")) %>%
    mutate(prevalence = long_covid/value,
           relative = prevalence/median(prevalence))
  
  last_timestep$id <- as.character(1:nrow(last_timestep))
  
  #Fortify shape for ggplot2
  shapefile <- shapefile[order(shapefile$JURISDIC_1), ]
  row.names(shapefile) <- as.character(1:nrow(shapefile))
  shapefile$id <- as.character(1:nrow(shapefile))
  
  shp_fort <- fortify(shapefile)
  shp_ggplot <- shp_fort %>%
    left_join(last_timestep, by = "id")
  
  #Map plot
  set_up_equal_colours <- range(last_timestep$prevalence/median(last_timestep$prevalence))
  
  county_prevalence <- ggplot() +
    geom_polygon(data = subset(shp_ggplot, county != "all"),
                 aes(x = long,
                     y = lat,
                     group = group,
                     fill = 100 * relative),
                 color = "black") +
    theme_void() +
    labs(fill = "Relative prevalence (%)",
         title = "")  +
    scale_fill_distiller(direction = -1, palette = "RdBu",
                         limits = c(100 - max(abs(1 - set_up_equal_colours - 0.01)) * 100, 
                                    100 + max(abs(1 - set_up_equal_colours - 0.01)) * 100)) +
    theme(legend.position = "bottom",
          legend.text = element_text(size = 12),
          legend.title = element_text(size = 12),
          legend.key.width = unit(2, "cm"),
          legend.title.align = 0.5) +
    guides(fill = guide_colourbar(title.position = "top"))
  
  print(county_prevalence)
  
  county_prevalence
  
}