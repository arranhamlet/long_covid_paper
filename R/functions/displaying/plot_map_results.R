
plot_map_results <- function(model_estimates,
                             state_demography_data,
                             shapefile){
  
  #This turns off exess information (not warnings or messages) that dplyr outputs
  options(dplyr.summarise.inform = FALSE)
  
  #Process dataframe
  median_long <- model_estimates %>%
    group_by(county, timestep) %>%
    summarise(across(c(mid_into_long_covid:high_all_long_inc_perm), ~sum(., na.rm = T))) %>%
    mutate(county = gsub(" County", "", county)) %>%
    left_join(state_demography_data %>% 
                filter(data_type == "Max. Total Population" & category == "0-4") %>% 
                dplyr::select(geography, value), 
              by = c("county" = "geography"),
              relationship = "many-to-many") %>%
    mutate(prevalence = mid_all_long_inc_perm/value) %>%
    mutate(county = paste0(county, " County"))
  
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
              by = c("county" = "geography"),
              relationship = "many-to-many") %>%
    mutate(prevalence = long_covid/value,
           relative = prevalence/median(prevalence),
           norm_prev = (relative - min(relative))/(max(relative) - min(relative)))
  

  last_timestep$id <- as.character(1:nrow(last_timestep))
  
  #Fortify shape for ggplot2
  shapefile <- shapefile[order(shapefile$JURISDIC_1), ]
  row.names(shapefile) <- as.character(1:nrow(shapefile))
  shapefile$id <- as.character(1:nrow(shapefile))
  
  shp_fort <- fortify(shapefile)
  shp_ggplot <- shp_fort %>%
    left_join(last_timestep, by = "id")
  
  #Map plot
  county_prevalence <- ggplot() +
    geom_polygon(data = subset(shp_ggplot, county != "all"),
                 aes(x = long,
                     y = lat,
                     group = group,
                     fill = norm_prev),
                 color = "black") +
    theme_void() +
    labs(fill = "Relative prevalence (%)",
         title = "")  +
    scale_fill_distiller(direction = -1, palette = "RdBu",
                         limits = c(0, 1)) +
    theme(legend.position = "bottom",
          legend.text = element_text(size = 12),
          legend.title = element_text(size = 12),
          legend.key.width = unit(2, "cm"),
          legend.title.align = 0.5) +
    guides(fill = guide_colourbar(title.position = "top"))
  
  #Heatmap of transmission over time
  state_coordinates <- data.frame(county = paste0(shapefile$JURISDIC_1, " County"), 
                                  coordinates(shapefile)) %>%
    arrange(X1) %>%
    left_join(median_long, by = "county") %>%
    mutate(timestep = factor(timestep,
                             levels = as.character(sort(yearmonth(ym(unique(median_long$timestep)))))),
           county = gsub(" County", "", county),
           county = factor(county, levels = rev(unique(county)))) %>%
    group_by(timestep) %>%
    mutate(relative_prevalence = prevalence/median(prevalence, na.rm = T),
           relative_prevalence = ifelse(is.na(relative_prevalence), 0, relative_prevalence),
           relative_normalised = (relative_prevalence-min(relative_prevalence, na.rm = T))/(max(relative_prevalence, na.rm = T) - min(relative_prevalence, na.rm = T)))
  
  county_heatmap <- ggplot(data = subset(state_coordinates, county != "NA" & county != "all"),
                           aes(x = (timestep),
                               y = county,
                               fill = relative_normalised)) +
    geom_tile() +
    theme_bw() +
    labs(x = "",
         y = "County (ordered West to East)",
         fill = "Normalized prevalence (0-1)") +
    theme(axis.text.x = element_text(angle = 90),
          legend.position = "bottom",
          legend.key.width = unit(2, "cm")) +
    scale_fill_distiller(direction = -1, palette = "RdBu",
                         limits = c(0, 1)) +
    scale_x_discrete(labels = c("2020 Jan",
                                rep("", 11),
                                "2021 Jan",
                                rep("", 11),
                                "2022 Jan",
                                rep("", 11),
                                "2023 Jan",
                                ""))
  
  #Combine plots
  map_heatmap_plot <- ggarrange(county_prevalence, 
                                county_heatmap, 
                                widths = c(2, 1), 
                                common.legend = T,
                                labels = c("A", "B"),
                                legend = "bottom")
  
  print(map_heatmap_plot)
  
  map_heatmap_plot
  
}
