options(scipen = 999)

#Load in packages
if(!require("pacman")) install.package("pacman")  #If pacman package doesnt exist, install it
#p_load looks to see if packages exists, and if they do loads them and if they dont, installs and loads them
pacman::p_load(odin,       #This is the package that contains the language odin which solves the ODEs
               ggplot2,    #used for plotting
               scales,     #used for plotting nice scales
               rio,        #used to import data
               here,       #used to simplify file paths
               data.table, #used to manipulate data.frames
               lubridate,  #used to clean dates
               janitor,    #used to clean data
               devtools,   #used to install packages 
               gghighlight,
               rgdal,
               ggpubr,
               lhs,
               maptools,
               viridis,
               grid,
               raster,
               tsibble,    #used for yearweek
               tidyverse  #a variety of packages for manipulating data 
               
)  

#Load functions
invisible(sapply(list.files("R/functions/", full.names = T), function(x) source(x)))

#Shapefile
WA_adm1 <- readOGR(here("data", "shapefile", "WA_County_Bndys.shp"))
WA_adm1 <- WA_adm1[order(WA_adm1$JURISDIC_1), ]
row.names(WA_adm1) <- as.character(1:nrow(WA_adm1))
WA_adm1 <- spTransform(WA_adm1, CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))

#Symptom prevalence
symptom_prevalence_raw <- import(here("data", "processed", "demographic", "long_covid_prevalence_by_group_data_processed.csv"))
symptom_prevalence <- symptom_prevalence_raw %>%
  subset(indicator == "Currently experiencing long COVID, as a percentage of all adults" &
           subgroup == "Washington") %>%
  mutate(across(contains("date"),
                .fns = mdy)) %>%
  group_by(time_period) %>%
  mutate(mean_date = mean(c(time_period_start_date,
                            time_period_end_date)))

#Cases and hospitalizations
model_data <- prepare_data_for_model(LHC_param_names = "permanent_non_hosp_prop",
                                     LHC_param_values = 1)

#Demographic data
demog_data <- import(here("data", "processed", "demographic", "demographic_data_processed.csv"))

state_demog <- subset(demog_data, year == 2020 & geography != "State Total" &
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

state_demog_single <- state_demog %>% subset(category %in% c("Male", "Female"))

state_demog_agg <- aggregate(list(population = state_demog_single[, c("value")]),
                             by = state_demog_single[, c("year", "geography")],
                             FUN = sum) %>%
  mutate(geography = paste0(geography, " County"))

#Load in model outputs
model_run <- import(here("data", "processed", "model_predictions", "county_model_run_20230224.csv"))

date_till <- yearmonth(Sys.Date()) - 1
these_dates <- yearmonth(ym(dimnames(model_data$raw_case_data)[[6]]))
here_date <- which(these_dates == date_till)

case_hosp <- data.frame(
  yearmonth = rep(these_dates[1:which(these_dates == date_till)], 4),
  type = rep(c("Symptomatic infections", "Reported cases", "Hospitalizations", "Long COVID"), 
             each = length(these_dates[1:here_date])),
  value = c(model_data$case_ascertainment$infections_symptomatic,
            apply(model_data$raw_case_data, 6, sum)[1:here_date],
            apply(model_data$hospitalizations, 6, sum)[1:here_date],
            head(subset(model_run, county == "all" & age_group == "all")$mid_into_long_covid, here_date)),
  low = c(rep(NA, here_date * 3), head(subset(model_run, county == "all" & age_group == "all")$low_into_long_covid, here_date)),
  high = c(rep(NA, here_date * 3), head(subset(model_run, county == "all" & age_group == "all")$high_into_long_covid, here_date))
) %>%
  mutate(type = factor(type, levels = c("Symptomatic infections", "Reported cases", "Hospitalizations", "Long COVID")))

long_incidence <- data.frame(
  yearmonth = subset(model_run, county == "all" & age_group == "all")$timestep,
  value = subset(model_run, county == "all" & age_group == "all")$mid_all_long_inc_perm,
  type = "Long COVID",
  value_low = subset(model_run, county == "all" & age_group == "all")$low_all_long_inc_perm,
  value_high = subset(model_run, county == "all" & age_group == "all")$high_all_long_inc_perm
)

sex_pop_agg <- state_demog %>% 
  subset(category %in% c("Male", "Female")) %>%
  group_by(category) %>%
  summarise(value = sum(value))

race_pop_agg <- state_demog %>% 
  subset(category %in% c("Two or more races + Other races, not Hispanic", "Asian alone, not Hispanic",                    
                         "Black alone, not Hispanic", "Hispanic or Latino (may be of any race)",    
                         "White alone, not Hispanic")) %>%
  group_by(category) %>%
  summarise(value = sum(value))

model_run_sex <- model_run %>%
  # filter(sex %in% c("Male", "Female")) %>%
  group_by(sex, timestep) %>%
  summarise(across(c(mid_into_long_covid:high_all_long_inc_perm), ~sum(., na.rm = T))) %>%
  left_join(sex_pop_agg, by = c("sex" = "category")) %>%
  group_by(sex)

age_pop_agg <- state_demog %>% 
  subset(category %in% c("0-4", "5-19",                                         
                         "20-34", "35-49",                                        
                         "50-64", "65+")) %>%
  group_by(category) %>%
  summarise(value = sum(value))

pop_data_raw <- import_list(here("data", "raw", "demographic", "ofm_pop_sade_state_2010_to_2020.xlsx"))[[2]]

pop_data <- pop_data_raw %>%
  clean_names() %>%
  subset(age_group != "Total" & 
           area_name == "Washington" & 
           year == 2020) %>%
  select(age_group, total) %>%
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

model_run_age <- model_run %>%
  filter(age_group %in% c("0-17", "18-29", "30-39", "40-49", "50-59", "60-69", "70-79", "80+")) %>%
  group_by(age_group, timestep) %>%
  summarise(across(c(mid_into_long_covid:high_all_long_inc_perm), ~sum(., na.rm = T))) %>%
  left_join(pop_data, by = "age_group")

model_run_race <- model_run %>%
  filter(race %in% c("Two or more races + Other races, not Hispanic", "Asian alone, not Hispanic",                    
                     "Black alone, not Hispanic", "Hispanic or Latino (may be of any race)",    
                     "White alone, not Hispanic")) %>%
  group_by(race, timestep) %>%
  summarise(across(c(mid_into_long_covid:high_all_long_inc_perm), ~sum(., na.rm = T))) %>%
  left_join(race_pop_agg, by = c("race" = "category"))

model_run_state_total <- model_run %>%
  subset(age_group == "18+")

model_run_state_total_allages <- model_run %>%
  subset(age_group == "all")

median_long <- model_run %>%
  group_by(county, timestep) %>%
  summarise(across(c(mid_into_long_covid:high_all_long_inc_perm), ~sum(., na.rm = T))) %>%
  left_join(state_demog_agg %>% dplyr::select(geography, population), by = c("county" = "geography")) %>%
  mutate(prevalence = mid_all_long_inc_perm/population) %>%
  as.data.frame()

#Overall in the state
pop_use <- subset(demog_data, year == 2020 & 
                    geography == "State Total" & 
                    data_type == "Max. Total Population" & 
                    category == "Male")$value
over_18_pop <- ((1 - 0.217) * pop_use)

#Cases and hospitalizations
case_hosp_graph <- ggplot(data = case_hosp,
                          aes(x = as.Date(yearmonth) + 30,
                              y = value,
                              group = type,
                              # fill = type,
                              ymin = low,
                              ymax = high,
                              color = type)) +
  geom_line(alpha = 0.75,
            linewidth = .75) +
  geom_ribbon(aes(fill = type), alpha = 0.25) +
  theme_bw() +
  labs(x = "",
       y = "Monthly incidence",
       color = "") +
  scale_y_continuous(labels = comma) +
  theme(legend.background = element_rect(fill = NA),
        legend.position = "none",
        plot.margin = unit(c(.25, .25, -.5, .25), "cm")) +
  scale_fill_manual(name = "",
                    values = c("Symptomatic infections" = "#fee090",
                               "Reported cases" = "#fc8d59",
                               "Hospitalizations" = "#d73027",
                               "Long COVID" = "#1f78b4"),
                    aesthetics = c("color", "fill")) +
  facet_wrap(~type, nrow = 1, scales = "free_y") +
  scale_x_date(limits = c(ymd("2020/01/01"),
                          floor_date(Sys.Date(), "month")))

#State 
state_total_long_COVID <- ggplot() +
  #18+
  geom_line(data = subset(model_run_state_total, 
                          timestep %in% as.character(these_dates[1:here_date])),
            aes(x = as.Date(ym(timestep)) + 30,
                y = 100 * mid_all_long_inc_perm/over_18_pop,
                color = "Model estimates (18+)"), aesthetic  = 1) +
  geom_ribbon(data = subset(model_run_state_total, 
                            timestep %in% as.character(these_dates[1:here_date])),
              aes(x = as.Date(ym(timestep)) + 30,
                  ymin = 100 * low_all_long_inc_perm/over_18_pop,
                  ymax = 100 * high_all_long_inc_perm/over_18_pop, 
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
                 color = "Household pulse estimates (18+)")) +
  geom_errorbar(data = symptom_prevalence,
                aes(x = mean_date,
                    y = value,
                    ymin = low_ci,
                    ymax = high_ci,
                    color = "Household pulse estimates (18+)")) +
  scale_fill_manual(name = "",
                    values = c("Model estimates (18+)" = "#1f78b4",
                               "Household pulse estimates (18+)" = "black",
                               "Model estimates (all)" = "#a6cee3"),
                    aesthetics = c("color", "fill")) +
  scale_x_date(limits = c(ymd("2020/01/01"),
                          floor_date(Sys.Date(), "month")))

input_prevalence_plot <- ggarrange(case_hosp_graph, 
                                   state_total_long_COVID, 
                                   ncol = 1, 
                                   heights = c(1, 1.25),
                                   align = "v", 
                                   labels = c("A", "B"))

input_prevalence_plot

ggsave("figs/input_state_prevalence.png", 
       input_prevalence_plot,
       height = 5, width = 8)

#By age
model_run_age <- model_run_age %>%
  mutate(plot_together = case_when(
    age_group %in% c("0-17",
                     "18-29",
                     "30-39",
                     "40-49") ~ "Under 50",
    age_group %in% c("50-59",
                     "60-69",
                     "70-79",
                     "80+") ~ "Over 50"
  ),
  plot_together = factor(plot_together, 
                         levels = c("Under 50",
                                    "Over 50")))

end_age_prevalence <- subset(model_run_age, timestep == max(timestep)) %>%
  mutate(end_prev = mid_all_long_inc_perm/total) %>%
  pull(end_prev) %>%
  median()

#By sex
final_prev <- median(subset(model_run_sex, timestep == max(timestep))$mid_all_long_inc_perm/subset(model_run_sex, timestep == max(timestep))$value, na.rm = T)

sex_prevalence <- ggplot(data = subset(model_run_sex, timestep == max(timestep) & sex %in% c("Female", "Male")), 
                         aes(x = sex,
                             y = mid_all_long_inc_perm/value/final_prev * 100,
                             ymin = low_all_long_inc_perm/value/final_prev * 100,
                             ymax = high_all_long_inc_perm/value/final_prev * 100,
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



age_prevalence <- ggplot(data = subset(model_run_age, timestep == max(timestep)), 
                         aes(x = age_group,
                             y = mid_all_long_inc_perm/total/end_age_prevalence * 100,
                             ymin = low_all_long_inc_perm/total/end_age_prevalence * 100,
                             ymax = high_all_long_inc_perm/total/end_age_prevalence * 100,
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
model_run_race <- model_run_race %>%
  mutate(race_small = gsub(" alone, not Hispanic| \\(may be of any race)|, not Hispanic",
                           "",
                           gsub(" \\+", "\n+", race)))

end_race_prevalence <- subset(model_run_race, timestep == max(timestep)) %>%
  mutate(end_prev = mid_all_long_inc_perm/value) %>%
  pull(end_prev) %>%
  median()

race_prevalence <- ggplot(data = subset(model_run_race, timestep == max(timestep)), 
                          aes(x = race_small,
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

combo_sex_age_race <- ggarrange(ggarrange(sex_prevalence, age_prevalence, labels = c("A", "B")),
                                race_prevalence, labels = c("", "C"), ncol = 1, heights = c(1, 1))

ggsave("figs/combo_sex_age_race_single_timepoint.jpg", combo_sex_age_race, width = 11, height = 7)


#Do a map now
#Fortify shape
state_demog$id <- as.character(1:nrow(state_demog))
last_timestep <- subset(median_long, county != "all" & county != "NA" & timestep == max(timestep))
last_timestep$id <- as.character(1:nrow(last_timestep))
last_timestep <- last_timestep  %>%
  dplyr::select(county, timestep, prevalence, id, mid_all_long_inc_perm, population)

WA_adm1_fort <- fortify(WA_adm1)
WA_adm1_ggplot <- WA_adm1_fort %>%
  left_join(last_timestep, by = "id")

#Map plot
set_up_equal_colours <- range(WA_adm1_ggplot$prevalence/median(WA_adm1_ggplot$prevalence))

county_prevalence <- ggplot() +
  geom_polygon(data = subset(WA_adm1_ggplot, county != "all" & timestep == max(timestep)),
               aes(x = long,
                   y = lat,
                   group = group,
                   fill = 100 * prevalence/median(prevalence)),
               color = "black") +
  theme_void() +
  labs(fill = "Relative prevalence (%)",
       title = "")  +
  scale_fill_distiller(direction = -1, palette = "RdBu",
                       limits = c(100 - max(abs(1 - set_up_equal_colours)) * 100, 
                                  100 + max(abs(1 - set_up_equal_colours)) * 100)) +
  theme(legend.position = "bottom",
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 12),
        legend.key.width = unit(2, "cm"),
        legend.title.align = 0.5) +
  guides(fill = guide_colourbar(title.position = "top"))

ggsave("figs/map_relative_long_COVID.jpg", county_prevalence, height = 5, width = 7.5, dpi = 450)  

#Heatmap relative over time
state_coordinates <- data.frame(county = paste0(WA_adm1$JURISDIC_1, " County"), 
                                coordinates(WA_adm1)) %>%
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

max_legend <- paste(100 * round(quantile(state_coordinates$relative_prevalence, .95), 2), "-", 
                    100 * round(max(state_coordinates$relative_prevalence[is.finite(state_coordinates$relative_prevalence)]), 1))


county_heatmap <- ggplot(data = subset(state_coordinates, county != "NA" & county != "all"),
                         aes(x = (timestep),
                             y = county,
                             fill = relative_normalised)) +
  geom_tile() +
  theme_bw() +
  labs(x = "",
       y = "County (ordered West to East)",
       fill = "Relative prevalence (%)") +
  theme(axis.text.x = element_text(angle = 90),
        legend.position = "bottom",
        legend.key.width = unit(2, "cm")) +
          scale_fill_viridis(option = "C",
                             limit = c(0, 1),
                             na.value = "#fde725")

map_heatmap_combo <- ggarrange(county_prevalence,
          county_heatmap,
          labels = c("A", "B"),
          widths = c(1, 1.5), ncol = 2)

ggsave("figs/map_heatmap_combo.jpg", map_heatmap_combo, height = 6, width = 11, dpi = 450)  


scatterplot_point <- ggplot(data = subset(state_coordinates, 
                     county != "NA" & 
                       county != "all" & 
                       timestep == last(levels(state_coordinates$timestep))),
       mapping = aes(y = county, x = relative_normalised)) + 
  geom_point() +
  theme_bw() +
  labs(x = "Longitude",
       y = "Prevalence normalised (0-1)")

grid.arrange(county_heatmap,
             scatterplot_point, ncol = 2, nrow = 1, widths=  c(4, 1))



output <- pretty_format_model_breakdown(model_run_all = model_run,
                              pulse_data = symptom_prevalence_raw,
                              pop_data,
                              sex_pop_agg,
                              race_pop_agg,
                              cost_per_disabled = 15068)


ggplot(data = subset(state_coordinates, county != "NA" & county != "all" & timestep == last(timestep)), 
       aes(x = round(X1, 0), y = round(X2, 0), fill = relative_normalised, group = county)) + 
  geom_tile()




