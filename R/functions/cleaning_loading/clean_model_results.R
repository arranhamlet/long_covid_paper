# age_group = "all"
# sex = "all"
# race = "all"
# vaccination = "all"
# timestep = "all"
# county = "all"
# column_subset = "long_non_hosp;long_hosp;permanent;into_long_covid"
# time_unit = "yearweek"

clean_model_results <- function(raw_case_data,
                                model_results,
                                county_or_total = "total", #Specify if the model run was county level
                                time_unit = "yearweek",
                                age_group = "all",
                                sex = "all",
                                race = "all",
                                vaccination = "all",
                                timestep = "all",
                                county = "all",
                                column_subset = "long_non_hosp;long_hosp;permanent;into_long_covid",
                                return = "all"){
  
  options(dplyr.summarise.inform = FALSE)
  
  #Set up names
  all_names <- dimnames(raw_case_data)
  
  #Find which naming patterns to extract
  this_age_group <- if(age_group == "all") 1:length(all_names[[1]]) else which(all_names[[1]] %in% age_group)
  this_sex <- if(sex == "all") 1:length(all_names[[2]]) else which(all_names[[2]] %in% sex)
  this_race <- if(race == "all") 1:length(all_names[[3]]) else which(all_names[[3]] %in% race)
  this_vaccination <- if(vaccination == "all") 1:length(all_names[[4]]) else which(all_names[[4]] %in% vaccination)
  this_county <- if(county == "all") 1:length(all_names[[5]]) else which(all_names[[5]] %in% county)
  this_timestep <- if(timestep == "all") 1:length(all_names[[6]]) else which(all_names[[6]] %in% timestep)
  this_naming_scheme <- apply(expand.grid(this_age_group, this_sex, this_race, this_vaccination, this_county), 1, paste, collapse = ",")
  
  #Subsetting columns further, do it
  if(column_subset != "all"){
    these_columns <- model_results[, which(grepl(paste(strsplit(column_subset, ";")[[1]], collapse = "|"), colnames(model_results)))]
    these_columns_clean <- gsub("\\[.*", "", colnames(these_columns))
    keep_these <- which(these_columns_clean %in% strsplit(column_subset, ";")[[1]])
    these_columns <- these_columns[, keep_these]
  }
  
  #Remove columns we dont use
  column_names <- do.call(rbind, strsplit(gsub("]", "", colnames(these_columns)), ",|\\["))
  column_names <- column_names[which(column_names[, 1] %in% paste(strsplit(column_subset, ";")[[1]])), ]
  
  #Use this function
  time_step_converted<- if(time_unit == "yearweek"){
    yearweek(all_names[[6]]) 
  } else if(time_unit == "yearmonth") yearmonth(ym(all_names[[6]]))
  
  #Create new easy to read dataframe
  easy_df <- as.data.frame(rbindlist(sapply(1:ncol(these_columns), function(x){
    this_name <- column_names[x, ]
    data.frame(age_group = all_names[[1]][as.numeric(this_name[2])],
               sex = all_names[[2]][as.numeric(this_name[3])],
               race = all_names[[3]][as.numeric(this_name[4])],
               vaccination = all_names[[4]][as.numeric(this_name[5])],
               county = all_names[[5]][as.numeric(this_name[6])],
               data_type = this_name[1],
               timestep = time_step_converted,
               value = these_columns[, x])
  }, simplify = FALSE)))
  
  if(return == "all"){
    overall_df <- easy_df %>%
      group_by(data_type, timestep) %>%
      summarise(value = sum(value, na.rm = TRUE)) %>%
      mutate(age_group = "all",
             sex = "all",
             race = "all",
             vaccination = "all",
             county = "all") %>%
      dplyr::select(age_group, sex, race, vaccination, county, data_type, timestep, value)
    
    adult_df <- easy_df %>%
      subset(age_group != "0-17") %>%
      group_by(data_type, timestep) %>%
      summarise(value = sum(value, na.rm = TRUE)) %>%
      mutate(age_group = "18+",
             sex = "all",
             race = "all",
             vaccination = "all",
             county = "all") %>%
      dplyr::select(age_group, sex, race, vaccination, county, data_type, timestep, value)
    
    full_df <- rbind(overall_df, adult_df, easy_df) %>%
      spread(key = data_type,
             value = value) %>%
      mutate(all_permanent = rowSums(across(contains("permanent")), na.rm = T),
             all_long = rowSums(across(contains("long_")), na.rm = T),
             all_long_inc_perm = all_permanent + all_long) %>%
      as.data.frame()
    
    full_df
    
  } else {
    
    easy_df %>%
      subset(age_group != "0-17") %>%
      group_by(data_type, timestep) %>%
      summarise(value = sum(value, na.rm = TRUE)) %>%
      mutate(age_group = "18+",
             sex = "all",
             race = "all",
             vaccination = "all",
             county = "all") %>%
      dplyr::select(age_group, sex, race, vaccination, county, data_type, timestep, value) %>%
      spread(key = data_type,
             value = value) %>%
      mutate(all_permanent = rowSums(across(contains("permanent")), na.rm = T),
             all_long = rowSums(across(contains("long_")), na.rm = T),
             all_long_inc_perm = all_permanent + all_long)
      
  }
  
}
