
load_best_fit <- function(id = NULL){
  
  #Find all fit folders
  fit_folders <- dir(here("data", "processed", "fit_parameters", "individual_fits"), 
                     full.names=TRUE)
  
  #Decide what to load
  if(is.null(id)){
    #Created time
    when_created <- file.info(fit_folders)$ctime
    #Find these files
    all_files_latest_fit <- list.files(fit_folders[which.max(when_created)],
                                       full.names = T)
  } else {
    all_files_latest_fit <- list.files(path = fit_folders[grepl(id, fit_folders)],
                                       recursive = T,
                                       full.names = T)
  }
  
  #Find the file with the best value
  load_this_file <- all_files_latest_fit %>%
    gsub(pattern = paste0(paste(fit_folders, collapse = "|"), "|/|\\\\|individual_fit_|.csv"), 
         replacement = "", 
         ~.) %>%
    gsub(pattern = "_.*",
         replacement = "",
         ~.) %>%
    as.numeric() %>%
    which.min()
  
  #Loaded
  import(all_files_latest_fit[load_this_file]) %>%
    rename(
      parameter = key,
      fitted_value = value
    ) %>%
    mutate(which_fit = fitted_value != default_values)
  
}
