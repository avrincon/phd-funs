#' Function to extract sample windows
#'
#' @description 
#' For each sample, a subset data frame is created with all behaviours that 
#' occurred between window_start and window_end. Adds a column called "sample_ref",
#' which identifies which sample the window belongs to. Then stacks all sample 
#' windows on top of each other into one big data frame. 
#' 
#' Currently needs these columns with exact names: "at_sec_since_mdn", 
#' "focal_animal", "date"
#' 
#' @param df Data frame of focal observations
#' @param ur Vector of urine sample numbers
#' @param window_start Start of window relative to urine (in seconds)  
#' @param window_end End of window relative to urine (in seconds)
#'
#' @return Retruns a data frame of all sample windows. 
#' 
#' @export
#' @import dplyr


extract_windows <- function(df, ur, window_start, window_end){
  # create empty list to fill in with results
  all_windows <- vector("list", length(ur))
  
  # fill list of dataframes with for loop
  # each df is a subset with behaviours in each sample window for a specific sample
  for(sample in seq_len(length(ur))){
    # time in focal when sample was collected
    sample_time <- 
      df %>% filter(sample_num == ur[[sample]]) %>% pull("at_sec_since_mdn")
    
    sample_male_id <- 
      df %>%  filter(sample_num == ur[[sample]]) %>% pull("focal_animal")
    
    sample_date <- 
      df %>% filter(sample_num == ur[[sample]]) %>% pull("date")
    
    # define start and end time of peak hormone secretion in urine
    # 15 - 60 min in OT, 45 - 530 min in steroids
    sample_window_start <- sample_time - window_start 
    sample_window_end <- sample_time - window_end 
    # filter for behaviours that occured in window.
    sample_window <- df %>% 
      filter(at_sec_since_mdn >= sample_window_end &  
               at_sec_since_mdn <= sample_window_start & 
               focal_animal == sample_male_id & 
               date == sample_date)
    
    # bind "ur" entry to subsetted dataframe
    sample_window <- sample_window %>% 
      bind_rows(slice(df, which(sample_num ==ur[[sample]]))) 
    
    # create sample ref column with the sample number to ID window
    # create column with time sample was collected in sec since midnight
    # fill column with how many mins before sample was collected that a behaviour occured
    sample_window <- sample_window %>% 
      mutate(
        sample_ref = ur[[sample]],
        sample_t_sec_since_mdn = sample_time,
        time_relative_to_sample_m = (sample_t_sec_since_mdn - at_sec_since_mdn)/60)
    
    # add window for sample to df with all sample windows 
    all_windows[[sample]] <- sample_window
  } 
  # combine list of dataframes into a single dataframe
  all_windows <- bind_rows(all_windows)
} 