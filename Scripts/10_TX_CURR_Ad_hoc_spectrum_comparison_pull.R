# PROJECT: Here it goes again
# PURPOSE: Munge and Analysis of TX_Curr by coarse age bands, sex, and by psnu
# AUTHOR: Tim Essam | SI
# REF ID:   57b86b5b
# LICENSE: MIT
# DATE: 2023-02-02
# NOTES: Tim Essam | SI

# LOCALS & SETUP ============================================================================

  # Load Q1 functions
  source("Scripts/helper-call_all_helpers.R")

  moh_data <- "Data/MoH TX_CURR File for December 2022.xls"
  
  # MOH crosswalk to DATIM bands
  moh_cw <- tibble::tribble(
        ~sex, ~ageasentered,     ~sex_age_group,
    "Female",         "50+",   "Female, (+50)y",
    "Female",       "10-14", "Female, (10-14)y",
    "Female",       "15-19", "Female, (15-19 )y",
    "Female",       "20-24", "Female, (20-24)y",
    "Female",       "25-29", "Female, (25-29)y",
    "Female",       "30-34", "Female, (30-34)y",
    "Female",       "35-49", "Female, (35-49)y",
    "Female",         "5-9",   "Female, (5-9)y",
    "Female",         "1-4",   "Female, 0-2 yrs",
    "Female",         "1-4",   "Female, 3-4 yrs",
      "Male",         "50+",     "Male, (+50)y",
      "Male",       "10-14",   "Male, (10-14)y",
      "Male",       "15-19",   "Male, (15-19 )y",
      "Male",       "20-24",   "Male, (20-24)y",
      "Male",       "25-29",   "Male, (25-29)y",
      "Male",       "30-34",   "Male, (30-34)y",
      "Male",       "35-49",   "Male, (35-49)y",
      "Male",         "5-9",     "Male, (5-9)y",
      "Male",         "1-4",     "Male, 0-2 yrs",
      "Male",         "1-4",     "Male, 3-4 yrs"
    )

  
  
  

# MUNGE ============================================================================
  
  df_genie <- gophr::read_msd(file_path)  
  
# VIZ ============================================================================

 # Request: Coarse Age/Sex Breakdown of TX_CURR for FY23Q1
  df_genie %>% 
    filter(indicator == "TX_CURR",
           standardizeddisaggregate == "Age/Sex/HIVStatus", 
           fiscal_year == metadata$curr_fy) %>% 
    group_by(operatingunit, snu1, psnu, psnuuid, trendscoarse, sex) %>% 
    summarise(tx_curr = sum(cumulative, na.rm = T)) %>% 
    ungroup() %>% 
    group_by(snu1, trendscoarse, sex) %>% 
    mutate(tx_curr_snu1 = sum(tx_curr)) %>% 
    ungroup() %>% 
    group_by(trendscoarse, sex) %>% 
    mutate(tx_curr_zmb = sum(tx_curr)) %>% 
    ungroup() 
    #write_csv("Dataout/ZMB_FY23Q1_TX_CURR_coarseage_sex.csv")
  
  
  # Pull in MOH TX_CURR data
  df_moh <- readxl::read_excel(moh_data) %>% 
    janitor::clean_names()
  
  df_moh %>% count(sex_age_group) %>% pull(sex_age_group) %>% 
    writeLines()
  
  df_moh_datim <- df_moh %>% 
    left_join(., moh_cw)
  
  # Need to create district and provincial totals as well as pull TX_CURR across if previous period is missing
  # Let's reshape long, then we can use the lag operator to fill if the value is NA
  
  df_moh_datim %>% 
    pivot_longer(currently_on_art_october_2022:currently_on_art_december_2022, 
                 )
    
# SPINDOWN ============================================================================

