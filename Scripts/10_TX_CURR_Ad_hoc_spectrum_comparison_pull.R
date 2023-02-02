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

# MUNGE ============================================================================
  
  df_genie <- read_msd(file_path)  
  
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
    ungroup() %>% 
    write_csv("Dataout/ZMB_FY23Q1_TX_CURR_coarseage_sex.csv")
  
  

# SPINDOWN ============================================================================

