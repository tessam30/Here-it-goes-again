# PROJECT: Here-it-goes-again
# PURPOSE: Munge and Analysis of Index Testing
# AUTHOR: Tim Essam | SI
# REF ID:   3eac253d
# LICENSE: MIT
# DATE: 2023-01-31
# NOTES: Tim Essam | SI

# LOCALS & SETUP ============================================================================
    
  # Load Q1 functions
  source("Scripts/helper-call_all_helpers.R")

  library(rcartocolor)

  # SI specific paths/functions  
    load_secrets()

  # Grab metadata
   get_metadata(file_path)
  
  # REF ID for plots
    ref_id <- "3eac253d"
    
  # Functions  
    munge_modality <- function(df, ...){   
      df_hts_full <- df %>% 
        filter(indicator == "HTS_TST_POS",
               standardizeddisaggregate == "Modality/Age/Sex/Result",
               fiscal_year <= metadata$curr_fy, 
               funding_agency == "USAID", ...) %>% 
        mutate(mod_type = case_when(
          str_detect(modality, "Index") ~ "Index",
          str_detect(modality, "OtherPITC") ~ "Other PITC",
          str_detect(modality, "PMTCT") ~ "PMTCT",
          modality == "VCT" ~ "VCT",
          str_detect(modality, "SNSMod") ~ "Community SNS",
          TRUE ~ "Other")
        ) %>%
        group_by(fiscal_year, mod_type, mech_name) %>%
        summarise(across(starts_with("qtr"), sum, na.rm = TRUE)) %>%
        ungroup() %>%
        reshape_msd() %>%
        select(-period_type) %>%
        group_by(period) %>%
        mutate(contribution = value/sum(value)) %>%
        ungroup() %>%
        mutate(start = case_when(period == min(period) ~ contribution),
               end = case_when(period == max(period) ~ contribution)) %>%
        mutate(mod_order = fct_reorder(mod_type, value, .desc = T)) %>% 
        complete(mod_type, period, mech_name) %>% 
        group_by(mod_type, mech_name) %>% 
        fill(mod_order, .direction = "up") %>% 
        group_by(period, mech_name) %>% 
        mutate(pd_tot = sum(value, na.rm = T), 
               pd_25 = pd_tot * 0.25, 
               pd_50 = pd_tot * 0.5,
               pd_75 = pd_tot * 0.75) %>% 
        ungroup() %>% 
        mutate(mod_color = case_when(
          mod_type == "Index" ~ "#855C75", 
          mod_type == "VCT" ~ "#D9AF6B",
          mod_type == "Other PITC" ~ "#AF6458",
          mod_type == "PMTCT"~ "#736F4C",
          mod_type == "Community SNS" ~ "#526A83",
          TRUE ~ "#7C7C7C"
        ),
        note = case_when(
          mod_type == "Index" & period == "FY22Q1" ~ "HTS_TST_POS",
          TRUE ~ NA_character_
        )) %>% 
        filter(!is.na(mod_order))
      return(df_hts_full)
    }
  

# LOAD DATA ============================================================================  

  df_genie <- read_msd(file_path)

# MUNGE ============================================================================
    
    munge_modality(df_genie %>% mutate(mech_code = 123456, mech_name = "USAID"), 
                   mech_code == 123456) %>% 
      plot_modality(.)
  
  
# VIZ ============================================================================

  #  

# SPINDOWN ============================================================================

