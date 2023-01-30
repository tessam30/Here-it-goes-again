# PROJECT: Here-it-goes-again
# PURPOSE: Analysis of FY23Q1 data for cascade plots
# AUTHOR: Tim Essam | SI
# REF ID:   3067f005
# LICENSE: MIT
# DATE: 2023-01-30
# NOTES: Tim Essam | SI

# LOCALS & SETUP ============================================================================

  # Load Q1 functions
  source("Scripts/helper-call_all_helpers.R")
    
  library(cascade)

  # SI specific paths/functions  
    load_secrets()

      
  # Grab metadata
   get_metadata(file_path)
  
  # REF ID for plots
    ref_id <- "3067f005"
    
  # Functions  
  

# LOAD DATA ============================================================================  

    df_msd <- read_msd(file_path)  %>% 
      fix_mech_names() %>% 
      mutate(snu1 = str_remove_all(snu1, " Province")) %>% 
      clean_agency() %>% 
      swap_targets() %>% 
      filter(funding_agency == "USAID")

# CREATE USAID CASCADE ============================================================================
  
  #  USING DEV VERSION to get around TX_CURR_LAG2 missing
    
    # All of PEPFAR Zambia cascade
    return_cascade(df_msd, 1) %>% prinf()
    
    # Generate plots for all agencies
    batch_cascade_plot(df_msd, imgpath = "Images/Cascade/USAID", imgtype =".svg")
    
  
# MECHANISM CASCADES ============================================================================

    # Loop over mechs
    
    return_cascade(df_msd %>% filter(mech_name == "ZAM Health"), 1)
    
    
    batch_cascade_plot(df_msd %>% filter(mech_name == "DISCOVER-H"),
                       imgpath = "Images/Cascade/DISCOVER", imgtype =".svg")
    
    batch_cascade_plot(df_msd %>% filter(mech_name == "SAFE"),
                       imgpath = "Images/Cascade/SAFE", imgtype =".svg")
    
    batch_cascade_plot(df_msd %>% filter(mech_name == "Action HIV"),
                       imgpath = "Images/Cascade/ACTION_HIV", imgtype =".svg")
    
    batch_cascade_plot(df_msd %>% filter(mech_name == "ZAM Health"),
                       imgpath = "Images/Cascade/ZAM Health", imgtype =".svg")

# SPINDOWN ============================================================================

