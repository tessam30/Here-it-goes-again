# PROJECT: Pull Q1 Data for SP Partners
# PURPOSE: Munge and Analysis of Q1 data
# AUTHOR: Tim Essam | SI
# REF ID:   cbd313dc
# LICENSE: MIT
# DATE: 2023-03-01
# NOTES: Tim Essam | SI

# LOCALS & SETUP ============================================================================

  # Libraries
    library(gagglr)
    library(tidyverse)
    library(scales)
    library(sf)
    library(extrafont)
    library(tidytext)
    library(patchwork)
    library(ggtext)
    library(glue)
    
    
  # SI specific paths/functions  
    load_secrets()
    merdata <- file.path(glamr::si_path("path_msd"))
    file_path <- return_latest(folderpath = merdata,
      pattern = "PSNU_IM_FY21-23_20230210_v1_1_Zambia")
      
  # Grab metadata
   get_metadata(file_path)
  
  # REF ID for plots
    ref_id <- "cbd313dc"
    
  # Functions  
  

# LOAD DATA ============================================================================  

  msd <- read_msd(file_path)

# MUNGE ============================================================================
  
  # Pull for keep partners
  mech_list <-  as.character(c(85114L, 85120L, 85121L, 17410L, 85117L, 18487L, 17422L, 17399L, 160806L, 82086L))
  indic_list <- c("PP_PREV", "KP_PREV", "OVC_SERV", "OVC_HIVSTAT", "HTS_TST", "HTS_TST_POS")
    
    
  msd %>% filter(mech_code %in% mech_list, 
                 indicator %in% indic_list,
                 standardizeddisaggregate %in% c("Total Numerator", "KeyPop", "KeyPop/Result"), 
                 fiscal_year == metadata$curr_fy) %>% 
    group_by(mech_code, mech_name, prime_partner_name, indicator, standardizeddisaggregate) %>% 
    summarise(across(c("cumulative","targets"), sum, na.rm = T), .groups = "drop") %>% 
    mutate(achv = cumulative / targets) %>% 
    write_csv("Dataout/SP_pull_Amara_2023_03_01.csv")
  
# VIZ ============================================================================

  #  

# SPINDOWN ============================================================================

