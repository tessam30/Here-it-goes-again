# PROJECT: Another round of analysis of PLHIV by district / age
# PURPOSE: Munge and Analysis of Spectrum COP23 estimates
# AUTHOR: Tim Essam | SI
# REF ID:   6e237fd3
# LICENSE: MIT
# DATE: 2023-03-06
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
    library(readxl)
    library(glue)
    
    
  # SI specific paths/functions  
    load_secrets()
    merdata <- file.path(glamr::si_path("path_msd"))
    plhiv_path <- "Data/PLHIV by district March 5.xlsx"
    msd_path <- return_latest(merdata, "MER_Structured_Datasets_PSNU_IM_FY21-23_20230210_v1_1_Zambia")
          
  # Grab metadata
   #get_metadata(file_path)
  
  # REF ID for plots
    ref_id <- "6e237fd3"
    
  # Functions  
  

# LOAD DATA ============================================================================  

    # Check sheet names in file; keep header and body of data, we'll pivot and use
    # the index value to crosswalk the appropriate age bands with data body
    excel_sheets(plhiv_path)  
    
    header <- read_excel(plhiv_path, sheet = "PLHIV_est_2023", n_max = 1) %>% 
      janitor::clean_names()
    
    data_body <- read_excel(plhiv_path, sheet = "PLHIV_est_2023", skip = 2) %>% 
      janitor::clean_names()
    
    names(data_body)
    
    # PEPFAR ART M&E data
    df_msd <- read_msd(msd_path)
    
    # Get a list of standardized provinces to use
    # Flagging LusakaP because there is a also a Lusaka District that will get
    # merged with Lusaka province if one is not careful
    prov_list <- df_msd %>% 
      filter(str_detect(snu1, "Military", negate = T)) %>% 
      mutate(snu1 = str_remove_all(snu1, " Province")) %>% distinct(snu1) %>%
      mutate(snu1 = ifelse(snu1 == "Lusaka", "LusakaP", snu1)) %>% 
      pull()  

# MUNGE ============================================================================
  
    # Tag provinces, making a new column that we can copy down to map districts to parent SNU
    # Also want to drop 
    plhiv <- data_body %>% 
      mutate(  
        prov_tag = ifelse(district %in% prov_list, district, NA_character_), 
        prov_drop = ifelse(district %in% prov_list, "drop", "keep")
        ) %>% 
      fill(prov_tag, .direction = c("down")) %>% 
      filter(prov_drop == "keep") %>% 
      pivot_longer(cols = female_2:male_15, 
                   names_to = "sex",
                   values_to = "plhiv") %>% 
      separate(sex,into = c("sex", "cw_number"), sep = "_") %>% 
      rename(psnu = district)
  
# VIZ ============================================================================

  #  

# SPINDOWN ============================================================================

