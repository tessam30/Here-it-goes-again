# PROJECT: Here-it-goes-again
# PURPOSE: Munge and Analysis of COP23 Estimates from SPECTRUM
# AUTHOR: Tim Essam | SI
# REF ID:   d77f9986
# LICENSE: MIT
# DATE: 2023-02-05
# NOTES: Tim Essam | SI

# LOCALS & SETUP ============================================================================

    # Load Q1 functions
    #devtools::install_github(repo = "USAID-OHA-SI/cascade", ref = "dev")
    source("Scripts/helper-call_all_helpers.R")

  library(readxl)
  
  # REF ID for plots
    ref_id <- "d77f9986"
    
  # Functions  
  # Filepath
    plhiv_path <- "Data/Preliminary output Feb 03 2023.xlsx"

# LOAD DATA ============================================================================  

  # Check sheet names in file; keep header and body of data, we'll pivot and use
  # the index value to crosswalk the appropriate age bands with data body
    excel_sheets(plhiv_path)  
  
    header <- read_excel(plhiv_path, sheet = "District", n_max = 1) %>% 
    janitor::clean_names()
  
    data_body <- read_excel(plhiv_path, sheet = "District", skip = 2) %>% 
    janitor::clean_names()
  
    names(data_body)

  
  # Need to grab SNUs to filter out the extra rows in the genie pull
  df_genie <- read_msd(file_path)
  
  

  
# MUNGE ============================================================================

  # filter list of PSNUs to tag districts (copy down)
  prov_list <- df_genie %>% 
    filter(str_detect(snu1, "Military", negate = T)) %>% 
    mutate(snu1 = str_remove_all(snu1, " Province")) %>% distinct(snu1) %>%
    mutate(snu1 = ifelse(snu1 == "Lusaka", "LusakaP", snu1)) %>% 
    pull()  
  
  # Add in Zambia b/c the district tab includes SNU1 and OU totals
  prov_list <- c(prov_list, "Zambia")
  
  psnu_list <- df_genie %>% 
    filter(str_detect(psnu, "Military|above", negate = T)) %>% 
    mutate(psnu = str_remove_all(psnu, " District")) %>% 
    distinct(psnu, psnuuid) 

  # TX_CURR for Q1
  # In spectrum estimates, age bands start at 0-4, need to account for this
  df_tx <- df_genie %>% 
    filter(indicator == "TX_CURR",
           standardizeddisaggregate == "Age/Sex/HIVStatus",
           fiscal_year == metadata$curr_fy, 
           ageasentered %ni% c("Unknown Age", "50+")) %>% 
    mutate(age_datim = case_when(
      ageasentered %in% c("<01", "01-04") ~ "00-04", 
      TRUE ~ ageasentered
    )) %>% 
    group_by(age_datim, indicator, sex, psnu, psnuuid, snu1) %>% 
    summarize(tx_fy23q1 = sum(cumulative, na.rm = T), .groups = "drop") 
    
  # This is the crosswalk we need for the ages
  df_tx %>% count(age_datim)
    
  # Tag provinces, making a new column that we can copy down to map districts to parent SNU
  plhiv <- data_body %>% 
    mutate(prov_tag = ifelse(x1 %in% prov_list, x1, NA_character_), 
           prov_drop = ifelse(x1 %in% prov_list, "drop", "keep")) %>% 
    fill(prov_tag, .direction = c("down")) %>% 
    filter(prov_drop == "keep") %>% 
    pivot_longer(cols = male_2:female_35, 
                 names_to = "sex",
                 values_to = "plhiv") %>% 
    separate(sex,into = c("sex", "cw_number"), sep = "_") %>% 
    rename(psnu = x1)
  
  # PLHIV header and crosswalk to get to age bands
  # One oddity put a "female" value in the age band spot. Remove this first
  # There is also a missing column, need 34 (2:35)
  # Need to fix the 0-4 and 5-9 age bands to match datim
  plhiv_cw <- header %>% 
    select(-1) %>% 
    mutate(x5 = NA_character_,
           x35 = NA_character_) %>% 
    pivot_longer(cols = x2:x35,
                 names_to = "cw_number",
                 values_to = "age") %>% 
    mutate(cw_number = str_remove_all(cw_number, "x")) %>% 
    fill(age, .direction = c("down"))
  
  # Fix the districts that are named differently
  plhiv_est <- 
    plhiv %>% 
    left_join(plhiv_cw) %>% 
    mutate(psnu = case_when(
      psnu == "Chiengi" ~ "Chienge",
      psnu == "Kapiri Mposhi" ~ "Kapiri-Mposhi",
      psnu == "Senga Hill" ~ "Senga",
      psnu == "Mushindano" ~ "Mushindamo",
      psnu == "Milengi" ~ "Milenge",
      psnu == "Shangombo" ~ "Shang'ombo" ,
      psnu == "Chikankanta" ~ "Chikankata",
      TRUE ~ psnu
    ))
  
  # How much do the district overlap? Should be 116
  setdiff( psnu_list$psnu, unique(plhiv_est$psnu))
  
  # Fix age bands before merging with df_tx
  plhiv_est <- plhiv_est %>% 
    left_join(psnu_list) %>% 
    mutate(age_datim = case_when(
      age == "0-4" ~ "00-04",
      age == "5-9" ~ "05-09",
      age %in% c("65-69", "70-74", "75-79", "80+") ~ "65+",
      TRUE ~ age
    ),
    sex = str_to_title(sex)) 
  

  plhiv_gap_df <- plhiv_est %>% 
    group_by(age_datim, sex, psnuuid) %>% 
    summarize(plhiv_datim = sum(plhiv, na.rm = T), .groups = "drop") %>% 
    right_join(., df_tx) %>% 
    mutate(tx_gap = plhiv_datim - tx_fy23q1,
           snu1 = str_remove_all(snu1, " Province")) %>% 
    clean_column()
    
  
# VIZ ============================================================================

  # BY SNU1, SHOW how the PLHIV GAPS STACK UP across district
  
  plhiv_gap_df %>% 
    filter(str_detect(snu1, "Military", negate = T)) %>% 
    group_by(snu1, age_datim) %>% 
    summarise(across(c(tx_gap, tx_fy23q1, plhiv_datim), sum, na.rm = T), .groups = "drop") 
    ggplot(aes(y = age_datim)) +
    geom_col(aes(x = plhiv_datim), fill = golden_sand, width = 0.75 ) +
    geom_col(aes(x = tx_fy23q1), fill = denim, width = 0.75) +
    facet_wrap(~snu1, 
               labeller = labeller(.multi_line = F), 
               scales = "free_x") +
    geom_text(aes(x = plhiv_datim, label = comma(round(tx_gap, 0))), 
              family = "Source Sans Pro",
              size = 8/.pt, 
              hjust = -1, 
              color = grey90k) +
    si_style_xgrid(facet_space = 0.5) 
    
  # What is alleged PLHIV GAP for PEDS? FOR AYPS?
    plhiv_gap_df %>% 
      mutate(age_custom = case_when(
        age_datim %in% c("00-04", "05-09", "10-14") ~ "Peds",
        age_datim %in% c("15-19", "20-24") ~ "AYP",
        TRUE ~ age_datim
      )) %>% 
      group_by(age_custom) %>% 
      summarise(across(c(plhiv_datim, tx_fy23q1, tx_gap), sum, na.rm = T), .groups = "drop") 
    
  
  

# SPINDOWN ============================================================================

