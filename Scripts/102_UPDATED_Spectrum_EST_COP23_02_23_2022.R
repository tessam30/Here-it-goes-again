# PROJECT: Calculate COP23 EPI Gaps for Zambia
# PURPOSE: Munge and Analysis of Spectrum and MOH TX_CURR
# AUTHOR: Tim Essam | SI
# REF ID:   ab4c5847
# LICENSE: MIT
# DATE: 2023-02-21
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
    library(readxl)
    library(gtExtras)
    library(gt)
    
  # SI specific paths/functions  
    source("Scripts/helper-call_all_helpers.R")

  # Data paths  
  # NOTE - BEFORE READING IN PLHIV estimates, MAKE SURE LUSAKA PROVINCE IS FLAGGED LusakaP
  # O/wise you will get grossly inflated gap estimates
    plhiv_path <- "Data/PLHIV by 5-year age bands Feb 22 2023.xlsx"
    moh_data <- "Data/MoH TX_CURR File for December 2022.xls"
    msd_path <- return_latest(merdata, "MER_Structured_Datasets_PSNU_IM_FY21-23_20230210_v1_1_Zambia")
  

  # Grab metadata
    get_metadata(msd_path)
  
  # REF ID for plots
    ref_id <- "ab4c5847"

    
  # MOH crosswalk to DATIM bands
  # NOTE: The 35-49 bands differ from DATIM!
    moh_cw <- tibble::tribble(
      ~sex, ~ageasentered,     ~sex_age_group,
      "Female",         "50+",   "Female, (+50)y",
      "Female",       "10-14", "Female, (10-14)y",
      "Female",       "15-19", "Female, (15-19 )y",
      "Female",       "20-24", "Female, (20-24)y",
      "Female",       "25-29", "Female, (25-29)y",
      "Female",       "30-34", "Female, (30-34)y",
      "Female",       "35-49", "Female, (35-49)y",
      "Female",         "05-09",   "Female, (5-9)y",
      "Female",         "00-04",   "Female, 0-2 yrs",
      "Female",         "00-04",   "Female, 3-4 yrs",
      "Male",         "50+",     "Male, (+50)y",
      "Male",       "10-14",   "Male, (10-14)y",
      "Male",       "15-19",   "Male, (15-19 )y",
      "Male",       "20-24",   "Male, (20-24)y",
      "Male",       "25-29",   "Male, (25-29)y",
      "Male",       "30-34",   "Male, (30-34)y",
      "Male",       "35-49",   "Male, (35-49)y",
      "Male",         "05-09",     "Male, (5-9)y",
      "Male",         "00-04",     "Male, 0-2 yrs",
      "Male",         "00-04",     "Male, 3-4 yrs"
    )
    

    

# LOAD DATA ============================================================================  

    # Check sheet names in file; keep header and body of data, we'll pivot and use
    # the index value to crosswalk the appropriate age bands with data body
    excel_sheets(plhiv_path)  
    
    header <- read_excel(plhiv_path, sheet = "District", n_max = 1) %>% 
      janitor::clean_names()
    
    data_body <- read_excel(plhiv_path, sheet = "District", skip = 2) %>% 
      janitor::clean_names()
    
    names(data_body)
    
    data_body_prov <- read_excel(plhiv_path, sheet = "Provincial_fix", skip = 2) %>% 
      janitor::clean_names()
    
    # MOH ART data
    df_moh <- readxl::read_excel(moh_data) %>% 
      janitor::clean_names()

    # PEPFAR ART M&E data
    df_msd <- read_msd(msd_path)
    
# MUNGE MSD FOR PROVINCES -------------------------------------------------
    
    # Get a list of standardized provinces to use
    # Flagging LusakaP because there is a also a Lusaka District that will get
    # merged with Lusaka province if one is not careful
    prov_list <- df_msd %>% 
      filter(str_detect(snu1, "Military", negate = T)) %>% 
      mutate(snu1 = str_remove_all(snu1, " Province")) %>% distinct(snu1) %>%
      mutate(snu1 = ifelse(snu1 == "Lusaka", "LusakaP", snu1)) %>% 
      pull()  
    
    # Add in Zambia b/c the district tab includes SNU1 and OU totals
    prov_list <- c(prov_list, "Zambia")
    
    psnu_list <- df_msd %>% 
      filter(str_detect(psnu, "Military|above", negate = T)) %>% 
      mutate(psnu = str_remove_all(psnu, " District")) %>% 
      distinct(psnu, psnuuid) 
        
    # TX_CURR for Q1
    # In spectrum estimates, age bands start at 0-4, need to account for this
    df_msd_tx <- df_msd %>% 
      filter(indicator == "TX_CURR",
             standardizeddisaggregate == "Age/Sex/HIVStatus",
             fiscal_year == metadata$curr_fy, 
             ageasentered %ni% c("Unknown Age")) %>% 
      mutate(age_datim = case_when(
        ageasentered %in% c("<01", "01-04") ~ "00-04", 
        ageasentered %in% c("50+", "50-54", "55-59", "60-64", "65+") ~ "50+", 
        TRUE ~ ageasentered
      )) %>% 
      group_by(age_datim, indicator, sex, psnu, psnuuid, snu1) %>% 
      summarize(tx_fy23q1 = sum(cumulative, na.rm = T), .groups = "drop") 
    
    # Geerate SNU estimates
    df_tx_snu <- df_msd_tx %>% 
      group_by(snu1, age_datim, indicator, sex) %>% 
      summarise(tx_fy23q1 = sum(tx_fy23q1, na.rm = T), .groups = "drop") %>% 
      mutate(snu1 = str_remove_all(snu1, " Province"))
    
    # Generate agency estimates
    df_tx_agency <- 
      df_msd_tx %>% 
      filter(str_detect(snu1, "Military", negate = T)) %>% 
      mutate(snu1 = str_remove_all(snu1, " Province")) %>% 
      left_join(., prov_agency_cw) %>% 
      group_by(snu1_agency, age_datim, indicator, sex) %>% 
      summarise(tx_fy23q1 = sum(tx_fy23q1, na.rm = T), .groups = "drop")

# SPECTRUM ESTIMATES ------------------------------------------------------

    # Tag provinces, making a new column that we can copy down to map districts to parent SNU
    # Also want to drop 
    plhiv <- data_body %>% 
      mutate(x1 = case_when(
        x1 == "North-Western" ~ "NorthWestern", 
        TRUE ~ x1),
             prov_tag = ifelse(x1 %in% prov_list, x1, NA_character_), 
             prov_drop = ifelse(x1 %in% prov_list, "drop", "keep")) %>% 
      fill(prov_tag, .direction = c("down")) %>% 
      filter(prov_drop == "keep") %>% 
      pivot_longer(cols = male_2:female_35, 
                   names_to = "sex",
                   values_to = "plhiv") %>% 
      separate(sex,into = c("sex", "cw_number"), sep = "_") %>% 
      rename(psnu = x1)
    
    plhiv_snu <- data_body_prov %>% 
      pivot_longer(cols = male_2:female_35, 
                   names_to = "sex",
                   values_to = "plhiv") %>% 
      separate(sex,into = c("sex", "cw_number"), sep = "_") %>% 
      rename(snu1 = x1)   
    
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
    setdiff(psnu_list$psnu, unique(plhiv_est$psnu))
    
    # Check age bands
    plhiv_est %>% count(age)
    
    # Fix age bands before merging with df_tx
    plhiv_psnu <- plhiv_est %>% 
      left_join(psnu_list) %>% 
      mutate(age_datim = case_when(
        age == "0-4" ~ "00-04",
        age == "5-9" ~ "05-09",
        age %in% c("50-54", "55-59", "60-64", "65-69", "70-74", "75-79", "80+") ~ "50+",
        TRUE ~ age
      ),
      sex = str_to_title(sex)) 
    
    plhiv_snu <- plhiv_snu %>% 
      left_join(plhiv_cw) %>%  mutate(age_datim = case_when(
        age == "0-4" ~ "00-04",
        age == "5-9" ~ "05-09",
        age %in% c("50-54", "55-59", "60-64", "65-69", "70-74", "75-79", "80+") ~ "50+",
        TRUE ~ age
      ),
      sex = str_to_title(sex),
      snu1 = ifelse(snu1 == "North-Western", "NorthWestern", snu1)) %>% 
      group_by(age_datim, sex, snu1) %>% 
      summarize(plhiv = sum(plhiv, na.rm = T)) %>% 
      left_join(., prov_agency_cw)
    
    # Do the Provincial names align? YES, after modification above
    plhiv_snu %>% count(age_datim, sex) %>% spread(sex, n)
    
    setdiff(unique(plhiv_snu$snu1), unique(df_tx_snu$snu1))
    
    
# MUNGE MOH Data  ============================================================================
    # First, let's tackle the MOH data and get in a format we can merge with DATIM
    # Check the age categories MOH uses
    df_moh %>% 
      count(sex_age_group) %>% 
      pull(sex_age_group) %>% 
      writeLines()
    
    df_msd %>% 
      filter(indicator == "TX_CURR",
             standardizeddisaggregate == "Age/Sex/HIVStatus") %>% 
      count(ageasentered) 
    
    # Align to crosswalk so we can merge w/ PLHIV estimates
    # Ensure that Northwestern is spelled NorthWestern
    df_moh_datim <- df_moh %>% 
      left_join(., moh_cw) %>% 
      mutate(province = ifelse(province == "Northwestern", "NorthWestern", province))
    
    # Check if the districts/Provinces match to what is in Spectrum estimates
    compare_vars(unique(df_moh_datim$district), unique(plhiv_psnu$psnu))
    compare_vars(unique(df_moh_datim$province), unique(plhiv_snu$snu1))
    
    
    # Need to create district and provincial totals as well as pull TX_CURR across if previous period is missing
    # Let's reshape long, then we can use the lag operator to fill if the value is NA
    
    df_moh_datim_trim <- 
      df_moh_datim %>% 
      mutate(art_adj = case_when(
        is.na(currently_on_art_november_2022) & is.na(currently_on_art_december_2022) ~ currently_on_art_october_2022,
        is.na(currently_on_art_december_2022) & !is.na(currently_on_art_november_2022) ~ currently_on_art_november_2022,
        TRUE ~ currently_on_art_december_2022)
      ) %>% 
      select(-starts_with("currently"))
    
    
    # Create PSNU results
    df_moh_psnu <- 
      df_moh_datim_trim %>% 
      group_by(snu1 = province, psnu = district, age_datim = ageasentered, sex) %>% 
      summarize(tx_curr_moh = sum(art_adj, na.rm = T), .groups = "drop")
    
    df_moh_snu1 <- 
      df_moh_psnu %>% 
      group_by(snu1, age_datim, sex) %>% 
      summarize(tx_curr_moh = sum(tx_curr_moh), .groups = "drop")      
    


# MUNGE: COMBINE ALL THE ESTIMATES! ---------------------------------------

  # Use Spectrum as the base, merge on MOH ART data
    map(list(plhiv_psnu, df_moh_psnu), ~names(.x))

  # Need to collapse spectrum estimates down to MOH / DATIM age bands  
  # There also appears to be some age / sex / districts that are not covered by MOH data
  # So the 35-49 is a MOH range of that is not aligned to MER
  # Lunga does have a gap, but we'll ignore it for now as we pry will not look at PSNU / sex / fine age bands
   tmp <-  plhiv_psnu %>%
      mutate(age_datim = case_when(
        age_datim %in% c("35-39", "40-44", "45-49") ~ "35-49",
        TRUE ~ age_datim
      )) %>% 
      group_by(age_datim, sex, psnuuid, psnu) %>% 
      summarize(plhiv_datim = sum(plhiv, na.rm = T), .groups = "drop") %>% 
      count(psnu, age_datim, sex) 
    
   tmp2 <- df_moh_psnu %>% count(psnu, age_datim, sex)
    
  setdiff(tmp, tmp2) %>% prinf()  

    
  # NOW MERGE!!! BOTH MOH AND DATIM TO COMPARE
  
  
  
  art_gap_psnu <- plhiv_psnu %>%
    mutate(age_datim = case_when(
      age_datim %in% c("35-39", "40-44", "45-49") ~ "35-49",
      TRUE ~ age_datim
    )) %>% 
    group_by(age_datim, sex, psnuuid, psnu) %>% 
    summarize(plhiv = sum(plhiv, na.rm = T), .groups = "drop") %>% 
    tidylog::left_join(., df_moh_psnu) %>% 
    mutate(art_gap = plhiv - tx_curr_moh)
  
  art_gap_snu1 <- 
    plhiv_snu %>% 
    mutate(age_datim = case_when(
      age_datim %in% c("35-39", "40-44", "45-49") ~ "35-49",
      TRUE ~ age_datim
    )) %>% 
    group_by(age_datim, sex, snu1, snu1uid) %>% 
    summarize(plhiv = sum(plhiv, na.rm = T), .groups = "drop") %>% 
    tidylog::left_join(., df_moh_snu1) %>% 
    mutate(art_gap = plhiv - tx_curr_moh)
  

    
    
# VIZ ============================================================================

  #  

# SPINDOWN ============================================================================

