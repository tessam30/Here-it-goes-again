# PROJECT: Here-it-goes-again
# PURPOSE: Analysis of FY23Q1 data for data review
# AUTHOR: Tim Essam | SI
# REF ID:   0bdaedbe
# LICENSE: MIT
# DATE: 2023-01-30
# NOTES: Tim Essam | SI

# LOCALS & SETUP ============================================================================

  library(gt)
  library(gtExtras)
  library(selfdestructin5)

  load_secrets()

  source("Scripts/helper-call_all_helpers.R")
    
  # Functions  
  

# LOAD DATA ============================================================================  

  df_genie <- read_msd(file_path)  %>% 
    fix_mech_names() %>% 
    mutate(snu1 = str_remove_all(snu1, " Province")) %>% 
    clean_agency() %>% 
    swap_targets() 
  
# MUNGE ============================================================================
 
  # Check if data is in yet 
  df_genie %>% filter(fiscal_year == 2023, indicator %in% cascade_ind) %>% View()


# SUMMARY TABLES ===================================================================

  
  
  mdb_df   <- make_mdb_df(df_genie)
  mdb_tbl  <- reshape_mdb_df(mdb_df, metadata$curr_pd)  
  
  # Create the treatment data frame needed for derived indicators
  mdb_df_tx    <- make_mdb_tx_df(df_genie)
  mdb_tbl_tx   <- reshape_mdb_tx_df(mdb_df_tx, metadata$curr_pd)
  
  mdb_tbl %>% 
    # filter(indicator != "GEND_GBV") %>%
    create_mdb(ou = "Zambia", type = "main", metadata$curr_pd, metadata$source) %>% 
    gtsave(path = "Images", filename = glue::glue("Zambia_{metadata$curr_pd}_mdb_main.png"))  
  
  
  create_mdb(mdb_tbl_tx, ou = "Zambia", type = "treatment", metadata$curr_pd, metadata$source) %>% 
    bold_column(., Q4) %>% 
    embiggen() %>% 
    gtsave(., path = "Images", filename = glue::glue("{metadata$curr_pd}_Zambia_MMD_VL_MD.png"))   
  
  
  
# VIZ ============================================================================

  #  

# SPINDOWN ============================================================================

