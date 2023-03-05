# PROJECT: Ad hoc analysis of COP23 numbers
# PURPOSE: Munge and Analysis of Spectrum results
# AUTHOR: Tim Essam | SI
# REF ID:   a7e33ab5
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
    file_path <- "../../../Downloads/COP 23 Target Setting.xlsx"
      
  # Grab metadata
  # REF ID for plots
    ref_id <- "a7e33ab5"
    
  # Functions  
  

# LOAD DATA ============================================================================  

  tgt <- read_excel(file_path, skip = 3, n_max = 10) %>% 
      janitor::clean_names()
  names(tgt)

# MUNGE ============================================================================
  
  # WHAT IS USAIDS contribution of the gap?
  tgt %>% 
    group_by(agency) %>% 
    summarize(plhiv = sum(cop23_plhiv_feb_22nd),
              tx_curr = sum(moh_q1_fy23_results_spectrum_input),
              cop23_tgt = sum(cop23_targeted_coverage),
              gap = plhiv - tx_curr) %>% 
    ungroup() %>% 
    mutate(gap_share = gap / sum(gap, na.rm = T),
           tx_share = tx_curr/ sum(tx_curr, na.rm = T),
           tx_share_cop23 = cop23_tgt / sum(cop23_tgt, na.rm = T)) %>% 
    gt(rowname_col  = "agency") %>% 
    fmt_number(columns = 2:5,
               decimals = 0)   %>% 
    fmt_percent(columns = 6:8, 
                decimals = 0) %>% 
    grand_summary_rows(
      columns = 2:6,
      fns = list(Zambia = ~sum(., na.rm = TRUE)), 
      formatter = fmt_number, 
      decimals = 0
    ) %>% 
    gt_theme_nytimes() %>% 
    tab_style(
      style = list(
        cell_text(weight = "bold")
      ),
      locations = cells_row_groups()
    ) %>% 
    tab_header(title = "TREATMENT GAP BY FUNDING AGENCY") %>% 
    tab_source_note(
      source_note = gt::md(glue("Spectrum 2023 Estimates"))) %>% 
    tab_options(
      source_notes.font.size = px(10)) %>% 
    cols_label(plhiv = "PLHIV", 
               tx_curr= "MOH FY23Q1 TX_CURR ", 
               gap = "Treatment Gap",
               gap_share = "Share of Overall Gap",
               tx_share = "Current TX_CURR share",
               tx_share_cop23 = "Projected TX_CURR share"
    )
    
  
  
# VIZ ============================================================================

  #  

# SPINDOWN ============================================================================

