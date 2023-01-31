# PROJECT: Here-it-goes-again
# PURPOSE: Munge and Analysis of Testing
# AUTHOR: Tim Essam | SI
# REF ID:   fa267d47
# LICENSE: MIT
# DATE: 2023-01-31
# NOTES: Tim Essam | SI

# LOCALS & SETUP ============================================================================

    # Load Q1 functions
    source("Scripts/helper-call_all_helpers.R")
    
    library(rcartocolor)
    
    # SI specific paths/functions  
    load_secrets()
    
    msd_path <- return_latest(folderpath = merdata,
                              pattern = "PSNU_IM_FY20-23.*Zambia.zip")
    
    # Grab metadata
    get_metadata(file_path)
  
  # REF ID for plots
    ref_id <- "fa267d47"
    
  # Functions  
  

# LOAD DATA ============================================================================  

    df_genie_in <- read_msd(file_path) %>% 
      filter(funding_agency == "USAID")
    
    df_msd <- read_msd(msd_path) %>% 
      filter(fiscal_year %in% c(2020, 2021), funding_agency == "USAID")
    
    # bind these together b/c we need past TX_CURR to compute VLC
    df_genie <- df_genie_in %>% 
      bind_rows(df_msd) %>% 
      filter(funding_agency == "USAID") %>% 
      fix_mech_names() %>% 
      mutate(snu1 = str_remove_all(snu1, " Province")) %>% 
      clean_agency() %>% 
      swap_targets() 

# MUNGE ============================================================================
  
    df_hts_base <- 
      df_genie %>% 
      filter(indicator %in% c("HTS_TST_POS", "TX_NEW", "HTS_TST"), 
             standardizeddisaggregate == "Total Numerator",
             fiscal_year <= metadata$curr_fy, 
             funding_agency == "USAID") %>% 
      group_by(indicator, fiscal_year) %>% 
      summarise(across(targets:qtr4, sum, na.rm = T), .groups = "drop") %>% 
      reshape_msd(direction ="semi-wide") %>% 
      group_by(indicator) %>% 
      fill(targets, .direction = "down") %>% 
      filter(nchar(period) != 4) 
    
    df_hts_tgt <- df_hts_base %>% select(-results) %>% 
      pivot_wider(names_from = indicator, values_from = targets) %>% 
      mutate(linkage = TX_NEW / HTS_TST_POS)
    
    df_linkage <- 
      df_hts_base %>% 
      select(-targets) %>% 
      pivot_wider(names_from = indicator, values_from = results) %>% 
      mutate(linkage = TX_NEW / HTS_TST_POS)  
  
# VIZ ============================================================================

   bottom_hts <-  df_linkage %>% 
    ggplot(aes(x = period)) +
      geom_col(aes(y = HTS_TST_POS), fill = "#855C75",
               position = position_nudge(x = 0.1), width = 0.5) +
      geom_col(aes(y = TX_NEW), fill = "#D9AF6B", 
               position = position_nudge(x = -0.1), width = 0.5) +
      si_style_ygrid() +
      scale_y_continuous(labels = comma) +
      labs(x = NULL, y = NULL) +
      expand_limits(x = c(0, 9)) 
    
    # Linkage plot - #b08472
    top_hts <- df_linkage %>% 
      ggplot(aes(x = period, group = 1)) +
      geom_line(aes(y = linkage), color = grey50k, size = 0.5) +
      geom_point(aes(y = linkage), shape = 19, color = "#b08472",  size = 3) + 
      geom_point(aes(y = linkage), shape = 1, color = grey90k,  size = 3) + 
      geom_text(aes(y = linkage, label = percent(linkage, 1)), 
                size = 9/.pt,
                family = "Source Sans Pro",
                fontface = "bold", 
                color = "#b08472", 
                vjust = -1.5) +
      si_style_nolines() +
      expand_limits(y = c(.85, 1), x = c(0, 9)) +
      theme(axis.text.y = element_blank(), 
            axis.text.x = element_blank()) +
      labs(x = NULL, y = NULL) +
      annotate("text", x = 13.5, y = 0.9, label = "Linkage", 
               size = 11/.pt, color = "#b08472")
    
    top_hts / bottom_hts +
      plot_layout(heights = c(1, 4))
    si_save("Graphics/Linkage_summary.svg")

# Testing by age / snu1 ============================================================================

