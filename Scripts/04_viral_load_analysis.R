# PROJECT:  Here-it-goes-again
# PURPOSE: Munge and Analysis of viral load indicators
# AUTHOR: Tim Essam | SI
# REF ID:   ead26f8a
# LICENSE: MIT
# DATE: 2023-01-31
# NOTES: Tim Essam | SI

# LOCALS & SETUP ============================================================================

  # Load helpers
  source("Scripts/helper-call_all_helpers.R")
  
  msd_path <- return_latest(folderpath = merdata,
                           pattern = "PSNU_IM_FY20-23.*Zambia.zip")

  # Grab metadata
   get_metadata(file_path)
  
  # REF ID for plots
    ref_id <- "ead26f8a"
    
  # Functions  
    mech_names_order <- c("SAFE", "Action HIV", "DISCOVER-H", "ZAM Health")

# LOAD DATA ============================================================================  

  df_genie <- read_msd(file_path)
    
  df_msd <- read_msd(msd_path) %>% filter(fiscal_year %in% c(2020, 2021), funding_agency == "USAID")
  
  df_genie <- df_genie %>% bind_rows(df_msd) %>% 
    filter(funding_agency == "USAID") %>% 
    fix_mech_names() %>% 
    mutate(snu1 = str_remove_all(snu1, " Province")) %>% 
    clean_agency() %>% 
    swap_targets() 
    
df_genie %>% count(funding_agency)

# MUNGE ============================================================================
    df_vl <- df_genie %>% 
      filter(funding_agency == "USAID") %>% 
      create_vl_df() %>% 
    filter(str_detect(period, "20", negate = T))
    
    df_vl_peds <- df_genie %>% 
      filter(funding_agency == "USAID", trendscoarse == "<15") %>% 
      create_vl_df() %>% 
      filter(str_detect(period, "20", negate = T))
    
    # Remap mech names so EQUIP becomes ACTION HIV TO SHOW across time
    df_vl_ip <- df_genie %>% 
      filter(funding_agency == "USAID", trendscoarse == "<15", 
             mech_name %ni% c("Placeholder - 86412")) %>%
      create_vl_df(mech_name) %>% 
      filter(str_detect(period, "20", negate = T)) %>% 
      mutate(mech_name = fct_relevel(mech_name, mech_names_order))
    
    num_pds <- length(unique(df_vl$period))
    
    top <- df_vl %>% 
      ggplot(aes(x = period, group = 1)) +
      geom_line(aes(y = vls), color = burnt_sienna) +
      geom_point(aes(y = vls), shape = 21, fill = burnt_sienna, size = 3,
                 color = "white") +
      geom_line(aes(y = vlc), color = denim) +
      geom_point(aes(y = vlc), shape = 21, fill = denim, size = 3,
                 color = "white") +
      geom_text(aes(y = vlc, label = percent(vlc, 1)), size = 9/.pt,
                family = "Source Sans Pro", color = denim, 
                vjust = -1) +
      geom_text(aes(y = vls, label = percent(vls, 1)), size = 9/.pt,
                family = "Source Sans Pro", color = burnt_sienna, 
                vjust = -1) +
      annotate("text", x = num_pds + .5, y = .97, label = "Viral Load\nSuppression",
               color = burnt_sienna, size = 10/.pt,
               hjust = 0.1, 
               family = "Source Sans Pro") +
      annotate("text", x = num_pds + .5, y = .69, label = "Viral Load\nCoverage",
               color = denim, size = 10/.pt,
               hjust = 0.1, 
               family = "Source Sans Pro") +
      si_style_nolines() +
      expand_limits(x = c(1, num_pds+2), y = c(0.7,1.05)) +
      theme(axis.text.y = element_blank(), 
            axis.text.x = element_blank()) +
      labs(x = NULL, y = NULL)
    
    
    
    bottom <- df_vl %>% 
      ggplot(aes(x = period)) +
      geom_col(aes(y = tx_curr_lag2), fill = grey10k) +
      geom_col(aes(y = tx_pvls_d), fill = denim) +
      si_style_ygrid() +
      scale_y_continuous(labels = comma) +
      expand_limits(x = c(1, num_pds+2)) +
      labs(x = NULL, y = NULL) +
      annotate("segment", x = num_pds + .5, xend = num_pds + .5, y = 401090, yend = 529000, 
               color = grey70k) +
      annotate("text", x = num_pds + .65, y = 450000, label = "Coverage gap", 
               hjust = 0, size = 8/.pt, family = "Source Sans Pro", 
               color = grey70k)+
      annotate("text", x = num_pds+1, y = 540000, label = "TX_CURR_LAG2", 
               size = 8/.pt, family = "Source Sans Pro", color = grey50k) +
      annotate("text", x = num_pds+1, y = 300000, label = "TX_PVLS_D", 
               size = 8/.pt, family = "Source Sans Pro", color = denim)
    
    top / bottom + plot_layout(heights = c(1, 3))+
      plot_annotation(title = glue("VIRAL LOAD SUMMARY FOR {metadata$curr_fy}"),
                      caption = metadata$caption) &
      theme(plot.tag = element_text(family = "Source Sans Pro"))
    
    si_save("Graphics/VL_summary_2022.svg")
  
# VIZ ============================================================================

  #  

# SPINDOWN ============================================================================

