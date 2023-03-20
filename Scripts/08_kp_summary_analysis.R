# PROJECT: Here it goes again
# PURPOSE: Munge and Analysis of Key Population Data
# AUTHOR: Tim Essam | SI
# REF ID:   0aea9a5a
# LICENSE: MIT
# DATE: 2023-02-06
# NOTES: Tim Essam | SI

# LOCALS & SETUP ============================================================================

  # Libraries
  source("Scripts/helper-call_all_helpers.R")
      
  # Grab metadata
   get_metadata(file_path)
  
  # REF ID for plots
    ref_id <- "0aea9a5a"
    
  # Functions  
  

# LOAD DATA ============================================================================  

  df_genie <- read_psd(file_path)

# MUNGE ============================================================================
  
  df_kp <- df_genie %>% 
      filter(indicator %in% c("HTS_TST", "HTS_TST_POS", "TX_CURR", "TX_NEW")) %>% 
      clean_indicator() %>% 
      filter(standardizeddisaggregate %in% c("KeyPop/Result", 
                                             "KeyPop/HIVStatus", 
                                             "KeyPop/Indication/HIVStatus"),
             fiscal_year == metadata$curr_fy) %>% 
      group_by(indicator, fiscal_year, otherdisaggregate) %>% 
      summarise(across(c(targets, cumulative), sum, na.rm = T), .groups = "drop") %>% 
      mutate(achv = cumulative / targets)
      
    
  df_kp_tx <- df_genie %>% 
    filter(indicator %in% c("TX_NEW", "TX_PVLS", "TX_CURR")) %>% 
    clean_indicator() %>% 
    filter(standardizeddisaggregate %in% c("KeyPop/HIVStatus", "KeyPop/Indication/HIVStatus"),
           fiscal_year == metadata$curr_fy - 1, 
           otherdisaggregate != "MSM, Targeted") %>% 
    mutate(otherdisaggregate = str_replace_all(otherdisaggregate, ", Routine", "")) %>%
    group_by(indicator, fiscal_year, otherdisaggregate) %>% 
    summarise(across(c(targets, cumulative), \(x) sum(x, na.rm = T)), .groups = "drop") %>% 
    mutate(achv = cumulative / targets)
  
  df_kp_vl <- df_genie %>% 
    filter(indicator %in% c( "TX_CURR", "TX_PVLS")) %>% 
    clean_indicator() %>% 
    filter(standardizeddisaggregate %in% c("KeyPop/HIVStatus", "KeyPop/Indication/HIVStatus")) %>% 
    mutate(otherdisaggregate = str_replace_all(otherdisaggregate, ", Targeted|, Routine", "")) %>%
    group_by(indicator, fiscal_year, otherdisaggregate) %>% 
    summarise(across(matches("qtr"), \(x) sum(x, na.rm = T)), .groups = "drop") %>% 
    reshape_msd() %>% 
    pivot_wider(names_from = indicator,
                values_from = "value") %>% 
    arrange(otherdisaggregate, period) %>% 
    mutate(TX_CURR_LAG2 = lag(TX_CURR, n = 2), .by = otherdisaggregate,
           VLS = TX_PVLS / TX_PVLS_D,
           VLC = TX_PVLS_D / TX_CURR_LAG2)
    
# VIZ ============================================================================

    df_kp_tx %>% 
      ggplot(aes(x = otherdisaggregate)) +
      geom_col(aes(y = targets), fill = grey20k, width = 0.6, position = position_nudge(x = -0.1)) +
      geom_col(aes(y = cumulative, fill = indicator), width = 0.6) +
      facet_wrap(~indicator, scales = "free_y") +
      geom_text(aes(y = cumulative, label = percent(achv, 1)),
                size = 12/.pt,
                family = "Source Sans Pro",
                color = grey90k,
                vjust = -0.25
                ) +
      scale_fill_si(palette = "siei", discrete = T) +
      scale_x_discrete(labels = function(x) str_wrap(x, width = 20)) +
      scale_y_continuous(labels = comma) +
      si_style_ygrid() +
      theme(legend.position = "none") +
      labs(x = NULL, y = NULL, 
            title = glue("KP Summary for FY22"),
           caption = glue("{metadata$caption}"))
    si_save(glue("Images/ZMB_{metadata$curr_pd}_kp_achv_by_disag_tx.png"))
   
    
    num_pds <- df_kp_vl %>% filter(period >= "FY22Q1") %>% distinct(period) %>% count() %>% pull()
    
    df_kp_vl %>% 
      filter(period >= "FY22Q1") %>% 
      select(-c(TX_CURR, period_type, TX_PVLS, TX_PVLS_D, TX_CURR_LAG2)) %>% 
      rename(vls = VLS, vlc = VLC) %>% 
      # pivot_longer(cols = VLS:VLC,
      #              names_to = "VL",
      #              values_to = "values") %>% 
      # mutate(fill_color = ifelse(VL == "VLC", burnt_sienna, denim)) %>% 
      ggplot(aes(x = period, group = 1)) + 
      geom_line(aes(y = vls), color = burnt_sienna) +
      geom_point(aes(y = vls), shape = 21, fill = burnt_sienna, size = 3,
                 color = "white") +
      geom_line(aes(y = vlc), color = denim) +
      geom_point(aes(y = vlc), shape = 21, fill = denim, size = 3,
                 color = "white") +
      facet_wrap(~otherdisaggregate) +
      scale_color_identity() +
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
      si_style_ygrid() +
      expand_limits(x = c(1, num_pds+0.5), y = c(0.7,1.15)) +
      scale_y_continuous(labels = percent, breaks = seq(0.25, 1, 0.25)) +
      labs(x = NULL, y = NULL,
           title = glue("KP VIRAL LOAD COVERAGE & SUPPRESSION TRENDS"),
           caption = glue("{metadata$caption}"))
    si_save(glue("Graphics/ZMB_{metadata$curr_pd}_kp_achv_by_disag_vl.svg"))
    
# SPINDOWN ============================================================================

