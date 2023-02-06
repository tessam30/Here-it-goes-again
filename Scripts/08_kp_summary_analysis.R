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

  df_genie <- read_msd(file_path)

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
      
  
# VIZ ============================================================================

    df_kp %>% 
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
            title = glue("KP Summary for {metadata$curr_pd}"),
           caption = glue("{metadata$caption}"))
    si_save(glue("Images/ZMB_{metadata$curr_pd}_kp_achv_by_disag.png"))
   

# SPINDOWN ============================================================================

