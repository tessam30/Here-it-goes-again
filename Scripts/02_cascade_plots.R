# PROJECT: Here-it-goes-again
# PURPOSE: Analysis of FY23Q1 data for cascade plots
# AUTHOR: Tim Essam | SI
# REF ID:   3067f005
# LICENSE: MIT
# DATE: 2023-01-30
# NOTES: Tim Essam | SI

# LOCALS & SETUP ============================================================================

  # Load Q1 functions
  #devtools::install_github(repo = "USAID-OHA-SI/cascade", ref = "dev")
  source("Scripts/helper-call_all_helpers.R")
    
  library(cascade)

  # SI specific paths/functions  
    load_secrets()

      
  # Grab metadata
   get_metadata(file_path)
  
  # REF ID for plots
    ref_id <- "3067f005"
    
  # Functions  
  

# LOAD DATA ============================================================================  

    df_msd <- read_msd(file_path)  %>% 
      fix_mech_names() %>% 
      mutate(snu1 = str_remove_all(snu1, " Province")) %>% 
      clean_agency() %>% 
      swap_targets() %>% 
      filter(funding_agency == "USAID")

# CREATE USAID CASCADE ============================================================================
  
  #  USING DEV VERSION to get around TX_CURR_LAG2 missing
    
    # All of PEPFAR Zambia cascade
    return_cascade(df_msd, 1) %>% prinf()
    
    # Generate plots for all agencies
    batch_cascade_plot(df_msd, imgpath = "Images/Cascade/ZMB_PEPFAR", imgtype =".svg")
    
    batch_cascade_plot(df_msd %>% filter(funding_agency == "USAID"), 
                       imgpath = "Images/Cascade/USAID", imgtype =".svg")
  
# MECHANISM CASCADES ============================================================================

    # Loop over mechs
    
    return_cascade(df_msd %>% filter(mech_name == "ZAM Health"), 1)
    
    
    batch_cascade_plot(df_msd %>% filter(mech_name == "DISCOVER-H"),
                       imgpath = "Images/Cascade/DISCOVER", imgtype =".png")
    
    batch_cascade_plot(df_msd %>% filter(mech_name == "SAFE"),
                       imgpath = "Images/Cascade/SAFE", imgtype =".png")
    
    batch_cascade_plot(df_msd %>% filter(mech_name == "Action HIV"),
                       imgpath = "Images/Cascade/ACTION_HIV", imgtype =".png")
    
    batch_cascade_plot(df_msd %>% filter(mech_name == "ZAM Health"),
                       imgpath = "Images/Cascade/ZAM Health", imgtype =".png")

# Sparkline summaries  ============================================================================
    
    df_spark <- return_cascade(df_msd %>% filter(funding_agency == "USAID"), 1)
      # mutate(results = case_when(
      #   indicator == "TX_NET_NEW" & period == "FY22Q1" ~ 0,
      #   TRUE ~ results
      # ))
      # 
    df_spark %>% 
      mutate(start_point = case_when(
        period == min(period) ~ 1,
        TRUE ~ 0
      ),
      end_point = case_when(
        period == max(period) ~ 1,
        TRUE ~ 0
      ),
      ends = ifelse(start_point == 1 | end_point == 1, 1, 0)
      ) %>% group_by(indicator) %>% 
      mutate(min_results = min(results),
             indic_colors = dplyr::case_when(indicator == "HTS_TST" ~ "#877ec9", 
                                             indicator == "HTS_TST_POS" ~ "#b5aaf9", 
                                             indicator == "TX_NEW" ~ glitr::golden_sand_light, 
                                             indicator == "TX_NET_NEW" ~ glitr::golden_sand_light, 
                                             indicator == "TX_CURR" ~ glitr::golden_sand, 
                                             indicator == "TX_PVLS_D" ~ glitr::scooter_med, 
                                             indicator == "TX_PVLS" ~ glitr::scooter),
             indic_group = case_when(
               indicator %in% c("HTS_TST", "HTS_TST_POS") ~ "Testing",
               indicator %in% c("TX_CURR", "TX_NEW", "TX_NET_NEW") ~ "Treatment",
               indicator %in% c("TX_PVLS", "TX_PVLS_D") ~ "Viral Load"
             )
      ) %>% 
      ungroup() %>% 
      filter(indicator != "TX_CURR_Lag2") %>% 
      ggplot(aes(x = period, y = results, group = indicator)) +
      geom_ribbon(aes(ymin = min_results, ymax = results, fill = indic_colors), alpha = 0.25) +
      geom_line(size = 0.75, color = grey70k) +
      geom_point(data = . %>% filter(end_point == 1), shape = 19, color = grey80k, size = 3) +
      geom_point(data = . %>% filter(end_point == 0), shape = 19, color = grey80k, size = 1.5) +
      geom_text(data = . %>% filter(ends == 1), aes(label = label_number_si(accuracy = 1)(results), 
                                                    vjust = -1, size = 12/.pt, 
                                                    family = "Source Sans Pro"))+
      
      facet_wrap(~indicator, scales = "free_y", ncol = 2) +
      si_style_nolines() +
      scale_y_continuous(labels =  label_number_si(), expand = c(0.5, 0.5)) +
      labs(x = NULL, y = NULL,
           title = "CASCADE TRENDS FOR FY22 FOR ACTION HIV",
           caption = glue("{metadata$caption}")) +
      scale_x_discrete(expand = c(0.05, 0)) +
      theme(axis.text.y = element_blank(), 
            strip.text = element_text(size = 15),
            legend.position  = "none") +
      scale_fill_identity()

