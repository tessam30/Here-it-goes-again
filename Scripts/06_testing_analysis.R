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
    pull_bounds <- function(df, age = "<15", var, bound = ""){
      
      if(bound == "max"){
      df %>% 
        filter(trendscoarse == age) %>% 
        summarise(max = max({{var}})) %>% 
        pull()
      }
      
      else if(bound == "min"){
        df %>% 
          filter(trendscoarse == age) %>% 
          summarise(min = min({{var}})) %>% 
          pull()
      }
    }
  

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
  
# LINKAGE VIZ ============================================================================

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
    
# OVERALL TESTING TRENDS --------------------------------------------------
    
    df_hts_combo <- df_hts_base %>% 
      select(-targets) %>% 
      spread(indicator, results) %>% 
      mutate(positivity = HTS_TST_POS / HTS_TST)
    
    
    df_hts_combo %>% 
      ggplot(aes(x = period)) +
      geom_col(aes(y = HTS_TST), fill = "#e0d4db", width = 0.75,
               position = position_nudge(x = 0.1)) +
      geom_col(aes(y = HTS_TST_POS), fill = "#855C75", width = 0.75) +
      geom_text(aes(y = HTS_TST_POS, label = percent(HTS_TST_POS/HTS_TST, 1)),
                size = 11/.pt, 
                family = "Source Sans Pro", 
                color = grey90k,
                vjust = -0.5) +
      #geom_col(aes(y = HTS_TST_cmltv), width = 0.5, fill = grey50k) +
      #geom_col(aes(y = HTS_TST_POS_cmltv), width = 0.5, fill = "#855C75") +
      si_style_ygrid() +
      scale_y_continuous(labels = label_number_si()) +
      labs(x = NULL, y = NULL, title = "TESTING REMAINS HIGH BUT POSITIVITY IS DOWN",
           caption = metadata$caption)+
      coord_cartesian(expand = F)
    si_save("Graphics/HTS_positivity_summary.svg")
    
    
    

# Testing by age / snu1 ============================================================================

  #TODO: Need to find a cleaner way to do the second half of this  
    
    df_hts_age_geo <- 
      df_genie %>% 
      filter(indicator %in% c("HTS_TST_POS", "TX_NEW", "HTS_TST"), 
             standardizeddisaggregate %in% c("Modality/Age/Sex/Result", "Age/Sex/HIVStatus"),
             fiscal_year <= metadata$curr_fy, 
             funding_agency == "USAID", 
             trendscoarse != "Unknown Age") %>% 
      group_by(indicator, fiscal_year, trendscoarse, snu1) %>% 
      summarise(across(targets:qtr4, sum, na.rm = T), .groups = "drop") %>% 
      reshape_msd(direction ="semi-wide") %>% 
      group_by(indicator, trendscoarse, snu1) %>% 
      fill(targets, .direction = "down")  %>% 
      filter(nchar(period) != 4) %>% 
      ungroup()

# PLOTS TESTING / TST_POS by GEOGRAPHY AGE
# What is the share represented by each age/geo for each period?
    
   df_hts_viz <-  df_hts_age_geo %>% 
      filter(snu1 %ni% c("Southern", "Western")) %>% 
      select(-targets) %>% 
      spread(indicator, results) %>% 
      group_by(period, trendscoarse) %>% 
      mutate(share = HTS_TST / sum(HTS_TST, na.rm = T),
             share_check = sum(share),
             share_pos = HTS_TST_POS / sum(HTS_TST_POS, na.rm = T),
             share_pos_check = sum(share_pos)) %>% 
      ungroup() %>% 
      mutate(share_label = case_when(
        period %in% c(max(period), min(period))  ~ share, 
        TRUE ~ NA_real_
      ),
      share_label_pos = case_when(
        period %in% c(max(period), min(period))  ~ share_pos, 
        TRUE ~ NA_real_
      ), 
      snu1_order = fct_reorder2(snu1, HTS_TST, share_label, .desc = T)
      )
      
   #SNU1 list
   snu1_list <- c("Muchinga", "NorthWestern", "Lusaka", "Eastern")
   
   max_peds <- pull_bounds(df_hts_viz, var = HTS_TST, bound = "max") * 1.10
   max2_peds <- pull_bounds(df_hts_viz %>% filter(snu1 %in% snu1_list),
                            var = HTS_TST, bound = "max")
   max_adults <- pull_bounds(df_hts_viz, age = "15+", 
                             var = HTS_TST, bound = "max")
   max2_adults <- pull_bounds(df_hts_viz %>% filter(snu1 %in% snu1_list),
                             age = "15+", var = HTS_TST, bound = "max")   
     
   peds_share_note <- 
     df_hts_viz %>% filter(trendscoarse == "<15", period == max(period)) %>% 
     select(snu1, share) %>% 
     slice_max(share, n = 2) %>% 
     summarize(tot = sum(share)) %>% 
     pull()
   
   df_hts_viz %>% 
     mutate(viz_bounds = case_when(
       snu1 %ni% snu1_list & trendscoarse == "<15" ~ max_peds,
       snu1 %in% snu1_list & trendscoarse == "<15" ~ max2_peds,
       snu1 %ni% snu1_list & trendscoarse == "15+" ~ max_adults,
       snu1 %in% snu1_list & trendscoarse == "15+" ~ max2_adults,
      )
     ) %>% 
      ggplot(aes(x = period, group = trendscoarse)) +
      geom_blank(aes(y = viz_bounds, group = trendscoarse)) +
      geom_col(aes(y = HTS_TST), fill = "#AA4499", alpha = 0.9) +
      facet_wrap(trendscoarse ~ snu1_order, scales = "free_y",
                 labeller = labeller(.multi_line = FALSE)) +
      geom_text(aes(y = HTS_TST, label = percent(share_label, 1.00)),
                size = 9/.pt,
                family = "Source Sans Pro",
                fontface = "bold", 
                color = grey90k, 
                vjust = -0.5) +
      si_style_ygrid() +
     scale_x_discrete(breaks = every_nth(n = 4)) +
     scale_y_continuous(labels = label_number_si()) +
     labs(x = NULL, y = NULL,
          title = glue("HTS_TST VOLUME BY PROVINCE AND COARSE AGE"),
          subtitle = glue("Copperbelt and Central contributed {percent(peds_share_note)} of all pediatric tests in {metadata$curr_pd}"),
          caption = glue("{metadata$caption}")) 
   si_save("Graphics/HTS_TST_age_geograophy_trends.svg")
     
   
# HTS_TST_POS
   
   max_peds_pos <- pull_bounds(df_hts_viz, var = HTS_TST_POS, bound = "max") * 1.10
   max2_peds_pos <- pull_bounds(df_hts_viz %>% filter(snu1 %in% snu1_list),
                            var = HTS_TST_POS, bound = "max")
   max_adults_pos <- pull_bounds(df_hts_viz, age = "15+", 
                             var = HTS_TST_POS, bound = "max")
   max2_adults_pos <- pull_bounds(df_hts_viz %>% filter(snu1 %in% snu1_list),
                              age = "15+", var = HTS_TST_POS, bound = "max")  
    
   
   tst_pos_note <- 
     df_hts_viz %>% filter( period == max(period)) %>% 
     select(snu1, HTS_TST_POS, trendscoarse) %>% 
     group_by(trendscoarse) %>% 
     #slice_max(share, n = 2) %>% 
     summarize(tot = sum(HTS_TST_POS)) %>% 
     pull()
   
   df_hts_viz %>% 
     mutate(viz_bounds = case_when(
       snu1 %ni% snu1_list & trendscoarse == "<15" ~ max_peds_pos,
       snu1 %in% snu1_list & trendscoarse == "<15" ~ max2_peds_pos,
       snu1 %ni% snu1_list & trendscoarse == "15+" ~ max_adults_pos,
       snu1 %in% snu1_list & trendscoarse == "15+" ~ max2_adults_pos,
     )
     ) %>% 
     ggplot(aes(x = period, group = trendscoarse)) +
     geom_blank(aes(y = viz_bounds, group = trendscoarse)) +
     geom_col(aes(y = HTS_TST_POS), fill = "#44AA99", alpha = 0.9) +
     facet_wrap(trendscoarse ~ snu1_order, scales = "free_y",
                labeller = labeller(.multi_line = FALSE)) +
     geom_text(aes(y = HTS_TST_POS, label = percent(share_label_pos, 1.00)),
               size = 9/.pt,
               family = "Source Sans Pro",
               fontface = "bold", 
               color = grey90k, 
               vjust = -0.5) +
     si_style_ygrid() +
     scale_x_discrete(breaks = every_nth(n = 4)) +
     scale_y_continuous(labels = label_number_si()) +
     labs(x = NULL, y = NULL,
          title = glue("HTS_TST_POS BY PROVINCE AND COARSE AGE"),
          subtitle = glue("Testing identified {tst_pos_note[1]} pediatric and {comma(tst_pos_note)[2]} adult cases in {metadata$curr_pd}"),
          caption = glue("{metadata$caption}")) 
   si_save("Graphics/HTS_TST_POS_age_geograophy_trends.svg")
   
   # Yield Heatbox to go below
   df_hts_viz %>% 
     mutate(positivity = HTS_TST_POS / HTS_TST) %>% 
     ggplot(aes(x = period, y = 1)) +
     geom_col(aes(y = 14), fill = "white") +
     geom_col(aes(y = 2, fill = positivity)) +
     geom_text(aes(label = percent(positivity, 1.00)),
               size = 7/.pt,
               family = "Source Sans Pro",
               color = grey90k) +
     facet_wrap(trendscoarse ~ snu1_order, scales = "free_y",
                labeller = labeller(.multi_line = FALSE)) +
     scale_fill_carto_c(palette = "Sunset",
                        limits = c(0, .15), 
                        oob = scales::squish) +
     si_style_ygrid() +
     scale_x_discrete(breaks = every_nth(n = 4)) +
     theme(legend.position = "none")
   
   si_save("Graphics/HTS_TST_POS_heatmap_age_geograophy_trends.svg")
   

