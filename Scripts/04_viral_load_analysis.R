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

  df_genie_in <- read_msd(file_path)
    
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
    # NOTE: VLC is a much different number than what was shown in Q4
    # FY22Q4 analysis used the TX_CURR_LAG2 created by DATIM folks
    # FY23Q1 uses the unadjusted Lag, so VLC looks worse than it is
    # ZAM HEALTH has the same issue, this is why we see no VLC in 2 periods
    df_vl_ip <- df_genie %>% 
      filter(funding_agency == "USAID",
             mech_name %ni% c("Placeholder - 86412")) %>%
      create_vl_df(mech_name) %>% 
      ungroup() %>% 
      filter(str_detect(period, "20", negate = T)) %>% 
      mutate(mech_name = fct_relevel(mech_name, c("SAFE", "Action HIV", "DISCOVER-H",
                                                  "ZAM Health")))
    

# VIZ VLS / VLC USAID ---------------------------------------------------------------

    #TODO: make the two viz components into reusable functions
    # parameters 
    # - number of periods
    # - coverage gap
    # - annotation start & stop points
    # - vlc and vls label points in x and y space
    
    num_pds <- length(unique(df_vl$period))
    
    top <- df_vl_peds %>% 
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
  
# PEDS ============================================================================

    top <- df_vl_peds %>% 
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
      annotate("text", x = num_pds + .5, y = .74, label = "Viral Load\nCoverage",
               color = denim, size = 10/.pt,
               hjust = 0.1, 
               family = "Source Sans Pro") +
      si_style_nolines() +
      expand_limits(x = c(1, num_pds+2), y = c(0.7,1.05)) +
      theme(axis.text.y = element_blank(), 
            axis.text.x = element_blank()) +
      labs(x = NULL, y = NULL)
    
    bottom <- df_vl_peds %>% 
      ggplot(aes(x = period)) +
      geom_col(aes(y = tx_curr_lag2), fill = grey10k) +
      geom_col(aes(y = tx_pvls_d), fill = denim) +
      si_style_ygrid() +
      scale_y_continuous(labels = comma) +
      expand_limits(x = c(1, num_pds+2)) +
      labs(x = NULL, y = NULL) +
      annotate("segment", x = num_pds + .5, xend = num_pds + .5, y = 13173, yend = 17682, 
               color = grey70k) +
      annotate("text", x = num_pds + .65, y = 15682, label = "Coverage gap", 
               hjust = 0, size = 8/.pt, family = "Source Sans Pro", 
               color = grey70k)+
      annotate("text", x = num_pds+1, y = 17682, label = "TX_CURR_LAG2", 
               size = 8/.pt, family = "Source Sans Pro", color = grey50k) +
      annotate("text", x = num_pds+1, y = 13173, label = "TX_PVLS_D", 
               size = 8/.pt, family = "Source Sans Pro", color = denim)
    
      top / bottom + plot_layout(heights = c(1, 3)) +
      plot_annotation(title = glue("PEDIATRIC VIRAL LOAD SUMMARY FOR {metadata$curr_fy}"),
                      caption = metadata$caption) &
      theme(plot.tag = element_text(family = "Source Sans Pro"))
      
      si_save("Graphics/VL_summary_peds_2022.svg")
      
# BY PARTNER SUMMARY ============================================================================

      # IP VERSION ON 1 GRAPH using small multiples
      top_ip <- 
        df_vl_ip %>% 
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
        si_style_nolines(facet_space = 0.5) +
        facet_wrap(~mech_name, nrow = 1) +
        theme(axis.text.y = element_blank(), 
              axis.text.x = element_blank()) +
        labs(x = NULL, y = NULL) +
        expand_limits(y = c(0.7,1)) 
      
      bottom_ip <- 
        df_vl_ip %>% 
        ggplot(aes(x = period)) +
        geom_col(aes(y = tx_curr_lag2), fill = grey10k) +
        geom_col(aes(y = tx_pvls_d), fill = denim) +
        si_style_ygrid(facet_space = 0.5) +
        scale_y_continuous(labels = comma) +
        labs(x = NULL, y = NULL) +
        facet_wrap(~mech_name, nrow = 1) +
        scale_x_discrete(labels = c("FY21Q1", "", "", "",
                                    "FY22Q1", "", "", "", 
                                    "FY23Q1")) +
        coord_cartesian(expand = F) +
        #get rid of facet labels
        theme(strip.text.x = element_blank())+
        labs(caption = metadata$caption)
      
      plot_ip <- top_ip / bottom_ip + plot_layout(heights = c(1, 3)) +
        plot_annotation(title = glue("VIRAL LOAD SUMMARY FOR {metadata$curr_fy} BY PARTNER")) &
        theme(plot.tag = element_text(family = "Source Sans Pro"))
      
      si_save("Graphics/VL_summary_partners.svg")
      

# VIRAL LOAD BY PROVINCES -------------------------------------------------
      
      df_vl_snu1 <- df_genie %>% 
        filter(funding_agency == "USAID",
               indicator %in% c("TX_CURR","TX_CURR_Lag2", "TX_PVLS"),
               standardizeddisaggregate %in% c("Age/Sex/HIVStatus", "Age/Sex/Indication/HIVStatus")) %>% 
        clean_indicator() %>% 
        group_by(snu1, indicator, fiscal_year) %>% 
        summarise(across(starts_with("qtr"), sum, na.rm = TRUE), .groups = "drop") %>% 
        reshape_msd(include_type = FALSE) %>% 
        pivot_wider(names_from = indicator,
                    names_glue = "{tolower(indicator)}") %>% 
        arrange(snu1, period) %>% 
        group_by(snu1) %>% 
        mutate(tx_curr_lag2 = lag(tx_curr, n = 2),
               vlc = tx_pvls_d/tx_curr_lag2,
               vls = tx_pvls / tx_pvls_d,
               vls_adj = tx_pvls /tx_curr_lag2) %>% 
        ungroup()
      
      
      df_vl_snu1 %>% 
        filter(snu1 != "Lusaka", 
               str_detect(period, "20", negate = T)) %>% 
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
        facet_wrap(~snu1) +
        #scale_x_discrete(breaks = every_nth(n = 4)) +
        si_style_nolines(facet_space = 0.5) +
        expand_limits(x = c(1, 10), y = c(0.7,1.05)) +
        theme(axis.text.y = element_blank()) +
        labs(x = NULL, y = NULL) +
        labs(title = "USAID VIRAL LOAD COVERAGE AND VIRAL LOAD SUPPRESSION TRENDS", 
             caption = glue("{metadata$caption}"))
      si_save("Graphics/ZMB_vls_vlc_by_snu1.svg")      
      
# VIRAL LOAD BY AGE/SEX -------------------------------------------------
      
      df_vl <- df_genie %>% 
        filter(funding_agency == "USAID",
               indicator %in% c("TX_CURR","TX_CURR_Lag2", "TX_PVLS"),
               standardizeddisaggregate %in% c("Age/Sex/HIVStatus", "Age/Sex/Indication/HIVStatus"),
               ageasentered != "Unknown Age") %>% 
        mutate(ageasentered = case_when(trendscoarse == "<15" ~ trendscoarse,
                                        ageasentered %in% c("50-54","55-59", "60-64", "65+") ~ "50+",
                                        TRUE ~ ageasentered)) %>% 
        clean_indicator() %>% 
        group_by(snu1, indicator, ageasentered, fiscal_year) %>% 
        summarise(across(starts_with("qtr"), sum, na.rm = TRUE), .groups = "drop") %>% 
        reshape_msd(include_type = FALSE) %>% 
        pivot_wider(names_from = indicator,
                    names_glue = "{tolower(indicator)}")
      
      df_vl <- df_vl %>% 
        arrange(snu1, ageasentered, period) %>% 
        group_by(snu1, ageasentered) %>% 
        mutate(tx_curr_lag2 = lag(tx_curr, n = 2),
               vlc = tx_pvls_d/tx_curr_lag2,
               vls = tx_pvls / tx_pvls_d,
               vls_adj = tx_pvls /tx_curr_lag2) %>% 
        ungroup() %>% 
        filter(period == max(period))
      
      df_avg <- df_vl %>% 
        group_by(snu1) %>% 
        summarise(avg_vlc = mean(vlc),
                  avg_vls = mean(vls),
                  avg_vls_adj = mean(vls_adj)) %>% 
        ungroup()
      
      
      df_usaid_vl <- df_vl %>% 
        summarise(vlc = sum(tx_pvls_d, na.rm = TRUE)/sum(tx_curr_lag2, na.rm = TRUE),
                  vls = sum(tx_pvls, na.rm = TRUE) / sum(tx_pvls_d, na.rm = TRUE),
                  vls_adj = sum(tx_pvls, na.rm = TRUE) /sum(tx_curr_lag2, na.rm = TRUE))
      
      
      df_vl %>% 
        mutate(tx_curr_pct = 1) %>% 
        ggplot(aes(tx_curr_pct, ageasentered)) +
        geom_vline(data = df_avg, aes(xintercept = avg_vlc, color = grey80k)) +
        geom_col(fill = grey10k, alpha = 0.6) +
        geom_col(aes(vls), fill = scooter_light, alpha = 0.75, width = 0.75) +
        geom_col(aes(vlc), fill = scooter, alpha = 1, width = 0.75) +
        geom_vline(xintercept = 1, linewidth = 0.5, color = grey30k) +
        geom_vline(xintercept = 0, linewidth = 0.5, color = grey30k) +
        geom_richtext(aes(vls, label = glue("<span style='color:#505050'>{percent(vls, 1)}</span>")),
                      label.color = NA, fill = NA,
                      nudge_x = -0.04, size = 3,
                      family = "Source Sans Pro") +
        geom_richtext(aes(vlc, label = glue("<span style='color:white'>{percent(vlc, 1)}</span>")),
                      label.color = NA, fill = NA,
                      nudge_x = -0.04, size = 3,
                      family = "Source Sans Pro") +
        geom_text(data = df_avg, aes(x = avg_vlc, y = avg_vlc, label = percent(avg_vlc, 1)),
                  hjust = .3, vjust = 1.6,
                  family = "Source Sans Pro",
                  color = grey90k, size = 10/.pt) +
        facet_wrap(~snu1) +
        coord_cartesian(expand = T, clip = "off") +
        scale_color_identity() +
        scale_x_continuous(label = percent) +
        labs(x = NULL, y = NULL, 
             title = glue("In {metadata$curr_pd}, USAID VLC and VLS are at {percent(df_usaid_vl$vlc, 1)} and {percent(df_usaid_vl$vls, 1)} respectively") %>% toupper,
             subtitle = glue("<span style='color:{scooter}'>**VLC**</span> and <span style='color:{scooter_light}'>**VLS**</span> rates"),
             caption = glue("Source: {metadata$source}")) +
        si_style_nolines() +
        theme(panel.spacing = unit(.5, "line"),
              plot.subtitle = element_markdown(),
              strip.text = element_markdown(),
              axis.text.x = element_blank()) 
      
      si_save("Graphics/VLC_VLS_by_province.svg")
      