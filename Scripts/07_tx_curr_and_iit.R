# PROJECT: Here it goes again
# PURPOSE: Munge and Analysis of IIT and TX_CURR trends
# AUTHOR: Tim Essam | SI
# REF ID:   5b0d0e46
# LICENSE: MIT
# DATE: 2023-02-01
# NOTES: Tim Essam | SI

# LOCALS & SETUP ============================================================================

  # Load Q1 functions
    source("Scripts/helper-call_all_helpers.R")
    
    library(rcartocolor)
    library(lubridate)
    library(ggtext)
    library(fontawesome)
    library(extrafont)
    library(tidytext)
    library(patchwork)
    library(ggtext)
    library(selfdestructin5)
    library(gt)
    library(cascade) # Use dev version
    library(ggpattern)
    library(gtExtras)
  
  # SI specific paths/functions  
    load_secrets()
    
  # Site level Genie file for IIT analysis
    site_path <- return_latest(folderpath = merdata, 
                               pattern = "SITE_IM_Zambia_Daily")
  
  # REF ID for plots
    ref_id <- "5b0d0e46"
    
  # Functions  
  

# LOAD DATA ============================================================================  

  df_site <- read_msd(site_path)
  df_genie <- read_msd(file_path) %>% 
    mutate(snu1 = str_remove_all(snu1, " Province")) 
    
  full_pds <- (min(df_site$fiscal_year) - 1) %>% 
    paste0("-10-01") %>% 
    as_date() %>% 
    seq.Date(convert_qtr_to_date(metadata$curr_pd), by = "3 months") %>% 
    convert_date_to_qtr()
  
  pd_brks <- str_replace(full_pds, "FY.*(1|3)$", "")    

# PEDIATRIC TX_CURR GROWTH/SHRINKAGE=====================================================
  
  df_tx <- df_genie %>% 
    filter(funding_agency == "USAID",
           indicator == "TX_CURR",
           standardizeddisaggregate == "Age/Sex/HIVStatus",
           trendscoarse != "Unknown Age") %>% 
    group_by(indicator, fiscal_year, trendscoarse, snu1) %>% 
    group_by(indicator, fiscal_year, trendscoarse, snu1) %>%
    summarise(across(c(targets, starts_with("qtr")), sum, na.rm = TRUE), .groups = "drop") %>% 
    reshape_msd("quarters") %>% 
    select(-results_cumulative)
  
  df_tx <- df_tx %>% 
    group_by(snu1, trendscoarse) %>%
    mutate(decline = results < lag(results, 1),
           decline_shp = ifelse(decline == TRUE, "\u25Bc", "\u25B2"),
           fill_color = case_when(fiscal_year < metadata$curr_fy ~ trolley_grey,
                                  decline == TRUE ~ golden_sand,
                                  TRUE ~ scooter),
           fill_alpha = ifelse(fiscal_year < metadata$curr_fy, .6, .9),
           results_latest = case_when(period == max(period) ~ results),
           decline_latest = case_when(period == max(period) ~ decline_shp)) %>% 
    fill(results_latest,decline_latest, .direction = "up") %>% 
    mutate(disp_name = glue("{snu1} {decline_latest}")) %>% 
    ungroup()  
  
  v_tx_lrg <- df_tx %>% 
    filter(period == max(period),
           trendscoarse == "<15") %>% 
    arrange(desc(results)) %>% 
    mutate(cumsum = cumsum(results)/sum(results)) %>% 
    slice_head(n = 11) %>% 
    pull(snu1)
  
  df_tx %>%
    filter(
      snu1 %ni% c("Eastern", "Southern"),
      trendscoarse == "<15") %>% 
    ggplot(aes(period, results, fill = fill_color, alpha = fill_alpha)) +
    geom_col() +
    geom_text(data = . %>% filter(period == max(period)), 
              aes(label = label_number_si()(results_latest)), 
              vjust = -.7, color = matterhorn,
              family = "Source Sans Pro") +
    facet_wrap(~fct_reorder2(disp_name, period, results), scales = "free_y") +
    scale_fill_identity() +
    scale_alpha_identity() +
    scale_y_continuous(label = label_number_si()) +
    scale_x_discrete(labels = pd_brks) +
    coord_cartesian(expand = T, clip = "off") +
    labs(x = NULL, y = NULL, 
         title = glue("OVERALL PEDIATRIC TX_CURR THROUGH {metadata$curr_pd}"),
         subtitle = glue("FY22 flagged <span style='color:{golden_sand}'>decline</span>/<span style='color:{scooter}'>growth</span>"),
         caption = glue("Source: {metadata$source}")) +
    si_style_ygrid() +
    theme(panel.spacing = unit(.5, "line"),
          plot.subtitle = element_markdown(),
          strip.text = element_markdown())
  
  si_save("Images/TX_CURR_pediatric_growth_snu.png")
  
# SHOW SHIFT IN TX_CURR AWAY from TWO DISTRICT================================================================

  df_genie %>% 
    filter(indicator == "TX_CURR", 
           standardizeddisaggregate == "Total Numerator") %>% 
    group_by(psnu, fiscal_year, indicator, funding_agency) %>% 
    summarise(across(c("targets", "cumulative"))) %>% 
    ungroup() %>% 
    filter(str_detect(psnu, "Chama|Itezhi")) %>% 
    group_by(psnu, fiscal_year) %>% 
    fill(cumulative, .direction = "updown") %>% 
    filter(!is.na(targets)) %>% 
    mutate(FY22_achv = cumulative / targets) %>% 
    ggplot(aes(x = factor(fiscal_year))) +
    geom_col(aes(y = targets), fill = grey30k, width = 0.5, position = position_nudge(x = -0.25)) +
    geom_col(aes(y = cumulative), fill = scooter_med, width = 0.5, position = position_nudge(x = 0.25)) +
    geom_text(aes(y = cumulative, label = comma(cumulative)),
              family = "Source Sans Pro",
              size = 9/.pt,
              color = grey90k,
              position = position_nudge(x = 0.25), 
              vjust = 1) +
    geom_text(aes(y = targets, label = comma(targets)), 
                  family = "Source Sans Pro",
                  size = 9/.pt,
                  color = grey90k,
                  position = position_nudge(x = -0.25), 
                  vjust = -1) +
    facet_wrap(~psnu) +
    si_style_ygrid()
  si_save("Graphics/TX_CURR_target_shift.svg", scale = 1.5)

# SPINDOWN ============================================================================

