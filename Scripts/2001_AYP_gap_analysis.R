# PROJECT: TameDP check on Zambia TaST
# PURPOSE: Munge and Analysis of TaST DRAFT
# AUTHOR: Tim Essam | SI
# REF ID:   7be07a44
# LICENSE: MIT
# DATE: 2023-03-27
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
    library(tameDP)

  # SI specific paths/functions  
    load_secrets()
    merdata <- file.path(glamr::si_path("path_msd"))
    dp_path <- "../../../Downloads/2023-04-14 Target Setting Tool_5PM.xlsx"
    
    msd_path <- return_latest(merdata, "PSNU_IM.*Zambia")
    
    nat_subnat_path <- return_latest(merdata, "SUBNAT")
  
  # Grab metadata
    get_metadata(msd_path)
  
  # REF ID for plots
    ref_id <- "7be07a44"
  
  # Grab SNU1 info
    snu1_cw <- read_psd(msd_path) %>% distinct(snu1, snu1uid, psnu, psnuuid)


# LOAD DATA ============================================================================  

  dp <- tame_dp(dp_path)
    
  df_subnat <- read_psd(nat_subnat_path) %>% filter(operatingunit == "Zambia")
  
  # df_subnat %>% filter(indicator %in% c("PLHIV", "TX_CURR_SUBNAT"), 
  #                      standardizeddisaggregate == "Age/Sex/HIVStatus",
  #                      #ageasentered %in% c("10-14", "15-19", "20-24"), 
  #                      ageasentered %in% c("<01", "01-04", "05-09", "10-14"),
  #                      fiscal_year == 2022) %>% 
  #   group_by(snu1, snu1uid, indicator, standardizeddisaggregate) %>% 
  #   summarise(total = sum(targets, na.rm = T), .groups = "drop") %>% 
  #   spread(indicator, total) %>% 
  #   ungroup() %>% 
  #   left_join(df_msd_tx) %>% 
  #   mutate(gap = PLHIV - TX_CURR_SUBNAT,
  #          gap2 = PLHIV - tx_curr_q2FY23,
  #          gap_sh = gap / sum(gap), 
  #          gap_sh2 = gap2 / sum(gap2)) %>% 
  #   arrange(gap_sh2)

    
  df_msd <- read_psd(msd_path)
  
  df_plhiv <- tame_dp(dp_path, type = 'PLHIV') %>% 
    left_join(., snu1_cw, by = c("psnuuid"))

  

# AYP Calculations --------------------------------------------------------


  # Ayp msd
  df_msd_tx <- df_msd %>% 
    filter(indicator %in% c("TX_CURR"), 
           standardizeddisaggregate == "Age/Sex/HIVStatus",
           ageasentered %in% c("15-19", "20-24"), 
           #ageasentered %in% c("<01", "01-04", "05-09", "10-14"),
           fiscal_year == 2023) %>% 
    group_by(snu1) %>% 
    summarise(tx_curr_q2FY23 = sum(cumulative, na.rm = T)) %>% 
    ungroup()
  
  df_gap_ayp <- df_plhiv %>% 
    filter(indicator %in% c("PLHIV", "TX_CURR_SUBNAT"),
                      ageasentered %in% c("15-24"), 
                      #ageasentered %in% c("<01", "01-09", "10-14"), 
                      fiscal_year == 2024) %>% 
    group_by(snu1, indicator, standardizeddisaggregate) %>% 
    summarise(total = sum(targets, na.rm = T), .groups = "drop") %>% 
    spread(indicator, total) %>% 
    left_join(df_msd_tx, by = c("snu1")) %>% 
    mutate(gap = PLHIV - TX_CURR_SUBNAT,
           gap2 = PLHIV - tx_curr_q2FY23, 
           gap_sh = gap / sum(gap),
           art_gap = gap2 / sum(gap2)) %>% 
    arrange(art_gap)
  
  #  AYP Testing
  df_msd_tst <- 
    df_msd %>% 
    filter(indicator %in% c("HTS_TST", "HTS_TST_POS"), 
           standardizeddisaggregate == "Modality/Age/Sex/Result", 
           ageasentered %in% c("15-19", "20-24"),
           fiscal_year == metadata$curr_fy, 
           str_detect(snu1, "_Mil", negate = T)) %>% 
    group_by(indicator, snu1) %>% 
    summarise(value = sum(cumulative, na.rm = T), .groups = "drop") %>% 
    pivot_wider(names_from = "indicator") %>% 
    mutate(across(c(where(is.double)), ~(.x / sum(.x, na.rm = T)), .names = "{.col}_share")) 
  
  df_gap_ayp %>% left_join(df_msd_tst) %>% 
    mutate(snu1 = str_remove_all(snu1, " Province")) %>% 
    ggplot(aes(y = HTS_TST_share, x = art_gap)) +
    geom_abline(intercept = 0, slope = 1, color = grey50k) +
    geom_point() +
    ggrepel::geom_text_repel(aes(label = snu1)) +
    scale_x_continuous(labels = percent, limits = c(0, 0.3), breaks = seq(0, 0.3, 0.05)) +
    scale_y_continuous(labels = percent, limits = c(0, 0.3), breaks = seq(0, 0.3, 0.05)) +
    si_style() +
    scale_color_viridis_c(option = "B", direction = -1)+
    labs(title = "AYP TESTING", 
         caption = glue("{metadata$caption} & COP23 FLATPACK"))
  si_save("Graphics/AYP_hts_tst_to_art_gap_scatter2.svg", height = 8, width = 8)

  # ART GAP SUMMARY
  df_gap_ayp %>% 
    mutate(snu1 = str_remove_all(snu1, " Province")) %>% 
    mutate(snu1_order = fct_reorder(snu1, art_gap)) %>% 
    ggplot(aes(y = snu1_order, x = art_gap )) +
    geom_col() +
    geom_text(aes(label = comma(gap2)), size = 10/.pt, 
              hjust = -0.1) +
    scale_x_continuous(labels = percent, limits = c(0, .35)) +
    si_style_xgrid() +
    labs(x = "Estimated ART Gap (PLHIV - TX_CURR_FY23Q2",
         y = NULL,
         title = "COPPERBELT HAS THE LARGEST ESTIMATED ART GAP FOR AYPS (15-24 years)",
         caption = glue("{metadata$caption} & COP23 FLATPACK"))
  si_save("Images/AYP_art_gap_snu1_summary.png")

# PEDs GAP summary --------------------------------------------------------

  # PEDS  msd
  df_msd_tx_peds <- df_msd %>% 
    filter(indicator %in% c("TX_CURR"), 
           standardizeddisaggregate == "Age/Sex/HIVStatus",
           ageasentered %in% c("<01", "01-04", "05-09", "10-14"),
           fiscal_year == 2023) %>% 
    group_by(snu1) %>% 
    summarise(tx_curr_q2FY23 = sum(cumulative, na.rm = T)) %>% 
    ungroup()
  
  df_gap_peds <- df_plhiv %>% 
    filter(indicator %in% c("PLHIV", "TX_CURR_SUBNAT"),
           ageasentered %in% c("<01", "01-09", "10-14"), 
           fiscal_year == 2024) %>% 
    group_by(snu1, indicator, standardizeddisaggregate) %>% 
    summarise(total = sum(targets, na.rm = T), .groups = "drop") %>% 
    spread(indicator, total) %>% 
    left_join(df_msd_tx_peds, by = c("snu1")) %>% 
    mutate(gap = PLHIV - TX_CURR_SUBNAT,
           gap2 = PLHIV - tx_curr_q2FY23, 
           gap_sh = gap / sum(gap),
           art_gap = gap2 / sum(gap2)) %>% 
    arrange(art_gap)
  
  #  PEDS Testing
  df_msd_tst_peds <- 
    df_msd %>% 
    filter(indicator %in% c("HTS_TST", "HTS_TST_POS"), 
           standardizeddisaggregate == "Modality/Age/Sex/Result", 
           ageasentered %in% c("<01", "01-04", "05-09", "10-14"),
           fiscal_year == metadata$curr_fy, 
           str_detect(snu1, "_Mil", negate = T)) %>% 
    group_by(indicator, snu1) %>% 
    summarise(value = sum(cumulative, na.rm = T), .groups = "drop") %>% 
    pivot_wider(names_from = "indicator") %>% 
    mutate(across(c(where(is.double)), ~(.x / sum(.x, na.rm = T)), .names = "{.col}_share")) 
  
  df_gap_peds %>% left_join(df_msd_tst_peds) %>% 
    arrange(HTS_TST_share)
    mutate(snu1 = str_remove_all(snu1, " Province")) %>% 
    ggplot(aes(y = HTS_TST_share, x = art_gap)) +
    geom_abline(intercept = 0, slope = 1, color = grey50k) +
    geom_point(aes(size = (HTS_TST_POS_share))) +
    ggrepel::geom_text_repel(aes(label = snu1)) +
    scale_x_continuous(labels = percent, limits = c(0, 0.35), breaks = seq(0, 0.35, 0.05)) +
    scale_y_continuous(labels = percent, limits = c(0, 0.35), breaks = seq(0, 0.35, 0.05)) +
    si_style() +
    theme(legend.position = "none") +
    labs(title = "PEDS TESTING", 
         caption = glue("{metadata$caption} & COP23 FLATPACK")) 
  
  si_save("Graphics/PEDS_hts_tst_to_art_gap_scatter.svg", height = 8, width = 8)
  
  # PEDS GAP SUMMARY
  df_gap_peds %>% 
    mutate(snu1 = str_remove_all(snu1, " Province")) %>% 
    mutate(snu1_order = fct_reorder(snu1, art_gap)) %>% 
    ggplot(aes(y = snu1_order, x = art_gap )) +
    geom_col() +
    geom_text(aes(label = comma(gap2)), size = 10/.pt, 
              hjust = -0.1) +
    scale_x_continuous(labels = percent, limits = c(0, .19)) +
    si_style_xgrid() +
    labs(x = "Estimated ART Gap (PLHIV - TX_CURR_FY23Q2",
         y = NULL,
         title = "LUSAKA HAS THE LARGEST ESTIMATED ART GAP FOR PEDS (15-24 years)",
         caption = glue("{metadata$caption} & COP23 FLATPACK"))
    