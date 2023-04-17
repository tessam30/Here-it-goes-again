# PROJECT: Provincial pull of Targets for TX_CURR
# PURPOSE: Munge and Analysis of TX_CURR by SNU1
# AUTHOR: Tim Essam | SI
# REF ID:   3a665f2f
# LICENSE: MIT
# DATE: 2023-04-17
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
    library(gt)
    library(gtExtras)
    
  # SI specific paths/functions  
    load_secrets()  
    merdata <- file.path(glamr::si_path("path_msd"))
    file_path <- return_latest(folderpath = merdata,
      pattern = "PSNU_IM_FY21-23_20230317_v2_1_Zambia.zip")
    
    tst_path <- "../../../Downloads/2023-11-04 Target Setting Tool_16hrs_ForTim 13Apr2023.xlsx"
      
  # Grab metadata
   get_metadata(file_path)
  
  # REF ID for plots
    ref_id <- "3a665f2f"
    
  # Functions  
  

# LOAD DATA ============================================================================  

  df_msd <- read_psd(file_path)



  df_dp <- tame_dp(tst_path) %>% filter(indicator == "TX_CURR", 
                                        standardizeddisaggregate == "Age/Sex/HIVStatus", 
                                        fiscal_year == 2024) 

    
# MUNGE ============================================================================
  
   # Pull CW of PSNUs so we can collapse to SNU1 level
  df_tx_snu <- df_msd %>% 
      filter(indicator == "TX_CURR", standardizeddisaggregate == "Age/Sex/HIVStatus") %>% 
      filter(standardizeddisaggregate == "Age/Sex/HIVStatus", fiscal_year > 2021) %>% 
      group_by(fiscal_year, indicator, snu1) %>% 
      summarize(across(c(cumulative, targets), \(x) sum(x, na.rm = T)), .groups = "drop")  

  df_psnu_cw <-  df_msd %>% distinct(snu1, snu1uid, psnu, psnuuid)
  
  df_dp_tg <- df_dp %>% 
    left_join(., df_psnu_cw, by = c("psnuuid", "psnu")) %>%
    group_by(fiscal_year, indicator, snu1) %>% 
    summarize(across(c(cumulative, targets), \(x) sum(x, na.rm = T)), .groups = "drop")  
    

  df_tx_snu %>% 
    bind_rows(df_dp_tg) %>% 
    mutate(achv = cumulative / targets) %>%
    select(-indicator) %>% 
    ggplot(aes(x = fiscal_year)) +
    geom_col(aes(y = targets), fill = grey20k, position = position_nudge(x = 0.2)) +
    geom_col(aes(y = cumulative), fill = scooter_med) +
    geom_text(aes(y = cumulative, label = percent(achv, 1))) +
    si_style() + 
    facet_wrap(~snu1, scales = "free_y") +
    scale_y_continuous(labels = comma)
  
  
# VIZ ============================================================================

  df_tx_snu %>% 
    bind_rows(df_dp_tg) %>% 
    select(-indicator) %>% 
    pivot_longer(cols = cumulative:targets, names_to = "type", values_to = "val") %>% 
    mutate(type = str_c(fiscal_year, type, sep = "_")) %>% 
    select(-fiscal_year) %>% 
    spread(type, val) %>% 
    arrange(desc(`2024_targets`)) %>% 
    mutate(`2024_cumulative` = NA_integer_,
           snu1 = str_replace_all(snu1, " Province", "")) %>% 
    mutate(`2022_achv` = `2022_cumulative`/`2022_targets`, .after = `2022_targets`) %>% 
    mutate(`2023_achv` = `2023_cumulative`/`2023_targets`, .after = `2023_targets`) %>% 
    mutate(`2024_achv` = `2024_cumulative`/`2024_targets`, .after = `2024_targets`) %>%
    gt() %>% 
    fmt_number(columns = c(2:3, 5:6, 9), decimals = 0) %>% 
    fmt_percent(columns = c(4, 7), decimals = 0) %>% 
    sub_missing(missing_text = ".") %>% 
    gt_theme_nytimes() %>% 
    cols_label(`2022_cumulative` = "results",
               `2023_cumulative` = "results",
               `2024_cumulative` = "results",
               `2022_targets` = "targets",
               `2023_targets` = "targets", 
               `2024_targets` = "targets", 
               `2022_achv` = "achv",
               `2023_achv` = "achv",
               `2024_achv` = "achv") %>% 
    tab_spanner(columns = 2:4, label = "COP21") %>% 
    tab_spanner(columns = 5:7, label = "COP22") %>% 
    tab_spanner(columns = 8:10, label = "COP23") %>% 
    tab_header(
      title = glue("PROVINCIAL SUMMARY TX_CURR TARGETS & RESULTS")
    ) %>% 
    tab_source_note(
      source_note = gt::md(glue("Source: Datapack COP23 2023-11-04  draft & {metadata$source} | Ref id: {ref_id}"))) %>% 
    tab_options(
      source_notes.font.size = px(10)) %>% 
    tab_style(
      style = list(
        cell_text(weight = 700)
      ),
      locations = cells_body(
        columns = c(3, 6, 9)
      )
    ) %>% 
    gtsave_extra(filename = glue("Images/ZMB_provincial_summary_tx_curr_trends.png"))

# SPINDOWN ============================================================================

