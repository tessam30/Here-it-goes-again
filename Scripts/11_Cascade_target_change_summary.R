# PROJECT: Here it goes again 
# PURPOSE: Munge and Analysis of target shifts in FY23
# AUTHOR: Tim Essam | SI
# REF ID:   2f07484e
# LICENSE: MIT
# DATE: 2023-02-02
# NOTES: Tim Essam | SI

# LOCALS & SETUP ============================================================================


    # Load Q1 functions
    source("Scripts/helper-call_all_helpers.R")


# LOAD DATA ============================================================================  

   df_genie <- read_msd(file_path) %>% 
     fix_mech_names() %>% 
     clean_agency() %>% 
     swap_targets() 
   

# Want a summary of cascade level indicator target shifts  =========================================
  
   df_tgts <- df_genie %>% 
     filter(funding_agency == "USAID", 
            indicator %in% c("HTS_TST", "HTS_TST_POS", "TX_NEW", "TX_CURR", "TX_PVLS"),
            standardizeddisaggregate %in% c("Total Numerator", "Total Denominator")) %>% 
    clean_indicator() 
   
   df_tgts_all <- df_tgts %>% 
     bind_rows(df_tgts %>% mutate(mech_name = "USAID")) %>% 
    group_by(mech_name, indicator, fiscal_year) %>% 
     summarise(tgts = sum(targets, na.rm = T)) %>% 
     ungroup() %>% 
     spread(fiscal_year, tgts) %>% 
     mutate(delta = `2023`-`2022`,
            pct_delta = (`2023` - `2022`)/`2022`) %>% 
     filter(str_detect(mech_name, "USAID|SAFE|DIS|Action|Plac")) %>% 
     mutate(mech_name = ifelse(str_detect(mech_name, "Place"), "ZIHA", mech_name)) %>% 
     mutate(mech_name = fct_relevel(mech_name, c("USAID", "SAFE", "Action HIV", 
                                                       "DISCOVER-H", "ZIHA"))) %>% 
     arrange(mech_name) %>% 
     mutate(decline_shp = ifelse(pct_delta <0, "\u25Bc", "\u25B2"))
     
     
    df_tgts_all %>% 
      filter(mech_name == "USAID") %>% 
      gt(groupname_col = "mech_name") %>% 
      fmt_number(columns = 3:5, 
                 decimals = 0) %>% 
      fmt_percent(columns = 6, decimals = 0) %>% 
      gt_hulk_col_numeric(6, trim = TRUE, ) %>% 
      cols_label(indicator = "",
                 pct_delta = "% change",
                 decline_shp = "") %>% 
      tab_source_note(
        source_note = gt::md(glue("{metadata$caption}"))) %>% 
      tab_options(
        source_notes.font.size = px(10)) %>% 
      tab_header(
        title = glue("TARGET CHANGE SUMMARY: FY22 TO FY23"),
      ) %>% 
      gt_theme_nytimes() %>% 
      gtsave_extra("Images/USAID_target_change_summary_table.png")
    
    
    df_tgts_all %>% 
      filter(mech_name %ni% c("USAID", "ZIHA")) %>% 
      gt(groupname_col = "mech_name") %>% 
      fmt_number(columns = 3:5, 
                 decimals = 0) %>% 
      fmt_percent(columns = 6, decimals = 0) %>% 
      gt_hulk_col_numeric(6, trim = TRUE, ) %>% 
      cols_label(indicator = "",
                 pct_delta = "% change",
                 decline_shp = "") %>% 
      tab_source_note(
        source_note = gt::md(glue("{metadata$caption}"))) %>% 
      tab_options(
        source_notes.font.size = px(10)) %>% 
      tab_header(
        title = glue("TARGET CHANGE SUMMARY: FY22 TO FY23"),
      ) %>% 
      gt_theme_nytimes() %>% 
      gtsave_extra("Images/USAID_partner_target_change_summary_table.png")
    
    
    df_tgts_all %>% 
      filter(mech_name %in% c("ZIHA")) %>% 
      select(-c(delta, pct_delta, decline_shp)) %>% 
      gt(groupname_col = "mech_name") %>% 
      fmt_number(columns = 4, 
                 decimals = 0) %>% 
      sub_missing(columns = 3, 
                  missing_text = "-") %>% 
      tab_source_note(
        source_note = gt::md(glue("{metadata$caption}"))) %>% 
      tab_options(
        source_notes.font.size = px(10)) %>% 
      tab_header(
        title = glue("ZIHA FY23 TARGETS"),
      ) %>% 
      gt_theme_nytimes() %>% 
    gtsave_extra("Images/USAID_ZIHA_target_change_summary_table.png")
    
   
    
  
# VIZ ============================================================================

  df_tgts_all %>% 
       filter(mech_name == "USAID") %>% 
       ggplot(aes(x = factor(fiscal_year), y = tgts)) +
       geom_col() +
       facet_wrap(~indicator, scales = "free_y", nrow = 1)

# SPINDOWN ============================================================================

