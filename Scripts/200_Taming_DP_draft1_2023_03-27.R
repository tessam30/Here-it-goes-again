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
    msd_path <- msd_path <- return_latest(merdata, "PSNU_IM_FY21-23_20230210.*Zambia")
    #dp_path <- "Data/2023-03-27 Target Setting Tool_For Check point 1 1812.xlsx"  
    dp_path <- "../../../Downloads/2023-04-14 Target Setting Tool_5PM.xlsx"
    
  # Grab metadata
   get_metadata(msd_path)
  
  # REF ID for plots
    ref_id <- "7be07a44"
    
  # Functions  
  

# LOAD DATA ============================================================================  

  dp <- tame_dp(dp_path)
  dp_plhiv <- tame_dp(dp_path, type = 'PLHIV')
    
  msd <- read_psd(msd_path)

# MUNGE ============================================================================
  
 dp_tgs <- dp %>% 
    clean_indicator() %>% 
    filter(ageasentered %in% c("<01", "01-09", "10-14")) %>% 
      group_by(indicator, standardizeddisaggregate, fiscal_year) %>% 
      summarise(targets = sum(targets, na.rm = T), .groups = "drop") %>% 
    spread(fiscal_year, targets) %>% 
    mutate(diff = `2024` - `2023`,
           delta = ((`2024`/`2023`) - 1) %>% percent(., 1))
  
  gs_id <- "1gQioul3yS1o92OAqEVGU6vFjlt4MkNQTqmy9TxxAug0"
  googlesheets4::sheet_write(dp_tgs, ss = gs_id, sheet = 3)

 msd %>% 
   filter(indicator == "TX_CURR", fiscal_year == 2023) %>% 
   group_by(standardizeddisaggregate) %>% 
   summarise(targets = sum(targets, na.rm = T), .groups = "drop")
 
 
 snu1_cw <- msd %>% distinct(snu1, psnu, psnuuid)
 
 dp_plhiv %>% 
   left_join(., snu1_cw) %>% 
   clean_indicator() %>% 
   filter(ageasentered %in% c("<01", "01-09", "10-14")) %>% 
   group_by(indicator, fiscal_year, standardizeddisaggregate) %>% 
   summarize(targets = sum(targets, na.rm =T)) %>% 
   spread(fiscal_year, targets) %>% 
   filter(indicator %in% c("PLHIV", "TX_CURR_SUBNAT"))
 
 msd %>% 
   filter(indicator == "TX_CURR", 
          fiscal_year == 2023,
          trendscoarse == "<15",  
          standardizeddisaggregate == "Age/Sex/HIVStatus") %>% 
   summarise(tx_curr = sum(cumulative, na.rm = T), .groups = "drop")
 

  
# VIZ ============================================================================

  dp %>% 
   filter(indicator == "HTS_TST_POS", 
          ageasentered %in% c("01-09", '10-14', '01-04', )) %>%  
          #standardizeddisaggregate == "Modality/Age/Sex/Result") %>% 
   group_by(fiscal_year, indicator, standardizeddisaggregate) %>% 
   summarise(sum = sum(targets, na.rm = T))
 
 dp %>% filter(indicator == "TX_CURR") %>% count(ageasentered)

# CASCADES ============================================================================
 
 
 dp_plhiv %>% 
   left_join(., snu1_cw) %>% 
   clean_indicator() %>% 
   filter(ageasentered %in% c("<01", "01-09", "10-14")) %>% 
   group_by(indicator, fiscal_year, standardizeddisaggregate) %>% 
   summarize(targets = sum(targets, na.rm =T)) %>% 
   spread(fiscal_year, targets) 
 
# CASCADE from MSD
 msd %>% 
   filter(indicator %in% c("TX_CURR", "TX_PVLS", "HTS_TST_POS"),
          trendscoarse == "<15",
          standardizeddisaggregate %in% c("Age/Sex/HIVStatus", "Age/Sex/Indication/HIVStatus"),
          fiscal_year == 2023) %>% 
   clean_indicator() %>% 
   group_by(indicator) %>% 
   summarise(val = sum(cumulative, na.rm = T))
 
# PLOT CASCADES
 
 csd_df <- googlesheets4::read_sheet(ss = "1vKvj3Gr7W6CPc6zWg9K2mbRHlvF1d3bdjvxNIbJ2dvs")
 
  csd_df %>% 
    mutate(indic_order = fct_reorder(indicator, order),
           source_order = fct_relevel(source, c("benchmark", "subnat", "mer"))) %>% 
    ggplot(aes(x = indic_order, y = value)) +
    geom_col(data = . %>% filter(source == "benchmark"), aes(fill = fill_col), 
             position = position_nudge(x = -0.1), width = 0.5) + 
    geom_text(data = . %>% filter(source == "benchmark"), 
              aes(label = comma(value)), size = 10/.pt,
                  family = "Source Sans Pro",
              vjust = -0.5, 
              color = grey50k,
              position = position_nudge(x = -0.1)) +
    geom_col(data = . %>% filter(source == "subnat" & indicator != "CLHIV"), aes(fill = fill_col), width = 0.5) +
    geom_text(data = . %>% filter(source == "subnat" & indicator != "CLHIV"), 
              aes(label = comma(value)), size = 10/.pt,
              family = "Source Sans Pro",
              vjust = -0.5, 
              color = grey90k) +
    geom_label(data = . %>% filter(source == "subnat" & indicator != "CLHIV"), 
              aes(label = percent(cascade_val, 1)), size = 10/.pt,
              family = "Source Sans Pro",
             vjust = 1.2) +
    scale_fill_identity() +
    si_style_ygrid() +
    scale_y_continuous(labels = label_number_si(), limits = c(0, 65000)) +
    labs(x = NULL, y = NULL,
         caption = glue("Source: Target Setting Tool 2023-04-14 & {metadata$source}"))
  si_save("Images/zmb_cascade_subnat_cop23.png")
  
  
  
  # ABSOLUTE
  csd_df %>% 
    mutate(indic_order = fct_reorder(indicator, order),
           source_order = fct_relevel(source, c("benchmark", "subnat", "mer"))) %>% 
    ggplot(aes(x = indic_order, y = value)) +
    geom_col(data = . %>% filter(source == "benchmark"), aes(fill = fill_col), 
             position = position_nudge(x = -0.1), width = 0.5) + 
    geom_text(data = . %>% filter(source == "benchmark"), 
              aes(label = comma(value)), size = 10/.pt,
              family = "Source Sans Pro",
              vjust = -0.5, 
              color = grey50k,
              position = position_nudge(x = -0.1)) +
    geom_col(data = . %>% filter(source == "mer" & indicator != "CLHIV"), aes(fill = fill_col), width = 0.5) +
    geom_text(data = . %>% filter(source == "mer" & indicator != "CLHIV"), 
              aes(label = comma(value)), size = 10/.pt,
              family = "Source Sans Pro",
              vjust = -0.5, 
              color = grey90k) +
    geom_label(data = . %>% filter(source == "mer" & indicator != "CLHIV"), 
               aes(label = percent(absolute_val, 1)), size = 10/.pt,
               family = "Source Sans Pro",
               vjust = 1.2) +
    scale_fill_identity() +
    si_style_ygrid() +
    scale_y_continuous(labels = label_number_si(), limits = c(0, 65000)) +
    labs(x = NULL, y = NULL,
         caption = glue("Source: Target Setting Tool 2023-04-14 & {metadata$source}"))
  si_save("Images/zmb_cascade_mer_cop23_absolute.png")
         