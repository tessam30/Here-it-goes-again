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
   group_by(indicator, fiscal_year, standardizeddisaggregate) %>% 
   summarize(targets = sum(targets, na.rm =T)) %>% 
   spread(fiscal_year, targets) %>% 
   filter(indicator == "PLHIV")
  
# VIZ ============================================================================

  #  

# SPINDOWN ============================================================================

