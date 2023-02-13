# PROJECT: Visualize PHIA results by Prvince
# PURPOSE: Munge and Analysis of PHIA data
# AUTHOR: Tim Essam | SI
# REF ID:   9e261603
# LICENSE: MIT
# DATE: 2023-02-13
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
    library(googlesheets4)
    
    
  # SI specific paths/functions  
    load_secrets()
  
  # REF ID for plots
    ref_id <- "9e261603"
    
  # Functions  
    collapse_transformer <- function(regex = "[*]$", ...) {
      function(text, envir) {
        collapse <- grepl(regex, text)
        if (collapse) {
          text <- sub(regex, "", text)
        }
        res <- identity_transformer(text, envir)
        if (collapse) {
          glue_collapse(res, ...)  
        } else {
          res
        }
      }
    }
    
    
    
    highest_snus <- function(df, indic = "HIV Prevalence"){
      df %>% 
        filter(indicator == {{indic}}, prov_leader < 4) %>%
        arrange(desc(est)) %>% 
        mutate(snu1 = str_to_upper(snu1)) %>% 
        pull(snu1) 
    }
  

# LOAD DATA ============================================================================  

  phia <- read_sheet("1pT53ohURDYrawWeC0rJ1omfd2_awMp-F1q0kxZlaHsY", sheet = "PHIA_GEO") %>% 
      group_by(indicator) %>% 
      mutate(prov_leader = dense_rank(-est))

# MUNGE ============================================================================
  
    prev_snus <- highest_snus(phia)   
    
    
  phia %>% 
      filter(indicator == "HIV Prevalence") %>% 
      mutate(prov = fct_reorder(snu1, est),
             est_label = case_when(
               prov == "Lusaka" ~ str_c(label_percent(scale = 1)(est), " Prevalence"), 
               TRUE ~ str_c(label_percent(scale = 1)(est))
             )) %>% 
      ggplot(aes(y = prov)) +
      geom_linerange(aes(xmin = lb, xmax = ub), size = 2.5, color = grey20k) +
      geom_point(aes(x = est, fill = est), size = 4, shape = 21) +
      geom_text(data = . %>% filter(prov_leader == 1), 
                aes(x = lb, label = "lower\nbound"), size = 8/.pt, 
                family = "Source Sans Pro",
                color = grey50k, 
                hjust = 1) +
      geom_text(data = . %>% filter(prov_leader == 1),
                aes(x = ub, label = "upper\nbound"), size = 8/.pt, 
                family = "Source Sans Pro",
                color = grey50k, 
                hjust = 0) +
      geom_text(aes(x = est, label = est_label), size = 12/.pt, 
                family = "Source Sans Pro",
                color = grey90k, 
                vjust = -1) +
      scale_x_continuous(labels = label_percent(scale = 1), position = "top") +
      rcartocolor::scale_fill_carto_c(palette = "SunsetDark") +
      si_style_xgrid() +
      theme(legend.position = "none") +
      labs(x = "HIV Prevalence", y = NULL,
           title = glue("ACCORDING TO THE 2021 ZAMPHIA, HIV PREVALENCE (15+) IS HIGHEST IN {prev_snus*} PROVINCES", .transformer = collapse_transformer(sep = ", ", last = " AND ")),
           subtitle = "USAID primarily covers Copperbelt, Central, Luapula, Northern, Northwestern, and Muchinga",
           caption = "Source: ZAMPHIA 2021 Summary Sheet | December 2022")
    si_save("Images/ZAMPHIA_Prevalence_by_snu1.png", scale = 1.25)
    
    

# VIRAL LOAD SUPPRESSION ============================================================================

   vls_snus <- highest_snus(phia, indic = "VLS")
      
    
    phia %>% 
      filter(indicator == "VLS") %>% 
      mutate(prov = fct_reorder(snu1, est),
             est_label = case_when(
               prov == "Southern" ~ str_c(label_percent(scale = 1)(est), " VLS"), 
               TRUE ~ str_c(label_percent(scale = 1)(est))
             )) %>% 
      ggplot(aes(y = prov)) +
      geom_linerange(aes(xmin = lb, xmax = ub), size = 2.5, color = grey20k) +
      geom_point(aes(x = est, fill = est), size = 4, shape = 21) +
      geom_text(data = . %>% filter(prov_leader == 1), 
                aes(x = lb, label = "lower\nbound"), size = 8/.pt, 
                family = "Source Sans Pro",
                color = grey50k, 
                hjust = 1) +
      geom_text(data = . %>% filter(prov_leader == 1),
                aes(x = ub, label = "upper\nbound"), size = 8/.pt, 
                family = "Source Sans Pro",
                color = grey50k, 
                hjust = 0) +
      geom_text(aes(x = est, label = est_label), size = 12/.pt, 
                family = "Source Sans Pro",
                color = grey90k, 
                vjust = -1) +
      scale_x_continuous(labels = label_percent(scale = 1), position = "top") +
      rcartocolor::scale_fill_carto_c(palette = "SunsetDark") +
      si_style_xgrid() +
      theme(legend.position = "none") +
      labs(x = "HIV Prevalence", y = NULL,
           title = glue("ACCORDING TO THE 2021 ZAMPHIA, VIRAL LOAD SUPPRESSION (15+) IS HIGHEST IN {vls_snus*} PROVINCES", .transformer = collapse_transformer(sep = ", ", last = " AND ")),
           subtitle = "USAID primarily covers Copperbelt, Central, Luapula, Northern, Northwestern, and Muchinga",
           caption = "Source: ZAMPHIA 2021 Summary Sheet | December 2022")
    si_save("Images/ZAMPHIA_VLS_by_snu1.png", scale = 1.4)

# SPINDOWN ============================================================================

