# PROJECT: MOH TX_CURR by Age Band
# PURPOSE: Munge and Analysis of TX_CURR
# AUTHOR: Lemlem Baraki | SI
# REF ID:   6dfb637d
# LICENSE: MIT
# DATE: 2023-03-28
# NOTES: Lemlem Baraki | SI

# LOCALS & SETUP ============================================================================

# Libraries
library(glitr)
library(glamr)
library(gisr)
library(gophr)
library(tidyverse)
library(scales)
library(sf)
library(extrafont)
library(tidytext)
library(patchwork)
library(ggtext)
library(gagglr)
library(readxl)
library(glue)


#SI specific paths/functions  
load_secrets()
merdata <- file.path(glamr::si_path("path_msd"))
msd_path <- return_latest(merdata, "MER_Structured_Datasets_PSNU_IM_FY21-23_20230210_v1_1_Zambia")
txcurr_path <- ("Zambia/TXCURR by 5-year age bands.xlsx") #changed provinces: LusakaP and NorthWestern to match 

#Grab metadata
  
#REF ID for plots
    ref_id <- "6dfb637d"
    
#Functions  
   

# LOAD DATA ============================================================================  

    #TX_CURR
    excel_sheets(txcurr_path) 
    
    #header - includes title + age bands 
    header2 <- read_excel(txcurr_path, range = "TXCURR!A1:AI2", n_max = 1) %>% #used "range" to specify columns
      janitor::clean_names() 
    
    view(header2)
    
    #data_body - includes districts/provinces + sex + values 
    data_body2 <- read_excel(txcurr_path, sheet = "TXCURR", skip = 2) %>% 
      janitor::clean_names() 
    
    
    names(data_body2)
    view(data_body2)
    
    #PEPFAR
    df_msd <- read_psd(msd_path)
    
    # Get a list of standardized provinces to use
    # Flagging LusakaP because there is a also a Lusaka District that will get
    # merged with Lusaka province if one is not careful
    prov_list <- df_msd %>% 
      filter(str_detect(snu1, "Military", negate = T)) %>% 
      mutate(snu1 = str_remove_all(snu1, " Province")) %>% distinct(snu1) %>%
      mutate(snu1 = ifelse(snu1 == "Lusaka", "LusakaP", snu1)) %>% 
      pull() 

# MUNGE ============================================================================
  
    #Tag provinces, making a new column that we can copy down to map districts to parent SNU
    #Also want to drop 
    txcurr <- data_body2 %>% 
      mutate(  
        prov_tag = ifelse(x1 %in% prov_list, x1, NA_character_), 
        prov_drop = ifelse(x1 %in% prov_list, "drop", "keep")
      ) %>% 
      fill(prov_tag, .direction = c("down")) %>% 
      filter(prov_drop == "keep") %>% 
      pivot_longer(cols = male_2:female_35, 
                   names_to = "sex",
                   values_to = "txcurr") %>% 
      separate(sex,into = c("sex", "cw_number"), sep = "_") %>% 
      rename(psnu = x1)  
    
    #Need to tag in the age to the crosswalk
    txcurr_cw <- header2 %>% 
      select(-1) %>% 
      pivot_longer(cols = x2:x35,
                   names_to = "cw_number",
                   values_to = "age") %>% 
      mutate(cw_number = str_remove_all(cw_number, "x")) %>% 
      fill(age, .direction = c("down")) #misreading the age order
    view(txcurr_cw) 
    
    
    #check merge is compatible 
    map(list(txcurr$cw_number, txcurr_cw$cw_number), ~summary(.x))
    
    txcurr_est_df <- txcurr %>% left_join(., txcurr_cw, by = "cw_number") #moves the 5-9 age range? 
    
    #make a new column(age_fct)
      #make age a factor 
      #use forcats::fct_relevel to set the order ex.)fct_relevel(.f, ..., after = 0L)
        #relevel "5-9" after "0-4" level 
    df_txcurr <-txcurr_est_df %>% 
      mutate(age_fct = str_remove_all(age, "'"))%>% 
      mutate(age_fct = factor(age_fct))%>%
      mutate(age_fct = fct_relevel(age_fct, "5-9", after = 1))
         
    view(df_txcurr)
# VIZ ============================================================================

  # # Review the code chunk below and try to create a population pyramid of the estimates by age / sex
    # for each province (All contained in a single ggplot)
      #use age_fct as y-variable 
    
    #txcurr_est_df %>% 
    df_txcurr %>%
      group_by(age_fct, sex, prov_tag) %>% 
      summarize(txcurr = sum(txcurr, na.rm = T), .groups = "drop") %>% 
      #ggplot(aes(y = age)) +
      ggplot(aes(y = age_fct))+
      geom_col(data = . %>% filter(sex == "female"), aes(x = -txcurr), fill = moody_blue) +
      geom_col(data = . %>% filter(sex == "male"), aes(x = txcurr), fill = genoa) +
      geom_text(data = . %>% filter(sex == "female"),
                aes(x = -txcurr, label = comma(txcurr, 1)), 
                family = "Source Sans Pro", 
                size = 10/.pt,
                hjust = 1.1,
                color = grey90k) +
      geom_text(data = . %>% filter(sex == "male"),
                aes(x = txcurr, label = comma(txcurr, 1)),
                family = "Source Sans Pro",
                size = 10/.pt,
                hjust = -0.1,
                color = grey90k) +
      annotate("text", x = -20000, y = 0.97, label = "female", 
      size=11/.pt, color = moody_blue, family = "Source Sans Pro")+ 
      annotate("text", x = 20000, y = 0.97, label = "male", 
      size=11/.pt, color = genoa, family = "Source Sans Pro")+
      geom_vline(xintercept = 0, linewidth = 1, color = grey90k) +
      facet_wrap(~prov_tag)+ 
      si_style_xgrid() +
      expand_limits(x=c(-40000,40000))+ #now you can see LusakaP
      scale_x_continuous(labels = ~ scales::label_number(scales_cut = cut_short_scale())(abs(.))) + 
      labs(x = NULL, y = NULL, 
           title = glue("TXCURR by Age Band"),
           caption = glue("Source: Spectrum TXCURR by 5-year Age Bands | {ref_id}"))
    
    si_save("MOH-TX_CURR.png", path="Images")

# SPINDOWN ============================================================================
