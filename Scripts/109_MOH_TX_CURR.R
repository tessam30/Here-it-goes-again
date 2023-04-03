# PROJECT: MOH TX_CURR by Age Band
# PURPOSE: Munge and Analysis of TX_CURR
# AUTHOR: Lemlem Baraki & Tim Essam | SI
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
  library(gt)
  library(gtExtras)


  #SI specific paths/functions  
  load_secrets()
  merdata <- file.path(glamr::si_path("path_msd"))
  shpdata <- file.path(glamr::si_path("path_vector")) # for shapefiles
  msd_path <- return_latest(merdata, "_PSNU_IM_FY21-23.*Zambia") # grabs clean data
  txcurr_path <- ("Data/TXCURR by 5-year age bands.xlsx") #changed provinces: LusakaP and NorthWestern to match 
 
  # USE PLHIV from DATAPACK
  dp_path <- "Data/2023-03-27 Target Setting Tool_For Check point 1 1812.xlsx"  
   
  #Grab metadata
    
  #REF ID for plots
      ref_id <- "6dfb637d"
      
  #Functions  
    gt_format <- function(gt_obj){
      gt_obj %>% 
        tab_source_note(
          source_note = gt::md(glue("Source: COP23 Datapack and GOZ MOH ART Database"))) %>% 
        tab_options(
          source_notes.font.size = px(10)) %>% 
        tab_style(
          style = list(
            cell_text(weight = "bold")
          ),
          locations = cells_row_groups()
        ) %>% 
        cols_label(plhiv = "PLHIV COP23",
                   txcurr = "MOH ART 2022",
                   tx_share = "Agency TX share",
                   art_gap = "ART Gap", 
                   artgap_share = "Agency Gap share") %>% 
        gt_theme_nytimes() 
        }
 
     

# LOAD DATA ============================================================================  

    # PLHIV ESTIMATES FROM DATAPACK
    dp_plhiv <- tame_dp(dp_path, type = 'PLHIV')  %>% 
        filter(indicator == "PLHIV") %>% 
        clean_psnu() %>% 
        mutate(sex = str_to_lower(sex),
               age = ageasentered) %>% 
        select(psnu, psnuuid, indicator, fiscal_year, sex, age, plhiv = targets)

    # Get providence coverage for agencies  
    prov_agency_cw <- googlesheets4::read_sheet(ss = "1JUxbHkOg_k5yHWJ9A7PZOmU6Pj_i2UbgDTpU4mhuw6o")  
      
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
      mutate(snu1 = str_remove_all(snu1, " Province")) %>% 
      distinct(snu1) %>%
      mutate(snu1 = ifelse(snu1 == "Lusaka", "LusakaP", snu1)) %>% 
      pull() 
    
    
    # LOAD shapefiles -- These are the new ones
    snu1_geo <- st_read("../Zambezi/GIS/snu1_fy22.shp") %>% 
      mutate(prov = str_remove_all(snu1, " Province"))
    plot(snu1_geo)
    
    cntry <- "Zambia"
    spdf_pepfar <- gisr::get_vcpolygons(path = shpdata, name = "VcPepfarPolygons.shp")
    zmb_geo <- purrr::map(3:5, ~spdf_pepfar %>% gisr::extract_boundaries(country = cntry, 
                                                                         level = .x))
    names(zmb_geo) <- list("adm0", "snu1", "psnu")

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
    df_txcurr <- txcurr_est_df %>% 
      mutate(age_fct = str_remove_all(age, "'") %>% factor()) %>% 
      mutate(age_fct = fct_relevel(age_fct, "5-9", after = 1))
      # mutate(age_fct = factor(age_fct))%>%
      # mutate(age_fct = fct_relevel(age_fct, "5-9", after = 1))
    
    # Merge in the PSNU UID so we can make maps; Drop above site as it's extra
    df_psnu <- df_msd %>% 
      distinct(psnuuid, psnu) %>% 
      filter(str_detect(psnuuid,  "\\?", negate = T)) %>%
      filter(str_detect(psnu, "_Mil", negate = T)) %>% 
      clean_psnu()
  
    

# ALIGN DATA SETS ---------------------------------------------------------

    # Need to do a couple of things
    # Align PSNUS
    # Align Sex and Age bands
    
    # Align the MOH ART PSNUs so we can get balanced join
    df_txcurr <- 
      df_txcurr %>% 
      mutate(psnu = case_when(
        psnu == "Chiengi" ~ "Chienge",
        psnu == "Kapiri Mposhi" ~ "Kapiri-Mposhi",
        psnu == "Senga Hill" ~ "Senga",
        psnu == "Mushindano" ~ "Mushindamo",
        psnu == "Milengi" ~ "Milenge",
        psnu == "Shangombo" ~ "Shang'ombo" ,
        psnu == "Chikankanta" ~ "Chikankata",
        TRUE ~ psnu
      ))
    
    
    # Check the names of the PSNUs in both datasets
    # Swap the order to see the base comparison differences
    setdiff(unique(df_txcurr$psnu) %>% sort(), unique(df_psnu$psnu))
    setdiff(unique(df_psnu$psnu) %>% sort(), unique(df_txcurr$psnu))    
         
    # Source the "1008_Spectrum_PLHIVEST_Age+Sex.R" to get the PLHIV estimates
    # Need to align the TX_CURR age bands to the PLHIV age bands
    setdiff(unique(df_txcurr$age), unique(dp_plhiv$age))
    setdiff(unique(dp_plhiv$age), unique(df_txcurr$age))
    
    # Collapse function to recalculate ages
    sum_age <- function(df, ...){
      df %>% 
        group_by(...) %>% 
        mutate(age = sum(age, na.rm = T), .groups = "drop")
    }
    
    # So, from PLHIV we need to collapse "<01 + 01-09" into 0-9 and in 
    # TX_CURR 35-39...45-49 need to get lumped into 35-49 and 50-54...80+ into 50+
    dp_plhiv <- 
      dp_plhiv %>% 
      mutate(age = case_when(
        age %in% c("<01", "01-09") ~ "0-9",
        TRUE ~ age
      )) %>% 
      summarise(plhiv = sum(plhiv, na.rm = T), .by = c(psnu, psnuuid, sex, age)) 
    
    
    df_txcurr_adj <- 
      df_txcurr %>% 
      mutate(age = case_when(
        age %in% c("0-4", "5-9") ~ "0-9",
        age %in% c("15-19", "20-24") ~ "15-24",
        age %in% c("25-29", "30-34") ~ "25-34",
        age %in% c("35-39", "40-44", "45-49") ~ "35-49",
        age %in% c("50-54", "55-59", "60-64", "65-69", "70-74", "75-79", "80+") ~ "50+",
        TRUE ~ age)) %>% 
      summarise(txcurr = sum(txcurr, na.rm = T), .by = c(psnu, prov_tag, sex, age)) %>% 
      mutate(snu1 = ifelse(prov_tag == "LusakaP", "Lusaka", prov_tag)) %>% 
      left_join(., prov_agency_cw, by = c("snu1"))
      
    
    # Check that we have same number of rows in each data set (are age collapse correct)
    nrow(df_txcurr_adj) == nrow(dp_plhiv)
    
    
    # Now join the three data sets
    df_cop23_est <- 
      dp_plhiv %>% 
      left_join(., df_txcurr_adj) %>% 
      left_join(., df_psnu, by = c("psnuuid", "psnu")) %>% 
      mutate(age_fct = factor(age),
             art_gap = plhiv - txcurr) 
      
    

# GAP TABLES --------------------------------------------------------------


  # Summary Tables for USAID Share of GAP
    calc_agency_gap <- function(df, ...){
      df %>% 
        group_by(...) %>% 
        summarise(across(c(plhiv, txcurr, art_gap), \(x) sum(x, na.rm = T))) %>%
        ungroup() %>% 
        mutate(PLHIV_share = plhiv / sum(plhiv), .after = plhiv) %>% 
        mutate(tx_share = txcurr / sum(txcurr), .after = txcurr) %>% 
        mutate(artgap_share = art_gap / sum(art_gap))
      }
    
    # By agency
    calc_agency_gap(df_cop23_est, snu1_agency) %>% 
      gt() %>% 
      gt_format() %>% 
      fmt_number(c(2, 4, 6), 
                 decimals = 0) %>% 
      fmt_percent(c(3, 5, 7), 
                  decimals = 0) %>% 
      tab_header(title = "TREATMENT GAP BY FUNDING AGENCY") %>% 
      gt_color_rows(art_gap, palette = RColorBrewer::brewer.pal("Reds", n = 6)) %>% 
      gtsave_extra( filename = "Images/COP23_tx_gap_agency.png")
      
    
  # By agency age
    calc_agency_gap(df_cop23_est, snu1_agency, snu1) %>% 
      group_by(snu1_agency) %>% 
      mutate(snu1 = fct_reorder(snu1, artgap_share, .desc = T)) %>% 
      ungroup() %>% 
      arrange(snu1) %>% 
      gt(groupname_col = "snu1_agency") %>% 
      gt_format() %>% 
      cols_label(snu1 = "Province") %>% 
      fmt_number(c(3, 5, 7), 
                 decimals = 0) %>% 
      fmt_percent(c(4, 6, 8), 
                  decimals = 0) %>% 
      tab_header(title = "TREATMENT GAP BY FUNDING AGENCY & PROVINCE") %>% 
      gt_color_rows(art_gap, palette = RColorBrewer::brewer.pal("Reds", n = 7), domain = c(0, 5e4),  na.color = "#ffffbf") %>% 
      gtsave_extra( filename = "Images/COP23_tx_gap_agency_province.png")
    
    # By Age
    calc_agency_gap(df_cop23_est, snu1_agency, age) %>% 
      mutate(PLHIV_share = plhiv / sum(plhiv), .after = plhiv) %>% 
      mutate(tx_share = txcurr / sum(txcurr), .after = txcurr) %>% 
      mutate(artgap_share = art_gap / sum(art_gap)) %>%
      gt(groupname_col = "snu1_agency") %>% 
      gt_format() %>% 
      fmt_number(c(3, 5, 7), 
                 decimals = 0) %>% 
      fmt_percent(c(4, 6, 8), 
                  decimals = 0) %>% 
      tab_header(title = "TREATMENT GAP BY FUNDING AGENCY & AGE") %>% 
      gt_color_rows(art_gap, palette = RColorBrewer::brewer.pal("Reds", n = 7), domain = c(0, 4e4),  na.color = "#ffffbf") %>% 
      gtsave_extra( filename = "Images/COP23_tx_gap_agency_age.png")
    
    calc_agency_gap(df_cop23_est, snu1, sex) %>% 
      mutate(PLHIV_share = plhiv / sum(plhiv), .after = plhiv) %>% 
      mutate(tx_share = txcurr / sum(txcurr), .after = txcurr) %>% 
      mutate(artgap_share = art_gap / sum(art_gap)) %>%
      mutate(snu1 = fct_reorder(snu1, artgap_share, .desc = T)) %>% 
      arrange(snu1) %>% 
      gt(groupname_col = "snu1") %>% 
      gt_format() %>% 
      cols_label(tx_share = "TX share",
                artgap_share = "Gap share") %>% 
      fmt_number(c(3, 5, 7), 
                 decimals = 0) %>% 
      fmt_percent(c(4, 6, 8), 
                  decimals = 0) %>% 
      tab_header(title = "TREATMENT GAP BY PROVINCE & SEX") %>% 
      gt_color_rows(art_gap, palette = RColorBrewer::brewer.pal("Reds", n = 9), domain = c(0, 3.2e4),  na.color = "#ffffbf") %>% 
    gtsave_extra( filename = "Images/COP23_tx_gap_province_sex.png")
    
# CREATE BAR GRAPH PLOTS OF GAP
    df_cop23_est %>% 
      summarise(across(c(plhiv, txcurr, art_gap), \(x) sum(x, na.rm = T)), .by = c(snu1, age_fct)) %>% 
      mutate(snu_order = fct_reorder(snu1, plhiv, .desc = T)) %>% 
      ggplot(aes(y = age_fct)) +
      geom_col(aes(x = plhiv), fill = grey20k, width = 0.75 ) +
      geom_col(aes(x = txcurr), fill = scooter, width = 0.75) +
      facet_wrap(~snu_order, 
                 labeller = labeller(.multi_line = F)) +
      geom_text(aes(x = plhiv, label = comma(round(art_gap, 0))), 
                family = "Source Sans Pro",
                size = 8/.pt, 
                hjust = 0, 
                color = grey90k) +
      si_style_xgrid(facet_space = 0.5) +
      scale_x_continuous(labels = comma) +
      labs(x = NULL, y = NULL, 
           title = glue("COP23 PRELIMINARY PLHIV ESTIMATES & MOH DECEMBER 2022 TX_CURR"),
           subtitle = "PLHIV gap represented by space between PLHIV estimates & TX_CURR",
           caption = glue("Source: COP23 Datapack and GOZ MOH ART Database")) %>% 
    si_save("Graphics/COP23_PLHIV_GAP_updated.svg")
    
            
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
