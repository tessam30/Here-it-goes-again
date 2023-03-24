# PROJECT: Another round of analysis of PLHIV by district / age
# PURPOSE: Munge and Analysis of Spectrum COP23 estimates
# AUTHOR: Tim Essam | SI
# REF ID:   6e237fd3
# LICENSE: MIT
# DATE: 2023-03-06
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
    library(readxl)
    library(glue)
    
    
  # SI specific paths/functions  
    load_secrets()
    merdata <- file.path(glamr::si_path("path_msd"))
    plhiv_path <- "Data/PLHIV by district March 5.xlsx"
    msd_path <- return_latest(merdata, "MER_Structured_Datasets_PSNU_IM_FY21-23_20230210_v1_1_Zambia" )
          
  # Grab metadata
   #get_metadata(file_path)
  
  # REF ID for plots
    ref_id <- "6e237fd3"
    
  # Functions  
  

# LOAD DATA ============================================================================  

    # Check sheet names in file; keep header and body of data, we'll pivot and use
    # the index value to crosswalk the appropriate age bands with data body
    excel_sheets(plhiv_path)  
    
    header <- read_excel(plhiv_path, sheet = "PLHIV_est_2023", n_max = 1) %>% 
      janitor::clean_names()
    
    data_body <- read_excel(plhiv_path, sheet = "PLHIV_est_2023", skip = 2) %>% 
      janitor::clean_names()
    
    names(data_body)
    
    # PEPFAR ART M&E data
    df_msd <- read_msd(msd_path)
    
    # Get a list of standardized provinces to use
    # Flagging LusakaP because there is a also a Lusaka District that will get
    # merged with Lusaka province if one is not careful
    prov_list <- df_msd %>% 
      filter(str_detect(snu1, "Military", negate = T)) %>% 
      mutate(snu1 = str_remove_all(snu1, " Province")) %>% distinct(snu1) %>%
      mutate(snu1 = ifelse(snu1 == "Lusaka", "LusakaP", snu1)) %>% 
      pull()  

# MUNGE ============================================================================
  
    # Tag provinces, making a new column that we can copy down to map districts to parent SNU
    # Also want to drop 
    plhiv <- data_body %>% 
      mutate(  
        prov_tag = ifelse(district %in% prov_list, district, NA_character_), 
        prov_drop = ifelse(district %in% prov_list, "drop", "keep")
        ) %>% 
      fill(prov_tag, .direction = c("down")) %>% 
      filter(prov_drop == "keep") %>% 
      pivot_longer(cols = female_2:male_15, 
                   names_to = "sex",
                   values_to = "plhiv") %>% 
      separate(sex,into = c("sex", "cw_number"), sep = "_") %>% 
      rename(psnu = district)
  
    # Need to tag in the sex attributes to the crosswalk
    # Looking at the original Excel file we see that Column B corresponds to cw_number == 2
    # So for all even cw_numbers we will tag them as female, and the rest as male
    # Realized I don't need to do this as sex is included in the plhiv df (leaving in to show)
    plhiv_cw <- header %>% 
      select(-1) %>% 
      pivot_longer(cols = x2:x15,
                   names_to = "cw_number",
                   values_to = "age") %>% 
      mutate(cw_number = str_remove_all(cw_number, "x")) %>% 
      fill(age, .direction = c("down")) %>% 
      mutate(sex = case_when(
        as.numeric(cw_number) %% 2 == 0 ~ "female",
        TRUE ~ "male"
      ))
    
    # Check that the merge variables are compatible types
    map(list(plhiv$cw_number, plhiv_cw$cw_number), ~summary(.x))

    
    plhiv_est_df <- plhiv %>% left_join(., plhiv_cw)
    
# TODO ============================================================================

  # TODO: Munging practice
  # Try to create a dataframe of just the provincial estimates using the names in the prov list
  # and the `data_body` and `header` data frame

# VIZ ============================================================================
    
    # TODO: 
    # Review the code chunk below and try to create a population pyramid of the estimates by age / sex
    # for each province (All contained in a single ggplot)
    
    plhiv_est_df %>% 
      group_by(sex, age) %>% 
      summarize(plhiv = sum(plhiv, na.rm = T), .groups = "drop") %>% 
      ggplot(aes(y = age)) +
      geom_col(data = . %>% filter(sex == "female"), aes(x = -plhiv), fill = moody_blue) +
      geom_col(data = . %>% filter(sex == "male"), aes(x = plhiv), fill = genoa) +
      geom_text(data = . %>% filter(sex == "female"),
                aes(x = -plhiv, label = comma(plhiv, 1)),
                family = "Source Sans Pro SemiBold",
                size = 10/.pt,
                hjust = 1.1,
                color = grey90k) +
      geom_text(data = . %>% filter(sex == "male"),
                aes(x = plhiv, label = comma(plhiv, 1)),
                family = "Source Sans Pro SemiBold",
                size = 10/.pt,
                hjust = -0.1,
                color = grey90k) +
      geom_vline(xintercept = 0, linewidth = 1, color = grey90k) +
      si_style_xgrid() +
      scale_x_continuous(labels = ~ scales::label_number_si()(abs(.))) +
      labs(x = NULL, y = NULL, 
           title = glue("WHAT IS THE MAIN TAKEAWAY?"),
           caption = glue("Source: Spectrum PLHIV estimates | {ref_id}"))
  

