# PROJECT: Zambia's PLHIV Estimates by Age/Sex
# PURPOSE: Munge and Analysis of Spectrum Estimates 
# AUTHOR: Lemlem Baraki | SI
# REF ID:   ba2c9f8b
# LICENSE: MIT
# DATE: 2023-03-23
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
  
  # NOTES - EDITED EXCEL FILE IN THE FOLLOWING MANNER
  # 1) Flag Lusaka province as LusakaP so we can distinguish from PSNU
  # 2) Removed "-" in NorthWestern
  # 3) Carried "Area" name down into cell A3
    
  # SI specific paths/functions  
    load_secrets()
    merdata <- file.path(glamr::si_path("path_msd"))
    plhiv_path <- ("Data/PLHIV estimates by age band.xlsx") #moved the "Area" title up + added "LusakaP" + fixed "North-Western"
    msd_path <- return_latest(merdata, "PSNU_IM_FY21-23_20230210.*Zambia")
    
    # get msd metadata
    get_metadata(msd_path)
  
  # REF ID for plots
    ref_id <- "ba2c9f8b"
    
  # pull in agency cw  
    prov_agency_cw <- googlesheets4::read_sheet(ss = "1JUxbHkOg_k5yHWJ9A7PZOmU6Pj_i2UbgDTpU4mhuw6o")

# LOAD DATA ============================================================================  
    # Check sheet names in file; keep header and body of data, we'll pivot and use
    # the index value to crosswalk the appropriate age bands with data body
    
    excel_sheets(plhiv_path) 
    
    #header - includes title + age bands 
    header2 <- read_excel(plhiv_path, range = "Sheet1!A1:O2", n_max = 1) %>% #used "range" to specify columns
      janitor::clean_names() 
        
    #data_body - includes districts/provinces + sex + values 
    data_body2 <- read_excel(plhiv_path, sheet = "Sheet1", skip = 2) %>% 
      janitor::clean_names() 
    
    names(data_body2)
    view(data_body2)
    
    # PEPFAR ART M&E data
    df_msd <- read_psd(msd_path)
    
    # Get a list of standardized provinces to use
    # Flagging LusakaP because there is a also a Lusaka District that will get
    # merged with Lusaka province if one is not careful
    prov_list <- df_msd %>% 
      filter(str_detect(snu1, "Military", negate = T)) %>% 
      mutate(snu1 = str_remove_all(snu1, " Province")) %>% distinct(snu1) %>%
      mutate(snu1 = ifelse(snu1 == "Lusaka", "LusakaP", snu1)) %>% 
      pull()  
    
    str(prov_list)

# MUNGE ============================================================================
  
 #Want to have 5 columns:district, province, age, sex, plhiv_est

    # Tag provinces, making a new column that we can copy down to map districts to parent SNU
    pl_hiv <- data_body2 %>% 
      mutate(  
        prov_tag = ifelse(area %in% prov_list, area, NA_character_), #districts on the prov_list put in "prov_tag"
        prov_drop = ifelse(area %in% prov_list, "drop", "keep")) %>% #districts not on prov_list tagged "keep" in "prov_drop"
      fill(prov_tag, .direction = c("down")) %>% 
      filter(prov_drop == "keep") %>% 
      pivot_longer(cols = female_2:male_15, 
                   names_to = "sex",
                   values_to = "plhiv") %>% 
      separate(sex,into = c("sex", "cw_number"), sep = "_") %>% 
      rename(psnu = area)
    
    view(pl_hiv)
    
    # Need to tag in the age to the crosswalk
    pl_hiv_cw <- header2 %>% 
      select(-1) %>% 
      pivot_longer(cols = x2:x15,
                   names_to = "cw_number",
                   values_to = "age") %>% 
      mutate(cw_number = str_remove_all(cw_number, "x")) %>% 
      fill(age, .direction = c("down"))
    view(pl_hiv_cw)
    
    # Check that the merge variables are compatible types
    map(list(pl_hiv$cw_number, pl_hiv_cw$cw_number), ~summary(.x))
    
    # Join data sets, tag USAID / CDC provinces
    plhiv_est_df <- pl_hiv %>% left_join(., pl_hiv_cw) %>% 
      left_join(prov_agency_cw %>% mutate(snu1 = ifelse(snu1 == "Lusaka", "LusakaP", snu1)),
                by = c("prov_tag" = "snu1"))
    view(plhiv_est_df)
    
    #summary 
    plhiv_est_df%>% 
      summarize(plhiv = sum(plhiv)) #total is 1,411,776 - matches Excel
    
    # How does CDC v USAID breakdown compare?
    plhiv_est_df %>% 
      rename(snu1 = prov_tag) %>% 
      mutate(snu1 = ifelse(snu1 == "LusakaP", "Lusaka", snu1)) %>% 
      group_by(snu1, snu1_agency, sex) %>% 
      summarise(plhiv = sum(plhiv), .groups = "drop") %>% 
      mutate(plhiv_sh = plhiv/sum(plhiv)) %>% 
      arrange(snu1_agency, plhiv_sh) %>% 
      group_by(sex, snu1_agency) %>% 
      mutate(agency_sh = cumsum(plhiv_sh))
    
    plhiv_est_df <- 
      plhiv_est_df %>% 
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
    
  
# VIZ ============================================================================

  #Create a population pyramid of the estimates by age/sex
    #for each province - facet_wrap
      #scales::label_number_si() --> now label_number(scales_cut = cut_short_scale())
      #ggplot2::annotate --> female(moody_blue) + male(genoa)
      #ggplot2::expand_limits
    
    plhiv_est_df %>% 
      group_by(sex, age, prov_tag) %>% #need to list prov_tag if using in df
      summarize(plhiv = sum(plhiv, na.rm = T), .groups = "drop") %>% 
      ggplot(aes(y = age)) +
      geom_col(data = . %>% filter(sex == "female"), aes(x = -plhiv), fill = moody_blue) +
      geom_col(data = . %>% filter(sex == "male"), aes(x = plhiv), fill = genoa) +
      geom_text(data = . %>% filter(sex == "female"),#geom_text - list plhiv est for each age band
                aes(x = -plhiv, label = comma(plhiv, 1)), 
                family = "Source Sans Pro", #semibold not appearing
                size = 10/.pt,
                hjust = 1.1,
                color = grey90k) +
      geom_text(data = . %>% filter(sex == "male"),
                aes(x = plhiv, label = comma(plhiv, 1)),
                family = "Source Sans Pro",
                size = 10/.pt,
                hjust = -0.1,
                color = grey90k) +
     #annotate("text", x = "plhiv", y = 0.9, label = "female", 
               #size=11/.pt, color = moody_blue, family = "Source Sans Pro")+ 
      #annotate("text", x = "plhiv", y = 0.9, label = "male", 
               #size=11/.pt, color = genoa, family = "Source Sans Pro")+
      geom_vline(xintercept = 0, linewidth = 1, color = grey90k) +
      facet_wrap(~prov_tag)+ 
      si_style_xgrid() +
      expand_limits(x=c(-120000,120000))+ #now you can see LusakaP
      scale_x_continuous(labels = ~ scales::label_number(scales_cut = cut_short_scale())(abs(.))) + #label_number_si --> scales_cut
      labs(x = NULL, y = NULL, 
           title = glue("PLHIV Estimates by Age Band"),
           caption = glue("Source: Spectrum PLHIV estimates | {ref_id}"))
    
    #si_save("PLHIV_Estimates.png", path="Images")
    
    

# SPINDOWN ============================================================================
