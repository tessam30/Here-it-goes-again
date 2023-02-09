# PROJECT: Here-it-goes-again
# PURPOSE: Munge and Analysis of COP23 Estimates from SPECTRUM
# AUTHOR: Tim Essam | SI
# REF ID:   d77f9986
# LICENSE: MIT
# DATE: 2023-02-05
# NOTES: Tim Essam | SI

# LOCALS & SETUP ============================================================================

    # Load Q1 functions
    #devtools::install_github(repo = "USAID-OHA-SI/cascade", ref = "dev")
    source("Scripts/helper-call_all_helpers.R")

  library(readxl)
  library(gtExtras)
  library(gt)
  
  # REF ID for plots
    ref_id <- "d77f9986"
    
  # Functions  
  # Filepath
    plhiv_path <- "Data/Preliminary output Feb 03 2023.xlsx"

# LOAD DATA ============================================================================  

  # Check sheet names in file; keep header and body of data, we'll pivot and use
  # the index value to crosswalk the appropriate age bands with data body
    excel_sheets(plhiv_path)  
  
    header <- read_excel(plhiv_path, sheet = "District", n_max = 1) %>% 
    janitor::clean_names()
  
    data_body <- read_excel(plhiv_path, sheet = "District", skip = 2) %>% 
    janitor::clean_names()
  
    names(data_body)
    
    data_body_prov <- read_excel(plhiv_path, sheet = "Provincial", skip = 1) %>% 
      janitor::clean_names()
  
  # Need to grab SNUs to filter out the extra rows in the genie pull
  df_genie <- read_msd(file_path)
  
  
# MUNGE ============================================================================

  # filter list of PSNUs to tag districts (copy down)
  prov_list <- df_genie %>% 
    filter(str_detect(snu1, "Military", negate = T)) %>% 
    mutate(snu1 = str_remove_all(snu1, " Province")) %>% distinct(snu1) %>%
    mutate(snu1 = ifelse(snu1 == "Lusaka", "LusakaP", snu1)) %>% 
    pull()  
  
  # Add in Zambia b/c the district tab includes SNU1 and OU totals
  prov_list <- c(prov_list, "Zambia")
  
  psnu_list <- df_genie %>% 
    filter(str_detect(psnu, "Military|above", negate = T)) %>% 
    mutate(psnu = str_remove_all(psnu, " District")) %>% 
    distinct(psnu, psnuuid) 

  # TX_CURR for Q1
  # In spectrum estimates, age bands start at 0-4, need to account for this
  df_tx <- df_genie %>% 
    filter(indicator == "TX_CURR",
           standardizeddisaggregate == "Age/Sex/HIVStatus",
           fiscal_year == metadata$curr_fy, 
           ageasentered %ni% c("Unknown Age")) %>% 
    mutate(age_datim = case_when(
      ageasentered %in% c("<01", "01-04") ~ "00-04", 
      ageasentered %in% c("50+", "50-54", "55-59", "60-64", "65+") ~ "50+", 
      TRUE ~ ageasentered
    )) %>% 
    group_by(age_datim, indicator, sex, psnu, psnuuid, snu1) %>% 
    summarize(tx_fy23q1 = sum(cumulative, na.rm = T), .groups = "drop") 
    
  # This is the crosswalk we need for the ages
  df_tx %>% count(age_datim)
  
  
  df_tx_snu <- df_tx %>% 
    group_by(snu1, age_datim, indicator, sex) %>% 
    summarise(tx_fy23q1 = sum(tx_fy23q1, na.rm = T), .groups = "drop") %>% 
    mutate(snu1 = str_remove_all(snu1, " Province"))
  
  df_tx_agency <- 
    df_tx %>% 
    filter(str_detect(snu1, "Military", negate = T)) %>% 
    mutate(snu1 = str_remove_all(snu1, " Province")) %>% 
    left_join(., prov_agency_cw) %>% 
    group_by(snu1_agency, age_datim, indicator, sex) %>% 
    summarise(tx_fy23q1 = sum(tx_fy23q1, na.rm = T), .groups = "drop")
  
  
  
# Show which provinces are not reporting 50+ disaggregates
  df_genie %>% 
    filter(indicator == "TX_CURR",
           standardizeddisaggregate == "Age/Sex/HIVStatus",
           fiscal_year == metadata$curr_fy, 
           ageasentered %ni% c("Unknown Age")) %>% 
    group_by(ageasentered, indicator, snu1) %>% 
    summarize(tx_fy23q1 = sum(cumulative, na.rm = T), .groups = "drop") %>% 
    spread(ageasentered, tx_fy23q1)
  
  

# MUNGE PLHIV ESTIMATES ---------------------------------------------------

  # Tag provinces, making a new column that we can copy down to map districts to parent SNU
  plhiv <- data_body %>% 
    mutate(prov_tag = ifelse(x1 %in% prov_list, x1, NA_character_), 
           prov_drop = ifelse(x1 %in% prov_list, "drop", "keep")) %>% 
    fill(prov_tag, .direction = c("down")) %>% 
    filter(prov_drop == "keep") %>% 
    pivot_longer(cols = male_2:female_35, 
                 names_to = "sex",
                 values_to = "plhiv") %>% 
    separate(sex,into = c("sex", "cw_number"), sep = "_") %>% 
    rename(psnu = x1)
  
  plhiv_snu <- data_body_prov %>% 
    pivot_longer(cols = male_2:female_35, 
                 names_to = "sex",
                 values_to = "plhiv") %>% 
    separate(sex,into = c("sex", "cw_number"), sep = "_") %>% 
    rename(snu1 = x1)
  
  # PLHIV header and crosswalk to get to age bands
  # One oddity put a "female" value in the age band spot. Remove this first
  # There is also a missing column, need 34 (2:35)
  # Need to fix the 0-4 and 5-9 age bands to match datim
  plhiv_cw <- header %>% 
    select(-1) %>% 
    mutate(x5 = NA_character_,
           x35 = NA_character_) %>% 
    pivot_longer(cols = x2:x35,
                 names_to = "cw_number",
                 values_to = "age") %>% 
    mutate(cw_number = str_remove_all(cw_number, "x")) %>% 
    fill(age, .direction = c("down"))
  
  # Fix the districts that are named differently
  plhiv_est <- 
    plhiv %>% 
    left_join(plhiv_cw) %>% 
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
  
  # How much do the district overlap? Should be 116
  setdiff( psnu_list$psnu, unique(plhiv_est$psnu))
  
  # Fix age bands before merging with df_tx
  plhiv_psnu <- plhiv_est %>% 
    left_join(psnu_list) %>% 
    mutate(age_datim = case_when(
      age == "0-4" ~ "00-04",
      age == "5-9" ~ "05-09",
      age %in% c("50-54", "55-59", "60-64", "65-69", "70-74", "75-79", "80+") ~ "50+",
      TRUE ~ age
    ),
    sex = str_to_title(sex)) 
  

  plhiv_snu <- plhiv_snu %>% 
    left_join(plhiv_cw) %>%  mutate(age_datim = case_when(
      age == "0-4" ~ "00-04",
      age == "5-9" ~ "05-09",
      age %in% c("50-54", "55-59", "60-64", "65-69", "70-74", "75-79", "80+") ~ "50+",
      TRUE ~ age
    ),
    sex = str_to_title(sex),
    snu1 = ifelse(snu1 == "North-Western", "NorthWestern", snu1)) %>% 
    group_by(age_datim, sex, snu1) %>% 
    summarize(plhiv = sum(plhiv, na.rm = T)) %>% 
    left_join(., prov_agency_cw)
  
  # Do the Provincial names align? YES, after modification above
  plhiv_snu %>% count(age_datim, sex) %>% spread(sex, n)
  
  setdiff(unique(plhiv_snu$snu1), unique(df_tx_snu$snu1))
  
    

# CALCUATE THE TX GAP -----------------------------------------------------

  plhiv_gap_df <- plhiv_psnu %>% 
    group_by(age_datim, sex, psnuuid, psnu) %>% 
    summarize(plhiv_datim = sum(plhiv, na.rm = T), .groups = "drop") %>% 
    right_join(., df_tx) %>% 
    mutate(tx_gap = plhiv_datim - tx_fy23q1,
           snu1 = str_remove_all(snu1, " Province")) %>% 
    clean_column() %>% 
    left_join(., prov_agency_cw)
  
  plhiv_gap_snu <- plhiv_snu %>% 
    group_by(age_datim, sex, snu1_agency, snu1) %>% 
    summarize(plhiv_datim = sum(plhiv, na.rm = T), .groups = "drop") %>%
    right_join(., df_tx_snu) %>% 
    mutate(tx_gap = plhiv_datim - tx_fy23q1) 
    
  
  plhiv_gap_agency <- plhiv_snu %>% 
    group_by(age_datim, sex, snu1_agency) %>% 
    summarize(plhiv_datim = sum(plhiv, na.rm = T), .groups = "drop") %>%
    right_join(., df_tx_agency) %>% 
    mutate(tx_gap = plhiv_datim - tx_fy23q1) 

  
# VIZ ============================================================================

  # BY SNU1, SHOW how the PLHIV GAPS STACK UP across district
  # Ages are wonky post 50+, need to recalculate gap to this point
  
  plhiv_gap_snu %>% 
    filter(str_detect(snu1, "Military", negate = T)) %>% 
    group_by(snu1, age_datim) %>% 
    summarise(across(c(tx_gap, tx_fy23q1, plhiv_datim), sum, na.rm = T), .groups = "drop") %>% 
    mutate(snu_order = fct_reorder(snu1, plhiv_datim, .desc = T)) %>% 
    ggplot(aes(y = age_datim)) +
    geom_col(aes(x = plhiv_datim), fill = grey20k, width = 0.75 ) +
    geom_col(aes(x = tx_fy23q1), fill = scooter, width = 0.75) +
    facet_wrap(~snu_order, 
               labeller = labeller(.multi_line = F), 
               scales = "free_x") +
    geom_text(aes(x = plhiv_datim, label = comma(round(tx_gap, 0))), 
              family = "Source Sans Pro",
              size = 8/.pt, 
              hjust = 0, 
              color = grey90k) +
    si_style_xgrid(facet_space = 0.5) +
    scale_x_continuous(labels = comma) +
    labs(x = NULL, y = NULL, 
         title = glue("COP23 PRELIMINARY PLHIV ESTIMATES & FY23Q1 TX_CURR"),
         subtitle = "PLHIV gap represented by space between PLHIV estimates & TX_CURR",
         caption = glue("{metadata$caption} & Spectrum 2023 Estimates"))
  si_save("Graphics/COP23_PLHIV_GAP_free_scales.svg")
  


# TREATMENT GAP BY AGENCY AND AGE BANDS -----------------------------------
  
  # TODO -- clean up labels, get rid of negatives
  # Calculate for overall gaps (table)

  plhiv_gap_agency %>% 
    group_by(snu1_agency, age_datim, sex) %>% 
    mutate(pct_gap = (tx_gap / plhiv_datim)) %>% 
    ungroup() %>% 
    ggplot(aes(y = age_datim)) +
    geom_col(data = . %>% filter(sex == "Female"), aes(x = -plhiv_datim), fill = grey20k, width = 0.75 ) +
    geom_col(data = . %>% filter(sex == "Female"), aes(x = -tx_fy23q1), fill = moody_blue, width = 0.75) +
    geom_col(data = . %>% filter(sex == "Male"), aes(x = plhiv_datim), fill = grey20k, width = 0.75 ) +
    geom_col(data = . %>% filter(sex == "Male"), aes(x = tx_fy23q1), fill = genoa, width = 0.75) +
    facet_wrap(~snu1_agency, 
               labeller = labeller(.multi_line = F)) +
    geom_text(data = . %>% filter(sex == "Female"), 
              aes(x = -plhiv_datim, label = str_c(comma(round(tx_gap, 0)), "\n", percent(pct_gap, 1))), 
              family = "Source Sans Pro SemiBold",
              size = 10/.pt, 
              hjust = 1.1, 
              color = grey90k) +
    geom_text(data = . %>% filter(sex == "Male"), 
              aes(x = plhiv_datim, label = str_c(comma(round(tx_gap, 0)), "\n", percent(pct_gap, 1))), 
              family = "Source Sans Pro SemiBold",
              size = 10/.pt, 
              hjust = -0.1, 
              color = grey90k) +
    geom_vline(xintercept = 0, linewidth = 1, color = grey90k) +
    si_style_xgrid(facet_space = 0.5) +
    scale_x_continuous(labels = comma) +
    labs(x = NULL, y = NULL, 
         title = glue("COP23 PRELIMINARY PLHIV ESTIMATES & FY23Q1 TX_CURR BY AGENCY, SEX & AGE"),
         subtitle = "Treatment gap represented by space between PLHIV estimates & TX_CURR",
         caption = glue("{metadata$caption} & Spectrum 2023 Estimates"))
  si_save("Graphics/COP23_PLHIV_GAP_AGENCY_SEX_AGE.svg")
  
  

# TX_GAP BY AGENCY --------------------------------------------------------

  plhiv_gap_agency %>% 
    group_by(snu1_agency, sex) %>% 
    summarize(across(c(plhiv_datim, tx_fy23q1, tx_gap), sum, na.rm = T), .groups = "drop") %>% 
    mutate(gap_share = (tx_gap / sum(tx_gap))) %>% 
    gt(groupname_col = "snu1_agency") %>% 
    #gt_theme_nytimes() %>% 
    summary_rows(groups = TRUE,
                 columns = 3:5,
                 fns = list("Agency Total" = ~sum(., na.rm = TRUE)), 
                 formatter = fmt_number, 
                 decimals = 0) %>% 
    summary_rows(groups = TRUE,
                 columns = 6,
                 fns = list("Agency Total" = ~sum(., na.rm = T)), 
                 formatter = fmt_percent, 
                 decimals = 0) %>% 
    fmt_number(columns = 3:5, 
               decimals = 0) %>% 
    fmt_percent(columns = 6, 
                decimals = 0) %>% 
    grand_summary_rows(
                       columns = 3:5,
                       fns = list(Zambia = ~sum(., na.rm = TRUE)), 
                       formatter = fmt_number, 
                       decimals = 0
                       ) %>% 
    gt_theme_nytimes() %>% 
    tab_style(
      style = list(
        cell_text(weight = "bold")
      ),
      locations = cells_row_groups()
    ) %>% 
    tab_header(title = "TREATMENT GAP BY FUNDING AGENCY & SEX") %>% 
    tab_source_note(
      source_note = gt::md(glue("{metadata$caption} & Spectrum 2023 Estimates"))) %>% 
    tab_options(
      source_notes.font.size = px(10)) %>% 
    tab_style(style = cell_text(
      font = google_font("Source Sans Pro"), 
      weight = 600, 
      size = px(16)), 
      locations = cells_grand_summary()) %>% 
    tab_style(style = cell_text(
      font = google_font("Source Sans Pro"), 
      weight = 600, 
      size = px(16)), 
      locations = cells_summary()) %>% 
    cols_label(plhiv_datim = "PLHIV", 
               tx_fy23q1 = "FY23 TX_CURR ", 
               tx_gap = "Treatment Gap",
               gap_share = "Share of Overall Gap"
    ) %>% 
    gtsave_extra( filename = "Images/COP23_TX_SEX_GAP_SHARE.png")

    

 # PLHIV GAP BY AGE BANDS --------------------------------------------------

  
  # What is alleged PLHIV GAP for PEDS? FOR AYPS?
   plhiv_gap_age <-  plhiv_gap_snu %>% 
      mutate(age_custom = case_when(
        age_datim %in% c("00-04", "05-09", "10-14") ~ "Peds",
        age_datim %in% c("15-19", "20-24") ~ "AYP",
        TRUE ~ age_datim
      )) %>% 
      group_by(age_custom) %>% 
      summarise(across(c(plhiv_datim, tx_fy23q1, tx_gap), sum, na.rm = T), .groups = "drop") %>% 
      mutate(age_custom = fct_relevel(age_custom, c("Peds", "AYP", "25-29",
                                                    "30-34", "35-39", "40-44",
                                                    "45-49", "50+"))) %>%
      arrange(age_custom) %>%
    mutate(gap_share = tx_gap / sum(tx_gap, na.rm = T)) %>% 
    janitor::adorn_totals("row")
    
  plhiv_gap_age %>% 
      gt() %>% 
      gt::cols_label(plhiv_datim = "PLHIV", 
                tx_fy23q1 = "FY23 TX_CURR ", 
                tx_gap = "Treatment Gap",
                age_custom = "Age",
                gap_share = "Share of Gap") %>% 
      fmt_number(where(is.numeric), 
                 decimals = 0) %>% 
      fmt_percent(columns = gap_share, 
                  decimals = 0) %>% 
      tab_source_note(
        source_note = gt::md(glue("{metadata$caption} & Spectrum 2023 Estimates"))) %>% 
      tab_options(
        source_notes.font.size = px(10)) %>% 
      gt_theme_nytimes() %>% 
      tab_header(
        title = glue("COP23 PRELIMINARY TREATMENT GAP SUMMARY"),
      ) %>% 
      tab_style(
        style = list(
          cell_text(weight = "bold")
        ),
        locations = cells_body(
          columns = everything(),
          rows = age_custom == "Total"
        )
      ) %>% 
     gtsave_extra( filename = "Images/COP23_TX_AGE_GAP.png")
    
  

#  BY SNU, what do the numbers look like? ---------------------------------

  plhiv_gap_age_snu <-  plhiv_gap_snu %>% 
    mutate(age_custom = case_when(
      age_datim %in% c("00-04", "05-09", "10-14") ~ "Peds",
      age_datim %in% c("15-19", "20-24") ~ "AYP",
      TRUE ~ age_datim
    )) %>% 
    group_by(age_custom, snu1) %>% 
    summarise(across(c(plhiv_datim, tx_fy23q1, tx_gap), sum, na.rm = T), .groups = "drop") %>% 
    mutate(age_custom = fct_relevel(age_custom, c("Peds", "AYP", "25-29",
                                                  "30-34", "35-39", "40-44",
                                                  "45-49", "50+"))) %>%
    arrange(age_custom)



make_snu1_plhiv_table <- function(df, snu){
  df %>% 
    filter(snu1 == {{snu}}) %>% 
    mutate(gap_share = tx_gap / sum(tx_gap, na.rm = T)) %>% 
    janitor::adorn_totals("row") %>% 
    mutate(snu1 = ifelse(snu1 == "-", {{snu}}, snu1)) %>% 
    gt(groupname_col = "snu1") %>% 
    gt::cols_label(plhiv_datim = "PLHIV", 
                   tx_fy23q1 = "FY23 TX_CURR ", 
                   tx_gap = "Treatment Gap",
                   age_custom = "Age", 
                   gap_share = "Share of Gap") %>% 
    fmt_number(where(is.numeric), 
               decimals = 0) %>% 
    fmt_percent(columns = gap_share,
                decimals = 0) %>% 
    tab_source_note(
      source_note = gt::md(glue("{metadata$caption} & Spectrum 2023 Estimates"))) %>% 
    tab_options(
      source_notes.font.size = px(10)) %>% 
    gt_theme_nytimes() %>% 
    tab_header(
      title = glue("COP23 PRELIMINARY TREATMENT GAP SUMMARY"),
    ) %>% 
    tab_style(
      style = list(
        cell_text(weight = "bold")
      ),
      locations = cells_body(
        columns = everything(),
        rows = age_custom == "Total"
      )
    ) %>% 
    tab_style(
      style = list(
        cell_text(weight = "bold")
        ),
      locations = cells_row_groups()
    ) %>% 
    gtsave_extra( filename = glue("Images/COP23_{snu}_TX_AGE_GAP.png"))
  }

prov_list <- plhiv_gap_age_snu %>% 
  filter(str_detect(snu1, "Military", negate = T)) %>% 
  distinct(snu1) %>% 
  pull()

map(prov_list, .f = ~make_snu1_plhiv_table(plhiv_gap_age_snu, snu = .x))



