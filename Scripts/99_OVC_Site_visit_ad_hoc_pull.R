# PROJECT: Here it goes again
# PURPOSE: Munge and Analysis of IIT and TX_CURR trends
# AUTHOR: Tim Essam | SI
# REF ID:   5b0d0e46
# LICENSE: MIT
# DATE: 2023-02-01
# NOTES: Tim Essam | SI

# LOCALS & SETUP ============================================================================

# Load Q1 functions
source("Scripts/helper-call_all_helpers.R")

library(rcartocolor)
library(lubridate)
library(ggtext)
library(fontawesome)
library(extrafont)
library(tidytext)
library(patchwork)
library(ggtext)
library(selfdestructin5)
library(gt)
library(cascade) # Use dev version
library(ggpattern)
library(gtExtras)

# SI specific paths/functions  
load_secrets()

# Site level Genie file for IIT analysis
site_path <- return_latest(folderpath = merdata, 
                           pattern = "SITE_IM_Zambia_Daily")

get_metadata(file_path)

# REF ID for plots
ref_id <- "5b0d0e46"

# Functions  


# LOAD DATA ============================================================================  

df_site <- read_msd(site_path) %>% filter(funding_agency == "USAID")

df_site %>% 
  filter(str_detect(mech_name, "ECAP"), 
         str_detect(psnu, "Livingstone")) %>% 
  distinct(orgunituid, sitename)

df_site %>% 
  filter(orgunituid == "", fiscal_year %in% c(2022, 2023)) %>% 
  count(sitename, mech_code, indicator)
