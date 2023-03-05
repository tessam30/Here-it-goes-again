# PROJECT: PRECOP heatmap for MD
# PURPOSE: Munge and Analysis of DHS and Other data
# AUTHOR: Tim Essam | SI
# REF ID:   92dd6bb0
# LICENSE: MIT
# DATE: 2023-03-03
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
    library(GGally)
    library(raster)
    library(rasterVis)
    
    
  # SI specific paths/functions  
    load_secrets()
    merdata <- file.path(glamr::si_path("path_msd"))
    shpdata <- glamr::si_path("path_vector")
    file_path <- "../../../Downloads/COP 23 Target Setting.xlsx"
      
  # Grab metadata
  
  # REF ID for plots
    ref_id <- "92dd6bb0"
    
  # Functions  
  map_prov <- function(df, province = "Lusaka"){
    df %>% 
      filter(prov == {{province}}) %>% 
      ggplot(aes(geometry = geometry)) +
      geom_sf(data = snu1_geo, aes(geometry = geometry), fill = grey20k, color = "white") +
      geom_sf(fill = scooter_med, color = "white") +
      geom_sf_text(aes(label = {{province}}), 
                   size = 10/.pt, 
                   color = grey90k,
                   family = "Source Sans Pro",
                   ) +
      si_style_map() +
      labs(x = NULL, y = NULL)
  }
  
  # Pop density from 2022 Census
 pop_density <- tibble::tribble(
                           ~snu1, ~value,           ~indicator,
                        "Zambia",   26.1, "Population density",
                       "Central",   23.9, "Population density",
                    "Copperbelt",     88, "Population density",
                       "Eastern",   35.6, "Population density",
                       "Luapula",   29.9, "Population density",
                        "Lusaka",  140.1, "Population density",
                      "Muchinga",   13.1, "Population density",
                      "Northern",   20.8, "Population density",
                  "NorthWestern",   10.1, "Population density",
                      "Southern",   27.9, "Population density",
                       "Western",   10.8, "Population density"
                  )


  

# MAPS ============================================================================ 
 
 
    #  PULL IN PSNU MAPS info
    snu1_geo <- st_read("../Zambezi/GIS/snu1_fy22.shp") %>% 
      mutate(prov = str_remove_all(snu1, " Province"))
    plot(snu1_geo)
    
    cntry <- "Zambia"
    spdf_pepfar <- gisr::get_vcpolygons(path = shpdata, name = "VcPepfarPolygons.shp")
    zmb_geo <- purrr::map(3:5, ~spdf_pepfar %>% gisr::extract_boundaries(country = cntry, 
                                                                         level = .x))
    names(zmb_geo) <- list("adm0", "snu1", "psnu")
    
    
    # Create small multiples pulling apart the provinces
    prov_list <- snu1_geo %>% distinct(prov) %>% pull
    
    map_prov(snu1_geo)
    
    names(prov_list) <- prov_list

    plots <- map(prov_list,  ~map_prov(snu1_geo, .x)) 
    
    zmb_map <- zmb_geo$adm0 %>% 
      ggplot(aes(geometry = geometry)) +
      geom_sf() +
      geom_sf(data = snu1_geo, aes(geometry = geometry), fill = scooter_med, color = "white") +
      si_style_map()+
      labs(x = NULL, y = NULL)
    
    
    reduce(plots, `+`) +
      zmb_map
  

# DHS API & COP23 Targets ============================================================================
  
    library(RJSONIO)
    library(rdhs)
  
    indicators <- dhs_indicators()
    
    indic_list <- c("ED_LITR_M_LIT", "ED_EDAT_B_MYR", "ED_LITR_W_LIT", "ED_LITR_M_LIT", "CN_NUTS_C_HA2", "ED_NARP_B_BTH", "ED_EDUC_W_PRI", "ED_EDUC_M_PRI")
    
    resp <- dhs_data(indicatorIds = indic_list, countryIds = "ZM", surveyYearStart = 2018,breakdown = "subnational") %>% 
      select(indicator = Indicator, value = Value, snu1 = CharacteristicLabel)
    
    resp_zmb <- dhs_data(indicatorIds = indic_list, countryIds = "ZM", surveyYearStart = 2018) %>% 
      mutate(CharacteristicLabel = "Zambia") %>% 
      select(indicator = Indicator, value = Value, snu1 = CharacteristicLabel)
    
    zmb_df <- bind_rows(resp, resp_zmb, pop_density) %>% 
      mutate(snu1 = ifelse(snu1 == "North-Western", "NorthWestern", snu1)) %>% 
      mutate(indicator = fct_relevel(indicator, c("Population density",
                                                  "Women who are literate",
                                                  "Men who are literate",
                                                  "Women with primary education",
                                                  "Men with primary education",
                                                  "Net primary school attendance rate: Total",
                                                  "Children stunted",
                                                  "Median number of years of education: Both sexes")
                                     )
             )
    
    
    # FY23 Targets
    tgt <- read_excel(file_path, skip = 3, n_max = 10) %>% 
      janitor::clean_names()
    names(tgt)
  
# VIZ ============================================================================

    # Simple PCA on indicators
    zmb_df_wide <- zmb_df %>% 
      filter(str_detect(indicator, "Median", negate = T),
             snu1 != "Zambia") %>% 
      spread(indicator, value)
    
    set.seed(41)
    mod_clust <-  kmeans(select(zmb_df_wide, -snu1), centers = 5)    
    summary(mod_clust)  
    
    
    library(broom)
    pca_fit <- zmb_df_wide %>% 
      select(-snu1) %>% 
      prcomp(scale = T)

    pca_fit %>%
      augment(zmb_df_wide) %>% # add original dataset back in
      ggplot(aes(.fittedPC1, .fittedPC2)) + 
      geom_point(size = 1.5) +
      geom_vline(xintercept = 0) +
      geom_hline(yintercept = 0) +
      ggrepel::geom_label_repel(aes(label = snu1)) 
    
     
   zmb_df %>% 
      filter(str_detect(indicator, "Median", negate = T)) %>% 
      mutate(snu1_order = tidytext::reorder_within(snu1, value, indicator)) %>% 
      group_by(indicator) %>% 
      mutate(rank = dense_rank(value), .groups = "drop") %>% 
      mutate(mean_rank = mean(rank),
             snu1_order2 = fct_reorder(snu1, rank)) %>% 
      ggplot(aes(y = snu1_order2, x = indicator, fill = value)) +
      geom_tile(color = "white") +
      geom_text(aes(label = value), size = 10/.pt, 
                ) +
      scale_fill_viridis_c(direction = -1) 
   
   +
     facet_wrap(~indicator, scales = "free") +
     scale_y_reordered() +
     si_style()
    
   
   zmb_df_pc <-  zmb_df %>% 
     filter(str_detect(indicator, "Median", negate = T)) %>% 
     spread(indicator, value) %>% 
     mutate(snu1 = as.factor(snu1)) %>% 
     select(snu1, `Population density`, ``)


   ggparcoord(zmb_df_pc,
              columns = 2:8, groupColumn = 1, order = "allClass",
              showPoints = TRUE, 
              alphaLines = 0.5,
              scale="Center",
              mapping = ggplot2::aes(size = 1) 
   ) + 
  si_style()
   
# MAP Grided Surfaces ============================================================================
   
  raslist <- list.files(path = "GIS", pattern = "*MS_MEAN_v01.tif", recursive = T, full.names = T)
  zmb <- lapply(raslist, raster)

  # Create a raster stack for quick min/max calcuations
  zmb_stack <- raster::stack(raslist)
  minValue(zmb_stack)
  minV <- min(minValue(zmb_stack))
  maxV <- max(maxValue(zmb_stack))   
  
  cat(raslist,sep="\n")
  
  # Set for mapping
  ind_list <- lapply(zmb, rasterToPoints)
  
  ggplot(zmb_stack ) + geom_tile(aes(fill = value)) +
    facet_wrap(~ variable) +
    scale_fill_distiller(palette = "YlOrRd", direction = 1) 
  
  
  # Function to plot surfaces
map_surface <- function(indicator = "EDLITRMLIT"){

  df <- zmb_stack[[grep(indicator, names(zmb_stack))]] 
  df <- as(df, "SpatialPixelsDataFrame")
  test_df <- as.data.frame(df)
   
   map_df = setNames(test_df, c("z", "lon", "lat"))
   names(map_df)
  
   ggplot(map_df) +
     geom_tile(aes(fill = z, y = lat, x = lon)) +
     geom_sf(data = zmb_geo$adm0, aes(geometry = geometry), fill = "NA", color = grey90k, size = 2) +
     #geom_sf(data = snu1_geo, aes(geometry = geometry), fill = "NA", color = "white") +
     scale_fill_viridis_c(direction = -1, na.value="white") +
     si_style_map() +
     labs(x = NULL, y = NULL)
}


map_surface("CNNUTSCHA2") +
  geom_sf(data = prov_dhs_shp, aes(geometry = geometry), fill = "NA", color = "white") +
  geom_sf_text(data = prov_dhs_shp %>% filter(str_detect(indicator, "stunted")), 
            aes(label = percent(round(value, digits = 0), scale = 1), geometry = geometry))


# Merge in DHS to snu1_geo (or vice versa)
prov_dhs_shp <- snu1_geo %>% right_join(zmb_df, by = c("prov" = "snu1"))

