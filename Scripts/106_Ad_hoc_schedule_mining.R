# PROJECT: Visualize Room Agendas for DATA/HIS SMES
# PURPOSE: Munge and Analysis of Schedule Data
# AUTHOR: Tim Essam | SI
# REF ID:   4f486b53
# LICENSE: MIT
# DATE: 2023-03-05
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
    
    
  # SI specific paths/functions  
    load_secrets()
    gd_id <- c("1xDbgejuxuY6lbDF_G-hAu-jJXL85MrottofBP98FEWs")
      
    
  # Functions  
  

# LOAD DATA ============================================================================  

  df <- googlesheets4::read_sheet(gd_id)

# MUNGE ============================================================================
  
  df %>% 
      mutate(wday = wday(Date, label = T),
             lunch = str_detect(Topic, "Lunch|lunch|Tea"),
             fill_col = case_when(
               lunch == TRUE ~ grey10k,
               Flag == "Yes" ~ scooter_med,
               TRUE ~ grey20k)
             ) %>% 
      ggplot() +
      # geom_linerange(aes(y = "A",
      #                    xmin = start,
      #                    xmax = end,
      #                    colour = fill_col), 
      #                linewidth = 4) +
      # geom_segment(aes(y = wday,
      #                  yend = wday,
      #                  x = start,
      #                  xend = end,
      #                  colour = "white"), size = 6) +
      geom_segment(aes(y = wday,
                       yend = wday,
                       x = start,
                       xend = end,
                       colour = fill_col), size = 6) +
      geom_text(data = . %>% filter(!is.na(Flag)), 
                                    aes(x = start, label = Topic, y = wday), size = 8/.pt, hjust = 0) +
      facet_wrap(~OU, ncol = 1, scale = "free_y") +
      scale_color_identity() +
      scale_y_discrete(limits=rev)+
      si_style_xgrid(facet_space = 0.25) +
      scale_x_datetime(breaks = scales::date_breaks("60 mins"), date_labels = "%H:%M") +
      labs(x = NULL, y = NULL, title = "Occurences of HIS or Data Systems in OU Schedules")
               

  
# VIZ ============================================================================

    
    ggplot(data, aes(fill=condition, y=value, x=specie)) + 
      geom_bar(position="stack", stat="identity")
    
    
      base <- ggplot(
        se.df,
        aes(
          x = Start.Date, reorder(Action,Start.Date), color = Comms.Type
        ))
      base + geom_segment(aes(
        xend = End.Date,
        ystart = Action, yend = Action
      ), size = 5) + 
        facet_grid(Source ~ .,scale = "free_y",space = "free_y", drop = TRUE) 

# SPINDOWN ============================================================================

