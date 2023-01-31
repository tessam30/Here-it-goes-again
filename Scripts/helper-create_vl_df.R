# Creates a wide viral load coverage data frame for use in basic plots
create_vl_df <- function(df, ...) {
  df <- df %>%
    filter(
      indicator %in% c("TX_CURR", "TX_PVLS"),
      standardizeddisaggregate %in% c(
        "Age/Sex/HIVStatus",
        "Age/Sex/Indication/HIVStatus"
      )
    ) %>%
    gophr::clean_indicator() %>%
    group_by(indicator, fiscal_year, ...) %>%
    summarise(across(starts_with("qtr"), sum, na.rm = TRUE), 
              .groups = "drop") %>%
    reshape_msd(include_type = FALSE) %>%
    pivot_wider(
      names_from = indicator,
      names_glue = "{tolower(indicator)}"
    ) %>%
    group_by(...) %>% 
    mutate(
      tx_curr_lag2 = lag(tx_curr, n = 2),
      vlc = tx_pvls_d / tx_curr_lag2,
      vls = tx_pvls / tx_pvls_d,
      vls_adj = tx_pvls / tx_curr_lag2
    ) %>% 
    ungroup()
  return(df)
}  