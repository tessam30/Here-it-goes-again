# Swap targets from EQUIP to Action HIV -- but can be used generally to swap targets
# SWAP EQUIP TARGETS ------------------------------------------------------

swap_targets <- function(.data, mech1 = "18304", mech2 = "82075", new_mech = "Action HIV") {
  # Using EQUIP as default as this has to be done each time in FY21
  .data %>%
    mutate(mech_code = ifelse(mech_code == {{mech1}}, {{mech2}}, mech_code),
           mech_name = ifelse(mech_code == {{mech2}}, new_mech, mech_name))
} 
