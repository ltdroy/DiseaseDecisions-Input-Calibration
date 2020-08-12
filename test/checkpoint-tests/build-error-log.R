## import needed data

hh_count_check_df <- readRDS(fs::path("work", "caches", "hh_count_check_df.RDS"))
raw_hh_data <- read_spss(fs::path("input", "UKTUS-Data", "uktus15_household.sav")) %>%
  mutate(HH_id = serial) 

## Construct record of problematic rows

hh_count_check_df %>%
  dplyr::mutate(
    # Here I need to identify cases where the individual summary and original file mistmatch
    mismatch_510  = DM510  != DM510_ind,
    mismatch_1115 = DM1115 != DM1115_ind,
    mismatch_1619 = DM1619 != DM1619_ind
  ) %>%
  dplyr::mutate(
    any_mismatch = (mismatch_510 | mismatch_1115 | mismatch_1619)
  ) %>%
  dplyr::filter(
    # I only want the rows where there is a mismatch
    any_mismatch
  ) %>%
  dplyr::left_join(
    # I want rows of the raw data whose households correspond to mismatches
    ., 
    y=raw_hh_data, by="HH_id"
  ) %>%
  dplyr::select(
    # I need to select the key information that data holders
    # would need in order to fix the data
    serial=HH_id,
    any_mismatch,
    DM510=DM510.y,
    mismatch_510,
    DM1115=DM1115.y,
    mismatch_1115,
    DM1619=DM1619.y,
    mismatch_1619,
    dplyr::starts_with("DVAge_P")
  ) %>% saveRDS(
    file=fs::path("work", "caches", "hh_data_error_log.RDS")
  )








