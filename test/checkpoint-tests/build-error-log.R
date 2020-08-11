## import needed data

hh_count_check_df <- readRDS("work/caches/hh_count_check_df.RDS")
hhdata <- readRDS("work/checkpoints/hhdata.RDS")
hh_ind <- readRDS("output/final-processed-data/model-pop-final.RDS")

## Construct nested list of mismatching households age 5-10

mismatching_on_510_indexes <- hh_count_check_df$DM510 != hh_count_check_df$DM510_ind

mismatch_510_nested_data <- map(
  hh_count_check_df$HH_id[mismatching_on_510_indexes], 
  function(id, df_flat, df_expanded){
    return(
      list(
        flat     = df_flat %>% dplyr::filter(HH_id == id),
        expanded = df_expanded %>% dplyr::filter(HH_id == id)
      )
    )
  },
  df_flat     = hhdata,
  df_expanded = hh_ind
)

