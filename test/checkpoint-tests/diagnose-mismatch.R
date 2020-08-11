
# mismatched row
mmr <- 95

# mismatched metric
mmm <- "DM1619"

# Count mismatch row
count_row <- hh_count_check_df[mmr, c("HH_id", mmm, paste0(mmm, "_ind"))]

# Ind mismatch subset
ind_mm <- hh_ind %>% dplyr::filter(HH_id == count_row$HH_id)

# hh mismatch row
hh_row <- hhdata %>% filter(serial == count_row$HH_id) %>%
  .[ ,c("serial", paste0("DVAge_P", 1:10), mmm)]

View(count_row)
View(ind_mm)
View(hh_row)