
source("R/makefile.R")

# Import testing tools
library(testthat)

# Integrity of the hh_data file

# The number of unique hh ids is the same as the number of rows in the hh file
expect_equal(nrow(hhdata), length(unique(hh_ind$HH_id)))

# The number of unique hh ids is the same as the number of rows in the hh file
expect_equal(nrow(hhdata), length(unique(hhdata$serial)))

# The number of values per hh in the ind, matches expect hh size

hh_file_stripped <- hhdata %>% dplyr::select(
  HH_id = serial,
  DVHsize,
  NumChild,
  NumAdult,
  DM510,
  DM016,
  DM014,
  DM1115,
  DM1619
  )

hh_count_df <- hh_ind %>% 
  dplyr::group_by(HH_id) %>%
  dplyr::mutate(Age = as.numeric(Age)) %>%
  dplyr::summarise(
    DVHsize_ind  = n(),
    NumChild_ind = sum(Age <= 15),
    NumAdult_ind = sum(Age > 15),
    DM510_ind    = sum(Age >= 5 & Age <= 10),
    DM016_ind    = sum(Age <= 16),
    DM014_ind    = sum(Age <= 14),
    DM1115_ind   = sum(Age >= 11 & Age <= 15),
    DM1619_ind   = sum(Age >= 16 & Age <= 19)
    )

hh_count_check_df <- full_join(hh_file_stripped, hh_count_df, by="HH_id")

# The number of members per hh matches hh_file
expect_equal(as.numeric(hh_count_check_df$DVHsize), as.numeric(hh_count_check_df$DVHsize_ind))

# The number of children per hh matches hh_file
expect_equal(as.numeric(hh_count_check_df$NumChild), as.numeric(hh_count_check_df$NumChild_ind))

# The number of adults per hh matches hh_file
expect_equal(as.numeric(hh_count_check_df$NumAdult), as.numeric(hh_count_check_df$NumAdult_ind))

# The number of 5 - 10 year olds
expect_equal(as.numeric(hh_count_check_df$DM510), as.numeric(hh_count_check_df$DM510_ind))

# The number of 0 to 16 years olds
expect_equal(as.numeric(hh_count_check_df$DM016), as.numeric(hh_count_check_df$DM016_ind))

# The number of 0 to 14 year olds
expect_equal(as.numeric(hh_count_check_df$DM014), as.numeric(hh_count_check_df$DM014_ind))

# The number of 11 to 15 year olds 
expect_equal(as.numeric(hh_count_check_df$DM1115), as.numeric(hh_count_check_df$DM1115_ind))

# The number of 16 to 19 year olds
expect_equal(as.numeric(hh_count_check_df$DM1619), as.numeric(hh_count_check_df$DM1619_ind))

# The person-by-person metrics match across datasets

## Age Metrics

for(hh_size in 1:10){
  print(hh_size)
  for(pi in 1:hh_size){
    print(pi)
    expect_equal(
      as.numeric(hhdata %>% filter(DVHsize==hh_size) %>% .[[paste0("DVAge_P", pi)]]),
      hh_ind %>% dplyr::group_by(HH_id) %>% 
        dplyr::summarise(check = Age[pi], np = n()) %>% 
        filter(np == hh_size) %>%
        .[["check"]] %>%
        as.numeric()
    )
  }
}

## Sex metric

for(hh_size in 1:10){
  print(hh_size)
  for(pi in 1:hh_size){
    print(pi)
    expect_equal(
      as.character(hhdata %>% filter(DVHsize==hh_size) %>% .[[paste0("DMSex_P", pi)]]),
      hh_ind %>% dplyr::group_by(HH_id) %>% 
        dplyr::summarise(check = Sex[pi], np = n()) %>% 
        filter(np == hh_size) %>%
        .[["check"]]
    )
  }
}

## Checking that PN is always missing when N is greater than the number of individuals. 




