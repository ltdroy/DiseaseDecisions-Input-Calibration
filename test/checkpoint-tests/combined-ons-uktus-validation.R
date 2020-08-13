expect_equal(
  read.csv(
    fs::path("work", "checkpoints", "combined_hh_size_dist.csv")
  ) %>%
    dplyr::group_by(Source) %>%
    dplyr::summarise(
      total_prop = sum(Prop)
    ) %>%
    .[["total_prop"]], 
  c(1,1)
)

expect_equal(
  read.csv(
    fs::path("work", "checkpoints", "combined_single_hh_dist.csv")
  ) %>%
    dplyr::group_by(Group, Source) %>%
    dplyr::summarise(
      total_prop = sum(Prop)
    ) %>%
    .[["total_prop"]], 
  rep(x = 1, 4)
)

expect_equal(
  read.csv(
    fs::path("work", "checkpoints", "combined_age_gender_dist.csv")
  ) %>%
    dplyr::group_by(Sex, source) %>%
    dplyr::summarise(
      total_prop = sum(Prop)
    ) %>%
    .[["total_prop"]], 
  rep(x = 1, 4)
)


