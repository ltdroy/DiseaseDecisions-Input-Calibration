testthat::test_that(
  desc = "calculate_Proportions_by_group segregates groups properly",
  code = {
    input_df <- data.frame(
      group = c(1, 1, 2, 2, 2, 2),
      values = c(1, 1, 1, 1, 1, 1)
      )
    
    output_df <- data.frame(
      group = c(1, 1, 2, 2, 2, 2),
      values = c(1, 1, 1, 1, 1, 1),
      prop   = c(0.5, 0.5, 0.25, 0.25, 0.25, 0.25)
    )
    
    testthat::expect_equivalent(
      calc_Proportions_by_group(input_df,
                                group_var="group",
                                value_var="values"),
      output_df
    )
    
    }
  )





