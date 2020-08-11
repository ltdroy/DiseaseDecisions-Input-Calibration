testthat::test_that(
  desc = "Label columns, labels a dataframe as expected",
  code = {
    
    test_input <- data.frame(
      x = labelled::labelled(
        c(1,2,2,2,3,9,1,3,2,NA),
        c(yes = 1, no = 3, "don't know" = 8, refused = 9)
        ),
      y = labelled::labelled(
        c(1,2,2,2,3,9,1,3,2,NA),
        c(yes = 1, no = 3, "don't know" = 8, refused = 9)
        ),
      stringsAsFactors = FALSE
      )
    
     test_output <- data.frame(
       x = c("yes", "2", "2", "2", "no", "refused", "yes", "no", "2", NA),
       y = c("yes", "2", "2", "2", "no", "refused", "yes", "no", "2", NA),
       stringsAsFactors = FALSE
     )
     
     testthat::expect_equivalent(label_columns(test_input), test_output)
    
   }
  )

testthat::test_that(
  desc = "Spread by individuals works as expected",
  code = {
    
    test_input <- data.frame(
      DVHsize = 2,
      DVAge_P1 = 3,
      DVAge_P2 = 15,
      DMSex_P1 = "Female",
      DMSex_P2 = "Male",
      serial   = 99,
      stringsAsFactors = FALSE
    )
    
    test_output <- data.frame(
      Age = c("3", "15"),
      Sex = c("Female", "Male"),
      Size = c(2, 2),
      HH_id = c(99, 99),
      stringsAsFactors = FALSE
    )
    
    testthat::expect_equivalent(spread_by_individuals(test_input), test_output)
  }
)

