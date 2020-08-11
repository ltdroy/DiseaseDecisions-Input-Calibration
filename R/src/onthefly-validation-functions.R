check_labelling_of_key_variables <- function(df){
  
  testthat::expect_length(object = unique(df$DMSex_P1), n = 2)
  testthat::expect_true(sum(is.na(df$DMSex_P1))==0)
  testthat::expect_true(sum(is.na(df$DVAge_P1))==0)
  
  return(df)
}

check_productive_hhs_selected <- function(df){
  
  testthat::expect_true(all(df$HhOut=="Productive : At least one individual interview but not from all eligible household members" |
                            df$HhOut == "Productive : Household interview completed, all eligible household members completed individual interviews and diary" ))
  
  return(df)
}

check_split <- function(rowlist, old_df){

  testthat::expect_length(rowlist, n = nrow(old_df))
  
  return(rowlist)
  
}