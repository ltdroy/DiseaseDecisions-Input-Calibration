
#' Transform 'haven_labelled' columns in a dataframe into character columns
#' using the labels as values
#'   
#'
#' @param df 
#'
#' @return a data.frame 
#' @export
#'
#' @examples
label_columns <- function(df){
  
  df_labelled <- purrr::map_dfc(df, label_column)
  
  return(df_labelled)
  
}

#' If vector has class 'haven_labelled', convert to character vector using the 
#' labels as values
#'
#' @param column A vector
#'
#' @return A vector (converted to character using labels,
#'         if labelled vector was supplied). Otherwise returned unchanged. 
#' @export
#'
#' @examples
label_column <- function(column){
  
  if("haven_labelled" %in% class(column)){
    column <- labelled::to_character(column, levels="labels", nolabel_to_na=FALSE)
  }
  
  return(column)
}

#' Filter the dataframe to select only cases with one of the two 'productive'
#' outcome labels
#'
#' @param df The UKTUS household file
#'
#' @return The UKTUS household file, filtered for productive cases.
#' @export
#'
#' @examples
select_productive_hhs <- function(df){
  
  return(
    df %>% dplyr::filter(
      HhOut == "Productive : At least one individual interview but not from all eligible household members" |
      HhOut == "Productive : Household interview completed, all eligible household members completed individual interviews and diary" 
    )
  )
  
}

#' Given a df row for a household from the UKTUS household file, return
#' a dataframe with one row per individual in the household.
#'
#' @param df_row 
#'
#' @return A dataframe with one row per individual in the household. 
#'         The columns give the age and sex of each household member,
#'         as well as duplicating household information (size, ID) for each
#'         row. 
#' @export
#'
#' @examples
spread_by_individuals <- function(df_row){
  
  # This avoids inspecting the input multiple times
  household_size <- df_row$DVHsize
  
  testthat::expect_true(all(paste0("DVAge_P", 1:household_size) %in% names(df_row)))
  testthat::expect_true(all(paste0("DMSex_P", 1:household_size) %in% names(df_row)))
  testthat::expect_equal(
    sum(is.na(df_row[, paste0("DVAge_P", 1:household_size)])), 0
    )
  testthat::expect_equal(
    sum(is.na(df_row[, paste0("DMSex_P", 1:household_size)])), 0
  )
  
  return(
    
    data.frame(
      
      # This selects the age info for each houehold member, then converts
      # from one row dataframe to a character vector
      Age = df_row[, paste0("DVAge_P", 1:household_size)] %>% as.character(),
      
      Sex = df_row[, paste0("DMSex_P", 1:household_size)] %>% as.character(),
      
      Size = rep(household_size, household_size),
      
      HH_id = rep(df_row$serial, household_size),
      
      stringsAsFactors = FALSE
      
    )
    
  )
  
}


