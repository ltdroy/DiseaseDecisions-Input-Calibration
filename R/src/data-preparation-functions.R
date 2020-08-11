
label_columns <- function(df){
  
  library(purrr)
  library(labelled)
  
  label_column <- function(column){
    if("haven_labelled" %in% class(column)){
      column <- labelled::to_character(column, levels="labels", nolabel_to_na=FALSE)
    }
    return(column)
  }
  
  df_labelled <- map_dfc(df, label_column)
  
  return(df_labelled)
  
}

select_productive_hhs <- function(df){
  
  return(
    df %>% filter(
      HhOut == "Productive : At least one individual interview but not from all eligible household members" | HhOut == "Productive : Household interview completed, all eligible household members completed individual interviews and diary" 
    )
  )
  
}

spread_by_individuals <- function(df_row){
  
  household_size <- df_row$DVHsize
  
  return(
    
    data.frame(
      
      Age = df_row[, paste0("DVAge_P", 1:household_size)] %>% as.character(),
      
      Sex = df_row[, paste0("DMSex_P", 1:household_size)] %>% as.character(),
      
      Size = rep(household_size, household_size),
      
      HH_id = rep(df_row$serial, household_size),
      
      stringsAsFactors = FALSE
      
    )
    
  )
  
}


