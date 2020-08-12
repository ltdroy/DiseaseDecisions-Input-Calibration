#' Prepare custom ONS table data of age/gender demographics for comparison
#'
#' @param df Custom ONS data loaded from input/ONS-Validation-Data "age-structure-stripped.csv"
#'
#' @return
#' @export
#'
#' @examples
prepare_ons_age_gender_data <- function(df){
  # Data shows count of each age (as a number), divided into genders
  # Data is in long format (Age, N, Sex)
  df <- df %>% 
    dplyr::select(Age_c=Age, N, Sex) %>%
    dplyr::group_by(Sex) %>% # The subsequent mutate step is grouped by sex
    dplyr::mutate(prop = N / sum(N)) %>% # Each count is converted to a proportion of the total within Male/Female
    dplyr::mutate(source="ONS") # Attach recognition of data source
  
  return(df)
}

#' Prepare output data from UKTUS for comparison with ONS aggregate data (age, gender)
#'
#' @param df The final processed data from this repo
#'           output/final-processed-data/model-pop-final.csv
#'
#' @return
#' @export
#'
#' @examples
prepare_UKTUS_data_for_age_gender_comp <- function(df){
  
  df <- df %>%
    # Match the age categorization of the ONS data
    dplyr::mutate(
      Age_c = case_when(
        Age < 90 ~ as.integar(Age),
        Age >= 90 ~ 90L
       )
      ) %>%
    dplyr::group_by(Age_c, Sex) %>% 
    dplyr::summarise(N = n()) %>% # Count individuals by age and gender
    dplyr::ungroup() %>%
    dplyr::select(Age_c, N, Sex) %>%
    dplyr::group_by(Sex) %>% # Calculate proportions within genders
    dplyr::mutate(
      prop = N / sum(N), 
      source = "UKTUS"
    )
  
  return(df)
}

#' Prepare custom ONS data (single occupancy HHs) for comparison with UKTUS data
#'
#' @param df Custom ONS data loaded from input/ONS-Validation-Data "single-person-households-by-age.csv"
#'
#' @return
#' @export
#'
#' @examples
prepare_ons_singlehh_data <- function(df){
  # Data shows count of single households by age and gender
  df <- df %>% 
    dplyr::mutate(Source="ONS") %>% # Add source information
    dplyr::filter(Group != "All") %>% # Male and female to be evaluated separately
    dplyr::group_by(Group) %>% # The subsequent mutate step needs to be separated for men and women
    dplyr::mutate(
      Prop = Count / sum(Count) 
    )
  
  return(df)
}

#' Prepare UKTUS output data for comparison with ONS aggregate data (Single HHs)
#'
#' @param df The final processed data from this repo
#'           output/final-processed-data/model-pop-final.csv
#'           
#' @return
#' @export
#'
#' @examples
prepare_UKTUS_data_for_single_hh_comp <- function(df){
  
  df <- df %>%
    dplyr::group_by(HH_id) %>%
    dplyr::summarise( # This groups the data to the household level
      Size = n(),
      Age = Age[1], # We only need one age, because we are only looking at single person households!
      Sex = Sex[1],
    ) %>%
    dplyr::ungroup() %>%
    dplyr::filter(Size == 1, Age > 15) %>% # Ons age groups begin at 16
    dplyr::mutate(
      Age.group = case_when( # This mirrors the ONS groupings
        Age >= 16 & Age <= 24 ~ "16-24",
        Age >= 25 & Age <= 44 ~ "25-44",
        Age >= 45 & Age <= 64 ~ "45-64",
        Age >= 65 & Age <= 74 ~ "65-74",
        Age >= 75            ~ "75 and over"
       )
      ) %>%
    dplyr::select(Age.group, Group=Sex) %>% # We only need to know the age group and sex for each singleton HH
    dplyr::group_by(Age.group, Group) %>% # Note: 'Group' matches the variable names in ONS
    dplyr::summarise(Count = n()) %>%
    dplyr::ungroup() %>% 
    dplyr::group_by(Group) %>% # We need proportions within levels of Sex
    dplyr::mutate(
      Prop = Count / sum(Count),
      Source = "UKTUS"
    ) %>%
    dplyr::select( # This matches the variable names in the processed ONS data
      Age.group,
      Count,
      Group,
      Source,
      Prop
    )
    
  return(df)
  
}

#' Prepare custom ONS data (household size distribution) for comparison with UKTUS data
#'
#' @param df Custom ONS data loaded from input/ONS-Validation-Data "household-sizes-stripped.csv"
#'
#' @return
#' @export
#'
#' @examples
prepare_ons_hhsize_data <- function(df){
  
  df <- df %>% dplyr::mutate(
    Prop = Count.2019 / sum(Count.2019),
    Source = "ONS"
    ) %>%
    dplyr::select(Size, Count = Count.2019, Prop, Source)
  
  return(df)
  
}

#' Prepare UKTUS output data for comparison with ONS aggregate data (HH size distribution)
#'
#' @param df The final processed data from this repo
#'           output/final-processed-data/model-pop-final.csv
#'
#' @return
#' @export
#'
#' @examples
prepare_UKTUS_data_for_hh_size_comp <- function(df){
  
  df <- df %>%
    dplyr::group_by(HH_id) %>%
    dplyr::summarise(Size = n()) %>% # We aggregate to the HH level and calculate HH size
    dplyr::ungroup() %>%
    dplyr::mutate(
      Size = case_when( # Note that all hhs above 6 are classified as 6 in the ONS table
        Size < 6 ~ Size,
        Size >= 6 ~ 6
      )
    ) %>%
    dplyr::group_by(Size) %>% # Now we need to regroup by size, to calculate size frequencies
    dplyr::summarise(Count = n()) %>%
    dplyr::ungroup() %>%
    dplyr::mutate( # Finally, we add the proportion of households of each size
      Prop = Count / sum(Count),
      Source = "UKTUS"
    ) %>%
    dplyr::select(
      Size, 
      Count,
      Prop,
      Source
    )
  
  return(df)
  
}






