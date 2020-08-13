apply_styling <- function(plt, 
                          legend_pos = "bottom",
                          legend_fsize = 8,
                          base_textsize = 10,
                          axis_fsize = 8
                          ){
  
  return(
    plt +
      ggplot2::theme_classic() +
      theme(legend.position=legend_pos, # Set text attributes, and legend position
            legend.text=element_text(size=legend_fsize, color="black"),
            text=element_text(size=base_textsize),
            axis.text = element_text(size=axis_fsize, color="black")
      )
  )
  
}

save_ggplot <- function(plt, label, fig_width=5.25, fig_height=5.25){
  
  ggsave( 
    filename = fs::path("output", "graphics", paste0(label, ".jpeg")),
    plot = plt,
    device = "jpeg",
    width = fig_width,
    height = fig_height,
    units = "in",
    dpi = 800
  )
  
}

read_and_combine_ons_and_uktus <- function(ons_file, ons_prep_function, uktus_prep_function){
  
  ons_data <- read.csv(
    fs::path("input", "ONS-Validation-Data", ons_file)
    ) %>% ons_prep_function()
  uktus_data <- read.csv(
    fs::path("output", "final-processed-data", "model-pop-final.csv"), 
    ) %>% uktus_prep_function() %>% select_at(names(ons_data))
  
  rbind(
    ons_data,
    uktus_data 
  )
  
}


#' Add variable 'Prop' which represents the Proportion of the total value of variable within groups
#'
#' @param df A data.frame/tibble
#' @param group_var A string indicating the variable to use for groups
#' @param value_var A string indicating the variable to transform to a Proportion
#'
#' @return
#' @export
#'
#' @examples
calc_Proportions_by_group <- function(df, group_var, value_var){
  
  df %>% 
    dplyr::group_by_at(group_var) %>%
    dplyr::mutate(Prop = calc_Proportion(!!as.symbol( value_var ))) %>%
    dplyr::ungroup()
    
  
}

#' Return values in a vector as Proportions of the sum of all vector elements
#'
#' @param x 
#'
#' @return
#' @export
#'
#' @examples
calc_Proportion <- function(x){

  return(x / sum(x))
  
}

#' Save df to the work/checkpoints folder as csv, return df
#'
#' @param df 
#' @param label 
#'
#' @return
#' @export
#'
#' @examples
checkpoint_this_as_csv <- function(df, label){
  
  write.csv(df, 
            fs::path(
              "work",
              "checkpoints",
              paste0(label, ".csv")
              )
            )
  
  return(df)
  
  
}



#' Prepare custom ONS table data of age/gender demographics for comparison
#'
#' @param df Custom ONS data loaded from input/ONS-Validation-Data "age-structure-ONS-UK.csv"
#'
#' @return
#' @export
#'
#' @examples
prepare_ons_age_gender_data <- function(df){
  # Data shows count of each age (as a number), divided into genders
  # Data is in long format (Age, N, Sex)
  df <- df %>% 
    checkpoint_this_as_csv(., label = "pre_ons_age_gender") %>%
    dplyr::select(Age_c=Age, N, Sex) %>%
    calc_Proportions_by_group(., group_var = "Sex", value_var = "N") %>%
    dplyr::mutate(source="ONS") %>% # Attach recognition of data source
    checkpoint_this_as_csv(., label = "post_ons_age_gender")
  
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
        Age < 90 ~ as.integer(Age),
        Age >= 90 ~ 90L
       )
      ) %>%
    dplyr::group_by(Age_c, Sex) %>% 
    dplyr::summarise(N = n()) %>% # Count individuals by age and gender
    dplyr::ungroup() %>%
    dplyr::select(Age_c, N, Sex) %>%
    calc_Proportions_by_group(., group_var = "Sex", value_var = "N") %>%
    dplyr::mutate(
      source = "UKTUS"
    ) %>%
    checkpoint_this_as_csv(., label = "post_UKTUS_age_gender_comp")
  
  return(df)
}

#' Prepare custom ONS data (single occupancy HHs) for comparison with UKTUS data
#'
#' @param df Custom ONS data loaded from input/ONS-Validation-Data "single-hh-composition-ONS-UK.csv"
#'
#' @return
#' @export
#'
#' @examples
prepare_ons_singlehh_data <- function(df){
  # Data shows count of single households by age and gender
  df <- df %>%
    checkpoint_this_as_csv(., label = "pre_ons_single_hh") %>%
    dplyr::mutate(Source="ONS") %>% # Add source information
    dplyr::filter(Group != "All") %>% # Male and female to be evaluated separately
    calc_Proportions_by_group(., group_var = "Group", value_var = "Count") %>%
    checkpoint_this_as_csv(., label = "post_ons_single_hh")

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
    dplyr::group_by(Group) %>% # We need Proportions within levels of Sex
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
    ) %>%
    dplyr::ungroup() %>%
    checkpoint_this_as_csv(., label = "post_UKTUS_single_hh")
    
  return(df)
  
}

#' Prepare custom ONS data (household size distribution) for comparison with UKTUS data
#'
#' @param df Custom ONS data loaded from input/ONS-Validation-Data "single-hh-composition-ONS-UK.csv"
#'
#' @return
#' @export
#'
#' @examples
prepare_ons_hhsize_data <- function(df){
  
  df <- df %>%
    checkpoint_this_as_csv(., label = "pre_ons_hhsize") %>%
    dplyr::mutate(
       Prop = Count.2019 / sum(Count.2019),
       Source = "ONS"
    ) %>%
    dplyr::select(Size, Count = Count.2019, Prop, Source) %>%
    checkpoint_this_as_csv(., label = "post_ons_hhsize")
  
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
        Size < 6 ~ as.integer(Size),
        Size >= 6 ~ 6L
      )
    ) %>%
    dplyr::group_by(Size) %>% # Now we need to regroup by size, to calculate size frequencies
    dplyr::summarise(Count = n()) %>%
    dplyr::ungroup() %>%
    dplyr::mutate( # Finally, we add the Proportion of households of each size
      Prop = Count / sum(Count),
      Source = "UKTUS"
    ) %>%
    dplyr::select(
      Size, 
      Count,
      Prop,
      Source
    ) %>%
    checkpoint_this_as_csv(., label = "post_uktus_hhsize")
  
  return(df)
  
}






