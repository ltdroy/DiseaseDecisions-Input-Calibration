# Import libraries
library(haven)
library(tidyverse)
library(labelled)
library(testthat)
library(fs)
library(ggplot2)
library(extrafont)
library(readxl)
library(rio)
library(stringr)

# Patch folder structure

source(fs::path("R","src", "patch-folder-structure.R"))

## Source custom functions

source(fs::path("R", "src", "data-preparation-functions.R"))

source(fs::path("R", "src", "onthefly-validation-functions.R"))

## Import and prepare data

hhdata <- read_spss(fs::path("input","UKTUS-Data", "uktus15_household.sav")) %>% # Import household data
  label_columns() %>%
  check_labelling_of_key_variables() %>%
  select_productive_hhs() %>%
  check_productive_hhs_selected()

saveRDS(hhdata, file = fs::path("work","checkpoints", "hhdata.RDS"))

## Split into individuals

hh_ind <- hhdata %>% 
  split(., seq(nrow(.))) %>% # Split into a list of rows
  check_split(., hhdata) %>%
  map_dfr(., spread_by_individuals) # Expand into 1 df of individuals per house (and recombine)

## Export Data

write.csv(hh_ind, fs::path("output", "final-processed-data", "model-pop-final.csv"))
saveRDS(hh_ind, fs::path("output", "final-processed-data", "model-pop-final.RDS"))

## Run tests

### Validation of output data

source(fs::path("test", "checkpoint-tests", "output-file-validation.R"))

### Log validation errors

source(fs::path("test", "checkpoint-tests", "build-error-log.R"))

### Unit tests for custom functions

source(fs::path("test", "unit-tests", "custom-function-unit-tests.R"))

## ONS Equivalence Validation

source(fs::path("R", "src", "prepare-ons-data.R"))

source(fs::path("R", "src", "functions-for-ONS-equivilence.R"))

source(fs::path("test", "unit-tests", "unit-tests-ons-equivilence.R"))

### Comparison on age distribution (by age)

p1 <- read_and_combine_ons_and_uktus(
  ons_file = "age-structure-ONS-UK.csv",
  ons_prep_function = prepare_ons_age_gender_data,
  uktus_prep_function = prepare_UKTUS_data_for_age_gender_comp 
) %>%
  checkpoint_this_as_csv(., label = "combined_age_gender_dist") %>%
  ggplot(., aes(x = Age_c, y = Prop)) +
  geom_bar(alpha = 0.5,
           stat = "identity",
           width = 1) +
  facet_grid(rows = vars(Sex), cols = vars(source)) +
  labs(x = "Age in Years (1-89, 90+)",
       y = "Relative Freq (Proportion)")

p1 %>% apply_styling() %>%
  save_ggplot("age_structure_comp")

## Comparison by single hh composition

p2 <- read_and_combine_ons_and_uktus(
  ons_file = "single-hh-composition-ONS-UK.csv",
  ons_prep_function = prepare_ons_singlehh_data,
  uktus_prep_function = prepare_UKTUS_data_for_single_hh_comp
) %>%
  checkpoint_this_as_csv(., label = "combined_single_hh_dist") %>%
  ggplot(., aes(x = Age.group, y = Prop, fill=Source)) +
  geom_bar(alpha = 0.5,
           stat = "identity",
           position="dodge") +
  facet_wrap(~ Group) +
  labs(x = "Age group",
       y = "Relative Freq (Proportion)")

p2 %>% apply_styling() %>%
  save_ggplot("single_hh_comp", fig_width = 5.6)

## Comparison by hh size distribution

p3 <- read_and_combine_ons_and_uktus(
  ons_file = "hh-sizes-ONS-UK.csv",
  ons_prep_function = prepare_ons_hhsize_data,
  uktus_prep_function = prepare_UKTUS_data_for_hh_size_comp
) %>%
  checkpoint_this_as_csv(., label = "combined_hh_size_dist") %>%
  ggplot(., aes(x = Size, y = Prop, fill = Source)) +
  geom_bar(alpha = 0.5,
           stat = "identity",
           position = "dodge") +
  scale_x_continuous(breaks = c(1, 2, 3, 4, 5, 6)) +
  labs(x = "Household Size (1-5, 6+)",
       y = "Relative Freq (Proportion)")

p3 %>% apply_styling() %>%
  save_ggplot("hh_distribution_comp")

## Validate combined data files

source(fs::path("test", "checkpoint-tests", "combined-ons-uktus-validation.R"))

## Render the README File


