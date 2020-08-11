
# Import libraries
library(haven)
library(tidyverse)
library(labelled)
library(testthat)

## Source custom functions

source("R/src/data-preparation-functions.R")
source("R/src/onthefly-validation-functions.R")


## Import and prepare data

hhdata <- read_spss("input/CTUS Data/uktus15_household.sav") %>% # Import household data
  label_columns() %>%
  check_labelling_of_key_variables() %>%
  select_productive_hhs() %>%
  check_productive_hhs_selected()

saveRDS(hhdata, file = "work/checkpoints/hhdata.RDS")

## Split into individuals

hh_ind <- hhdata %>% 
  split(., seq(nrow(.))) %>% # Split into a list of rows
  check_split(., hhdata) %>%
  map_dfr(., spread_by_individuals) # Expand into 1 df of individuals per house (and recombine)

## Export Data

write.csv(hh_ind, "output/final-processed-data/model-pop-final.csv")
saveRDS(hh_ind, "output/final-processed-data/model-pop-final.RDS")

## Run tests

### Validation of output data

source("test/checkpoint-tests/output-file-validation.R")

### Log validation errors



### Unit tests for custom functions

source("test/unit-tests/custom-function-unit-tests.R")

## ONS Equivalence Validation






