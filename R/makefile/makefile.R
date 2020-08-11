
# Import libraries
library(haven)
library(tidyverse)

## Import data

source("R/data-preparation-functions.R")

hhdata <- read_spss("input/uktus15_household.sav") %>% # Import household data
  label_columns() %>% 
  select_productive_hhs()

## Split into individuals

hh_ind <- hhdata %>% 
  split(., seq(nrow(.))) %>% # Split into a list of rows
  map_dfr(., spread_by_individuals) # Expand into 1 df of individuals per house (and recombine)

## Export Data

write.csv(hh_ind, "output/model-pop-final.csv")
saveRDS(hh_ind, "output/model-pop-final.RDS")

## Run tests



