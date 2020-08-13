# Prepare age gender structure file

rio::import(
  "https://www.ons.gov.uk/visualisations/dvc671/pyramids2/datadownload.xls",
  sheet = "2018"
) %>%
  dplyr::filter(geogname == "UNITED KINGDOM") %>%
  tidyr::pivot_longer(.,
                      cols = m_18_0:f_tot,
                      names_to = "Age_code",
                      values_to = "N") %>%
  dplyr::filter(Age_code != "m_tot", Age_code != "f_tot") %>%
  dplyr::mutate(
    Age = as.numeric(stringr::str_extract(string = Age_code, pattern = "\\d+$")),
    Sex = stringr::str_extract(string = Age_code, pattern = "^\\w")
  ) %>%
  dplyr::mutate(Sex = case_when(Sex == "m" ~ "Male",
                                Sex == "f" ~ "Female"),
                Source = "ONS") %>%
  dplyr::select(Age, N, Sex, Source) %>%
  write.csv(fs::path("input", "ONS-Validation-Data", "age-structure-ONS-UK.csv"),
            row.names = FALSE
            )

# Prepare hh size distribution file

rio::import(
  "https://www.ons.gov.uk/file?uri=%2fpeoplepopulationandcommunity%2fbirthsdeathsandmarriages%2ffamilies%2fdatasets%2ffamiliesandhouseholdsfamiliesandhouseholds%2fcurrent/fandh2019final.xls",
  sheet = "5",
  range = "BS9:BS14",
  col_names = FALSE
) %>%
  dplyr::select(Count.2019 = `...1`) %>%
  dplyr::mutate(Size = 1:6) %>%
  dplyr::select(Size,
                Count.2019) %>%
  write.csv(fs::path("input", "ONS-Validation-Data", "hh-sizes-ONS-UK.csv"),
            row.names = FALSE)
  
# Prepare single hh composition file

rio::import(
  "https://www.ons.gov.uk/file?uri=%2fpeoplepopulationandcommunity%2fbirthsdeathsandmarriages%2ffamilies%2fdatasets%2ffamiliesandhouseholdsfamiliesandhouseholds%2fcurrent/fandh2019final.xls",
  sheet = "6",
  range = "A9:BU26",
  col_names = FALSE
) %>%
  dplyr::select(Age.group=`...1`, Count=`...71`) %>%
  mutate(
    Group = c(rep("All", 6), rep("Male", 6), rep("Female", 6))
  ) %>%
  dplyr::filter(
    Age.group != "People", Age.group != "Male", Age.group != "Female"
  ) %>%
  dplyr::mutate(
    Source = "ONS"
  ) %>%
  write.csv(fs::path("input", "ONS-Validation-Data", "single-hh-composition-ONS-UK.csv"),
            row.names = FALSE)



