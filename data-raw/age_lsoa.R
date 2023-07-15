# https://www.ons.gov.uk/datasets/create/filter-outputs/3ed959a7-1fe1-451f-9bed-6a189a263a56#get-data

age_lsoa_raw <- read_csv("https://www.ons.gov.uk/datasets/create/filter-outputs/3ed959a7-1fe1-451f-9bed-6a189a263a56?f=get-data&format=csv#get-data")

age_lsoa <- age_lsoa_raw |>
  janitor::clean_names()

age_lsoa <- age_lsoa |>
  select(lsoa = lower_layer_super_output_areas,
         age_91_categories,
         n = observation) |>
  mutate(across(where(is.character), tolower),
         age_91_categories = if_else(age_91_categories == "aged under 1 year",
                                     0,
           str_extract(age_91_categories, "\\d+") |>
             as.numeric()
         ))

usethis::use_data(age_lsoa, overwrite = TRUE)
