# https://www.ons.gov.uk/datasets/create/filter-outputs/d55c516b-ab6e-4cbc-b7f9-56a957b0be65#get-data

age_disability_la_raw <- read_csv("https://www.ons.gov.uk/datasets/create/filter-outputs/d55c516b-ab6e-4cbc-b7f9-56a957b0be65?f=get-data&format=csv#get-data")

age_disability_la <- age_disability_la_raw |>
  janitor::clean_names()

age_disability_la <- age_disability_la |>
  select(la = lower_tier_local_authorities,
         age_91_categories,
         disability_3_categories,
         n = observation) |>
  mutate(across(where(is.character), tolower),
         age_91_categories = if_else(age_91_categories == "aged under 1 year",
                                     0,
           str_extract(age_91_categories, "\\d+") |>
             as.numeric()
         ))

usethis::use_data(age_disability_la, overwrite = TRUE)
