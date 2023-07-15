# https://www.ons.gov.uk/datasets/create/filter-outputs/eaf0785d-4f27-40c6-8c56-816da0058fc7#get-data

age_nssec_la_raw <- read_csv("https://www.ons.gov.uk/datasets/create/filter-outputs/eaf0785d-4f27-40c6-8c56-816da0058fc7?f=get-data&format=csv#get-data")

age_nssec_la <- age_nssec_la_raw |>
  janitor::clean_names()

age_nssec_la <- age_nssec_la |>
  select(la = lower_tier_local_authorities,
         age_91_categories,
         nssec_10_categories = national_statistics_socio_economic_classification_ns_se_c_10_categories,
         n = observation) |>
  mutate(across(where(is.character), tolower),
         age_91_categories = if_else(age_91_categories == "aged under 1 year",
                                     0,
           str_extract(age_91_categories, "\\d+") |>
             as.numeric()
         ))

usethis::use_data(age_nssec_la, overwrite = TRUE)
