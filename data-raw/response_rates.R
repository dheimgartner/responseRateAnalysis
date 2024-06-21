## code to prepare `response_rates` dataset goes here

devtools::load_all()

library(tidyverse)

path <- "data-raw/response_rates.xlsx"

rr <- readxl::read_xlsx(path)

stop("check range to read in!")

nmax = 77

rr <- readxl::read_xlsx(path, n_max = nmax)

names(rr)

rr <-
  rr %>%
  mutate(flag_yes_no = yes_recruitment_no_incentive != 0,
         flag_yes_yes = yes_recruitment_yes_incentive != 0,
         flag_no_no = no_recruitment_no_incentive != 0) %>%
  select(-c(yes_recruitment_no_incentive,
            yes_recruitment_yes_incentive,
            no_recruitment_no_incentive))

response_rates <- rr

usethis::use_data(response_rates, overwrite = TRUE)
