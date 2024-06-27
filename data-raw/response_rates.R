## code to prepare `response_rates` dataset goes here

devtools::load_all()

library(tidyverse)

path <- "data-raw/response_rates.xlsx"

rr <- readxl::read_xlsx(path)

stop("check range to read in!")

nmax = 80

rr <- readxl::read_xlsx(path, n_max = nmax)

names(rr)

response_rates <- rr

usethis::use_data(response_rates, overwrite = TRUE)
