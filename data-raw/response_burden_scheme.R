## code to prepare `response_burden_scheme` dataset goes here

library(tidyverse)

path <- "data-raw/response_burden_scheme.xlsx"

response_burden_scheme <- readxl::read_xlsx(path, trim_ws = FALSE)

usethis::use_data(response_burden_scheme, overwrite = TRUE)
