rm(list = ls())
devtools::load_all()

list.files("data-raw", "rda$", full.names = TRUE) %>%
lapply(load, globalenv())

ls() %>%
{paste0(
  "usethis::use_data(",
  paste(., collapse = ", "),
  ")"
)} %>%
parse(text = .) %>%
eval(.)
