library(dplyr)
library(purrr)
library(yaml)

load_columns <- function(file) {
  paste0("data-raw/", file) %>%
    yaml.load_file() %>%
    map(as_tibble) %>%
    bind_rows()
}

column_glossary <-
  bind_rows(
    load_columns("glance.yaml"),
    load_columns("augment.yaml"),
    load_columns("tidy.yaml"),
    .id = "method"
  ) %>% mutate(
    method = recode(
      method,
      "1" = "glance",
      "2" = "augment",
      "3" = "tidy"
    )
  )

usethis::use_data(column_glossary, overwrite = TRUE)
