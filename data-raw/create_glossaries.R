library(tidyverse)

methods <- c("tidy", "glance", "augment")

recode_method <- function(df) {
  mutate(df, method = recode(method, "1" = "tidy", "2" = "glance", "3" = "augment"))
}

load_yaml <- function(method, columns = TRUE) {
  prefix <- if (columns) "columns" else "arguments"
  key_name <- if (columns) "column" else "argument"
  yml_file <- paste0("data-raw/", prefix, "_", method, ".yaml")

  yml <- yaml::read_yaml(yml_file)
  # rewrite yaml alphabetically for convenience
  yml <-  yml[order(names(yml))]
  write_yaml(yml, yml_file)

  yml %>%
    unlist() %>%
    tibble::enframe(key_name, "description")
}

column_glossary <- methods %>%
  map_df(load_yaml, .id = "method") %>%
  recode_method()

argument_glossary <- methods %>%
  map_df(load_yaml, columns = FALSE, .id = "method") %>%
  recode_method()

usethis::use_data(argument_glossary, column_glossary, overwrite = TRUE)

