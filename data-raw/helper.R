
# while we're in the process of defining helpers, it's nice to know what tidiers
# the column come from in the first place. this is a temporary(?) helper

tidiers <- function(column) {
  library(tidyverse)
  # assumes modeltests and broom git repos are cloned to same folder and pwd is modeltests folder

  # TODO: make sure to only

  doc_files <- list.files("../broom/man", full.names = TRUE)

  get_name <- function(file) {
    lines <- readLines(file)
    column_present <- any(str_detect(lines, column))  # need to add the \item{column} wrapper
    name <- substring(lines[3], 7)
    name <- substring(name, 1, nchar(name) - 1)
    if (column_present) name else NULL
  }

  as.character(compact(map(doc_files, get_name)))
}


tidiers("estimate")
tidiers("class")
