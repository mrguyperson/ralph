awesome_rladies <- function(v) {

  sapply(v, function(x) {
    if (x == 1) {
      verb <- "is"
      noun <- "RLady"
    }

    if (x > 1) {
      verb <- "are"
      noun <- "RLadies"
    }

    as.character(glue::glue("There {verb} {x} awesome {noun}!"))
  })
}

write_rladies <- function(x){
  if (x == 1) {
    verb <- "is"
    noun <- "RLady"
  }

  if (x > 1) {
    verb <- "are"
    noun <- "RLadies"
  }

  as.character(glue::glue("There {verb} {x} awesome {noun}!"))
}

compose_rladies <- function(x){
  purrr::map_chr(x, write_rladies)
}
