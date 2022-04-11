#' Computes a tidy correlation
#'
#' more information goes here
#'
#' @param data input data set
#' @param var1 name of variable 1 (with or without quotes)
#' @param var2 name of variable 2 (with or without quotes)
#'
#' @return A tibble with Pearson correlation and p-value
#' @export
#'
#' @examples
#' compute_corr(data = faithful, var1 = eruptions, var2 = waiting)
#'
#' @importFrom rlang .data
#'
compute_corr <- function(data, var1, var2) {

  var1_char <- rlang::as_label(rlang::ensym(var1))
  var2_char <- rlang::as_label(rlang::ensym(var2))

  # alert user if variable not in data set ----

  if (!(var1_char %in% names(data))){
    #stop(glue::glue("{var1_char} is not in the data set."))
    usethis::ui_stop("{var1_char} is not in the data set.")
  }

  if (!(var2_char %in% names(data))){
    #stop(glue::glue("{var2_char} is not in the data set."))
    usethis::ui_stop("{var2_char} is not in the data set.")

  }

  # compute correlation ----
  results <-
    stats::cor.test(
      x = data %>% dplyr::pull({{ var1 }}),
      y = data %>% dplyr::pull({{ var2 }})
    ) %>%
    # tidy up results ----
    broom::tidy() %>%
    # retain and rename relevant bits ----
    dplyr::select(
      correlation = .data$estimate,
      pval = .data$p.value
    )

  attr(results$correlation, "names") <- NULL

  return(results)
}
