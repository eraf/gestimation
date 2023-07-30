## exposure modeling based standardization function

#' A function to calculate effect of a binary exposure (i.e. treatment) on the
#' outcome using standardization in exposure-modeling approach.
#'
#' @param data A data.frame containing all variables specified in the provided
#'             `outcome_model`.
#' @param exposure_model The exposure model.
#' @param y The outcome variable.
#'
#' @return A tibble containing risks for exposure level 0 and 1, Risk Difference,
#'         Risk ratio, Odds Ratio
#' @export

exposure_modeling_standardization <- function(data, exposure = NULL, y = NULL,
                                              exposure_model = NULL, outcome_model = NULL,
                                              id = NULL) {
  exposure <- exposure_model$terms[[2]]

  broom::augment(exposure_model, type.predict = "response", newdata = data) %>%
    dplyr::mutate(
      eL = dplyr::if_else({{ exposure }} == 1, .fitted, 1 - .fitted)
    ) %>%
    dplyr::mutate(
      w1 = ({{ exposure }}/eL) / sum({{ exposure }}/eL),
      w0 = ((1 - {{ exposure }})/eL) / sum((1 - {{ exposure }})/eL),
      effect1 = w1 * {{ y }},
      effect0 = w0 * {{ y }}
    ) %>%
    dplyr::summarize(
      risk0 = sum(effect0),
      risk1 = sum(effect1)
    ) %>%
    calc_assoc(.)
}
