# functions to estimate effect --------------------------------------------

## outcome modeling based standardization function

#' A function to calculate effect of a binary exposure (i.e. treatment) on the
#' outcome using standardization in outcome-modeling approach.
#'
#' @param data A data.frame containing all variables specified in the provided
#'             `outcome_model`.
#' @param outcome_model The outcome model .
#' @param exposure The exposure (i.e. treatment) variable.
#'
#' @return A tibble containing risks for exposure level 0 and 1, Risk Difference,
#'         Risk ratio, Odds Ratio
#' @export

outcome_modeling_standardization <- function(data, exposure = NULL, y = NULL,
                                             exposure_model = NULL, outcome_model = NULL,
                                             id = NULL) {
  exposure_level_1 <- outcome_model %>%
    broom::augment(
      newdata = data %>% mutate({{ exposure }} := 1),
      type.predict = "response"
    )
  exposure_level_0 <- outcome_model %>%
    broom::augment(
      newdata = data %>% mutate({{ exposure }} := 0),
      type.predict = "response"
    )

  tibble::tibble(
    risk0 = mean(exposure_level_0$.fitted),
    risk1 = mean(exposure_level_1$.fitted)
  ) %>%
    calc_assoc(.)
}
