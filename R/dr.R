#' Doubly robust estimator used to estimate effect
#'
#' @param data A data.frame containing all variables specified in the provided
#'             `outcome_model` and `exposure_model`.
#' @param exposure The exposure (i.e. treatment) variable.
#' @param y The outcome variable.
#' @param exposure_model The exposure model.
#' @param outcome_model The outcome model.
#' @param id The unique identifier of subjects.
#'
#' @return A tibble containing risks for exposure level 0 and 1, Risk Difference,
#'         Risk ratio, Odds Ratio
#' @export

dr <- function(data, exposure = NULL, y = NULL,
               exposure_model = NULL, outcome_model = NULL,
               id = NULL) {
  datY1 <- augment(outcome_model, type.predict = 'response', newdata = data %>%
                     mutate({{ exposure }} := 1)) %>%
    select(!!sym(id), Y = {{ y }}, Yhat = .fitted) %>%
    left_join(
      augment(exposure_model, newdata = data, type.predict = "response") %>%
        mutate(eL = .fitted) %>%
        select(!!sym(id), A = {{ exposure }}, eL), by = id
    )
  EY1 <- datY1 |>
    mutate(p1 = A * Y/eL,
           p2 = Yhat * (A - eL)/eL,
           est = p1 - p2) |>
    summarise(EY1 = mean(est)) %>%
    as.numeric()

  datY0 <- augment(outcome_model, type.predict = 'response', newdata = data %>%
                     mutate({{ exposure }} := 0)) %>%
    select(!!sym(id), Y = {{ y }}, Yhat = .fitted) %>%
    left_join(
      augment(exposure_model, newdata = data, type.predict = "response") %>%
        mutate(eL = .fitted) %>%
        select(!!sym(id), A = {{ exposure }}, eL), by = id
    )
  EY0 <- datY0 %>%
    mutate(
      p1 = (1 - A) * Y / (1 - eL),
      p2 = Yhat * (A - eL) / (1 - eL),
      est = p1 + p2
    ) %>%
    summarise(EY0 = mean(est)) %>%
    as.numeric()



  return(tibble::tibble(r1 = EY1,
                        r0 = EY0,
                        rd = EY1 - EY0,
                        rr = EY1 / EY0,
                        or = EY1 / (1 - EY1) * (1 - EY0) / EY0))
}
