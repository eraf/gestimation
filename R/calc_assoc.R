calc_assoc <- function(data) {
  data %>%
    mutate(
      risk_diff = risk1 - risk0,
      risk_ratio = risk1 / risk0,
      odds_ratio = (risk1/(1-risk1)) / (risk0/(1-risk0))
    )
}
