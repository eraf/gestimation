devtools::loaded_packages()
devtools::load_all()

ate <- function(r0, r1) {
  tibble::tibble(
    risk_difference = r1 - r0,
    risk_ratio = r1 / r0,
    odds_ratio = r1 / (1 - r1) * (1 - r0) / r0
  )
}
