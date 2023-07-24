bootstrap_ci <- function(df, f, outmod = NULL, trtmod = NULL,
              x = NULL, y = NULL, id = NULL) {
  rd <- rep(NA, 20)
  rr <- rep(NA, 20)
  or <- rep(NA, 20)
  for(i in 1:20) {
    s <- slice_sample(df, prop = 1,
                      replace = T)
    out <- f(data = s, exposure_model = trtmod, outcome_model = outmod,
             exposure = {{ x }}, y = {{ y }}, id = id)
    rd[i] <- as.numeric(out[3])
    rr[i] <- as.numeric(out[4])
    or[i] <- as.numeric(out[5])
  }
  mean = c(mean(rd), mean(rr), mean(or))
  sd = c(sd(rd), sd(rr), sd(or))
  a = data.frame(mean, sd, row.names = c('risk diff', 'risk ratio',
                                         'odds ratio')) %>%
    mutate(conf.low = mean - 1.96*sd,
           conf.high = mean + 1.96*sd)
  return(round(a, 2))
}
