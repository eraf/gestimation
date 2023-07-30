# Answers to the problems -------------------------------------------------

load("C:/Users/erafh/gestimation/dataset/brfss.RData")

library(dplyr)

brfss0 <- brfss %>% filter(gt65 == 0)


exposure_mod <- glm(insured ~ female + whitenh + blacknh + hisp + multinh +
                      gthsedu + rural, family = "binomial",data = brfss0)

exposure_modeling_standardization(data = brfss0,
                                  exposure_model =  exposure_mod,
                                  y = flushot)

bootstrap_ci(df = brfss0, f = exposure_modeling_standardization,
  trtmod = exposure_mod, y = flushot)

