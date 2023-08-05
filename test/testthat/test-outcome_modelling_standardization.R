library(dplyr)

brfss0 <- brfss %>% filter(gt65 == 0)

outcome_mod <- glm(flushot ~ insured + female + whitenh + blacknh + hisp +
                     multinh + gthsedu + rural, family = "binomial",
                   data = brfss0)

outcome_modeling_standardization(data = brfss0, outcome_model =  outcome_mod,
                                 exposure =  insured)


bootstrap_ci(df = brfss0, f = outcome_modeling_standardization,
  outmod = outcome_mod, x = insured)
