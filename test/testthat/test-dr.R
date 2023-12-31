library(dplyr)
library(broom)
brfss0 <- brfss %>% filter(gt65 == 0)

exposure_mod <- glm(insured ~ female + whitenh + blacknh + hisp + multinh +
                      gthsedu + rural, family = "binomial",data = brfss)
outcome_mod <- glm(flushot ~ insured + female + whitenh + blacknh + hisp +
                     multinh + gthsedu + rural, family = "binomial",
                   data = brfss0)

brfss0 %>%
  mutate(id = row_number()) %>%
  dr(exposure = insured, y = flushot,
       outcome_model = outcome_mod, exposure_model = exposure_mod, id = "id")

bootstrap_ci(df = brfss1, f = dr, trtmod = exposure_mod, outmod = outcome_mod,
  x = insured, y = flushot, id = 'id')
