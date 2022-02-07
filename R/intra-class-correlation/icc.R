


library(tidyverse)
library(lme4) # lmm
library(modelr) # get residuals
# library(performance) # compute ICC
library(insight) # getting variance components

n <- 20 # number of individuals (units of analysis) between 10 and 20
p <- 20 # number of measures between 2 and 20
icc_set <- 0.5 # intra class correlation between 0.001 and 0.999
var_random <- 10 # between 0.01 and 50
var_resid <- var_random/icc_set - var_random

# simulate data
set.seed(1234)
intercept <- 100
patient <- rep(seq(1:n), p) %>% factor()
patient_effect <- rnorm(n, 0, sqrt(var_random))
measure <- rep(1:p, each = n)
y <- rnorm(n*p, intercept + (patient_effect %>% rep(p)), sqrt(var_resid))
datalong <- tibble(patient, measure, y)

model <- datalong %>%
  lmer(y ~ measure + (1|patient), REML = TRUE, data = .)



# showing clustering
datalong %>%
  ggplot(aes(x = patient, y = y)) +
  geom_boxplot()

# showing clustering after inclusion of random effect
datalong %>%
  add_residuals(model) %>%
  ggplot(aes(x = patient, y = resid)) +
  geom_boxplot() +
  ylab("Residual")

# ICC <- model %>% icc()
# get_variance_residual(model)
# get_variance_fixed(model)
# get_variance_random(model)

# icc for a random intercept model
icc <- get_variance_random(model)/(get_variance_random(model) + get_variance_residual(model))

cat(icc_set, icc)





