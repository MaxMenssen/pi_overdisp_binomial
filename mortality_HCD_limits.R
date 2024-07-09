library(predint)

# HC data set
mortality_HCD
?mortality_HCD

# hist range
min(mortality_HCD$dead)
max(mortality_HCD$dead)

#-------------------------------------------------------------------------------
# np-chart

pi_bar <- sum(mortality_HCD$dead)/ sum(mortality_HCD$dead+mortality_HCD$alive)

50*pi_bar - 2 * sqrt(50 * pi_bar * (1-pi_bar))
50*pi_bar + 2 * sqrt(50 * pi_bar * (1-pi_bar))

#-------------------------------------------------------------------------------
# Mean ± 2 sd

mean(mortality_HCD$dead) - 2 * sd(mortality_HCD$dead)
mean(mortality_HCD$dead) + 2 * sd(mortality_HCD$dead)


#-------------------------------------------------------------------------------
# beta-binomial prediction interval
set.seed(3294)
pred_int_bb <- beta_bin_pi(histdat=mortality_HCD,
                           newsize=50)

pred_int_bb

# Estimates for pi and rho
pred_int_bb$pi
pred_int_bb$rho


#-------------------------------------------------------------------------------
# Quasi-binomial prediction interval
set.seed(3294)
pred_int_qb <- quasi_bin_pi(histdat=mortality_HCD,
                           newsize=50)

pred_int_qb

# Estimates for pi and rho
pred_int_qb$pi
pred_int_qb$phi

#-------------------------------------------------------------------------------

# Bayesian GLMM
# Load pi_bayes_glmm from GitHub before usage

# install.packages("rstan")
# install.packages("rstanarm")

library(rstan)
library(rstanarm)

pi_bayes_glmm(mortality_HCD)

#-------------------------------------------------------------------------------

# Bayesian hierarchical PI
# Load pi_bayes_hier from GitHub before usage

library(rstan)

pi_bayes_hier(mortality_HCD)


