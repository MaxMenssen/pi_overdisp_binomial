#-------------------------------------------------------------------------------
#--------------- Prediction interval from Bayesian GLMM ------------------------
#-------------------------------------------------------------------------------


pi_bayes_glmm <- function(dat, sided = "both", al = .05, seed = 1216) {
        ## cluster size:
        n <- sum(dat[1,])
        ## number of clusters:
        H <- nrow(dat)
        ## Bernoulli data: one row per individual observation: 
        dat01 <- data.frame(
                succ = c(sapply(dat[, 1], function(z){c(rep(1L, z), rep(0L, n - z))})), 
                cid = rep(1:H, each = n)
        ) 
        ## fit GLMM using stan/rstanarm
        fit <- rstanarm::stan_glmer(succ ~ 1 + (1 | cid), data = dat01, 
                                    family = binomial(), 
                                    iter = 2500, warmup = 1250, chains = 4, 
                                    save_warmup = FALSE, seed = seed)
        ## extract and derive MCMC samples for relevant parameters:
        ## fixed global intercept and cluster-wise random intercepts:
        E <- rstan::extract(fit$stanfit, pars = c("alpha", "b"))
        ## linear predictor for future cluster observation:
        et <- E$alpha[, 1] + E$b[, H+1]
        ## one future observation:
        set.seed(seed)
        yy <- rbinom(n = 5000, size = n, prob = plogis(et))
        ## PI limit(s):
        if (sided == "both") {
                res <- quantile(yy, c(al / 2, 1 - al / 2))
                names(res) <- c("lower", "upper")
        }
        else {
                res <- quantile(yy, ifelse(sided == "lower", al, 1 - al))
                names(res) <- sided
        }
        return(res)
}
