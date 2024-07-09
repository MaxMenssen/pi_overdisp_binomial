#-------------------------------------------------------------------------------
#---------- Bayesian hierarchical prediction interval --------------------------
#-------------------------------------------------------------------------------

pi_bayes_hier <- function(dat, sided = "both", al = .05, seed = 1216) {
        ## stan code:
        code_char <- "
    data {
      int<lower=1> H; // number of clusters
      int<lower=1> n; // number of observations per cluster
      int<lower=0> YY[H]; // numbers of successes out of n 
    }
    parameters {
      real<lower=0,upper=1> my; // beta-prop-distribution mean
      real<lower=0> ka; // beta-prop-distribution precision  
      real<lower=0,upper=1> pi[H]; // clusters' success probabilities
    }
    model {
      ka ~ gamma(2, 0.005); // weakly informative prior giving precision's domain 
      pi ~ beta_proportion(my, ka);
      YY ~ binomial(n, pi);
    }
    generated quantities {
      real<lower=0,upper=1> pi_pred; 
        // sampled success probability for a future cluster
      int<lower=0,upper=n> YY_pred; 
        // sampled successes' count for a future cluster
      pi_pred = beta_proportion_rng(my, ka);
      YY_pred = binomial_rng(n, pi_pred);
    }
  "
        n <- sum(dat[1,])
        H <- nrow(dat)
        dat1 <- dat[, 1]
        ## fit hierarchical Beta-Binomial model using stan/rstan:
        fit <- rstan::stan(model_code = code_char, iter = 2500, chains = 4, 
                           data = list(H = H, n = n, YY = dat1), 
                           control = list(max_treedepth = 20L), save_warmup = FALSE, 
                           seed = seed, cores = 4)
        ## extract MCMC samples for relevant parameters:
        E <- rstan::extract(fit)
        ## samples for one future observation:
        yy <- as.integer(E$YY_pred)
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

# res1 <- pi_bayes_hier(predint::mortality_HCD)