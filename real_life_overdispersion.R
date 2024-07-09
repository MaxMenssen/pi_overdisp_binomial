#-------------------------------------------------------------------------------
#---------------- Table 5 of Tarone 1982 (binomial) ----------------------------
#-------------------------------------------------------------------------------

tumor <- c(rep(0, 7), rep(0, 4), rep(0, 2), 0, rep(1, 4), rep(1, 2),
           rep(1,2), 2, 2, 2, 2, rep(2, 6), 1,
           5, 2, 5, 2, 7, 7, rep(3, 2), 2, 9, 10, rep(4, 7), 10, rep(4, 3),
           5, 11, 12, rep(5, 2), 6, 5, 6, rep(6,3), 16, 15, 15, 9)

total <- c(rep(20, 7), rep(19, 4), rep(18,2), 17, rep(20, 4), rep(19, 2),
           rep(18, 2), 27, 25, 24, 23, rep(20, 6), 10,
           49, 19, 46, 17, 49, 47, rep(20, 2), 13, 48, 50, rep(20, 7), 48, rep(19, 3),
           22, 46, 49, rep(20,2), 23, 19, 22, rep(20,3), 52, 47, 46, 24)


stromal_polyp <- data.frame(tumor,
                            total)

fit <- glm(cbind(tumor, total-tumor) ~ 1, family=quasibinomial(), data=stromal_polyp)
summary(fit)$dispersion

#-------------------------------------------------------------------------------
#----------------- Tab 5 Carlus et al 2013 -------------------------------------
#-------------------------------------------------------------------------------

# Survival of males after 12 month
m12 <- data.frame(surv=c(67, 84, 74, 67, 65, 68, 68, 80),
                  total=c(70, 85, 75, 70, 70, 70, 70, 80))

fit_m12 <- glm(cbind(surv, total-surv) ~1, family=quasibinomial(), data=m12)

summary(fit_m12)$dispersion # 1.346919


#-------------------------------------------------------------------------------

# Survival of males after 24 month
m24 <- data.frame(surv=c(21, 27, 21, 25, 23, 25, 22, 19),
                  total=c(60, 60, 50, 60, 60, 60, 60, 60))


fit_m24 <- glm(cbind(surv, total-surv) ~1, family=quasibinomial(), data=m24)

summary(fit_m24)$dispersion # 0.4807638

#-------------------------------------------------------------------------------

# Survival of females after 12 month
f12 <- data.frame(surv=c(66, 81, 75, 68, 69, 68, 67, 77),
                  total=c(70, 85, 75, 70, 70, 70, 70, 80))

fit_f12 <- glm(cbind(surv, total-surv) ~1, family=quasibinomial(), data=f12)

summary(fit_f12)$dispersion # 0.8002679


#-------------------------------------------------------------------------------

# Survival of males after 24 month
f24 <- data.frame(surv=c(36, 39, 27, 37, 41, 27, 38, 29),
                  total=c(60, 60, 50, 60, 60, 60, 60, 60))


fit_f24 <- glm(cbind(surv, total-surv) ~1, family=quasibinomial(), data=f24)

summary(fit_f24)$dispersion # 1.680864

#-------------------------------------------------------------------------------
#----------------- Table 3 from Tarone 1982 (Poisson) --------------------------
#-------------------------------------------------------------------------------

colonies <- c(10, 10, 13, 14, 14, 15, 15, 16, 16, 16, 17, 17, 17, 17, 18, 
              19, 19, 19, 19, 20, 20, 20, 20, 20, 20, 21, 21, 21, 21, 21, 
              21, 22, 22, 23, 23, 23, 23, 24, 26, 26, 27, 27, 28, 28, 29,
              29, 29, 29, 29, 31, 32, 33, 34, 34, 35, 35, 37, 37, 38, 39, 
              39, 39, 40, 44, 46, 47)

fit_colonies <- glm(colonies ~ 1, family = quasipoisson())
summary(fit_colonies)$dispersion # 3.182295



