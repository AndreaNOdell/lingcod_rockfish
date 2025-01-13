# load in data and functions ---------------------
load("cleaned_data/lingcod_parms.Rdata")
load("cleaned_data/rockfish_parms.Rdata")
source("scripts/model.R")
library(parallel)


# 1. Effect of prey-selectivity on yelloweye rockfish equilibrium -----------------

SBt = 0.4 # Specified SBt/SB0eq value of interest. In most cases, 0.4
nsims = 150 # number of simulations
cv = 0.5 # yelloweye estimated 0.48 and lingcod estimated 0.55 stock assessment
autocorr_R = 0.23
mpa.year = 0
rec.year = 25
handl = 0.3
bycatch = 0.05
tf = 5+mpa.year+350
hist.f = 0.4
hist.by = 0.13

no_prey = get_pop_ts(rockfish, lingcod, nsim = nsims, corr = 0, autocorr_L = 0.23, autocorr_R = autocorr_R, 
                     cv = cv, tf = tf, rec.yr = rec.year, mpa.yr = mpa.year, 
                     hist.f = hist.f, hist.by = hist.by, f = 0, b = bycatch, 
                     quant95 = 0.29, handling = handl, min.selectivity = TRUE, min.age.consumed = 4, 
                     rockfish.prop = 0.000001, yelloweye.prop = 0.0000001)
#save(no_prey, file = "results/prey_selectivity/no_prey.RData")

generalist = get_pop_ts(rockfish, lingcod, nsim = nsims, corr = 0, autocorr_L = 0.23, autocorr_R = autocorr_R, 
                      cv = cv, tf = tf, rec.yr = rec.year, mpa.yr = mpa.year, 
                      hist.f = hist.f, hist.by = hist.by, f = 0, b = bycatch, 
                      quant95 = 0.29, handling = handl, min.selectivity = TRUE, min.age.consumed = 4, 
                      rockfish.prop = 0.05, yelloweye.prop = 0.02)
#save(generalist, file = "results/prey_selectivity/generalist.RData")

intermediate = get_pop_ts(rockfish, lingcod, nsim = nsims, corr = 0, autocorr_L = 0.23, autocorr_R = autocorr_R, 
                       cv = cv, tf = tf, rec.yr = rec.year, mpa.yr = mpa.year, 
                       hist.f = hist.f, hist.by = hist.by, f = 0, b = bycatch, 
                       quant95 = 0.29, handling = handl, min.selectivity = TRUE, min.age.consumed = 4, 
                       rockfish.prop = 0.2, yelloweye.prop = 0.07)
#save(intermediate, file = "results/prey_selectivity/intermediate.RData")


specialist = get_pop_ts(rockfish, lingcod, nsim = nsims, corr = 0, autocorr_L = 0.23, autocorr_R = autocorr_R, 
                        cv = cv, tf = tf, rec.yr = rec.year, mpa.yr = mpa.year, 
                        hist.f = hist.f, hist.by = hist.by, f = 0, b = bycatch, 
                        quant95 = 0.29, handling = handl, min.selectivity = TRUE, min.age.consumed = 4, 
                        rockfish.prop = 0.1, yelloweye.prop = 0.5)
#save(specialist, file = "results/prey_selectivity/specialist.RData")



# 2. Continuous fishing scenarios -------------------------------------

    # Script sent to UC Davis FARM cluster (high performance computing)
    # See code/HPC_scripts/fishing_effect_vs_prey_select.R


# 3. Create deterministic time-series to make sure model is working as expected --------

SBt = 0.4 # Specified SBt/SB0eq value of interest. In most cases, 0.4
nsims = 2 # only 2 because deterministic (can't do 1 because of avg calculations in model)
cv = 0 # deterministic
autocorr_R = 0.23
mpa.year = 0
rec.year = 25
handl = 0.3
bycatch = 0.05
tf = 5+mpa.year+350
hist.f = 0.4
hist.by = 0.13
f = c(0, 0.084, 0.168)

ts_deterministic = mcmapply(f = f, get_pop_ts,
                            MoreArgs = list(rockfish, lingcod, nsim = nsims, corr = 0, 
                                            autocorr_L = 0.23, autocorr_R = autocorr_R,
                                            cv = cv, tf = tf, rec.yr = rec.year, 
                                            mpa.yr = mpa.year, hist.f = hist.f, 
                                            hist.by = hist.by, b = bycatch, quant95 = 0.29, 
                                            handling = handl, min.selectivity = TRUE, 
                                            min.age.consumed = 4,rockfish.prop = 0.05, 
                                            yelloweye.prop = 0.02), 
                            SIMPLIFY = FALSE, mc.cores = parallel::detectCores()-1)
#save(ts_deterministic, file = "results/deterministic/ts_deterministic.RData")


hist.by = 0.15

ts_deterministic_specialist = mcmapply(f = f, get_pop_ts,
                            MoreArgs = list(rockfish, lingcod, nsim = nsims, corr = 0, 
                                            autocorr_L = 0.23, autocorr_R = autocorr_R,
                                            cv = cv, tf = tf, rec.yr = rec.year, 
                                            mpa.yr = mpa.year, hist.f = hist.f, 
                                            hist.by = hist.by, b = bycatch, quant95 = 0.29, 
                                            handling = handl, min.selectivity = TRUE, 
                                            min.age.consumed = 4, rockfish.prop = 0.1, 
                                            yelloweye.prop = 0.5), 
                            SIMPLIFY = FALSE, mc.cores = parallel::detectCores()-1)
#save(ts_deterministic_specialist, file = "results/deterministic/ts_deterministic_specialist.RData")



hist.f = 0.4
hist.by = 0.168

ts_deterministic_hightest = mcmapply(f = f, get_pop_ts,
                                       MoreArgs = list(rockfish, lingcod, nsim = nsims, corr = 0, 
                                                       autocorr_L = 0.23, autocorr_R = autocorr_R,
                                                       cv = cv, tf = tf, rec.yr = rec.year, 
                                                       mpa.yr = mpa.year, hist.f = hist.f, 
                                                       hist.by = hist.by, b = bycatch, quant95 = 0.29, 
                                                       handling = handl, min.selectivity = TRUE, 
                                                       min.age.consumed = 4, rockfish.prop = 0.1, 
                                                       yelloweye.prop = 0.7), 
                                       SIMPLIFY = FALSE, mc.cores = parallel::detectCores()-1)



