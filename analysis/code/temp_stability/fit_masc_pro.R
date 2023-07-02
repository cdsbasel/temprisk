# DESCRIPTION -------------------------------------------------------------

# In this script we fit the MASC model (Anusic & Schimmack, 2016 JPSP). 
# We also specify the dataset used to fit the MASC model and create new variables where required.

# Author(s): Alexandra Bagaini(1), Rui Mata(1), and Paul-Christian  Buerkner(2)  
# (1)Centre for Cognitive and Decision Sciences, Faculty of Psychology, University of Basel.
# (2)Department of Statistics, TU Dortmund University

# PACKAGES ---------------------------------------------------------------

library(tidyverse)
library(brms)
library(cmdstanr)
library(loo)
library(bayesplot)
library(tidybayes)
library(posterior)

color_scheme_set("teal")
# FUNCTIONS ----------------------------------------------------------------

source("helper_functions.R")


# PATHS ---------------------------------------------------


data_w_path <- c("processing/output/temp_stability/") # where is the data stored
output_data_path <- c("analysis/output/temp_stability/") # where to store the output 


#________________FILES___________________#

data_file <- "complete_agg_retest_yb10.csv" # name of retest data 

dat <- read_csv(paste0(data_w_path,data_file))


# DATA PREP. --------------------------------------------------------------


#________________SCALING VARS___________________#
data_w <- dat %>% 
  filter( data_transform == "none" &
           age_group != "10-90" &
           gender_group != "all" & 
           rho_val == .5 &
           month_bin == 3 &
           age_bin == 10 & 
           min_n == 30 &
           cor_metric == "pearson" &
           measure_category == "pro") %>% 
  mutate(time_diff_dec = time_diff_bin/10,
         female_prop_c = case_when(gender_group == "male" ~ -0.5,
                                   gender_group == "female" ~ 0.5),
         age_dec_c = (mean_age - 40)/10,
         age_dec_c2 = age_dec_c^2,
         domain_name = factor(domain_name),
         domain_name = relevel(domain_name, ref = "soc"),
         # sum contrast coding 
         domain_name = sum_coding(domain_name, lvls = levels(domain_name)))



# explore data
data_w %>%
  ggplot(aes(x = time_diff_dec, y = wcor))+
  geom_point(size = .25) +
  facet_grid(age_group~domain_name)

data_w %>%
  ggplot(aes(x = time_diff_dec, y = wcor))+
  geom_point(size = .25) +
  facet_wrap(panel~.) + theme_minimal()

length(unique(data_w$sample))
length(unique(data_w$panel))
plot_obs_per_grp(data_w)

# MODEL FITTING -------------------------------------------------------------


family <- brmsfamily(
  family = "student", 
  link = "identity"
)


formula <- bf(
  wcor|resp_se(sei, sigma = TRUE) ~ rel * (change * ((stabch^time_diff_dec) - 1) + 1),
  nlf(rel ~ inv_logit(logitrel)),
  nlf(change ~ inv_logit(logitchange)),
  nlf(stabch ~ inv_logit(logitstabch)),
  logitrel ~ 1 + age_dec_c*domain_name + age_dec_c2*domain_name +  female_prop_c + (1 + age_dec_c + age_dec_c2 + female_prop_c | sample),
  logitchange ~ 1 + age_dec_c*domain_name + age_dec_c2*domain_name + female_prop_c, 
  logitstabch ~ 1  + age_dec_c*domain_name + age_dec_c2*domain_name + female_prop_c,
  nl = TRUE
)


# # weakly informative priors 
priors <-
  prior(normal(0,1), nlpar="logitrel", class = "b") +
  prior(normal(0,1), nlpar="logitchange", class = "b") +
  prior(normal(0,1), nlpar="logitstabch", class = "b") +
  prior(cauchy(0,1), nlpar="logitrel", class = "sd")  +
  prior(cauchy(0,1), class = "sigma")  +
  prior(lkj(1), group="sample", class = "L") 


# fit model
fit_masc <- brm(
  formula = formula,
  prior = priors,
  family = family,
  data = data_w,
  cores = 2, 
  chains = 2,
  threads = threading(2), 
  iter = 7000,
  warmup = 2000, 
  backend = "cmdstanr",
  save_pars = save_pars(all = TRUE),
  stan_model_args = list(stanc_options = list("O1")), 
  sample_prior = TRUE,
  control = list(max_treedepth = 10, adapt_delta = 0.95), 
  init = "0",
  seed = 1299
)

#________________SAVING OUTPUT___________________#

saveRDS(fit_masc, paste0(output_data_path,"masc_pro.rds"))


# MODEL EVAL: MCMC DIAGNOSTICS --------------------------------------------------------

# model summary 
fit_masc


# trace plots & param. estimates
plot(fit_masc, N = 5, ask = TRUE)



# MODEL EVAL: PP CHECKS --------------------------------------------------------


# simulations vs. obs: Overall
pp_check(fit_masc,
         type ="dens_overlay",
         ndraws = 100)


pp_check(fit_masc,
         type ="stat",
         stat = "mean",
         ndraws = 1000,
         binwidth = .001)

pp_check(fit_masc,
         type ="stat",
         stat = "sd",
         ndraws = 500,
         binwidth = .001)

pp_check(fit_masc,
         type ="stat",
         stat = "median",
         ndraws = 500,
         binwidth = .001)

pp_check(fit_masc,
         type ="stat",
         stat = "mad",
         ndraws = 500,
         binwidth = .001)

pp_check(fit_masc,
         type ="stat_2d")


pp_check(fit_masc,
         type ="scatter_avg")



# simulations vs. obs: By Domain
pp_check(fit_masc,
         type ="dens_overlay_grouped",
         group = "domain_name",
         ndraws = 25)


pp_check(fit_masc,
         type ="stat_grouped",
         stat = "mean",
         group = "domain_name",
         ndraws = 1000)

pp_check(fit_masc,
         type ="stat_grouped",
         stat = "sd",
         group = "domain_name",
         ndraws = 1000)

pp_check(fit_masc,
         type ="stat_grouped",
         stat = "median",
         group = "domain_name",
         ndraws = 1000)


pp_check(fit_masc,
         type ="stat_grouped",
         stat = "mad",
         group = "domain_name",
         ndraws = 1000)


pp_check(fit_masc,
         type ="scatter_avg_grouped",
         group = "domain_name")


# simulations vs. obs: By Age Group
ppc_stat_grouped(y = data_w$wcor,
                 yrep = posterior_predict(fit_masc, ndraws = 1000),
                 group = data_w$age_group,
                 stat = "mean") + theme_minimal()

ppc_stat_grouped(y = data_w$wcor,
                 yrep = posterior_predict(fit_masc, ndraws = 1000),
                 group = data_w$age_group,
                 stat = "median")

ppc_stat_grouped(y = data_w$wcor,
                 yrep = posterior_predict(fit_masc, ndraws = 1000),
                 group = data_w$age_group,
                 stat = "sd")

ppc_stat_grouped(y = data_w$wcor,
                 yrep = posterior_predict(fit_masc, ndraws = 1000),
                 group = data_w$age_group,
                 stat = "mad")

ppc_stat_grouped(y = data_w$wcor,
                 yrep = posterior_predict(fit_masc, ndraws = 1000),
                 group = data_w$age_group,
                 stat = "skew")


# simulations vs. obs: By Age Group & Domain
ppc_stat_grouped(y = data_w$wcor,
                 yrep = posterior_predict(fit_masc, ndraws = 1000),
                 group = paste0(data_w$age_group,data_w$domain_name),
                 stat = "mean") + theme_minimal()

ppc_stat_grouped(y = data_w$wcor,
                 yrep = posterior_predict(fit_masc, ndraws = 1000),
                 group = paste0(data_w$age_group,data_w$domain_name),
                 stat = "median") + theme_minimal()



# simulations vs. obs: By Panel
pp_check(fit_masc,
         type ="stat_grouped",
         group = "panel",
         ndraws = 500)





# MODEL EVAL: LOO --------------------------------------------------------

# loo & pareto K
model_loo <- loo(fit_masc, save_psis = TRUE, cores = 2)
plot(model_loo, diagnostic = "k")
plot(model_loo, diagnostic = "n_eff")

# loo pit
w <- weights(model_loo$psis_object)
ppc_loo_pit_overlay(y = fit_masc$data$wcor, 
                    yrep = posterior_predict(fit_masc), 
                    lw = w)
ppc_loo_pit_qq(y = fit_masc$data$wcor, 
               yrep = posterior_predict(fit_masc), 
               lw = w)



# MODEL EVAL: COND. EFF. --------------------------------------------------------

# conditional_effects: domain
conditions <- data.frame(domain_name = unique(data_w$domain_name))
rownames(conditions) <- unique(data_w$domain_name)

plot(
  conditional_effects(
    fit_masc, 
    conditions = conditions,
    re_formula = NA,
    method = "fitted",
    spaghetti = TRUE,
    ndraws = 200
  ),
  ncol = 4,
  points = TRUE
)


# conditional_effects: panel
conditions <- data.frame(panel = unique(data_w$panel))
rownames(conditions) <- unique(data_w$panel)

plot(
  conditional_effects(
    fit_masc,
    conditions = conditions,
    re_formula = NULL,
    method = "fitted",
    spaghetti = TRUE,
    ndraws = 200
  ),
  ncol = 4,
  points = TRUE
)



