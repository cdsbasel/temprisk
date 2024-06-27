# DESCRIPTION -------------------------------------------------------------

# In this script we fit the MASC model (Anusic & Schimmack, 2016 JPSP) to the
# data analyzed by Anusic & Schimmack (2016)
#
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

data_w_path <- c("processing/output/temp_stability/") # where is the  data stored
output_data_path <- c("analysis/output/temp_stability/") # where to store the output data


# DATA PREP. --------------------------------------------------------------------

#________________FILE OF INTEREST___________________#

data_file <- "anusic_schimmack_2015.csv" # name of retest data 


col_specs <- cols(
  author = col_character(),
  sample = col_double(),
  n = col_double(),
  prop_female = col_double(),
  age = col_double(),
  items = col_double(),
  time_diff = col_double(),
  retest = col_double(),
  construct = col_character()
)


data_as <-read_csv(paste0(data_w_path,data_file), col_types = col_specs)

# complete author list
data_as <- data_as %>% 
  fill(author, .direction = "down") 

data_as <- data_as[complete.cases(data_as),] # 4 rows removed because of missing sample size
data_as <- data_as %>%  filter(age >= 10 & age <= 90)  # select only data from 10-90

# variable coding/transformation
data_as <- data_as %>%  
  mutate(se = sqrt((1 - retest^2)^2)/(n - 1),
         time_diff_bin =  time_binning(x = time_diff, year_bin =  3/12),
         time_diff_dec = time_diff_bin/10,
         female_prop_c = prop_female - .5,
         age_dec_c = (age - 40)/10, # setting another age as the mean
         age_dec_c2 = age_dec_c^2,
         item_num_c = case_when(items > 1 ~.5,
                                TRUE ~-.5),
         items_ln_c = as.vector(scale(log(items), scale = FALSE, center = TRUE)),
         age_group = age_binning(age, age_bin = 10),
         # using sum contrast coding
         construct = factor(construct),
         construct = relevel(construct, ref="pers"),
         construct = sum_coding(construct, lvls = levels(construct))) 


# explore data
data_as %>%
  ggplot(aes(x = time_diff_dec, y = retest))+
  geom_point(size = .25) +
  facet_grid(age_group~construct)


data_as %>%
  ggplot(aes(x = age, y = retest))+
  facet_wrap(.~construct) +
  geom_point(size = .25)


length(unique(data_as$author))


# MODEL FITTING -------------------------------------------------------------

family <- brmsfamily(
  family = "student", 
  link = "identity"
)


formula <- bf(
  retest|resp_se(se, sigma = TRUE) ~ rel * (change * ((stabch^time_diff_dec) - 1) + 1),
  nlf(rel ~ inv_logit(logitrel)),
  nlf(change ~ inv_logit(logitchange)),
  nlf(stabch ~ inv_logit(logitstabch)),
  logitrel ~ 1 + age_dec_c*construct + age_dec_c2*construct +  female_prop_c + item_num_c,
  logitchange ~ 1 + age_dec_c*construct + age_dec_c2*construct + female_prop_c, 
  logitstabch ~ 1  + age_dec_c*construct + age_dec_c2*construct + female_prop_c,
  nl = TRUE
)


# # weakly informative priors 
priors <-
  prior(normal(0,1), nlpar="logitrel", class = "b") +
  prior(normal(0,1), nlpar="logitchange", class = "b") +
  prior(normal(0,1), nlpar="logitstabch", class = "b") +
  prior(cauchy(0,1), class = "sigma") 



# fit model
fit_masc <- brm(
  formula = formula,
  prior = priors,
  family = family,
  data = data_as,
  cores = 2, 
  chains = 2,
  # threads = threading(2), 
  iter = 7000,
  warmup = 2000, 
  backend = "cmdstanr",
  save_pars = save_pars(all = TRUE),
  stan_model_args = list(stanc_options = list("O1")), 
  sample_prior = TRUE,
  control = list(max_treedepth = 10, adapt_delta = 0.95), 
  init = "0",
  seed = 34229
)


#________________SAVING OUTPUT___________________#

saveRDS(fit_masc, paste0(output_data_path,"masc_as.rds"))




# MODEL EVAL: MCMC DIAGNOSTICS --------------------------------------------------------

# model summary 
fit_masc


# trace plots & param. estimates
plot(fit_masc, N = 5, ask = TRUE)


inv_logit <- function(x) {plogis(x)}
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
         group = "construct",
         ndraws = 25)


pp_check(fit_masc,
         type ="stat_grouped",
         stat = "mean",
         group = "construct",
         ndraws = 1000)

pp_check(fit_masc,
         type ="stat_grouped",
         stat = "sd",
         group = "construct",
         ndraws = 1000)

pp_check(fit_masc,
         type ="stat_grouped",
         stat = "median",
         group = "construct",
         ndraws = 1000)


pp_check(fit_masc,
         type ="stat_grouped",
         stat = "mad",
         group = "construct",
         ndraws = 1000)


pp_check(fit_masc,
         type ="scatter_avg_grouped",
         group = "construct")




# MODEL EVAL: LOO --------------------------------------------------------

# loo & pareto K
model_loo <- loo(fit_masc, save_psis = TRUE, cores = 2)
plot(model_loo, diagnostic = "k")
plot(model_loo, diagnostic = "n_eff")

# loo pit
w <- weights(model_loo$psis_object)
ppc_loo_pit_overlay(y = fit_masc$data$retest, 
                    yrep = posterior_predict(fit_masc), 
                    lw = w)
ppc_loo_pit_qq(y = data_w$wcor, 
               yrep = posterior_predict(fit_masc), 
               lw = w)



# MODEL EVAL: COND. EFF. --------------------------------------------------------

# conditional_effects: domain
conditions <- data.frame(construct = unique(data_as$construct))
rownames(conditions) <- unique(data_as$construct)

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




