# lmm w/ varimp

# loo used to compare. elpd diff needs to be over 2 * se_diff for me to care.

# -- head -- #

setwd('~/Github/developmental_disorders_SL/')
library(tidyverse)
library(naniar)
library(rstanarm)

# -- read -- #

d = read_csv('data/df.csv')
varimp1 = read_tsv('varimp1.tsv')
varimp2 = read_tsv('varimp2.tsv')

# -- mungle -- #

# response1 = "sent_rep"
# response2 = "expr_vocab"

d1 = d |> 
  mutate(group = fct_relevel(group, 'TD')) |> 
  select(sent_rep,n_back_2_mean_score,digit_span_forward,group,digit_span_backward,AGL_offline) |> 
  na.omit()

d2 = d |> 
  mutate(group = fct_relevel(group, 'TD')) |> 
  select(expr_vocab,group,AGL_offline,digit_span_backward,PS_vis_RT_med,digit_span_forward,n_back_2_mean_score) |> 
  na.omit()

# -- models -- #

## m1: var imp

fit1 = stan_glm(sent_rep ~ n_back_2_mean_score + digit_span_forward + group + digit_span_backward + AGL_offline, data = d1, chains = 4, cores = 4, iter = 2000)
fit1a = stan_glm(sent_rep ~ digit_span_forward + group + digit_span_backward + AGL_offline, data = d1, chains = 4, cores = 4, iter = 2000)
fit1b = stan_glm(sent_rep ~ n_back_2_mean_score + group + digit_span_backward + AGL_offline, data = d1, chains = 4, cores = 4, iter = 2000)
fit1c = stan_glm(sent_rep ~ n_back_2_mean_score + digit_span_forward + digit_span_backward + AGL_offline, data = d1, chains = 4, cores = 4, iter = 2000)
fit1d = stan_glm(sent_rep ~ n_back_2_mean_score + digit_span_forward + group + digit_span_backward, data = d1, chains = 4, cores = 4, iter = 2000)

loo1 = loo(fit1)
loo1a = loo(fit1a)
loo1b = loo(fit1b)
loo1c = loo(fit1c)
loo1d = loo(fit1d)
loo_compare(loo1,loo1a)
loo_compare(loo1,loo1b)
loo_compare(loo1,loo1c)
loo_compare(loo1,loo1d) # worse

## m1: interactions

# agl offline

fit1e = stan_glm(sent_rep ~ group * AGL_offline, data = d, chains = 4, cores = 4, iter = 2000) # d, not d1!
fit1f = stan_glm(sent_rep ~ group + AGL_offline, data = d, chains = 4, cores = 4, iter = 2000)

loo_compare(loo(fit1e),loo(fit1f))

# nothing

## m2: varimp

fit2 = stan_glm(expr_vocab ~ group + AGL_offline + digit_span_backward + PS_vis_RT_med + digit_span_forward + n_back_2_mean_score, data = d2, chains = 4, cores = 4, iter = 2000)
fit2a = stan_glm(expr_vocab ~ AGL_offline + digit_span_backward + PS_vis_RT_med + digit_span_forward + n_back_2_mean_score, data = d2, chains = 4, cores = 4, iter = 2000)
fit2b = stan_glm(expr_vocab ~ group + digit_span_backward + PS_vis_RT_med + digit_span_forward + n_back_2_mean_score, data = d2, chains = 4, cores = 4, iter = 2000)
fit2c = stan_glm(expr_vocab ~ group + AGL_offline + PS_vis_RT_med + digit_span_forward + n_back_2_mean_score, data = d2, chains = 4, cores = 4, iter = 2000)
fit2d = stan_glm(expr_vocab ~ group + AGL_offline + digit_span_backward + digit_span_forward + n_back_2_mean_score, data = d2, chains = 4, cores = 4, iter = 2000)
fit2e = stan_glm(expr_vocab ~ group + AGL_offline + digit_span_backward + PS_vis_RT_med + n_back_2_mean_score, data = d2, chains = 4, cores = 4, iter = 2000)
fit2f = stan_glm(expr_vocab ~ group + AGL_offline + digit_span_backward + PS_vis_RT_med + digit_span_forward, data = d2, chains = 4, cores = 4, iter = 2000)

loo2 = loo(fit2, k_threshold = 0.7)
loo2a = loo(fit2a, k_threshold = 0.7)
loo2b = loo(fit2b, k_threshold = 0.7)
loo2c = loo(fit2c, k_threshold = 0.7)
loo2d = loo(fit2d, k_threshold = 0.7)
loo2e = loo(fit2e, k_threshold = 0.7)
loo2f = loo(fit2f, k_threshold = 0.7)
loo_compare(loo2,loo2a)
loo_compare(loo2,loo2b)
loo_compare(loo2,loo2c) # !
loo_compare(loo2,loo2d)
loo_compare(loo2,loo2e)
loo_compare(loo2,loo2f) # !

## m2: interactions

# digit_span_backward
# n_back_2_mean_score
fit2g = stan_glm(expr_vocab ~ group * digit_span_backward, data = d, chains = 4, cores = 4, iter = 2000)
fit2h = stan_glm(expr_vocab ~ group + digit_span_backward, data = d, chains = 4, cores = 4, iter = 2000)
fit2i = stan_glm(expr_vocab ~ group * n_back_2_mean_score, data = d, chains = 4, cores = 4, iter = 2000)
fit2j = stan_glm(expr_vocab ~ group + n_back_2_mean_score, data = d, chains = 4, cores = 4, iter = 2000)

loo_compare(loo(fit2g, k_threshold = 0.7),loo(fit2h, k_threshold = 0.7))
loo_compare(loo(fit2i, k_threshold = 0.7),loo(fit2j, k_threshold = 0.7))

# nothing