# bootstrapping spearman cors
# -- head -- #

setwd('~/Github/developmental_disorders_SL/')

set.seed(123)

library(tidyverse)
library(broom)
library(performance)
library(boot)
library(ggridges)
library(ggthemes)

# -- fun -- #

bootSpearman = function(data, indices) {
  d = data[indices, ]  # allows boot to select sample
  cor(d$predictor_value, d$outcome_value, method = "spearman")
}

bootDraws = function(my_data){
  boot_results = boot(data = my_data, statistic = bootSpearman, R = 10000)
  boot_draws = as.double(boot_results$t)
  return(boot_draws)
}

# -- read -- #

d = read_csv('data/df.csv')

# -- mungle -- #

dl = d |> 
  mutate(group = fct_relevel(group, 'TD')) |> 
  select(ID,group,expr_vocab,sent_rep,AGL_offline,AGL_medRT_diff,PS_vis_RT_med,PS_ac_RT_med,digit_span_forward,digit_span_backward,n_back_2_mean_score) |>
  pivot_longer(-c(ID,group,expr_vocab,sent_rep), names_to = 'predictor_name', values_to = 'predictor_value') |> 
  pivot_longer(-c(ID,group,predictor_name,predictor_value), names_to = 'outcome_name', values_to = 'outcome_value') |>   
  filter(
    !is.na(predictor_value),
    !is.na(outcome_value)
         ) 

# -- model -- #

spearman_draws = dl |> 
  select(ID,predictor_name,group,outcome_name,outcome_value,predictor_value) |> 
  nest(.by = c(predictor_name,group,outcome_name)) |> 
  mutate(
    draws = map(data, bootDraws)
  ) |> 
  select(-data) 

spearman_draws = spearman_draws |> 
  unnest(draws)

# -- write -- #

spearman_draws  |> 
  write_tsv('data/boot_estimates.gz')