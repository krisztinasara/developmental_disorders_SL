# lmm

# -- head -- #

setwd('~/Github/developmental_disorders_SL/')
library(tidyverse)
library(broom)
library(performance)

# -- read -- #

d = read_csv('data/df.csv')
# varimp1 = read_tsv('varimp1.tsv')
# varimp2 = read_tsv('varimp2.tsv')

# -- mungle -- #

# response1 = "sent_rep"
# response2 = "expr_vocab"

# predictors
#`AGL_medRT_diff` = "statistical learning:\nartificial grammar learning\nresponse time",
#    `AGL_offline` = "statistical learning:\nartificial grammar learning\noffline",
#    `PS_vis_RT_med` = "perceptual speed:\nvisual response time",
#    `PS_ac_RT_med` = "perceptual speed:\nauditory response time",
#    `digit_span_forward` = "working memory:\nforward digit span",
#    `digit_span_backward` = "working memory:\nbackward digit span",
#    `n_back_2_mean_score` = "working memory:\nn-back",

dl = d |> 
  mutate(group = fct_relevel(group, 'TD')) |> 
  select(ID,group,expr_vocab,sent_rep,AGL_offline,AGL_medRT_diff,PS_vis_RT_med,PS_ac_RT_med,digit_span_forward,digit_span_backward,n_back_2_mean_score) |>
  pivot_longer(-c(ID,group,expr_vocab,sent_rep), names_to = 'predictor_name', values_to = 'predictor_value') |> 
  pivot_longer(-c(ID,group,predictor_name,predictor_value), names_to = 'outcome_name', values_to = 'outcome_value') |>   
  filter(
    !is.na(predictor_value),
    !is.na(outcome_value)
         )

dn = dl  |> 
  nest(.by = c(outcome_name,predictor_name))

# -- models -- #

dc = dn |> 
  mutate(
    lm1 = map(data, ~ lm(outcome_value ~ predictor_value + group, data = .)),
    lm2 = map(data, ~ lm(outcome_value ~ predictor_value * group, data = .)),
    chisq = map2(lm1,lm2, ~ as_tibble(test_likelihoodratio(.x,.y))),
    tidy_lm1 = map(lm1, ~ tidy(., conf.int = T)),
    tidy_lm2 = map(lm2, ~ tidy(., conf.int = T))
  )

estimates = dc |> 
unnest(chisq) |> 
filter(df_diff == 3) |> 
mutate(
  adjusted_p = p * 14,
  best_lm = ifelse(adjusted_p < 0.05, tidy_lm2, tidy_lm1),
) |> 
unnest(best_lm) |> 
select(outcome_name, predictor_name, Chi2, p, adjusted_p, term, statistic, conf.high, conf.low) 

estimates |> 
 write_tsv('data/lmm_estimates.tsv')
 
