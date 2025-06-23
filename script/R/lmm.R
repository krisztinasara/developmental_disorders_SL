# lmm

# -- head -- #

setwd('~/Github/developmental_disorders_SL/')
library(tidyverse)
library(broom)
library(performance)

# bf formula
# exp((BIC_0 - BIC_1) / 2)

# -- read -- #

d = read_csv('data/df.csv')

# -- set up -- #

d2 = d |> 
  select(expr_vocab,sent_rep,ID,group,AGL_medRT_diff,AGL_offline,digit_span_forward,digit_span_backward,PS_vis_RT_med,PS_ac_RT_med,n_back_2_mean_score)

de = filter(d2, !is.na(expr_vocab)) |> 
  select(-sent_rep)
ds = filter(d2, !is.na(sent_rep)) |> 
  select(-expr_vocab)

# -- impute -- #

cols_to_impute = setdiff(names(d2), c('sent_rep','expr_vocab','group','ID')) # first two are never NA anyway but just in case. 

for (col in cols_to_impute) {
  med = median(de[[col]], na.rm = TRUE)
  de[[col]][is.na(de[[col]])] = med
}

for (col in cols_to_impute) {
  med = median(ds[[col]], na.rm = TRUE)
  ds[[col]][is.na(ds[[col]])] = med
}

# -- lm -- #

lm_e_0 = lm(expr_vocab ~ group + 
            AGL_medRT_diff + 
            AGL_offline + 
            digit_span_forward + 
            digit_span_backward + 
            PS_vis_RT_med + 
            PS_ac_RT_med,
            data = de
          )

lm_e_1 = lm(expr_vocab ~ 
              AGL_medRT_diff * group + 
              AGL_offline + 
              digit_span_forward + 
              digit_span_backward + 
              PS_vis_RT_med + 
              PS_ac_RT_med,
            data = de
)

lm_e_2 = lm(expr_vocab ~
              AGL_medRT_diff + 
              AGL_offline * group + 
              digit_span_forward + 
              digit_span_backward + 
              PS_vis_RT_med + 
              PS_ac_RT_med,
            data = de
)

lm_e_3 = lm(expr_vocab ~
              AGL_medRT_diff + 
              AGL_offline + 
              digit_span_forward * group + 
              digit_span_backward + 
              PS_vis_RT_med + 
              PS_ac_RT_med,
            data = de
)

lm_e_4 = lm(expr_vocab ~
              AGL_medRT_diff + 
              AGL_offline + 
              digit_span_forward + 
              digit_span_backward * group + 
              PS_vis_RT_med + 
              PS_ac_RT_med,
            data = de
)

lm_e_5 = lm(expr_vocab ~ group + 
              AGL_medRT_diff + 
              AGL_offline + 
              digit_span_forward + 
              digit_span_backward + 
              PS_vis_RT_med * group + 
              PS_ac_RT_med,
            data = de
)

lm_e_6 = lm(expr_vocab ~ group + 
              AGL_medRT_diff + 
              AGL_offline + 
              digit_span_forward + 
              digit_span_backward + 
              PS_vis_RT_med +
              PS_ac_RT_med * group,
            data = de
)


lm_s_0 = lm(sent_rep ~ group + 
              AGL_medRT_diff + 
              AGL_offline + 
              digit_span_forward + 
              digit_span_backward + 
              PS_vis_RT_med + 
              PS_ac_RT_med,
            data = ds
)

lm_s_1 = lm(sent_rep ~ 
              AGL_medRT_diff * group + 
              AGL_offline + 
              digit_span_forward + 
              digit_span_backward + 
              PS_vis_RT_med + 
              PS_ac_RT_med,
            data = ds
)

lm_s_2 = lm(sent_rep ~
              AGL_medRT_diff + 
              AGL_offline * group + 
              digit_span_forward + 
              digit_span_backward + 
              PS_vis_RT_med + 
              PS_ac_RT_med,
            data = ds
)

lm_s_3 = lm(sent_rep ~
              AGL_medRT_diff + 
              AGL_offline + 
              digit_span_forward * group + 
              digit_span_backward + 
              PS_vis_RT_med + 
              PS_ac_RT_med,
            data = ds
)

lm_s_4 = lm(sent_rep ~
              AGL_medRT_diff + 
              AGL_offline + 
              digit_span_forward + 
              digit_span_backward * group + 
              PS_vis_RT_med + 
              PS_ac_RT_med,
            data = ds
)

lm_s_5 = lm(sent_rep ~ group + 
              AGL_medRT_diff + 
              AGL_offline + 
              digit_span_forward + 
              digit_span_backward + 
              PS_vis_RT_med * group + 
              PS_ac_RT_med,
            data = ds
)

lm_s_6 = lm(sent_rep ~ group + 
              AGL_medRT_diff + 
              AGL_offline + 
              digit_span_forward + 
              digit_span_backward + 
              PS_vis_RT_med +
              PS_ac_RT_med * group,
            data = ds
)

# -- model checks -- #

# e
check_collinearity(lm_e_1)
check_collinearity(lm_e_2)
check_collinearity(lm_e_3)
check_collinearity(lm_e_4)
check_collinearity(lm_e_5)
check_collinearity(lm_e_6)
# s
check_collinearity(lm_s_1)
check_collinearity(lm_s_2)
check_collinearity(lm_s_3)
check_collinearity(lm_s_4)
check_collinearity(lm_s_5)
check_collinearity(lm_s_6)




