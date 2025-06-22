# -- setup -- #

setwd('~/Github/developmental_disorders_SL/')

library(tidyverse)
library(patchwork)
library(ggthemes)
library(sjPlot)
library(corrplot)
library(performance)
library(scales)
library(glue)
library(jsonlite)
library(broom)

# -- names -- #

my_labeller = as_labeller(
  c(
    "TD" = 'Typically Developing',
    "DLD" = 'Developmental\nLanguage Disorder',
    "ADHD" = 'Attention-Deficit/\nHyperactivity Disorder',
    "ASD" = 'Autism Spectrum\nDisorder'
  )
)

long_names = c(
    `ID` = "Id",
    # ez nem redundáns a dummy coded diagnózissal? (lehet, hogy nem gond)
    `age_years` = "age in years",
    `AGL_medRT_diff` = "statistical learning:\nartificial grammar learning\nresponse time",
    `AGL_offline` = "statistical learning:\nartificial grammar learning\noffline",
    `PS_vis_RT_med` = "perceptual speed:\nvisual response time",
    `PS_ac_RT_med` = "perceptual speed:\nauditory response time",
    `digit_span_forward` = "working memory:\nforward digit span",
    `digit_span_backward` = "working memory:\nbackward digit span",
    `n_back_2_mean_score` = "working memory:\nn-back",
      `expr_vocab` = "language:\nexpressive vocabulary",
    `sent_rep` = "language:\nsentence repetition",
    `group_TD` = 'typically developing',
    `group_DLD` = 'Developmental\nLanguage Disorder',
    `group_ADHD` = 'Attention-Deficit/\nHyperactivity Disorder',
    `group_ASD` = 'Autism Spectrum\nDisorder',
    `sent_rep_pred` = "predicted\nsentence repetition",
    `expr_vocab_pred` = "predicted\nexpressive vocabulary"
  )
  
# -- read -- #

d = read_csv('data/df.csv')

# -- model -- #

nms = names(long_names)[str_detect(names(long_names), '(group_|sent_rep_pred|expr_vocab_pred|age|IQ)', negate = T)]

# whoops
nms = c(nms,'group')

longd = d |> 
  select(all_of(nms)) |> 
  pivot_longer(-c(ID,group,expr_vocab,sent_rep), names_to = 'predictor_name', values_to = 'predictor_value') |> 
  pivot_longer(-c(ID,group,predictor_name,predictor_value), names_to = 'outcome_name', values_to = 'outcome_value') |>   
  filter(
    !is.na(predictor_value),
    !is.na(outcome_value)
         )

longd2 = longd |> 
  nest(.by = c(group,outcome_name,predictor_name)) |> 
  mutate(
    data = map(
      data, 
      ~ mutate(., 
               outcome_value_s = scales::rescale(outcome_value),
               predictor_value_s = scales::rescale(predictor_value)
               )
      )
    )

ests = longd2 |> 
  mutate(
    pearson = map(
      data, 
      ~ tidy(cor.test(.$outcome_value_s,.$predictor_value_s, method = 'pearson', conf.level = .99))
    )
  ) |> 
  unnest(pearson) |> 
  select(group,outcome_name,predictor_name,estimate,conf.low,conf.high) 

# -- write -- #

ests |> 
    write_tsv('data/pearson.tsv')