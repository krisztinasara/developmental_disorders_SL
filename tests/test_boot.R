setwd('~/Github/developmental_disorders_SL/')

library(testthat)
library(tidyverse)
library(boot)

bootSpearman <- function(data, indices) {
  d <- data[indices, ]
  cor(d$predictor_value, d$outcome_value, method = "spearman")
}

bootDraws <- function(my_data){
  boot_results <- boot(data = my_data, statistic = bootSpearman, R = 100)
  # 100 for speed in tests; script uses 10000
  as.double(boot_results$t)
}

test_that("Input CSV is loaded", {
  d <- read_csv('data/df.csv')
  expect_true(is.data.frame(d))
  expect_gt(nrow(d), 0)
})

test_that("Data wrangling produces long-format data", {
  d <- read_csv('data/df.csv')
  dl <- d |>
    mutate(group = fct_relevel(group, 'TD')) |>
    select(ID, group, expr_vocab, sent_rep, AGL_offline, AGL_medRT_diff, PS_vis_RT_med, PS_ac_RT_med,
           digit_span_forward, digit_span_backward, n_back_2_mean_score) |>
    pivot_longer(-c(ID, group, expr_vocab, sent_rep), names_to = 'predictor_name', values_to = 'predictor_value') |>
    pivot_longer(-c(ID, group, predictor_name, predictor_value), names_to = 'outcome_name', values_to = 'outcome_value') |>
    filter(!is.na(predictor_value), !is.na(outcome_value))
  expect_true(all(c("ID", "group", "predictor_name", "predictor_value", "outcome_name", "outcome_value") %in% names(dl)))
  expect_gt(nrow(dl), 0)
})

test_that("Bootstrapping on a group returns draws", {
  d <- read_csv('data/df.csv')
  dl <- d |>
    mutate(group = fct_relevel(group, 'TD')) |>
    select(ID, group, expr_vocab, sent_rep, AGL_offline, AGL_medRT_diff, PS_vis_RT_med, PS_ac_RT_med,
           digit_span_forward, digit_span_backward, n_back_2_mean_score) |>
    pivot_longer(-c(ID, group, expr_vocab, sent_rep), names_to = 'predictor_name', values_to = 'predictor_value') |>
    pivot_longer(-c(ID, group, predictor_name, predictor_value), names_to = 'outcome_name', values_to = 'outcome_value') |>
    filter(!is.na(predictor_value), !is.na(outcome_value))
  grouped <- dl |>
    select(ID, predictor_name, group, outcome_name, outcome_value, predictor_value) |>
    nest(.by = c(predictor_name, group, outcome_name))
  # Just test first group for speed
  draws <- bootDraws(grouped$data[[1]])
  expect_type(draws, "double")
  expect_gt(length(draws), 10)
})

test_that("Final output format is correct", {
  d <- read_csv('data/df.csv')
  dl <- d |>
    mutate(group = fct_relevel(group, 'TD')) |>
    select(ID, group, expr_vocab, sent_rep, AGL_offline, AGL_medRT_diff, PS_vis_RT_med, PS_ac_RT_med,
           digit_span_forward, digit_span_backward, n_back_2_mean_score) |>
    pivot_longer(-c(ID, group, expr_vocab, sent_rep), names_to = 'predictor_name', values_to = 'predictor_value') |>
    pivot_longer(-c(ID, group, predictor_name, predictor_value), names_to = 'outcome_name', values_to = 'outcome_value') |>
    filter(!is.na(predictor_value), !is.na(outcome_value))
  spearman_draws <- dl |>
    select(ID, predictor_name, group, outcome_name, outcome_value, predictor_value) |>
    nest(.by = c(predictor_name, group, outcome_name)) |>
    mutate(draws = map(data, bootDraws)) |>
    select(-data) |>
    unnest(draws)
  expect_true(all(c("predictor_name", "group", "outcome_name", "draws") %in% names(spearman_draws)))
  expect_gt(nrow(spearman_draws), 0)
})