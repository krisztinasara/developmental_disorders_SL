# rf on data

# -- head -- #

setwd('~/Github/developmental_disorders_SL/')
library(tidyverse)
library(ggbiplot)
library(ggthemes)
library(patchwork)

# -- read -- #

d = read_csv('data/df.csv')

# -- setup -- #

d2 = d |> 
  rename(
    `Sentence\nRepetition` = sent_rep,
    `Expressive\nVocabulary` = expr_vocab,
    `Artificial Grammar\nLearning Offline` = AGL_offline,
    `Median Reaction Time\nDifference in AGL` = AGL_medRT_diff,
    # `Intelligence Quotient` = IQ,
    `Digit Span Forward\n(jittered)` = digit_span_forward,
    `Digit Span Backward\n(jittered)` = digit_span_backward,
    `Processing Speed\nVisual RT Median` = PS_vis_RT_med,
    `Processing Speed\nAcoustic RT Median` = PS_ac_RT_med,
    `N-Back 2\nMean Score` = n_back_2_mean_score,
    `Artificial Grammar\nLearning\nsentence (jittered)` = AFC_sent,
    `Artificial Grammar\nLearning\nphrase (jittered)` = AFC_phr,
  ) |> 
  mutate(
    group2 = case_when(
      group == 'TD' ~ 'Typically Developing',
      group == 'DLD' ~ 'Developmental\nLanguage Disorder',
      group == 'ADHD' ~ 'Attention-Deficit/\nHyperactivity Disorder',
      group == 'ASD' ~ 'Autism Spectrum\nDisorder',
    ) |> fct_relevel('Developmental\nLanguage Disorder','Attention-Deficit/\nHyperactivity Disorder','Autism Spectrum\nDisorder','Typically Developing')
  ) |> 
  mutate(value = 1) |> 
  pivot_wider(names_from = group2, values_from = value, values_fill = 0) |>
  select(-`Typically Developing`,-ID,-age_years,-IQ,-group) |> 
  filter_all(all_vars(is.finite(.)))

# -- pca -- #

pca1 = princomp(d2, cor = T)
loadings(pca1)
p1 = ggscreeplot(pca1) +
  theme_few()
p2 = ggbiplot(pca1, obs.scale = 1, var.scale = 1, 
         groups = NULL, ellipse = FALSE, circle = TRUE, alpha = .1, varname.size = 1.5) +
  theme_few()

p1 + p2
ggsave('viz/pca.png', width = 8, height = 4, dpi = 900)
