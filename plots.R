setwd('~/Github/developmental_disorders_SL/')
library(tidyverse)
library(ggthemes)
library(patchwork)
library(scales)
library(ggrain)

d = read_csv('data/df.csv')

dnice = d |> 
  rename(
  `Sentence Repetition` = sent_rep,
  `Expressive Vocabulary` = expr_vocab,
  `Artificial Grammar\nLearning Offline` = AGL_offline,
  `Median Reaction Time\nDifference in AGL` = AGL_medRT_diff,
  `Intelligence Quotient` = IQ,
  `Digit Span Forward` = digit_span_forward,
  `Digit Span Backward` = digit_span_backward,
  `Processing Speed\nVisual RT Median` = PS_vis_RT_med,
  `Processing Speed\nAcoustic RT Median` = PS_ac_RT_med,
  `N-Back 2 Mean Score` = n_back_2_mean_score
) |> 
  mutate(
    group2 = case_when(
      group == 'TD' ~ 'Typically Developing',
      group == 'DLD' ~ 'Developmental Language Disorder',
      group == 'ADHD' ~ 'Attention-Deficit/Hyperactivity Disorder',
      group == 'ASD' ~ 'Autism Spectrum Disorder',
    ) |> fct_relevel('Developmental Language Disorder','Attention-Deficit/Hyperactivity Disorder','Autism Spectrum Disorder','Typically Developing')
  )

dnice |> 
  select(ID,group2,contains(' ')) |> 
  pivot_longer(-c(ID,group2), names_to = 'measurement', values_to = 'value') |> 
  filter(!is.na(value)) |> 
  mutate(scaled = scale(value), .by = measurement) |> 
  ggplot(aes(group2,scaled,fill = group2)) +
    # geom_tufteboxplot() +
    geom_rain() +
    coord_flip() +
    theme_bw() +
    theme(
      axis.title.y = element_blank(),
      panel.grid.minor = element_blank(),
      panel.grid.major.y = element_blank()
          ) +
    guides(fill = F) +
    xlab('scaled value') +
    facet_wrap(~ measurement, nrow = 2) +
    scale_fill_grey()

