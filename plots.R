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
  `Digit Span Forward\n(jittered)` = digit_span_forward,
  `Digit Span Backward\n(jittered)` = digit_span_backward,
  `Processing Speed\nVisual RT Median` = PS_vis_RT_med,
  `Processing Speed\nAcoustic RT Median` = PS_ac_RT_med,
  `N-Back 2 Mean Score` = n_back_2_mean_score,
  `Artificial Grammar Learning\nsentence (jittered)` = AFC_sent,
  `Artificial Grammar Learning\nphrase (jittered)` = AFC_phr,
) |> 
  mutate(
    group2 = case_when(
      group == 'TD' ~ 'Typically Developing',
      group == 'DLD' ~ 'Developmental Language Disorder',
      group == 'ADHD' ~ 'Attention-Deficit/Hyperactivity Disorder',
      group == 'ASD' ~ 'Autism Spectrum Disorder',
    ) |> fct_relevel('Developmental Language Disorder','Attention-Deficit/Hyperactivity Disorder','Autism Spectrum Disorder','Typically Developing')
  )

p1 = dnice |> 
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

plotCol = function(colname){
  if (colname %in% c('Digit Span Forward\n(jittered)', 'Digit Span Backward\n(jittered)', 'Artificial Grammar Learning\nphrase (jittered)', 'Artificial Grammar Learning\nsentence (jittered)')){
    ggplot(dnice, aes(group2,!!sym(colname))) +
      geom_rain(likert = T) +
      coord_flip() +
      theme_bw() +
      xlab('') +
      ylab(colname)
  } else {
    ggplot(dnice, aes(group2,!!sym(colname))) +
      geom_rain(likert = F) +
      coord_flip() +
      theme_bw() +
      xlab('') +
      ylab(colname)
  }
}

colnames = names(dnice)[!names(dnice) %in% c('ID','age_years','group','group2','prod')]

plots = map(colnames, plotCol)
(plots[[1]] + plots[[2]] + theme(axis.text.y = element_blank()) + plots[[3]] + theme(axis.text.y = element_blank())) /
(plots[[4]] + plots[[5]] + theme(axis.text.y = element_blank()) + plots[[6]] + theme(axis.text.y = element_blank())) /
(plots[[7]] + plots[[8]] + theme(axis.text.y = element_blank()) + plots[[9]] + theme(axis.text.y = element_blank())) /
(plots[[10]] + plots[[11]] + theme(axis.text.y = element_blank()) + plots[[12]] + theme(axis.text.y = element_blank()))

ggsave('esofelho.pdf', width = 8, height = 12)
