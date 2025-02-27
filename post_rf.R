# rf on data

# -- head -- #

setwd('~/Github/developmental_disorders_SL/')
library(tidyverse)
library(performance)
library(broom)
library(sjPlot)

# -- read -- #

d = read_csv('data/df.csv')
varimp1 = read_tsv('varimp1.tsv')
varimp2 = read_tsv('varimp2.tsv')

# -- varimp plots -- #

range(varimp1$percentage)
range(varimp2$percentage)

p1 = varimp1 |> 
  mutate(
    name = case_when(
      variable == "AGL_offline" ~ "Artificial Grammar Learning Offline",
      variable == "AGL_medRT_diff" ~ "Median Reaction Time Difference in AGL",
      variable == "IQ" ~ "Intelligence Quotient",
      variable == "digit_span_forward" ~ "Digit Span Forward (jittered)",
      variable == "digit_span_backward" ~ "Digit Span Backward (jittered)",
      variable == "PS_vis_RT_med" ~ "Processing Speed Visual RT Median",
      variable == "PS_ac_RT_med" ~ "Processing Speed Acoustic RT Median",
      variable == "n_back_2_mean_score" ~ "N-Back 2 Mean Score",
      variable == "AFC_sent" ~ "Artificial Grammar Learning sentence (jittered)",
      variable == "AFC_phr" ~ "Artificial Grammar Learning phrase (jittered)",
      variable == "DLD" ~ "Developmental Language Disorder",
      variable == "ADHD" ~ "Attention-Deficit/ Hyperactivity Disorder",
      variable == "ASD" ~ "Autism Spectrum Disorder"
    ) |> 
      fct_reorder(percentage)
  ) |> 
  select(name,percentage) |> 
  ggplot(aes(percentage,name)) +
  geom_col() +
  theme_few() +
  xlab('variable importance (scaled)') +
  xlim(0,.171) +
  theme(axis.title = element_blank(),axis.text.x = element_blank(), axis.ticks.x = element_blank()) +
  ggtitle('sentence repetition')

p2 = varimp2 |> 
  mutate(
    name = case_when(
      variable == "AGL_offline" ~ "Artificial Grammar Learning Offline",
      variable == "AGL_medRT_diff" ~ "Median Reaction Time Difference in AGL",
      variable == "IQ" ~ "Intelligence Quotient",
      variable == "digit_span_forward" ~ "Digit Span Forward (jittered)",
      variable == "digit_span_backward" ~ "Digit Span Backward (jittered)",
      variable == "PS_vis_RT_med" ~ "Processing Speed Visual RT Median",
      variable == "PS_ac_RT_med" ~ "Processing Speed Acoustic RT Median",
      variable == "n_back_2_mean_score" ~ "N-Back 2 Mean Score",
      variable == "AFC_sent" ~ "Artificial Grammar Learning sentence (jittered)",
      variable == "AFC_phr" ~ "Artificial Grammar Learning phrase (jittered)",
      variable == "DLD" ~ "Developmental Language Disorder",
      variable == "ADHD" ~ "Attention-Deficit/ Hyperactivity Disorder",
      variable == "ASD" ~ "Autism Spectrum Disorder"
    ) |> 
      fct_reorder(percentage)
  ) |> 
  select(name,percentage) |> 
  ggplot(aes(percentage,name)) +
  geom_col() +
  theme_few() +
  # xlab('variable importance (scaled)') +
  xlim(0,.171) +
  theme(axis.title.y = element_blank()) +
  ggtitle('expressive vocabulary')

p1 / p2
ggsave('viz/varimp.png', width = 5, height = 5, dpi = 900)

# -- lm -- #

lm11 = lm(sent_rep ~ n_back_2_mean_score * group, data = d)
lm12 = lm(sent_rep ~ digit_span_forward * group, data = d)
lm13 = lm(sent_rep ~ digit_span_backward * group, data = d)
lm14 = lm(sent_rep ~ AGL_offline * group, data = d)
lm15 = lm(sent_rep ~ AGL_medRT_diff * group, data = d)
lm11b = lm(sent_rep ~ n_back_2_mean_score + group, data = d)
lm12b = lm(sent_rep ~ digit_span_forward + group, data = d)
lm13b = lm(sent_rep ~ digit_span_backward + group, data = d)
lm14b = lm(sent_rep ~ AGL_offline + group, data = d)
lm15b = lm(sent_rep ~ AGL_medRT_diff + group, data = d)
anova(lm11,lm11b)
anova(lm12,lm12b)
anova(lm13,lm13b) # p = 0.0477
anova(lm14,lm14b)
anova(lm15,lm15b)
plot(compare_performance(lm11,lm11b, metrics = 'common'))
plot(compare_performance(lm12,lm12b, metrics = 'common'))
plot(compare_performance(lm13,lm13b, metrics = 'common'))
plot(compare_performance(lm14,lm14b, metrics = 'common'))
plot(compare_performance(lm15,lm15b, metrics = 'common'))

lm21 = lm(sent_rep ~ digit_span_backward * group, data = d)
lm22 = lm(sent_rep ~ AGL_offline * group, data = d)
lm23 = lm(sent_rep ~ PS_vis_RT_med * group, data = d)
lm24 = lm(sent_rep ~ PS_ac_RT_med * group, data = d)
lm25 = lm(sent_rep ~ n_back_2_mean_score * group, data = d)
lm21b = lm(sent_rep ~ digit_span_backward + group, data = d)
lm22b = lm(sent_rep ~ AGL_offline + group, data = d)
lm23b = lm(sent_rep ~ PS_vis_RT_med + group, data = d)
lm24b = lm(sent_rep ~ PS_ac_RT_med + group, data = d)
lm25b = lm(sent_rep ~ n_back_2_mean_score + group, data = d)

anova(lm21,lm21b) # .0477
anova(lm22,lm22b)
anova(lm23,lm23b)
anova(lm24,lm24b)
anova(lm25,lm25b)
plot(compare_performance(lm21,lm21b, metrics = 'common'))
plot(compare_performance(lm22,lm22b, metrics = 'common'))
plot(compare_performance(lm23,lm23b, metrics = 'common'))
plot(compare_performance(lm24,lm24b, metrics = 'common'))
plot(compare_performance(lm25,lm25b, metrics = 'common'))

# -- viz -- #

keep1 = varimp1 |> 
  filter(percentage > .085) |> 
  pull(variable)

keep2 = varimp2 |> 
  filter(percentage > .085) |> 
  pull(variable)

plots1 = d |> 
  select(ID,sent_rep,contains(keep1)) |> 
  rename(`Sentence Repetition` = sent_rep) |> 
  pivot_longer(-c(ID,`Sentence Repetition`)) |> 
  nest(.by = name) |> 
  mutate(
    name2 = case_when(
      name == "digit_span_forward" ~ "Digit Span Forward (jittered)",
      name == "digit_span_backward" ~ "Digit Span Backward (jittered)",
      name == "AGL_offline" ~ "Artificial Grammar Learning Offline",
      name == "AGL_medRT_diff" ~ "Median Reaction Time Difference in AGL",
      name == "n_back_2_mean_score" ~ "N-Back 2 Mean Score",      
    ),
    plot = map2(data, name2, ~ 
                 ggplot(.x, aes(value,`Sentence Repetition`)) +
                 geom_point() +
                 geom_smooth() +
                 theme_minimal() +
                 xlab(.y)
            )
  )

wrap_plots(plots1$plot)
ggsave('viz/sent_rep.png', width = 9, height = 3, dpi = 900)

plots2 = d |> 
  select(ID,expr_vocab,contains(keep1)) |> 
  rename(`Expressive Vocabulary` = expr_vocab) |> 
  pivot_longer(-c(ID,`Expressive Vocabulary`)) |> 
  nest(.by = name) |> 
  mutate(
    name2 = case_when(
      name == "digit_span_forward" ~ "Digit Span Forward (jittered)",
      name == "digit_span_backward" ~ "Digit Span Backward (jittered)",
      name == "AGL_offline" ~ "Artificial Grammar Learning Offline",
      name == "AGL_medRT_diff" ~ "Median Reaction Time Difference in AGL",
      name == "n_back_2_mean_score" ~ "N-Back 2 Mean Score",      
    ),
    plot = map2(data, name2, ~ 
                  ggplot(.x, aes(value,`Expressive Vocabulary`)) +
                  geom_point() +
                  geom_smooth() +
                  theme_minimal() +
                  xlab(.y)
    )
  )

wrap_plots(plots2$plot)
ggsave('viz/expr_vocab.png', width = 9, height = 3, dpi = 900)
