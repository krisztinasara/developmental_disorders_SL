# -- setup -- #

setwd('~/Github/developmental_disorders_SL/')

library(tidyverse)
library(patchwork)
library(ggthemes)
library(sjPlot)
library(corrplot)
library(performance)
library(glue)
library(jsonlite)

# -- read -- #

d = read_csv('data/df.csv')
p_ev = read_csv('data/predictions_expr_vocab.csv')
p_sr = read_csv('data/predictions_sent_rep.csv')
f_ev = read_csv('data/feature_importances_expr_vocab.csv')
f_sr = read_csv('data/feature_importances_sent_rep.csv')
read_json('data/results_expr_vocab.json')
read_json('data/results_sent_rep.json')

# -- wrangle -- #

d = d |> 
  left_join(p_ev) |> 
  left_join(p_sr)

all_names = names(d)[!names(d) %in% c('ID','age_years','prod','group')]
var_names = names(d)[!names(d) %in% c('ID','age_years','prod','sent_rep','expr_vocab')]
var_names_2 = c(var_names[var_names != 'group'],'DLD','ASD','ADHD')
long_names = c(
  `expr_vocab` = "Expressive vocabulary",
  `sent_rep` = 'Sentence repetition',
  `prod` = 'production',
  `IQ` = "Intelligence Quotient",
  `group` = "Group",
  `AGL_medRT_diff` = "Median Reaction Time\nDifference in AGL",
  `AGL_offline` = "Artificial Grammar\nLearning Offline",
  `digit_span_forward` = "Digit Span Forward\n(jittered)",
  `digit_span_backward` = "Digit Span Backward\n(jittered)",
  `PS_vis_RT_med` = "Processing Speed\nVisual RT Median",
  `PS_ac_RT_med` = "Processing Speed\nAcoustic RT Median",
  `n_back_2_mean_score` = "N-Back 2\nMean Score",
  `AFC_phr` = "Artificial Grammar\nLearning\nphrase (jittered)",
  `AFC_sent` = "Artificial Grammar\nLearning\nsentence (jittered)",
  `TD` = 'Typically Developing',
  `DLD` = 'Developmental\nLanguage Disorder',
  `ADHD` = 'Attention-Deficit/\nHyperactivity Disorder',
  `ASD` = 'Autism Spectrum\nDisorder'
)

all_names_long = map_chr(
  all_names,
  ~ long_names[.]
)

var_names_long = map_chr(
  var_names, 
  ~ long_names[.]
)

var_names_2_long = map_chr(
  var_names_2, 
  ~ long_names[.]
)

# -- corr -- #

cors = d |>
  mutate(
    DLD = group == 'DLD',
    ASD = group == 'ASD',
    ADHD = group == 'ADHD'
  ) |> 
  select(all_of(var_names_2)) |> 
  na.omit() |> 
  cor() |> 
  as.data.frame() |> 
  rownames_to_column(var = "Var1") %>%
  pivot_longer(cols = -Var1, names_to = "Var2", values_to = "Correlation")

cors |> 
  mutate(
    Var1 = fct_relevel(Var1, var_names_2),
    Var2 = fct_relevel(Var2, var_names_2)
    ) |> 
  ggplot(aes(Var1,Var2,fill = Correlation, label = round(Correlation,2))) +
  geom_tile() +
  geom_text(colour = 'white') +
  scale_fill_viridis_c(option = 'turbo') +
  theme_few() +
  theme(
    axis.title = element_blank(),
    axis.ticks = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 0),
    plot.margin = margin(5.5, 50, 5.5, 5.5)
    ) +
  guides(fill = 'none') +
  scale_y_discrete(labels = fct_rev(var_names_2_long)) +
  scale_x_discrete(labels = fct_rev(var_names_2_long), position = 'top')

ggsave('viz/correlations.png', width = 9, height = 6, dpi = 900)

# -- missing -- #

d |>
  select(ID,all_of(all_names)) |> 
  pivot_longer(-ID) |> 
  mutate(
    missing = is.na(value),
    name = fct_relevel(name, all_names)
  ) |> 
  ggplot(aes(ID,name,fill = missing)) +
  geom_tile() +
  scale_fill_manual(values = c('darkgrey','white')) +
  guides(fill = 'none') +
  theme_few() +
  theme(
    axis.title.x = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks = element_blank()
        ) +
  scale_y_discrete(labels = fct_rev(all_names_long))

ggsave('viz/missing.png', width = 12, height = 6, dpi = 900)

# -- xgboost predictions -- #

p1 = d |> 
  ggplot(aes(sent_rep,sent_rep_pred)) +
  geom_point() +
  theme_bw() +
  xlab('sentence repetition') +
  ylab('sentence repetition (predicted)')

p2 = d |> 
  ggplot(aes(expr_vocab,expr_vocab_pred)) +
  geom_point() +
  theme_bw() +
  xlab('expressive vocabulary') +
  ylab('expressive vocabulary (predicted)')

p1 + p2
ggsave('viz/predictions.png', width = 6, height = 3, dpi = 900)

# -- varimp -- #

p1 = f_sr |> 
  mutate(Feature = fct_reorder(Feature,Importance)) |> 
  ggplot(aes(Importance,Feature)) +
  geom_col() +
  theme_few() +
  theme(axis.title.y = element_blank()) +
  ggtitle('Sentence repetition model')

p2 = f_ev |> 
  mutate(Feature = fct_reorder(Feature,Importance)) |> 
  ggplot(aes(Importance,Feature)) +
  geom_col() +
  theme_few() +
  theme(axis.title.y = element_blank()) +
  ggtitle('Expressive vocabulary model')

p1 + p2

# -- lms -- #

fit = function(out_name,var_name){
  lm(as.formula(paste(out_name, "~", var_name)), data = d)
}

fit_sent_rep = partial(fit, out_name = 'sent_rep')
fit_expr_vocab = partial(fit, out_name = 'expr_vocab')

# quiet_fit_sent_rep = quietly(fit_sent_rep)
# 
# map(
#   var_names,
#   ~ quiet_fit_sent_rep(.)$warnings
# )
# I forgot the outcome in varnames

fits_sent_rep = map(
  var_names,
  fit_sent_rep
)

fits_expr_vocab = map(
  var_names,
  fit_expr_vocab
)

p1 = plot(compare_performance(fits_sent_rep, metrics = 'common')) +
  scale_colour_viridis_d(labels = var_names_long, option = 'turbo') +
  ggtitle('sentence repetiton')
p2 = plot(compare_performance(fits_expr_vocab, metrics = 'common')) +
  scale_colour_viridis_d(labels = var_names_long, option = 'turbo') +
  ggtitle('expressive vocabulary')

p1 / p2 + plot_layout(guides = 'collect')

ggsave('viz/modelfits.png', width = 9, height = 6, dpi = 900)
