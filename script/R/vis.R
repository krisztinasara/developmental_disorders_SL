# -- setup -- #

setwd('~/Github/developmental_disorders_SL/')

library(tidyverse)
library(patchwork)
library(ggthemes)
library(ggridges)
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
  
# -- fun -- #

# parse json object of xgboost results and parameters into df
parseResults = function(parsed){
  
  # Extract best_params into a data frame
  df_params = data.frame(
    variable = names(parsed$best_params),
    value = unlist(parsed$best_params),
    stringsAsFactors = FALSE
  )
  
  # Add neg_mean_squared_error
  df_params = rbind(
    df_params,
    data.frame(variable = "neg_mean_squared_error", value = parsed$neg_mean_squared_error)
  )
  
  return(df_params)
}

# rename columns using long names
rename_columns = function(dat, mapping) {
  # For each left-hand (old) name in mapping, check if it appears in dat
  # If found, rename that column to the corresponding value
  for (col_name in names(mapping)) {
    if (col_name %in% names(dat)) {
      names(dat)[names(dat) == col_name] = mapping[[col_name]]
    }
  }
  return(dat)
}

draw_raw = function(dat, my_var, y_var = c("sent_rep", "expr_vocab")) {
  # y_var can be either "sent_rep" or "expr_vocab"
  y_var <- match.arg(y_var)
  
  my_name = long_names[my_var]
  ylab_txt = if (y_var == "sent_rep") "Sentence repetition" else "Expressive vocabulary"
  ylim_range = if (y_var == "sent_rep") c(0, 1.3) else c(0.25, 1)
  
  dat |> 
    mutate(group = fct_relevel(group, 'TD')) |> 
    ggplot(aes(!!sym(my_var), !!sym(y_var))) +
    geom_point(alpha = .25) +
    # geom_density_2d(colour = 'lightgrey') +
    geom_smooth(method = "gam", formula = y ~ s(x, k = 3)) +
    # geom_rug() +
    theme_few() +
    facet_wrap( ~ group, nrow = 1, labeller = my_labeller) +
    xlab(my_name) +
    ylab(ylab_txt) +
    ylim(ylim_range)
}

rename_columns_2 = partial(rename_columns, mapping = long_names)

draw_raw_sr_2 = partial(draw_raw, dat = d, y_var = "sent_rep")
draw_raw_ev_2 = partial(draw_raw, dat = d, y_var = 'expr_vocab')

# -- read -- #

d = read_csv('data/df.csv')
p_ev = read_csv('data/predictions_expr_vocab.csv')
p_sr = read_csv('data/predictions_sent_rep.csv')
f_ev = read_csv('data/feature_importances_expr_vocab.csv')
f_sr = read_csv('data/feature_importances_sent_rep.csv')
x_ev = read_json('data/results_expr_vocab.json')
x_sr = read_json('data/results_sent_rep.json')

draws = read_tsv('data/boot_estimates.gz')
pearson = read_tsv('data/pearson.tsv')

# -- wrangle -- #

x_ev = parseResults(x_ev)
x_sr = parseResults(x_sr)

d = d |> 
  left_join(p_ev) |> 
  left_join(p_sr) |> 
  select(-prod,-AFC_phr,-AFC_sent)

# -- corr -- #

# build correlation table of all predictor and outcome variables
cors = d |>
  select(-ID,-age_years,-group,-sent_rep_pred,-expr_vocab_pred,-IQ,-sent_rep,-expr_vocab) |> 
  rename_columns_2() |> 
  # rename_with(~ str_replace_all(.x, "\\n", " ")) |> # get rid of \n
  na.omit() |> 
  cor(method = 'spearman') |> 
  as.data.frame() |> 
  rownames_to_column(var = "Var1") %>%
  pivot_longer(cols = -Var1, names_to = "Var2", values_to = "Correlation")

# fix factor levels
my_factor_levels = c(
  "Developmental\nLanguage Disorder",
  "Autism Spectrum\nDisorder",
  "Attention-Deficit/\nHyperactivity Disorder",
  "perceptual speed:\nvisual response time",
  "perceptual speed:\nauditory response time",
  "working memory:\nforward digit span",
  "working memory:\nbackward digit span",
  "working memory:\nn-back",
  "statistical learning:\nartificial grammar learning\nresponse time",
  "statistical learning:\nartificial grammar learning\noffline"
)

# visualise it
cors |>
  mutate(
    Var1 = factor(Var1, levels = my_factor_levels),
    Var2 = factor(Var2, levels = my_factor_levels) |> fct_rev(),
    Correlation = ifelse(
      str_detect(Var1, 'Disorder') & str_detect(Var2, 'Disorder') & Correlation != 1, NA, Correlation
    )
  ) |> 
  ggplot(aes(Var1,Var2,fill = Correlation, label = round(Correlation,2))) +
  geom_tile() +
  geom_text(colour = 'white') +
  theme_few() +
  theme(
    axis.title = element_blank(),
    axis.ticks = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 0),
    plot.margin = margin(5.5, 50, 5.5, 5.5)
    ) +
  guides(fill = 'none') +
  scale_x_discrete(position = 'top') +
  scale_fill_viridis_c(option = 'cividis')

ggsave('viz/correlations.png', width = 9, height = 5, dpi = 900)

# -- missing -- #

d |>
  select(-group,-sent_rep_pred,-expr_vocab_pred) |> 
  rename_columns_2() |> 
  # rename_with(~ str_replace_all(., '\\n', ' ')) |> 
  pivot_longer(-Id) |>
  mutate(
    missing = is.na(value),
    name = fct_inorder(name)
  ) |> 
  ggplot(aes(Id,name,fill = missing)) +
  geom_tile() +
  scale_fill_manual(values = c('black','white')) +
  guides(fill = 'none') +
  theme_few() +
  theme(
    axis.title = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks = element_blank()
        )

ggsave('viz/missing.png', width = 7, height = 5, dpi = 900)

# -- xgboost predictions -- #

p2 = d |> 
  ggplot(aes(sent_rep,sent_rep_pred)) +
  geom_point() +
  geom_smooth() +
  theme_bw() +
  xlab('sentence repetition') +
  ylab('sentence repetition (predicted)')

p1 = d |> 
  ggplot(aes(expr_vocab,expr_vocab_pred)) +
  geom_point() +
  geom_smooth() +
  theme_bw() +
  xlab('expressive vocabulary') +
  ylab('expressive vocabulary (predicted)')

p1 + p2
ggsave('viz/predictions.png', width = 6, height = 3, dpi = 900)

# -- varimp -- #

p2 = f_sr |> 
  mutate(
    Feature = ifelse(Feature %in% names(long_names), long_names[Feature], Feature) |>
      #str_replace_all('\\n', ' ') |> 
      fct_reorder(Importance)
    ) |> 
  ggplot(aes(Importance,Feature)) +
  geom_col() +
  theme_few() +
  theme(
    axis.ticks.y = element_blank(),
    axis.title.y = element_blank()
        ) +
  ggtitle('Sentence repetition model')

p1 = f_ev |> 
  mutate(
    Feature = ifelse(Feature %in% names(long_names), long_names[Feature], Feature) |>
      #str_replace_all('\\n', ' ') |> 
      fct_reorder(Importance)
  ) |> 
  ggplot(aes(Importance,Feature)) +
  geom_col() +
  theme_few() +
  theme(
    axis.ticks.y = element_blank(),
    axis.title.y = element_blank()
  ) +
  ggtitle('Expressive vocabulary model')

p1 / p2
ggsave('viz/importances.png', dpi = 900, width = 6, height = 10.5)

# -- scatterplots -- #

# brutalis ganyolas!
keep_names = d |>
  select(-ID,-age_years,-group,-sent_rep_pred,-expr_vocab_pred,-IQ,-group_DLD,-group_ASD,-group_ADHD,-sent_rep,-expr_vocab) |> 
  names()

sent_rep_plots = map(keep_names, draw_raw_sr_2)
expr_vocab_plots = map(keep_names, draw_raw_ev_2)

wrap_plots(sent_rep_plots, ncol = 1) + plot_annotation(title = 'Sentence repetition and other predictors') + plot_layout(axes = 'collect')
ggsave('viz/sent_rep.png', dpi = 900, width = 6.5, height = 18)

wrap_plots(expr_vocab_plots, ncol = 1) + plot_annotation(title = 'Expressive vocabulary and other predictors') + plot_layout(axes = 'collect')
ggsave('viz/expr_vocab.png', dpi = 900, width = 6.5, height = 18)

# -- forest plots -- #

## pearson

pearson2 = pearson |> 
  mutate(
    predictor_label = ifelse(
      predictor_name %in% names(long_names),
      long_names[predictor_name],
      predictor_name
    )
  )

p1 = pearson2 |> 
  filter(outcome_name == 'expr_vocab') |>   
  ggplot() +
  geom_point(aes(estimate,group)) +
  geom_linerange(aes(xmin = conf.low, xmax = conf.high, y = group)) +
  theme_minimal() +
  theme(
    axis.text.y = element_blank(), # since this is on left
    axis.ticks.y = element_blank(),
    panel.border = element_blank(),
    axis.title.y = element_blank(),
    strip.text.y.left = element_text(angle = 0, hjust = 0.5) # horizontal facet labels
  ) + 
  xlab('Pearson correlation (with 99% CI)') +
  scale_y_discrete(position = "right") +
  facet_wrap(~ predictor_label, strip.position = "left", ncol = 1) +
  ggtitle('Expressive vocabulary') +
  geom_vline(xintercept = 0, lty = 3)

p2 = pearson2 |> 
  filter(outcome_name == 'sent_rep') |>   
  ggplot() +
  geom_point(aes(estimate,group)) +
  geom_linerange(aes(xmin = conf.low, xmax = conf.high, y = group)) +
  theme_minimal() +
  theme(
    strip.text = element_blank(), # since this is on the right
    axis.ticks.y = element_blank(),
    panel.border = element_blank(),
    axis.title.y = element_blank()
  ) + 
  xlab('Pearson correlation (with 99% CI)') +
  scale_y_discrete(position = "right") +
  facet_wrap(~ predictor_label, ncol = 1) +
  ggtitle('Sentence repetition') +
  geom_vline(xintercept = 0, lty = 3)

p1 + p2 + plot_layout(axis_titles = "collect")

ggsave('viz/pearson.png', dpi = 900, width = 7.5, height = 5, bg = 'white')

## spearman

draws2 = draws |> 
  mutate(
    predictor_label = ifelse(
      predictor_name %in% names(long_names),
      long_names[predictor_name],
      predictor_name
    )
  )

p3 = draws2 |> 
  filter(outcome_name == 'expr_vocab') |>   
  ggplot(aes(draws, group)) +
  stat_density_ridges(
    bandwidth = 0.029,
    geom = "density_ridges_gradient", 
    rel_min_height = 0.01,
    scale = 1
  ) +
  theme_minimal() +
  guides(fill = 'none') +
  theme(
    panel.border = element_blank(),
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    strip.text.y.left = element_text(angle = 0, hjust = 0.5) # horizontal facet labels
  ) + 
  xlab('Spearman correlation (bootstrapped, 99% range)') +
  scale_y_discrete(position = "right", expand = expansion(mult = c(.1, .35))) +
  facet_wrap(~ predictor_label, strip.position = "left", ncol = 1) +
  ggtitle('Expressive vocabulary') +
  geom_vline(xintercept = 0, lty = 3)

p4 = draws2 |> 
  filter(outcome_name == 'sent_rep') |>   
  ggplot(aes(draws, group)) +
  stat_density_ridges(
    bandwidth = 0.029,
    geom = "density_ridges_gradient", 
    rel_min_height = 0.01,
    scale = 1
  ) +
  theme_minimal() +
  guides(fill = 'none') +
  theme(
    panel.border = element_blank(),
    strip.text = element_blank(),
    axis.title.y = element_blank()
  ) + 
  xlab('Spearman correlation (bootstrapped, 99% range)') +
  scale_y_discrete(position = "right", expand = expansion(mult = c(.1, .35))) +
  facet_wrap(~ predictor_label, strip.position = "left", ncol = 1) +
  ggtitle('Sentence repetition') +
  geom_vline(xintercept = 0, lty = 3)

p3 + p4 + plot_layout(axis_titles = 'collect')

ggsave('viz/spearman.png', dpi = 900, width = 6, height = 7.5, bg = 'white')
