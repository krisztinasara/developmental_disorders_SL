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

# take df with fixed col names and bootstrap a distribution of spearman correlations
bootstrapSpearman = function(my_df){
  
  my_df = longd2$data[[1]]
  
  my_stat = function(data, indices) {
    cor(data$outcome_value_s[indices],
        data$predictor_value_s[indices],
        method = 'spearman')
  }
  
  boot_result = boot(
    data = my_df,
    statistic = my_stat, 
    R = 2000
  )
  
  bootstrapped_distribution = data.frame(spearman_rho = boot_result$t[,1])
  return(bootstrapped_distribution)
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
  select(-ID,-age_years,-group,-sent_rep_pred,-expr_vocab_pred,-IQ,-group_DLD,-group_ASD,-group_ADHD,-sent_rep,-expr_vocab) |> 
  rename_columns_2() |> 
  # rename_with(~ str_replace_all(.x, "\\n", " ")) |> # get rid of \n
  na.omit() |> 
  cor(method = 'spearman') |> 
  as.data.frame() |> 
  rownames_to_column(var = "Var1") %>%
  pivot_longer(cols = -Var1, names_to = "Var2", values_to = "Correlation")

# visualise it
cors |>
  mutate(
    Var1 = fct_inorder(Var1) |> fct_rev(),
    Var2 = Var2 |>
      # str_replace_all('\\n', ' ') |> 
      fct_inorder()
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

ggsave('viz/correlations.png', width = 7.5, height = 5, dpi = 900)

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

p1 = d |> 
  ggplot(aes(sent_rep,sent_rep_pred)) +
  geom_point() +
  geom_smooth() +
  theme_bw() +
  xlab('sentence repetition') +
  ylab('sentence repetition (predicted)')

p2 = d |> 
  ggplot(aes(expr_vocab,expr_vocab_pred)) +
  geom_point() +
  geom_smooth() +
  theme_bw() +
  xlab('expressive vocabulary') +
  ylab('expressive vocabulary (predicted)')

p1 + p2
ggsave('viz/predictions.png', width = 6, height = 3, dpi = 900)

# -- varimp -- #

p1 = f_sr |> 
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

p2 = f_ev |> 
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
ggsave('viz/importances.png', dpi = 900, width = 6, height = 12)

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

# -- forest plot -- #

# thank you o3
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
         ) |> 
  mutate(predictor_tidy = recode(predictor_name, !!!long_names))

longd2 = longd |> 
  nest(.by = c(group,outcome_name,predictor_tidy)) |> 
  mutate(
    data = map(
      data, 
      ~ mutate(., 
               outcome_value_s = rescale(outcome_value),
               predictor_value_s = rescale(predictor_value)
               )
      )
    )

ests = longd2 |> 
  mutate(
    pearson = map(
      data, 
      ~ tidy(cor.test(.$outcome_value_s,.$predictor_value_s, method = 'pearson'))
    )
  ) |> 
  unnest(pearson) |> 
  select(group,outcome_name,predictor_tidy,estimate,conf.low,conf.high) |> 
  mutate(
    max_est = max(estimate),
    .by = predictor_tidy
  ) |> 
  mutate(
    group = fct_relevel(group, 'DLD','ASD','ADHD','TD'),
    predictor_ordered = reorder(predictor_tidy, max_est),
    pred_num = as.numeric(factor(predictor_ordered)),
    pred_jit = pred_num + case_when(
      group == 'TD' ~ -.27,
      group == 'ADHD' ~ -.09,
      group == 'ASD' ~ .09,
      group == 'DLD' ~ .27
    )
  )

ests |>
  ggplot(aes(estimate, pred_jit, colour = group)) +
  geom_point() +
  geom_linerange(aes(xmin = conf.low, xmax = conf.high)) +
  scale_y_continuous(
    breaks = unique(ests$pred_num),
    labels = unique(ests$predictor_tidy)
  ) +
  facet_wrap(~ outcome_name, labeller = as_labeller(c("sent_rep" = "sentence\nrepetition", "expr_vocab" = "expressive\nvocabulary"))) +
  theme_bw() +
  theme(
    # panel.grid.major.x = element_blank(),    # Remove major vertical grid lines
    panel.grid.minor.x = element_blank(),    # Remove minor vertical grid lines
    panel.grid.minor.y = element_blank(),     # Remove minor horizontal grid lines
    axis.title.y = element_blank(),
    legend.position = 'top'
  ) +
  xlab('Pearson correlation\nwith 95% confidence interval') +
  scale_colour_colorblind(labels = c('typically\ndeveloping','attention-deficit\nhyperactivity disorder','autism spectrum\ndisorder','developmental\nlanguage disorder')) +
  guides(colour = guide_legend(ncol = 1)) +
  geom_vline(xintercept = 0, lty = 3)
ggsave('viz/betas.png', dpi = 'print', width = 5, height = 7.5)
