library(ggthemes)
library(patchwork)
library(sjPlot)

plots1 = d |>
  nest(.by = group) |> 
  mutate(
    plot = map2(
      group, data, ~ ggplot(.y, aes(
        sent_rep,sent_rep_pred_rf
      )) +
        geom_point() +
        geom_smooth() +
        theme_few() +
        ggtitle(.x)
    )
  ) |> 
  pull(plot)

wrap_plots(plots1)  

with(d, cor.test(sent_rep,sent_rep_pred_rf))
# .95, .99

plots2 = d |>
  nest(.by = group) |> 
  mutate(
    plot = map2(
      group, data, ~ ggplot(.y, aes(
        expr_vocab,expr_vocab_pred_rf
      )) +
        geom_point() +
        geom_smooth() +
        theme_few() +
        ggtitle(.x)
    )
  ) |> 
  pull(plot)

wrap_plots(plots2)  

with(d, cor.test(expr_vocab,expr_vocab_pred_rf))
# .86-.93

response1
varimp1 |> 
  select(variable,percentage) |> 
  knitr::kable(digits = 2)
response2
varimp2 |> 
  select(variable,percentage) |> 
  knitr::kable(digits = 2)

response1

varimp1 |> 
  filter(percentage > .09) |> 
  pull(variable)

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
anova(lm13,lm13b)
anova(lm14,lm14b)
anova(lm15,lm15b)

plot_model(lm13, 'pred', terms = c('digit_span_backward','group')) +
  theme_few() +
  facet_wrap( ~ group) +
  guides(colour = F)

response2

varimp2 |> 
  filter(percentage > .085) |> 
  pull(variable)

lm22 = lm(sent_rep ~ digit_span_backward * group, data = d)
lm22 = lm(sent_rep ~ AGL_offline * group, data = d)
lm23 = lm(sent_rep ~ PS_vis_RT_med * group, data = d)
lm24 = lm(sent_rep ~ PS_ac_RT_med * group, data = d)
lm25 = lm(sent_rep ~ n_back_2_mean_score * group, data = d)
lm22b = lm(sent_rep ~ digit_span_backward + group, data = d)
lm22b = lm(sent_rep ~ AGL_offline + group, data = d)
lm23b = lm(sent_rep ~ PS_vis_RT_med + group, data = d)
lm24b = lm(sent_rep ~ PS_ac_RT_med + group, data = d)
lm25b = lm(sent_rep ~ n_back_2_mean_score + group, data = d)

anova(lm22,lm22b)
anova(lm22,lm22b)
anova(lm23,lm23b)
anova(lm24,lm24b)
anova(lm25,lm25b)

