library(tidyverse)
library(ggthemes)
library(patchwork)

d = read_csv('data/df.csv')
varimp = read_tsv('varimp1.tsv')

vars = varimp |> 
  slice(1:7) |> 
  pull(variable)

makeScatterplot = function(dat,col1,col2,lab1,lab2){
  dat |> 
    mutate(group = fct_relevel(group, 'TD')) |> 
    ggplot(aes({{col1}},{{col2}}, colour = group)) +
    geom_point(alpha = .25) +
    geom_smooth() +
    theme_few() +
    scale_colour_viridis_d(option = 'turbo') +
    xlab(lab1) +
    ylab(lab2) +
    theme(
      axis.title = element_blank(), 
      axis.text = element_blank(), 
      axis.ticks = element_blank()
      )
}

makeBoxplotGr = function(dat, col1, lab1){
  dat |> 
    mutate(group = fct_relevel(group)) |> 
    ggplot(aes(y = {{col1}}, x = group, fill = group)) +
    geom_boxplot() +
    ylab(lab1) +
    theme_few() +
    scale_fill_viridis_d(option = 'turbo') +
    guides(fill = 'none') +
    theme(
      axis.title.x = element_blank(),
      axis.text.x = element_blank(), 
      axis.ticks.x = element_blank(),
      axis.title.y = element_text(angle = 0, vjust = 0.5)
    )
}

makeBoxplotGrAlt = function(dat, col1, lab1){
  dat |> 
    mutate(group = fct_relevel(group)) |> 
    ggplot(aes(x = {{col1}}, y = group, fill = group)) +
    geom_boxplot() +
    xlab(lab1) +
    theme_few() +
    scale_fill_viridis_d(option = 'turbo') +
    guides(fill = 'none') +
    theme(
      axis.title.y = element_blank(),
      axis.text.y = element_blank(), 
      axis.ticks.y = element_blank()
    )
}

makeScatterplotSR = partial(makeScatterplot, dat = d, col1 = sent_rep, lab1 = 'sentence\nrepetition')

makeScatterplotEV = partial(makeScatterplot, dat = d, col1 = expr_vocab, lab1 = 'expressive\nvocabulary')

makeBoxplotGrD = partial(makeBoxplotGr, dat = d)

p1b = makeScatterplotSR(n_back_2_mean_score,'N-back\nmean score')
p2b = makeScatterplotSR(digit_span_forward,'digit span\n(forward)')
p3b = makeScatterplotSR(digit_span_backward,'digit span\n(backward)')
p4b = makeScatterplotSR(AGL_offline,'art. grammar\nlearning\noffline')
p5b = makeScatterplotSR(AGL_medRT_diff,'art. grammar\nlearning\nmedian RT difference')
p6b = makeScatterplotSR(PS_ac_RT_med,'processing speed\nmedian RT')

p1a = makeBoxplotGrD(n_back_2_mean_score,'N-back\nmean score')
p2a = makeBoxplotGrD(digit_span_forward,'digit span\n(forward)')
p3a = makeBoxplotGrD(digit_span_backward,'digit span\n(backward)')
p4a = makeBoxplotGrD(AGL_offline,'art. grammar\nlearning\noffline')
p5a = makeBoxplotGrD(AGL_medRT_diff,'art. grammar\nlearning\nmedian RT difference')
p6a = makeBoxplotGrD(PS_ac_RT_med,'processing speed\nmedian RT')
p7 = makeBoxplotGrAlt(dat = d, col1 = sent_rep, lab1 = 'sentence\nrepetition')

(p1a + p1b) /
  (p2a + p2b) /
  (p3a + p3b) /
  (p4a + p4b) /
  (p5a + p5b) /
  (p6a + p6b) /
  (plot_spacer() + p7) + plot_layout(guides = 'collect')

ggsave('monstre_abra_1.png', dpi = 900, width = 6, height = 12)
