library(tidyverse)
library(ggpubr)
library(patchwork)

# load data
dem = read_csv("../data/dem_groups_res.csv")
df = read_csv("../data/df.csv")

# make group a factor with this order: TD, ADHD, ASD, DLD
dem$group = factor(dem$group, levels = c("TD", "ADHD", "ASD", "DLD"))

# plot age from age_mean and age_SD (as error bars) by group from data with stat = identity
age = ggplot(dem, aes(x = group, y = age_mean, ymin = age_mean - age_SD, ymax = age_mean + age_SD)) +
  geom_pointrange() +
  theme_minimal() +
  labs(y = "Age") +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank())

# plot IQ from IQ_mean and IQ_SD (as error bars) by group from data with stat = identity
IQ = ggplot(dem, aes(x = group, y = IQ_mean, ymin = IQ_mean - IQ_SD, ymax = IQ_mean + IQ_SD)) +
  geom_pointrange() +
  theme_minimal() +
  labs(y = "IQ") +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank())

# make a new long df with group and n_males and n_females
sex = dem |>
  select(group, n_males, n_females) |>
  pivot_longer(cols = c(n_males, n_females)) |>
  ggplot(aes(x = group, y = value, fill = name)) +
  geom_bar(stat = "identity", position = "stack") +
  theme_minimal() +
  labs(x = "Group", y = "Group size and sex ratio") +
  theme(legend.position = "none")

# combine plots
ggarrange(age, IQ, sex, nrows = 3, ncol = 1)



# scatterplot
library(ggplot2)
library(cowplot)
library(dplyr)
library(rlang)

draw_combined_plot = function(df, group_name, x, y, x_name, y_name) {
  
  within_var = list()
  
  for (group_name in c("TD", "ADHD", "ASD", "DLD")) {
    
    p_main = df |> filter(group == group_name) |>
      ggplot(aes_string(x = x, y = y)) +
      geom_point() +
      geom_smooth(method = "lm") +
      theme_bw() +
      coord_cartesian(
        xlim = c(min(df[[x]], na.rm = TRUE), max(df[[x]], na.rm = TRUE)),
        ylim = c(min(df[[y]], na.rm = TRUE), max(df[[y]], na.rm = TRUE))
      ) +
      theme(
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 14),
        plot.margin = margin(0, 0, 0, 0)
      )
    
    p_top = df |> filter(group == group_name) |>
      ggplot(aes_string(x = x)) +
      geom_density(fill = "black", alpha = 0.3) +
      theme_void() +
      theme(plot.margin = margin(0, 0, 0, 0)) +
      coord_cartesian(
        xlim = c(min(df[[x]], na.rm = TRUE), max(df[[x]], na.rm = TRUE))
      )
    
    p_right = df |> filter(group == group_name) |>
      ggplot(aes_string(x = y)) +
      geom_density(fill = "black", alpha = 0.3) +
      theme_void() +
      theme(plot.margin = margin(0, 0, 0, 0)) +
      coord_flip(xlim = c(min(df[[y]], na.rm = TRUE), max(df[[y]], na.rm = TRUE)))
    
    within_var[[group_name]] = plot_grid(
      p_top, NULL,
      p_main, p_right,
      ncol = 2, nrow = 2,
      rel_heights = c(0.25, 1),
      rel_widths = c(1, 0.25),
      align = "hv",
      axis = "tblr"
    )
  }
  
  combined_plot = plot_grid(
    within_var[["TD"]],
    within_var[["ADHD"]],
    within_var[["ASD"]],
    within_var[["DLD"]],
    ncol = 4, nrow = 1,
    scale = 0.8,
    labels = c("TD", "ADHD", "ASD", "DLD"), label_size = 18,
    hjust = -1
  )
  
  final_plot = ggdraw(combined_plot) +
    draw_label(x_name, x = 0.1, y = 0, vjust = 1, angle = 0, size = 18) +
    draw_label(y_name, x = 0, y = 0.5, vjust = -1, angle = 90, size = 18)
  
  return(final_plot)
  
}

plot_grid(
  draw_combined_plot(
    df = df,
    group_name = "group",
    x = "AGL_medRT_diff",
    y = "expr_vocab",
    x_name = "AGL online",
    y_name = "expressive vocabulary"
    ),
  draw_combined_plot(
    df = df,
    group_name = "group",
    x = "AGL_offline",
    y = "expr_vocab",
    x_name = "AGL offline",
    y_name = "expressive vocabulary"
  ),
  draw_combined_plot(
    df = df,
    group_name = "group",
    x = "AGL_medRT_diff",
    y = "sent_rep",
    x_name = "AGL online",
    y_name = "sentence repetition"
  ),
  draw_combined_plot(
    df = df,
    group_name = "group",
    x = "AGL_offline",
    y = "sent_rep",
    x_name = "AGL offline",
    y_name = "sentence repetition"
  ),
  labels = c("A", "B", "C", "D"),
  ncol = 1,
  scale = 0.8,
  hjust = -5,
  label_size = 20
)

