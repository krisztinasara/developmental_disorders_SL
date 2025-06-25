library(tidyverse)
library(patchwork)
library(ggthemes)

# load data
dem = read_csv("C:/Users/krisztinasara/github/developmental_disorders_SL/data/dem_groups_res.csv")
df = read_csv("C:/Users/krisztinasara/github/developmental_disorders_SL/data/df.csv")

# make group a factor with this order: TD, ADHD, ASD, DLD
dem$group = factor(dem$group, levels = c("TD", "ADHD", "ASD", "DLD"))

# plot age from age_mean and age_SD (as error bars) by group from data with stat = identity
age = ggplot(dem, aes(x = group, y = age_mean, ymin = age_mean - age_SD, ymax = age_mean + age_SD)) +
  geom_pointrange() +
  theme_minimal() +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank()) +
  ylab("Age in years") +
  ggtitle("Age across neurodevelopmental groups")

# plot IQ from IQ_mean and IQ_SD (as error bars) by group from data with stat = identity
IQ = ggplot(dem, aes(x = group, y = IQ_mean, ymin = IQ_mean - IQ_SD, ymax = IQ_mean + IQ_SD)) +
  geom_pointrange() +
  theme_minimal() +
  theme_minimal() +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank()) +
  ylab("Raven’s Colored\nProgressive Matrices\nscore") +
  ggtitle("IQ across neurodevelopmental groups")

# make a new long df with group and n_males and n_females
sex = dem |>
  select(group, boys, girls) |>
  pivot_longer(cols = c(boys, girls)) |>
  ggplot(aes(x = group, y = value, fill = name)) +
  geom_bar(stat = "identity", position = "stack") +
  theme_minimal() +
  #theme(legend.position = "none") +
  theme(
    axis.title.x = element_blank(),
    legend.title = element_blank()
    ) +
  ylab("Number of\nparticipants") +
  ggtitle("Number of participants across\nneurodevelopmental groups") +
  scale_fill_viridis_d(option = 'cividis')

# combine plots
age + IQ + sex + plot_layout(ncol = 1, axis_titles = 'collect')

# save the plot
ggsave('C:/Users/krisztinasara/github/developmental_disorders_SL/viz/demography.png', dpi = 900, width = 5, height = 5, bg = 'white')
