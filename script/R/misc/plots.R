library(tidyverse)
library(ggpubr)

# load data
data = read_csv("C:/Users/krisztinasara/github/developmental_disorders_SL/data/dem_groups_res.csv")

# make group a factor with this order: TD, ADHD, ASD, DLD
data$group = factor(data$group, levels = c("TD", "ADHD", "ASD", "DLD"))

# plot age from age_mean and age_SD (as error bars) by group from data with stat = identity
age = ggplot(data, aes(x = group, y = age_mean, ymin = age_mean - age_SD, ymax = age_mean + age_SD)) +
  geom_pointrange() +
  theme_minimal() +
  labs(y = "Age") +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank())

# plot IQ from IQ_mean and IQ_SD (as error bars) by group from data with stat = identity
IQ = ggplot(data, aes(x = group, y = IQ_mean, ymin = IQ_mean - IQ_SD, ymax = IQ_mean + IQ_SD)) +
  geom_pointrange() +
  theme_minimal() +
  labs(y = "IQ") +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank())

# make a new long df with group and n_males and n_females
sex = data |>
  select(group, n_males, n_females) |>
  pivot_longer(cols = c(n_males, n_females)) |>
  ggplot(aes(x = group, y = value, fill = name)) +
  geom_bar(stat = "identity", position = "stack") +
  theme_minimal() +
  labs(x = "Group", y = "Group size and sex ratio") +
  theme(legend.position = "none")

# combine plots
ggarrange(age, IQ, sex, nrows = 3, ncol = 1)
