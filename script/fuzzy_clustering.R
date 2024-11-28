## ---------------------------
##
## title: cluster analysis of TD, ADHD, ASD and DLD children based on language and cognitive abilities
##
## description: this is just some fun with Fuzzy Clustering without any expertise in the methodology
##
## author: Krisztina Sára Lukics
##
## date: 2024-11-28
##
## ---------------------------

library(tidyverse)

df = read_csv("../data/df.csv")
dfs = df |> select(-c(ID, age_years, IQ, AFC_phr, AFC_sent, prod))
dfs = dfs |> select(-c(group)) |> purrr::map(scale) |> as.data.frame()
group = df$group
dfs = cbind(dfs, group)

imputed_data = mice::mice(dfs)
#summary(imputed_data)
#imputed_data$imp

dfsc = mice::complete(imputed_data)

fcm_result <- fclust::FKM(
  X = dfsc |> select(-group),
  k = 2,
  m = 2,
  maxit = 500
  )

fcm_result$U
fcm_result$H
fcm_result$clus
fcm_result$criterion

dfsc = cbind(dfsc, fcm_result$clus |> as.data.frame() |> select(Cluster))

groups = dfsc |>
  group_by(Cluster, group) |>
  count() |>
  arrange(Cluster, desc(n))
groups
