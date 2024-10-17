library(patchwork)

d$group = factor(d$group, levels = c('TD', 'DLD', 'ASD', 'ADHD'))

plots = d |> 
  pivot_longer(-c(ID,age_years,group)) |> 
  nest(.by = name) |> 
  mutate(
    plot = map2(data, name, 
               ~ ggplot(.x, aes(group,value)) +
                 geom_violin() +
                 geom_boxplot(width = .1) +
                 theme_bw() +
                 ylab(.y)
    )
  )

wrap_plots(plots$plot)
ggsave('statok.png', width = 12, height = 9)

sums = d |> 
  pivot_longer(-c(ID,age_years,group)) |> 
  group_by(name,group) |> 
  summarise(
    q1 = quantile(value, 0.25, na.rm = TRUE),
    q2 = median(value, na.rm = TRUE),
    q3 = quantile(value, 0.75, na.rm = TRUE),
    mean = mean(value, na.rm = TRUE)
  ) |> 
  mutate(across(where(is.double), ~ round(.x, 2))) |> 
  googlesheets4::write_sheet('https://docs.google.com/spreadsheets/d/1BuVbSZOBIsTfTV2801K_-hxxIJdlGmaSeojz9Uh21uI/edit?usp=sharing', 'Sheet1')
