library(shiny)
library(tidyverse)
library(patchwork)
library(ggthemes)

# -- names -- #

my_labeller <- as_labeller(
  c(
    "TD" = 'Typically Developing',
    "DLD" = 'Developmental\nLanguage Disorder',
    "ADHD" = 'Attention-Deficit/\nHyperactivity Disorder',
    "ASD" = 'Autism Spectrum\nDisorder'
  )
)

long_names <- c(
  `ID` = "Id",
  `group` = "Diagnosis",
  `age_years` = "Age in years",
  `expr_vocab` = "Expressive vocabulary",
  `sent_rep` = 'Sentence repetition',
  `IQ` = "Intelligence Quotient",
  `AGL_medRT_diff` = "Artificial Grammar Learning\nmedian RT difference",
  `AGL_offline` = "Artificial Grammar Learning\noffline",
  `digit_span_forward` = "Forward digit span\n(jittered)",
  `digit_span_backward` = "Backward digit span\n(jittered)",
  `PS_vis_RT_med` = "Processing speed\nvisual median RT",
  `PS_ac_RT_med` = "Processing Speed\nacoustic median RT",
  `n_back_2_mean_score` = "N-back 2\nmean score",
  `group_TD` = 'Typically developing',
  `group_DLD` = 'Developmental\nLanguage Disorder',
  `group_ADHD` = 'Attention-Deficit/\nHyperactivity Disorder',
  `group_ASD` = 'Autism Spectrum\nDisorder',
  `sent_rep_pred` = "Predicted\nsentence repetition",
  `expr_vocab_pred` = "Predicted\nexpressive vocabulary"
)

# -- function -- #

draw_raw_combined <- function(dat, my_var_label, y_var = c("sent_rep", "expr_vocab")) {
  y_var <- match.arg(y_var)
  # get variable name from label
  my_var <- names(long_names)[long_names == my_var_label][1]
  my_name <- long_names[my_var]
  
  y_labels <- c(
    sent_rep = "Sentence repetition",
    expr_vocab = "Expressive vocabulary"
  )
  y_lims <- list(
    sent_rep = c(0, 1.3),
    expr_vocab = c(0.25, 1)
  )
  
  dat |> 
    mutate(group = fct_relevel(group, 'TD')) |> 
    ggplot(aes(x = !!sym(my_var), y = !!sym(y_var))) +
    geom_point(alpha = .25) +
    # geom_density_2d(colour = 'lightgrey') +
    geom_smooth(method = "gam", formula = y ~ s(x, k = 3)) +
    # geom_rug() +
    theme_few() +
    facet_wrap( ~ group, nrow = 1, labeller = my_labeller) +
    xlab(my_name) +
    ylab(y_labels[[y_var]]) +
    ylim(y_lims[[y_var]][1], y_lims[[y_var]][2])
}

# -- read data -- #

d <- read_csv('data/df.csv')

# -- shiny app -- #

ui <- fluidPage(
  titlePanel("Developmental Disorders Plot Explorer"),
  sidebarLayout(
    sidebarPanel(
      selectInput("my_var_label", "Select X variable:",
                  choices = setNames(long_names[!(names(long_names) %in% c("ID", "group", "sent_rep", "expr_vocab", "sent_rep_pred", "expr_vocab_pred", "group_TD", "group_DLD", "group_ADHD", "group_ASD"))], 
                                     long_names[!(names(long_names) %in% c("ID", "group", "sent_rep", "expr_vocab", "sent_rep_pred", "expr_vocab_pred", "group_TD", "group_DLD", "group_ADHD", "group_ASD"))]),
                  selected = long_names["age_years"]),
      selectInput("y_var", "Select Y variable:",
                  choices = c("Sentence repetition" = "sent_rep", "Expressive vocabulary" = "expr_vocab"),
                  selected = "sent_rep")
    ),
    mainPanel(
      plotOutput("main_plot", height = "600px")
    )
  )
)

server <- function(input, output, session) {
  output$main_plot <- renderPlot({
    req(input$my_var_label, input$y_var)
    draw_raw_combined(dat = d, my_var_label = input$my_var_label, y_var = input$y_var)
  })
}

shinyApp(ui, server)