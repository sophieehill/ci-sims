library(shiny)
library(ggplot2)
library(tidyverse)
library(devtools)

# Note: this code is adapted from Joshua Loyal's shiny app:
# https://github.com/joshloyal/confidence-interval-app/blob/master/app.R

# Main changes:
# default to show true population proportion and confidence intervals
# allow user to vary the true population value
# squash graph to enable the shiny app to display correctly on a slide
# change terminology (experiments / samples) to (samples / sample size)


# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  title = "Confidence Intervals",
  
  fluidRow(
    column(3,
           h4('Experiment Controls'),
           sliderInput("sample_size",
                       "Sample size:",
                       min = 10,
                       max = 1000,
                       step = 10,
                       value = 100),
           sliderInput("n_samples",
                       "Number of samples:",
                       min = 10,
                       max = 100,
                       step = 10,
                       value = 10),
           sliderInput("alpha",
                       "Confidence level:",
                       min = 5,
                       max = 100,
                       step = 5,
                       value = 95),
           sliderInput("pop_prop",
                       "True population value:",
                       min = 0,
                       max = 1,
                       step = 0.1,
                       value = 0.4),
           hr(),
           br(),
           p('Github Repo: ', a('sophieehill/ci-sims',
                                href='https://github.com/sophieehill/ci-sims')
           )
    ),
    column(8,
           plotOutput('IntervalPlot', height = '500px')
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  output$IntervalPlot <- renderPlot({
    set.seed(123)
    
    n_samples = input$n_samples
    sample_size = input$sample_size
    alpha = 1 - (input$alpha / 100)
    
    # create a synthetic population 
    n_population <- 100000
    population <- rbernoulli(n_population, p = input$pop_prop)
    true_proportion <- mean(population)
    
    
    sample_props <- vector('numeric', n_samples)
    upper_cis <- vector('numeric', n_samples)
    lower_cis <- vector('numeric', n_samples)
    contains_true_prop <- 0
    for (i in 1:n_samples) {
      # sample from the population with replacement.
      sample <- sample(population, sample_size, replace = FALSE)
      
      # sample proportion 
      sample_prop <- mean(sample)
      
      # margin of error
      sample_std <- (sqrt(sample_prop * (1 - sample_prop)) / sqrt(n_samples))
      z_value <- qnorm(alpha/2, lower.tail = FALSE)
      margin_of_error <- z_value * sample_std
      
      # store values 
      sample_props[i] <- sample_prop
      upper_cis[i] <- sample_prop + margin_of_error
      lower_cis[i] <- sample_prop - margin_of_error
      if (true_proportion >= lower_cis[i] && true_proportion <= upper_cis[i]) {
        contains_true_prop <- contains_true_prop + (1/n_samples)
      }
    }
    
    confidence_text <- paste0(input$alpha, "%")
    title_text <- paste(confidence_text, "Confidence Intervals", sep = " ")
    sample_text <- paste0("Number of samples: ", input$n_samples)
    subtitle_text <- paste0(sample_text, '\n', 'Observed coverage: ',
                              round(contains_true_prop, 2) * 100, '%')

    p <- tibble(
      sample_prop = sample_props,
      upper_ci = upper_cis,
      lower_ci = lower_cis) %>%
      mutate(experiment_number = as.factor(row_number())) %>%
      ggplot(aes(x = experiment_number, y = sample_prop)) +
      geom_point(size = 3, color = 'steelblue') +
      scale_y_continuous(limits = c(0, 1)) +
      coord_flip() +
      ggtitle(title_text, subtitle = subtitle_text) +
      ylab('Sample Proportion') + 
      xlab('Experiment Number') + 
      theme_minimal() +
      theme(axis.text.y = element_blank(),
            axis.ticks.y = element_blank(),
            axis.line.y = element_blank())
    
    
      p <- p + geom_hline(aes(yintercept = true_proportion),
                          linetype = 'dashed', size = 1)
    
    
      p <- p +
        geom_linerange(aes(ymin = lower_ci, ymax = upper_ci)) +
        geom_point(size = 3, color = 'steelblue')
      
  
    p
  })
}

# Run the application
shinyApp(ui = ui, server = server)
