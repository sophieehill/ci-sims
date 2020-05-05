library(shiny)
library(ggplot2)
library(tidyverse)
library(devtools)

# setwd("~/Google Drive/Harvard/Shiny/ci-sims")


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
               h4('Controls'),
               sliderInput("sample_size",
                           "Sample size:",
                           min = 10,
                           max = 1000,
                           step = 10,
                           value = 100),
               sliderInput("n_samples",
                           "Number of samples:",
                           min = 0,
                           max = 500,
                           step = 10,
                           value = 10),
               sliderInput("conf_level",
                           "Confidence level:",
                           min = 5,
                           max = 99,
                           step = 1,
                           value = 95),
               sliderInput("pop_prop",
                           "True population value:",
                           min = 0,
                           max = 1,
                           step = 0.05,
                           value = 0.4),
               actionButton("refresh_button", "Refresh"),
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
        # set.seed(123)
        input$refresh_button # This makes the renderPlot depend on the go_button
        
        
        n_samples = input$n_samples
        sample_size = input$sample_size
        alpha = 1 - (input$conf_level / 100)
        
        # create a synthetic population 
        n_population <- 100000
        population <- rbernoulli(n_population, p = input$pop_prop)
        true_proportion <- mean(population)
        
        
        sample_props <- vector('numeric', n_samples)
        upper_cis <- vector('numeric', n_samples)
        lower_cis <- vector('numeric', n_samples)
        contains_true_prop <- vector('numeric', n_samples)
        for (i in 1:n_samples) {
            # sample from the population 
            sample <- sample(population, sample_size, replace = FALSE)
            
            # sample proportion 
            sample_prop <- mean(sample)
            
            # margin of error
            sample_std <- (sqrt(sample_prop * (1 - sample_prop)) / sqrt(sample_size))
            z_value <- qnorm(alpha/2, lower.tail = FALSE)
            margin_of_error <- z_value * sample_std
            
            # store values 
            sample_props[i] <- sample_prop
            upper_cis[i] <- sample_prop + margin_of_error
            lower_cis[i] <- sample_prop - margin_of_error
            contains_true_prop[i] <- ifelse(true_proportion >= lower_cis[i] & true_proportion <= upper_cis[i], 1,0)
        }

        confidence_text <- paste0(input$alpha, "%")
        title_text <- paste(confidence_text, "Confidence Intervals", sep = " ")
        subtitle_text <- paste0('Observed coverage: ',
                                round(mean(contains_true_prop), 2) * 100, '%')
        
        contains_true_prop_f <- ifelse(contains_true_prop==1, "true", "false")
        contains_true_prop_f <- factor(contains_true_prop_f)
        
        p <- tibble(
            sample_prop = sample_props,
            upper_ci = upper_cis,
            lower_ci = lower_cis) %>%
            mutate(sample_number = as.factor(row_number())) %>%
            ggplot(aes(x = sample_number, y = sample_prop)) +
            scale_y_continuous(limits = c(0, 1)) +
            coord_flip() +
            ggtitle(title_text, subtitle = subtitle_text) +
            ylab('Sample Proportion') + 
            xlab("") + 
            theme_minimal() +
            theme(axis.text.y = element_blank(),
                  axis.ticks.y = element_blank(),
                  axis.line.x = element_line(color="black", size = 0.5),
                  legend.position = "none") +
            scale_x_discrete(breaks = NULL) 
        
        p <- p + geom_hline(aes(yintercept = true_proportion),
                            linetype = 'dashed', size = 1)
        p <- p +
            geom_linerange(aes(ymin = lower_ci, ymax = upper_ci, color=contains_true_prop_f)) +
            geom_point(aes(color=contains_true_prop_f), size=2) +
            scale_color_manual(values=c("true" = "#009E73", "false" = "#D55E00"))
        
        
        p
    })
    }


# Run the application
shinyApp(ui = ui, server = server)

