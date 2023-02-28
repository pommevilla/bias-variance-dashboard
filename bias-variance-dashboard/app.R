################ Setup

library(shiny)
library(tidyverse)

theme_set(theme_light() +
            theme(panel.grid = element_blank()))

################ Helpers

get_rmse <- function(trained_poly_model, df) {
  predictions <- predict(trained_poly_model, newdata = df)
  rmse <- sqrt(mean((predictions - df$y) ^ 2)) %>% round(2)
  
  return(rmse)
}

nice_expansion <- expansion(mult = c(0.1, 0.1), add = 0)



############### App
ui <- fluidPage(
  
  # Upper sliders
  fluidRow(
    align = "center",
    column(
      4,
      offset = 2,
      sliderInput(
      "poly_degree",
      "Degree of polynomial:",
      min = 1,
      max = 20,
      value = 1
      )
    ),
    column(
      4,
      sliderInput(
        "training_test_split",
        "Training set proportion",
        min = 0.15,
        max = 0.95,
        value = 0.7
      )
    ),
  ),
  
  fluidRow(
    align = "center",
    column(
      8,
      offset = 2,
      plotOutput("distPlot")
    )
  ),
  
  fluidRow(
    align = "center",
    column(
      2,
      offset = 3,
      textOutput("training_mse_out")
    ),
    column(
      2,
      textOutput("testing_mse_out")
    ),
    column(
      2,
      textOutput("mse_diff_out")
    )
  )
)

server <- function(input, output) {
  
  # Gather inputs
  training_set_proportion <- reactive(input$training_test_split %>% as.numeric())
  degree_selected <- reactive(input$poly_degree)
  
  # Create new data frame as necessary
  this_df <- reactive({
    set.seed(489)
    data.frame(x = runif(75, 1, 20)) %>%
      rowwise() %>%
      mutate(y = x ^ 2 / 10 + x * runif(1, 0, 25) ) %>% 
      ungroup() %>% 
      mutate(Set = if_else(
        row_number() < 75 * training_set_proportion(),
        "Training",
        "Testing"
      ))
  })
  

  training_set <- reactive(
    this_df() %>%
      filter(Set == "Training")
      # filter(row_number() <= num_samples() * training_set_proportion())
    )
  
  testing_set <- reactive(
    this_df() %>%
      filter(Set == "Testing")
      # filter(row_number() > num_samples() * training_set_proportion())
  )
  
  
  poly_model <-
    reactive(lm(y ~ poly(x, degree_selected(), raw = TRUE), data = training_set()))
  
  
  training_mse <- reactive(
    get_rmse(poly_model(), training_set()) 
  )
  testing_mse <- reactive(
    get_rmse(poly_model(), testing_set())
  )
  mse_dff <- reactive(training_mse() - testing_mse() %>% round(2))

  
  output$distPlot <- renderPlot({
    this_df() %>%
      # mutate(Set = if_else(
      #   row_number() > 75 * training_set_proportion(),
      #   "Testing",
      #   "Training"
      # )) %>% 
      ggplot(aes(x, y)) +
      geom_point(size = 4, shape = 21, aes(fill = Set)) +
      geom_smooth(
        data = training_set(),
        method = lm,
        formula = y ~ poly(x, degree_selected(), raw = TRUE),
        se = FALSE
      ) +
      theme(
        # axis.text = element_blank(),
        legend.position = c(0.1, 0.8),
        legend.background = element_rect(fill = "white", color = "gray90", size = 1),
        axis.ticks = element_blank()
      ) +
      labs(
        x = "",
        y = ""
      ) +
      scale_x_continuous(expand = nice_expansion) +
      scale_y_continuous(expand = nice_expansion) +
      coord_cartesian()
    
  })
  output$training_mse_out <- renderText(paste0("Training RMSE: ", training_mse()))
  output$testing_mse_out <- renderText(paste0("Testing RMSE: ", testing_mse()))
  output$mse_diff_out <- renderText(paste0("Difference: ", mse_dff()))
}

# Run the application
shinyApp(ui = ui, server = server)
