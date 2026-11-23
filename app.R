library(shiny)
library(tidyverse)
library(fpp3)

# 1. Load and prepare the wine data
wine_raw <- read_csv(
  "data/AustralianWines.csv",
  show_col_types = FALSE
)

wine_raw <- wine_raw |>
  rename_with(str_trim) |>
  mutate(
    across(
      -Month,
      ~ parse_number(as.character(.x))
    )
  )

wine_long <- wine_raw |>
  mutate(
    Month = yearmonth(
      as.Date(paste0("01-", Month), format = "%d-%b-%y")
    )
  ) |>
  pivot_longer(
    cols = -Month,
    names_to = "Varietal",
    values_to = "Sales"
  ) |>
  mutate(
    Varietal = str_to_title(str_trim(Varietal))
  ) |>
  arrange(Varietal, Month)

wine_ts <- wine_long |>
  as_tsibble(
    index = Month,
    key = Varietal
  )



# 2. Define the user interface
ui <- fluidPage(
  
  titlePanel("Australian Wine Forecasting Explorer"),
  
  sidebarLayout(
    
    sidebarPanel(
      
      h4("Historical Data Explorer"),
      
      selectizeInput(
        inputId = "varieties",
        label = "Select wine varieties:",
        choices = sort(unique(wine_ts$Varietal)),
        selected = "Fortified",
        multiple = TRUE,
        options = list(
          plugins = list("remove_button")
        )
      ),
      
      dateRangeInput(
        inputId = "date_range",
        label = "Select date range:",
        start = as.Date(min(wine_ts$Month)),
        end = as.Date(max(wine_ts$Month)),
        min = as.Date(min(wine_ts$Month)),
        max = as.Date(max(wine_ts$Month)),
        format = "yyyy-mm"
      ),
      
      hr(),
      
      h4("Forecasting Settings"),
      
      dateInput(
        inputId = "train_end",
        label = "Training period ends:",
        value = as.Date("1992-12-01"),
        min = as.Date("1980-01-01"),
        max = as.Date("1992-12-01"),
        format = "yyyy-mm"
      ),
      
      sliderInput(
        inputId = "horizon",
        label = "Forecast horizon (months):",
        min = 3,
        max = 24,
        value = 12,
        step = 1
      )
      
    ),
    
    mainPanel(
      
      tabsetPanel(
        
        # Tab 1: Explore and Forecast
        tabPanel(
          title = "Explore & Forecast",
          
          br(),
          
          h3("Historical Monthly Wine Sales"),
          
          plotOutput(
            outputId = "sales_plot",
            height = "650px"
          ),
          
          hr(),
          
          h3("Forecast Model Comparison"),
          
          h4("Validation Accuracy"),
          
          DT::DTOutput("accuracy_table"),
          
          br(),
          
          h4("Model Specifications"),
          
          uiOutput("model_specs"),
          
          br(),
          
          h4("Comparative Forecasts"),
          
          plotOutput(
            outputId = "forecast_plot",
            height = "600px"
          ),
          
          br(),
          
          h4("Current Forecast Settings"),
          
          textOutput("settings_summary")
        ),
        
        # Tab 2: About
        tabPanel(
          title = "About",
          
          br(),
          
          h3("About This Application"),
          
          p(
            "The Australian Wine Forecasting Explorer is an interactive ",
            "time series application designed to explore historical ",
            "monthly wine sales and compare forecasting models."
          ),
          
          h4("Dataset"),
          
          p(
            "The dataset contains monthly sales observations for six ",
            "Australian wine varieties from January 1980 through ",
            "December 1994."
          ),
          
          p(
            "The data are organized by month and wine variety, ",
            "allowing users to explore individual varieties or compare ",
            "multiple varieties simultaneously."
          ),
          
          h4("Forecasting Models"),
          
          tags$ul(
            tags$li(
              strong("TSLM: "),
              "A trend and seasonal linear model using time-based ",
              "and seasonal predictors."
            ),
            tags$li(
              strong("ETS: "),
              "An automatically selected exponential smoothing model."
            ),
            tags$li(
              strong("ARIMA: "),
              "An automatically selected autoregressive integrated ",
              "moving average model."
            )
          ),
          
          h4("Model Evaluation"),
          
          p(
            "The selected training cutoff determines which observations ",
            "are used to fit the models. The following months are used ",
            "for validation, based on the selected forecast horizon."
          ),
          
          p(
            "Model performance is compared using three accuracy ",
            "measures:"
          ),
          
          tags$ul(
            tags$li(
              strong("RMSE: "),
              "Root Mean Squared Error."
            ),
            tags$li(
              strong("MAE: "),
              "Mean Absolute Error."
            ),
            tags$li(
              strong("MAPE: "),
              "Mean Absolute Percentage Error."
            )
          ),
          
          p(
            "The model with the lowest validation RMSE is highlighted ",
            "for each selected wine variety."
          ),
          
          h4("How to Use the Application"),
          
          tags$ol(
            tags$li(
              "Select one or more wine varieties in the sidebar."
            ),
            tags$li(
              "Choose a date range to explore historical sales."
            ),
            tags$li(
              "Set the training period cutoff."
            ),
            tags$li(
              "Adjust the forecast horizon between 3 and 24 months."
            ),
            tags$li(
              "Review the validation accuracy table and comparative ",
              "forecast plots."
            )
          ),
          
          h4("Data Source"),
          
          p(
            "The application uses the AustralianWines.csv dataset ",
            "included with this project."
          )
        )
        
      )
    )
  )
)



# 3. Define the server
server <- function(input, output, session) {
  
  output$settings_summary <- renderText({
    
    paste(
      "Training ends:",
      format(input$train_end, "%B %Y"),
      "| Forecast horizon:",
      input$horizon,
      "months"
    )
    
  })
  
  selected_data <- reactive({
    
    validate(
      need(
        length(input$varieties) > 0,
        "Please select at least one wine variety."
      ),
      need(
        !is.null(input$date_range),
        "Please select a date range."
      )
    )
    
    wine_ts |>
      filter(
        Varietal %in% input$varieties,
        Month >= yearmonth(input$date_range[1]),
        Month <= yearmonth(input$date_range[2])
      )
  })
  
  
  # Fit the three forecasting models and evaluate them
  model_results <- reactive({
    
    req(input$varieties, input$train_end, input$horizon)
    
    train_end <- yearmonth(input$train_end)
    
    # Training observations
    wine_train <- wine_ts |>
      filter(
        Varietal %in% input$varieties,
        Month <= train_end
      )
    
    # Validation observations
    wine_test <- wine_ts |>
      filter(
        Varietal %in% input$varieties,
        Month > train_end,
        Month <= train_end + input$horizon
      )
    
    validate(
      need(
        nrow(wine_train) > 0,
        "No training observations are available."
      ),
      need(
        nrow(wine_test) > 0,
        "No validation observations are available. Choose an earlier training cutoff."
      )
    )
    
    # Fit TSLM, ETS, and ARIMA
    wine_models <- wine_train |>
      model(
        TSLM = TSLM(Sales ~ trend() + season()),
        ETS = ETS(Sales),
        ARIMA = ARIMA(Sales)
      )
    
    # Generate validation forecasts
    wine_forecasts <- wine_models |>
      forecast(h = input$horizon)
    
    # Calculate validation accuracy
    wine_accuracy <- accuracy(wine_forecasts, wine_test) |>
      select(Varietal, .model, RMSE, MAE, MAPE) |>
      group_by(Varietal) |>
      mutate(
        Best = if_else(
          RMSE == min(RMSE, na.rm = TRUE),
          "Best RMSE",
          ""
        )
      ) |>
      ungroup() |>
      arrange(Varietal, RMSE)
    
    list(
      models = wine_models,
      forecasts = wine_forecasts,
      accuracy = wine_accuracy,
      training = wine_train,
      validation = wine_test
    )
  })
  
  # Display validation accuracy
  output$accuracy_table <- DT::renderDT({
    DT::datatable(
      model_results()$accuracy,
      rownames = FALSE,
      options = list(
        dom = "t",
        ordering = FALSE
      )
    ) |>
      DT::formatStyle(
        "Best",
        target = "row",
        backgroundColor = DT::styleEqual(
          "Best RMSE",
          "#dff0d8"
        ),
        fontWeight = DT::styleEqual(
          "Best RMSE",
          "bold"
        )
      )
  })
  
  # Display model specifications
  output$model_specs <- renderUI({
    tagList(
      tags$p(
        tags$b("TSLM: "),
        "Sales ~ trend() + season()"
      ),
      tags$p(
        tags$b("ETS: "),
        "Automatically selected exponential smoothing model"
      ),
      tags$p(
        tags$b("ARIMA: "),
        "Automatically selected ARIMA model"
      )
    )
  })
  
  # Display comparative forecasts
  output$forecast_plot <- renderPlot({
    
    results <- model_results()
    
    autoplot(
      results$forecasts,
      results$training
    ) +
      autolayer(
        results$validation,
        Sales,
        colour = "black",
        linewidth = 0.8
      ) +
      facet_wrap(
        ~Varietal,
        scales = "free_y",
        ncol = 1
      ) +
      labs(
        title = "Forecast Model Comparison",
        subtitle = "TSLM, ETS, and ARIMA with actual validation observations",
        x = "Year",
        y = "Monthly Sales",
        colour = "Forecast Model"
      ) +
      theme_minimal()
    
  })
  
  output$sales_plot <- renderPlot({
    
    plot_data <- selected_data()
    
    validate(
      need(
        nrow(plot_data) > 0,
        "No observations are available for this selection."
      )
    )
    
    plot_data |>
      ggplot(aes(x = Month, y = Sales, color = Varietal, group = Varietal)) +
      geom_line(na.rm = TRUE, linewidth = 0.7) +
      facet_wrap(
        ~Varietal,
        scales = "free_y",
        ncol = 2
      ) +
      scale_x_yearmonth(
        date_breaks = "5 years",
        date_labels = "%Y"

      ) +
      labs(
        title = "Australian Monthly Wine Sales",
        subtitle = "Historical sales by selected wine variety",
        x = "Year",
        y = "Monthly Sales"
      ) +
      theme_minimal() +
      theme(
        legend.position = "none",
        strip.text = element_text(face = "bold")
      )
    
  })
  
}

# 4. Launch the application
shinyApp(ui = ui, server = server)
