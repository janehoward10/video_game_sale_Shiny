library(shiny)
library(tidyverse)


games <- read.csv("/Users/janehoward/Desktop/Case_Study_3_DS_501/vgsales.csv")

# cleaning data, removing na 
games_clean <- games %>%
  mutate( 
    Year     = as.numeric(Year),
    Genre    = as.factor(Genre),
    Platform = as.factor(Platform)
  ) %>%
  filter(
    !is.na(Year),
    !is.na(Global_Sales),
    !is.na(Genre),
    !is.na(Platform)
  )

model_data <- games_clean %>%
  select(Global_Sales, Year, Genre, Platform)

#starting the UI

ui <- fluidPage(
  titlePanel("Video Game Global Sales - Linear Regression"),
  
  sidebarLayout(
    sidebarPanel(
      h4("Model Settings"),
      
      sliderInput(
        "split", "Train/Test Split (% used for Training):",
        min = 50, max = 90, value = 70, step = 5
      ),
      
      hr(),
      h4("Game Characteristics for Prediction"),
      
      selectInput(
        "genre", "Genre:",
        choices = sort(unique(model_data$Genre))
      ),
      
      selectInput(
        "platform", "Platform:",
        choices = sort(unique(model_data$Platform))
      ),
      
      sliderInput(
        "year", "Year of Release:",
        min = min(model_data$Year, na.rm = TRUE),
        max = max(model_data$Year, na.rm = TRUE),
        value = 2010, step = 1
      )
    ),
    
    mainPanel(
      p("This app uses a linear regression model to predict global video game sales (in millions) using the video game's release year, genre, and platform."),
      p("The Video Games dataset comes from Kaggle and contains historical sales for 16,000 games across several platforms. 
        The model is trained on a random subset of the data, the training set, and evaluated on remaining games (test)."),
      p("Use the controls on the left to change the training split and select a genre, platform, and year. The app will then retrain the linear model, 
        compute the RMSE, and predict the global sales for a hypothetical game with the chosen characteristics from the left."),
      
      h3("Model Performance"),
      textOutput("rmseText"),
      
      h3("Predicted Global Sales for Selected Game"),
      textOutput("predictionText"),
      
      h3("Example Games (Matching Genre & Platform)"),
      tableOutput("exampleGames"), 
      
      h3("Linear Regression Summary"),
      verbatimTextOutput("coefText"),
      
      h3("Distribution of Global Sales"),
      plotOutput("salesPlot"),
      
      h3("Zoomed: Sales Under 2 Million"),
      plotOutput("salesPlotZoom")
    )
  )
)


server <- function(input, output) {
  lmResults <- reactive({
    df <- model_data
    
    # Here creating the train/test split 
    set.seed(123)
    n <- nrow(df)
    train_index <- sample(1:n, size = input$split / 100 * n)
    
    train_data <- df[train_index, ]
    test_data  <- df[-train_index, ]
    
    test_data <- test_data %>%
      filter(
        Platform %in% train_data$Platform,
        Genre %in% train_data$Genre
      )
    
    # I was experiencing warnings from factor levels that weren't being used, so dropping them 
    train_data <- droplevels(train_data)
    test_data  <- droplevels(test_data)
    
    # Here is wear the linear regression is being fit 
    lm_model <- lm(Global_Sales ~ Year + Genre + Platform, data = train_data)
    
    preds <- predict(lm_model, newdata = test_data)
    valid_idx <- !is.na(preds)
    preds  <- preds[valid_idx]
    actual <- test_data$Global_Sales[valid_idx]
    
    rmse <- sqrt(mean((actual - preds)^2))
    
    list(model = lm_model, rmse = rmse)
  })
  
 # output
  output$rmseText <- renderText({
    results <- lmResults()
    paste(
      "RMSE on test set:",
      round(results$rmse, 3),
      "million units"
    )
  })
  
  output$predictionText <- renderText({
    results <- lmResults()
    model   <- results$model
    
    new_game <- data.frame(
      Year     = as.numeric(input$year),
      Genre    = factor(input$genre,    levels = levels(model_data$Genre)),
      Platform = factor(input$platform, levels = levels(model_data$Platform))
    )
    
    pred <- predict(model, newdata = new_game)
    
    paste(
      "Predicted Global Sales:",
      round(pred, 2),
      "million units"
    )
  })
  
  output$exampleGames <- renderTable({
    games_clean %>%
      filter(
        Genre == input$genre,
        Platform == input$platform
      ) %>%
      select(Name, Year, Global_Sales) %>%
      arrange(desc(Global_Sales)) %>%
      head(5)
  })
  
  
  output$coefText <- renderPrint({
    results <- lmResults()
    summary(results$model)
  })
  
  output$salesPlot <- renderPlot({
    ggplot(model_data, aes(x = Global_Sales)) +
      geom_histogram(bins = 30) +
      labs(
        x = "Global Sales (millions)",
        y = "Number of Games",
        title = "Distribution of Global Video Game Sales"
      )
  })
  
  output$salesPlotZoom <- renderPlot({
    ggplot(model_data %>% filter(Global_Sales < 2), aes(x = Global_Sales)) +
      geom_histogram(bins = 30) +
      labs(
        x = "Global Sales (millions)",
        y = "Number of Games",
        title = "Global Sales Under 2 Million (Zoomed In)"
      )
  })
}

shinyApp(ui = ui, server = server)
