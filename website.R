# Load required libraries
install.packages("shiny")
install.packages("randomForest")
install.packages("plotly")

library(shiny)
library(randomForest)
library(plotly)

# Define UI
ui <- fluidPage(
  titlePanel("Heart Attack Risk Prediction"),
  
  tags$div(
    p("This Shiny app was developed by Ali Eren Kayhan and Emre Y??ld??r??m. You can find more information by visiting  ",
      a("project GitHub Wiki", href = "https://github.com/Alierenkayhan/Heart_Attack_Risk_Prediction_Machine_Learning/wiki", target = "_blank"),
      "."
    )
  ),
  
  sidebarLayout(
    sidebarPanel(
      h4("Patient Information"),
      numericInput("age", "Age:", value = 50, min = 1),
      selectInput("sex", "Gender:", c("Male", "Female")),
      numericInput("cholesterol", "Cholesterol:", value = 200, min = 50),
      numericInput("blood_pressure", "Blood Pressure:", value = 120, min = 80),
      numericInput("heart_rate", "Heart Rate:", value = 70, min = 40),
      selectInput("diabetes", "Diabetes:", c("No", "Yes")),
      selectInput("family_history", "Family History:", c("No", "Yes")),
      selectInput("smoking", "Smoking:", c("Non-smoker", "Smoker")),
      selectInput("obesity", "Obesity:", c("Not obese", "Obese")),
      selectInput("alcohol_consumption", "Alcohol Consumption:", c("None", "Light", "Moderate", "Heavy")),
      numericInput("exercise_hours", "Exercise Hours per Week:", value = 3, min = 0),
      selectInput("diet", "Diet:", c("Healthy", "Average", "Unhealthy")),
      selectInput("previous_heart_problems", "Previous Heart Problems:", c("No", "Yes")),
      selectInput("medication_use", "Medication Use:", c("No", "Yes")),
      numericInput("stress_level", "Stress Level (1-10):", value = 5, min = 1, max = 10),
      numericInput("sedentary_hours", "Sedentary Hours per Day:", value = 8, min = 0),
      numericInput("income", "Income Level:", value = 50000, min = 0),
      numericInput("bmi", "Body Mass Index (BMI):", value = 25, min = 10),
      numericInput("triglycerides", "Triglycerides:", value = 150, min = 50),
      numericInput("physical_activity_days", "Physical Activity Days per Week:", value = 3, min = 0),
      numericInput("sleep_hours", "Sleep Hours per Day:", value = 7, min = 1),
      selectInput("country", "Country:", c("USA", "Canada", "UK", "Other")),
      actionButton("predict_button", "Predict Heart Attack Risk", class = "btn-primary")
    ),
    
    mainPanel(
      textOutput("prediction_text"),
      plotlyOutput("scatter_plot"),
    
    )
  ),
  
  tags$footer(
    tags$div(
      h4("References"),
      tags$ul(
        tags$li("Deneme")
      )
    ),
    div(
      style = "text-align: center; margin-top: 20px;",
      h6("This Shiny app was developed by Ali Eren Kayhan and Emre Y??ld??r??m."),
      p("For more information, visit the ",
        a("GitHub repository", href = "https://github.com/Alierenkayhan/Heart_Attack_Risk_Prediction_Machine_Learning", target = "_blank"),
        "."
      )
    )
  )
)

# Define server
server <- function(input, output) {
  observeEvent(input$predict_button, {
    # Create a data frame with input values
    input_data <- data.frame(
      Age = input$age,
      Sex = ifelse(input$sex == "Male", 1, 0),
      Cholesterol = input$cholesterol,
      Blood_Pressure = input$blood_pressure,
      Heart_Rate = input$heart_rate,
      Diabetes = ifelse(input$diabetes == "Yes", 1, 0),
      Family_History = ifelse(input$family_history == "Yes", 1, 0),
      Smoking = ifelse(input$smoking == "Smoker", 1, 0),
      Obesity = ifelse(input$obesity == "Obese", 1, 0),
      Alcohol_Consumption = input$alcohol_consumption,
      Exercise_Hours_Per_Week = input$exercise_hours,
      Diet = input$diet,
      Previous_Heart_Problems = ifelse(input$previous_heart_problems == "Yes", 1, 0),
      Medication_Use = ifelse(input$medication_use == "Yes", 1, 0),
      Stress_Level = input$stress_level,
      Sedentary_Hours_Per_Day = input$sedentary_hours,
      Income = input$income,
      BMI = input$bmi,
      Triglycerides = input$triglycerides,
      Physical_Activity_Days_Per_Week = input$physical_activity_days,
      Sleep_Hours_Per_Day = input$sleep_hours,
      Country = input$country
    )
    
    # Load the trained model (replace "your_model_file.RDS" with your actual model file)
    loaded_model <- readRDS("your_model_file.RDS")
    
    # Predict heart attack risk
    prediction <- predict(loaded_model, newdata = input_data)
    
    # Display the prediction
    output$prediction_text <- renderText({
      if (prediction == 1) {
        "High risk of heart attack!"
      } else {
        "Low risk of heart attack."
      }
    })
    
    # Create a scatter plot with age and cholesterol
    output$scatter_plot <- renderPlotly({
      plot_data <- data.frame(Age = input$age, Cholesterol = input$cholesterol)
      plot_ly(data = plot_data, x = ~Age, y = ~Cholesterol, type = "scatter", mode = "markers") %>%
        layout(title = "Age vs Cholesterol",
               xaxis = list(title = "Age"),
               yaxis = list(title = "Cholesterol"))
    })
  })
}

# Run the Shiny app
shinyApp(ui = ui, server = server)
