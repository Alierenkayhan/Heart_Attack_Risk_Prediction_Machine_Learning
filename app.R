# Load required libraries
install.packages(c("shiny", "e1071", "plotly"))

library(shiny)
library(e1071)
library(plotly)

# Define UI
ui <- fluidPage(
  tags$head(
    tags$meta(charset = "UTF-8")
  ),
  
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
      selectInput("gender", "Gender:", c("Male", "Female")),
      numericInput("impluse", "Impulse:", value = 70, min = 40),
      numericInput("pressurehight", "Blood Pressure (High):", value = 120, min = 80),
      numericInput("pressurelow", "Blood Pressure (Low):", value = 80, min = 40),
      numericInput("glucose", "Glucose:", value = 100, min = 50),
      numericInput("kcm", "KCM:", value = 2, min = 1),
      numericInput("troponin", "Troponin:", value = 0.01, min = 0.001),
      actionButton("predict_button", "Predict Heart Attack Risk", class = "btn-primary")
    ),
    
    mainPanel(
      div(
        id = "prediction_text_container",
        h2("Predictions:"),
        uiOutput("prediction_text"),
        style = "margin-bottom: 20px; text-align: left; font-family: 'Arial', sans-serif; font-size: 16px;"
      ),
      plotlyOutput("scatter_plot_features")      
    )
  ),
  
  tags$footer(
    tags$div(
      h4("References"),
      tags$ul(
        tags$li(" The dataset is obtained from https://www.kaggle.com/datasets/bharath011/heart-disease-classification-dataset/data on December 27, 2023."),
        tags$li(" Sozan S. Maghdid , Tarik A. Rashid. (2023).Heart Disease Classification Dataset (V2 ed.). https://www.kaggle.com/datasets/bharath011/heart-disease-classification-dataset/data")
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
  loaded_naive_bayes_model <- readRDS("Models/naive_bayes_model.RDS")
  loaded_knn_model <- readRDS("Models/knn_model.RDS")
  loaded_decisionTree_model <- readRDS("Models/decisionTree_model.RDS")
  
  observeEvent(input$predict_button, {
    # Create a data frame with input values
    input_data <- data.frame(
      age = as.numeric(input$age),
      gender = as.numeric(ifelse(input$gender == "Male", 1, 0)),
      impluse = as.numeric(input$impluse),
      pressurehight = as.numeric(input$pressurehight),
      pressurelow = as.numeric(input$pressurelow),
      glucose = as.numeric(input$glucose),
      kcm = as.numeric(input$kcm),
      troponin = as.numeric(input$troponin)
    )
    
    # Predict using Naive Bayes
    naive_bayes_result <- as.character(predict(loaded_naive_bayes_model, newdata = input_data))
    
    # Predict using k-NN
    knn_result <- as.character(predict(loaded_naive_bayes_model, newdata = input_data))
    
    
    # Predict using decisionTree
    decisionTree_result <- as.character(predict(loaded_decisionTree_model, newdata = input_data))
    
    
    # Display the predictions
    output$prediction_text <- renderUI({
      predictions_list <- list(
        Naive_Bayes = switch(
          naive_bayes_result,
          "negative" = paste("<span style='color: green; font-weight: bold;'>Low risk of heart attack</span>"),
          "positive" = paste("<span style='color: red; font-weight: bold;'>High risk of heart attack.</span>"),
          "Unknown result." = "Unknown result."
        ),
        kNN = switch(
          knn_result,
          "negative" = paste("<span style='color: green; font-weight: bold;'>Low risk of heart attack</span>"),
          "positive" = paste("<span style='color: red; font-weight: bold;'>High risk of heart attack.</span>"),
          "Unknown result." = "Unknown result."
        ),
        Decision_Tree = switch(
          decisionTree_result,
          "negative" = paste("<span style='color: green; font-weight: bold;'>Low risk of heart attack</span>"),
          "positive" = paste("<span style='color: red; font-weight: bold;'>High risk of heart attack.</span>"),
          "Unknown result." = "Unknown result."
        )
      )
      
      HTML(paste(
        "<ul>",
        paste("<li>Naive Bayes Prediction: ", predictions_list$Naive_Bayes, "</li>"),
        paste("<li>k-NN Prediction: ", predictions_list$kNN, "</li>"),
        paste("<li>Decision Tree Prediction: ", predictions_list$Decision_Tree, "</li>"),
        "</ul>"
      ))
    })
    
    
    # Create a scatter plot
    output$scatter_plot_features <- renderPlotly({
      scatter_plot_data <- data.frame(
        Age = input_data$age,
        Troponin = input_data$troponin
      )
      scatter_plot <- plot_ly(data = scatter_plot_data, x = ~Age, y = ~Troponin, type = "scatter", mode = "markers", alpha = 0.5)
      
       scatter_plot <- scatter_plot %>% 
        layout(
          title = "Scatter Plot of Age vs Troponin",
          xaxis = list(title = "Age"),
          yaxis = list(title = "Troponin")
        )
      
      return(scatter_plot)
    })
  })
}

# Run the Shiny app
shinyApp(ui = ui, server = server)
