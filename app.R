# Load required libraries
packages_to_install <- c("shiny", "e1071", "plotly", "ggplot2", "caret", "class", "C50", "party", "partykit","rsconnect","forecast")

library(shiny)
library(e1071)
library(plotly)
library(ggplot2)
library(caret)
library(class)
library(C50)
library(party)
library(partykit)
library(rsconnect)
library(forecast)

# Define UI
ui <- fluidPage(
  tags$head(
    tags$meta(charset = "UTF-8")
  ),
  
  titlePanel("Heart Attack Risk Prediction"),
  
  tags$div(
    p("This Shiny app was developed by ",
      a("Ali Eren Kayhan", href = "https://www.linkedin.com/in/alierenkayhan/", target = "_blank"),
      " and ",
      a("Emre Y\u0131ld\u0131r\u0131m.", href = "https://www.linkedin.com/in/emreyildirim99/", target = "_blank"),
      "You can find more information by visiting ",
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
        tags$li(" Sozan S. Maghdid , Tarik A. Rashid. (2023).Heart Disease Classification Dataset (V2 ed.). https://www.kaggle.com/datasets/bharath011/heart-disease-classification-dataset/data"),
        tags$li("ggplot2: Hadley Wickham (2016). ggplot2: Elegant Graphics for Data Analysis. Springer-Verlag New York. ISBN 978-3-319-24277-4."),
        tags$li("shiny: Winston Chang, Joe Cheng, JJ Allaire, Yihui Xie and Jonathan McPherson (2021). shiny: Web Application Framework for R. R package version 1.6.0."),
        tags$li("e1071: David Meyer, Evgenia Dimitriadou, Kurt Hornik, Andreas Weingessel (2019). e1071: Misc Functions of the Department of Statistics, Probability Theory Group (Formerly: E1071), TU Wien. R package version 1.7-6."),
        tags$li("plotly: Carson Sievert (2021). plotly for R. R package version 4.10.0."),
        tags$li("caret: Max Kuhn (2021). caret: Classification and Regression Training. R package version 6.0-90."),
        tags$li("class: Original by Brian Ripley. Modernized and modified by Kurt Hornik & Albrecht Gebhardt (2019). class: Functions for Classification. R package version 7.3-15."),
        tags$li("C50: Original by Ross Quinlan. Modernized and maintained by Thomas B??hler, Torsten Hothorn, Kurt Hornik, Lars Kuhnt and Gero Szepannek. (2021). C50: C5.0 Decision Trees and Rule-Based Models. R package version 0.1.3."),
        tags$li("party: Torsten Hothorn, Kurt Hornik, Achim Zeileis (2006). Unbiased Recursive Partitioning: A Conditional Inference Framework. Journal of Computational and Graphical Statistics 15(3), 651-674. DOI 10.1198/106186006X133933."),
        tags$li("partykit: Torsten Hothorn, Achim Zeileis (2015). partykit: A Modular Toolkit for Recursive Partytioning in R. Journal of Machine Learning Research 16(1), 390-393."),
        tags$li("forecast: Hyndman, R. J., & Athanasopoulos, G. (2021). forecast: Forecasting functions for time series and linear models. R package version 8.15. R Foundation for Statistical Computing.")

       )
    ),
    div(
      style = "text-align: center; margin-top: 20px;",
      h6("This Shiny app was developed by ",
         a("Ali Eren Kayhan", href = "https://www.linkedin.com/in/alierenkayhan/", target = "_blank"),
         " and ",
         a("Emre Y\u0131ld\u0131r\u0131m", href = "https://www.linkedin.com/in/emreyildirim99/", target = "_blank"),
         "."),
      p("For more information, visit the ",
        a("GitHub repository", href = "https://github.com/Alierenkayhan/Heart_Attack_Risk_Prediction_Machine_Learning", target = "_blank"),
        "."
      )
    )
  )
)
 

# Define servers
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

# Run the Shiny appF
shinyApp(ui = ui, server = server)
