# Consumer Shopping Behavior and Spending Patterns
# Interactive Shiny Dashboard

library(shiny)

# Load the dataset
shopping_data <- read.csv("consumer_shopping_behavior.csv")

# User Interface
ui <- fluidPage(
  
  titlePanel("Consumer Shopping Behavior and Spending Patterns"),
  
  sidebarLayout(
    
    # Sidebar filters
    sidebarPanel(
      
      h4("Filter Consumer Data"),
      
      selectInput(
        "frequency",
        "Shopping Frequency:",
        choices = c("All", sort(unique(shopping_data$Shopping_Frequency))),
        selected = "All"
      ),
      
      selectInput(
        "method",
        "Preferred Shopping Method:",
        choices = c("All", sort(unique(shopping_data$Preferred_Shopping_Method))),
        selected = "All"
      ),
      
      selectInput(
        "category",
        "Product Category:",
        choices = c("All", sort(unique(shopping_data$Product_Category))),
        selected = "All"
      )
    ),
    
    # Main dashboard
    mainPanel(
      
      h3("Shopping Summary"),
      
      fluidRow(
        column(
          4,
          wellPanel(
            h4("Average Monthly Spending"),
            h3(textOutput("average_spending"))
          )
        ),
        
        column(
          4,
          wellPanel(
            h4("Average Monthly Income"),
            h3(textOutput("average_income"))
          )
        ),
        
        column(
          4,
          wellPanel(
            h4("Average Satisfaction"),
            h3(textOutput("average_satisfaction"))
          )
        )
      ),
      
      hr(),
      
      h3("Data Visualizations"),
      
      h4("1. Average Monthly Spending by Product Category"),
      plotOutput("category_plot"),
      
      hr(),
      
      h4("2. Monthly Income vs. Monthly Spending"),
      plotOutput("income_plot"),
      
      hr(),
      
      h4("3. Shopping Frequency Distribution"),
      plotOutput("frequency_plot"),
      
      hr(),
      
      h3("Consumer Data Table"),
      tableOutput("consumer_table")
    )
  )
)

# Server
server <- function(input, output) {
  
  # Filter the dataset based on user selections
  filtered_data <- reactive({
    
    data <- shopping_data
    
    if (input$frequency != "All") {
      data <- data[data$Shopping_Frequency == input$frequency, ]
    }
    
    if (input$method != "All") {
      data <- data[data$Preferred_Shopping_Method == input$method, ]
    }
    
    if (input$category != "All") {
      data <- data[data$Product_Category == input$category, ]
    }
    
    data
  })
  
  # Average monthly spending
  output$average_spending <- renderText({
    data <- filtered_data()
    
    if (nrow(data) == 0) {
      return("No data")
    }
    
    paste0("PHP ", format(round(mean(data$Monthly_Spending_PHP), 2),
                          big.mark = ",", nsmall = 2))
  })
  
  # Average monthly income
  output$average_income <- renderText({
    data <- filtered_data()
    
    if (nrow(data) == 0) {
      return("No data")
    }
    
    paste0("PHP ", format(round(mean(data$Monthly_Income_PHP), 2),
                          big.mark = ",", nsmall = 2))
  })
  
  # Average satisfaction rating
  output$average_satisfaction <- renderText({
    data <- filtered_data()
    
    if (nrow(data) == 0) {
      return("No data")
    }
    
    paste0(round(mean(data$Satisfaction_Rating), 2), " / 5")
  })
  
  # Graph 1: Average spending by product category
  output$category_plot <- renderPlot({
    data <- filtered_data()
    
    if (nrow(data) == 0) {
      plot.new()
      text(0.5, 0.5, "No data available for this selection")
    } else {
      spending <- aggregate(
        Monthly_Spending_PHP ~ Product_Category,
        data = data,
        FUN = mean
      )
      
      barplot(
        spending$Monthly_Spending_PHP,
        names.arg = spending$Product_Category,
        main = "Average Monthly Spending by Product Category",
        xlab = "Product Category",
        ylab = "Average Spending (PHP)",
        col = "skyblue",
        las = 2
      )
    }
  })
  
  # Graph 2: Monthly income vs. monthly spending
  output$income_plot <- renderPlot({
    data <- filtered_data()
    
    if (nrow(data) == 0) {
      plot.new()
      text(0.5, 0.5, "No data available for this selection")
    } else {
      plot(
        data$Monthly_Income_PHP,
        data$Monthly_Spending_PHP,
        main = "Monthly Income vs. Monthly Spending",
        xlab = "Monthly Income (PHP)",
        ylab = "Monthly Spending (PHP)",
        pch = 19,
        col = "blue"
      )
      
      if (nrow(data) >= 2 &&
          length(unique(data$Monthly_Income_PHP)) >= 2) {
        abline(
          lm(Monthly_Spending_PHP ~ Monthly_Income_PHP, data = data),
          col = "red",
          lwd = 2
        )
      }
    }
  })
  
  # Graph 3: Shopping frequency distribution
  output$frequency_plot <- renderPlot({
    data <- filtered_data()
    
    if (nrow(data) == 0) {
      plot.new()
      text(0.5, 0.5, "No data available for this selection")
    } else {
      frequency_counts <- table(data$Shopping_Frequency)
      
      barplot(
        frequency_counts,
        main = "Shopping Frequency Distribution",
        xlab = "Shopping Frequency",
        ylab = "Number of Consumers",
        col = "lightgreen",
        las = 2
      )
    }
  })
  
  # Consumer data table
  output$consumer_table <- renderTable({
    data <- filtered_data()
    
    if (nrow(data) == 0) {
      return(data.frame(Message = "No records match the selected filters."))
    }
    
    data[, c(
      "Consumer_ID",
      "Age",
      "Shopping_Frequency",
      "Preferred_Shopping_Method",
      "Product_Category",
      "Monthly_Spending_PHP",
      "Payment_Method",
      "Satisfaction_Rating"
    )]
  })
}

# Run the Shiny application
shinyApp(ui = ui, server = server)