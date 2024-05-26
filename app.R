library(shiny)
library(dplyr)

# UI
ui <- fluidPage(
  titlePanel("Literature Review"),
  sidebarLayout(
    sidebarPanel(
      selectInput("Topics", "Select Topics", choices = NULL, multiple = TRUE),
      selectInput("Categories", "Select Categories", choices = NULL, multiple = TRUE),
      selectInput("Main_Questions", "Select Main Question", choices = NULL, multiple = FALSE),
      selectInput("Related_Questions", "Select Related Question", choices = NULL, multiple = FALSE)
    ),
    mainPanel(
      textOutput("Answers_Main"),
      textOutput("Answers_Related")
    )
  )
)

# Server
server <- function(input, output, session) {
  # Read the data
  literature_data <- read.csv("Literature_QandA.csv", stringsAsFactors = FALSE)

  # Update the choices for the select inputs based on the csv data
  updateSelectInput(session, "Topics", choices = unique(literature_data$Topics))
  updateSelectInput(session, "Categories", choices = unique(literature_data$Categories))

  # Reactive expression to filter data based on user inputs
  filtered_data <- reactive({
    req(input$Topics, input$Categories)
    literature_data %>%
      filter(Topics %in% input$Topics & Categories %in% input$Categories)
  })

  # Observe the filtered data to update the Main_Questions choices
  observeEvent(filtered_data(), {
    updateSelectInput(session, "Main_Questions", choices = unique(filtered_data()$Main_Questions))
  })

  # Observe the Main_Questions to update the Related_Questions choices
  observeEvent(input$Main_Questions, {
    req(input$Main_Questions)
    related_data <- filtered_data() %>%
      filter(Main_Questions == input$Main_Questions)
    updateSelectInput(session, "Related_Questions", choices = unique(related_data$Related_Questions))
  })

  # Output for Answers_Main
  output$Answers_Main <- renderText({
    req(input$Main_Questions)
    data <- filtered_data() %>%
      filter(Main_Questions == input$Main_Questions)
    paste(data$Answers_Main, collapse = "\n")
  })

  # Output for Answers_Related
  output$Answers_Related <- renderText({
    req(input$Related_Questions)
    data <- filtered_data() %>%
      filter(Main_Questions == input$Main_Questions & Related_Questions == input$Related_Questions)
    paste(data$Answers_Related, collapse = "\n")
  })
}

# Run the application
shinyApp(ui = ui, server = server)
