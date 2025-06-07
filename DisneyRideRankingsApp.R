library(shiny)
library(shinydashboard)
library(tidyverse)

rides <- read.csv("https://raw.githubusercontent.com/TheWaitTimes/ride_rankings/refs/heads/main/ridenames.csv")


# UI
ui <- dashboardPage(
  dashboardHeader(title = "Disney Rides Ranking Quiz"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Quiz", tabName = "quiz", icon = icon("star")),
      menuItem("Instructions", tabName = "instructions", icon = icon("info-circle"))
    ),
    br(),
    selectInput(
      "park_filter",
      "Filter by Park Location:",
      choices = c("All", unique(rides$Park_location)),
      selected = "All"
    ),
    actionButton("reset", "Reset Quiz")
  ),
  dashboardBody(
    tabItems(
      tabItem(
        tabName = "instructions",
        h3("Instructions"),
        p("Choose which Ride you prefer in each pair. The app keeps track of your choices and shows your personalized ranking at the end!"),
        p("Use the sidebar to filter rides by park and reset the quiz.")
      ),
      tabItem(
        tabName = "quiz",
        fluidRow(
          box(width = 12, uiOutput("quiz_ui"))
        ),
        fluidRow(
          box(width = 12, uiOutput("ranking_section"))
        )
      )
    )
  )
)

# Server logic
server <- function(input, output, session) {
  # Reactive rides data based on park filter
  filtered_rides <- reactive({
    if (is.null(input$park_filter) || input$park_filter == "All") {
      rides
    } else {
      rides %>% filter(Park_location == input$park_filter)
    }
  })
  
  # Shuffle pairs at the start and on reset
  pairs <- reactiveVal(NULL)
  choices <- reactiveVal(NULL)
  pair_idx <- reactiveVal(1)
  
  # Reset logic, also triggers when park_filter changes
  observeEvent(list(input$reset, input$park_filter), {
    df <- filtered_rides()
    n <- nrow(df)
    if (n < 2) {
      pairs(NULL)
      choices(NULL)
      pair_idx(1)
      return()
    }
    all_pairs <- t(combn(n, 2))
    all_pairs <- all_pairs[sample(nrow(all_pairs)), , drop=FALSE]
    pairs(all_pairs)
    choices(rep(NA, nrow(all_pairs)))
    pair_idx(1)
  }, priority = 100)
  
  # Render UI for the current pair
  output$quiz_ui <- renderUI({
    df <- filtered_rides()
    all_pairs <- pairs()
    idx <- pair_idx()
    if (is.null(all_pairs) || idx > nrow(all_pairs)) {
      if (nrow(df) < 2) {
        return(h4("Not enough rides for a quiz in this park."))
      }
      return(h4("Quiz complete! See your ranking below."))
    }
    i <- all_pairs[idx, 1]
    j <- all_pairs[idx, 2]
    fluidRow(
      column(5, box(
        width = 12, status = "primary", solidHeader = TRUE,
        h4(df$Ride_name[i]),
        p(strong("Park: "), df$Park_location[i]),
        p(strong("Location: "), df$Park_area[i]),
        actionButton("choose_left", "Choose", width = "100%")
      )),
      column(2, div(style = "text-align:center;padding-top:60px;", h3("VS"))),
      column(5, box(
        width = 12, status = "warning", solidHeader = TRUE,
        h4(df$Ride_name[j]),
        p(strong("Park: "), df$Park_location[j]),
        p(strong("Location: "), df$Park_area[j]),
        actionButton("choose_right", "Choose", width = "100%")
      ))
    )
  })
  
  # Handle choices
  observeEvent(input$choose_left, {
    idx <- pair_idx()
    ch <- choices()
    ch[idx] <- "left"
    choices(ch)
    pair_idx(idx + 1)
  })
  
  observeEvent(input$choose_right, {
    idx <- pair_idx()
    ch <- choices()
    ch[idx] <- "right"
    choices(ch)
    pair_idx(idx + 1)
  })
  
  # Compute ranking based on user choices
  ranking_tbl <- reactive({
    df <- filtered_rides()
    all_pairs <- pairs()
    ch <- choices()
    if (is.null(all_pairs) || nrow(df) < 2) return(NULL)
    n <- nrow(df)
    scores <- setNames(rep(0, n), df$Ride_name)
    for (k in seq_along(ch)) {
      if (!is.na(ch[k])) {
        i <- all_pairs[k, 1]
        j <- all_pairs[k, 2]
        if (ch[k] == "left") {
          scores[i] <- scores[i] + 1
        } else if (ch[k] == "right") {
          scores[j] <- scores[j] + 1
        }
      }
    }
    ranking <- data.frame(
      Ride = names(scores),
      Score = as.integer(scores),
      stringsAsFactors = FALSE
    )
    ranking <- ranking[order(-ranking$Score), ]
    ranking
  })
  
  # Only show the ranking table at the end
  output$ranking_section <- renderUI({
    df <- filtered_rides()
    all_pairs <- pairs()
    idx <- pair_idx()
    if (is.null(all_pairs) || idx <= nrow(all_pairs)) return(NULL)
    box(
      width = 12, status = "success", solidHeader = TRUE,
      h4("Your Ranking:"),
      tableOutput("ranking_tbl")
    )
  })
  
  output$ranking_tbl <- renderTable({
    ranking_tbl()
  }, striped = TRUE, bordered = TRUE)
}

# Run the app
shinyApp(ui, server)

