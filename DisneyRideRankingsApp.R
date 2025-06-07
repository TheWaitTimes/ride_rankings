library(shiny)
library(shinydashboard)
library(tidyverse)
library(elo)

# Read rides data
rides <- read.csv("https://raw.githubusercontent.com/TheWaitTimes/ride_rankings/refs/heads/main/ridenames.csv")

norm <- function(x) {
  (x - min(x)) / (max(x) - min(x))
}

ui <- dashboardPage(
  dashboardHeader(
    title = span(
      "Disney Rides Ranking Quiz",
      style = "font-family: 'Germania One', cursive;"
    )
  ),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Rankings", tabName = "quiz", icon = icon("star")),
      menuItem("Instructions", tabName = "instructions", icon = icon("info-circle"))
    ),
    br(),
    checkboxGroupInput(
      "park_filter",
      "Select Park Locations to Include:",
      choices = unique(rides$Park_location),
      selected = unique(rides$Park_location)
    ),
    actionButton("reset", "Reset Quiz")
  ),
  dashboardBody(
    tags$head(
      tags$link(
        href = "https://fonts.googleapis.com/css?family=Germania+One&display=swap",
        rel = "stylesheet"
      ),
      tags$link(
        href = "https://fonts.googleapis.com/css?family=Mouse+Memoirs&display=swap",
        rel = "stylesheet"
      ),
      tags$style(HTML("
        body, .box, .sidebar, .main-header, .main-sidebar, .main-footer, h1, h2, h3, h4, h5, h6, label, .content-wrapper, .skin-blue .main-sidebar, .skin-blue .main-header .logo, .skin-blue .main-header .navbar, .skin-blue .main-header .navbar .navbar-brand, .skin-blue .main-header .navbar .navbar-title, .skin-blue .main-header .logo {
          font-family: 'Mouse Memoirs', sans-serif !important;
           font-size: 23px !important;
        }
        .main-header .logo span, .main-header .logo, .main-header .navbar .navbar-brand, .main-header .navbar .navbar-title, .main-header .navbar .logo {
          font-family: 'Germania One', cursive !important;
           font-size: 22px !important;
        }
        #choose_left, #choose_right {
          font-size: 1em !important;
        }
      "))
    ),
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

server <- function(input, output, session) {
  # Reactive rides data based on park filter
  filtered_rides <- reactive({
    if (is.null(input$park_filter) || length(input$park_filter) == 0) {
      rides[0, ] # Return empty if nothing selected
    } else {
      rides %>% filter(Park_location %in% input$park_filter)
    }
  })
  
  # Subsampled pairwise logic
  pairs <- reactiveVal(NULL)
  choices <- reactiveVal(NULL)
  pair_idx <- reactiveVal(1)
  elo_ratings <- reactiveVal(NULL)
  
  # Reset logic, also triggers when park_filter changes
  observeEvent(list(input$reset, input$park_filter), {
    df <- filtered_rides()
    n <- nrow(df)
    if (n < 2) {
      pairs(NULL)
      choices(NULL)
      pair_idx(1)
      elo_ratings(rep(1500, n))
      return()
    }
    # Subsampled pairs
    num_pairs_to_ask <- min(40, n * (n-1) / 2)
    all_possible_pairs <- t(combn(n, 2))
    if (nrow(all_possible_pairs) > num_pairs_to_ask) {
      sampled_pairs <- all_possible_pairs[sample(nrow(all_possible_pairs), num_pairs_to_ask), , drop=FALSE]
    } else {
      sampled_pairs <- all_possible_pairs
    }
    pairs(sampled_pairs)
    choices(rep(NA, nrow(sampled_pairs)))
    pair_idx(1)
    elo_ratings(rep(1500, n))
  }, priority = 100)
  
  # Render UI for the current pair
  output$quiz_ui <- renderUI({
    df <- filtered_rides()
    all_pairs <- pairs()
    idx <- as.integer(pair_idx())
    if (is.null(idx) || is.na(idx) || !is.numeric(idx) || idx < 1) idx <- 1
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
        style = "text-align:center;", # center all content
        h4(df$Ride_name[i]),
        p(strong("Park: "), df$Park_location[i]),
        p(strong("Location: "), df$Park_area[i]),
        actionButton("choose_left", "Choose", width = "100%")
      )),
      column(2, div(style = "text-align:center;padding-top:60px;", h3("VS"))),
      column(5, box(
        width = 12, status = "warning", solidHeader = TRUE,
        style = "text-align:center;", # center all content
        h4(df$Ride_name[j]),
        p(strong("Park: "), df$Park_location[j]),
        p(strong("Location: "), df$Park_area[j]),
        actionButton("choose_right", "Choose", width = "100%")
      ))
    )
  })
  
  # Handle choices and update Elo
  observeEvent(input$choose_left, {
    idx <- as.integer(pair_idx())
    if (is.null(idx) || is.na(idx) || !is.numeric(idx) || idx < 1) idx <- 1
    all_pairs <- pairs()
    if (is.null(all_pairs) || idx > nrow(all_pairs)) return()
    ch <- choices()
    ch[idx] <- "left"
    choices(ch)
    df <- filtered_rides()
    i <- all_pairs[idx, 1]
    j <- all_pairs[idx, 2]
    ratings <- as.numeric(elo_ratings())
    n_rides <- nrow(df)
    if (length(ratings) != n_rides || is.null(i) || is.null(j) ||
        is.na(i) || is.na(j) || i < 1 || j < 1 || i > n_rides || j > n_rides) {
      showNotification("Error: Internal index mismatch. Please reset and try again.", type = "error")
      return()
    }
    out <- elo::elo.calc(wins.A = 1, elo.A = ratings[i], elo.B = ratings[j], k = 32)
    ratings[i] <- out[1]
    ratings[j] <- out[2]
    elo_ratings(ratings)
    pair_idx(idx + 1)
  })
  
  observeEvent(input$choose_right, {
    idx <- as.integer(pair_idx())
    if (is.null(idx) || is.na(idx) || !is.numeric(idx) || idx < 1) idx <- 1
    all_pairs <- pairs()
    if (is.null(all_pairs) || idx > nrow(all_pairs)) return()
    ch <- choices()
    ch[idx] <- "right"
    choices(ch)
    df <- filtered_rides()
    i <- all_pairs[idx, 1]
    j <- all_pairs[idx, 2]
    ratings <- as.numeric(elo_ratings())
    n_rides <- nrow(df)
    if (length(ratings) != n_rides || is.null(i) || is.null(j) ||
        is.na(i) || is.na(j) || i < 1 || j < 1 || i > n_rides || j > n_rides) {
      showNotification("Error: Internal index mismatch. Please reset and try again.", type = "error")
      return()
    }
    out <- elo::elo.calc(wins.A = 0, elo.A = ratings[i], elo.B = ratings[j], k = 32)
    ratings[i] <- out[1]
    ratings[j] <- out[2]
    elo_ratings(ratings)
    pair_idx(idx + 1)
  })
  
  # Compute ranking based on Elo
  ranking_tbl <- reactive({
    df <- filtered_rides()
    ratings <- as.numeric(elo_ratings())
    if (is.null(df) || is.null(ratings) || length(ratings) != nrow(df)) return(NULL)
    data.frame(
      Ride = df$Ride_name,
      Elo = as.integer(ratings),
      stringsAsFactors = FALSE
    ) %>% arrange(desc(Elo)) %>%
      mutate(Elo = norm(Elo)*100,
             Elo = round(Elo, 1)) %>%
      rename(Rating = Elo)
  })
  
  # Only show the ranking table at the end
  output$ranking_section <- renderUI({
    df <- filtered_rides()
    all_pairs <- pairs()
    idx <- as.integer(pair_idx())
    if (is.null(all_pairs) || is.null(idx) || idx <= nrow(all_pairs)) return(NULL)
    box(
      width = 12, status = "success", solidHeader = TRUE,
      h4("Your Ranking:"),
      tableOutput("ranking_tbl")
    )
  })
  
  output$ranking_tbl <- renderTable({
    ranking_tbl()
  }, striped = TRUE, bordered = TRUE, digits = 1)
}

shinyApp(ui, server)




