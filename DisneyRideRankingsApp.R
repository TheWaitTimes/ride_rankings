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
    actionButton("reset", "Reset List"),
    tags$hr(),
    div(
      style = "text-align: center;",
      tags$a(
        href = "https://buymeacoffee.com/cfbnumbers",
        target = "_blank",
        tags$img(
          src = "https://miro.medium.com/v2/resize:fit:1090/0*lHgOW3tB_MfDAlBf.png",
          style = "width: 150px; height: auto;",
          alt = "Buy me a coffee"
        )
      )
    )
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
        p("Use the sidebar to filter rides by park and reset the quiz."),
        p("If you enjoyed the game, click the button to buy me a coffee! (Or Dole Whip... or those glazed pecans!).")
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
  filtered_rides <- reactive({
    if (is.null(input$park_filter) || length(input$park_filter) == 0) {
      rides[0, ]
    } else {
      rides %>% filter(Park_location %in% input$park_filter)
    }
  })
  norm <- function(x) {
    (x - min(x)) / (max(x) - min(x))
  }
  
  # Dynamic K-factor for Elo
  get_dynamic_k <- function(num) {
    base_k <- 32
    if (num < 10) return(base_k)
    if (num < 30) return(base_k / 2)
    base_k / 4
  }
  
  # --- Reactives and State ---
  pairs <- reactiveVal(NULL)            # the subsampled pairs for this session
  choices <- reactiveVal(NULL)          # user choices per pair
  pair_idx <- reactiveVal(1)            # which pair is currently being shown
  elo_ratings <- reactiveVal(NULL)      # current Elo for each ride
  num_comparisons <- reactiveVal(NULL)  # how many times each ride has been compared
  compared_pairs <- reactiveVal(matrix(ncol=2, nrow=0)) # matrix of pairs already compared
  
  # --- Reset logic, also triggers when park_filter changes ---
  observeEvent(list(input$reset, input$park_filter), {
    df <- filtered_rides()
    n <- nrow(df)
    if (n < 2) {
      pairs(NULL)
      choices(NULL)
      pair_idx(1)
      elo_ratings(rep(1500, n))
      num_comparisons(rep(0, n))
      compared_pairs(matrix(ncol=2, nrow=0))
      return()
    }
    # Subsampled pairs
    all_possible_pairs <- t(combn(n, 2))
    num_pairs_to_ask <- min(80, nrow(all_possible_pairs))  #80 max pairs
    sampled_pairs <- all_possible_pairs[sample(nrow(all_possible_pairs), num_pairs_to_ask), , drop = FALSE]
    pairs(sampled_pairs)
    choices(character(0))
    pair_idx(1)
    elo_ratings(rep(1500, n))
    num_comparisons(rep(0, n))
    compared_pairs(matrix(ncol=2, nrow=0))
  }, priority = 100)
  
  # --- Helper: Next adaptive pair within sampled pairs ---
  get_next_pair <- function(rides, elo_scores, compared, sampled_pairs) {
    n <- nrow(rides)
    if (n < 2 || is.null(sampled_pairs) || nrow(sampled_pairs) == 0) return(NULL)
    # Remove already compared pairs
    remaining_pairs <- sampled_pairs
    if (nrow(compared) > 0) {
      already <- apply(remaining_pairs, 1, function(pair) {
        any(apply(compared, 1, function(cp) all(cp == pair)))
      })
      remaining_pairs <- remaining_pairs[!already, , drop = FALSE]
    }
    if (nrow(remaining_pairs) == 0) return(NULL)
    elo_scores <- as.numeric(elo_scores)
    if (any(is.na(elo_scores))) elo_scores[is.na(elo_scores)] <- 1500
    elo_diffs <- abs(elo_scores[remaining_pairs[,1]] - elo_scores[remaining_pairs[,2]])
    min_diff_idx <- which.min(elo_diffs)
    remaining_pairs[min_diff_idx, ]
  }
  
  # --- UI for the current pair ---
  output$quiz_ui <- renderUI({
    df <- filtered_rides()
    all_pairs <- pairs()
    idx <- as.integer(pair_idx())
    if (is.null(idx) || is.na(idx) || !is.numeric(idx) || idx < 1) idx <- 1
    if (is.null(all_pairs) || idx > nrow(all_pairs)) {
      if (nrow(df) < 2) {
        return(h4("Not enough rides for a quiz in this park."))
      }
      return(h4("Rankings complete! See your list below:"))
    }
    i <- all_pairs[idx, 1]
    j <- all_pairs[idx, 2]
    fluidRow(
      column(5, box(
        width = 12, status = "primary", solidHeader = TRUE,
        style = "text-align:center;",
        h4(df$Ride_name[i]),
        p(strong("Park: "), df$Park_location[i]),
        p(strong("Location: "), df$Park_area[i]),
        actionButton("choose_left", "Choose", width = "100%")
      )),
      column(2, div(style = "text-align:center;padding-top:60px;", h3("VS"))),
      column(5, box(
        width = 12, status = "warning", solidHeader = TRUE,
        style = "text-align:center;",
        h4(df$Ride_name[j]),
        p(strong("Park: "), df$Park_location[j]),
        p(strong("Location: "), df$Park_area[j]),
        actionButton("choose_right", "Choose", width = "100%")
      ))
    )
  })
  
  # --- Choice handling & Elo update ---
  handle_choice <- function(winner_side) {
    idx <- as.integer(pair_idx())
    all_pairs <- pairs()
    df <- filtered_rides()
    ratings <- as.numeric(elo_ratings())
    n_rides <- nrow(df)
    nc <- num_comparisons()
    comp <- compared_pairs()
    if (is.null(all_pairs) || idx > nrow(all_pairs)) return()
    i <- all_pairs[idx, 1]
    j <- all_pairs[idx, 2]
    if (length(ratings) != n_rides || is.null(i) || is.null(j) ||
        is.na(i) || is.na(j) || i < 1 || j < 1 || i > n_rides || j > n_rides) {
      showNotification("Error: Internal index mismatch. Please reset and try again.", type = "error")
      return()
    }
    # Determine winner/loser
    wins.A <- ifelse(winner_side == "left", 1, 0)
    # Dynamic K-factor for each ride
    k_i <- get_dynamic_k(nc[i])
    k_j <- get_dynamic_k(nc[j])
    k <- mean(c(k_i, k_j))
    out <- elo::elo.calc(wins.A = wins.A, elo.A = ratings[i], elo.B = ratings[j], k = k)
    ratings[i] <- out[1]
    ratings[j] <- out[2]
    # Track comparison count
    nc[i] <- nc[i] + 1
    nc[j] <- nc[j] + 1
    # Track compared pairs (so we don't repeat)
    comp <- rbind(comp, c(i, j))
    # Find next pair adaptively in the sampled pool
    next_pair <- get_next_pair(df, ratings, comp, all_pairs)
    if (is.null(next_pair)) {
      # No more pairs, finish
      pairs(all_pairs)
      pair_idx(nrow(all_pairs) + 1)
      elo_ratings(ratings)
      num_comparisons(nc)
      compared_pairs(comp)
      return()
    }
    pairs(rbind(all_pairs, next_pair))
    pair_idx(idx + 1)
    elo_ratings(ratings)
    num_comparisons(nc)
    compared_pairs(comp)
  }
  
  observeEvent(input$choose_left, {
    handle_choice("left")
  })
  observeEvent(input$choose_right, {
    handle_choice("right")
  })
  
  # --- Rankings table ---
  ranking_tbl <- reactive({
    df <- filtered_rides()
    ratings <- as.numeric(elo_ratings())
    if (is.null(df) || is.null(ratings) || length(ratings) != nrow(df)) return(NULL)
    data.frame(
      Ride = df$Ride_name,
      Elo = as.numeric(ratings), # Force numeric
      stringsAsFactors = FALSE
    ) %>% arrange(desc(Elo)) %>%
      mutate(Elo = norm(Elo)*100,
             Elo = round(Elo, 1)) %>%
      rename(Rating = Elo) %>%
      mutate(Rank = rank(-Rating, ties.method = "min")) %>%
      select(Rank, Ride, Rating)
  })
  
  output$ranking_section <- renderUI({
    df <- filtered_rides()
    all_pairs <- pairs()
    idx <- as.integer(pair_idx())
    if (is.null(all_pairs) || is.null(idx) || idx <= nrow(all_pairs)) return(NULL)
    tbl <- ranking_tbl()
    n <- nrow(tbl)
    if (n > 15) {
      split_point <- ceiling(n / 2)
      fluidRow(
        column(6, tableOutput("ranking_tbl_left")),
        column(6, tableOutput("ranking_tbl_right"))
      )
    } else {
      box(
        width = 12, status = "success", solidHeader = TRUE,
        h4("Your Ranking:"),
        tableOutput("ranking_tbl")
      )
    }
  })
  
  output$ranking_tbl <- renderTable({
    ranking_tbl()
  }, striped = TRUE, bordered = TRUE, digits = 1)
  output$ranking_tbl_left <- renderTable({
    tbl <- ranking_tbl()
    n <- nrow(tbl)
    if (n > 15) {
      split_point <- ceiling(n / 2)
      tbl[1:split_point, ]
    } else {
      NULL
    }
  }, striped = TRUE, bordered = TRUE, digits = 1)
  output$ranking_tbl_right <- renderTable({
    tbl <- ranking_tbl()
    n <- nrow(tbl)
    if (n > 15) {
      split_point <- ceiling(n / 2)
      tbl[(split_point+1):n, ]
    } else {
      NULL
    }
  }, striped = TRUE, bordered = TRUE, digits = 1)
}


shinyApp(ui, server)



