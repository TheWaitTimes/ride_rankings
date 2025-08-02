library(shiny)
library(shinydashboard)
library(tidyverse)
library(elo)

# Read all datasets
rides <- read.csv("https://raw.githubusercontent.com/TheWaitTimes/ride_rankings/refs/heads/main/ridenames.csv")
resorts <- read.csv("https://raw.githubusercontent.com/TheWaitTimes/ride_rankings/refs/heads/main/resorts_data.csv")
snacks <- read.csv("https://raw.githubusercontent.com/TheWaitTimes/ride_rankings/refs/heads/main/snacks.csv")

norm <- function(x) {
  (x - min(x)) / (max(x) - min(x))
}

ui <- dashboardPage(
  title = "Disney World Rankings",
  dashboardHeader(
    title = span(
      "Disney World Rankings",
      style = "font-family: 'Germania One', cursive;"
    ),
    tags$li(
      class = "dropdown",
      tags$a(
        href = "#",
        id = "sidebar-toggle",
        tags$i(class = "fa fa-bars", style = "color:white;font-size:28px;"),
        style = "padding: 10px;"
      )
    )
  ),
  dashboardSidebar(
    sidebarMenu(
      id = "sidebar",
      menuItem("Ride Rankings", tabName = "quiz", icon = icon("rocket")),
      menuItem("Resorts Rankings", tabName = "resorts_quiz", icon = icon("hotel")),
      menuItem("Park Snacks Rankings", tabName = "snacks_quiz", icon = icon("ice-cream")),   # NEW MENU ITEM
      menuItem("Instructions", tabName = "instructions", icon = icon("info-circle"))
    ),
    br(),
    conditionalPanel(
      condition = "input.sidebar == 'quiz'",
      checkboxGroupInput(
        "park_filter",
        "Select Park Locations to Include:",
        choices = unique(rides$Park_location),
        selected = unique(rides$Park_location)
      )
    ),
    conditionalPanel(
      condition = "input.sidebar == 'quiz' || input.sidebar == 'resorts_quiz' || input.sidebar == 'snacks_quiz'",
      actionButton("reset", "Reset List/Start Rankings", style='font-size: 20px')
    ),
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
    body, .box, .sidebar, .main-sidebar, .main-footer, h1, h2, h3, h4, h5, h6, label, .content-wrapper, .skin-blue .main-sidebar {
      font-family: 'Mouse Memoirs', sans-serif !important;
      font-size: 23px !important;
    }
    .main-header, .main-header * {
      font-family: 'Germania One', cursive !important;
      font-size: 22px !important;
    }
    #choose_left, #choose_right, #choose_resorts_left, #choose_resorts_right, #choose_snacks_left, #choose_snacks_right {
      font-size: 1em !important;
    }
  ")),
      tags$script(HTML("
  $(document).on('shiny:connected', function() {
    $('#sidebar-toggle').on('click', function(e) {
      e.preventDefault();
      $('body').toggleClass('sidebar-collapse');
    });
  });
")),
      tags$style(HTML("
  #sidebar-toggle { display: none; }
  @media (max-width: 1200px) {
    #sidebar-toggle { display: inline-block !important; }
  }
"))
    ),
    tabItems(
      tabItem(
        tabName = "instructions",
        h3("Instructions"),
        p("Choose which Ride, Resort, or Snack you prefer in each pair. The app keeps track of your choices and shows your personalized ranking at the end!"),
        p("Use the sidebar to filter by park (for rides) and reset the quiz."),
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
      ),
      tabItem(
        tabName = "resorts_quiz",
        fluidRow(
          box(width = 12, uiOutput("resorts_quiz_ui"))
        ),
        fluidRow(
          box(width = 12, uiOutput("resorts_ranking_section"))
        )
      ),
      tabItem(
        tabName = "snacks_quiz",
        fluidRow(
          box(width = 12, uiOutput("snacks_quiz_ui"))
        ),
        fluidRow(
          box(width = 12, uiOutput("snacks_ranking_section"))
        )
      )
    )
  )
)

server <- function(input, output, session) {
  # --- Rides logic (original) ---
  filtered_rides <- reactive({
    if (input$sidebar == "quiz") {
      if (is.null(input$park_filter) || length(input$park_filter) == 0) {
        rides[0, ]
      } else {
        rides %>% filter(Park_location %in% input$park_filter)
      }
    } else {
      rides
    }
  })
  norm <- function(x) (x - min(x)) / (max(x) - min(x))
  get_dynamic_k <- function(num) {
    base_k <- 32
    if (num < 10) return(base_k)
    if (num < 30) return(base_k / 2)
    base_k / 4
  }
  pairs <- reactiveVal(NULL)
  choices <- reactiveVal(NULL)
  pair_idx <- reactiveVal(1)
  elo_ratings <- reactiveVal(NULL)
  num_comparisons <- reactiveVal(NULL)
  compared_pairs <- reactiveVal(matrix(ncol=2, nrow=0))
  observeEvent(list(input$reset, input$park_filter), {
    if (input$sidebar == "quiz") {
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
      all_possible_pairs <- t(combn(n, 2))
      num_pairs_to_ask <- min(80, nrow(all_possible_pairs))
      sampled_pairs <- all_possible_pairs[sample(nrow(all_possible_pairs), num_pairs_to_ask), , drop = FALSE]
      pairs(sampled_pairs)
      choices(character(0))
      pair_idx(1)
      elo_ratings(rep(1500, n))
      num_comparisons(rep(0, n))
      compared_pairs(matrix(ncol=2, nrow=0))
    }
  }, priority = 100)
  get_next_pair <- function(df, elo_scores, compared, sampled_pairs) {
    n <- nrow(df)
    if (n < 2 || is.null(sampled_pairs) || nrow(sampled_pairs) == 0) return(NULL)
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
    wins.A <- ifelse(winner_side == "left", 1, 0)
    k_i <- get_dynamic_k(nc[i])
    k_j <- get_dynamic_k(nc[j])
    k <- mean(c(k_i, k_j))
    out <- elo::elo.calc(wins.A = wins.A, elo.A = ratings[i], elo.B = ratings[j], k = k)
    ratings[i] <- out[1]
    ratings[j] <- out[2]
    nc[i] <- nc[i] + 1
    nc[j] <- nc[j] + 1
    comp <- rbind(comp, c(i, j))
    next_pair <- get_next_pair(df, ratings, comp, all_pairs)
    if (is.null(next_pair)) {
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
  observeEvent(input$choose_left, { handle_choice("left") })
  observeEvent(input$choose_right, { handle_choice("right") })
  ranking_tbl <- reactive({
    df <- filtered_rides()
    ratings <- as.numeric(elo_ratings())
    if (is.null(df) || is.null(ratings) || length(ratings) != nrow(df)) return(NULL)
    data.frame(
      Ride = df$Ride_name,
      Elo = as.numeric(ratings),
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
  
  # --- Resorts logic (uses all resorts, no filtering) ---
  filtered_resorts <- reactive({
    resorts
  })
  resorts_pairs <- reactiveVal(NULL)
  resorts_choices <- reactiveVal(NULL)
  resorts_pair_idx <- reactiveVal(1)
  resorts_elo_ratings <- reactiveVal(NULL)
  resorts_num_comparisons <- reactiveVal(NULL)
  resorts_compared_pairs <- reactiveVal(matrix(ncol=2, nrow=0))
  observeEvent(input$reset, {
    if (input$sidebar == "resorts_quiz") {
      df <- filtered_resorts()
      n <- nrow(df)
      if (n < 2) {
        resorts_pairs(NULL)
        resorts_choices(NULL)
        resorts_pair_idx(1)
        resorts_elo_ratings(rep(1500, n))
        resorts_num_comparisons(rep(0, n))
        resorts_compared_pairs(matrix(ncol=2, nrow=0))
        return()
      }
      all_possible_pairs <- t(combn(n, 2))
      num_pairs_to_ask <- min(80, nrow(all_possible_pairs))
      sampled_pairs <- all_possible_pairs[sample(nrow(all_possible_pairs), num_pairs_to_ask), , drop = FALSE]
      resorts_pairs(sampled_pairs)
      resorts_choices(character(0))
      resorts_pair_idx(1)
      resorts_elo_ratings(rep(1500, n))
      resorts_num_comparisons(rep(0, n))
      resorts_compared_pairs(matrix(ncol=2, nrow=0))
    }
  }, priority = 100)
  get_resorts_next_pair <- function(df, elo_scores, compared, sampled_pairs) {
    n <- nrow(df)
    if (n < 2 || is.null(sampled_pairs) || nrow(sampled_pairs) == 0) return(NULL)
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
  output$resorts_quiz_ui <- renderUI({
    df <- filtered_resorts()
    all_pairs <- resorts_pairs()
    idx <- as.integer(resorts_pair_idx())
    if (is.null(idx) || is.na(idx) || !is.numeric(idx) || idx < 1) idx <- 1
    if (is.null(all_pairs) || idx > nrow(all_pairs)) {
      if (nrow(df) < 2) {
        return(h4("Not enough resorts for a quiz in this park."))
      }
      return(h4("Rankings complete! See your list below (Or hit reset list to start a new list)"))
    }
    i <- all_pairs[idx, 1]
    j <- all_pairs[idx, 2]
    fluidRow(
      column(5, box(
        width = 12, status = "primary", solidHeader = TRUE,
        style = "text-align:center;",
        h4(df$Resort_name[i]),
        p(strong("Area: "), df$Park_location[i]),
        p(strong("Category: "), df$Park_area[i]),
        actionButton("choose_resorts_left", "Choose", width = "100%")
      )),
      column(2, div(style = "text-align:center;padding-top:60px;", h3("VS"))),
      column(5, box(
        width = 12, status = "warning", solidHeader = TRUE,
        style = "text-align:center;",
        h4(df$Resort_name[j]),
        p(strong("Area: "), df$Park_location[j]),
        p(strong("Category: "), df$Park_area[j]),
        actionButton("choose_resorts_right", "Choose", width = "100%")
      ))
    )
  })
  handle_resorts_choice <- function(winner_side) {
    idx <- as.integer(resorts_pair_idx())
    all_pairs <- resorts_pairs()
    df <- filtered_resorts()
    ratings <- as.numeric(resorts_elo_ratings())
    n_resorts <- nrow(df)
    nc <- resorts_num_comparisons()
    comp <- resorts_compared_pairs()
    if (is.null(all_pairs) || idx > nrow(all_pairs)) return()
    i <- all_pairs[idx, 1]
    j <- all_pairs[idx, 2]
    if (length(ratings) != n_resorts || is.null(i) || is.null(j) ||
        is.na(i) || is.na(j) || i < 1 || j < 1 || i > n_resorts || j > n_resorts) {
      showNotification("Error: Internal index mismatch. Please reset and try again.", type = "error")
      return()
    }
    wins.A <- ifelse(winner_side == "left", 1, 0)
    k_i <- get_dynamic_k(nc[i])
    k_j <- get_dynamic_k(nc[j])
    k <- mean(c(k_i, k_j))
    out <- elo::elo.calc(wins.A = wins.A, elo.A = ratings[i], elo.B = ratings[j], k = k)
    ratings[i] <- out[1]
    ratings[j] <- out[2]
    nc[i] <- nc[i] + 1
    nc[j] <- nc[j] + 1
    comp <- rbind(comp, c(i, j))
    next_pair <- get_resorts_next_pair(df, ratings, comp, all_pairs)
    if (is.null(next_pair)) {
      resorts_pairs(all_pairs)
      resorts_pair_idx(nrow(all_pairs) + 1)
      resorts_elo_ratings(ratings)
      resorts_num_comparisons(nc)
      resorts_compared_pairs(comp)
      return()
    }
    resorts_pairs(rbind(all_pairs, next_pair))
    resorts_pair_idx(idx + 1)
    resorts_elo_ratings(ratings)
    resorts_num_comparisons(nc)
    resorts_compared_pairs(comp)
  }
  observeEvent(input$choose_resorts_left, { handle_resorts_choice("left") })
  observeEvent(input$choose_resorts_right, { handle_resorts_choice("right") })
  resorts_ranking_tbl <- reactive({
    df <- filtered_resorts()
    ratings <- as.numeric(resorts_elo_ratings())
    if (is.null(df) || is.null(ratings) || length(ratings) != nrow(df)) return(NULL)
    data.frame(
      Resort = df$Resort_name,
      Elo = as.numeric(ratings),
      stringsAsFactors = FALSE
    ) %>% arrange(desc(Elo)) %>%
      mutate(Elo = norm(Elo)*100,
             Elo = round(Elo, 1)) %>%
      rename(Rating = Elo) %>%
      mutate(Rank = rank(-Rating, ties.method = "min")) %>%
      select(Rank, Resort, Rating)
  })
  output$resorts_ranking_section <- renderUI({
    df <- filtered_resorts()
    all_pairs <- resorts_pairs()
    idx <- as.integer(resorts_pair_idx())
    if (is.null(all_pairs) || is.null(idx) || idx <= nrow(all_pairs)) return(NULL)
    tbl <- resorts_ranking_tbl()
    n <- nrow(tbl)
    if (n > 15) {
      split_point <- ceiling(n / 2)
      fluidRow(
        column(6, tableOutput("resorts_ranking_tbl_left")),
        column(6, tableOutput("resorts_ranking_tbl_right"))
      )
    } else {
      box(
        width = 12, status = "success", solidHeader = TRUE,
        h4("Your Ranking:"),
        tableOutput("resorts_ranking_tbl")
      )
    }
  })
  output$resorts_ranking_tbl <- renderTable({
    resorts_ranking_tbl()
  }, striped = TRUE, bordered = TRUE, digits = 1)
  output$resorts_ranking_tbl_left <- renderTable({
    tbl <- resorts_ranking_tbl()
    n <- nrow(tbl)
    if (n > 15) {
      split_point <- ceiling(n / 2)
      tbl[1:split_point, ]
    } else {
      NULL
    }
  }, striped = TRUE, bordered = TRUE, digits = 1)
  output$resorts_ranking_tbl_right <- renderTable({
    tbl <- resorts_ranking_tbl()
    n <- nrow(tbl)
    if (n > 15) {
      split_point <- ceiling(n / 2)
      tbl[(split_point+1):n, ]
    } else {
      NULL
    }
  }, striped = TRUE, bordered = TRUE, digits = 1)
  
  # --- Snacks logic (NEW) ---
  filtered_snacks <- reactive({
    snacks
  })
  snacks_pairs <- reactiveVal(NULL)
  snacks_choices <- reactiveVal(NULL)
  snacks_pair_idx <- reactiveVal(1)
  snacks_elo_ratings <- reactiveVal(NULL)
  snacks_num_comparisons <- reactiveVal(NULL)
  snacks_compared_pairs <- reactiveVal(matrix(ncol=2, nrow=0))
  observeEvent(input$reset, {
    if (input$sidebar == "snacks_quiz") {
      df <- filtered_snacks()
      n <- nrow(df)
      if (n < 2) {
        snacks_pairs(NULL)
        snacks_choices(NULL)
        snacks_pair_idx(1)
        snacks_elo_ratings(rep(1500, n))
        snacks_num_comparisons(rep(0, n))
        snacks_compared_pairs(matrix(ncol=2, nrow=0))
        return()
      }
      all_possible_pairs <- t(combn(n, 2))
      num_pairs_to_ask <- min(80, nrow(all_possible_pairs))
      sampled_pairs <- all_possible_pairs[sample(nrow(all_possible_pairs), num_pairs_to_ask), , drop = FALSE]
      snacks_pairs(sampled_pairs)
      snacks_choices(character(0))
      snacks_pair_idx(1)
      snacks_elo_ratings(rep(1500, n))
      snacks_num_comparisons(rep(0, n))
      snacks_compared_pairs(matrix(ncol=2, nrow=0))
    }
  }, priority = 100)
  get_snacks_next_pair <- function(df, elo_scores, compared, sampled_pairs) {
    n <- nrow(df)
    if (n < 2 || is.null(sampled_pairs) || nrow(sampled_pairs) == 0) return(NULL)
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
  output$snacks_quiz_ui <- renderUI({
    df <- filtered_snacks()
    all_pairs <- snacks_pairs()
    idx <- as.integer(snacks_pair_idx())
    if (is.null(idx) || is.na(idx) || !is.numeric(idx) || idx < 1) idx <- 1
    if (is.null(all_pairs) || idx > nrow(all_pairs)) {
      if (nrow(df) < 2) {
        return(h4("Not enough snacks for a quiz."))
      }
      return(h4("Rankings complete! See your list below (Or hit reset list to start a new list)"))
    }
    i <- all_pairs[idx, 1]
    j <- all_pairs[idx, 2]
    fluidRow(
      column(5, box(
        width = 12, status = "primary", solidHeader = TRUE,
        style = "text-align:center;",
        h4(df$Snack_name[i]),
        p(strong("Park: "), df$Park_location[i]),
        p(strong("Location: "), df$Park_area[i]),
        actionButton("choose_snacks_left", "Choose", width = "100%")
      )),
      column(2, div(style = "text-align:center;padding-top:60px;", h3("VS"))),
      column(5, box(
        width = 12, status = "warning", solidHeader = TRUE,
        style = "text-align:center;",
        h4(df$Snack_name[j]),
        p(strong("Park: "), df$Park_location[j]),
        p(strong("Location: "), df$Park_area[j]),
        actionButton("choose_snacks_right", "Choose", width = "100%")
      ))
    )
  })
  handle_snacks_choice <- function(winner_side) {
    idx <- as.integer(snacks_pair_idx())
    all_pairs <- snacks_pairs()
    df <- filtered_snacks()
    ratings <- as.numeric(snacks_elo_ratings())
    n_snacks <- nrow(df)
    nc <- snacks_num_comparisons()
    comp <- snacks_compared_pairs()
    if (is.null(all_pairs) || idx > nrow(all_pairs)) return()
    i <- all_pairs[idx, 1]
    j <- all_pairs[idx, 2]
    if (length(ratings) != n_snacks || is.null(i) || is.null(j) ||
        is.na(i) || is.na(j) || i < 1 || j < 1 || i > n_snacks || j > n_snacks) {
      showNotification("Error: Internal index mismatch. Please reset and try again.", type = "error")
      return()
    }
    wins.A <- ifelse(winner_side == "left", 1, 0)
    k_i <- get_dynamic_k(nc[i])
    k_j <- get_dynamic_k(nc[j])
    k <- mean(c(k_i, k_j))
    out <- elo::elo.calc(wins.A = wins.A, elo.A = ratings[i], elo.B = ratings[j], k = k)
    ratings[i] <- out[1]
    ratings[j] <- out[2]
    nc[i] <- nc[i] + 1
    nc[j] <- nc[j] + 1
    comp <- rbind(comp, c(i, j))
    next_pair <- get_snacks_next_pair(df, ratings, comp, all_pairs)
    if (is.null(next_pair)) {
      snacks_pairs(all_pairs)
      snacks_pair_idx(nrow(all_pairs) + 1)
      snacks_elo_ratings(ratings)
      snacks_num_comparisons(nc)
      snacks_compared_pairs(comp)
      return()
    }
    snacks_pairs(rbind(all_pairs, next_pair))
    snacks_pair_idx(idx + 1)
    snacks_elo_ratings(ratings)
    snacks_num_comparisons(nc)
    snacks_compared_pairs(comp)
  }
  observeEvent(input$choose_snacks_left, { handle_snacks_choice("left") })
  observeEvent(input$choose_snacks_right, { handle_snacks_choice("right") })
  snacks_ranking_tbl <- reactive({
    df <- filtered_snacks()
    ratings <- as.numeric(snacks_elo_ratings())
    if (is.null(df) || is.null(ratings) || length(ratings) != nrow(df)) return(NULL)
    data.frame(
      Snack = df$Snack_name,
      Elo = as.numeric(ratings),
      stringsAsFactors = FALSE
    ) %>% arrange(desc(Elo)) %>%
      mutate(Elo = norm(Elo)*100,
             Elo = round(Elo, 1)) %>%
      rename(Rating = Elo) %>%
      mutate(Rank = rank(-Rating, ties.method = "min")) %>%
      select(Rank, Snack, Rating)
  })
  output$snacks_ranking_section <- renderUI({
    df <- filtered_snacks()
    all_pairs <- snacks_pairs()
    idx <- as.integer(snacks_pair_idx())
    if (is.null(all_pairs) || is.null(idx) || idx <= nrow(all_pairs)) return(NULL)
    tbl <- snacks_ranking_tbl()
    n <- nrow(tbl)
    if (n > 15) {
      split_point <- ceiling(n / 2)
      fluidRow(
        column(6, tableOutput("snacks_ranking_tbl_left")),
        column(6, tableOutput("snacks_ranking_tbl_right"))
      )
    } else {
      box(
        width = 12, status = "success", solidHeader = TRUE,
        h4("Your Ranking:"),
        tableOutput("snacks_ranking_tbl")
      )
    }
  })
  output$snacks_ranking_tbl <- renderTable({
    snacks_ranking_tbl()
  }, striped = TRUE, bordered = TRUE, digits = 1)
  output$snacks_ranking_tbl_left <- renderTable({
    tbl <- snacks_ranking_tbl()
    n <- nrow(tbl)
    if (n > 15) {
      split_point <- ceiling(n / 2)
      tbl[1:split_point, ]
    } else {
      NULL
    }
  }, striped = TRUE, bordered = TRUE, digits = 1)
  output$snacks_ranking_tbl_right <- renderTable({
    tbl <- snacks_ranking_tbl()
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




