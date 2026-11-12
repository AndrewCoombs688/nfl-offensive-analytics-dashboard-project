# ============================================================
# NFL Offensive Analytics Dashboard
# ============================================================

library(dplyr)
library(tidyr)
library(shiny)
library(nflreadr)
library(ggplot2)

cat("\014")
rm(list = ls())

# -------------------------
# Load data and choices
# -------------------------

pbp_2026 <- load_pbp(2026)

available_weeks <- sort(unique(na.omit(pbp_2026$week)))
team_choices <- sort(unique(na.omit(pbp_2026$posteam)))

pbp_2026 <- pbp_2026 %>%
  mutate(
    red_zone = if_else(
      yardline_100 <= 20,
      "Red Zone",
      "Outside Red Zone",
      missing = NA_character_
    )
  )

# -------------------------
# Logistic regression data
# -------------------------

epa_model_data <- pbp_2026 %>%
  filter(
    play_type %in% c("run", "pass"),
    !is.na(epa),
    !is.na(down),
    !is.na(ydstogo),
    !is.na(yardline_100),
    !is.na(score_differential)
  ) %>%
  mutate(
    success = as.integer(epa > 0),
    play_type = factor(play_type, levels = c("run", "pass"))
  )

# -------------------------
# Player EPA data
# -------------------------

rushing_player_epa <- pbp_2026 %>%
  filter(
    play_type == "run",
    !is.na(rusher_player_name),
    !is.na(epa)
  ) %>%
  transmute(
    week,
    player_id = rusher_player_id,
    player_name = rusher_player_name,
    team = posteam,
    epa
  )

passing_player_epa <- pbp_2026 %>%
  filter(
    play_type == "pass",
    !is.na(passer_player_name),
    !is.na(epa)
  ) %>%
  transmute(
    week,
    player_id = passer_player_id,
    player_name = passer_player_name,
    team = posteam,
    epa
  )

receiving_player_epa <- pbp_2026 %>%
  filter(
    play_type == "pass",
    !is.na(receiver_player_name),
    !is.na(epa)
  ) %>%
  transmute(
    week,
    player_id = receiver_player_id,
    player_name = receiver_player_name,
    team = posteam,
    epa
  )

player_epa <- bind_rows(
  rushing_player_epa,
  passing_player_epa,
  receiving_player_epa
) %>%
  filter(!is.na(player_name), !is.na(team))

# -------------------------
# NFL team color palettes
# -------------------------

team_palettes <- list(
  ARI = c("#97233F", "#000000", "#FFB612"),
  ATL = c("#A71930", "#000000", "#A5ACAF"),
  BAL = c("#241773", "#9E7C0C", "#000000"),
  BUF = c("#00338D", "#C60C30", "#A5ACAF"),
  CAR = c("#0085CA", "#101820", "#BFC0BF"),
  CHI = c("#0B162A", "#C83803", "#A5ACAF"),
  CIN = c("#FB4F14", "#000000", "#FFB612"),
  CLE = c("#311D00", "#FF3C00", "#FFB612"),
  DAL = c("#003594", "#869397", "#041E42"),
  DEN = c("#FB4F14", "#002244", "#A5ACAF"),
  DET = c("#0076B6", "#B0B7BC", "#000000"),
  GB  = c("#203731", "#FFB612", "#A5ACAF"),
  HOU = c("#03202F", "#A71930", "#A5ACAF"),
  IND = c("#002C5F", "#A2AAAD", "#A5ACAF"),
  JAX = c("#006778", "#D7A22A", "#101820"),
  JAC = c("#006778", "#D7A22A", "#101820"),
  KC  = c("#E31837", "#FFB81C", "#A5ACAF"),
  LA  = c("#003594", "#FFA300", "#FFD100"),
  LAR = c("#003594", "#FFA300", "#FFD100"),
  LAC = c("#0080C6", "#FFC20E", "#A5ACAF"),
  LV  = c("#000000", "#A5ACAF", "#C60C30"),
  LVR = c("#000000", "#A5ACAF", "#C60C30"),
  OAK = c("#000000", "#A5ACAF", "#C60C30"),
  MIA = c("#008E97", "#FC4C02", "#005778"),
  MIN = c("#4F2683", "#FFC62F", "#A5ACAF"),
  NE  = c("#002244", "#C60C30", "#B0B7BC"),
  NO  = c("#D3BC8D", "#101820", "#A5ACAF"),
  NYG = c("#0B2265", "#A71930", "#A5ACAF"),
  NYJ = c("#125740", "#A5ACAF", "#000000"),
  PHI = c("#004C54", "#A5ACAF", "#000000"),
  PIT = c("#101820", "#FFB612", "#A5ACAF"),
  SF  = c("#AA0000", "#B3995D", "#A5ACAF"),
  SEA = c("#002244", "#69BE28", "#A5ACAF"),
  TB  = c("#D50A0A", "#34302B", "#FF7900"),
  TEN = c("#0C2340", "#4B92DB", "#C8102E"),
  WAS = c("#5A1414", "#FFB612", "#A5ACAF"),
  WSH = c("#5A1414", "#FFB612", "#A5ACAF")
)

# -------------------------
# Plot theme and helper
# -------------------------

nfl_theme <- function(base_size = 13, primary = "#172B4D") {
  theme_minimal(base_size = base_size) +
    theme(
      plot.title = element_text(
        face = "bold",
        color = primary,
        size = rel(1.15)
      ),
      plot.subtitle = element_text(
        color = "#667085",
        margin = margin(b = 12)
      ),
      axis.title = element_text(
        color = primary,
        face = "bold"
      ),
      axis.text = element_text(color = "#475467"),
      panel.grid.minor = element_blank(),
      panel.grid.major.y = element_line(
        color = "#E7ECF2",
        linewidth = 0.5
      ),
      panel.grid.major.x = element_blank(),
      legend.position = "top",
      legend.title = element_blank(),
      plot.background = element_rect(
        fill = "white",
        color = NA
      ),
      panel.background = element_rect(
        fill = "white",
        color = NA
      ),
      plot.margin = margin(20, 18, 16, 14)
    )
}

empty_plot <- function(
    message = "No data available for this selection") {
  ggplot() +
    annotate(
      "text",
      x = 0,
      y = 0,
      label = message,
      size = 5,
      color = "#667085"
    ) +
    xlim(-1, 1) +
    ylim(-1, 1) +
    theme_void()
}



# -------------------------
# League-wide ranking calculations
# -------------------------

calculate_team_rankings <- function(data) {
  basic <- data %>%
    filter(play_type %in% c("run", "pass"), !is.na(epa)) %>%
    group_by(posteam) %>%
    summarise(
      offensive_plays = n(),
      success_plays = sum(epa > 0),
      success_rate = mean(epa > 0),
      run_plays = sum(play_type == "run"),
      run_epa = if (run_plays > 0) mean(epa[play_type == "run"]) else NA_real_,
      pass_plays = sum(play_type == "pass"),
      pass_epa = if (pass_plays > 0) mean(epa[play_type == "pass"]) else NA_real_,
      .groups = "drop"
    )
  
  third <- data %>%
    filter(down == 3, play_type %in% c("run", "pass"), !is.na(third_down_converted)) %>%
    group_by(posteam) %>%
    summarise(
      third_down_attempts = n(),
      third_down_conversions = sum(third_down_converted == 1),
      third_down_rate = third_down_conversions / third_down_attempts,
      .groups = "drop"
    )
  
  third_long <- data %>%
    filter(down == 3, ydstogo >= 7, play_type %in% c("run", "pass"),
           !is.na(third_down_converted)) %>%
    group_by(posteam) %>%
    summarise(
      third_long_attempts = n(),
      third_long_conversions = sum(third_down_converted == 1),
      third_long_rate = third_long_conversions / third_long_attempts,
      .groups = "drop"
    )
  
  fourth <- data %>%
    filter(down == 4, play_type %in% c("run", "pass"), !is.na(fourth_down_converted)) %>%
    group_by(posteam) %>%
    summarise(
      fourth_down_attempts = n(),
      fourth_down_conversions = sum(fourth_down_converted == 1),
      fourth_down_rate = fourth_down_conversions / fourth_down_attempts,
      .groups = "drop"
    )
  
  data %>%
    filter(!is.na(posteam)) %>%
    distinct(posteam) %>%
    rename(team = posteam) %>%
    left_join(basic, by = c("team" = "posteam")) %>%
    left_join(third, by = c("team" = "posteam")) %>%
    left_join(third_long, by = c("team" = "posteam")) %>%
    left_join(fourth, by = c("team" = "posteam")) %>%
    mutate(
      across(c(offensive_plays, success_plays, run_plays, pass_plays,
               third_down_attempts, third_down_conversions,
               third_long_attempts, third_long_conversions,
               fourth_down_attempts, fourth_down_conversions),
             ~ tidyr::replace_na(.x, 0)),
      run_epa_rank = min_rank(desc(run_epa)),
      pass_epa_rank = min_rank(desc(pass_epa)),
      success_rate_rank = min_rank(desc(success_rate)),
      third_down_rank = min_rank(desc(third_down_rate)),
      third_long_rank = min_rank(desc(third_long_rate)),
      fourth_down_rank = min_rank(desc(fourth_down_rate))
    )
}

# -------------------------
# Shiny UI
# -------------------------

ui <- fluidPage(
  
  tags$head(
    tags$style(HTML("
      body {
        background: #F4F7FB;
        color: #172B4D;
        font-family: -apple-system, BlinkMacSystemFont,
          'Segoe UI', sans-serif;
      }

      .app-header {
        background: linear-gradient(120deg, #102A43, #1F5A85);
        color: white;
        padding: 24px 28px;
        margin: -15px -15px 22px -15px;
        border-radius: 0 0 14px 14px;
      }

      .app-header-content {
        display: flex;
        align-items: center;
        justify-content: space-between;
        gap: 24px;
      }

      .app-header-text {
        min-width: 0;
      }

      .app-header h1 {
        font-weight: 750;
        margin: 0 0 6px 0;
        font-size: 29px;
      }

      .app-header p {
        margin: 0;
        color: #D9EAF7;
        font-size: 14px;
      }

      .team-logo-container {
        display: flex;
        align-items: center;
        justify-content: center;
        flex: 0 0 auto;
        min-width: 100px;
        min-height: 82px;
        padding: 5px 12px;
        border-radius: 12px;
        background: rgba(255, 255, 255, 0.12);
      }

      .team-logo-container img {
        display: block;
        width: auto;
        height: 76px;
        max-width: 112px;
        object-fit: contain;
        filter: drop-shadow(0 2px 3px rgba(0, 0, 0, 0.2));
      }

      @media (max-width: 600px) {
        .app-header { padding: 18px 18px; }
        .app-header h1 { font-size: 23px; }
        .app-header p { font-size: 12px; }
        .team-logo-container { min-width: 70px; min-height: 62px; padding: 4px 7px; }
        .team-logo-container img { height: 56px; max-width: 76px; }
      }

      .well {
        background: white;
        border: 1px solid #E4EAF1;
        border-radius: 12px;
        box-shadow: 0 3px 12px rgba(16,42,67,.05);
      }

      .panel-card {
        background: white;
        border: 1px solid #E4EAF1;
        border-radius: 12px;
        padding: 18px 20px;
        margin-bottom: 18px;
        box-shadow: 0 3px 12px rgba(16,42,67,.04);
      }

      .panel-card h3 {
        margin-top: 0;
        color: #173B5E;
        font-weight: 700;
        font-size: 19px;
      }

      .section-note {
        color: #667085;
        font-size: 13px;
        margin-bottom: 15px;
      }

      .control-label {
        color: #344054;
        font-weight: 650;
      }

      .form-control {
        border-radius: 8px;
        border-color: #D0D9E4;
        box-shadow: none;
      }

      table {
        background: white;
      }

      .table {
        margin-bottom: 0;
      }

      .table thead th {
        background: #EEF4F9;
        color: #173B5E;
        border-bottom: 1px solid #D8E2EC !important;
      }

      .table-striped > tbody > tr:nth-of-type(odd) {
        background-color: #F8FAFC;
      }

      /* Keep the dashboard tabs visible at the top while scrolling. */
      #main_dashboard_tab > .nav.nav-tabs {
        position: sticky;
        top: 0;
        z-index: 1000;
        background: #FFFFFF;
        padding-top: 8px;
        border-bottom: 2px solid #D8E2EC;
        margin-bottom: 18px;
      }

      #main_dashboard_tab > .nav.nav-tabs > li > a {
        color: #344054;
        font-weight: 700;
        border-radius: 8px 8px 0 0;
      }

      #main_dashboard_tab > .nav.nav-tabs > li.active > a,
      #main_dashboard_tab > .nav.nav-tabs > li.active > a:hover,
      #main_dashboard_tab > .nav.nav-tabs > li.active > a:focus {
        color: #FFFFFF;
        background: var(--team-primary, #173B5E);
        border-color: var(--team-primary, #173B5E);
      }
    ")),
    uiOutput("team_theme")
  ),
  
  div(
    class = "app-header",
    div(
      class = "app-header-content",
      div(
        class = "app-header-text",
        h1("NFL Offensive Analytics"),
        p(
          "Explore passing, rushing, player EPA, touchdown locations, ",
          "red-zone performance, and situational conversion rates."
        )
      ),
      uiOutput("team_logo")
    )
  ),
  
  # Main navigation is at the top level: each tab owns its own controls/content.
  tabsetPanel(
    id = "main_dashboard_tab",
    type = "tabs",
    tabPanel(
      "Team Dashboard",
      # -------------------------
      # Positive EPA predictor
      # -------------------------
      
      div(
        class = "panel-card",
        h3("Positive EPA Play Predictor"),
        p(
          class = "section-note",
          paste(
            "Enter a play situation to estimate the probability",
            "that a run or pass produces positive EPA.",
            "Positive EPA means the play increased expected points;",
            "it does not necessarily mean a touchdown or first down."
          )
        ),
        
        fluidRow(
          column(
            2,
            selectInput(
              "pred_play_type",
              "Play type",
              choices = c("Pass" = "pass", "Rush" = "run"),
              selected = "pass"
            )
          ),
          column(
            2,
            selectInput(
              "pred_down",
              "Down",
              choices = 1:4,
              selected = 3
            )
          ),
          column(
            2,
            numericInput(
              "pred_ydstogo",
              "Yards to go",
              value = 5,
              min = 1,
              max = 99,
              step = 1
            )
          ),
          column(
            3,
            numericInput(
              "pred_yardline",
              "Yards from opponent end zone",
              value = 40,
              min = 1,
              max = 99,
              step = 1
            )
          ),
          column(
            3,
            numericInput(
              "pred_score_diff",
              "Score differential",
              value = 0,
              min = -50,
              max = 50,
              step = 1
            )
          )
        ),
        
        uiOutput("epa_probability_output")
      ),
      sidebarLayout(
        sidebarPanel(
          width = 3,
          h3("Dashboard filters"),
          selectInput(
            "team", "Offensive team", choices = team_choices,
            selected = if ("ARI" %in% team_choices) "ARI" else team_choices[1]
          ),
          selectInput(
            "week", "Week", choices = c("Overall", as.character(available_weeks)),
            selected = "Overall"
          ),
          tags$hr(),
          p(class = "section-note",
            "All charts and tables update with the selected team and week. Overall combines all available weeks."),
          p(class = "section-note",
            "EPA is expected points added; values above zero are positive offensive outcomes relative to the pre-play expectation.")
        ),
        mainPanel(width = 9,
                  # Top players
                  div(
                    class = "panel-card",
                    h3("Top Players by Average EPA Per Play"),
                    p(
                      class = "section-note",
                      paste(
                        "Players are filtered to the selected team and week.",
                        "Rushing-play EPA is assigned to the rusher;",
                        "passing-play EPA is assigned to both the quarterback and",
                        "the identified receiver. At least five qualifying plays are required."
                      )
                    ),
                    tableOutput("player_epa_table")
                  ),
                  
                  # NEW: Touchdown field visualization
                  div(
                    class = "panel-card",
                    h3("Touchdown Starting Field Position"),
                    p(
                      class = "section-note",
                      paste(
                        "Each dot represents a rushing or passing touchdown.",
                        "The dot's horizontal position shows where the play started;",
                        "colors distinguish passing and rushing touchdowns."
                      )
                    ),
                    plotOutput("touchdown_field_plot", height = "350px")
                  ),
                  
                  fluidRow(
                    column(
                      6,
                      div(
                        class = "panel-card",
                        h3("Pass vs. Rush EPA"),
                        plotOutput("epa_plot", height = "250px")
                      )
                    ),
                    
                    column(
                      6,
                      div(
                        class = "panel-card",
                        h3("EPA by Red-Zone Status"),
                        plotOutput("redzone_plot", height = "350px")
                      )
                    )
                  ),
                  
                  div(
                    class = "panel-card",
                    h3("Run vs. Pass EPA in the Red-Zone"),
                    plotOutput("redzone_playtype_plot", height = "300px")
                  ),
                  
                  fluidRow(
                    column(
                      6,
                      div(
                        class = "panel-card",
                        h3("Success Rate"),
                        tableOutput("success_table"),
                        plotOutput("success_plot", height = "300px")
                      )
                    ),
                    
                    column(
                      6,
                      div(
                        class = "panel-card",
                        h3("Third-Down Conversion Rate"),
                        tableOutput("third_down_table"),
                        plotOutput("third_down_plot", height = "300px")
                      )
                    )
                  ),
                  
                  fluidRow(
                    column(
                      6,
                      div(
                        class = "panel-card",
                        h3("Third-and-Long Conversion (7+ yards)"),
                        tableOutput("third_long_table"),
                        plotOutput("third_long_plot", height = "300px")
                      )
                    ),
                    
                    column(
                      6,
                      div(
                        class = "panel-card",
                        h3("Fourth-Down Conversion Rate"),
                        tableOutput("fourth_down_table"),
                        plotOutput("fourth_down_plot", height = "300px")
                      )
                    )
                  )
        )
      )
    ),
    tabPanel(
      "League Rankings",
      # These rankings compare all teams and use their own week selector.
      div(
        class = "panel-card",
        h3("League-wide offensive rankings"),
        p(
          class = "section-note",
          paste(
            "Compare every team, independent of the team selected on the Team Dashboard.",
            "Overall combines all loaded weeks; choosing a week uses that week only.",
            "Each metric is ranked separately, with rank 1 being best."
          )
        ),
        fluidRow(
          column(
            3,
            selectInput(
              "ranking_week", "Ranking period",
              choices = c("Overall", as.character(available_weeks)),
              selected = "Overall"
            )
          ),
          column(9, uiOutput("ranking_period_note"))
        ),
        tableOutput("league_rankings_table")
      ),
      fluidRow(
        column(
          4,
          div(
            class = "panel-card",
            h3("Metric leaderboard"),
            selectInput(
              "ranking_metric", "Metric to compare",
              choices = c(
                "Run EPA" = "run_epa",
                "Pass EPA" = "pass_epa",
                "Offensive success rate" = "success_rate",
                "Third-down conversion rate" = "third_down_rate",
                "Third-and-long conversion rate" = "third_long_rate",
                "Fourth-down conversion rate" = "fourth_down_rate"
              ),
              selected = "run_epa"
            ),
            p(class = "section-note", "Bars use each NFL team's primary color.")
          )
        ),
        column(
          8,
          div(
            class = "panel-card",
            h3("Team comparison"),
            plotOutput("ranking_plot", height = "650px")
          )
        )
      )
    )
  )
)

# ============================================================
# Shiny server
# ============================================================

server <- function(input, output, session) {
  
  # -------------------------
  # Team colors
  # -------------------------
  
  team_palette <- reactive({
    colors <- team_palettes[[input$team]]
    
    if (is.null(colors)) {
      colors <- c("#173B5E", "#E07A35", "#98A2B3")
    }
    
    colors
  })
  
  # -------------------------
  # Selected team's logo
  # -------------------------
  
  output$team_logo <- renderUI({
    # Show the selected team's logo only on the Team Dashboard.
    # The League Rankings tab uses the generic header instead.
    if (!identical(input$main_dashboard_tab, "Team Dashboard")) {
      return(NULL)
    }
    
    req(input$team)
    
    # ESPN's public logo CDN uses a few abbreviations that differ
    # from the abbreviations used by nflreadr/nflverse.
    team_logo_slugs <- c(
      ARI = "ari", ATL = "atl", BAL = "bal", BUF = "buf",
      CAR = "car", CHI = "chi", CIN = "cin", CLE = "cle",
      DAL = "dal", DEN = "den", DET = "det", GB = "gb",
      HOU = "hou", IND = "ind", JAX = "jax", JAC = "jax",
      KC = "kc", LA = "lar", LAR = "lar", LAC = "lac",
      LV = "lv", LVR = "lv", OAK = "lv", MIA = "mia",
      MIN = "min", NE = "ne", NO = "no", NYG = "nyg",
      NYJ = "nyj", PHI = "phi", PIT = "pit", SF = "sf",
      SEA = "sea", TB = "tb", TEN = "ten", WAS = "wsh",
      WSH = "wsh"
    )
    
    logo_slug <- unname(team_logo_slugs[input$team])
    
    if (length(logo_slug) == 0 || is.na(logo_slug)) {
      return(tags$div(class = "team-logo-container"))
    }
    
    logo_url <- paste0(
      "https://a.espncdn.com/i/teamlogos/nfl/500/",
      logo_slug,
      ".png"
    )
    
    tags$div(
      class = "team-logo-container",
      tags$img(
        src = logo_url,
        alt = paste(input$team, "NFL logo"),
        title = paste(input$team, "logo"),
        onerror = "this.style.display='none';"
      )
    )
  })
  
  current_plot_colors <- reactive({
    colors <- team_palette()
    
    c(
      "Pass" = colors[1],
      "Rush" = colors[2],
      "Red Zone" = colors[2],
      "Outside Red Zone" = colors[1],
      "run" = colors[2],
      "pass" = colors[1]
    )
  })
  
  output$team_theme <- renderUI({
    # Use team colors only on the Team Dashboard. Return to the app's
    # original generic navy/blue theme on the League Rankings tab.
    if (identical(input$main_dashboard_tab, "Team Dashboard")) {
      colors <- team_palette()
    } else {
      colors <- c("#102A43", "#1F5A85", "#98A2B3")
    }
    
    tags$style(HTML(sprintf("
      :root {
        --team-primary: %s;
        --team-secondary: %s;
        --team-accent: %s;
      }

      .app-header {
        background: linear-gradient(
          120deg,
          var(--team-primary),
          var(--team-secondary)
        ) !important;
      }

      .panel-card h3,
      .control-label {
        color: var(--team-primary) !important;
      }

      .team-prediction {
        background: color-mix(
          in srgb,
          var(--team-primary) 7%%,
          white
        ) !important;
        border-color: var(--team-secondary) !important;
      }

      .team-prediction-value {
        color: var(--team-primary) !important;
      }
    ",
                            colors[1],
                            colors[2],
                            colors[3]
    )))
  })
  
  # -------------------------
  # Selected team and week
  # -------------------------
  
  selected_pbp <- reactive({
    
    data <- pbp_2026 %>%
      filter(posteam == input$team)
    
    if (input$week != "Overall") {
      data <- data %>%
        filter(week == as.numeric(input$week))
    }
    
    data
  })
  
  # ==========================================================
  # Team-specific logistic regression model
  # ==========================================================
  
  team_epa_model_data <- reactive({
    epa_model_data %>%
      filter(posteam == input$team)
  })
  
  team_epa_success_model <- reactive({
    
    data <- team_epa_model_data()
    
    if (
      nrow(data) < 30 ||
      length(unique(data$success)) < 2 ||
      length(unique(data$play_type)) < 2
    ) {
      return(NULL)
    }
    
    tryCatch(
      glm(
        success ~ play_type + down + ydstogo +
          yardline_100 + score_differential,
        data = data,
        family = binomial(link = "logit")
      ),
      error = function(e) NULL
    )
  })
  
  predicted_epa_probability <- reactive({
    
    req(
      input$pred_play_type,
      input$pred_down,
      input$pred_ydstogo,
      input$pred_yardline,
      input$pred_score_diff
    )
    
    model <- team_epa_success_model()
    
    if (is.null(model)) {
      return(NA_real_)
    }
    
    new_play <- data.frame(
      play_type = factor(
        input$pred_play_type,
        levels = levels(epa_model_data$play_type)
      ),
      down = as.numeric(input$pred_down),
      ydstogo = input$pred_ydstogo,
      yardline_100 = input$pred_yardline,
      score_differential = input$pred_score_diff
    )
    
    as.numeric(
      predict(
        model,
        newdata = new_play,
        type = "response"
      )
    )
  })
  
  output$epa_probability_output <- renderUI({
    
    probability <- predicted_epa_probability()
    training_data <- team_epa_model_data()
    
    if (is.na(probability)) {
      return(
        tags$div(
          class = "team-prediction",
          style = paste(
            "border:1px solid;",
            "border-radius:10px;",
            "padding:16px;",
            "margin-top:10px;"
          ),
          
          tags$div(
            style = "font-weight:700;",
            paste(
              "A team-specific model is not available for",
              input$team
            )
          ),
          
          tags$div(
            style = "font-size:13px; color:#667085;",
            paste(
              "The model needs at least 30 eligible plays, both positive",
              "and non-positive EPA outcomes, and both runs and passes."
            )
          ),
          
          tags$div(
            style = "font-size:13px; color:#667085;",
            paste(
              "Eligible plays for this team:",
              format(nrow(training_data), big.mark = ",")
            )
          )
        )
      )
    }
    
    tags$div(
      class = "team-prediction",
      style = paste(
        "border:1px solid;",
        "border-radius:10px;",
        "padding:16px;",
        "margin-top:10px;"
      ),
      
      tags$div(
        style = "font-size:13px; color:#475467; font-weight:600;",
        paste(
          input$team,
          "— estimated probability of positive EPA"
        )
      ),
      
      tags$div(
        class = "team-prediction-value",
        style = "font-size:32px; font-weight:800;",
        paste0(round(100 * probability, 1), "%")
      ),
      
      tags$div(
        style = "font-size:13px; color:#667085;",
        paste(
          "Model trained on",
          format(nrow(training_data), big.mark = ","),
          "eligible plays from",
          input$team,
          "across all loaded weeks."
        )
      )
    )
  })
  
  # ==========================================================
  # Top players by average EPA
  # ==========================================================
  
  output$player_epa_table <- renderTable({
    
    data <- player_epa %>%
      filter(team == input$team)
    
    if (input$week != "Overall") {
      data <- data %>%
        filter(week == as.numeric(input$week))
    }
    
    result <- data %>%
      group_by(player_id, player_name, team) %>%
      summarise(
        total_plays = n(),
        total_epa = sum(epa, na.rm = TRUE),
        average_epa_per_play = mean(epa, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      filter(total_plays >= 5) %>%
      arrange(desc(average_epa_per_play)) %>%
      slice_head(n = 10) %>%
      transmute(
        Player = player_name,
        Team = team,
        `Qualifying Plays` = total_plays,
        `Total EPA` = round(total_epa, 3),
        `Average EPA per Play` = round(average_epa_per_play, 3)
      )
    
    if (nrow(result) == 0) {
      data.frame(
        Message = paste(
          "No players meet the five-play minimum",
          "for this selection."
        )
      )
    } else {
      result
    }
    
  }, striped = TRUE, bordered = FALSE, spacing = "m",
  hover = TRUE, rownames = FALSE)
  
  # ==========================================================
  # NEW: Touchdown starting field position
  # ==========================================================
  
  output$touchdown_field_plot <- renderPlot({
    
    data <- selected_pbp() %>%
      filter(
        play_type %in% c("run", "pass"),
        touchdown == 1,
        !is.na(yardline_100)
      ) %>%
      mutate(
        # yardline_100 measures distance from the opponent's
        # goal line. Reverse it to measure from the offense's
        # own goal line.
        field_x = 100 - yardline_100,
        
        # Random vertical positions separate overlapping dots.
        field_y = runif(n(), min = 7, max = 46),
        
        play_type = factor(
          play_type,
          levels = c("pass", "run"),
          labels = c("Pass", "Rush")
        )
      )
    
    if (!nrow(data)) {
      return(empty_plot("No touchdown plays available"))
    }
    
    ggplot() +
      
      # Main playing field
      annotate(
        "rect",
        xmin = 0,
        xmax = 100,
        ymin = 0,
        ymax = 53.3,
        fill = "#267A45"
      ) +
      
      # End zones
      annotate(
        "rect",
        xmin = -10,
        xmax = 0,
        ymin = 0,
        ymax = 53.3,
        fill = "#173B5E"
      ) +
      
      annotate(
        "rect",
        xmin = 100,
        xmax = 110,
        ymin = 0,
        ymax = 53.3,
        fill = "#173B5E"
      ) +
      
      # Yard lines
      geom_vline(
        xintercept = seq(0, 100, by = 10),
        color = "white",
        linewidth = 0.8,
        alpha = 0.8
      ) +
      
      # Hash marks
      geom_hline(
        yintercept = c(17.8, 35.5),
        color = "white",
        linewidth = 0.5,
        alpha = 0.5
      ) +
      
      # Yard-line numbers
      annotate(
        "text",
        x = seq(10, 90, by = 10),
        y = 4,
        label = c(10, 20, 30, 40, 50, 40, 30, 20, 10),
        color = "white",
        size = 3.5,
        fontface = "bold"
      ) +
      
      # End-zone labels
      annotate(
        "text",
        x = c(-5, 105),
        y = 26.5,
        label = c("OWN\nEND ZONE", "OPPONENT\nEND ZONE"),
        color = "white",
        fontface = "bold",
        size = 3.2
      ) +
      
      # Touchdown starting positions
      geom_point(
        data = data,
        aes(
          x = field_x,
          y = field_y,
          color = play_type
        ),
        size = 3.5,
        alpha = 0.85
      ) +
      
      scale_color_manual(
        values = current_plot_colors(),
        name = "Touchdown type"
      ) +
      
      coord_fixed(
        xlim = c(-10, 110),
        ylim = c(0, 53.3),
        expand = FALSE
      ) +
      
      labs(
        x = NULL,
        y = NULL,
        title = paste(
          input$team,
          "— touchdown starting positions"
        ),
        subtitle = paste(
          nrow(data), "total touchdowns |",
          sum(data$play_type == "Pass"), "passing |",
          sum(data$play_type == "Rush"), "rushing"
        )
      ) +
      
      theme_void() +
      
      theme(
        plot.title = element_text(
          face = "bold",
          color = team_palette()[1],
          size = 14
        ),
        plot.subtitle = element_text(
          color = "#667085",
          size = 10
        ),
        legend.position = "bottom",
        plot.margin = margin(15, 15, 10, 15)
      )
  })
  
  # ==========================================================
  # Pass vs. rush EPA plot
  # ==========================================================
  
  output$epa_plot <- renderPlot({
    
    data <- selected_pbp() %>%
      filter(play_type %in% c("run", "pass")) %>%
      group_by(play_type) %>%
      summarise(
        mean_epa = mean(epa, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      mutate(
        play_type = factor(
          play_type,
          levels = c("pass", "run"),
          labels = c("Pass", "Rush")
        )
      )
    
    if (!nrow(data)) {
      return(empty_plot())
    }
    
    ggplot(data, aes(play_type, mean_epa, fill = play_type)) +
      geom_hline(
        yintercept = 0,
        color = "#98A2B3",
        linetype = "dashed"
      ) +
      geom_col(width = 0.58, show.legend = FALSE) +
      geom_text(
        aes(
          label = sprintf("%.3f", mean_epa),
          vjust = ifelse(mean_epa >= 0, -0.5, 1.4)
        ),
        color = "#344054",
        size = 4
      ) +
      scale_fill_manual(
        values = current_plot_colors(),
        drop = FALSE
      ) +
      scale_y_continuous(
        expand = expansion(mult = c(0.12, 0.24))
      ) +
      labs(
        x = NULL,
        y = "Mean EPA"
      ) +
      nfl_theme(primary = team_palette()[1]) +
      theme(panel.grid.major.x = element_blank())
  })
  
  # ==========================================================
  # EPA by red-zone status
  # ==========================================================
  
  output$redzone_plot <- renderPlot({
    
    data <- selected_pbp() %>%
      filter(
        play_type %in% c("run", "pass"),
        !is.na(red_zone)
      ) %>%
      group_by(red_zone) %>%
      summarise(
        mean_epa = mean(epa, na.rm = TRUE),
        .groups = "drop"
      )
    
    if (!nrow(data)) {
      return(empty_plot("No red-zone data available"))
    }
    
    ggplot(
      data,
      aes(x = red_zone, y = mean_epa, fill = red_zone)
    ) +
      geom_hline(
        yintercept = 0,
        color = "#98A2B3",
        linetype = "dashed"
      ) +
      geom_col(width = 0.6, show.legend = FALSE) +
      geom_text(
        aes(
          label = sprintf("%.3f", mean_epa),
          vjust = ifelse(mean_epa >= 0, -0.5, 1.4)
        ),
        size = 4,
        color = "#344054"
      ) +
      scale_fill_manual(
        values = current_plot_colors(),
        drop = FALSE
      ) +
      scale_y_continuous(
        expand = expansion(mult = c(0.12, 0.24))
      ) +
      labs(
        x = NULL,
        y = "Mean EPA",
        title = "EPA by Field Position"
      ) +
      nfl_theme(primary = team_palette()[1]) +
      theme(
        axis.text.x = element_text(angle = 10, hjust = 1),
        panel.grid.major.x = element_blank()
      )
  })
  
  # ==========================================================
  # Run vs. pass EPA in red zone
  # ==========================================================
  
  output$redzone_playtype_plot <- renderPlot({
    
    data <- selected_pbp() %>%
      filter(
        red_zone == "Red Zone",
        play_type %in% c("run", "pass")
      ) %>%
      group_by(play_type) %>%
      summarise(
        mean_epa = mean(epa, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      mutate(
        play_type = factor(
          play_type,
          levels = c("pass", "run"),
          labels = c("Pass", "Rush")
        )
      )
    
    if (!nrow(data)) {
      return(empty_plot("No red-zone run/pass data available"))
    }
    
    ggplot(data, aes(play_type, mean_epa, fill = play_type)) +
      geom_hline(
        yintercept = 0,
        color = "#98A2B3",
        linetype = "dashed"
      ) +
      geom_col(width = 0.55, show.legend = FALSE) +
      geom_text(
        aes(
          label = sprintf("%.3f", mean_epa),
          vjust = ifelse(mean_epa >= 0, -0.5, 1.4)
        ),
        size = 4
      ) +
      scale_fill_manual(
        values = current_plot_colors(),
        drop = FALSE
      ) +
      scale_y_continuous(
        expand = expansion(mult = c(0.12, 0.24))
      ) +
      labs(
        x = NULL,
        y = "Mean EPA",
        title = "Red-Zone Play Selection"
      ) +
      nfl_theme(primary = team_palette()[1]) +
      theme(panel.grid.major.x = element_blank())
  })
  
  # ==========================================================
  # Success rate table
  # ==========================================================
  
  output$success_table <- renderTable({
    
    selected_pbp() %>%
      filter(
        play_type %in% c("run", "pass"),
        !is.na(epa)
      ) %>%
      mutate(success = as.integer(epa > 0)) %>%
      group_by(play_type) %>%
      summarise(
        total_plays = n(),
        successful_plays = sum(success),
        success_rate = round(mean(success) * 100, 1),
        .groups = "drop"
      )
    
  }, bordered = FALSE, rownames = FALSE)
  
  # ==========================================================
  # Success rate plot
  # ==========================================================
  
  output$success_plot <- renderPlot({
    
    data <- selected_pbp() %>%
      filter(
        play_type %in% c("run", "pass"),
        !is.na(epa)
      ) %>%
      mutate(success = as.integer(epa > 0)) %>%
      group_by(play_type) %>%
      summarise(
        success_rate = mean(success) * 100,
        .groups = "drop"
      ) %>%
      mutate(
        play_type = factor(
          play_type,
          levels = c("pass", "run"),
          labels = c("Pass", "Rush")
        )
      )
    
    if (!nrow(data)) {
      return(empty_plot("No success-rate data available"))
    }
    
    ggplot(data, aes(play_type, success_rate, fill = play_type)) +
      geom_col(width = 0.58, show.legend = FALSE) +
      geom_text(
        aes(label = sprintf("%.1f%%", success_rate)),
        vjust = -0.5,
        size = 4
      ) +
      scale_fill_manual(
        values = current_plot_colors(),
        drop = FALSE
      ) +
      scale_y_continuous(
        limits = c(0, 110),
        breaks = seq(0, 100, 20),
        expand = expansion(mult = c(0, 0.02))
      ) +
      labs(
        x = NULL,
        y = "Successful plays (%)",
        title = "Success Rate by Play-Type"
      ) +
      nfl_theme(primary = team_palette()[1]) +
      theme(panel.grid.major.x = element_blank())
  })
  
  # ==========================================================
  # Third-down conversion table
  # ==========================================================
  
  output$third_down_table <- renderTable({
    
    data <- selected_pbp() %>%
      filter(
        down == 3,
        play_type %in% c("run", "pass"),
        !is.na(third_down_converted)
      )
    
    if (!nrow(data)) {
      return(data.frame(Message = "No third-down data available"))
    }
    
    data %>%
      group_by(play_type) %>%
      summarise(
        total_plays = n(),
        conversions = sum(third_down_converted == 1),
        conversion_rate = round(
          mean(third_down_converted == 1) * 100,
          1
        ),
        .groups = "drop"
      )
    
  }, bordered = FALSE, rownames = FALSE)
  
  # ==========================================================
  # Third-down conversion plot
  # ==========================================================
  
  output$third_down_plot <- renderPlot({
    
    data <- selected_pbp() %>%
      filter(
        down == 3,
        play_type %in% c("run", "pass"),
        !is.na(third_down_converted)
      ) %>%
      group_by(play_type) %>%
      summarise(
        conversion_rate = mean(third_down_converted == 1) * 100,
        .groups = "drop"
      ) %>%
      mutate(
        play_type = factor(
          play_type,
          levels = c("pass", "run"),
          labels = c("Pass", "Rush")
        )
      )
    
    if (!nrow(data)) {
      return(empty_plot("No third-down data available"))
    }
    
    ggplot(data, aes(play_type, conversion_rate, fill = play_type)) +
      geom_col(width = 0.58, show.legend = FALSE) +
      geom_text(
        aes(label = sprintf("%.1f%%", conversion_rate)),
        vjust = -0.5,
        size = 4
      ) +
      scale_fill_manual(
        values = current_plot_colors(),
        drop = FALSE
      ) +
      scale_y_continuous(
        limits = c(0, 110),
        breaks = seq(0, 100, 20),
        expand = expansion(mult = c(0, 0.02))
      ) +
      labs(
        x = NULL,
        y = "Conversion rate (%)",
        title = "Third-Down Conversions"
      ) +
      nfl_theme(primary = team_palette()[1]) +
      theme(panel.grid.major.x = element_blank())
  })
  
  # ==========================================================
  # Third-and-long conversion table
  # ==========================================================
  
  output$third_long_table <- renderTable({
    
    data <- selected_pbp() %>%
      filter(
        down == 3,
        ydstogo >= 7,
        play_type %in% c("run", "pass"),
        !is.na(third_down_converted)
      )
    
    if (!nrow(data)) {
      return(data.frame(Message = "No third-and-long data available"))
    }
    
    data %>%
      group_by(play_type) %>%
      summarise(
        total_plays = n(),
        conversions = sum(third_down_converted == 1),
        conversion_rate = round(
          mean(third_down_converted == 1) * 100,
          1
        ),
        .groups = "drop"
      )
    
  }, bordered = FALSE, rownames = FALSE)
  
  # ==========================================================
  # Third-and-long conversion plot
  # ==========================================================
  
  output$third_long_plot <- renderPlot({
    
    data <- selected_pbp() %>%
      filter(
        down == 3,
        ydstogo >= 7,
        play_type %in% c("run", "pass"),
        !is.na(third_down_converted)
      ) %>%
      group_by(play_type) %>%
      summarise(
        conversion_rate = mean(third_down_converted == 1) * 100,
        .groups = "drop"
      ) %>%
      mutate(
        play_type = factor(
          play_type,
          levels = c("pass", "run"),
          labels = c("Pass", "Rush")
        )
      )
    
    if (!nrow(data)) {
      return(empty_plot("No third-and-long data available"))
    }
    
    ggplot(data, aes(play_type, conversion_rate, fill = play_type)) +
      geom_col(width = 0.58, show.legend = FALSE) +
      geom_text(
        aes(label = sprintf("%.1f%%", conversion_rate)),
        vjust = -0.5,
        size = 4
      ) +
      scale_fill_manual(
        values = current_plot_colors(),
        drop = FALSE
      ) +
      scale_y_continuous(
        limits = c(0, 110),
        breaks = seq(0, 100, 20),
        expand = expansion(mult = c(0, 0.02))
      ) +
      labs(
        x = NULL,
        y = "Conversion rate (%)",
        title = "Third-and-long conversions"
      ) +
      nfl_theme(primary = team_palette()[1]) +
      theme(panel.grid.major.x = element_blank())
  })
  
  # ==========================================================
  # Fourth-down conversion table
  # ==========================================================
  
  output$fourth_down_table <- renderTable({
    
    data <- selected_pbp() %>%
      filter(
        down == 4,
        play_type %in% c("run", "pass"),
        !is.na(fourth_down_converted)
      )
    
    if (!nrow(data)) {
      return(data.frame(Message = "No fourth-down data available"))
    }
    
    data %>%
      group_by(play_type) %>%
      summarise(
        total_plays = n(),
        conversions = sum(fourth_down_converted == 1),
        conversion_rate = round(
          mean(fourth_down_converted == 1) * 100,
          1
        ),
        .groups = "drop"
      )
    
  }, bordered = FALSE, rownames = FALSE)
  
  # ==========================================================
  # Fourth-down conversion plot
  # ==========================================================
  
  output$fourth_down_plot <- renderPlot({
    
    data <- selected_pbp() %>%
      filter(
        down == 4,
        play_type %in% c("run", "pass"),
        !is.na(fourth_down_converted)
      ) %>%
      group_by(play_type) %>%
      summarise(
        conversion_rate = mean(fourth_down_converted == 1) * 100,
        .groups = "drop"
      ) %>%
      mutate(
        play_type = factor(
          play_type,
          levels = c("pass", "run"),
          labels = c("Pass", "Rush")
        )
      )
    
    if (!nrow(data)) {
      return(empty_plot("No fourth-down data available"))
    }
    
    ggplot(data, aes(play_type, conversion_rate, fill = play_type)) +
      geom_col(width = 0.58, show.legend = FALSE) +
      geom_text(
        aes(label = sprintf("%.1f%%", conversion_rate)),
        vjust = -0.5,
        size = 4
      ) +
      scale_fill_manual(
        values = current_plot_colors(),
        drop = FALSE
      ) +
      scale_y_continuous(
        limits = c(0, 110),
        breaks = seq(0, 100, 20),
        expand = expansion(mult = c(0, 0.02))
      ) +
      labs(
        x = NULL,
        y = "Conversion rate (%)",
        title = "Fourth-down conversions"
      ) +
      nfl_theme(primary = team_palette()[1]) +
      theme(panel.grid.major.x = element_blank())
  })
  
  # ==========================================================
  # League-wide rankings
  # These use ranking_week, not input$team or input$week.
  # ==========================================================
  
  ranking_data <- reactive({
    data <- pbp_2026
    if (input$ranking_week != "Overall") {
      data <- data %>% filter(week == as.numeric(input$ranking_week))
    }
    calculate_team_rankings(data)
  })
  
  output$ranking_period_note <- renderUI({
    if (input$ranking_week == "Overall") {
      p(class = "section-note", "Ranking period: all available weeks combined.")
    } else {
      p(class = "section-note", paste("Ranking period: Week", input$ranking_week, "only."))
    }
  })
  
  output$league_rankings_table <- renderTable({
    x <- ranking_data()
    if (!nrow(x)) return(data.frame(Message = "No league ranking data available for this period."))
    
    x %>%
      arrange(run_epa_rank) %>%
      transmute(
        Team = team,
        `Run EPA` = round(run_epa, 3), `Run EPA Rank` = run_epa_rank,
        `Pass EPA` = round(pass_epa, 3), `Pass EPA Rank` = pass_epa_rank,
        `Success Rate` = ifelse(is.na(success_rate), NA, paste0(round(100 * success_rate, 1), "%")),
        `Success Plays / Plays` = paste(success_plays, "/", offensive_plays),
        `3rd Down` = ifelse(is.na(third_down_rate), NA, paste0(round(100 * third_down_rate, 1), "%")),
        `3rd Down Record` = paste(third_down_conversions, "/", third_down_attempts),
        `3rd-and-Long` = ifelse(is.na(third_long_rate), NA, paste0(round(100 * third_long_rate, 1), "%")),
        `3rd-and-Long Record` = paste(third_long_conversions, "/", third_long_attempts),
        `4th Down` = ifelse(is.na(fourth_down_rate), NA, paste0(round(100 * fourth_down_rate, 1), "%")),
        `4th Down Record` = paste(fourth_down_conversions, "/", fourth_down_attempts)
      )
  }, striped = TRUE, bordered = FALSE, hover = TRUE, rownames = FALSE, spacing = "s")
  
  output$ranking_plot <- renderPlot({
    x <- ranking_data()
    metric <- input$ranking_metric
    validate(
      need(nrow(x) > 0, "No ranking data available for this period."),
      need(metric %in% names(x), "Choose a valid metric.")
    )
    
    x <- x %>% filter(!is.na(.data[[metric]])) %>% arrange(.data[[metric]])
    if (!nrow(x)) return(empty_plot("No data available for this metric"))
    
    rate_metric <- metric %in% c(
      "success_rate", "third_down_rate", "third_long_rate", "fourth_down_rate"
    )
    x <- x %>%
      mutate(
        plot_value = if (rate_metric) 100 * .data[[metric]] else .data[[metric]],
        team = factor(team, levels = team),
        # Use the primary color from each team's palette. This is deliberately
        # separate from team_palette(), which controls the selected-team dashboard.
        bar_color = vapply(as.character(team), function(team_name) {
          palette <- team_palettes[[team_name]]
          if (is.null(palette)) "#173B5E" else palette[1]
        }, character(1))
      )
    
    metric_label <- switch(
      metric,
      run_epa = "Average EPA per rushing play",
      pass_epa = "Average EPA per passing play",
      success_rate = "Offensive success rate (%)",
      third_down_rate = "Third-down conversion rate (%)",
      third_long_rate = "Third-and-long conversion rate (%)",
      fourth_down_rate = "Fourth-down conversion rate (%)"
    )
    
    # Place labels outside the bars so values remain readable.
    label_offset <- max(abs(x$plot_value), na.rm = TRUE) * 0.025
    if (!is.finite(label_offset) || label_offset == 0) label_offset <- 0.02
    x <- x %>% mutate(
      label_position = plot_value + ifelse(plot_value >= 0, label_offset, -label_offset),
      value_label = if (rate_metric) sprintf("%.1f%%", plot_value) else sprintf("%.3f", plot_value),
      label_alignment = ifelse(plot_value >= 0, 0, 1)
    )
    
    ggplot(x, aes(team, plot_value, fill = bar_color)) +
      geom_col(show.legend = FALSE, width = 0.72) +
      geom_text(
        aes(y = label_position, label = value_label, hjust = label_alignment),
        size = 3.2, color = "#263445"
      ) +
      coord_flip(clip = "off") +
      scale_fill_identity() +
      scale_y_continuous(expand = expansion(mult = c(0.16, 0.24))) +
      labs(x = NULL, y = metric_label, title = paste("Team comparison:", metric_label)) +
      nfl_theme() +
      theme(
        axis.text.y = element_text(face = "bold"),
        panel.grid.major.y = element_blank(),
        plot.margin = margin(12, 55, 12, 55)
      )
  })
}

# Launch the Shiny app
shinyApp(ui = ui, server = server)
