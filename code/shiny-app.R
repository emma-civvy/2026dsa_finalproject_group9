# app.R
library(shiny)
library(tidyverse)
library(bslib)
library(yardstick)

# --------------------------------------------------
# LOAD DATA
# --------------------------------------------------
train_df <- read_csv("../data/shinyapp/train-df.csv")
test_df  <- read_csv("../data/shinyapp/test_df.csv")
final_df <- read_csv("../data/shinyapp/final_df.csv")
vi_df    <- read_csv("../data/shinyapp/vi_df.csv")

# --------------------------------------------------
# VARIABLE GROUPS
# --------------------------------------------------
weather_vars <- c(
  "mean_dayl_s",
  "mean_srad_w_m_2",
  "mean_tmax_deg_c",
  "mean_tmin_deg_c",
  "mean_vp_pa",
  "sum_prcp_mm_day"
)

soil_vars <- c(
  "soilpH",
  "om_pct",
  "soilk_ppm",
  "soilp_ppm"
)

# --------------------------------------------------
# METRICS
# --------------------------------------------------
r2_val   <- rsq_vec(test_df$yield_mg_ha, test_df$.pred)
rmse_val <- rmse_vec(test_df$yield_mg_ha, test_df$.pred)

# --------------------------------------------------
# UI
# --------------------------------------------------
ui <- navbarPage(
  
  title = "Crop Yield Model Dashboard",
  
  theme = bs_theme(
    version = 5,
    bootswatch = "flatly",
    bg = "#e0e0e0",
    fg = "#111111",
    primary = "#3a3a3a"
  ),
  
  # ---------------- HOME ----------------
  tabPanel(
    "Home",
    
    fluidPage(
      br(),
      h2("Crop Yield Prediction Dashboard"),
      p("Exploration of yield patterns and model performance."),
      br(),
      
      fluidRow(
        column(4, card(card_header("R²"), h3(round(r2_val, 3)))),
        column(4, card(card_header("RMSE"), h3(round(rmse_val, 3)))),
        column(4, card(card_header("Training Rows"), h3(nrow(train_df))))
      )
    )
  ),
  
  # ---------------- MODEL ----------------
  tabPanel(
    "Model Performance",
    
    fluidPage(
      br(),
      fluidRow(
        column(6, plotOutput("pred_obs_plot", height = "500px")),
        column(6, plotOutput("vi_plot", height = "500px"))
      )
    )
  ),
  
  # ---------------- EDA ----------------
  tabPanel(
    "EDA",
    
    sidebarLayout(
      
      sidebarPanel(
        selectInput("soil_var", "Soil Variable:", soil_vars, selected = "om_pct"),
        selectInput("weather_var", "Weather Variable:", weather_vars, selected = "mean_srad_w_m_2")
      ),
      
      mainPanel(
        plotOutput("reg_plot", height = "350px"),
        br(),
        fluidRow(
          column(6, plotOutput("soil_density")),
          column(6, plotOutput("weather_density"))
        )
      )
    )
  ),
  
  
  # ---------------- YIELD EXPLORER ----------------
  tabPanel(
    "Yield Explorer",
    
    sidebarLayout(
      
      sidebarPanel(
        
        selectizeInput(
          "hybrid",
          "Select Hybrid(s):",
          choices = sort(unique(final_df$hybrid)),
          multiple = TRUE,
          options = list(
            placeholder = "Select hybrids to compare",
            maxOptions = 1000
          )
        )
      ),
      
      mainPanel(
        plotOutput("yield_density", height = "600px")
      )
    )
  ),
  
  # ---------------- ABOUT ----------------
  tabPanel(
    "About",
    
    fluidPage(
      br(),
      h3("About"),
      p("This app presents model results, exploratory analysis, and yield distributions across hybrids."),
      tags$ul(
        tags$li("EDA on training data"),
        tags$li("Model performance evaluation"),
        tags$li("Variable importance"),
        tags$li("Interactive hybrid comparison")
      )
    )
  )
)

# --------------------------------------------------
# SERVER
# --------------------------------------------------
server <- function(input, output, session) {
  
  # ---------------- REGRESSION ----------------
  output$reg_plot <- renderPlot({
    
    ggplot(train_df,
           aes(x = mean_srad_w_m_2, y = yield_mg_ha)) +
      geom_point(alpha = 0.15) +
      geom_smooth(method = "lm", color = "blue") +
      labs(
        title = "Yield vs Solar Radiation",
        x = "mean_srad_w_m_2",
        y = "Yield"
      ) +
      theme_minimal()
  })
  
  # ---------------- SOIL DENSITY ----------------
  output$soil_density <- renderPlot({
    
    ggplot(train_df,
           aes(x = .data[[input$soil_var]])) +
      geom_density(fill = "forestgreen", alpha = 0.5) +
      labs(title = paste("Density:", input$soil_var)) +
      theme_minimal()
  })
  
  # ---------------- WEATHER DENSITY ----------------
  output$weather_density <- renderPlot({
    
    ggplot(train_df,
           aes(x = .data[[input$weather_var]])) +
      geom_density(fill = "steelblue", alpha = 0.5) +
      labs(title = paste("Density:", input$weather_var)) +
      theme_minimal()
  })
  
  # ---------------- PRED VS OBS ----------------
  output$pred_obs_plot <- renderPlot({
    
    ggplot(test_df,
           aes(x = yield_mg_ha, y = .pred)) +
      geom_point(alpha = 0.3) +
      geom_abline(linetype = 2) +
      geom_smooth(method = "lm", color = "red") +
      
      annotate(
        "text",
        x = Inf, y = Inf,
        label = paste0(
          "R² = ", round(r2_val, 3),
          "\nRMSE = ", round(rmse_val, 3)
        ),
        hjust = 1.1,   # push slightly inside
        vjust = 1.5,
        size = 5,
        color = "blue"
      ) +
      
      labs(
        title = "Predicted vs Observed",
        x = "Observed",
        y = "Predicted"
      ) +
      theme_minimal()
  })
  
  # ---------------- VARIABLE IMPORTANCE ----------------
  output$vi_plot <- renderPlot({
    
    ggplot(vi_df,
           aes(x = Importance,
               y = reorder(Variable, Importance))) +
      geom_col(fill = "darkorange") +
      labs(title = "Variable Importance", y = NULL) +
      theme_minimal()
  })
  
  # ---------------- YIELD DENSITY ----------------
  output$yield_density <- renderPlot({
    
    req(input$hybrid)
    
    dat <- final_df %>%
      filter(hybrid %in% input$hybrid)
    
    ggplot(dat,
           aes(x = yield_mg_ha,
               fill = hybrid,
               color = hybrid)) +
      geom_density(alpha = 0.3, linewidth = 1) +
      labs(
        title = "Yield Distribution by Hybrid",
        x = "Yield",
        y = "Density"
      ) +
      theme_minimal()
  })
}

# --------------------------------------------------
shinyApp(ui = ui, server = server)