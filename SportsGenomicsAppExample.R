###############################
#      SPORTS GENOMICS APP
###############################

library(shiny)
library(tidyverse)
library(fmsb)         # for radar charts
library(shinythemes)

# =======================
# SNP SCORING FUNCTION
# =======================
score_snp <- function(genotype) {
  case_when(
    genotype %in% c("RR","DD","GG") ~ 2,
    genotype %in% c("RX","ID","GA") ~ 1,
    genotype %in% c("XX","II","AA") ~ 0,
    TRUE ~ NA_real_
  )
}

# =======================
# SHINY UI
# =======================

ui <- navbarPage(
  "Sports Genomics Dashboard",
  theme = shinytheme("flatly"),
  
  ######## TAB 1 — UPLOAD DATA ########
  tabPanel("1. Upload Data",
           sidebarLayout(
             sidebarPanel(
               fileInput("geno_file", "Upload Genotype CSV", accept = ".csv"),
               fileInput("perf_file", "Upload Performance CSV", accept = ".csv"),
               hr(),
               h4("Instructions:"),
               tags$ul(
                 tags$li("Both files must contain Athlete_ID column"),
                 tags$li("Genotype file must include: ACTN3, ACE, PPARGC1A"),
                 tags$li("Performance file must include: CMJ_cm, Sprint10m_s, YoYo_IR1_m, HRV_RMSSD, RPE")
               )
             ),
             mainPanel(
               h3("Preview Genotype Data"),
               tableOutput("geno_preview"),
               hr(),
               h3("Preview Performance Data"),
               tableOutput("perf_preview"),
               hr(),
               uiOutput("merge_status")
             )
           )
  ),
  
  ######## TAB 2 — GENETIC SCORING ########
  tabPanel("2. Genetic Scoring",
           sidebarLayout(
             sidebarPanel(
               h4("Scoring Logic"),
               tags$ul(
                 tags$li("RR / DD / GG → 2"),
                 tags$li("RX / ID / GA → 1"),
                 tags$li("XX / II / AA → 0")
               )
             ),
             mainPanel(
               h3("Genetic Scores (TGS)"),
               tableOutput("score_table"),
               hr(),
               h3("Genetic Power Score Plot"),
               plotOutput("tgs_plot")
             )
           )
  ),
  
  ######## TAB 3 — PERFORMANCE VISUALS ########
  tabPanel("3. Performance Visuals",
           sidebarLayout(
             sidebarPanel(
               selectInput("athlete_select", "Select Athlete:",
                           choices = NULL, selected = NULL)
             ),
             mainPanel(
               h3("Radar Chart: Athlete vs Team"),
               plotOutput("radar_plot", height = "500px"),
               hr(),
               h3("TGS vs CMJ Performance"),
               plotOutput("bar_dot_plot")
             )
           )
  ),
  
  ######## TAB 4 — INSIGHTS ########
  tabPanel("4. Insights",
           sidebarLayout(
             sidebarPanel(
               selectInput("athlete_insights", "Select Athlete:",
                           choices = NULL, selected = NULL)
             ),
             mainPanel(
               h3("Athlete Profile"),
               tableOutput("profile_table"),
               hr(),
               h3("Interpretation"),
               verbatimTextOutput("insights_text")
             )
           )
  )
)

# =======================
# SERVER
# =======================

server <- function(input, output, session) {
  
  ###############################
  # Load Uploaded Data
  ###############################
  
  geno_data <- reactive({
    req(input$geno_file)
    read_csv(input$geno_file$datapath)
  })
  
  perf_data <- reactive({
    req(input$perf_file)
    read_csv(input$perf_file$datapath)
  })
  
  output$geno_preview <- renderTable({
    head(geno_data())
  })
  
  output$perf_preview <- renderTable({
    head(perf_data())
  })
  
  ###############################
  # Merge Data
  ###############################
  
  merged <- reactive({
    req(geno_data(), perf_data())
    left_join(geno_data(), perf_data(), by = "Athlete_ID")
  })
  
  output$merge_status <- renderUI({
    if (nrow(merged()) > 0) {
      tagList(
        h4("Data Successfully Merged ✔️"),
        tableOutput("merged_preview")
      )
    }
  })
  
  output$merged_preview <- renderTable({
    head(merged())
  })
  
  # Update dropdown choices
  observe({
    updateSelectInput(session, "athlete_select",
                      choices = merged()$Athlete_ID)
    updateSelectInput(session, "athlete_insights",
                      choices = merged()$Athlete_ID)
  })
  
  ###############################
  # SCORING
  ###############################
  
  scored <- reactive({
    df <- merged()
    
    df$ACTN3_score <- score_snp(df$ACTN3)
    df$ACE_score <- score_snp(df$ACE)
    df$PPARGC1A_score <- score_snp(df$PPARGC1A)
    
    df$TGS <- (100/6) * (df$ACTN3_score + df$ACE_score + df$PPARGC1A_score)
    
    df$Category <- case_when(
      df$TGS >= 67 ~ "High",
      df$TGS >= 34 ~ "Moderate",
      TRUE ~ "Low"
    )
    
    df
  })
  
  output$score_table <- renderTable({
    scored()
  })
  
  ###############################
  # PLOT — TGS BAR PLOT
  ###############################
  
  output$tgs_plot <- renderPlot({
    df <- scored()
    
    ggplot(df, aes(x = Athlete_ID, y = TGS, fill = Category)) +
      geom_col() +
      scale_fill_manual(values = c("High" = "#2ecc71",
                                   "Moderate" = "#f39c12",
                                   "Low" = "#e74c3c")) +
      theme_minimal(base_size = 14) +
      labs(title = "Total Genetic Score (TGS)",
           y = "Score (0–100)")
  })
  
  ###############################
  # RADAR PLOT
  ###############################
  
  output$radar_plot <- renderPlot({
    req(input$athlete_select)
    
    df <- scored()
    athlete <- df %>% filter(Athlete_ID == input$athlete_select)
    
    # Metrics for radar
    metrics <- df %>%
      select(CMJ_cm, Sprint10m_s, YoYo_IR1_m, HRV_RMSSD)
    
    # Normalize
    min_vals <- apply(metrics, 2, min)
    max_vals <- apply(metrics, 2, max)
    team_avg <- apply(metrics, 2, mean)
    
    athlete_scaled <- (athlete %>% select(CMJ_cm, Sprint10m_s, YoYo_IR1_m, HRV_RMSSD) - min_vals) / 
      (max_vals - min_vals)
    
    team_scaled <- (team_avg - min_vals) / (max_vals - min_vals)
    
    radar_df <- rbind(max = rep(1, 4),
                      min = rep(0, 4),
                      athlete_scaled,
                      team_scaled)
    
    radarchart(radar_df,
               axistype = 1,
               pcol = c(NA, NA, "#3498db", "#2c3e50"),
               pfcol = c(NA, NA, scales::alpha("#3498db", 0.4),
                         scales::alpha("#2c3e50", 0.4)),
               plwd = 3)
  })
  
  ###############################
  # BAR + DOT PLOT
  ###############################
  
  ######## COMBINED BAR + DOT PLOT ########
  
  output$bar_dot_plot <- renderPlot({
    df <- scored()
    
    ggplot(df, aes(x = Athlete_ID)) +
      geom_col(aes(y = TGS, fill = Category), color = "black") +
      geom_point(aes(y = CMJ_cm), size = 4) +
      scale_fill_manual(values = c("High" = "#2ecc71",
                                   "Moderate" = "#f39c12",
                                   "Low" = "#e74c3c")) +
      theme_minimal(base_size = 14) +
      labs(y = "TGS (bars) / CMJ (dots)")
  })
  
  ###############################
  # INSIGHTS TAB
  ###############################
  
  output$profile_table <- renderTable({
    req(input$athlete_insights)
    scored() %>% filter(Athlete_ID == input$athlete_insights)
  })
  
  output$insights_text <- renderText({
    athlete <- scored() %>% filter(Athlete_ID == input$athlete_insights)
    
    paste0(
      "Genotype Category: ", athlete$Category, "\n",
      "TGS Score: ", round(athlete$TGS,1), "\n\n",
      "CMJ: ", athlete$CMJ_cm, " cm\n",
      "Sprint 10m: ", athlete$Sprint10m_s, " s\n",
      "YoYo IR1: ", athlete$YoYo_IR1_m, " m\n",
      "HRV RMSSD: ", athlete$HRV_RMSSD, "\n\n",
      "Interpretation:\n",
      "This athlete shows a ", athlete$Category, 
      " genetic profile. Compare performance values to team averages to assess alignment.\n",
      "Use caution: genetics indicate tendencies, not outcomes."
    )
  })
  
}

shinyApp(ui, server)
