###############################
#      SPORTS GENOMICS APP
###############################

library(shiny)
library(tidyverse)
library(fmsb)         # kept in case you use it elsewhere, not needed for radar now
library(shinythemes)

# =======================
# SNP SCORING FUNCTION
# =======================
score_snp <- function(genotype) {
  dplyr::case_when(
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
    readr::read_csv(input$geno_file$datapath, show_col_types = FALSE)
  })
  
  perf_data <- reactive({
    req(input$perf_file)
    readr::read_csv(input$perf_file$datapath, show_col_types = FALSE)
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
    dplyr::left_join(geno_data(), perf_data(), by = "Athlete_ID")
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
    ids <- merged()$Athlete_ID
    updateSelectInput(session, "athlete_select",
                      choices = ids)
    updateSelectInput(session, "athlete_insights",
                      choices = ids)
  })
  
  ###############################
  # SCORING
  ###############################
  
  scored <- reactive({
    df <- merged()
    
    df$ACTN3_score    <- score_snp(df$ACTN3)
    df$ACE_score      <- score_snp(df$ACE)
    df$PPARGC1A_score <- score_snp(df$PPARGC1A)
    
    df$TGS <- (100/6) * (df$ACTN3_score + df$ACE_score + df$PPARGC1A_score)
    
    df$Category <- dplyr::case_when(
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
           y = "Score (0–100)",
           x = "Athlete")
  })
  
  ###############################
  # RADAR PLOT — custom diamond radar (no fmsb)
  ###############################
  
  output$radar_plot <- renderPlot({
    req(input$athlete_select)
    
    df <- scored()
    metric_cols <- c("CMJ_cm", "Sprint10m_s", "YoYo_IR1_m", "HRV_RMSSD")
    
    # 1) Athlete row ----------------------------------------------------------
    athlete <- df %>%
      dplyr::filter(Athlete_ID == input$athlete_select) %>%
      dplyr::select(all_of(metric_cols)) %>%
      dplyr::mutate(across(everything(), as.numeric))
    req(nrow(athlete) == 1)
    
    # 2) Team metrics (for scaling & team mean) -------------------------------
    team_metrics <- df %>%
      dplyr::select(all_of(metric_cols)) %>%
      dplyr::mutate(across(everything(), as.numeric))
    
    min_vals  <- apply(team_metrics, 2, min,  na.rm = TRUE)
    max_vals  <- apply(team_metrics, 2, max,  na.rm = TRUE)
    team_mean <- apply(team_metrics, 2, mean, na.rm = TRUE)
    
    range_vals <- pmax(max_vals - min_vals, 1e-6)
    
    # scale both athlete + team to 0–1
    athlete_scaled <- (as.numeric(athlete[1, ]) - min_vals) / range_vals
    team_scaled    <- (team_mean - min_vals) / range_vals
    
    # 3) Geometry for diamond radar -----------------------------------------
    labels <- metric_cols
    n <- length(labels)
    
    # angles for 4 axes, rotated a bit for a diamond look
    angles <- seq(0, 2 * pi, length.out = n + 1)[1:n] + pi / 4
    
    # helper to convert r (0–1) to x,y
    coords <- function(r) {
      x <- r * cos(angles)
      y <- r * sin(angles)
      list(x = x, y = y)
    }
    
    # 4) Base plot setup ------------------------------------------------------
    op <- par(mar = c(2, 2, 4, 2))
    plot(0, 0,
         type = "n",
         xlim = c(-1.2, 1.2),
         ylim = c(-1.2, 1.2),
         axes = FALSE, xlab = "", ylab = "",
         asp = 1,
         main = paste("Performance Radar —", input$athlete_select))
    
    # grid (0.25, 0.5, 0.75, 1.0)
    seg <- 4
    for (s in seq(1 / seg, 1, by = 1 / seg)) {
      c <- coords(rep(s, n))
      polygon(c$x, c$y, border = "grey90")
    }
    
    # axes
    axis_coords <- coords(rep(1, n))
    segments(0, 0, axis_coords$x, axis_coords$y, col = "grey80")
    
    # metric labels
    text(axis_coords$x * 1.1, axis_coords$y * 1.1,
         labels = labels, cex = 1)
    
    # numeric rings (top side)
    for (s in seq(0.25, 1, by = 0.25)) {
      text(0, s, labels = paste0(round(s * 100), " (%)"),
           cex = 0.7, col = "grey50", pos = 3)
    }
    
    # 5) Polygons -------------------------------------------------------------
    ath <- coords(athlete_scaled)
    tm  <- coords(team_scaled)
    
    polygon(ath$x, ath$y,
            border = "#2980b9",
            col    = scales::alpha("#2980b9", 0.35),
            lwd    = 3)
    
    polygon(tm$x, tm$y,
            border = "black",
            col    = NA,
            lwd    = 2,
            lty    = 2)
    
    # 6) Legend ---------------------------------------------------------------
    legend("bottom",
           legend = c(input$athlete_select, "Team Avg"),
           col    = c("#2980b9", "black"),
           lty    = c(1, 2),
           lwd    = c(3, 2),
           horiz  = TRUE,
           bty    = "n")
    
    par(op)
  })
  
  
  ###############################
  # BAR + DOT PLOT
  ###############################
  
  output$bar_dot_plot <- renderPlot({
    df <- scored()
    
    ggplot(df, aes(x = Athlete_ID)) +
      geom_col(aes(y = TGS, fill = Category), color = "black") +
      geom_point(aes(y = CMJ_cm), size = 4) +
      scale_fill_manual(values = c("High" = "#2ecc71",
                                   "Moderate" = "#f39c12",
                                   "Low" = "#e74c3c")) +
      theme_minimal(base_size = 14) +
      labs(y = "TGS (bars) / CMJ (dots)",
           x = "Athlete")
  })
  
  ###############################
  # INSIGHTS TAB
  ###############################
  
  output$profile_table <- renderTable({
    req(input$athlete_insights)
    scored() %>% dplyr::filter(Athlete_ID == input$athlete_insights)
  })
  
  output$insights_text <- renderText({
    req(input$athlete_insights)
    athlete <- scored() %>% dplyr::filter(Athlete_ID == input$athlete_insights)
    
    paste0(
      "Genotype Category: ", athlete$Category, "\n",
      "TGS Score: ", round(athlete$TGS, 1), "\n\n",
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
