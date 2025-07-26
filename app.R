# Add these to your library imports at the top
library(shiny)
library(survival)
library(survminer)
library(tidyverse)
library(lubridate)
library(gridExtra)
library(grid)
library(data.table)
library(grDevices) # For PDF generation

# Create template CSV function
create_template_csv <- function() {
  df <- data.frame(
    Patient_ID = c("1", "2", "3", "4", "5", "6"),
    Treatment = c("Chemotherapy", "Immunotherapy", "Chemotherapy", 
                  "Immunotherapy", "Standard Care", "Standard Care"),
    Date_of_Diagnosis = c("21/03/2023", "15/04/2023", "01/05/2023",
                          "10/06/2023", "22/07/2023", "30/08/2023"),
    Date_of_Last_follow_up = c("15/12/2023", "20/12/2023", "31/12/2023",
                               "15/01/2024", "20/01/2024", "25/01/2024"),
    Dead_Alive = c("Alive", "Dead", "Alive", "Dead", "Alive", "Alive"),
    stringsAsFactors = FALSE
  )
  return(df)
}

# Your existing preprocess_data function remains the same
preprocess_data <- function(data) {
  setDT(data)
  data[, Date_of_Diagnosis := as.POSIXct(parse_date_time(Date_of_Diagnosis, 
                                                         orders = c("dmy", "mdy", "ymd")))]
  data[, Date_of_Last_follow_up := as.POSIXct(parse_date_time(Date_of_Last_follow_up, 
                                                              orders = c("dmy", "mdy", "ymd")))]
  data[, survival_time := as.numeric(difftime(Date_of_Last_follow_up, 
                                              Date_of_Diagnosis, 
                                              units = "days")) / 30.44]
  data[, status := as.integer(tolower(Dead_Alive) == "dead")]
  return(data)
}

# Add timestamp footer function
add_timestamp_footer <- function() {
  timestamp <- format(Sys.time(), "%d %B %Y, %H:%M:%S")
  footer_text <- paste0("RShiny Survival Curve App created by Associate Professor Ashley Ng Version 1 2025\n",
                        "Generated on: ", timestamp)
  return(footer_text)
}

# Modified UI with download buttons
ui <- fluidPage(
  titlePanel("Survival Analysis Dashboard"),
  
  sidebarLayout(
    sidebarPanel(
      h4("1. Get Started"),
      downloadButton("download_template", "Download CSV Template"),
      tags$div(
        style = "margin-top: 5px; font-size: 0.9em; color: #666;",
        "Use this template as a starting point for your data"
      ),
      hr(),
      h4("2. Upload Data"),
      fileInput("file", "Upload CSV File",
                accept = c("text/csv",
                           "text/comma-separated-values,text/plain",
                           ".csv")
      ),
      tags$div(id = "loading_spinner",
               style = "display: none;",
               tags$img(src = "spinner.gif")),
      
      textInput("treatment_search", "Filter Treatments:", ""),
      fluidRow(
        column(6,
               actionButton("select_all", "Select All", 
                            class = "btn-sm btn-block")
        ),
        column(6,
               actionButton("deselect_all", "Deselect All", 
                            class = "btn-sm btn-block")
        )
      ),
      br(),
      div(style = "max-height: 400px; overflow-y: auto;",
          checkboxGroupInput("treatments", "Select Treatments:", choices = NULL)
      )
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Data Preview", 
                 dataTableOutput("data_preview")),
        tabPanel("Overall Survival", 
                 div(style = "text-align: right; margin-bottom: 10px;",
                     downloadButton("download_overall_survival", "Download PDF")),
                 plotOutput("overall_survival", height = "600px")),
        tabPanel("Treatment-wise Survival", 
                 div(style = "text-align: right; margin-bottom: 10px;",
                     downloadButton("download_treatment_survival", "Download PDF")),
                 plotOutput("treatment_survival", height = "600px")),
        tabPanel("Statistics", 
                 div(style = "text-align: right; margin-bottom: 10px;",
                     downloadButton("download_statistics", "Download TXT")),
                 verbatimTextOutput("statistics"))
      )
    )
  )
)

# Modified server function with download handlers
server <- function(input, output, session) {
  # Your existing template download handler remains the same
  output$download_template <- downloadHandler(
    filename = function() {
      "survival_analysis_template.csv"
    },
    content = function(file) {
      template <- create_template_csv()
      write.csv(template, file, row.names = FALSE, quote = TRUE)
    }
  )
  
  rv <- reactiveValues(
    data = NULL,
    processed = FALSE
  )
  
  # Your existing data loading and processing code remains the same
  observeEvent(input$file, {
    req(input$file)
    withProgress(message = 'Loading data...', value = 0, {
      tryCatch({
        incProgress(0.3, detail = "Reading file...")
        data <- fread(input$file$datapath, data.table = TRUE)
        incProgress(0.3, detail = "Processing data...")
        processed_data <- preprocess_data(data)
        incProgress(0.3, detail = "Updating UI...")
        rv$data <- processed_data
        rv$processed <- TRUE
        updateCheckboxGroupInput(session, "treatments",
                                 choices = unique(processed_data$Treatment))
      }, error = function(e) {
        showNotification(paste("Error:", e$message), type = "error")
      })
    })
  })
  
  # Your existing reactive expressions remain the same
  filtered_treatments <- reactive({
    req(rv$data)
    if (is.null(input$treatment_search) || input$treatment_search == "") {
      return(unique(rv$data$Treatment))
    }
    unique(rv$data[grep(tolower(input$treatment_search), 
                        tolower(Treatment), value = FALSE)]$Treatment)
  })
  
  # Download handler for Overall Survival plot
  output$download_overall_survival <- downloadHandler(
    filename = function() {
      paste0("overall_survival_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".pdf")
    },
    content = function(file) {
      pdf(file, width = 11, height = 8)
      fit <- survfit(Surv(survival_time, status) ~ 1, data = rv$data)
      print(ggsurvplot(
        fit = fit,
        data = rv$data,
        xlab = "Time (months)",
        ylab = "Overall survival probability",
        risk.table = TRUE,
        conf.int = TRUE,
        title = "Overall Survival Curve",
        surv.median.line = "hv"
      ))
      grid.text(add_timestamp_footer(), x = 0.02, y = 0.02, 
                just = "left", gp = gpar(fontsize = 8))
      dev.off()
    }
  )
  
  # Download handler for Treatment-wise Survival plot
  output$download_treatment_survival <- downloadHandler(
    filename = function() {
      paste0("treatment_survival_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".pdf")
    },
    content = function(file) {
      pdf(file, width = 11, height = 8)
      plot_data <- rv$data[Treatment %in% input$treatments]
      fit <- survfit(Surv(survival_time, status) ~ Treatment, data = plot_data)
      print(ggsurvplot(
        fit = fit,
        data = plot_data,
        xlab = "Time (months)",
        ylab = "Survival probability",
        risk.table = TRUE,
        conf.int = TRUE,
        pval = TRUE,
        surv.median.line = "hv"
      ))
      grid.text(add_timestamp_footer(), x = 0.02, y = 0.02, 
                just = "left", gp = gpar(fontsize = 8))
      dev.off()
    }
  )
  
  # Download handler for Statistics
  output$download_statistics <- downloadHandler(
    filename = function() {
      paste0("survival_statistics_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".txt")
    },
    content = function(file) {
      plot_data <- rv$data[Treatment %in% input$treatments]
      fit <- survfit(Surv(survival_time, status) ~ Treatment, data = plot_data)
      surv_diff <- survdiff(Surv(survival_time, status) ~ Treatment, data = plot_data)
      
      # Capture the output
      sink(file)
      cat("SURVIVAL ANALYSIS STATISTICS\n\n")
      cat("Summary of survival analysis:\n")
      print(summary(fit))
      cat("\nLog-rank test results:\n")
      print(surv_diff)
      cat("\n", add_timestamp_footer(), "\n")
      sink()
    }
  )
  
  # Your existing output rendering code remains the same
  output$overall_survival <- renderPlot({
    req(rv$data, rv$processed)
    fit <- survfit(Surv(survival_time, status) ~ 1, data = rv$data)
    ggsurvplot(
      fit = fit,
      data = rv$data,
      xlab = "Time (months)",
      ylab = "Overall survival probability",
      risk.table = TRUE,
      conf.int = TRUE,
      title = "Overall Survival Curve",
      surv.median.line = "hv"
    )
  })
  
  output$treatment_survival <- renderPlot({
    req(rv$data, rv$processed, input$treatments)
    plot_data <- rv$data[Treatment %in% input$treatments]
    fit <- survfit(Surv(survival_time, status) ~ Treatment, data = plot_data)
    ggsurvplot(
      fit = fit,
      data = plot_data,
      xlab = "Time (months)",
      ylab = "Survival probability",
      risk.table = TRUE,
      conf.int = TRUE,
      pval = TRUE,
      surv.median.line = "hv"
    )
  })
  
  output$statistics <- renderPrint({
    req(rv$data, rv$processed, input$treatments)
    plot_data <- rv$data[Treatment %in% input$treatments]
    fit <- survfit(Surv(survival_time, status) ~ Treatment, data = plot_data)
    print(summary(fit))
    surv_diff <- survdiff(Surv(survival_time, status) ~ Treatment, data = plot_data)
    print(surv_diff)
  })
  
  output$data_preview <- renderDataTable({
    req(rv$data)
    rv$data
  }, options = list(pageLength = 10))
}

# Run the application
shinyApp(ui = ui, server = server)