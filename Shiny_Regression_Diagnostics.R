library(shiny)
library(car)
library(DT)
library(ggplot2)

ui <- fluidPage(
  titlePanel("Regression Analysis and Diagnostics"),
  
  sidebarLayout(
    sidebarPanel(
      fileInput("file1", "Upload CSV File", accept = ".csv"),
      selectInput("dependent", "Select Dependent Variable", choices = NULL),
      uiOutput("independent_vars_ui"),
      actionButton("runAnalysis", "Run Analysis"),
      hr(),
      h4("Results Summary"),
      verbatimTextOutput("regressionSummary"),
      verbatimTextOutput("vifSummary"),
      downloadButton("downloadResults", "Download Results")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Diagnostics Table",
                 DTOutput("resultsTable")),
        tabPanel("Plots",
                 plotOutput("cooksPlot"),
                 plotOutput("leveragePlot"))
      )
    )
  )
)

server <- function(input, output, session) {
  
  dataset <- reactive({
    req(input$file1)
    df <- read.csv(input$file1$datapath)
    df <- na.omit(df)  # Remove missing values
    df
  })
  
  observe({
    req(dataset())
    updateSelectInput(session, "dependent", choices = names(dataset()))
    output$independent_vars_ui <- renderUI({
      checkboxGroupInput("independent_vars", "Select Independent Variables", choices = names(dataset()))
    })
  })
  
  showErrorModal <- function(title, message) {
    showModal(modalDialog(
      title = title,
      message,
      easyClose = TRUE,
      footer = NULL
    ))
  }
  
  diagnostics <- eventReactive(input$runAnalysis, {
    req(dataset(), input$dependent, input$independent_vars)
    df <- dataset()
    depVar <- input$dependent
    indepVars <- input$independent_vars
    
    if (length(indepVars) < 1) {
      showErrorModal("Error", "Please select at least one independent variable.")
      return(NULL)
    }
    
    indepVars <- indepVars[sapply(indepVars, function(var) sd(df[[var]]) > 1e-8)]
    if (length(indepVars) < 1) {
      showErrorModal("Error", "All selected independent variables are constant or nearly constant. Please check your data.")
      return(NULL)
    }
    
    model_formula <- as.formula(paste(depVar, "~", paste(indepVars, collapse = "+")))
    model <- lm(model_formula, data = df)
    
    vif_values <- vif(model)
    
    x <- cbind(1, as.matrix(df[, indepVars]))
    H <- x %*% solve(t(x) %*% x) %*% t(x)
    sigma2 <- summary(model)$sigma^2
    cii <- (1 / ncol(x)) * diag(H) / (1 - diag(H)) * (resid(model) / sqrt(sigma2) / sqrt(1 - diag(H)))^2
    si <- ((nrow(df) - ncol(x)) * sigma2 - resid(model)^2 / (1 - diag(H))) / (nrow(df) - ncol(x) - 1)
    di <- resid(model) / si / sqrt(1 - diag(H)) * sqrt(diag(H) / (1 - diag(H)))
    cooks <- cooks.distance(model)
    leverage <- diag(H)
    
    results <- data.frame(
      Row = 1:nrow(df),
      Fitted_Values = fitted(model),
      Residuals = resid(model),
      Cii = cii,
      Si = si,
      Di = di,
      Cooks_Distance = cooks,
      Leverage = leverage
    )
    
    list(model = model, vif = vif_values, results = results)
  })
  
  output$regressionSummary <- renderPrint({
    req(diagnostics())
    summary(diagnostics()$model)
  })
  
  output$vifSummary <- renderPrint({
    req(diagnostics())
    vif_values <- diagnostics()$vif
    vif_values
  })
  
  output$resultsTable <- renderDT({
    req(diagnostics())
    datatable(diagnostics()$results, options = list(pageLength = 10, scrollX = TRUE))
  })
  
  output$cooksPlot <- renderPlot({
    req(diagnostics())
    cooks_data <- diagnostics()$results
    plot(cooks_data$Row, cooks_data$Cooks_Distance, type = "h", col = "blue", lwd = 2,
         main = "Cook's Distance",
         xlab = "Observation Index", ylab = "Cook's Distance")
    abline(h = 4 / nrow(cooks_data), col = "red", lty = 2, lwd = 2)
  })
  
  output$leveragePlot <- renderPlot({
    req(diagnostics())
    cooks_data <- diagnostics()$results
    plot(cooks_data$Leverage, cooks_data$Cooks_Distance, pch = 19, col = "blue",
         main = "Leverage vs Cook's Distance",
         xlab = "Leverage", ylab = "Cook's Distance")
    abline(h = 4 / nrow(cooks_data), col = "red", lty = 2, lwd = 2)
    abline(v = 2 * mean(cooks_data$Leverage), col = "green", lty = 2, lwd = 2)
  })
  
  output$downloadResults <- downloadHandler(
    filename = function() {
      paste("diagnostics_results_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(diagnostics()$results, file, row.names = FALSE)
    }
  )
}

shinyApp(ui = ui, server = server)
