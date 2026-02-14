library(shiny)
library(ggplot2)
library(car)
library(lmtest)
library(nortest)
library(MASS)
library(DT)

ui <- fluidPage(
  titlePanel("Regression Analysis and Assumption Testing"),
  
  sidebarLayout(
    sidebarPanel(
      fileInput("file1", "Upload CSV File", accept = ".csv"),
      selectInput("dependent", "Select Dependent Variable", choices = NULL),
      uiOutput("independent_vars_ui"),
      actionButton("runAnalysis", "Run Analysis"),
      hr(),
      h4("Test Results"),
      verbatimTextOutput("ksTestResult"),
      verbatimTextOutput("lillieTestResult"),
      verbatimTextOutput("shapiroTestResult"),
      verbatimTextOutput("adTestResult"),
      verbatimTextOutput("bptestResult"),
      verbatimTextOutput("dwTestResult"),
      verbatimTextOutput("vifResult"),
      downloadButton("downloadResults", "Download Results")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Plots",
                 plotOutput("scatterPlot"),
                 plotOutput("histogram"),
                 plotOutput("boxPlot"),
                 plotOutput("residualPlot"),
                 plotOutput("qqPlot"),
                 plotOutput("acfPlot")),
        tabPanel("Model Summary",
                 verbatimTextOutput("modelSummary")),
        tabPanel("Dataset Table",
                 DTOutput("dataTable"))
      )
    )
  )
)

server <- function(input, output, session) {
  dataset <- reactive({
    req(input$file1)
    read.csv(input$file1$datapath)
  })
  
  observe({
    req(dataset())
    updateSelectInput(session, "dependent", choices = names(dataset()))
    output$independent_vars_ui <- renderUI({
      checkboxGroupInput("independent_vars", "Select Independent Variables", choices = names(dataset()))
    })
  })
  
  observeEvent(input$runAnalysis, {
    req(dataset(), input$dependent, input$independent_vars)
    df <- dataset()
    depVar <- input$dependent
    indepVars <- input$independent_vars
    
    if (length(indepVars) < 1) return()
    
    model_formula <- as.formula(paste(depVar, "~", paste(indepVars, collapse = "+")))
    model <- lm(model_formula, data = df)
    stuGR <- rstudent(model)
    
    output$ksTestResult <- renderPrint({
      ks.test(stuGR, "pnorm")
    })
    
    output$lillieTestResult <- renderPrint({
      lillie.test(stuGR)
    })
    
    output$shapiroTestResult <- renderPrint({
      shapiro.test(stuGR)
    })
    
    output$adTestResult <- renderPrint({
      ad.test(stuGR)
    })
    
    output$bptestResult <- renderPrint({
      bptest(model)
    })
    
    output$dwTestResult <- renderPrint({
      dwtest(model, alternative = "two.sided")
    })
    
    output$vifResult <- renderPrint({
      vif_values <- tryCatch({
        vif(model)
      }, error = function(e) {
        return("Multicollinearity detected or aliased coefficients in the model. Please check independent variables.")
      })
      vif_values
    })
    
    output$scatterPlot <- renderPlot({
      ggplot(df, aes_string(x = indepVars[1], y = depVar)) +
        geom_point(color = "blue", size = 2) +
        theme_minimal() +
        labs(title = paste("Scatter Plot:", depVar, "vs", indepVars[1]), x = indepVars[1], y = depVar)
    })
    
    output$histogram <- renderPlot({
      hist(stuGR, main = "Histogram of Residuals", xlab = "Residuals", col = "skyblue", border = "white")
    })
    
    output$boxPlot <- renderPlot({
      boxplot(stuGR, main = "Boxplot of Residuals", ylab = "Residuals", col = "orange")
    })
    
    output$residualPlot <- renderPlot({
      plot(fitted(model), resid(model), main = "Residuals vs Fitted", 
           xlab = "Fitted Values", ylab = "Residuals",
           pch = 20, col = "blue")
      abline(h = 0, col = "red", lwd = 2)
    })
    
    output$qqPlot <- renderPlot({
      qqnorm(stuGR)
      qqline(stuGR, col = "red", lwd = 2)
    })
    
    output$acfPlot <- renderPlot({
      acf(stuGR, main = "Autocorrelation Function of Residuals")
    })
    
    output$modelSummary <- renderPrint({
      summary(model)
    })
    
    output$dataTable <- renderDT({
      datatable(df, options = list(pageLength = 5))
    })
    
    output$downloadResults <- downloadHandler(
      filename = function() { paste("results_", Sys.Date(), ".csv", sep = "") },
      content = function(file) {
        results <- data.frame(
          Fitted_Values = fitted(model),
          Residuals = resid(model),
          Studentized_Residuals = stuGR
        )
        write.csv(results, file, row.names = FALSE)
      }
    )
  })
}

shinyApp(ui = ui, server = server)
