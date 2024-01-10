library(shiny)
library(shinydashboard)
library(DT)
library(ggplot2)
library(plotly)
library(tools)
library(stringr)
library(shinythemes)
library(shinyWidgets)
library(ggalt)
library(lmtest)
library(car)
library(nortest)
library(agricolae)

ui <- fluidPage(
  dashboardPage(
    skin = "red",
    dashboardHeader(title = "E-COMMERCE AND DIGITAL MARKETING DASHBOARD", titleWidth = 650),
    
    dashboardSidebar(
      sidebarMenu(
        id = "tabs",
        menuItem("File Upload", tabName = "upload", icon = icon("file")),
        menuItem("Download Plot", tabName = "download_plot", icon = icon("download")),
        br(),
        menuItem("E-COMMERCE DASHBOARD", badgeLabel = "E-COMMERCE", badgeColor = "red"),
        br(),
        menuItem("Monthly Sales", tabName = "predict", icon = icon("search")),
        menuItem("Assumption 1", tabName = "assumption1", icon = icon("check")),
        menuItem("Assumption 2", tabName = "assumption2", icon = icon("check")),
        menuItem("ANOVA Ecommerce", tabName = "anova", icon = icon("table")),
        menuItem("Regression Plot", tabName = "regression_plot_tab", icon = icon("chart-line")),
        menuItem("Boxplot Ecommerce", tabName = "boxplot_tab", icon = icon("chart-bar")),
        br(),
        menuItem("DIGITAL MARKETING DASHBOARD", badgeLabel = "DIGITAL MARKETING", badgeColor = "blue"),
        br(),
        menuItem("ANOVA Digital Marketing", tabName = "anova1", icon = icon("table")),
        menuItem("Boxplot Digital Marketing", tabName = "boxplot_tab1", icon = icon("chart-bar")),
        menuItem("Regresi", tabName = "regresi", icon = icon("chart-line")),
        menuItem("Plot", tabName = "regression_plot_tab1", icon = icon("chart-line"))
      )
    ), # dashboard sidebar end
    
    dashboardBody(
      tabItems(
        tabItem(
          tabName = "upload",
          titlePanel("Uploading Files"),
          sidebarLayout(
            sidebarPanel(
              fileInput("file1", "Choose CSV File",
                        multiple = TRUE,
                        accept = c("text/csv",
                                   "text/comma-separated-values,text/plain",
                                   ".csv")),
              tags$hr(),
              checkboxInput("header", "Header", TRUE),
              radioButtons("sep", "Separator",
                           choices = c(Comma = ",",
                                       Semicolon = ";",
                                       Tab = "\t"),
                           selected = ","),
              radioButtons("quote", "Quote",
                           choices = c(None = "",
                                       "Double Quote" = '"',
                                       "Single Quote" = "'"),
                           selected = '"'),
              tags$hr(),
              radioButtons("disp", "Display",
                           choices = c(Head = "head",
                                       All = "all"),
                           selected = "all")
            ),
            mainPanel(
              DTOutput("contents_table")  # Mengganti tableOutput dengan DTOutput
            )
          )
        ),
        
        tabItem(
          tabName = "predict",
          titlePanel("Monthly Sales Volume"),
          sidebarLayout(
            sidebarPanel(
              width = 12,
              textOutput("regression_equation"),
              numericInput("input_x1", "Visitors:", value = 200000, min = 0),
              numericInput("input_x2", "Transactions:", value = 10000, min = 0),
              numericInput("input_x3", "Items/Transactions:", value = 4.5, min = 0),
              numericInput("input_x4", "Rating:", value = 8.1, min = 0),
              numericInput("input_x5", "Ads:", value = 40000, min = 0),
              actionButton("predict_button", "Predict Monthly Sales"),
              textOutput("predicted_monthly_sales")  # corrected output id
            ),
            mainPanel(
              DTOutput("contents_predict_table")  # Mengganti tableOutput dengan DTOutput
            )
          )
        ),
        
        tabItem(
          tabName = "assumption1",
          box(
            title = "Durbin-Watson Test Results",
            solidHeader = TRUE,
            status = "info",
            width = 12,
            verbatimTextOutput("durbin_watson_test")
          ),
          box(
            title = "Breusch-Pagan Test (Homoskedasticity)",
            solidHeader = TRUE,
            status = "info",
            width = 12,
            verbatimTextOutput("breusch_pagan_test")
          )
        ),
        
        tabItem(
          tabName = "assumption2",
          box(
            title = "Lilliefors Test (Normality)",
            solidHeader = TRUE,
            status = "info",
            width = 12,
            verbatimTextOutput("lilliefors_test")
          ),
          box(
            title = "VIF (Multicollinearity)",
            solidHeader = TRUE,
            status = "info",
            width = 12,
            verbatimTextOutput("vif_test")
          )
        ),
        
        tabItem(
          tabName = "anova",
          box(
            title = "ANOVA Results",
            solidHeader = TRUE,
            status = "info",
            width = 12,
            verbatimTextOutput("anova_results")
          )
        ),
        
        tabItem(
          tabName = "regression_plot_tab",
          box(
            title = "Regression Plot",
            solidHeader = TRUE,
            status = "info",
            width = 12,
            plotOutput("regression_plot")
          )
        ),
        
        tabItem(
          tabName = "boxplot_tab",
          box(
            title = "Boxplot",
            solidHeader = TRUE,
            status = "info",
            width = 12,
            plotOutput("boxplot")
          )
        ),
        
        tabItem(
          tabName = "anova1",
          titlePanel("ANOVA & Tukey Results"),
          solidHeader = TRUE,
          status = "info",
          width = 12,
          verbatimTextOutput("anova_results1"),
          verbatimTextOutput("tukey_results")
        ),
        
        tabItem(
          tabName = "boxplot_tab1",
          box(
            title = "Boxplot",
            solidHeader = TRUE,
            status = "info",
            width = 12,
            plotOutput("boxplot_tab1")
          )
        ),
        
        tabItem(
          tabName = "regresi",
          box(
            title = "Regression Model for CTRs",
            solidHeader = TRUE,
            status = "info",
            width = 12,
            verbatimTextOutput("regresi")
          )
        ),
        
        tabItem(
          tabName = "regression_plot_tab1",
          box(
            title = "Regression Plot",
            solidHeader = TRUE,
            status = "info",
            width = 12,
            plotOutput("regression_plot1")
          )
        ),
        
        tabItem(
          tabName = "download_plot",
          titlePanel("Download Plot"),
          mainPanel(
            selectInput("plot_to_display", "Select Plot to Display",
                        choices = c("Plot 1", "Plot 2", "Plot 3", "Plot 4"),
                        selected = "Plot 1"),
            plotOutput("selected_plot"),
            downloadButton("download_button", "Download Plot")
          )
        )
        
      )
    )
  )
)

server <- function(input, output){
  file1 <- reactive({
    req(input$file1)
    read.csv(input$file1$datapath,
             header = input$header,
             sep = input$sep,
             quote = input$quote)
  })
  
  output$contents_table <- renderDT({
    req(file1())
    datatable(
      file1(),
      options = list(
        lengthMenu = c(20, 40, 60),
        pageLength = 20,
        scrollX = TRUE,
        dom = 'tip',
        ordering = TRUE
      )
    )
  })
  
  lm_model <- reactive({
    lm(y ~ x1 + x2 + x3 + x4 + x5, data = file1())
  })
  
  output$regression_equation <- renderText({
    req(lm_model())
    coefs <- coef(lm_model())
    intercept <- round(coefs[1], 2)
    other_coefs <- sapply(coefs[-1], function(x) round(x, 5))
    variable_names <- names(coefs)[-1]
    
    eq_str <- paste("Regression Equation: y =", intercept,
                    paste0(ifelse(other_coefs >= 0, "+", ""), other_coefs, variable_names, collapse = " "))
    
    return(eq_str)
  })
  
  output$predicted_monthly_sales <- renderText({
    req(input$predict_button, lm_model())
    new_data <- data.frame(
      x1 = input$input_x1,
      x2 = input$input_x2,
      x3 = input$input_x3,
      x4 = input$input_x4,
      x5 = input$input_x5
    )
    predicted_value <- predict(lm_model(), newdata = new_data)
    paste("Predicted Monthly Sales:", round(predicted_value, 2))
  })
  
  output$contents_predict_table <- renderDT({
    req(lm_model())
    datatable(
      as.data.frame(summary(lm_model())$coefficients),
      options = list(
        lengthMenu = c(10, 20, 30),
        pageLength = 10,
        scrollX = TRUE,
        dom = 'tip',
        ordering = TRUE
      )
    )
  })
  
  output$durbin_watson_test <- renderPrint({
    req(lm_model())
    dw_test <- dwtest(lm_model())
    
    stat <- dw_test$statistic
    p_value <- dw_test$p.value
    alternative <- dw_test$alternative
    
    result <- list(
      "Durbin-Watson Test Statistic" = stat,
      "P-value" = p_value,
      "Alternative Hypothesis" = alternative
    )
    
    return(result)
  })
  
  output$breusch_pagan_test <- renderPrint({
    req(lm_model())
    bp_test <- bptest(lm_model(), studentize = TRUE, data = file1())
    
    stat <- bp_test$statistic
    p_value <- bp_test$p.value
    
    result <- list(
      "Breusch-Pagan Test Statistic" = stat,
      "P-value" = p_value
    )
    
    return(result)
  })
  
  output$lilliefors_test <- renderPrint({
    req(lm_model())
    lillie_test <- lillie.test(lm_model()$residuals)
    
    stat <- lillie_test$statistic
    p_value <- lillie_test$p.value
    
    result <- list(
      "Lilliefors Test Statistic" = stat,
      "P-value" = p_value
    )
    
    return(result)
  })
  
  output$vif_test <- renderPrint({
    req(lm_model())
    vif_values <- vif(lm_model())
    
    result <- list(
      "VIF Values" = vif_values
    )
    
    return(result)
  })
  
  output$anova_results <- renderPrint({
    Model1 <- aov(y ~ x1*x5, data = file1())
    summary(Model1)
  })
  
  output$regression_plot <- renderPlot({
    req(lm_model())
    plot(lm_model())
  })
  
  output$boxplot <- renderPlot({
    req(file1())
    boxplot(file1()$y, file1()$x1, file1()$x2, file1()$x3, file1()$x4, file1()$x5,
            main = "Boxplot", col = c("blue", "red", "green", "orange", "purple", "brown"))
  })
  
  output$anova_results1 <- renderPrint({
    Model2 <- aov(CTRs ~ Sidebar, data = file1())
    summary(Model2)
  })
  
  output$tukey_results <- renderPrint({
    Model3 <- aov(CTRs ~ Sidebar, data = file1())
    Tukey <- HSD.test(Model3, "Sidebar", console = TRUE, alpha = 0.05)
    return(as.data.frame(Tukey$statistics))
  })
  
  output$boxplot_tab1 <- renderPlot({
    req(file1())
    ggplot(file1(), aes(x = Sidebar, y = CTRs, fill = Sidebar)) +
      geom_boxplot() +
      labs(title = "Boxplot CTRs by Sidebar Type",
           x = "Sidebar Type",
           y = "CTRs") +
      theme_minimal()
  })
  
  Model_Regresi <- reactive({
    lm(CTRs ~ Sidebar, data = file1())
  })
  
  output$regresi <- renderPrint({
    Model_Regresi <- lm(CTRs ~ Sidebar, data = file1())
    summary(Model_Regresi)
  })
  
  output$regression_plot1 <- renderPlot({
    req(Model_Regresi())
    plot(Model_Regresi())
  })
  
  output$selected_plot <- renderPlot({
    plot_function <- switch(
      input$plot_to_display,
      "Plot 1" = function() { plot(lm_model()) },
      "Plot 2" = function() { boxplot(file1()$y, file1()$x1, file1()$x2, file1()$x3, file1()$x4, file1()$x5, main = "Boxplot", col = c("blue", "red", "green", "orange", "purple", "brown")) },
      "Plot 3" = function() { plot(Model_Regresi()) },
      "Plot 4" = function() { ggplot(file1(), aes(x = Sidebar, y = CTRs, fill = Sidebar)) + geom_boxplot() + labs(title = "Boxplot CTRs by Sidebar Type", x = "Sidebar Type", y = "CTRs") + theme_minimal() }
    )
    plot_function()
  })
  
  output$download_button <- downloadHandler(
    filename = function() {
      paste("plot_", input$plot_to_display, "_", Sys.Date(), ".png", sep = "")
    },
    content = function(file) {
      plot_function <- switch(
        input$plot_to_display,
        "Plot 1" = function() { plot(lm_model()) },
        "Plot 2" = function() { boxplot(file1()$y, file1()$x1, file1()$x2, file1()$x3, file1()$x4, file1()$x5, main = "Boxplot", col = c("blue", "red", "green", "orange", "purple", "brown")) },
        "Plot 3" = function() { plot(Model_Regresi()) },
        "Plot 4" = function() { ggplot(file1(), aes(x = Sidebar, y = CTRs, fill = Sidebar)) + geom_boxplot() + labs(title = "Boxplot CTRs by Sidebar Type", x = "Sidebar Type", y = "CTRs") + theme_minimal() }
      )
      plot_function()
      ggsave(file, width = 800, height = 600)
    }
  )
  
}

shinyApp(ui, server)
