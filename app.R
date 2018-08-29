library(shiny)
library("shinythemes")
library("relaimpo")
getwd
setwd('./')
stocks <- read.csv("stats_project.csv", header = TRUE)

par(mar=rep(2,4))
source("project_functions.R")

apple <- daily_log_return(stocks$APPLE)$lgreturn
google <- daily_log_return(stocks$GOOGLE)$lgreturn
microsoft <- daily_log_return(stocks$MICROSOFT)$lgreturn
ibm <- daily_log_return(stocks$IBM)$lgreturn
amazon <- daily_log_return(stocks$AMAZON)$lgreturn
baidu <- daily_log_return(stocks$BAIDU)$lgreturn
alibaba <- daily_log_return(stocks$ALIBABA)$lgreturn
huawei <- daily_log_return(stocks$HUAWEI)$lgreturn
sina <- daily_log_return(stocks$SINA)$lgreturn
jdcom <- daily_log_return(stocks$JDCOM)$lgreturn

america_avg <- (apple + google + microsoft + ibm + amazon)/5
china_avg <- (baidu + alibaba + huawei + sina + jdcom)/5

america <- data.frame(apple, google, microsoft, ibm, amazon, america_avg)
china <- data.frame(baidu, alibaba, huawei, sina, jdcom, china_avg)



#SHINY
ui <- navbarPage("Welcome!",
                 tabPanel("One Stock",
                          sidebarPanel(selectInput('xcol', 'Stock', names(stocks)),
                                       numericInput('alpha', 'Confidence Level:', min = 0, max = 100, value = 95, step = 1)),
                          mainPanel(
                            tabsetPanel(type = "tabs",
                                        tabPanel("Histogram", 
                                                 plotOutput('plot1')),
                                        tabPanel("Normality", 
                                                 plotOutput('plot2'), 
                                                 h3(textOutput('goodness_of_fit'))),
                                        tabPanel("Estimates",
                                                 h3(textOutput('mean_ci')),
                                                 h3(textOutput('sd_ci'))),
                                        tabPanel("Time Series Regression",
                                                 plotOutput('regression_timeseries'),
                                                 h3(verbatimTextOutput('regression_timeseries_summary')),
                                                 plotOutput('regression_timeseries_residuals')
                                        )))),
                 tabPanel("Two Stocks",
                          sidebarPanel(
                            selectInput('xcol', 'Primary Stock', names(stocks)),
                            selectInput('acol', 'Secondary Stock', names(stocks)),
                            numericInput('alpha2', 'Confidence Level:', min = 0, max = 100, value = 95, step = 1)
                          ),
                          mainPanel(
                            tabsetPanel(type = "tabs",
                                        tabPanel("Mean Comparison",
                                                 h3(textOutput('similar_means'))),
                                        tabPanel("Independence",
                                                 h3(textOutput('independence'))),
                                        tabPanel("Two Stocks Regression",
                                                 plotOutput('regression_two_stocks'),
                                                 h3(verbatimTextOutput('regression_two_stocks_summary')),
                                                 plotOutput('regression_two_stocks_residuals'))
                            ))),
                 tabPanel("Global Analysis",
                          sidebarPanel(
                            selectInput('merica', 'US Stocks', names(america)),
                            selectInput('hina', 'China Stocks', names(china)),
                            numericInput('alpha3', 'Confidence Level:', min = 0, max = 100, value = 95, step = 1)
                          ),
                          mainPanel(
                            tabsetPanel(type = "tabs",
                                        tabPanel("Mean Comparison US vs China",
                                                 h3(textOutput('similar_china_us')),
                                                 h3(textOutput('similar_china_us_less')),
                                                 h3(textOutput('similar_china_us_greater'))),
                                        tabPanel("Independence",
                                                 h3(textOutput('independence_china_us'))),
                                        tabPanel("China vs US Regression",
                                                 plotOutput('regression_china_us'),
                                                 h3(verbatimTextOutput('regression_china_us_summary')),
                                                 plotOutput('regression_china_us_residuals')),
                                        tabPanel("Stock Predictor Importance",
                                                 h3(verbatimTextOutput('predictors_for_us_summary')),
                                                 h3(verbatimTextOutput('predictors_for_us')),
                                                 plotOutput("predictors_for_us_residuals"),
                                                 h3(verbatimTextOutput('predictors_for_china_summary')),
                                                 h3(verbatimTextOutput('predictors_for_china')),
                                                 plotOutput("predictors_for_china_residuals")),
                                        tabPanel("Volatility Comparison",
                                                 h3(textOutput('volatility_two')),
                                                 h3(textOutput('volatility_less')),
                                                 h3(textOutput('volatility_greater')))
                            ))),
                 theme = shinytheme("superhero")
                 )


server <- function(input, output) {
  
  selectedData <- reactive({
    stocks[, c(input$xcol, input$ycol)]
  })
  
  selectedData2 <- reactive({
    stocks[, c(input$acol, input$ycol)]
  })
  
  selectedUS <- reactive({
    america[, c(input$merica, input$ycol)]
  })
  
  selectedChina <- reactive({
    china[, c(input$hina, input$ycol)]
  })
  
  output$plot1 <- renderPlot({
    data <- daily_log_return(selectedData())$lgreturn
    hist(data, xlab = "Log return")
  })
  
  output$plot2 <- renderPlot({
    data <- daily_log_return(selectedData())$lgreturn
    qqnorm(data, xlab = "Log return", ylab = "Value")
    qqline(data)
  })
  
  output$mean_ci <- renderText({
    data <- daily_log_return(selectedData())$lgreturn
    alpha <- (1 - (input$alpha)/100)
    CI <- ci_mean(data, alpha)
    
    sprintf("The %d%% confidence interval for the mean daily log return is (%f, %f)", input$alpha, CI[1], CI[2])
  })
  
  output$sd_ci <- renderText({
    data <- daily_log_return(selectedData())$lgreturn
    alpha <- (1 - (input$alpha)/100)
    CI <- ci_sd(data, alpha)
    
    sprintf("The %d%% confidence interval for the variance of the daily log return is (%f, %f).", input$alpha, CI[1], CI[2])
  })
  
  output$regression_timeseries <- renderPlot({
    data <- daily_log_return(selectedData())$lgreturn
    dates <- seq(0, length(data) -1)
    plot(data ~ dates, xlab = "Time", ylab = "Value")
    regression <- lm(data ~ dates)
    abline(regression)
  })
  
  output$regression_timeseries_summary <- renderText({
    data <- daily_log_return(selectedData())$lgreturn
    dates <- seq(0, length(data) -1)
    regression <- lm(data ~ dates)
    fmt <- ("Coefficients: \n
            Intercept: %s \n
            Stock: %s \n\n\n
            R^2: %s")
    sprintf(fmt, regression$coefficients[1], regression$coefficients[2], summary(regression)$r.squared)
  })
  
  output$regression_timeseries_residuals <- renderPlot({
    data <- daily_log_return(selectedData())$lgreturn
    dates <- seq(0, length(data) -1)
    plot(data ~ dates, xlab = "Time", ylab = "Value")
    regression <- lm(data ~ dates)
    residuals <- resid(regression)
    plot(residuals, xlab = "Data points", ylab = "Residual")
    })
  
  output$regression_two_stocks <- renderPlot({
    data1 <- daily_log_return(selectedData())$lgreturn
    data2 <- daily_log_return(selectedData2())$lgreturn
    plot(data1 ~ data2, xlab = "Secondary Stock", ylab = "Primary Stock")
    regression <- lm(data1 ~ data2)
    abline(regression)
  })
  
  output$regression_two_stocks_summary <- renderText({
    data1 <- daily_log_return(selectedData())$lgreturn
    data2 <- daily_log_return(selectedData2())$lgreturn
    regression <- lm(data1 ~ data2)
    fmt <- ("Coefficients: \n
            Intercept: %s \n
            Secondary Stock: %s \n\n\n
            R^2: %s")
    sprintf(fmt, regression$coefficients[1], regression$coefficients[2], summary(regression)$r.squared)})
  
  output$regression_two_stocks_residuals <- renderPlot({
    data1 <- daily_log_return(selectedData())$lgreturn
    data2 <- daily_log_return(selectedData2())$lgreturn
    regression <- lm(data1 ~ data2)
    residuals <- resid(regression)
    plot(residuals, xlab = "Data points", ylab = "Residual")
  })
  
  output$goodness_of_fit <- renderText({
    data <- daily_log_return(selectedData())$lgreturn
    bool <- GoodnessOfFit(data, (1 - (input$alpha)/100))
    if(bool == TRUE){
      sprintf("There is insufficient evidence to suggest that the data is normal at a %d%% confidence level.", input$alpha)
    }
    else{
      sprintf("There is sufficient evidence to suggest that the data is normal at a %d%% confidence level.", input$alpha)
    }
  })
  
  output$independence <- renderText({
    data1 <- daily_log_return(selectedData())$lgreturn
    data2 <- daily_log_return(selectedData2())$lgreturn    
    bool <- Independence(data1, data2, (1 - (input$alpha2)/100))
    if(bool == TRUE){
      sprintf("There is insufficient evidence to suggest that the data are independent at a %d%% confidence level.", input$alpha2)
    }
    else{
      sprintf("There is sufficient evidence to suggest that the data are independent at a %d%% confidence level.", input$alpha2)
    }
  })
  
  output$similar_means <- renderText({
    data1 <- daily_log_return(selectedData())$lgreturn
    data2 <- daily_log_return(selectedData2())$lgreturn
    bool <- two_sample_t_test(data1, data2, (1 - (input$alpha2)/100))
    if(bool == TRUE){
      sprintf("There is insufficient evidence to suggest that the means of the data are different at a %d%% confidence level.", input$alpha2)
    }
    else{
      sprintf("There is sufficient evidence to suggest that the means of the data are different at a %d%% confidence level.", input$alpha2)
    }
  })
  
  output$similar_china_us <- renderText({
    bool <- two_sample_t_test(america_avg, china_avg, (1 - (input$alpha3)/100))
    
    if(bool == TRUE){
      sprintf("There is insufficient evidence to suggest that the mean of the data are different at a %d%% confidence level.", input$alpha3)
    }
    else{
      sprintf("There is sufficient evidence to suggest that the means of the data are different at a %d%% confidence level.", input$alpha3)
    }
  })
  
  output$similar_china_us_less <- renderText({
    bool <- two_sample_t_test_less(america_avg, china_avg, (1 - (input$alpha3)/100))
    
    if(bool == TRUE){
      sprintf("There is insufficient evidence to suggest that the mean of the chinese stocks is greater at a %d%% confidence level.", input$alpha3)
    }
    else{
      sprintf("There is sufficient evidence to suggest that the mean of the chinese stocks is greater at a %d%% confidence level.", input$alpha3)
    }
  })
  
  output$similar_china_us_greater <- renderText({
    bool <- two_sample_t_test_greater(america_avg, china_avg, (1 - (input$alpha3)/100))
    
    if(bool == TRUE){
      sprintf("There is insufficient evidence to suggest that the mean of the american stocks is greater at a %d%% confidence level.", input$alpha3)
    }
    else{
      sprintf("There is insufficient evidence to suggest that the mean of the american stocks is greater at a %d%% confidence level.", input$alpha3)
    }
  })
  
  output$independence_china_us <- renderText({
    bool <- Independence(america_avg, china_avg, (1 - (input$alpha3)/100))
    if(bool == TRUE){
      sprintf("There is insufficient evidence to suggest that the data are independent at a %d%% confidence level.", input$alpha3)
    }
    else{
      sprintf("There is sufficient evidence to suggest that the data are independent at a %d%% confidence level.", input$alpha3)
    }
  })
  
  output$regression_china_us <- renderPlot({
    plot(america_avg ~ china_avg, xlab = "China", ylab = "America")
    regression <- lm(america_avg ~ china_avg)
    abline(regression)
  })
  
  output$regression_china_us_summary <- renderText({
    regression <- lm(america_avg ~ china_avg)
    fmt <- ("Coefficients: \n
            Intercept: %s \n
            China Average: %s \n\n\n
            R^2: %s")
    sprintf(fmt, regression$coefficients[1], regression$coefficients[2], summary(regression)$r.squared)  })
  
  output$regression_china_us_residuals <- renderPlot({
    regression <- lm(america_avg ~ china_avg)
    residuals <- resid(regression)
    plot(residuals, xlab = "Data points", ylab = "Residual")
  })
  
  output$predictors_for_china <- renderText({
    data <- selectedChina()
    regression <- lm(data ~ apple + google + microsoft + ibm + amazon)
    X <- as.character(calc.relimp(regression, rela=TRUE)$lmg.rank)
    fmt <- ("Ranks of relative importance of US stocks in predicting the chosen Chinese company's returns. \n
            1: Most Important, 5: Least Important \n
            Apple: %s \n
            Google: %s \n
            Microsoft: %s \n 
            IBM: %s  \n
            Amazon: %s \n \n \n" )
    
    sprintf(fmt, X[1], X[2], X[3], X[4], X[5])
  })
  
  output$predictors_for_china_summary <- renderText({
    data <- selectedChina()
    regression <- lm(data ~ apple + google + microsoft + ibm + amazon)
    fmt <- ("Coefficients: \n
            Intercept: %s \n
            Apple: %s \n
            Google: %s \n
            Microsoft: %s \n
            IBM: %s \n
            Amazon: %s \n\n\n
            R^2: %s")
    sprintf(fmt, regression$coefficients[1], regression$coefficients[2], regression$coefficients[3],
            regression$coefficients[4], regression$coefficients[5], regression$coefficients[6], summary(regression)$r.squared)  })
  
  output$predictors_for_china_residuals <- renderPlot({
    data <- selectedChina()
    regression <- lm(data ~ apple + google + microsoft + ibm + amazon)
    residuals <- resid(regression)
    plot(residuals, xlab = "Data points", ylab = "Residual")
  })
  
  output$predictors_for_us <- renderText({
    data <- selectedUS()
    regression <- lm(data ~ baidu + alibaba + huawei + sina + jdcom)
    X <- as.character(calc.relimp(regression, rela=TRUE)$lmg.rank)
    fmt <- ("Ranks of relative importance of Chinese stocks in predicting the chosen US company's returns.\n
            1: Most Important, 5: Least Important \n
            Baidu: %s \n
            Alibaba: %s \n
            Huawei: %s \n
            Sina: %s \n
            Jdcom: %s \n \n \n
            ")
    sprintf(fmt, X[1], X[2], X[3], X[4], X[5])
  })
  
  output$predictors_for_us_residuals <- renderPlot({
    data <- selectedUS()
    regression <- lm(data ~ baidu + alibaba + huawei + sina + jdcom)    
    residuals <- resid(regression)
    plot(residuals, xlab = "Data points", ylab = "Residual")
  })
  
  output$predictors_for_us_summary <- renderText({
    data <- selectedUS()
    regression <- lm(data ~ baidu + alibaba + huawei + sina + jdcom)
    fmt <- ("Coefficients: \n
            Intercept: %s \n
            Baidu: %s \n
            Alibaba: %s \n
            Huawei: %s \n
            Sina: %s \n
            JDCOM: %s \n\n\n
            R^2: %s")
    sprintf(fmt, regression$coefficients[1], regression$coefficients[2], regression$coefficients[3],
            regression$coefficients[4], regression$coefficients[5], regression$coefficients[6], summary(regression)$r.squared)  })
  
  output$volatility_two <- renderText({
    data1 <- selectedUS()
    data2 <- selectedChina()
    bool <- f_test_twosided(data1, data2, (1 - (input$alpha3)/100))
    if(bool == TRUE){
      sprintf("There is insufficient evidence to suggest that the volatility of both stocks are different at a %d%% confidence level.", input$alpha3)
    }
    else{
      sprintf("There is sufficient evidence to suggest that the volatility of both stocks are different at a %d%% confidence level.", input$alpha3)
    }
  })
  
  output$volatility_less <- renderText({
    data1 <- selectedUS()
    data2 <- selectedChina()
    bool <- f_test_less(data1, data2, (1 - (input$alpha3)/100))
    if(bool == TRUE){
      sprintf("There is insufficient evidence to suggest that the chinese stock is more volatile at a %d%% confidence level.", input$alpha3)
    }
    else{
      sprintf("There is sufficient evidence to suggest that the chinese stock is more volatile at a %d%% confidence level.", input$alpha3)
    }
  })
  
  output$volatility_greater <- renderText({
    data1 <- selectedUS()
    data2 <- selectedChina()
    bool <- f_test_greater(data1, data2, (1 - (input$alpha3)/100))
    if(bool == TRUE){
      sprintf("There is insufficient evidence to suggest that the american stock is more volatile at a %d%% confidence level.", input$alpha3)
    }
    else{
      sprintf("There is sufficient evidence to suggest that the american stock is more volatile at a %d%% confidence level.", input$alpha3)
    }
  })
}

shinyApp(ui = ui, server = server)
