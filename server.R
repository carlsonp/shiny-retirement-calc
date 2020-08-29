library(shiny)
library(tidyverse)
library(lubridate)
library(plotly)
library(shinyWidgets) # https://github.com/dreamRs/shinyWidgets
library(DT)
library(shinydashboard)
library(shinyjs)
library(purrr)

# don't use scientific notation for display output
options(scipen=999)

shinyServer(function(input, output, session) {
  
  single_run <- function(df) {
    for (i in 2:nrow(df)) {
      df[i,]$stock_return_percentage = sample(stock_normal_dist(), 1)
      df[i,]$bond_return_percentage = sample(bond_normal_dist(), 1)
      
      # apply inflation
      df[i,]$inflation_percentage = sample(inflation_normal_dist(), 1)
      df[i,]$stock_return_percentage_adj_inflation = df[i,]$stock_return_percentage - df[i,]$inflation_percentage
      df[i,]$bond_return_percentage_adj_inflation = df[i,]$bond_return_percentage - df[i,]$inflation_percentage
      df[i,]$income_growth_percentage_adj_inflation = df[i,]$income_growth_percentage - df[i,]$inflation_percentage
      
      # apply income
      df[i,]$income = df[i-1,]$income * (1+(df[i,]$income_growth_percentage_adj_inflation/100))
      df[i,]$savings = df[i-1,]$income * (savings_percentage()/100)
      # apply income to purchasing additional brokerage
      df[i,]$brokerage_stock_amount = df[i,]$brokerage_stock_amount + (df[i,]$savings * (input$brokerage_stock_percentage/100))
      df[i,]$brokerage_bond_amount = df[i,]$brokerage_bond_amount + (df[i,]$savings * (input$brokerage_bond_percentage/100))
      df[i,]$brokerage_amount = df[i,]$brokerage_stock_amount + df[i,]$brokerage_bond_amount
      
      # apply brokerage
      df[i,]$brokerage_stock_amount = df[i-1,]$brokerage_stock_amount * (1+(df[i,]$stock_return_percentage/100))
      df[i,]$brokerage_bond_amount = df[i-1,]$brokerage_bond_amount * (1+(df[i,]$bond_return_percentage/100))
      df[i,]$brokerage_amount = df[i,]$brokerage_stock_amount + df[i,]$brokerage_bond_amount
      df[i,]$brokerage_stock_percentage = round((df[i,]$brokerage_stock_amount / df[i,]$brokerage_amount) * 100, 2)
      df[i,]$brokerage_bond_percentage = round((df[i,]$brokerage_bond_amount / df[i,]$brokerage_amount) * 100, 2)
      
      # TODO: account for asset allocation rebalancing over time
      
    }
    
    return(df)
  }
  
  monte_carlo <- reactive({
    num_simulations = 1000
    df <- data.frame(age = input$age:80,
                     inflation_percentage = NA,
                     income = input$income,
                     income_growth_percentage = input$income_growth_percentage,
                     income_growth_percentage_adj_inflation = NA,
                     savings = NA,
                     brokerage_amount = input$brokerage_amount,
                     brokerage_stock_percentage = input$brokerage_stock_percentage,
                     brokerage_stock_amount = input$brokerage_amount * (input$brokerage_stock_percentage/100),
                     stock_return_percentage = NA,
                     stock_return_percentage_adj_inflation = NA,
                     brokerage_bond_percentage = input$brokerage_bond_percentage,
                     brokerage_bond_amount = input$brokerage_amount * (input$brokerage_bond_percentage/100),
                     bond_return_percentage = NA,
                     bond_return_percentage_adj_inflation = NA
    )
    
    withProgress(message = 'Running Monte Carlo simulation', value = 0, {
      # iterate through monte carlo simulations
      ret <- NULL
      for (i in 1:num_simulations) {
        if (is.null(ret)) {
          ret <- single_run(df) %>% mutate(run = i)
        } else {
          ret <- rbind(ret, single_run(df) %>% mutate(run = i))
        }
        incProgress(1/num_simulations, detail = paste0((i/num_simulations)*100,"%"))
      }
    })
    return(ret)
  })
  
  brokerage_percentiles <- reactive({
    # https://tbradley1013.github.io/2018/10/01/calculating-quantiles-for-groups-with-dplyr-summarize-and-purrr-partial/
    # calculate percentiles for brokerage amounts at each age
    p <- c(0.1, 0.25, 0.5, 0.75, 0.9)
    p_funs <- purrr::map(p, ~purrr::partial(quantile, probs = .x, na.rm = TRUE)) %>% 
      purrr::set_names(nm = p)
    
    grp <- monte_carlo() %>%
      dplyr::group_by(age) %>% 
      dplyr::summarize_at(vars(brokerage_amount), funs(!!!p_funs)) %>%
      data.frame() %>%
      dplyr::rename("10th_Percentile" = X0.1) %>%
      dplyr::rename("25th_Percentile" = X0.25) %>%
      dplyr::rename("Median" = X0.5) %>%
      dplyr::rename("75th_Percentile" = X0.75) %>%
      dplyr::rename("90th_Percentile" = X0.9)
      
    return(grp)
  })
  
  output$stock_slider <- renderUI({
    sliderInput("brokerage_stock_percentage", "Brokerage Stock Percentage", value = 80, min = 0, max = 100, step = 1)
  })
  
  output$bond_slider <- renderUI({
    shinyjs::disabled(sliderInput("brokerage_bond_percentage", "Brokerage Bond Percentage", value = 100-input$brokerage_stock_percentage, min=0, max=100))
  })
  
  savings <- reactive({
    return(input$income - input$spending)
  })
  
  savings_percentage <- reactive({
    return(round(((input$income - input$spending) / input$income) * 100, 2))
  })
  
  output$savings <- renderUI({
    shiny::validate(
      need(!is.null(savings) & !is.null(savings_percentage()), 'Loading...')
    )
    tags$p(paste0("Savings: $", format(savings(), big.mark=",", scientific=FALSE), " (", savings_percentage(), "%)"), style="font-size:18px;")
  })
  
  fire_target <- reactive({
    tgt = (input$retirement_spending * (1+(input$avg_tax_rate_percentage/100))) * (100/input$target_withdrawl_percentage)
    return(tgt)
  })
  
  output$fire_target_ui <- renderUI({
    shiny::validate(
      need(!is.null(fire_target()), 'Loading...')
    )
    tags$p(paste0("FIRE Target: $", format(fire_target(), big.mark=",", scientific=FALSE)), style="font-size:18px;")
  })
  
  stock_normal_dist <- reactive({
    return(rnorm(5000, mean=input$avg_stock_return_percentage, sd=input$stock_stddev))
  })
  
  bond_normal_dist <- reactive({
    return(rnorm(5000, mean=input$avg_bond_return_percentage, sd=input$bond_stddev))
  })
  
  inflation_normal_dist <- reactive({
    return(rnorm(5000, mean=input$avg_inflation_percentage, sd=input$inflation_stddev))
  })
  
  output$stock_histogram <- renderPlotly({
    # https://plotly.com/r/histograms/
    plot_ly(x = ~stock_normal_dist(), type = "histogram") %>%
      layout(title = "Stock Returns Percentage Distribution",
             xaxis = list(title = "Return Percentage"),
             yaxis = list(title = "Number of Occurences"))
  })
  
  output$bond_histogram <- renderPlotly({
    # https://plotly.com/r/histograms/
    plot_ly(x = ~bond_normal_dist(), type = "histogram") %>%
      layout(title = "Bond Returns Percentage Distribution",
             xaxis = list(title = "Return Percentage"),
             yaxis = list(title = "Number of Occurences"))
  })
  
  output$inflation_histogram <- renderPlotly({
    # https://plotly.com/r/histograms/
    plot_ly(x = ~inflation_normal_dist(), type = "histogram") %>%
      layout(title = "Inflation Percentage Distribution",
             xaxis = list(title = "Percentage"),
             yaxis = list(title = "Number of Occurences"))
  })
  
  output$montecarlo_table <- DT::renderDataTable({
    shiny::validate(
      need(!is.null(monte_carlo()) & !is.na(monte_carlo()), 'Loading...')
    )
    
    DT::datatable(
      monte_carlo(),
      rownames = FALSE, # don't show row index
      # https://rstudio.github.io/DT/options.html
      options = list(scrollX = TRUE)
    )
  })
  
  output$retirement_graph <- renderPlotly({
    shiny::validate(
      need(!is.null(brokerage_percentiles()) & !is.na(brokerage_percentiles()), 'Loading...')
    )
    
    plot_ly(brokerage_percentiles(), x = ~age, y = ~`90th_Percentile`, type = "scatter", mode = "lines", name = '90% Percentile') %>%
      add_trace(x = ~age, y = ~`75th_Percentile`, name = '75% Percentile', fill = 'tonexty') %>%
      add_trace(x = ~age, y = ~`Median`, name = 'Median') %>%
      add_trace(x = ~age, y = ~`25th_Percentile`, name = '25% Percentile', fill = 'tonexty') %>%
      add_trace(x = ~age, y = ~`10th_Percentile`, name = '10% Percentile', fill = 'tonexty') %>%
      layout(title = "Retirement Graph",
             xaxis = list(title = "Age"),
             yaxis = list(title = "Brokerage Amount"))
  })
})
