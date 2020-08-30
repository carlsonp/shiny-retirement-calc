library(shiny)
library(tidyverse)
library(lubridate)
library(plotly)
library(shinyWidgets) # https://github.com/dreamRs/shinyWidgets
library(DT)
library(shinydashboard)
library(shinyjs)
library(purrr)
library(profvis)

# don't use scientific notation for display output
options(scipen=999)

shinyServer(function(input, output, session) {
  
  single_run <- function(df) {
    # convert to a data matrix, much much faster than a dataframe for these calculations
    df <- data.matrix(df)
    
    df[,'stock_return_percentage'] = round(sample(stock_normal_dist(), nrow(df)), 2)
    df[,'bond_return_percentage'] = round(sample(bond_normal_dist(), nrow(df)), 2)

    # apply inflation
    df[,'inflation_percentage'] = round(sample(inflation_normal_dist(), nrow(df)), 2)
    for (i in 2:nrow(df)) {
      # apply inflation to FIRE target
      df[i,]['fire_target'] = round(df[i,]['fire_target'] * (1+(df[i,]['inflation_percentage']/100)), 2)
    }

    for (i in 2:nrow(df)) {
      # apply income
      df[i,]['income'] = round(df[i-1,]['income'] * (1+(df[i,]['income_growth_percentage']/100)), 2)
      df[i,]['savings'] = round(df[i-1,]['income'] * (savings_percentage()/100), 2)
    }

    # apply income to purchasing additional brokerage
    df[,'brokerage_stock_amount'] = round(df[,'brokerage_stock_amount'] + (df[,'savings'] * (input$brokerage_stock_percentage/100)), 2)
    df[,'brokerage_bond_amount'] = round(df[,'brokerage_bond_amount'] + (df[,'savings'] * (input$brokerage_bond_percentage/100)), 2)

    for (i in 2:nrow(df)) {
      # apply brokerage
      df[i,]['brokerage_stock_amount'] = round(df[i-1,]['brokerage_stock_amount'] * (1+(df[i,]['stock_return_percentage']/100)), 2)
      df[i,]['brokerage_bond_amount'] = round(df[i-1,]['brokerage_bond_amount'] * (1+(df[i,]['bond_return_percentage']/100)), 2)
    }
    
    df[,'brokerage_amount'] = df[,'brokerage_stock_amount'] + df[,'brokerage_bond_amount']
    df[,'brokerage_stock_percentage'] = round((df[,'brokerage_stock_amount'] / df[,'brokerage_amount']) * 100, 2)
    df[,'brokerage_bond_percentage'] = round((df[,'brokerage_bond_amount'] / df[,'brokerage_amount']) * 100, 2)
    
    # did we hit the FIRE goal? remember this adjusts over time for inflation
    df[,'hit_fire_goal'] = df[,'brokerage_amount'] >= df[,'fire_target']
    
    # TODO: account for asset allocation rebalancing over time
    
    # clean out some of the variables set in the first row since they don't make sense
    df[1,]['inflation_percentage'] = NA
    df[1,]['savings'] = NA
    df[1,]['income_growth_percentage'] = NA
    df[1,]['stock_return_percentage'] = NA
    df[1,]['bond_return_percentage'] = NA

    # convert back to a dataframe
    df <- data.frame(df)
    return(df)
  }
  
  monte_carlo <- reactive({
    num_simulations = 250
    df <- data.frame(age = input$age:80,
                     inflation_percentage = NA,
                     fire_target = fire_target(),
                     hit_fire_goal = NA,
                     income = input$income,
                     income_growth_percentage = input$income_growth_percentage,
                     savings = 0,
                     brokerage_amount = input$brokerage_amount,
                     brokerage_stock_percentage = input$brokerage_stock_percentage,
                     brokerage_stock_amount = input$brokerage_amount * (input$brokerage_stock_percentage/100),
                     stock_return_percentage = NA,
                     brokerage_bond_percentage = input$brokerage_bond_percentage,
                     brokerage_bond_amount = input$brokerage_amount * (input$brokerage_bond_percentage/100),
                     bond_return_percentage = NA
    )
    
    withProgress(message = 'Running Monte Carlo simulation', value = 0, {
      # iterate through monte carlo simulations
      ret <- NULL
      for (i in 1:num_simulations) {
        if (is.null(ret)) {
          ret <- single_run(df) %>% mutate(run = i)
        } else {
          ret <- dplyr::bind_rows(ret, single_run(df) %>% mutate(run = i))
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
  
  hit_fire_target <- reactive({
    grp <- monte_carlo() %>%
      dplyr::group_by(age, hit_fire_goal) %>%
      summarize(cnt = n()) %>%
      mutate(hit_fire_goal = as.character(hit_fire_goal)) %>%
      mutate(hit_fire_goal = ifelse(hit_fire_goal == 1, "Hit_FIRE", "Missed_FIRE")) %>%
      pivot_wider(names_from = hit_fire_goal, values_from = cnt) %>%
      data.frame()
    
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
      monte_carlo() %>% dplyr::filter(run == 1),
      rownames = FALSE, # don't show row index
      # https://rstudio.github.io/DT/options.html
      options = list(scrollX = TRUE),
      selection = 'none'
    )
  })
  
  output$downloadmontecarlo <- downloadHandler(
    # https://shiny.rstudio.com/articles/download.html
    filename = function() {
      paste0("monte_carlo_", format(Sys.time(), "%Y-%m-%d"), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(monte_carlo(), file, row.names = FALSE)
    }
  )
  
  output$brokerage_graph <- renderPlotly({
    shiny::validate(
      need(!is.null(brokerage_percentiles()) & !is.na(brokerage_percentiles()), 'Loading...')
    )
    
    # https://plotly.com/r/line-charts/
    plot_ly(brokerage_percentiles(), x = ~age, y = ~`10th_Percentile`, type = "scatter", mode = "lines", name = '10% Percentile', line = list(color='rgb(205, 12, 24)')) %>%
      add_trace(x = ~age, y = ~`25th_Percentile`, name = '25% Percentile', line = list(color='rgb(15, 12, 240)')) %>%
      add_trace(x = ~age, y = ~`Median`, name = 'Median', line = list(color='rgb(0, 255, 0)')) %>%
      add_trace(x = ~age, y = ~`75th_Percentile`, name = '75% Percentile', line = list(color='rgb(15, 12, 240)')) %>%
      add_trace(x = ~age, y = ~`90th_Percentile`, name = '90% Percentile', line = list(color='rgb(205, 12, 24)')) %>%
      layout(title = "Brokerage Amount",
             xaxis = list(title = "Age"),
             yaxis = list(title = "Brokerage Amount"))
  })
  
  output$hit_fire_target_graph <- renderPlotly({
    shiny::validate(
      need(!is.null(hit_fire_target()) & !is.na(hit_fire_target()), 'Loading...')
    )
    
    # https://plotly.com/r/filled-area-plots/#stacked-area-chart-with-cumulative-values
    plot_ly(hit_fire_target(), x = ~age, y = ~Hit_FIRE, name = 'Hit FIRE Goal',
            type = 'scatter', mode = 'none', stackgroup = 'one', groupnorm = 'percent', fillcolor = 'rgb(5, 245, 24)') %>%
      add_trace(x = ~age, y = ~Missed_FIRE, name = 'Missed FIRE Goal', fillcolor = 'rgb(205, 12, 24)') %>%
      layout(title = "Hit FIRE Target?",
             xaxis = list(title = "Age"),
             yaxis = list(title = "Percentage of simulations that hit FIRE target"))
  })
})
