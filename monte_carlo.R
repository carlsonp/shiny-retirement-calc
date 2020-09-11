single_run_preretirement <- function(df) {
  # convert to a data matrix, much much faster than a dataframe for these calculations
  # cannot store character strings though!
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
    
    # brokerage asset rebalance
    if (input$rebalanceassets) {
      df[i,]['brokerage_amount'] = round(df[i,]['brokerage_stock_amount'] + df[i,]['brokerage_bond_amount'], 2)
      
      delta_stock = round(df[i,]['brokerage_stock_amount'] - (df[i,]['brokerage_amount'] * (input$target_stock_percentage/100)), 2)
      delta_bond = round(df[i,]['brokerage_bond_amount'] - (df[i,]['brokerage_amount'] * (input$target_bond_percentage/100)), 2)
      df[i,]['brokerage_stock_amount'] = df[i,]['brokerage_stock_amount'] - delta_stock
      df[i,]['brokerage_bond_amount'] = df[i,]['brokerage_bond_amount'] - delta_bond
    }
  }
  
  df[,'brokerage_amount'] = df[,'brokerage_stock_amount'] + df[,'brokerage_bond_amount']
  df[,'brokerage_stock_percentage'] = round((df[,'brokerage_stock_amount'] / df[,'brokerage_amount']) * 100, 2)
  df[,'brokerage_bond_percentage'] = round((df[,'brokerage_bond_amount'] / df[,'brokerage_amount']) * 100, 2)
  
  # did we hit the FIRE goal? remember this adjusts over time for inflation
  df[,'hit_fire_goal'] = df[,'brokerage_amount'] >= df[,'fire_target']
  
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



single_run_retirement <- function(df, yrs, death_projections) {
  # convert to a data matrix, much much faster than a dataframe for these calculations
  # cannot store character strings though!
  df <- data.matrix(df)
  
  # take yrs and run for that many iterations to simulate a "starting" retirement spending
  # amount adjusted for inflation
  spending = input$retirement_spending
  for (i in 1:yrs) {
    spending = spending * (1+(sample(inflation_normal_dist(), 1)/100))
  }
  df[,'retirement_spending'] = round(spending, 2)
  
  # apply year going forward
  for (i in 2:nrow(df)) {
    df[i,]['year'] = df[i-1,]['year']+1
  }
  
  if (input$useavglife) {
    # apply mortality chance based on gender
    for (i in 2:nrow(df)) {
      mortality_df <- death_projections %>%
        filter(gender %in% input$gender) %>%
        filter(age == df[i,]['age']) %>%
        filter(Year == df[i,]['year'])
      
      # account for cases outside the range of mortality data we have available
      # TODO: should we try to extrapolate future predictions outside of the social security ones?
      if (nrow(mortality_df) != 0) {
        if (df[i-1,]['deceased'] == 0 & runif(1) <= mortality_df$probability) {
          df[i,]['deceased'] = 1
        } else if (df[i-1,]['deceased'] == 1) {
          df[i,]['deceased'] = 1
        } else {
          # no-op, already set to 0
        }
      } else {
        # unknown, no data available
        df[i,]['deceased'] = 2
      }
    }
  }
  
  df[,'stock_return_percentage'] = round(sample(stock_normal_dist(), nrow(df)), 2)
  df[,'bond_return_percentage'] = round(sample(bond_normal_dist(), nrow(df)), 2)
  
  # apply inflation
  df[,'inflation_percentage'] = round(sample(inflation_normal_dist(), nrow(df)), 2)
  for (i in 2:nrow(df)) {
    # apply inflation to spending
    df[i,]['retirement_spending'] = round(df[i,]['retirement_spending'] * (1+(df[i,]['inflation_percentage']/100)), 2)
  }
  
  for (i in 2:nrow(df)) {
    # apply brokerage
    df[i,]['brokerage_stock_amount'] = round(df[i-1,]['brokerage_stock_amount'] * (1+(df[i,]['stock_return_percentage']/100)), 2)
    df[i,]['brokerage_bond_amount'] = round(df[i-1,]['brokerage_bond_amount'] * (1+(df[i,]['bond_return_percentage']/100)), 2)
    
    # apply spending
    df[i,]['brokerage_stock_amount'] = round(df[i,]['brokerage_stock_amount'] - (df[i,]['retirement_spending'] * (input$target_stock_retirement_percentage/100)), 2)
    df[i,]['brokerage_bond_amount'] = round(df[i,]['brokerage_bond_amount'] - (df[i,]['retirement_spending'] * (input$target_bond_retirement_percentage/100)), 2)
    
    # brokerage asset rebalance
    df[i,]['brokerage_amount'] = round(df[i,]['brokerage_stock_amount'] + df[i,]['brokerage_bond_amount'], 2)
    
    delta_stock = round(df[i,]['brokerage_stock_amount'] - (df[i,]['brokerage_amount'] * (input$target_stock_retirement_percentage/100)), 2)
    delta_bond = round(df[i,]['brokerage_bond_amount'] - (df[i,]['brokerage_amount'] * (input$target_bond_retirement_percentage/100)), 2)
    df[i,]['brokerage_stock_amount'] = df[i,]['brokerage_stock_amount'] - delta_stock
    df[i,]['brokerage_bond_amount'] = df[i,]['brokerage_bond_amount'] - delta_bond
  }
  
  df[,'brokerage_amount'] = df[,'brokerage_stock_amount'] + df[,'brokerage_bond_amount']
  df[,'brokerage_stock_percentage'] = round((df[,'brokerage_stock_amount'] / df[,'brokerage_amount']) * 100, 2)
  df[,'brokerage_bond_percentage'] = round((df[,'brokerage_bond_amount'] / df[,'brokerage_amount']) * 100, 2)
  
  # are we broke?
  df[,'broke'] = df[,'brokerage_amount'] <= 0
  
  # clean out some of the variables set in the first row since they don't make sense
  df[1,]['inflation_percentage'] = NA
  df[1,]['stock_return_percentage'] = NA
  df[1,]['bond_return_percentage'] = NA
  
  # convert back to a dataframe
  df <- data.frame(df)
  
  # apply gender back in since it's a character and not numeric
  df$gender = input$gender
  
  return(df)
}