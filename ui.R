library(shiny)
library(plotly)
library(shinyWidgets)
library(DT)
library(leaflet)
library(shinydashboard)
library(shinyjs)

dashboardPage(
  
  dashboardHeader(title = "Retirement Calculator"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Introduction", tabName = "Introduction", icon = icon("comment-dollar")),
      menuItem("Global Settings", tabName = "GlobalSettings", icon = icon("sliders-h")),
      menuItem("FIRE Settings", tabName = "FIRESettings", icon = icon("sliders-h")),
      menuItem("FIRE", tabName = "FIRE", icon = icon("chart-line")),
      menuItem("Retirement Settings", tabName = "RetirementSettings", icon = icon("sliders-h")),
      menuItem("Retirement", tabName = "Retirement", icon = icon("chart-area")),
      menuItem("About", tabName = "About", icon = icon("question"))
    )
  ),
  
  dashboardBody(
    shinyjs::useShinyjs(),
    
    # https://stackoverflow.com/questions/44043475/adjust-size-of-shiny-progress-bar-and-center-it
    # center Shiny progress bar in middle of screen
    tags$head(
      tags$style(
        HTML(".shiny-notification {
              height: 100px;
              width: 800px;
              position:fixed;
              top: calc(50% - 50px);;
              left: calc(50% - 400px);;
            }
           "
        )
      )
    ),
    
    tabItems(
      tabItem(tabName = "Introduction",
        tags$p("This is not intended to be fullproof and makes many assumptions, seek a financial professional for more in-depth individual advice."),
        tags$p("BUTTON HERE to Global settings")
      ),
      tabItem(tabName = "GlobalSettings",
        numericInput("age", "Current Age:", 35, min = 1, max = 78, step = 1),
        numericInput("retirement_spending", "Retirement Spending ($):", 40000, min = 1, step = 1),
        fixedRow(
          column(3,
             numericInput("avg_stock_return_percentage", "Average Stock Return Percentage:", 8.1, min = 0.1, max = 100, step = 0.1)
          ),
          column(3,
             numericInput("stock_stddev", "Stock Return Percentage Standard Deviation:", 17, min = 0, max = 75, step = 0.1)
          )
        ),
        plotlyOutput("stock_histogram"),
        fixedRow(
          column(3,
             numericInput("avg_bond_return_percentage", "Average Bond Return Percentage:", 2.4, min = 0.1, max = 100, step = 0.1)
          ),
          column(3,
             numericInput("bond_stddev", "Bond Return Percentage Standard Deviation:", 7, min = 0, max = 75, step = 0.1)
          )
        ),
        plotlyOutput("bond_histogram"),
        fixedRow(
          column(3,
             numericInput("avg_inflation_percentage", "Average Inflation Percentage:", 2, min = 0.1, max = 100, step = 0.1)
          ),
          column(3,
             numericInput("inflation_stddev", "Inflation Percentage Standard Deviation:", 4, min = 0, max = 75, step = 0.1)
          )
        ),
        plotlyOutput("inflation_histogram"),
        tags$p("BUTTON HERE to FIRE Settings")
      ),
      tabItem(tabName = "FIRESettings",
        fixedRow(
          column(3,
            numericInput("brokerage_amount", "Brokerage Investments ($):", value = 50000, min = 0, step = 1)
          ),
          column(3,
            uiOutput("stock_slider")
          ),
          column(3,
            uiOutput("bond_slider")
          )
        ),
        fixedRow(
          column(3,
            numericInput("income", "Income ($):", 60000, min = 1, step = 1)
          ),
          column(3,
            numericInput("spending", "Pre-Retirement Spending ($):", 45000, min = 1, step = 1)
          ),
          column(3,
            uiOutput("savings")
          )
        ),
        fixedRow(
          column(3,
            checkboxInput("rebalanceassets", "Rebalance brokerage asset allocation yearly", TRUE)
          ),
          column(3,
            uiOutput("target_stock")
          ),
          column(3,
            uiOutput("target_bond")
          )
        ),
        numericInput("income_growth_percentage", "Income Growth Percentage:", 1, min = 0, max = 100, step = 0.1),
        uiOutput("retirementSpending"),
        numericInput("target_withdrawl_percentage", "Target Withdrawl Rate Percentage:", 4, min = 0.1, step = 0.1),
        numericInput("avg_tax_rate_percentage", "Average Tax Rate Percentage:", 7, min = 0.1, max = 100, step = 0.1),
        tags$p("In todays dollars"),
        uiOutput("fire_target_ui"),
        tags$p("BUTTON HERE to FIRE")
      ),
      tabItem(tabName = "FIRE",
        downloadButton("downloadmontecarlo", "Download Monte Carlo Pre-Retirement Data"),
        tags$h3("Sample of the first Monte Carlo run:"),
        DT::dataTableOutput("montecarlo_table"),
        plotlyOutput("brokerage_graph"),
        plotlyOutput("hit_fire_target_graph"),
        tags$p("BUTTON HERE to Retirement Settings")
      ),
      tabItem(tabName = "RetirementSettings",
        numericInput("retirementage", "Retirement Age:", 65, min = 1, max = 100, step = 1),
        uiOutput("retirementyear"),
        uiOutput("retirementSpendingDup"),
        numericInput("retirementsavings", "Savings Upon Retirement ($):", 2000000, min = 1, step = 1),
        fixedRow(
          column(3,
            uiOutput("stock_current_slider")
          ),
          column(3,
            uiOutput("bond_current_slider")
          )
        ),
        fixedRow(
          column(3,
            uiOutput("target_stock_retirement")
          ),
          column(3,
            uiOutput("target_bond_retirement")
          )
        ),
        fixedRow(
          column(3,
             checkboxInput("useavglife", "Use average life expectancy data", TRUE)
          ),
          column(3,
             uiOutput("setupavglife")
          )
        ),
        tags$p("BUTTON HERE to retirement")
      ),
      tabItem(tabName = "Retirement",
        downloadButton("downloadmontecarloretirement", "Download Monte Carlo Retirement Data"),
        tags$h3("Sample of the first Monte Carlo run:"),
        DT::dataTableOutput("montecarlo_table_retirement"),
        plotlyOutput("brokerage_retirement_graph"),
        plotlyOutput("broke_graph"),
        plotlyOutput("deceased_graph")
      ),
      tabItem(tabName = "About",
      )
    )
  )
)