# shiny-retirement-calc

[![Actions Status](https://github.com/carlsonp/shiny-retirement-calc/workflows/github-actions/badge.svg)](https://github.com/carlsonp/shiny-retirement-calc/actions)

[Shiny](https://shiny.rstudio.com/) app for performing retirement planning via Monte Carlo simulation, FIRE, etc.

[Deployed on ShinyApps.io](https://carlsonp.shinyapps.io/shiny-retirement-calc/)

## Development

* Install packages with `install.packages()`
* Run the Shiny app via RStudio desktop

## CI/CD

* Automated deployment via Github Actions
* `SHINYAPPS_NAME`, `SHINYAPPS_SECRET`, and `SHINYAPPS_TOKEN` are repository secrets in Github where the
values include single quotes around them.  The name is the Github account username.

## Data

* [Social Security Death Probabilities Projections](https://www.ssa.gov/oact/HistEst/Death/2022/DeathProbabilities2022.html)

## Open Questions

* Are stock returns, bond returns, and inflation normally distributed?
* How do correlations between asset classes impact results?  Are there correlations between inflation, interest rates, and market returns?

## TODOs (add variables or account for)

* Nursing home costs
* Medical care / insurance
* Long term care insurance
* Taxes (current and in retirement), pre-tax and post-tax
* 401k/403b and employee matching
* Traditional and Roth IRAs
* Social security, pensions, etc.
* Asset reallocation over time to mimic target retirement funds, more bonds over stocks, etc.
* Inheritance

## Online Calculators

* [Vanguard retirement nest egg calculator](https://retirementplans.vanguard.com/VGApp/pe/pubeducation/calculators/RetirementNestEggCalc.jsf)
* [Vanguard retirement income calculator](https://retirementplans.vanguard.com/VGApp/pe/pubeducation/calculators/RetirementIncomeCalc.jsf)
* [Engaging Data FIRE calculator](https://engaging-data.com/fire-calculator/)
* [Engaging Data 4% rule calculator](https://engaging-data.com/visualizing-4-rule/)
* [Engaging Data marginal tax rates calculator](https://engaging-data.com/marginal-tax-rates/)
* [Engaging Data tax brackets calculator](https://engaging-data.com/tax-brackets/)
* [Engaging Data post-retirement calculator](https://engaging-data.com/will-money-last-retire-early/)

## Resources and Readings

* [Criticisms of Monte Carlo](http://retirementoptimizer.com/)
* [Monte Carlo in Python](https://pbpython.com/monte-carlo.html)
