library(rsconnect)

# Set the account info for deployment.
setAccountInfo(name   = Sys.getenv("SHINYAPPS_NAME"),
               token  = Sys.getenv("SHINYAPPS_TOKEN"),
               secret = Sys.getenv("SHINYAPPS_SECRET"))

# Deploy the application.
deployApp()