require(rsconnect)
deployApp(appFiles = c("ui.R", "server.R", "www/cerulean.css"),
          appName = "extreme-explorer")
