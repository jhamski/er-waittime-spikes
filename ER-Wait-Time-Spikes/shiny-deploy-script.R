library(rsconnect)

writeManifest(appDir = getwd())
deployApp(
  appName = "ER-Wait-Time-Spikes", 
  upload = TRUE, 
  appFileManifest = "manifest.txt",
  account = "jhamski@api.shinyapps.io", 
  server = "shinyapps.io")

