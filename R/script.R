library(RSelenium)

system("docker pull selenium/standalone-chrome", wait=TRUE)
Sys.sleep(5)
system("docker run -d -p 4445:4444 selenium/standalone-chrome", wait=TRUE)
Sys.sleep(5)

remDr <- remoteDriver("localhost", 4445L, "chrome")
remDr$open()
remDr$navigate("https://phptravels.com/demo")
html <- remDr$getPageSource()
writeChar(html[[1]], "result.html")
