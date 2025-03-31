# global.R
#
# implementing a simple two-sided one-proportion test based on:
# https://www.wolframalpha.com/input?i=one+proportion+hypothesis+test&assumption=%7B%22F%22%2C+%22ProportionTest%22%2C+%22phat%22%7D+-%3E%220.6%22&assumption=%7B%22F%22%2C+%22ProportionTest%22%2C+%22p0%22%7D+-%3E%220.5%22&assumption=%7B%22F%22%2C+%22ProportionTest%22%2C+%22n%22%7D+-%3E%2230%22

library(shiny)
source("ui.R", local = TRUE)
source("server.R")

shinyApp(ui = ui, server = server)
