# global.R

library(shiny)
library(shinyalert)
library(gt)
library(bslib)
library(shinythemes)
library(ggplot2)
library(katex)
source("ui.R", local = TRUE)
source("server.R")

shinyApp(ui = ui, server = server)
