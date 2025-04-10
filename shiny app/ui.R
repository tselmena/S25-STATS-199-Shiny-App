# ui.R =========================================================================

ui <- fluidPage(
  # theme 
  theme = shinytheme("flatly"),
  # title
  titlePanel("UCLA Intro Stats Calculator"),
  
  tabsetPanel(
    tabPanel("One Proportion Test",
      sidebarLayout(
        sidebarPanel(
          # allow users to check confidence interval and/or test results
          checkboxInput("show_ci", "Confidence Interval", TRUE),
          radioButtons(
            inputId = "alternative", 
            label = "Select the type of test:",
            choices = c("Two-sided" = "two.sided", 
                        "Left-tailed"  = "less", 
                        "Right-tailed" = "greater"),
            selected = "two.sided" # default selection
          ),
          # each input given default values
          numericInput("p", "Hypothesized Proportion", 0.5),
          numericInput("n", "Sample Size", 30),
          numericInput("p_hat", "Sample Proportion", 0.6),
        ),
      
      # output
      mainPanel(
          # fluidRow(
          #   column(width = 6, plotOutput("plot")), 
          #   column(width = 6, gt_output("test_table"), gt_output("conclusions")),
          # ),
          # fluidRow(
          #   column(width = 12, gt_output("ci_table"),textOutput("ci_conclusion"))
          # )
        plotOutput("plot"),
        #gt_output("test_table"), 
        #gt_output("conclusions"), 
        #gt_output("ci_table"),
        textOutput("ci_conclusion"),
        tags$head(tags$style("#ci_conclusion{color: #2774AE;
                                 font-size: 20px;
                                 font-style: bold;
                                 }"
          )
        ),
        # Inside mainPanel (or anywhere you like)
        #gt_output("spanner_table")
        gt_output("combined_table"),
        
        #h3("ci_conclusion")
        
        )
      )
    ),
    # add more tabs here, ex:
    tabPanel("Two Proportion Test"),
    tabPanel("Normal Test")
  )
)
