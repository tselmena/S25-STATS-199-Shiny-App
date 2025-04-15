
# ui.R =========================================================================

ui <- fluidPage(
  
  # theme
  theme = shinytheme("flatly"),
  
  # title
  titlePanel("UCLA Intro Stats Calculator"),
  
  tabsetPanel(
    # ======================================================================
    # TAB 1: One Proportion Test
    # ======================================================================
    tabPanel("One Proportion", sidebarLayout(
      sidebarPanel(
        # allow users to check confidence interval and/or test results
        checkboxInput("show_ci", "Confidence Interval", TRUE),
        radioButtons(
          inputId = "alternative",
          label = "Select the type of test:",
          choices = c(
            "Two-sided" = "two.sided",
            "Left-tailed"  = "less",
            "Right-tailed" = "greater"
          ),
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
        tags$head(
          tags$style(
            "#ci_conclusion{color: #2774AE;
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
    )),
    
    # ======================================================================
    # TAB 2: One Mean
    # ======================================================================
    tabPanel("One Mean", sidebarLayout(
      sidebarPanel(
        # select test using buttons (not reactive, user has to click calculate again to update test)
        radioButtons(
          inputId = "mean_alt",
          label = "Select the type of test:",
          choices = c(
            "Two-sided" = "two.sided",
            "Left-sided" = "less",
            "Right-sided" = "greater"
          ),
          selected = "two.sided" # default
        ),
        
        # each input given default values
        numericInput("mu", "Hypothesized Mean", 100),
        numericInput("n", "Sample Size", 25),
        numericInput("x_bar", "Sample Mean", 98),
        numericInput("s", "Sample Standard Deviation", 6)
      ),
      
      # output
      mainPanel(
        plotOutput("one_mean_plot"),
        gt_output("one_mean_test_table")
      )
    )),
    
    # ======================================================================
    # TAB 3: Difference Two Proportion
    # ======================================================================
    
    tabPanel(
      "Difference Two Proportion",
      sidebarLayout(sidebarPanel(
          
          # insert content here
        
        ), 
        mainPanel(
          
          # insert content here
          
        )
      )
    ),
    
    # ======================================================================
    # TAB 4: Difference Two Means
    # ======================================================================
    tabPanel(
      "Difference Two Means",
      sidebarLayout(sidebarPanel(
        
        # insert content here
        
        ), 
        mainPanel(
        
        # insert content here
        
        )
      )
    ),
    
    # ======================================================================
    # TAB 5: Normal Distribution
    # ======================================================================
    
    tabPanel(
      "Normal Distribution",
      sidebarLayout(
        # within this panel, create another panel containing three inputs
        sidebarPanel(
          numericInput("mean", "Mean", 0),
          
          numericInput("sd", "Standard Deviation", 1),
          
          # select test using buttons (not reactive, user has to click calculate again to update test)
          radioButtons(
            inputId = "range",
            label = "Select the range:",
            choices = c(
              "Above" = "above",
              "Below"  = "below",
              "Between" = "between",
              "Outside" = "outside"
            ),
            selected = "above" # default selection
          ),
          
          uiOutput("dynamic_inputs")
          
          # output
          
        ),
        mainPanel(textOutput("norm_prob"), plotOutput("norm_plot"))
      )
    ),
    
    # ======================================================================
    # TAB 6: t-Distribution
    # ======================================================================
    
    tabPanel(
      "t-Distribution",
      sidebarLayout(sidebarPanel(
        
        # insert content here
        
        ), 
      mainPanel(
        
        # insert content here
        
        )
      )
    ),
    
    # ======================================================================
    # TAB 7: Chi-square Distribution
    # ======================================================================
    
    tabPanel(
      "Chi-square",
      sidebarLayout(sidebarPanel(
        
        # insert content here
        
        ), 
      mainPanel(
        
        # insert content here
        
        )
      )
    ),
  )
)
