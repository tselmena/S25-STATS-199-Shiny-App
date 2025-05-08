# ui.R =========================================================================

ui <- fluidPage(
  
  # theme
  theme = shinytheme("flatly"),
  
  # title
  titlePanel("UCLA Intro Stats Calculator"),
  
  tabsetPanel(
    # ======================================================================
    # TAB 1: One Proportion (ui.R snippet)
    # ======================================================================
    tabPanel("One Proportion",
             sidebarLayout(
               sidebarPanel(
                 # checkbox for CI
                 checkboxInput("show_ci", "Confidence Interval", FALSE),
                 
                 # If CI is shown, let the user select the confidence level
                 conditionalPanel(
                   condition = "input.show_ci == true",
                   selectInput(
                     inputId = "conf_level",
                     label = "Confidence Level:",
                     choices = c("90%" = 0.90, "95%" = 0.95, "99%" = 0.99),
                     selected = 0.95
                   )
                 ),
                 # checkbox to turn the entire test on/off
                 checkboxInput("show_test", "Test", TRUE),
                 
                 # only show the test-type radio buttons if "Test" is checked
                 conditionalPanel(
                   condition = "input.show_test == true",
                   radioButtons(
                     inputId = "alternative",
                     label = "Select the type of test:",
                     choices = c("Two-sided" = "two.sided",
                                 "Left-tailed" = "less",
                                 "Right-tailed"= "greater"),
                     selected = "two.sided"
                   )
                 ),
                 
                 # Mumeric inputs for the test
                 numericInput("p", "Hypothesized Proportion", 0.5),
                 numericInput("n", "Sample Size", 30),
                 numericInput("p_hat", "Sample Proportion", 0.6)
               ),
               
               mainPanel(
                 # main plot (always on)
                 plotOutput("plot"),
                 # confidence interval text
                 textOutput("ci_label"),
                 tags$head(
                   tags$style("#ci_label {color: ##000000; font-size: 25px; font-style: bold;}")
                 ),
                 textOutput("ci_conclusion"),
                 # this is just to change color / make font larger
                 tags$head(
                   tags$style("#ci_conclusion {color: #2774AE; font-size: 20px; font-style: bold;}")
                 ),
        
                 # if "Test" is checked, show results and conclusions
                 conditionalPanel(
                   condition = "input.show_test == true",
                   gt_output("results_table"),
                   gt_output("conclusions")
                 ),
                 
                 # table for CI (rendered by show_ci)
                 #gt_output("ci_table")
               )
             )
    ),
    
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
        sidebarPanel(
          selectInput("mode", "Select Mode:", 
                      choices = c("Distribution Calculator" = "normal", 
                                  "Inverse Calculator" = "inverse")),
          
          numericInput("mean", "Mean", value = 0, step = 0.01),
          numericInput("sd", "Standard Deviation", value = 1, step = 0.01),
          
          radioButtons(
            inputId = "range",
            label = "Select Range:",
            choices = c("Above" = "above", "Below" = "below", 
                        "Between" = "between", "Outside" = "outside"),
            selected = "between"  # default now set to "between"
          ),
          
          conditionalPanel(
            condition = "input.mode == 'inverse' && (input.range == 'between' || input.range == 'outside')",
            checkboxInput("symmetric", "Symmetric Thresholds", value = TRUE)
          ),
          
          uiOutput("dynamic_inputs"),
          
          conditionalPanel(
            condition = "input.mode == 'inverse' && (input.range == 'between' || input.range == 'outside')",
            numericInput("prob_input", "Desired Probability", value = 0.95, step = 0.01)
          ),
          
          conditionalPanel(
            condition = "input.mode == 'inverse' && input.range != 'between' && input.range != 'outside'",
            numericInput("prob_input", "Probability", value = 0.95, step = 0.01)
          ),
          
          conditionalPanel(
            condition = "input.mode == 'inverse' && input.range != 'between' && input.range != 'outside'",
            radioButtons("known_side", "Known Threshold Is:",
                         choices = c("Lower" = "lower", "Upper" = "upper"),
                         selected = "lower")
          )
        ),
        
        mainPanel(
          textOutput("norm_prob"),
          textOutput("threshold_text"),
          plotOutput("norm_plot")
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
