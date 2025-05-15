# ui.R =========================================================================

ui <- fluidPage(
  
  # theme
  theme = shinytheme("flatly"),
  
  mathjax = TRUE,
  # title
  titlePanel("UCLA Stats Calculator"),
  
  tabsetPanel(
    # ======================================================================
    # TAB 1: One Proportion (ui.R snippet)
    # ======================================================================
    tabPanel("One Proportion",
             sidebarLayout(
               sidebarPanel(
                 # check box for CI and test ON
                 checkboxInput("show_ci", "Confidence Interval", TRUE),
                 checkboxInput("show_test", "Test", TRUE),
                 
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

                 # Numeric inputs for the test
                 conditionalPanel(
                   condition = "!(input.show_ci == true && input.show_test == false)",
                   numericInput("p", HTML("Hypothesized proportion (p<sub>0</sub>)"), 0.50, step = 0.01)
                 ),
                 
                 numericInput("n", "Sample Size (n)", 30, step = 1),
                 
                 conditionalPanel(
                   condition = "input.use_successes == false",
                   numericInput("p_hat", HTML("Sample proportion (p&#770;)"), 0.30, step = 0.01)
                 ),
                 
                 conditionalPanel(
                   condition = "input.use_successes == true",
                   numericInput("x_succ", "Number of successes  (x)", 10, step = 1)
                 ),
                 
                 checkboxInput("use_successes",
                               "Use number of successes instead", FALSE)
               ),
               
               mainPanel(
                 plotOutput("plot"),
                 # TEST ON 
                 conditionalPanel(
                   condition = "input.show_test == true",
                   splitLayout(
                     cellWidths = c("30%", "55%"),
                     cellArgs   = list(style = "padding:0; margin:0; vertical-align:top;"),
                     
                     # left  column
                     div(gt_output("results_table")),
                     
                     # right column: conclusions + CI
                     div(
                       style = "margin-left:20px;              /* horizontal gap */        \
             display:flex; flex-direction:column; row-gap:0px;",
                       gt_output("conclusions"),
                       conditionalPanel(
                         condition = "input.show_ci == true",
                         gt_output("ci_table_side")     
                       )
                     )
                   )
                 ),
                 
                 # TEST OFF and CI ON 
                 conditionalPanel(
                   condition = "input.show_test == false && input.show_ci == true",
                   div(style = "margin-top:25px; width:fit-content;",
                       gt_output("ci_table_bottom"))            
                 )
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
        numericInput("n", "Sample Size", 30),
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
    # TAB 6: t-Distribution
    # ======================================================================

    tabPanel(
      "t-Distribution",
      sidebarLayout(
        sidebarPanel(
          selectInput("t_mode", "Select Mode:", 
                      choices = c("Distribution Calculator" = "t", 
                                  "Inverse Calculator" = "inverse")),
          
          numericInput("df", "Degrees of Freedom", value = 10, step = 1, min = 1),
          
          radioButtons(
            inputId = "t_range",
            label = "Select Range:",
            choices = c("Above" = "above", "Below" = "below", 
                        "Between" = "between", "Outside" = "outside"),
            selected = "between"
          ),
          
          conditionalPanel(
            condition = "input.t_mode == 'inverse' && (input.t_range == 'between' || input.t_range == 'outside')",
            checkboxInput("t_symmetric", "Symmetric Thresholds", value = TRUE)
          ),
          
          uiOutput("t_dynamic_inputs"),
          
          conditionalPanel(
            condition = "input.t_mode == 'inverse' && (input.t_range == 'between' || input.t_range == 'outside')",
            numericInput("t_prob_input", "Desired Probability", value = 0.95, step = 0.01)
          ),
          
          conditionalPanel(
            condition = "input.t_mode == 'inverse' && input.t_range != 'between' && input.t_range != 'outside'",
            numericInput("t_prob_input", "Probability", value = 0.95, step = 0.01)
          ),
          
          conditionalPanel(
            condition = "input.t_mode == 'inverse' && input.t_range != 'between' && input.t_range != 'outside'",
            radioButtons("t_known_side", "Known Threshold Is:",
                         choices = c("Lower" = "lower", "Upper" = "upper"),
                         selected = "lower")
          )
        ),
        
        mainPanel(
          textOutput("t_prob"),
          textOutput("t_threshold_text"),
          plotOutput("t_plot")
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