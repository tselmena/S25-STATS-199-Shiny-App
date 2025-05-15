# ui.R =========================================================================

ui <- fluidPage(
  
  # theme
  theme = shinytheme("flatly"),
  
  mathjax = TRUE,
  # title
  titlePanel("UCLA Intro Stats Calculator"),
  
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
    
    tabPanel(
      "One Mean",
      sidebarLayout(
        sidebarPanel(
          checkboxInput("mean_show_ci",   "Confidence Interval", TRUE),
          checkboxInput("mean_show_test", "Test",                 TRUE),
          
          ## confidence level
          conditionalPanel(
            condition = "input.mean_show_ci == true",
            selectInput("mean_conf_level", "Confidence Level:",
                        choices  = c("90%" = 0.90, "95%" = 0.95, "99%" = 0.99),
                        selected = 0.95)
          ),
          
          ## alternative
          conditionalPanel(
            condition = "input.mean_show_test == true",
            radioButtons("mean_alt", "Select the type of test:",
                         choices = c("Two‑sided"  = "two.sided",
                                     "Left‑tailed" = "less",
                                     "Right‑tailed"= "greater"),
                         selected = "two.sided")
          ),
          
          ## numeric inputs
          numericInput("mean_mu0",  "Hypothesised mean (μ₀)",   value = 100),
          numericInput("mean_n",    "Sample size (n)",          value = 30, step = 1),
          numericInput("mean_xbar", "Sample mean (x̄)",         value = 98),
          numericInput("mean_s",    "Sample SD (s)",            value = 6)
        ),
        
        mainPanel(
          plotOutput("mean_plot"),
          
          ## TEST ON  → two‑column layout
          conditionalPanel(
            condition = "input.mean_show_test == true",
            splitLayout(
              cellWidths = c("30%", "55%"),
              cellArgs   = list(style="padding:0;margin:0;vertical-align:top;"),
              div(gt_output("mean_results_table")),
              div(style="margin-left:20px;display:flex;flex-direction:column;row-gap:0px;",
                  gt_output("mean_conclusions"),
                  conditionalPanel(
                    condition = "input.mean_show_ci == true",
                    gt_output("mean_ci_table_side")
                  )
              )
            )
          ),
          
          ## TEST OFF & CI ON  → CI below plot
          conditionalPanel(
            condition = "input.mean_show_test == false && input.mean_show_ci == true",
            div(style="margin-top:25px;width:fit-content;",
                gt_output("mean_ci_table_bottom"))
          )
        )
      )
    ),
    
    
    # ====================================================================
    # TAB 3: Difference Two Proportion
    # ====================================================================
    tabPanel(
      "Difference Two Proportion",
      sidebarLayout(
        sidebarPanel(
          ### same master switches ----------------------------------------
          checkboxInput("d2_show_ci",   "Confidence Interval", TRUE),
          checkboxInput("d2_show_test", "Test",                 TRUE),
          
          ### confidence level selector -----------------------------------
          conditionalPanel(
            condition = "input.d2_show_ci == true",
            selectInput("d2_conf_level", "Confidence Level:",
                        choices  = c("90%" = 0.90, "95%" = 0.95, "99%" = 0.99),
                        selected = 0.95)
          ),
          
          ### alternative hypotheses -------------------------------------
          conditionalPanel(
            condition = "input.d2_show_test == true",
            radioButtons("d2_alternative", "Select the type of test:",
                         choices = c("Two‑sided" = "two.sided",
                                     "Left-tailed" = "less",
                                     "Right-tailed" = "greater"),
                         selected = "two.sided")
          ),
          
          ### hypothesised difference Δ₀ (keep 0 for intro‑stats courses)
          numericInput("d2_delta0",
                       HTML("Hypothesised difference&nbsp;(Δ<sub>0</sub>)"), 0,
                       step = 0.01),
          
          ### group 1 inputs ---------------------------------------------
          tags$hr(),
          tags$strong("Group 1"),
          numericInput("d2_n1",   "Sample size (n₁)", 30,  step = 1),
          
          conditionalPanel(
            condition = "input.d2_use_successes1 == false",
            numericInput("d2_p1hat", HTML("Sample proportion (p&#770;<sub>1</sub>)"),
                         0.60, step = 0.01)
          ),
          
          conditionalPanel(
            condition = "input.d2_use_successes1 == true",
            numericInput("d2_x1", "Number of successes (x₁)", 18, step = 1)
          ),
          
          ### group 2 inputs ---------------------------------------------
          tags$hr(),
          tags$strong("Group 2"),
          numericInput("d2_n2",   "Sample size (n₂)", 40,  step = 1),
          conditionalPanel(
            condition = "input.d2_use_successes2 == false",
            numericInput("d2_p2hat", HTML("Sample proportion (p&#770;<sub>2</sub>)"),
                         0.55, step = 0.01)
          ),
          conditionalPanel(
            condition = "input.d2_use_successes2 == true",
            numericInput("d2_x2", "Number of successes (x₂)", 22, step = 1)
          ),
          
          ### common toggle ----------------------------------------------
          checkboxInput("d2_use_successes1",
                        "Use first number of successes instead", FALSE),
          
          checkboxInput("d2_use_successes2",
                        "Use second number of successes instead", FALSE)
        ),
        
        ### right‑hand side: plot + tables (same layout) -----------------
        mainPanel(
          plotOutput("d2_plot"),
          
          ## two‑column only when the test box is on
          conditionalPanel(
            condition = "input.d2_show_test == true",
            splitLayout(
              cellWidths = c("30%", "55%"),
              cellArgs   = list(style="padding:0;margin:0;vertical-align:top;"),
              div(gt_output("d2_results_table")),
              div(style = "margin-left:20px;display:flex;flex-direction:column;row-gap:0px;",
                  gt_output("d2_conclusions"),
                  conditionalPanel(
                    condition = "input.d2_show_ci == true",
                    gt_output("d2_ci_table_side")
                  )
              )
            )
          ),
          
          ## if CI is on but test is off, put CI below the plot
          conditionalPanel(
            condition = "input.d2_show_test == false && input.d2_show_ci == true",
            div(style="margin-top:25px;width:fit-content;",
                gt_output("d2_ci_table_bottom"))
          )
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
