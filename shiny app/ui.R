# ui.R =========================================================================

twomeans_str <- HTML(paste0(
  "<p class='intro-text-styling'>", 
  "This computes a t-test for the difference between two means for ",
  "independent (i.e., unpaired) samples. To compute a t-test for ",
  "two paired samples, use the One Mean tab and plug in ",
  "\u03BC", "<sub>diff</sub>",
  " for ",
  "\u03BC", "<sub>0</sub>",
  ", ",
  "x\u0304", "<sub>diff</sub>",
  " for ",
  "x\u0304",
  ", and ",
  "s", "<sub>diff</sub>",
  " for ",
  "s.",
  "</p>"
))

ui <- fluidPage(

  tags$head(
    tags$link(rel = "icon",
              href = "https://statistics.ucla.edu/wp-content/uploads/2023/08/cropped-cropped-ucla-logo-square-32x32.jpeg"
    ),
    tags$style(HTML("
      .intro-text-styling {
        font-size: 0.9em;  /* Slightly smaller than normal text */
        color: #808080;    /* A nice shade of grey */
        margin-bottom: 15px; /* Space below the text, before the plot */
        font-style: italic; 
      }
    "))
  ),
  
  # theme
  theme = shinytheme("flatly"),
  
  mathjax = TRUE, 
  
  # titlePanel 
  titlePanel(
    windowTitle = "UCLA Stats Calculator",    # Sets the browser window/tab title
    title = "UCLA Stats Calculator"           # Sets the visible title on the page
  ),
  
  
  tabsetPanel(
    
    # ======================================================================
    # TAB 1: Normal Distribution
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
          
        ),
        
        mainPanel(
          div(style = "margin-top: 20px;", 
              plotOutput("norm_plot")
          ),
          textOutput("norm_prob"),
          textOutput("threshold_text")
        )
      )
    ),
    
    # ======================================================================
    # TAB 2: t-Distribution
    # ======================================================================
    
    tabPanel(
      "t-Distribution",
      sidebarLayout(
        sidebarPanel(
          checkboxInput("show_normal_overlay", "Standard Normal Overlay", value = FALSE),
          
          selectInput("t_mode", "Select Mode:", 
                      choices = c("Distribution Calculator" = "t", 
                                  "Inverse Calculator" = "inverse")),
          
          numericInput("df", "Degrees of Freedom (n - 1)", value = 29, step = 1, min = 1),
          
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
          
        ),
        
        mainPanel(
          div(style = "margin-top: 20px;", 
              plotOutput("t_plot")
          ),
          textOutput("t_prob"),
          textOutput("t_threshold_text")
        )
      )
    ),
    
    # ======================================================================
    # TAB 3: Chi-square Distribution
    # ======================================================================
    
    tabPanel(
      "Chi-square Distribution",
      sidebarLayout(
        sidebarPanel(
          selectInput("chisq_mode", "Select Mode:", 
                      choices = c("Distribution Calculator" = "chisq", 
                                  "Inverse Calculator" = "inverse")),
          
          numericInput("chisq_df", "Degrees of Freedom", value = 1, min = 1, step = 1),
          
          # Show range selection in both modes
          radioButtons("chisq_range", "Select Range:",
                       choices = c("Above" = "above", "Below" = "below"),
                       selected = "below"),
          
          # Only show threshold input in distribution mode
          conditionalPanel(
            condition = "input.chisq_mode == 'chisq'",
            uiOutput("chisq_dynamic_inputs")
          ),
          
          # Only show desired probability input in inverse mode
          conditionalPanel(
            condition = "input.chisq_mode == 'inverse'",
            numericInput("chisq_prob_input", "Desired Probability", value = 0.95, step = 0.01)
          )
        ),
        mainPanel(
          div(style = "margin-top: 20px;", 
              plotOutput("chisq_plot")
          ),
          textOutput("chisq_prob"),
          textOutput("chisq_threshold_text")
          
        )
      )
    ),
    
    # ======================================================================
    # TAB 4: One Proportion 
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
                 
                 div(style = "margin-top: 20px;", 
                     plotOutput("plot")
                 ),
                 
                 # TEST ON - Stacked tables
                 conditionalPanel(
                   condition = "input.show_test == true",
                   div(style = "margin-top: 20px; width: fit-content;", # Outer div for width control
                       div(gt_output("results_table")), # Main results table
                       div(style = "margin-top: 15px;", # Spacing for conclusions table
                           gt_output("conclusions")
                       ),
                       conditionalPanel(
                         condition = "input.show_ci == true",
                         div(style = "margin-top: 15px;", # Spacing for CI table
                             gt_output("ci_table_side")     
                         )
                       )
                   )
                 ),
                 
                 # TEST OFF and CI ON (already stacked, styling adjusted)
                 conditionalPanel(
                   condition = "input.show_test == false && input.show_ci == true",
                   div(style = "margin-top: 20px; width:fit-content;",
                       gt_output("ci_table_bottom"))            
                 )
               )
             )
    ),
    
    # ======================================================================
    # TAB 5: One Mean
    # ======================================================================
    
    tabPanel(
      "One Mean",
      sidebarLayout(
        sidebarPanel(
          checkboxInput("mean_show_ci", "Confidence Interval", TRUE),
          checkboxInput("mean_show_test", "Test", TRUE),
          # confidence level
          conditionalPanel(
            condition = "input.mean_show_ci == true",
            selectInput("mean_conf_level", "Confidence Level:",
                        choices  = c("90%" = 0.90, "95%" = 0.95, "99%" = 0.99),
                        selected = 0.95)
          ),
          
          # alternative
          conditionalPanel(
            condition = "input.mean_show_test == true",
            radioButtons("mean_alt", "Select the type of test:",
                         choices = c("Two‑sided"  = "two.sided",
                                     "Left‑tailed" = "less",
                                     "Right‑tailed"= "greater"),
                         selected = "two.sided")
          ),
          
          ## numeric inputs
          numericInput("mean_mu0", "Hypothesised mean (μ₀)", value = 0),
          numericInput("mean_n", "Sample size (n)", value = 30, step = 1),
          numericInput("mean_xbar", "Sample mean(x̄)", value = .10),
          numericInput("mean_s", "Sample SD (s)", value = 1)
        ),
        
        mainPanel(
          div(style = "margin-top: 20px;", 
              plotOutput("mean_plot")
          ),
          
          # TEST ON - Stacked tables
          conditionalPanel(
            condition = "input.mean_show_test == true",
            div(style = "margin-top: 20px; width: fit-content;", # Outer div for width control
                div(gt_output("mean_results_table")), # Main results table
                div(style = "margin-top: 15px;", # Spacing for conclusions table
                    gt_output("mean_conclusions")
                ),
                conditionalPanel(
                  condition = "input.mean_show_ci == true",
                  div(style = "margin-top: 15px;", # Spacing for CI table
                      gt_output("mean_ci_table_side")
                  )
                )
            )
          ),
          
          # TEST OFF & CI ON (already stacked, styling adjusted)
          conditionalPanel(
            condition = "input.mean_show_test == false && input.mean_show_ci == true",
            div(style="margin-top: 20px; width:fit-content;",
                gt_output("mean_ci_table_bottom"))
          )
        )
      )
    ),
    
    
    # ====================================================================
    # TAB 6: Difference Two Proportions 
    # ====================================================================
    
    tabPanel(
      "Difference Two Proportions", # Title as provided in the snippet
      sidebarLayout(
        sidebarPanel(
          checkboxInput("d2_show_ci", "Confidence Interval", TRUE),
          checkboxInput("d2_show_test", "Test", TRUE),
          
          conditionalPanel(
            condition = "input.d2_show_ci == true",
            selectInput("d2_conf_level", "Confidence Level:",
                        choices  = c("90%" = 0.90, "95%" = 0.95, "99%" = 0.99),
                        selected = 0.95)
          ),
          
          conditionalPanel(
            condition = "input.d2_show_test == true",
            radioButtons("d2_alternative", "Select the type of test:",
                         choices = c("Two‑sided" = "two.sided",
                                     "Left-tailed" = "less",
                                     "Right-tailed" = "greater"),
                         selected = "two.sided")
          ),
          
          tags$hr(),
          tags$strong("Group 1"),
          numericInput("d2_n1", "Sample size (n₁)", 30,  step = 1),
          
          conditionalPanel(
            condition = "input.d2_use_successes1 == false",
            numericInput("d2_p1hat", HTML("Sample proportion (p&#770;<sub>1</sub>)"),
                         0.60, step = 0.01)
          ),
          
          conditionalPanel(
            condition = "input.d2_use_successes1 == true",
            numericInput("d2_x1", "Number of successes (x₁)", 18, step = 1)
          ),
          
          checkboxInput("d2_use_successes1",
                        "Use first number of successes instead", FALSE),
          
          tags$hr(),
          tags$strong("Group 2"),
          numericInput("d2_n2", "Sample size (n₂)", 40,  step = 1),
          conditionalPanel(
            condition = "input.d2_use_successes2 == false",
            numericInput("d2_p2hat", HTML("Sample proportion (p&#770;<sub>2</sub>)"),
                         0.55, step = 0.01)
          ),
          conditionalPanel(
            condition = "input.d2_use_successes2 == true",
            numericInput("d2_x2", "Number of successes (x₂)", 22, step = 1)
          ),
          checkboxInput("d2_use_successes2",
                        "Use second number of successes instead", FALSE)
        ),
        
        # main: plot and tables
        mainPanel(
          div(style = "margin-top: 20px;", 
              plotOutput("d2_zplot")
          ),
          
          # TEST ON - Stacked tables
          conditionalPanel(
            condition = "input.d2_show_test == true",
            div(style = "margin-top: 20px; width: fit-content;", # Outer div for width control
                div(gt_output("d2_results_table")), # Main results table
                div(style = "margin-top: 15px;", # Spacing for conclusions table
                    gt_output("d2_conclusions")
                ),
                conditionalPanel(
                  condition = "input.d2_show_ci == true",
                  div(style = "margin-top: 15px;", # Spacing for CI table
                      gt_output("d2_ci_table_side")
                  )
                )
            )
          ),
          
          # TEST OFF & CI ON (already stacked, styling adjusted)
          conditionalPanel(
            condition = "input.d2_show_test == false && input.d2_show_ci == true",
            div(style="margin-top: 20px; width:fit-content;",
                gt_output("d2_ci_table_bottom"))
          )
        )
      )
    ),
    
    
    # ======================================================================
    # TAB 7: Difference Two Means
    # ======================================================================
    
    tabPanel(
      "Difference Two Means",
      sidebarLayout(
        sidebarPanel(
          # Checkboxes for CI and Test
          checkboxInput("d2m_show_ci", "Confidence Interval", TRUE),
          checkboxInput("d2m_show_test", "Test", TRUE),
          checkboxInput("d2m_var_equal", "Assume equal variances", value = FALSE), # Default is unchecked
          
          
          # Confidence Level
          conditionalPanel(
            condition = "input.d2m_show_ci == true",
            selectInput("d2m_conf_level", "Confidence Level:",
                        choices  = c("90%" = 0.90, "95%" = 0.95, "99%" = 0.99),
                        selected = 0.95)
          ),
          
          # Type of Test
          conditionalPanel(
            condition = "input.d2m_show_test == true",
            radioButtons("d2m_alternative", "Select the type of test:",
                         choices = c("Two-sided" = "two.sided",
                                     "Left-tailed" = "less",
                                     "Right-tailed" = "greater"),
                         selected = "two.sided")
          ),
          
          tags$hr(),
          tags$strong("Group 1"),
          numericInput("d2m_n1", HTML("Sample size (n<sub>1</sub>)"), value = 30, min = 2, step = 1),
          numericInput("d2m_xbar1", HTML("Sample mean (x&#772;<sub>1</sub>)"), value = 123.8),
          numericInput("d2m_s1", HTML("Sample SD (s<sub>1</sub>)"), value = 4.6, min = 0),
          
          tags$hr(),
          tags$strong("Group 2"),
          numericInput("d2m_n2", HTML("Sample size (n<sub>2</sub>)"), value = 30, min = 2, step = 1),
          numericInput("d2m_xbar2", HTML("Sample mean (x&#772;<sub>2</sub>)"), value = 116.4),
          numericInput("d2m_s2", HTML("Sample SD (s<sub>2</sub>)"), value = 16.09, min = 0), 
          twomeans_str
        ), 
        mainPanel(
        
          div(style = "margin-top: 20px;", 
              plotOutput("d2m_plot")
          ),
          
          conditionalPanel(
            condition = "input.d2m_show_test == true",
            div(style = "margin-top: 20px; width: fit-content;", # Outer div for width control
                div(gt_output("d2m_results_table")), # Main results table
                div(style = "margin-top: 15px;", # Spacing for conclusions table
                    gt_output("d2m_conclusions")
                ),
                # Conditional Confidence Interval Table
                conditionalPanel(
                  condition = "input.d2m_show_ci == true",
                  div(style = "margin-top: 15px;", # Spacing for CI table
                      gt_output("d2m_ci_table_side") 
                  )
                )
            )
          ),
          
          conditionalPanel(
            condition = "input.d2m_show_test == false && input.d2m_show_ci == true",
            div(style = "margin-top: 20px; width: fit-content;", 
                gt_output("d2m_ci_table_bottom")
            )
          )
        )
      )
    ),
    
    # ======================================================================
    # TAB 8: Citation
    # ======================================================================
    
    tabPanel(
      "Citation",
      mainPanel(
        htmlOutput("copyright"),
        
        htmlOutput("APA"),
        
        htmlOutput("bibtex")
        
        )
      )
  )
)