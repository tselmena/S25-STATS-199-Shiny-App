# ui.R =========================================================================
# this is a test by Thomas
ui <- fluidPage(
  
  # title
  titlePanel("UCLA Statistics & Data Science"),
  
  tabsetPanel(
    # create a tab for a two-sided one proportion test
    tabPanel("One Proportion Test",
      
      # within this panel, create another panel containing three inputs
      sidebarPanel(
        
        # select test using buttons (not reactive, user has to click calculate again to update test)
        radioButtons(
          inputId = "alternative", 
          label = "Select the type of test:",
          choices = c("Two-sided" = "two.sided", 
                      "Left-sided"  = "less", 
                      "Right-sided" = "greater"),
          selected = "two.sided" # default selection
        ),
        
        # each input given default values
        numericInput("p",      "Hypothesized Proportion", 0.5),
        
        numericInput("n",      "Sample Size",              30),
        
        numericInput("p_hat",  "Observed Proportion",  0.6),
        
      ),
      
      # output
      mainPanel(
          
          plotOutput("plot"),
          gt_output("testTable")
        
        )
      )
    ),
    
    # add more tabs here, ex:
    tabPanel("Two Proportion Test")
    
  )

