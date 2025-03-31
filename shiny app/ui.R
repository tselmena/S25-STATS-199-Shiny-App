# ui.R =========================================================================

ui <- fluidPage(
  
  # title
  titlePanel("UCLA Statistics & Data Science"),
  
  tabsetPanel(
    # create a tab for a two-sided one proportion test
    tabPanel("One Proportion Test",
      
      # within this panel, create another panel containing three inputs
      sidebarPanel(
        
        # each input given default values
        numericInput("p",      "Hypothesized Proportion", 0.5),
        
        numericInput("n",      "Sample Size",              30),
        
        numericInput("p_hat",  "Observed Proportion",  0.6),
        
        # output hidden until the "Calculate" button is pushed
        actionButton("button_pushed", "Calculate")
      ),
      
      # output
      mainPanel(
        
        # only show the results if button_pushed was clicked at least once
        conditionalPanel(
          
          condition = "input.button_pushed > 0",  
          
          # print R output
          verbatimTextOutput("test_result")
        )
      )
    ),
    
    # add more tabs here, ex:
    tabPanel("Two Proportion Test")
    
  )
)
