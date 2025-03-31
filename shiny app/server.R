# server.R

server <- function(input, output, session) {
  
  # eventReactive: will only run when you click the button_pushed
  calc_results <- eventReactive(input$button_pushed, {
    
    # get input from ui.R
    p0  <- input$p
    n   <- input$n
    ph  <- input$p_hat
    
    # calculate number of successes based on input
    x   <- round(ph * n)  
  
    test_res <- prop.test(x, n, p = p0, alternative = "two.sided")
    test_res
  })
  
  # render the test output
  output$test_result <- renderPrint({
    
    # calc_results() will be NULL until button is pressed
    results <- calc_results()
    results
  })
  
}

