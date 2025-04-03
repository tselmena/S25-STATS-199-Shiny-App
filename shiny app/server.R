# server.R

server <- function(input, output, session) {
  
  calc_results <- reactive({
    
    # get inputs from ui.R
    p0  <- input$p
    n   <- input$n
    ph  <- input$p_hat
    x   <- round(ph * n)  # number of successes in the sample
    
    # validate inputs
    if (n <= 0 || x < 0 || x > n) {
      # handle edge cases where n or x is invalid
      return(NULL)
    }
    
    # run one-prop test
    prop.test(x, n, p = p0, alternative = input$alternative)
  })
  
  output$test_table <- render_gt({
    
    # get test results from calculations
    test_res <- calc_results()  
    
    alternative_str <- switch(
      input$alternative, 
      "less" = paste0("p < ", input$p),   # left-sided
      "greater" = paste0("p > ", input$p), # right-sided
      "two.sided" = paste0("p ≠ ", input$p) # two-sided
    )
    
    df_results <- data.frame(
      Label = c("Null Hypothesis", 
                "Alternative Hypothesis", 
                "Sample Size", 
                "Number of Successes", 
                "Sample Proportion", 
                "p-value"),
      
      Value = c(paste0("p = ", input$p), 
                alternative_str, 
                input$n, 
                round(input$p_hat * input$n), 
                test_res$estimate, 
                format(test_res$p.value, digits = 4))
    )
    
    gt(df_results) |>
      tab_header(title = "One Proportion Test Results") 
  })
  
  output$plot <- renderPlot({
    
    # get test results
    test_res <- calc_results()
    
    if (is.null(test_res)) {
      plot.new()
      text(0.5, 0.5, "Invalid input parameters", cex = 1.5)
      return()
    }
    
    x_vals <- 0:input$n
    x_obs <- round(input$p_hat * input$n)
    binom_probs <- dbinom(x_vals, size = input$n, prob = input$p) 
    
    # fun fact: #2774AE is the official UCLA Blue color 
    bar_colors <- rep("#2774AE", length(x_vals))
    
    if (input$alternative == "less") {
      # highlight all x <= x_obs
      # another fun fact: #FFD100 is the official UCLA Gold color
      bar_colors[which(x_vals <= x_obs)] <- "#FFD100"
      
    } else if (input$alternative == "greater") {
      # highlight all x >= x_obs
      bar_colors[which(x_vals >= x_obs)] <- "#FFD100"
      
    } else { # two.sided
      mu <- input$n * input$p
      dist_from_mean <- abs(x_obs - mu)
      x_lower <- floor(mu - dist_from_mean)
      x_upper <- ceiling(mu + dist_from_mean)
      index <- which(x_vals <= x_lower | x_vals >= x_upper)
      bar_colors[index] <- "#FFD100"
    }
    
    # create barplot
    barplot(
      height = binom_probs, 
      names.arg = x_vals, 
      col = bar_colors, 
      xlab = "Number of Successes (x)",
      ylab = "Probability",
      main = "Sampling Distribution Under Null Hypothesis"
    )
  })
}














