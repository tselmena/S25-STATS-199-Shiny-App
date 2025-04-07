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
      main = "Sampling Distribution Under Null Hypothesis",
      ylim = c(0, max(binom_probs) * 1.1) # adds space to top of tallest bar
    )
  })
  
  one_mean_test_results <- reactive({
    
    # get inputs from ui.R
    mu <- input$mu
    n <- input$n
    x_bar <- input$x_bar
    s <- input$s
    df <- n - 1
    
    # validate inputs
    if (n <= 0 || s <= 0) {
      return(NULL)
    }
    
    # t-statistic
    t_stat <- (x_bar - mu) / (s / sqrt(n))
    p_val <- switch(
      input$mean_alt,
      "less" = pt(t_stat, df = df),
      "greater" = 1 - pt(t_stat, df = df),
      "two.sided" = 2 * pt(-abs(t_stat), df = df)
    )
    
    # construct a fake "t.test" object
    list(
      statistic = c(t = t_stat),
      p.value = p_val,
      estimate = c(mean = x_bar),
      null.value = c(mu = mu),
      alternative = input$mean_alt,
      method = "One Sample t-test",
      data.name = "Manually computed",
      parameter = c(df = df)
    )
  })
  
  output$one_mean_test_table <- render_gt({
    
    # get test results
    test_res <- one_mean_test_results()  
    
    alternative_str <- switch(
      input$mean_alt, 
      "less" = paste0("μ < ", input$mu),   # left-sided
      "greater" = paste0("μ > ", input$mu), # right-sided
      "two.sided" = paste0("μ ≠ ", input$mu) # two-sided
    )
    
    df_results <- data.frame(
      Label = c("Null Hypothesis",
                "Alternative Hypothesis",
                "Sample Size",
                "Sample Mean",
                "Sample SD",
                "Degrees of Freedom",
                "p-value"),
      
      Value = c(paste0("μ = ", input$mu), 
                alternative_str, 
                input$n, 
                round(input$x_bar, 3),
                round(input$s, 3),
                round(test_res$parameter["df"], 2),
                format(test_res$p.value, digits = 4))
    )
    
    gt(df_results) |>
      tab_header(title = "One Sample t-test Results") 
  })
  
  output$one_mean_plot <- renderPlot({
    
    # get test results
    test_res <- one_mean_test_results()
    
    if (is.null(test_res)) {
      plot.new()
      text(0.5, 0.5, "Invalid input parameters", cex = 1.5)
      return()
    }
    
    n <- input$n
    df <- n - 1
    mu <- input$mu
    x_bar <- input$x_bar
    s <- input$s
    se <- s / sqrt(n)
    t_stat <- (x_bar - mu) / se
    
    x_vals <- seq(-4, 4, length.out = 400)
    y_vals <- dt(x_vals, df)
    
    # fun fact: #2774AE is the official UCLA Blue color 
    bar_color <- "#2774AE"
    shade_color <- "#FFD100"
    
    plot(x_vals, y_vals, type = "l", lwd = 2, col = bar_color,
         xlab = "t-value", ylab = "Density",
         main = "t-Distribution Under Null Hypothesis")
    
    # shade gold area based on alternative
    if (input$mean_alt == "less") {
      x_shade <- x_vals[x_vals <= t_stat]
      y_shade <- y_vals[x_vals <= t_stat]
      polygon(c(min(x_shade), x_shade, max(x_shade)),
              c(0, y_shade, 0),
              col = shade_color, border = NA)
      
    } else if (input$mean_alt == "greater") {
      x_shade <- x_vals[x_vals >= t_stat]
      y_shade <- y_vals[x_vals >= t_stat]
      polygon(c(min(x_shade), x_shade, max(x_shade)),
              c(0, y_shade, 0),
              col = shade_color, border = NA)
      
    } else if (input$mean_alt == "two.sided") {
      t_crit <- abs(t_stat)
      
      # left tail
      x_left <- x_vals[x_vals <= -t_crit]
      y_left <- y_vals[x_vals <= -t_crit]
      polygon(c(min(x_left), x_left, max(x_left)),
              c(0, y_left, 0),
              col = shade_color, border = NA)
      
      # right tail
      x_right <- x_vals[x_vals >= t_crit]
      y_right <- y_vals[x_vals >= t_crit]
      polygon(c(min(x_right), x_right, max(x_right)),
              c(0, y_right, 0),
              col = shade_color, border = NA)
    }
  })
}