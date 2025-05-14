# server.R =====================================================================

server <- function(input, output, session) {
  
  # ======================================================================
  # TAB 1: One Proportion Test
  # ======================================================================
  
  calc_results <- reactive({
    req(input$n)
    if (isTRUE(input$use_successes)) {
      x  <- input$x_succ
      ph <- x / input$n
    } else {
      ph <- input$p_hat
      x  <- round(ph * input$n)
    }

    validate(
      need(input$n > 0, "Sample size n must be > 0"),
      need(x >= 0 && x <= input$n, "x must be between 0 and n"),
      need(input$p >= 0 && input$p <= 1,
           "Hypothesised proportion p0 must be in [0,1]"),
      need(ph >= 0 && ph <= 1,
           "Sample proportion p_hat must be in [0,1]")
    )
    
    prop.test(x, input$n, p = input$p, alternative = input$alternative,
              conf.level = as.numeric(input$conf_level))
  })
  
  # ----------------------------------------------------------------------
  # Confidence interval check box
  # ----------------------------------------------------------------------
  observe({
    if (input$show_ci) {
      test_results <- calc_results()
      if (is.null(test_results)) {
        output$ci_table <- renderText({ "" })
        output$ci_conclusion <- renderText({ "Invalid input parameters." })
        return()
      }
    } else {
      # hide or clear these outputs if the checkbox is not selected
      output$ci_label <- renderText({ "" })
      output$ci_conclusion <- renderText({ "" })
    }
  })
  
  # ----------------------------------------------------------------------
  # Plot
  # ----------------------------------------------------------------------
  output$plot <- renderPlot({
    if (!input$show_test && !input$show_ci) {
      plot.new(); text(0.5, 0.5, "No test or CI selected", cex = 1.4)
      return()
    }
    
    x_obs <- if (isTRUE(input$use_successes)) {
      input$x_succ
    } else {
      round(input$p_hat * input$n)
    }
    
    if (x_obs < 0 || x_obs > input$n) {
      plot.new(); text(0.5, 0.5, "Invalid input parameters", cex = 1.5)
      return()
    }
  
    x_vals <- 0:input$n
    probs <- dbinom(x_vals, size = input$n, prob = input$p)
    bar_col <- rep("lightgrey", length(x_vals))
    
    # rejection region
    if (input$alternative == "less") {
      bar_col[x_vals <= x_obs] <- "#FFD100"
    } else if (input$alternative == "greater") {
      bar_col[x_vals >= x_obs] <- "#FFD100"
    } else {                                     
      mu <- input$n * input$p
      d <- abs(x_obs - mu)
      idx  <- which(x_vals <= floor(mu - d) |
                      x_vals >= ceiling(mu + d))
      bar_col[idx] <- "#FFD100"
    }
    
    barplot(height = probs, names.arg = x_vals, col = bar_col, border = "black",
            space = 0.2, xlab = "Number of Successes (x)", ylab = "Probability",
            main = "Sampling Distribution Under Null Hypothesis")
  })
  
  
  # ----------------------------------------------------------------------
  # Show or hide the test results and conclusions
  # ----------------------------------------------------------------------
  observe({
    if (input$show_test) {
      # results table
      output$results_table <- render_gt({
        test_res <- calc_results()
        if (is.null(test_res)) {
          return(gt(data.frame(Warning = "Invalid input parameters.")))
        }
        p_value <- formatC(test_res$p.value, format = "f", digits = 4)
        
        sample_p <- as.numeric(test_res$estimate)       
        successes <- round(sample_p * input$n)           
    
        observeEvent(input$p_hat, {
          if (!isTRUE(input$use_successes)) {
            updateNumericInput(session, "x_succ",
                               value = round(input$p_hat * input$n))
          }
        })
        
        observeEvent(input$x_succ, {
          if (isTRUE(input$use_successes)) {
            updateNumericInput(session, "p_hat",
                               value = input$x_succ / input$n)
          }
        })
        
        alt_str <- switch(
          input$alternative,
          "less" = paste0("p < ", input$p),
          "greater" = paste0("p > ", input$p),
          "two.sided" = paste0("p ≠ ", input$p)
        )

        df <- data.frame(
          label = c(
            "$H_0$",
            "$H_A$",
            "$n$",
            "$\\hat p$",
            "$x$",
            "$p‑value$"
          ),
          value = c(
            paste0("p = ", input$p),
            alt_str,
            input$n,
            round(test_res$estimate, 3),
            round(input$p_hat * input$n),
            format(p_value, digits = 4)
          )
        )
        
        df |>
          gt() |>                 
          fmt_markdown(columns = label) |>      
          tab_header(title = "One Proportion Test") |>
          tab_options(column_labels.hidden = TRUE, 
                      table.width = pct(100)) 
      })
      
      # conclusions (separate table below)
      output$conclusions <- render_gt({
        test_res <- calc_results()
        if (is.null(test_res)) {
          return(gt(data.frame(Warning = "Invalid input parameters.")))
        }
        p_value <- formatC(test_res$p.value, format = "f", digits = 4)
        alpha <- c(0.01, 0.05, 0.1)
        decisions <- ifelse(p_value < alpha, "**rejected**", "**not rejected**")
        df <- data.frame(
          conclusions = paste0("The null hypothesis is ", decisions, " at \u03B1 = ", alpha)
        )
        
        df |> 
          gt() |> 
          tab_header(title = "Test Conclusions") |> 
          tab_options(column_labels.hidden = TRUE, 
                      table.width = pct(100)
                      ) |> 
          fmt_markdown(columns = everything())
      })
      } else {
      # clear if show_test is off
        output$results_table <- renderText({ "" })
        output$conclusions   <- renderText({ "" })
      }
    
    ci_gt <- reactive({
      req(input$show_ci)
      test_res <- calc_results()
      validate(need(!is.null(test_res), "Invalid input parameters."))
      
      ci <- test_res$conf.int
      data.frame(
        label = c("Confidence level", "Interval",
                  "Hypothesized Proportion in Range"),
        value = c(
          paste0(round(as.numeric(input$conf_level)*100), "%"),
          sprintf("[%.3f, %.3f]", ci[1], ci[2]),
          ifelse(input$p >= ci[1] && input$p <= ci[2], "Yes", "No")
        )
      ) |>
        gt() |>
        fmt_markdown(columns = label) |>
        tab_header(title = "Confidence Interval") |>
        tab_options(column_labels.hidden = TRUE)
    })
    output$ci_table_side <- render_gt(
      ci_gt() |> 
        tab_options(column_labels.hidden = TRUE,
                    table.width = pct(100)))
    output$ci_table_bottom <- render_gt(
      ci_gt() |> 
        tab_options(column_labels.hidden = TRUE,
                    table.width = pct(100))
    )
    
  })
 
  # ======================================================================
  # TAB 2: One Mean
  # ======================================================================
  
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
    #bar_color <- "#2774AE"
    shade_color <- "#FFD100"
    
    plot(x_vals, y_vals, type = "l", lwd = 2, col = "lightgrey",
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
  
  # ======================================================================
  # TAB 3: Difference Two Proportion
  # ======================================================================
  
  
  
  
  # ======================================================================
  # TAB 4: Difference Two Means
  # ======================================================================
  
  
  
  
  # ======================================================================
  # TAB 5: Normal Distribution
  # ======================================================================

  debounced_num1 <- debounce(reactive(input$num1), 300)
  debounced_num2 <- debounce(reactive(input$num2), 300)
  
  observe({
    if (input$mode == "inverse" && input$range %in% c("between", "outside")) {
      updateCheckboxInput(session, "symmetric", value = TRUE)
    }
  })
  
  thresholds <- reactiveValues(num1 = -1.96, num2 = 1.96)
  threshold_locked_by_user <- reactiveVal(FALSE)
  updating_thresholds <- reactiveVal(FALSE)
  
  observeEvent(debounced_num1(), {
    if (updating_thresholds()) return()  # prevent feedback loop
    
    isolate({
      if (!is.null(debounced_num1()) && !is.na(debounced_num1())) {
        if (input$mode == "inverse" && input$range %in% c("between", "outside") && !input$symmetric) {
          if (input$known_side == "upper") {
            thresholds$num2 <- debounced_num1()  # Treat as upper
          } else {
            thresholds$num1 <- debounced_num1()  # Treat as lower
          }
        } else {
          thresholds$num1 <- debounced_num1()
        }
        threshold_locked_by_user(TRUE)
      }
    })
  })
  
  observeEvent(debounced_num2(), {
    if (updating_thresholds()) return()  # prevent feedback loop
    
    isolate({
      if (!is.null(debounced_num2()) && !is.na(debounced_num2())) {
        thresholds$num2 <- debounced_num2()
      }
    })
  }, ignoreInit = TRUE)
  
  output$dynamic_inputs <- renderUI({
    if (input$mode == "normal") {
      if (input$range %in% c("between", "outside")) {
        tagList(
          numericInput("num1", "Lower Threshold", value = round(thresholds$num1, 4), step = 0.01),
          numericInput("num2", "Upper Threshold", value = round(thresholds$num2, 4), step = 0.01)
        )
      } else {
        numericInput("num1", "Threshold", value = round(thresholds$num1, 4), step = 0.01)
      }
    } else {
      if (input$range %in% c("above", "below")) return(NULL)
      if (input$symmetric) return(NULL)
      
      tagList(
        radioButtons("known_side", "Known Threshold Is:",
                     choices = c("Lower" = "lower", "Upper" = "upper"),
                     selected = "lower"),
        uiOutput("single_input_ui")
      )
    }
  })
  
  output$single_input_ui <- renderUI({
    numericInput("num1",
                 label = if (input$known_side == "lower") "Lower Threshold" else "Upper Threshold",
                 value = if (input$known_side == "lower") round(thresholds$num1, 4) else round(thresholds$num2, 4),
                 step = 0.01)
  })
  
  calculate_asymmetric_thresholds <- function(known_side, known_value, prob_input, mean, sd, range_type) {
    if (range_type == "between") {
      if (known_side == "lower") {
        p_lower <- pnorm(known_value, mean, sd)
        target_upper <- prob_input + p_lower
        if (target_upper <= 0 || target_upper >= 1) return(list(error = "Invalid probability or threshold input."))
        computed <- qnorm(target_upper, mean, sd)
        return(list(num1 = known_value, num2 = computed))
      } else {
        p_upper <- 1 - pnorm(known_value, mean, sd)
        target_lower <- prob_input + p_upper
        if (target_lower <= 0 || target_lower >= 1) return(list(error = "Invalid probability and threshold combination."))
        computed <- qnorm(1 - target_lower, mean, sd)
        return(list(num1 = computed, num2 = known_value))
      }
    } else if (range_type == "outside") {
      if (known_side == "lower") {
        p_lower <- pnorm(known_value, mean, sd)
        target_upper <- prob_input - p_lower
        if (target_upper <= 0 || target_upper >= 1) return(list(error = "Invalid combination."))
        upper <- tryCatch({
          uniroot(function(x) pnorm(x, mean, sd, lower.tail = FALSE) - target_upper,
                  lower = mean, upper = mean + 10 * sd)$root
        }, error = function(e) NA)
        if (is.na(upper)) return(list(error = "Could not solve for upper threshold."))
        return(list(num1 = known_value, num2 = upper))
      } else {
        p_upper <- 1 - pnorm(known_value, mean, sd)
        target_lower <- prob_input - p_upper
        if (target_lower <= 0 || target_lower >= 1) return(list(error = "Invalid combination."))
        lower <- tryCatch({
          uniroot(function(x) pnorm(x, mean, sd) - target_lower,
                  lower = mean - 10 * sd, upper = mean)$root
        }, error = function(e) NA)
        if (is.na(lower)) return(list(error = "Could not solve for lower threshold."))
        return(list(num1 = lower, num2 = known_value))
      }
    }
    return(list(error = "Invalid range or threshold."))
  }
  
  norm_result <- reactive({
    req(input$mean, input$sd, input$range, input$mode)
    
    mean <- input$mean
    sd <- input$sd
    range_type <- input$range
    mode <- input$mode
    prob_input <- if (!is.null(input$prob_input)) as.numeric(input$prob_input) else NA
    
    num1 <- if (!is.null(debounced_num1())) as.numeric(debounced_num1()) else NA
    num2 <- if (!is.null(debounced_num2())) as.numeric(debounced_num2()) else NA
    
    if (mode == "inverse") {
      if (range_type == "below") {
        num1 <- qnorm(prob_input, mean, sd)
      } else if (range_type == "above") {
        num1 <- qnorm(1 - prob_input, mean, sd)
      } else if (input$symmetric && range_type %in% c("between", "outside")) {
        tail_prob <- if (range_type == "between") (1 - prob_input) / 2 else prob_input / 2
        dist <- qnorm(1 - tail_prob, mean, sd) - mean
        if (range_type == "between") {
          num1 <- mean - dist
          num2 <- mean + dist
        } else {
          num1 <- qnorm(tail_prob, mean, sd)
          num2 <- qnorm(1 - tail_prob, mean, sd)
        }
      } else {
        if (!is.na(num1)) {
          result <- calculate_asymmetric_thresholds(input$known_side, num1, prob_input, mean, sd, range_type)
          if (!is.null(result$error)) return(list(error = result$error))
          num1 <- result$num1
          num2 <- result$num2
          updating_thresholds(TRUE)
          thresholds$num1 <- num1
          thresholds$num2 <- num2
          updating_thresholds(FALSE)
        }
      }
    }
    
    x_vals <- seq(mean - 4 * sd, mean + 4 * sd, length.out = 1000)
    y_vals <- dnorm(x_vals, mean, sd)
    df <- data.frame(x = x_vals, y = y_vals)
    
    prob <- NA
    error_msg <- NULL
    shade_df <- data.frame(x = numeric(0), y = numeric(0))
    
    if (range_type == "above") {
      prob <- pnorm(num1, mean, sd, lower.tail = FALSE)
      shade_df <- df[df$x >= num1, ]
    } else if (range_type == "below") {
      prob <- pnorm(num1, mean, sd)
      shade_df <- df[df$x <= num1, ]
    } else if (range_type == "between") {
      if (num2 < num1) error_msg <- "Lower threshold must be less than upper threshold."
      else {
        prob <- pnorm(num2, mean, sd) - pnorm(num1, mean, sd)
        shade_df <- df[df$x >= num1 & df$x <= num2, ]
      }
    } else if (range_type == "outside") {
      if (num2 < num1) error_msg <- "Lower threshold must be less than upper threshold."
      else {
        prob <- 1 - (pnorm(num2, mean, sd) - pnorm(num1, mean, sd))
      }
    }
    
    if (!is.null(error_msg)) return(list(error = error_msg))
    
    list(prob = prob, data = df, shaded = shade_df, num1 = num1, num2 = num2)
  })
  
  output$norm_prob <- renderText({
    res <- norm_result()
    if (!is.null(res$error)) return(res$error)
    paste0("Probability = ", round(res$prob, 4))
  })
  
  output$threshold_text <- renderText({
    res <- norm_result()
    if (!is.null(res$error)) return("")
    if (input$range == "above") {
      paste0("Threshold: Above ", round(res$num1, 4))
    } else if (input$range == "below") {
      paste0("Threshold: Below ", round(res$num1, 4))
    } else if (input$range == "between") {
      paste0("Threshold: Between ", round(res$num1, 4), " and ", round(res$num2, 4))
    } else if (input$range == "outside") {
      paste0("Threshold: Outside ", round(res$num1, 4), " and ", round(res$num2, 4))
    }
  })
  
  output$norm_plot <- renderPlot({
    res <- norm_result()
    if (!is.null(res$error)) return(NULL)
    
    base_plot <- ggplot(res$data, aes(x, y)) +
      geom_line(color = "blue") +
      labs(title = "Normal Distribution", x = "X", y = "Density") +
      theme_minimal()
    
    if (input$range == "outside") {
      left <- subset(res$data, x <= res$num1)
      right <- subset(res$data, x >= res$num2)
      base_plot +
        geom_area(data = left, aes(x, y), fill = "lightblue", alpha = 0.5) +
        geom_area(data = right, aes(x, y), fill = "lightblue", alpha = 0.5)
    } else {
      base_plot +
        geom_area(data = res$shaded, aes(x, y), fill = "lightblue", alpha = 0.5)
    }
  })
  
  
  
  

  

  
  # ======================================================================
  # TAB 6: t-Distribution
  # ======================================================================



  # ======================================================================
  # TAB 7: Chi-square
  # ======================================================================

}
















