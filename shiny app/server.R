# server.R =====================================================================

server <- function(input, output, session) {
  
  shinyalert("UCLA Stats Calculator", "This program comes with ABSOLUTELY NO WARRANTY; for details, see the 'Citation' tab. This is free software, and you are welcome to redistribute it under certain conditions; for details, see the 'Citation' tab.", type = "info")
  
  # ======================================================================
  # TAB 1: Normal Distribution
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
      if (is.na(num1) || is.na(num2)) return(NULL)
      if (num2 < num1) error_msg <- "Lower threshold must be less than upper threshold."
      else {
        prob <- pnorm(num2, mean, sd) - pnorm(num1, mean, sd)
        shade_df <- df[df$x >= num1 & df$x <= num2, ]
      }
    } else if (range_type == "outside") {
      if (is.na(num1) || is.na(num2)) return(NULL)
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
    if (is.null(res)) return("")
    if (!is.null(res$error)) return(res$error)
    paste0("Probability = ", round(res$prob, 4))
  })
  
  output$threshold_text <- renderText({
    res <- norm_result()
    if (is.null(res)) return("")
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
    if (is.null(res)) return("")
    if (!is.null(res$error)) return(NULL)
    
    base_plot <- ggplot(res$data, aes(x, y)) +
      geom_line(color = "#2774AE", size = 1.2) +
      labs(title = expression(bold("Normal Distribution")), x = "X", y = "Density") +
      theme_minimal()
    
    if (input$range == "outside") {
      left <- subset(res$data, x <= res$num1)
      right <- subset(res$data, x >= res$num2)
      base_plot +
        geom_area(data = left, aes(x, y), fill = "#2774AE", alpha = 0.5) +
        geom_area(data = right, aes(x, y), fill = "#2774AE", alpha = 0.5)
    } else {
      base_plot +
        geom_area(data = res$shaded, aes(x, y), fill = "#2774AE", alpha = 0.5)
    }
  })
  
  # ======================================================================
  # TAB 2: t-Distribution
  # ======================================================================
  
  t_debounced_num1 <- debounce(reactive(input$t_num1), 300)
  t_debounced_num2 <- debounce(reactive(input$t_num2), 300)
  
  observe({
    if (input$t_mode == "inverse" && input$t_range %in% c("between", "outside")) {
      updateCheckboxInput(session, "t_symmetric", value = TRUE)
    }
  })
  
  t_thresholds <- reactiveValues(num1 = -1.96, num2 = 1.96)
  t_threshold_locked_by_user <- reactiveVal(FALSE)
  t_updating_thresholds <- reactiveVal(FALSE)
  
  observeEvent(t_debounced_num1(), {
    if (t_updating_thresholds()) return()
    isolate({
      if (!is.null(t_debounced_num1()) && !is.na(t_debounced_num1())) {
        if (input$t_mode == "inverse" && input$t_range %in% c("between", "outside") && !input$t_symmetric) {
          if (input$t_known_side == "upper") {
            t_thresholds$num2 <- t_debounced_num1()
          } else {
            t_thresholds$num1 <- t_debounced_num1()
          }
        } else {
          t_thresholds$num1 <- t_debounced_num1()
        }
        t_threshold_locked_by_user(TRUE)
      }
    })
  })
  
  observeEvent(t_debounced_num2(), {
    if (t_updating_thresholds()) return()
    isolate({
      if (!is.null(t_debounced_num2()) && !is.na(t_debounced_num2())) {
        t_thresholds$num2 <- t_debounced_num2()
      }
    })
  }, ignoreInit = TRUE)
  
  output$t_dynamic_inputs <- renderUI({
    if (input$t_mode == "t") {
      if (input$t_range %in% c("between", "outside")) {
        tagList(
          numericInput("t_num1", "Lower Threshold", value = round(t_thresholds$num1, 4), step = 0.01),
          numericInput("t_num2", "Upper Threshold", value = round(t_thresholds$num2, 4), step = 0.01)
        )
      } else {
        numericInput("t_num1", "Threshold", value = round(t_thresholds$num1, 4), step = 0.01)
      }
    } else {
      if (input$t_range %in% c("above", "below") || input$t_symmetric) return(NULL)
      
      tagList(
        radioButtons("t_known_side", "Known Threshold Is:",
                     choices = c("Lower" = "lower", "Upper" = "upper"),
                     selected = "lower"),
        uiOutput("t_single_input_ui")
      )
    }
  })
  
  output$t_single_input_ui <- renderUI({
    numericInput("t_num1",
                 label = if (input$t_known_side == "lower") "Lower Threshold" else "Upper Threshold",
                 value = if (input$t_known_side == "lower") round(t_thresholds$num1, 4) else round(t_thresholds$num2, 4),
                 step = 0.01)
  })
  
  calculate_t_asymmetric_thresholds <- function(known_side, known_value, prob_input, df, range_type) {
    if (range_type == "between") {
      if (known_side == "lower") {
        p_lower <- pt(known_value, df)
        target_upper <- prob_input + p_lower
        if (target_upper <= 0 || target_upper >= 1) return(list(error = "Invalid probability or threshold input."))
        computed <- qt(target_upper, df)
        return(list(num1 = known_value, num2 = computed))
      } else {
        p_upper <- 1 - pt(known_value, df)
        target_lower <- prob_input + p_upper
        if (target_lower <= 0 || target_lower >= 1) return(list(error = "Invalid probability and threshold combination."))
        computed <- qt(1 - target_lower, df)
        return(list(num1 = computed, num2 = known_value))
      }
    } else if (range_type == "outside") {
      if (known_side == "lower") {
        p_lower <- pt(known_value, df)
        target_upper <- prob_input - p_lower
        if (target_upper <= 0 || target_upper >= 1) return(list(error = "Invalid combination."))
        upper <- tryCatch({
          uniroot(function(x) pt(x, df, lower.tail = FALSE) - target_upper,
                  lower = 0, upper = 10)$root
        }, error = function(e) NA)
        if (is.na(upper)) return(list(error = "Could not solve for upper threshold."))
        return(list(num1 = known_value, num2 = upper))
      } else {
        p_upper <- 1 - pt(known_value, df)
        target_lower <- prob_input - p_upper
        if (target_lower <= 0 || target_lower >= 1) return(list(error = "Invalid combination."))
        lower <- tryCatch({
          uniroot(function(x) pt(x, df) - target_lower,
                  lower = -10, upper = 0)$root
        }, error = function(e) NA)
        if (is.na(lower)) return(list(error = "Could not solve for lower threshold."))
        return(list(num1 = lower, num2 = known_value))
      }
    }
    return(list(error = "Invalid range or threshold."))
  }
  
  t_result <- reactive({
    req(input$df, input$t_range, input$t_mode)
    
    df <- input$df
    range_type <- input$t_range
    mode <- input$t_mode
    prob_input <- if (!is.null(input$t_prob_input)) as.numeric(input$t_prob_input) else NA
    
    num1 <- if (!is.null(t_debounced_num1())) as.numeric(t_debounced_num1()) else NA
    num2 <- if (!is.null(t_debounced_num2())) as.numeric(t_debounced_num2()) else NA
    
    if (mode == "inverse") {
      if (range_type == "below") {
        num1 <- qt(prob_input, df)
      } else if (range_type == "above") {
        num1 <- qt(1 - prob_input, df)
      } else if (input$t_symmetric && range_type %in% c("between", "outside")) {
        tail_prob <- if (range_type == "between") (1 - prob_input) / 2 else prob_input / 2
        dist <- qt(1 - tail_prob, df)
        if (range_type == "between") {
          num1 <- -dist
          num2 <- dist
        } else {
          num1 <- qt(tail_prob, df)
          num2 <- qt(1 - tail_prob, df)
        }
      } else {
        if (!is.na(num1)) {
          result <- calculate_t_asymmetric_thresholds(input$t_known_side, num1, prob_input, df, range_type)
          if (!is.null(result$error)) return(list(error = result$error))
          num1 <- result$num1
          num2 <- result$num2
          t_updating_thresholds(TRUE)
          t_thresholds$num1 <- num1
          t_thresholds$num2 <- num2
          t_updating_thresholds(FALSE)
        }
      }
    }
    
    x_vals <- seq(-5, 5, length.out = 1000)
    y_vals <- dt(x_vals, df)
    df_data <- data.frame(x = x_vals, y = y_vals)
    
    prob <- NA
    error_msg <- NULL
    shade_df <- data.frame(x = numeric(0), y = numeric(0))
    
    if (range_type == "above") {
      prob <- pt(num1, df, lower.tail = FALSE)
      shade_df <- df_data[df_data$x >= num1, ]
    } else if (range_type == "below") {
      prob <- pt(num1, df)
      shade_df <- df_data[df_data$x <= num1, ]
    } else if (range_type == "between") {
      if (is.na(num1) || is.na(num2)) return(NULL)
      if (num2 < num1) error_msg <- "Lower threshold must be less than upper threshold."
      else {
        prob <- pt(num2, df) - pt(num1, df)
        shade_df <- df_data[df_data$x >= num1 & df_data$x <= num2, ]
      }
    } else if (range_type == "outside") {
      if (is.na(num1) || is.na(num2)) return(NULL)
      if (num2 < num1) error_msg <- "Lower threshold must be less than upper threshold."
      else {
        prob <- 1 - (pt(num2, df) - pt(num1, df))
      }
    }
    
    if (!is.null(error_msg)) return(list(error = error_msg))
    
    list(prob = prob, data = df_data, shaded = shade_df, num1 = num1, num2 = num2)
  })
  
  output$t_prob <- renderText({
    res <- t_result()
    if (is.null(res)) return("")
    if (!is.null(res$error)) return(res$error)
    paste0("Probability = ", round(res$prob, 4))
  })
  
  output$t_threshold_text <- renderText({
    res <- t_result()
    if (is.null(res)) return("")
    if (!is.null(res$error)) return("")
    if (input$t_range == "above") {
      paste0("Threshold: Above ", round(res$num1, 4))
    } else if (input$t_range == "below") {
      paste0("Threshold: Below ", round(res$num1, 4))
    } else if (input$t_range == "between") {
      paste0("Threshold: Between ", round(res$num1, 4), " and ", round(res$num2, 4))
    } else if (input$t_range == "outside") {
      paste0("Threshold: Outside ", round(res$num1, 4), " and ", round(res$num2, 4))
    }
  })
  
  output$t_plot <- renderPlot({
    res <- t_result()
    if (is.null(res)) return("")
    if (!is.null(res$error)) return(NULL)
    
    base_plot <- ggplot(res$data, aes(x, y)) +
      geom_line(color = "#2774AE", size = 1.2) +
      labs(title = expression(bold("t-Distribution")), x = "X", y = "Density") +
      theme_minimal()
    
    if (input$show_normal_overlay) {
      normal_df <- data.frame(
        x = res$data$x,
        y = dnorm(res$data$x)
      )
      base_plot <- base_plot +
        geom_line(data = normal_df, aes(x, y), color = "darkgrey", linetype = "dashed", size = 1.2)
    }
    
    if (input$t_range == "outside") {
      left <- subset(res$data, x <= res$num1)
      right <- subset(res$data, x >= res$num2)
      base_plot +
        geom_area(data = left, aes(x, y), fill = "#2774AE", alpha = 0.5) +
        geom_area(data = right, aes(x, y), fill = "#2774AE", alpha = 0.5)
    } else {
      base_plot +
        geom_area(data = res$shaded, aes(x, y), fill = "#2774AE", alpha = 0.5)
    }
  })
  
  # ======================================================================
  # TAB 3: Chi-square
  # ======================================================================
  
  chisq_debounced_num1 <- debounce(reactive(input$chisq_num1), 300)
  
  chisq_threshold <- reactiveValues(num1 = 2)
  chisq_threshold_locked_by_user <- reactiveVal(FALSE)
  
  observeEvent(chisq_debounced_num1(), {
    if (!is.null(chisq_debounced_num1()) && !is.na(chisq_debounced_num1())) {
      chisq_threshold$num1 <- chisq_debounced_num1()
      chisq_threshold_locked_by_user(TRUE)
    }
  })
  
  output$chisq_dynamic_inputs <- renderUI({
    numericInput(
      "chisq_num1",
      "Threshold",
      value = round(chisq_threshold$num1, 4),
      step = 0.01
    )
  })
  
  chisq_result <- reactive({
    req(input$chisq_df, input$chisq_range, input$chisq_mode)
    
    df <- input$chisq_df
    range_type <- input$chisq_range
    mode <- input$chisq_mode
    prob_input <- if (!is.null(input$chisq_prob_input)) as.numeric(input$chisq_prob_input) else NA
    num1 <- if (mode == "chisq") {
      if (!is.null(chisq_debounced_num1())) as.numeric(chisq_debounced_num1()) else NA
    } else {
      if (is.na(prob_input)) return(NULL)
      if (range_type == "below") {
        qchisq(prob_input, df)
      } else {
        qchisq(1 - prob_input, df)
      }
    }
    
    if (mode == "inverse" && !is.na(num1)) {
      chisq_threshold$num1 <- num1
    }
    
    x_vals <- seq(0, qchisq(0.999, df), length.out = 1000)
    y_vals <- dchisq(x_vals, df)
    df_data <- data.frame(x = x_vals, y = y_vals)
    
    prob <- NA
    shade_df <- data.frame(x = numeric(0), y = numeric(0))
    
    if (range_type == "above") {
      prob <- pchisq(num1, df, lower.tail = FALSE)
      shade_df <- df_data[df_data$x >= num1, ]
    } else if (range_type == "below") {
      prob <- pchisq(num1, df)
      shade_df <- df_data[df_data$x <= num1, ]
    }
    
    list(prob = prob, data = df_data, shaded = shade_df, num1 = num1)
  })
  
  output$chisq_prob <- renderText({
    res <- chisq_result()
    if (is.null(res)) return("Invalid input.")
    paste0("Probability = ", round(res$prob, 4))
  })
  
  output$chisq_threshold_text <- renderText({
    res <- chisq_result()
    if (is.null(res)) return("")
    if (input$chisq_range == "above") {
      paste0("Threshold: Above ", round(res$num1, 4))
    } else {
      paste0("Threshold: Below ", round(res$num1, 4))
    }
  })
  
  output$chisq_plot <- renderPlot({
    res <- chisq_result()
    if (is.null(res)) return(NULL)
    
    ggplot(res$data, aes(x, y)) +
      geom_line(color = "#2774AE", size = 1.2) +
      labs(title = expression(bold("Chi-square Distribution")), x = "X", y = "Density") +
      theme_minimal() +
      geom_area(data = res$shaded, aes(x, y), fill = "#2774AE", alpha = 0.5)
  })
  
  # ======================================================================
  # TAB 4: One Proportion Test
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
      need(input$p > 0 && input$p < 1,
           "Hypothesised proportion p0 must be in [0,1]"),
      need(ph >= 0 && ph <= 1,
           "Sample proportion p_hat must be in [0,1]")
    )
    
    prop.test(x, input$n, p = input$p, alternative = input$alternative,
              conf.level = as.numeric(input$conf_level))
  })
  
  # CI check box
  observe({
    if (input$show_ci) {
      test_results <- calc_results()
      if (is.null(test_results)) {
        output$ci_table <- renderText({ "" })
        output$ci_conclusion <- renderText({ "Invalid input parameters." })
        return()
      }
    } else {
      # hide or clear these outputs if the check box is not selected
      output$ci_label <- renderText({ "" })
      output$ci_conclusion <- renderText({ "" })
    }
  })
  
  # plot
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
      bar_col[x_vals <= x_obs] <- "#2774AE"
    } else if (input$alternative == "greater") {
      bar_col[x_vals >= x_obs] <- "#2774AE"
    } else {                                     
      mu <- input$n * input$p
      d <- abs(x_obs - mu)
      idx  <- which(x_vals <= floor(mu - d) |
                      x_vals >= ceiling(mu + d))
      bar_col[idx] <- "#2774AE"
    }
    
    df <- data.frame(x = x_vals, prob = probs, color = bar_col)
    
    # Compute mean and SD under null
    mu <- input$n * input$p
    sd <- sqrt(input$n * input$p * (1 - input$p))
    
    # Cutoffs at ±4 SD
    lower <- max(0, floor(mu - 4 * sd))
    upper <- min(input$n, ceiling(mu + 4 * sd))
    
    ggplot(df, aes(x = x, y = prob, fill = color)) +
      geom_col(color = "#2774AE") +
      scale_x_continuous(
        limits = c(lower, upper),
        breaks = scales::pretty_breaks(n = 10),
        expand = c(0, 0)
      ) +
      scale_fill_identity() +
      labs(
        title = expression(bold("Sampling Distribution Under Null Hypothesis")),
        x = "Number of Successes (x)",
        y = "Probability"
      ) +
      theme_minimal()
  })
  
  # show or hide the test results and conclusions
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
          "less" = paste0("$p < ", input$p, "$"),
          "greater" = paste0("$p > ", input$p, "$"),
          "two.sided" = paste0("$p ≠ ", input$p, "$")
        )
        
        df <- data.frame(
          label = c(
            "$H_0$", "$H_A$", "$n$", "$x$", "$\\hat p$", "$p$‑value"
          ),
          value = c(
            paste0("$p = ", input$p, "$"), alt_str, input$n, 
            round(input$p_hat * input$n), round(test_res$estimate, 3),
            p_value
          )
        )
        df |>
          gt() |>                 
          fmt_markdown(columns = everything()) |>      
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
          conclusions = paste0("The null hypothesis is ", decisions, " at $\\alpha = $ ", alpha)
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
    
    ci_gt_testoff <- reactive({
      req(input$show_ci)
      test_res <- calc_results()
      validate(need(!is.null(test_res), "Invalid input parameters."))
      
      ci <- test_res$conf.int
      data.frame(
        label = c("Confidence level", "Interval"),
        value = c(
          paste0(round(as.numeric(input$conf_level)*100), "%"),
          sprintf("[%.3f, %.3f]", ci[1], ci[2])
        )
      ) |>
        gt() |>
        fmt_markdown(columns = label) |>
        tab_header(title = "Confidence Interval") |>
        tab_options(column_labels.hidden = TRUE)
    })
    
    output$ci_table_side <- render_gt(
      ci_gt() |> 
        tab_options(column_labels.hidden = TRUE, table.width = pct(100)))
    
    output$ci_table_bottom <- render_gt(
      ci_gt_testoff() |> 
        tab_options(column_labels.hidden = TRUE, table.width = pct(100))
    )
  })
  
  # ======================================================================
  # TAB 5: One Mean
  # ======================================================================
  
  mean_results <- reactive({
    n <- input$mean_n
    s <- input$mean_s
    mu0 <- input$mean_mu0
    xbar <- input$mean_xbar
    df <- n - 1
    
    validate(
      need(n > 1, "Sample size n must be > 1"),
      need(s > 0, "Sample SD must be > 0")
    )
    
    t_stat <- (xbar - mu0) / (s / sqrt(n))
    p_val <- switch(input$mean_alt,
                    "less" = pt(t_stat, df), "greater" = 1 - pt(t_stat, df),
                    "two.sided" = 2 * (1 - pt(abs(t_stat), df)))
    
    conf <- as.numeric(input$mean_conf_level)
    moe <- qt(1 - (1-conf)/2, df) * s / sqrt(n)
    ci <- xbar + c(-1,1) * moe
    list(n=n, s=s, df=df, mu0=mu0, xbar=xbar,
         t_stat=t_stat, p_value=p_val, ci=ci)
  })
  
  # results table 
  output$mean_results_table <- render_gt({
    res <- mean_results()
    
    alt_sym <- switch(input$mean_alt,
                      "less" = "<", "greater" = ">", "two.sided" = "≠")
    df <- data.frame(
      label = c("$H_0$", "$H_A$",
                "$n$", "$\\bar{x}$", "$s$",
                "$t$", "$p$‑value"),
      value = c(paste0("$\\mu = ", res$mu0, "$"),
                paste0("$\\mu ", alt_sym, " ", res$mu0, "$"),
                res$n, round(res$xbar,3), round(res$s,3), 
                round(res$t_stat,3),
                formatC(res$p_value, format = "f", digits = 4))
    )
    df |>
      gt() |>
      fmt_markdown(columns = everything()) |>
      tab_header(title = "One Mean t‑Test") |>
      tab_options(column_labels.hidden = TRUE,
                  table.width = pct(100))
  })
  
  # conclusions table 
  output$mean_conclusions <- render_gt({
    res <- mean_results()
    alpha <- c(0.01,0.05,0.10)
    dec <- ifelse(res$p_value < alpha, "**rejected**", "**not rejected**")
    
    data.frame(
      conclusions = paste0("The null hypothesis is ", dec, " at $\\alpha = $ ", alpha)
    ) |>
      gt() |>
      tab_header(title = "Test Conclusions") |>
      tab_options(column_labels.hidden = TRUE,
                  table.width = pct(100)) |>
      fmt_markdown(columns = everything())
  })
  
  # CI helper & renderers 
  mean_ci_gt <- reactive({
    res <- mean_results()
    data.frame(
      label = c("Confidence level", "Interval"),
      value = c(paste0(round(as.numeric(input$mean_conf_level) * 100), "%"),
                sprintf("[%.3f, %.3f]", res$ci[1], res$ci[2]))
    ) |>
      gt() |>
      tab_header(title = "Confidence Interval") |>
      tab_options(column_labels.hidden = TRUE,
                  table.width = pct(100))
  })
  
  output$mean_ci_table_side   <- render_gt(mean_ci_gt())
  output$mean_ci_table_bottom <- render_gt(mean_ci_gt())
  
  # plot 
  output$mean_plot <- renderPlot({
    if (!input$mean_show_test && !input$mean_show_ci) {
      ggplot() + 
        annotate("text", x = 0.5, y = 0.5, label = "No test or CI selected", size = 6) + 
        theme_void()
      return()
    }
    
    res <- mean_results()
    x <- seq(-4, 4, len = 400)
    y <- dt(x, df = res$df)
    df_plot <- data.frame(x = x, y = y)
    shade <- "#2774AE"
    
    p <- ggplot(df_plot, aes(x = x, y = y)) +
      geom_line(color = "#2774AE", size = 1.2) +
      labs(
        x = "t-value", y = "Density",
        title = expression(bold("Sampling Distribution Under Null Hypothesis"))
      ) +
      theme_minimal()
    
    if (input$mean_show_test) {
      if (input$mean_alt == "less") {
        p <- p + 
          geom_area(data = subset(df_plot, x <= res$t_stat), 
                    aes(x = x, y = y), fill = shade, alpha = 0.5)
      } else if (input$mean_alt == "greater") {
        p <- p + 
          geom_area(data = subset(df_plot, x >= res$t_stat), 
                    aes(x = x, y = y), fill = shade, alpha = 0.5)
      } else {
        crit <- abs(res$t_stat)
        p <- p + 
          geom_area(data = subset(df_plot, x <= -crit), 
                    aes(x = x, y = y), fill = shade, alpha = 0.5) +
          geom_area(data = subset(df_plot, x >= crit), 
                    aes(x = x, y = y), fill = shade, alpha = 0.5)
      }
    }
    
    p
  })
  
  # ======================================================================
  # TAB 6: Difference Two Proportion
  # ======================================================================
  
  d2_results <- reactive({
    if (input$d2_use_successes1) {
      x1 <- input$d2_x1
      n1 <- input$d2_n1
      p1 <- x1 / n1
    } else {
      p1 <- input$d2_p1hat
      n1 <- input$d2_n1
      x1 <- round(p1 * n1)
    }
    
    if (input$d2_use_successes2) {
      x2 <- input$d2_x2
      n2 <- input$d2_n2
      p2 <- x2 / n2
    } else {
      p2 <- input$d2_p2hat
      n2 <- input$d2_n2
      x2 <- round(p2 * n2)
    }
    
    validate(
      need(n1 > 0 && n2 > 0, "Sample sizes must be > 0"),
      need(x1 >= 0 && x1 <= n1, "x₁ must be between 0 and n₁"),
      need(x2 >= 0 && x2 <= n2, "x₂ must be between 0 and n₂"),
      need(p1 >= 0 && p1 <= 1, "p̂₁ must be in [0,1]"),
      need(p2 >= 0 && p2 <= 1, "p̂₂ must be in [0,1]")
    )
    # calculate test statistic, p-value, and ci
    delta0 <- 0
    diff_hat <- p1 - p2
    se <- sqrt(p1*(1-p1)/n1 + p2*(1-p2)/n2)
    z_stat <- diff_hat / se
    p_val <- switch(input$d2_alternative,
                    "less" = pnorm(z_stat), "greater" = 1 - pnorm(z_stat),
                    "two.sided" = 2 * (1 - pnorm(abs(z_stat))))
    ci <- diff_hat + c(-1,1) * qnorm(1 - (1 - as.numeric(input$d2_conf_level))/2) * se
    
    # store everything for use later
    list(n1=n1, n2=n2, x1=x1, x2=x2, p1=p1, p2=p2,
         diff_hat = diff_hat,
         z_stat = z_stat,
         p_value = p_val,
         ci = ci)
  })
  
  # results table
  output$d2_results_table <- render_gt({
    res <- d2_results()
    alt_str2 <- switch(input$d2_alternative,
                       "less" = "<", "greater" = ">", "two.sided" = "≠")
    # hypothesis strings
    h0 <- paste0("$p_1 - p_2 = 0 $")
    ha <- paste0("$p_1 - p_2 ", alt_str2, " 0$")
    delta_lab <- format(res$delta0, trim = TRUE)
    # output
    df <- data.frame(
      label = c("$H_0$", "$H_A$", "$n_1$", "$x_1$", "$\\hat p_1$", "$n_2$",
                "$x_2$", "$\\hat p_2$", "$\\hat p_1 - \\hat p_2$", 
                "$z_{obs}$", "$p$‑value"),
      value = c(h0, ha,res$n1, res$x1, round(res$p1,3), res$n2, res$x2, 
                round(res$p2,3), round(res$diff_hat,3), round(res$z_stat, 4),
                formatC(res$p_value, format = "f", digits = 4))
    )
    df |>
      gt() |>                 
      fmt_markdown(columns = everything()) |>      
      tab_header(title = "Two Proportion Test") |>
      tab_options(column_labels.hidden = TRUE, 
                  table.width = pct(100)) 
  })
  
  # conclusions table
  output$d2_conclusions <- render_gt({
    res <- d2_results()
    alpha <- c(0.01, 0.05, 0.10)
    decision <- ifelse(res$p_value < alpha, "**rejected**", "**not rejected**")
    data.frame(
      conclusions = paste0("The null hypothesis is ", decision, " at $\\alpha = $ ", alpha)
    ) |>
      gt() |>
      tab_header(title = "Test Conclusions") |>
      tab_options(column_labels.hidden = TRUE,
                  table.width = pct(100)) |>
      fmt_markdown(columns = everything())
  })
  
  # confidence interval
  d2_ci_gt <- reactive({
    res <- d2_results()
    data.frame(
      label = c("Confidence level", "Interval"),
      value = c(paste0(round(as.numeric(input$d2_conf_level)*100), "%"),
                sprintf("[%.3f, %.3f]", res$ci[1], res$ci[2]))
    ) |>
      gt() |>
      tab_header(title = "Confidence Interval") |>
      tab_options(column_labels.hidden = TRUE, table.width = pct(100))
  })
  
  output$d2_ci_table_side <- render_gt(
    d2_ci_gt() |> 
      tab_options(column_labels.hidden = TRUE, table.width = pct(100)))
  
  output$d2_ci_table_bottom <- render_gt(
    d2_ci_gt() |> 
      tab_options(column_labels.hidden = TRUE, table.width = pct(100))
  )
  
  # plot
  output$d2_zplot <- renderPlot({
    if (!input$d2_show_test) {
      ggplot() +
        annotate("text", x = 0, y = 0, label = "Test switched off", size = 6) +
        theme_void()
      return()
    }
    
    res <- d2_results() 
    if (is.null(res)) {
      ggplot() +
        annotate("text", x = 0, y = 0, label = "Invalid input parameters", size = 6) +
        theme_void()
      return()
    }
    
    z_obs <- res$z_stat           
    x <- seq(-4, 4, length = 400)
    y <- dnorm(x) # N(0,1) pdf
    df_plot <- data.frame(x = x, y = y)
    shade <- "#2774AE"
    
    p <- ggplot(df_plot, aes(x = x, y = y)) +
      geom_line(color = "#2774AE", size = 1.2) +
      labs(
        x = "z-value", y = "Density",
        title = expression(bold("Sampling Distribution Under Null Hypothesis"))
      ) +
      theme_minimal()
    
    if (input$d2_alternative == "less") {
      p <- p + 
        geom_area(data = subset(df_plot, x <= z_obs),
                  aes(x = x, y = y), fill = shade, alpha = 0.5)
    } else if (input$d2_alternative == "greater") {
      p <- p + 
        geom_area(data = subset(df_plot, x >= z_obs),
                  aes(x = x, y = y), fill = shade, alpha = 0.5)
    } else {
      crit <- abs(z_obs)
      p <- p +
        geom_area(data = subset(df_plot, x <= -crit),
                  aes(x = x, y = y), fill = shade, alpha = 0.5) +
        geom_area(data = subset(df_plot, x >= crit),
                  aes(x = x, y = y), fill = shade, alpha = 0.5)
    }
    
    p
  })
  
  # ======================================================================
  # TAB 7: Difference Two Means
  # ======================================================================

  d2m_results <- reactive({
    n1    <- input$d2m_n1
    xbar1 <- input$d2m_xbar1
    s1    <- input$d2m_s1
    n2    <- input$d2m_n2
    xbar2 <- input$d2m_xbar2
    s2    <- input$d2m_s2
    delta0 <- 0
    conf_level <- as.numeric(input$d2m_conf_level)
    var_equal_assumed <- input$d2m_var_equal 
    
    
    validate(
      need(n1 >= 2, "Sample size n₁ must be ≥ 2."),
      need(n2 >= 2, "Sample size n₂ must be ≥ 2."),
      need(s1 > 0, "Sample SD s₁ must be > 0."),
      need(s2 > 0, "Sample SD s₂ must be > 0.")
    )
  
    diff_xbar <- xbar1 - xbar2
    df <- NA
    se <- NA
    t_stat <- NA
    p_val <- NA
    ci <- c(NA, NA)
    
    if (var_equal_assumed) {
      # Pooled t-test (assuming equal variances)
      df <- n1 + n2 - 2
      # Calculate pooled variance and then pooled standard deviation
      s_p_squared <- ((n1 - 1) * s1^2 + (n2 - 1) * s2^2) / df
      s_p <- sqrt(s_p_squared)
      # Standard error for pooled t-test
      se <- s_p * sqrt(1/n1 + 1/n2)
      
    } else {
      # Welch's t-test (not assuming equal variances)
      se <- sqrt(s1^2/n1 + s2^2/n2)
      # Welch-Satterthwaite degrees of freedom
      df_num <- (s1^2/n1 + s2^2/n2)^2
      df_den <- ( (s1^2/n1)^2 / (n1-1) ) + ( (s2^2/n2)^2 / (n2-1) )
      if (df_den == 0) { 
        df <- Inf 
      } else {
        df <- df_num / df_den
      }
    }
    # t-statistic (common formula structure, SE and df differ)
    if (is.na(se) || se == 0) { # check for NA or zero SE before division
      t_stat <- NA
    } else {
      t_stat <- (diff_xbar - delta0) / se
    }
    
    # p-value and confidence interval depend on t_stat, df, and se
    if (!is.na(t_stat) && !is.na(df) && df > 0) { # df must be positive
      p_val <- switch(input$d2m_alternative,
                      "less" = pt(t_stat, df),
                      "greater" = pt(t_stat, df, lower.tail = FALSE),
                      "two.sided" = 2 * pt(abs(t_stat), df, lower.tail = FALSE)
      )
      
      t_crit <- qt(1 - (1 - conf_level)/2, df)
      moe <- t_crit * se
      ci <- diff_xbar + c(-moe, moe)
    } else {
      p_val <- NA
      ci <- c(NA, NA)
    }
    
    list(
      n1 = n1, xbar1 = xbar1, s1 = s1,
      n2 = n2, xbar2 = xbar2, s2 = s2,
      delta0 = delta0, diff_xbar = diff_xbar,
      t_stat = t_stat, df = df, p_value = p_val,
      ci = ci, conf_level = conf_level,
      var_equal_assumed = var_equal_assumed 
    )
  })
  
  # Results table for Difference Two Means
  output$d2m_results_table <- render_gt({
    res <- d2m_results()
    validate(need(!is.null(res), "Calculation error or invalid inputs."))
    
    alt_sym <- switch(input$d2m_alternative,
                      "less" = "<", "greater" = ">", "two.sided" = "≠")
    
    h0_str <- paste0("$\\mu_1 - \\mu_2 = ", res$delta0, "$")
    ha_str <- paste0("$\\mu_1 - \\mu_2 ", alt_sym, " ", res$delta0, "$")
    
    df_out <- data.frame(
      label = c("$H_0$", "$H_A$",
                "$n_1$", "$\\bar{x}_1$", "$s_1$",
                "$n_2$", "$\\bar{x}_2$", "$s_2$",
                "$\\bar{x}_1 - \\bar{x}_2$",
                "$df$", "$t_{obs}$", "$p$‑value"),
      value = c(h0_str, ha_str,
                res$n1, round(res$xbar1, 3), round(res$s1, 3),
                res$n2, round(res$xbar2, 3), round(res$s2, 3),
                round(res$diff_xbar, 3),
                round(res$df, 2), round(res$t_stat, 3),
                formatC(res$p_value, format = "f", digits = 4))
    )
    
    df_out |>
      gt() |>
      fmt_markdown(columns = everything()) |>
      tab_header(title = "T-Test for Difference Between Two Means") |>
      tab_options(column_labels.hidden = TRUE, table.width = pct(100))
  })
  
  # Conclusions table for Difference Two Means
  output$d2m_conclusions <- render_gt({
    res <- d2m_results()
    validate(need(!is.null(res), "")) # Silently fail if res is null
    
    alpha <- c(0.01, 0.05, 0.10)
    decisions <- ifelse(res$p_value < alpha, "**rejected**", "**not rejected**")
    
    data.frame(
      conclusions = paste0("The null hypothesis is ", decisions, " at $\\alpha = $ ", alpha)
    ) |>
      gt() |>
      tab_header(title = "Test Conclusions") |>
      tab_options(column_labels.hidden = TRUE, table.width = pct(100)) |>
      fmt_markdown(columns = everything())
  })
  
  # CI helper for Difference Two Means
  d2m_ci_gt <- reactive({
    res <- d2m_results()
    validate(need(!is.null(res), "CI cannot be computed with current inputs."))
    
    data.frame(
      label = c("Confidence level", "Interval"),
      value = c(paste0(round(res$conf_level * 100), "%"),
                sprintf("[%.3f, %.3f]", res$ci[1], res$ci[2]))
    ) |>
      gt() |>
      fmt_markdown(columns = label) |>
      tab_header(title = "Confidence Interval") |>
      tab_options(column_labels.hidden = TRUE, table.width = pct(100))
  })
  
  output$d2m_ci_table_side   <- render_gt(d2m_ci_gt())
  output$d2m_ci_table_bottom <- render_gt(d2m_ci_gt())
  
  # Plot for Difference Two Means
  output$d2m_plot <- renderPlot({
    if (!input$d2m_show_test && !input$d2m_show_ci) {
      ggplot() +
        annotate("text", x = 0, y = 0, label = "No test or CI selected", size = 6) +
        theme_void()
      return()
    }
    
    res <- tryCatch(d2m_results(), error = function(e) NULL)
    if (is.null(res) || is.na(res$df) || res$df <= 0) {
      ggplot() +
        annotate("text", x = 0, y = 0, label = "Invalid input parameters for plot", size = 6) +
        theme_void()
      return()
    }
    
    t_obs <- res$t_stat
    df_val <- res$df
    plot_range <- max(4, abs(t_obs) + 1)
    x_vals <- seq(-plot_range, plot_range, length.out = 400)
    y_vals <- dt(x_vals, df = df_val)
    df_plot <- data.frame(x = x_vals, y = y_vals)
    
    shade_col <- "#2774AE"
    
    p <- ggplot(df_plot, aes(x = x, y = y)) +
      geom_line(color = "#2774AE", size = 1.2) +
      labs(
        title = expression(bold("Sampling Distribution Under Null Hypothesis")),
        x = "t-value",
        y = "Density"
      ) +
      theme_minimal()
    
    if (input$d2m_show_test) {
      if (input$d2m_alternative == "less") {
        p <- p + 
          geom_area(data = subset(df_plot, x <= t_obs),
                    aes(x = x, y = y), fill = shade_col, alpha = 0.5)
      } else if (input$d2m_alternative == "greater") {
        p <- p + 
          geom_area(data = subset(df_plot, x >= t_obs),
                    aes(x = x, y = y), fill = shade_col, alpha = 0.5)
      } else {
        crit_val <- abs(t_obs)
        p <- p +
          geom_area(data = subset(df_plot, x <= -crit_val),
                    aes(x = x, y = y), fill = shade_col, alpha = 0.5) +
          geom_area(data = subset(df_plot, x >= crit_val),
                    aes(x = x, y = y), fill = shade_col, alpha = 0.5)
      }
    }
    
    p
  })
  
  # ======================================================================
  # TAB 8: Citation
  # ======================================================================
  
  output$copyright <- renderUI({
    copy1 <- p("UCLA Stats Calculator is an R Shiny web app that makes critical-value calculations simple and intuitive, while also serving as a general-purpose tool for performing common statistical tests and confidence intervals.")
    copy2 <- p("Copyright (C) 2025 Tselmen Anuurad, Claudia Chan, Hayley Labia, and Thomas Maierhofer")
    copy3 <- p("This program is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.")
    copy4 <- p("This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.")
    copy5 <- p("You should have received a copy of the GNU General Public License along with this program. If not, see", tags$a(href = "https://www.gnu.org/licenses/", "https://www.gnu.org/licenses/"))
    copy6 <- p("Our license:", tags$a(href = "https://www.gnu.org/licenses/gpl-3.0.txt", "https://www.gnu.org/licenses/gpl-3.0.txt"))
    
    HTML(paste(copy1, copy2, copy3, copy4, copy5, copy6, sep = "<br>"))
  })
  
  output$APA <- renderUI({
    apa1 <- p("")
    apa2 <- p("APA: Anuurad, T., Chan, C., Labia, H., Maierhofer, T. (2025). UCLA Stats Calculator (Version 1.0) [Computer software].", tags$a(href = "https://github.com/tselmena/S25-STATS-199-Shiny-App", "https://github.com/tselmena/S25-STATS-199-Shiny-App"))
    apa3 <- p("")
    
    HTML(paste(apa1, apa2, apa3, sep = "<br>"))
  })
  
  output$bibtex <- renderUI({
    tag0 <- p("")
    tag1 <- p("BibTeX:")
    tag2 <- p("@misc{UCLAStatsCalculator,")
    tag3 <- p("title = {UCLA Stats Calculator},", style = "text-indent: 1em;")
    tag4 <- p("author = {Anuurad, Tselmen and Chan, Claudia and Labia, Hayley and Maierhofer, Thomas},", style = "text-indent: 1em;")
    tag5 <- p("year = {2025},", style = "text-indent: 1em;")
    tag6 <- p("version = {1.0},", style = "text-indent: 1em;")
    tag7 <- p("howpublished = {https://github.com/tselmena/S25-STATS-199-Shiny-App},", style = "text-indent: 1em;")
    tag8 <- p("note = {R Shiny application developed at UCLA; all authors contributed equally; supervised by Thomas Maierhofer}", style = "text-indent: 1em")
    tag9 <- p("}")
    
    HTML(paste(tag0, tag1, tag2, tag3, tag4, tag5, tag6, tag7, tag8, tag9))
  })
}
