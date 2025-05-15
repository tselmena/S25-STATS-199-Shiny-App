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
      need(input$p > 0 && input$p < 1,
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
            "$x$",
            "$\\hat p$",
            "$p‑value$"
          ),
          value = c(
            paste0("p = ", input$p),
            alt_str,
            input$n,
            round(input$p_hat * input$n),
            round(test_res$estimate, 3),
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
        tab_options(column_labels.hidden = TRUE,
                    table.width = pct(100)))
    
    output$ci_table_bottom <- render_gt(
      ci_gt_testoff() |> 
        tab_options(column_labels.hidden = TRUE,
                    table.width = pct(100))
    )
    
  })
 
  # ======================================================================
  # TAB 2: One Mean
  # ======================================================================

  ## ---------- core reactive -------------------------------------------------
  mean_results <- reactive({
    
    n    <- input$mean_n
    s    <- input$mean_s
    mu0  <- input$mean_mu0
    xbar <- input$mean_xbar
    df   <- n - 1
    
    validate(
      need(n  > 1,           "Sample size n must be > 1"),
      need(s  > 0,           "Sample SD must be > 0")
    )
    
    t_stat <- (xbar - mu0) / (s / sqrt(n))
    p_val  <- switch(input$mean_alt,
                     "less"      = pt(t_stat, df),
                     "greater"   = 1 - pt(t_stat, df),
                     "two.sided" = 2 * (1 - pt(abs(t_stat), df)))
    
    conf   <- as.numeric(input$mean_conf_level)
    moe    <- qt(1 - (1-conf)/2, df) * s / sqrt(n)
    ci     <- xbar + c(-1,1) * moe
    
    list(n=n, s=s, df=df, mu0=mu0, xbar=xbar,
         t_stat=t_stat, p_value=p_val, ci=ci)
  })
  
  ## ---------- results table --------------------------------------------------
  output$mean_results_table <- render_gt({
    res <- mean_results()
    
    alt_sym <- switch(input$mean_alt,
                      "less" = "<", "greater" = ">", "two.sided" = "≠")
    
    df <- data.frame(
      label = c("$H_0$", "$H_A$",
                "$n$", "$\\bar{x}$", "$s$",
                "$t$", "$p$‑value"),
      value = c(paste0("μ = ", res$mu0),
                paste0("μ ", alt_sym, " ", res$mu0),
                res$n,
                round(res$xbar,3),
                round(res$s,3),
                round(res$t_stat,3),
                format(res$p_value, digits = 4))
    )
    
    df |>
      gt() |>
      fmt_markdown(columns = label) |>
      tab_header(title = "One Mean t‑Test") |>
      tab_options(column_labels.hidden = TRUE,
                  table.width = pct(100))
  })
  
  ## ---------- conclusions table ---------------------------------------------
  output$mean_conclusions <- render_gt({
    res   <- mean_results()
    alpha <- c(0.01,0.05,0.10)
    dec   <- ifelse(res$p_value < alpha, "**rejected**", "**not rejected**")
    
    data.frame(
      conclusions = paste0("The null hypothesis is ",
                           dec, " at α = ", alpha)
    ) |>
      gt() |>
      tab_header(title = "Test Conclusions") |>
      tab_options(column_labels.hidden = TRUE,
                  table.width = pct(100)) |>
      fmt_markdown(columns = everything())
  })
  
  ## ---------- CI helper & renderers -----------------------------------------
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
  
  ## ---------- plot -----------------------------------------------------------
  output$mean_plot <- renderPlot({
    
    if (!input$mean_show_test && !input$mean_show_ci) {
      plot.new(); text(0.5,0.5,"No test or CI selected", cex = 1.4)
      return()
    }
    
    res <- mean_results()
    
    x <- seq(-4,4,len=400)
    y <- dt(x, df = res$df)
    shade <- "#FFD100"
    
    plot(x, y, type="l", lwd=2, col="lightgrey",
         xlab="t‑value", ylab="Density",
         main="t‑Distribution Under Null Hypothesis")
    
    if (input$mean_show_ci) {
      alpha <- 1 - as.numeric(input$mean_conf_level)
      t_cut <- qt(1 - alpha/2, res$df)
      idx   <- x >= -t_cut & x <= t_cut
      polygon(c(x[idx], rev(x[idx])),
              c(y[idx], rep(0, sum(idx))),
              col = "#D1E8FF", border = NA)  # light blue band
    }
    
    
    if (input$mean_show_test) {
      if (input$mean_alt == "less") {
        idx <- x <= res$t_stat
        polygon( c(x[idx], rev(x[idx])), c(y[idx], rep(0, sum(idx))),
                 col = shade, border = NA )
      } else if (input$mean_alt == "greater") {
        idx <- x >= res$t_stat
        polygon( c(x[idx], rev(x[idx])), c(y[idx], rep(0, sum(idx))),
                 col = shade, border = NA )
      } else {
        crit <- abs(res$t_stat)
        
        idxL <- x <= -crit           # left tail indices
        polygon( c(x[idxL], rev(x[idxL])), c(y[idxL], rep(0, sum(idxL))),
                 col = shade, border = NA )
        
        idxR <- x >=  crit           # right tail indices
        polygon( c(x[idxR], rev(x[idxR])), c(y[idxR], rep(0, sum(idxR))),
                 col = shade, border = NA )
      }
    }
  })
  
  # ======================================================================
  # TAB 3: Difference Two Proportion
  # ======================================================================

  z_q <- function(conf) qnorm(1 - (1 - conf) / 2)
  
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
    
    ## guard‑rails -----------------------------------------------------
    validate(
      need(n1 > 0 && n2 > 0,           "Sample sizes must be > 0"),
      need(x1 >= 0 && x1 <= n1,        "x₁ must be between 0 and n₁"),
      need(x2 >= 0 && x2 <= n2,        "x₂ must be between 0 and n₂"),
      need(p1 >= 0 && p1 <= 1,         "p̂₁ must be in [0,1]"),
      need(p2 >= 0 && p2 <= 1,         "p̂₂ must be in [0,1]")
    )
    
    delta0 <- ifelse(abs(input$d2_delta0) > 1, input$d2_delta0 / 100,
                     input$d2_delta0)
    
    ## Wald statistic --------------------------------------------------
    diff_hat <- p1 - p2
    se <- sqrt(p1*(1-p1)/n1 + p2*(1-p2)/n2)
    z_stat <- (diff_hat - delta0) / se
    
    p_val <- switch(input$d2_alternative,
                    "less"      = pnorm(z_stat),
                    "greater"   = 1 - pnorm(z_stat),
                    "two.sided" = 2 * (1 - pnorm(abs(z_stat))))
    
    ci <- diff_hat +
      c(-1,1) * qnorm(1 - (1 - as.numeric(input$d2_conf_level))/2) * se
    
    list(n1=n1, n2=n2, x1=x1, x2=x2, p1=p1, p2=p2,
         diff_hat = diff_hat,
         z_stat   = z_stat,
         p_value  = p_val,
         ci       = ci)
  })
  
  ## ────────────────────────────────────────────────────────────────────
  ##  2.  Tables
  ## ────────────────────────────────────────────────────────────────────
  output$d2_results_table <- render_gt({
    res <- d2_results()
    
    alt_str2 <- switch(input$d2_alternative,
                      "less"      = "<",
                      "greater"   = ">",
                      "two.sided" = "≠")
    
    h0 <- paste0("$p_1 - p_2 = ", input$d2_delta0, "$")
    ha <- paste0("$p_1 - p_2 ", alt_str2, " ", input$d2_delta0, "$")
    
    delta_lab <- format(res$delta0, trim = TRUE)
    
    df <- data.frame(
      label = c("$H_0$",
                "$H_A$",
                "$n_1$",
                "$x_1$",
                "$\\hat p_1$",
                "$n_2$",
                "$x_2$",
                "$\\hat p_2$",
                "$\\hat\\Delta$", 
                "$z$",
                "p‑value"),
      value = c(h0,
                ha,
                res$n1, 
                res$x1, 
                round(res$p1,3),
                res$n2,
                res$x2, 
                round(res$p2,3),
                round(res$diff_hat,3),
                round(res$z_stat, 3),
                format(res$p_value, digits = 4))
    )
    df |>
      gt() |>                 
      fmt_markdown(columns = everything()) |>      
      tab_header(title = "Two Proportion Test") |>
      tab_options(column_labels.hidden = TRUE, 
                  table.width = pct(100)) 
  })
  
  output$d2_conclusions <- render_gt({
    res <- d2_results()
    alpha <- c(0.01,0.05,0.10)
    decision <- ifelse(res$p_value < alpha, "**rejected**", "**not rejected**")
    data.frame(
      conclusions = paste0("The null hypothesis is ",
                           decision, " at α = ", alpha)
    ) |>
      gt() |>
      tab_header(title = "Test Conclusions") |>
      tab_options(column_labels.hidden = TRUE,
                  table.width = pct(100)) |>
      fmt_markdown(columns = everything())
  })
  
  d2_ci_gt <- reactive({
    res <- d2_results()
    data.frame(
      label = c("Confidence level", "Interval"),
      value = c(paste0(round(as.numeric(input$d2_conf_level)*100), "%"),
                sprintf("[%.3f, %.3f]", res$ci[1], res$ci[2]))
    ) |>
      gt() |>
      tab_header(title = "Confidence Interval") |>
      tab_options(column_labels.hidden = TRUE,
                  table.width = pct(100))
  })
  
  output$d2_ci_table_side <- render_gt(
    d2_ci_gt() |> 
        tab_options(column_labels.hidden = TRUE,
                    table.width = pct(100)))
  
  output$d2_ci_table_bottom <- render_gt(
    d2_ci_gt() |> 
      tab_options(column_labels.hidden = TRUE,
                  table.width = pct(100))
  )
  
  
  ## ────────────────────────────────────────────────────────────────────
  ##  3.  Dual‑PMF plot
  ## ────────────────────────────────────────────────────────────────────
  # output$d2_plot <- renderPlot({
  #   if (!input$d2_show_test && !input$d2_show_ci) {
  #     plot.new(); text(0.5,0.5,"No test or CI selected", cex = 1.4)
  #     return()
  #   }
  #   
  #   res <- d2_results()
  #   x1 <- 0:res$n1;  x2 <- 0:res$n2
  #   p1 <- dbinom(x1, res$n1, res$p1)
  #   p2 <- dbinom(x2, res$n2, res$p2)
  #   
  #   ylim <- c(0, 1.1*max(c(p1,p2)))
  #   barplot(p1, names.arg = x1, col = "lightgrey", border="black",
  #           space = 0.2, ylim = ylim,
  #           xlab = "Number of Successes (x)",
  #           ylab = "Probability",
  #           main = "Sampling Distributions Under Null Hypothesis")
  #   
  #   barplot(p2, names.arg = x2, col = "#6BB6FF", border="black",
  #           space = 0.2, add = TRUE, axes = FALSE)
  #   
  #   legend("topright", inset = 0.02,
  #          legend = c("Group 1","Group 2"),
  #          fill   = c("lightgrey","#6BB6FF"),
  #          bty = "n", cex = 0.9)
  # })
  
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

  t_debounced_num1 <- debounce(reactive(input$t_num1), 300)
  t_debounced_num2 <- debounce(reactive(input$t_num2), 300)
  
  observe({
    if (input$t_mode == "inverse" && input$t_range %in% c("between", "outside")) {
      updateCheckboxInput(session, "t_symmetric", value = TRUE)
    }
  })
  
  t_thresholds <- reactiveValues(num1 = -2, num2 = 2)
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
      if (num2 < num1) error_msg <- "Lower threshold must be less than upper threshold."
      else {
        prob <- pt(num2, df) - pt(num1, df)
        shade_df <- df_data[df_data$x >= num1 & df_data$x <= num2, ]
      }
    } else if (range_type == "outside") {
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
    if (!is.null(res$error)) return(res$error)
    paste0("Probability = ", round(res$prob, 4))
  })
  
  output$t_threshold_text <- renderText({
    res <- t_result()
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
    if (!is.null(res$error)) return(NULL)
    
    base_plot <- ggplot(res$data, aes(x, y)) +
      geom_line(color = "blue") +
      labs(title = "t-Distribution", x = "X", y = "Density") +
      theme_minimal()
    
    if (input$t_range == "outside") {
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
  # TAB 7: Chi-square
  # ======================================================================

}
















