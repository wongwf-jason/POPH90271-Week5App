# Library loading
## Shiny packages
library(shiny)
library(shinyWidgets)
## Visualisation packages
library(dplyr)
library(tidyr)
library(ggdist)
library(ggplot2)
library(plotly)
library(patchwork)
library(bslib)
## Mathematical packages
library(deSolve)
library(lhs)
library(distributional)

################################### SERVER #####################################

# 3 Model
## 3.1 Initialise inputs
initN <- 0 # Dummy N(0) value
N <- initN # Initialise N
initS <- 0 # Dummy S(0) value
initI_s <- 0 # Dummy I_s(0) value
initI_a <- 0 # Dummy I_a(0) value
initR <- N - initS - initI_s - initI_a # R(t) dependent on other states
initC <- 0 # Dummy C(0) value
initW <- 0 # Dummy W value

## 3.2 Model Function
sirsc.ode <- function(t, state, parameters) {
  with(as.list(c(state, parameters)), {
    ### 3.2.1 Calculate the total population size
    N <- S + I_s + I_a + R
    ### 3.2.2 Calculate the net (instantaneous) change in each state variable
    S_change <- 1/invBirthDeath_Rate/365 * N +
      1/invImmunityLoss_Rate/365 * R -
      ContamWaterCons_Rate * C / (Cholera_ID50 + C) * S -
      1/invBirthDeath_Rate/365 * S
    I_s_change <- (1 - Asymptomatic_Proportion) * ContamWaterCons_Rate * 
      C / (Cholera_ID50 + C) * S -
      (1/invRecovery_Rate + CholeraDeath_Rate + 1/invBirthDeath_Rate/365) * I_s
    I_a_change <- Asymptomatic_Proportion * ContamWaterCons_Rate * 
      C / (Cholera_ID50 + C) * S -
      (1/invRecovery_Rate + 1/invBirthDeath_Rate/365) * I_a
    R_change <- 1/invRecovery_Rate * (I_s + I_a) - 
      (1/invImmunityLoss_Rate/365 + 1/invBirthDeath_Rate/365) * R
    C_change <- Shedding_Rate_Unnormalised / N *
      (I_s + SheddingAsymptomatic_Modifier * I_a) -
      1/invCholeraDecay_Rate * C
    CholeraDead_change <- CholeraDeath_Rate * I_s
    ### 3.2.3 Return net changes as list
    return(list(
      c(
        S_change,
        I_s_change,
        I_a_change,
        R_change,
        C_change,
        CholeraDead_change
      )
    ))
  })
}

## 3.3 Time Window (by date)
#start_date <- as.Date("2024-05-01")
#end_date <- as.Date("2024-05-31")
#times <- seq(start_date, end_date, by = 1)

function(input, output) {
  ## 4.Y UoM Logo
  # Send a pre-rendered image, and don't delete the image after sending it
  output$Logo <- renderImage({list(src = "UoM_Logo.png",
                                   alt = paste("UoM Logo"),
                                   width = 40, height = 40)},
                             deleteFile = FALSE)
  
  ## 4.A Display selected distribution type
  observeEvent(input$alpha_disttype, {
    updateTabsetPanel(inputId = "disttype_alpha", selected = input$alpha_disttype)
  })
  observeEvent(input$mu_c_disttype, {
    updateTabsetPanel(inputId = "disttype_mu_c", selected = input$mu_c_disttype)
  })
  observeEvent(input$initC_disttype, {
    updateTabsetPanel(inputId = "disttype_initC", selected = input$initC_disttype)
  })
  ## 4.B Draw samples with selected distribution type
  LHS_samples <- reactive(switch(input$sampling_method,
                                 "Latin Hypercube Sampling" = randomLHS(input$sample_n, 3), # Replace 3 with count of uncertain parameters
                                 "Random Search" = matrix(runif(input$sample_n*3, min = 0, max = 1), input$sample_n, 3)
  ))
  
  
  
  
  
  
  
  
  ## 4.1 Reactive SIRSC model
  output$out_plot <- renderPlotly({
    ### 4.P.2 Progress message 2 RESTORE EARLIER PROGRESS MESSAGES AFTER FIXING INV DISTRIBUTIONS
    withProgress(message = "Specifying initial states and parameters...", value = 0.1, {
      LHS_samples <- as.data.frame(LHS_samples())
      colnames(LHS_samples) <- c("alpha_samples", "mu_c_samples", "initC_samples")
      
      # Apply inverse distribution to LHS values
      LHS_samples$alpha_samples <- switch(input$alpha_disttype,
                                          Uniform = qunif(p = LHS_samples$alpha_samples,
                                                          min = input$unif_alpha_range[1],
                                                          max = input$unif_alpha_range[2]),
                                          Normal = qnorm(p = LHS_samples$alpha_samples,
                                                         mean = input$norm_alpha_mean,
                                                         sd = input$norm_alpha_sd),
                                          Beta = qbeta(p = LHS_samples$alpha_samples,
                                                       shape1 = input$beta_alpha_shape1,
                                                       shape2 = input$beta_alpha_shape2),
                                          Binomial = qbinom(p = LHS_samples$alpha_samples,
                                                            size = input$binom_alpha_size,
                                                            prob = input$binom_alpha_prob),
                                          "Negative Binomial" = qnbinom(n = input$alpha_n,
                                                                        size = input$nbinom_alpha_size,
                                                                        prob = input$nbinom_alpha_prob),
                                          Poisson = qpois(q = input$alpha_n,
                                                          lambda = input$pois_alpha_lambda)
      )
      LHS_samples$mu_c_samples <- switch(input$mu_c_disttype,
                                         Uniform = qunif(p = LHS_samples$mu_c_samples,
                                                         min = input$unif_mu_c_range[1],
                                                         max = input$unif_mu_c_range[2]),
                                         Normal = qnorm(p = LHS_samples$mu_c_samples,
                                                        mean = input$norm_mu_c_mean,
                                                        sd = input$norm_mu_c_sd),
                                         Beta = qbeta(p = LHS_samples$mu_c_samples,
                                                      shape1 = input$beta_mu_c_shape1,
                                                      shape2 = input$beta_mu_c_shape2),
                                         Binomial = qbinom(p = LHS_samples$mu_c_samples,
                                                           size = input$binom_mu_c_size,
                                                           prob = input$binom_mu_c_prob),
                                         "Negative Binomial" = qnbinom(n = input$mu_c_n,
                                                                       size = input$nbinom_mu_c_size,
                                                                       prob = input$nbinom_mu_c_prob),
                                         Poisson = qpois(q = input$mu_c_n,
                                                         lambda = input$pois_mu_c_lambda)
      )
      LHS_samples$initC_samples <- switch(input$initC_disttype,
                                          Uniform = qunif(p = LHS_samples$initC_samples,
                                                          min = input$unif_initC_range[1],
                                                          max = input$unif_initC_range[2]),
                                          Normal = qnorm(p = LHS_samples$initC_samples,
                                                         mean = input$norm_initC_mean,
                                                         sd = input$norm_initC_sd),
                                          Beta = qbeta(p = LHS_samples$initC_samples,
                                                       shape1 = input$beta_initC_shape1,
                                                       shape2 = input$beta_initC_shape2),
                                          Binomial = qbinom(p = LHS_samples$initC_samples,
                                                            size = input$binom_initC_size,
                                                            prob = input$binom_initC_prob),
                                          "Negative Binomial" = qnbinom(n = input$initC_n,
                                                                        size = input$nbinom_initC_size,
                                                                        prob = input$nbinom_initC_prob),
                                          Poisson = qpois(q = input$initC_n,
                                                          lambda = input$pois_initC_lambda)
      )
      ### 4.1.1V Create state list for every sample (VECTORISED)
      state_list <- lapply(LHS_samples$initC_samples, function(initC_sample) {
        c(
          S = input$initS,
          I_s = input$initI_s,
          I_a = input$initI_a,
          R = initR,
          C = initC_sample,
          CholeraDead = 0 # Define CholeraDead compartment to monitor cholera deaths
        )
      })  
      
      ### 4.1.2 Time window (by number of days)
      times <- seq(1, input$days, by = 1)
      ### 4.1.3V Create parameter list for every sample of parameter combinations (VECTORISED)
      parameters_list <- mapply(function(ContamWaterCons_Rate_sample, CholeraDeath_Rate_sample) {
        c(
          ContamWaterCons_Rate = ContamWaterCons_Rate_sample,
          Cholera_ID50 = input$Cholera_ID50,
          Asymptomatic_Proportion = input$Asymptomatic_Proportion,
          CholeraDeath_Rate = CholeraDeath_Rate_sample,
          invRecovery_Rate = input$invRecovery_Rate,
          invImmunityLoss_Rate = input$invImmunityLoss_Rate,
          invBirthDeath_Rate = input$invBirthDeath_Rate,
          Shedding_Rate_Unnormalised = input$Shedding_Rate / input$W,
          SheddingAsymptomatic_Modifier = input$SheddingAsymptomatic_Modifier,
          invCholeraDecay_Rate = input$invCholeraDecay_Rate
        )
      }, LHS_samples$alpha_samples, LHS_samples$mu_c_samples, SIMPLIFY = FALSE)
      #browser()
      
      ### 4.1.4V Define function to solve model for each set of parameters
      solve_model <- function(state, parameters) {
        ode(
          y = state,
          #times = as.numeric(times - times[1]), # Time by dates
          times = times - times[1], # Time by number of days
          func = sirsc.ode,
          parms = parameters)
      }
      ### 4.P.2 Progress message 3
      incProgress(message = "Solving ordinary differential equations...", detail = HTML("Cholera is being transmitted \uD83E\uDDA0! This should take no longer than ~30 seconds \uD83D\uDE47."), amount = 0.2)
      #browser()    
      ### 4.1.5V Solve model for all sampled parameters and output to list (VECTORISED)
      solutions_list <- mapply(solve_model, state_list, parameters_list, SIMPLIFY = FALSE)
      #browser()
      ### 4.1.6V Combine all results into one data frame (VECTORISED)
      combined_out_df <<- do.call(rbind, lapply(seq_along(solutions_list), function(i) {
        out_df <- data.frame(solutions_list[[i]])
        out_df$sample <- i
        out_df$N <- out_df$S + out_df$I_s + out_df$I_a + out_df$R # Define Total compartment to monitor live population
        out_df$Total <-  out_df$S + out_df$I_s + out_df$I_a + out_df$R + out_df$CholeraDead # Define Total compartment to monitor live population + cholera deaths
        # out_df$Lambda <- out_df$ContamWaterCons_Rate_sample * out_df$C / (input$Cholera_ID50 + C) # Define Lambda column to monitor force of infection
        out_df
      }))
      
      #### 4.1.6.1 Add NA breaks in data frame to prevent plotly from joining traces of different samples
      combined_out_df <- combined_out_df |>
        group_by(sample) |>
        reframe(across(everything(), ~ c(., NA)))
      
      #### 4.P.4 Progress message 4
      incProgress(message = 'Rendering plots...', detail = "", amount = 0.7)
      
      
      ### 4.1.7 Plot interactive time series
      #### 4.1.7.1 Human compartments plot
      out_plot_human <- plot_ly(data = combined_out_df, x = ~time, opacity = 0.5) |>
        add_trace(y = ~S, name = 'Susceptible (S) ', type = 'scatter', mode = 'lines',
                  line = list(width = ifelse(input$sample_n %in% c(1, 2, 5, 10), 0.8,
                                             ifelse(input$sample_n == 100, 0.1, 0.05)))) |>
        add_trace(y = ~I_s, name = 'Symptomatic (I<sub>s</sub>)', type = 'scatter', mode = 'lines', 
                  line = list(width = ifelse(input$sample_n %in% c(1, 2, 5, 10), 0.8,
                                             ifelse(input$sample_n == 100, 0.1, 0.05)))) |>
        add_trace(y = ~I_a, name = 'Asymptomatic (I<sub>a</sub>)', type = 'scatter', mode = 'lines', 
                  line = list(width = ifelse(input$sample_n %in% c(1, 2, 5, 10), 0.8,
                                             ifelse(input$sample_n == 100, 0.1, 0.05)))) |>
        add_trace(y = ~R, name = 'Recovered (R)', type = 'scatter', mode = 'lines', 
                  line = list(width = ifelse(input$sample_n %in% c(1, 2, 5, 10), 0.8,
                                             ifelse(input$sample_n == 100, 0.1, 0.05)))) |>
        add_trace(y = ~N, name = 'Total Humans Alive (N)', type = 'scatter', mode = 'lines', 
                  line = list(width = ifelse(input$sample_n %in% c(1, 2, 5, 10), 0.8,
                                             ifelse(input$sample_n == 100, 0.1, 0.05)))) |>
        add_trace(y = ~CholeraDead, name = 'Cholera Deaths (D<sub>c</sub>)', type = 'scatter', mode = 'lines',
                  line = list(width = ifelse(input$sample_n %in% c(1, 2, 5, 10), 0.8,
                                             ifelse(input$sample_n == 100, 0.1, 0.05))),
                  visible = 'legendonly') |>
        add_trace(y = ~Total, name = 'Total Humans Alive + Cholera Deaths (N + D<sub>c</sub>)', type = 'scatter', mode = 'lines',
                  line = list(width = ifelse(input$sample_n %in% c(1, 2, 5, 10), 0.8,
                                             ifelse(input$sample_n == 100, 0.1, 0.05))),
                  visible = 'legendonly') |>
        layout(xaxis = list(title = "Time (days)"),
               yaxis = list(title = "Number of individuals (persons)"))
    })})
  
  ## 4.2 Distribution of samples
  ### 4.2.1 Confirmation of sampling distributions and sampling method selected
  output$out_sampling_method_selection <- renderUI(p(HTML(paste("You have chosen to sample <b>", input$sample_n, "</b> values of the parameter space. Now, <b>", input$sample_n, "iterations </b> of the SIRSC model across <b>", input$days, "days </b> are plotted below."))))
  ### 4.2.2 3D scatter to visualise sampled parameter space
  output$out_parameter_space <- renderPlotly({
    LHS_samples <- as.data.frame(LHS_samples())
    colnames(LHS_samples) <- c("alpha_samples", "mu_c_samples", "initC_samples")
    
    # Apply inverse distribution to LHS values
    LHS_samples$alpha_samples <- switch(input$alpha_disttype,
                                        Uniform = qunif(p = LHS_samples$alpha_samples,
                                                        min = input$unif_alpha_range[1],
                                                        max = input$unif_alpha_range[2]),
                                        Normal = qnorm(p = LHS_samples$alpha_samples,
                                                       mean = input$norm_alpha_mean,
                                                       sd = input$norm_alpha_sd),
                                        Beta = qbeta(p = LHS_samples$alpha_samples,
                                                     shape1 = input$beta_alpha_shape1,
                                                     shape2 = input$beta_alpha_shape2),
                                        Binomial = qbinom(p = LHS_samples$alpha_samples,
                                                          size = input$binom_alpha_size,
                                                          prob = input$binom_alpha_prob),
                                        "Negative Binomial" = qnbinom(n = input$alpha_n,
                                                                      size = input$nbinom_alpha_size,
                                                                      prob = input$nbinom_alpha_prob),
                                        Poisson = qpois(q = input$alpha_n,
                                                        lambda = input$pois_alpha_lambda)
    )
    LHS_samples$mu_c_samples <- switch(input$mu_c_disttype,
                                       Uniform = qunif(p = LHS_samples$mu_c_samples,
                                                       min = input$unif_mu_c_range[1],
                                                       max = input$unif_mu_c_range[2]),
                                       Normal = qnorm(p = LHS_samples$mu_c_samples,
                                                      mean = input$norm_mu_c_mean,
                                                      sd = input$norm_mu_c_sd),
                                       Beta = qbeta(p = LHS_samples$mu_c_samples,
                                                    shape1 = input$beta_mu_c_shape1,
                                                    shape2 = input$beta_mu_c_shape2),
                                       Binomial = qbinom(p = LHS_samples$mu_c_samples,
                                                         size = input$binom_mu_c_size,
                                                         prob = input$binom_mu_c_prob),
                                       "Negative Binomial" = qnbinom(n = input$mu_c_n,
                                                                     size = input$nbinom_mu_c_size,
                                                                     prob = input$nbinom_mu_c_prob),
                                       Poisson = qpois(q = input$mu_c_n,
                                                       lambda = input$pois_mu_c_lambda)
    )
    LHS_samples$initC_samples <- switch(input$initC_disttype,
                                        Uniform = qunif(p = LHS_samples$initC_samples,
                                                        min = input$unif_initC_range[1],
                                                        max = input$unif_initC_range[2]),
                                        Normal = qnorm(p = LHS_samples$initC_samples,
                                                       mean = input$norm_initC_mean,
                                                       sd = input$norm_initC_sd),
                                        Beta = qbeta(p = LHS_samples$initC_samples,
                                                     shape1 = input$beta_initC_shape1,
                                                     shape2 = input$beta_initC_shape2),
                                        Binomial = qbinom(p = LHS_samples$initC_samples,
                                                          size = input$binom_initC_size,
                                                          prob = input$binom_initC_prob),
                                        "Negative Binomial" = qnbinom(n = input$initC_n,
                                                                      size = input$nbinom_initC_size,
                                                                      prob = input$nbinom_initC_prob),
                                        Poisson = qpois(q = input$initC_n,
                                                        lambda = input$pois_initC_lambda)
    )
    
    #browser()
    plot_ly(data = LHS_samples, x = ~alpha_samples, y = ~mu_c_samples, z = ~initC_samples, colors = "#67B7D1") |>
      add_markers(marker = list(size = ifelse(input$sample_n %in% c(1, 2, 5, 10), 3,
                                              ifelse(input$sample_n == 100, 2, 1)))) |>
      layout(scene = list(xaxis = list(title = '\u03b1'),
                          yaxis = list(title = '\u03bc<sub>c</sub>'),
                          zaxis = list(title = 'C(0)')))
    
  })
  
  # Curve-wise plot
  output$out_curvewise_plot <- renderPlotly({
    LHS_samples <- as.data.frame(LHS_samples())
    colnames(LHS_samples) <- c("alpha_samples", "mu_c_samples", "initC_samples")
    
    # Apply inverse distribution to LHS values
    LHS_samples$alpha_samples <- switch(input$alpha_disttype,
                                        Uniform = qunif(p = LHS_samples$alpha_samples,
                                                        min = input$unif_alpha_range[1],
                                                        max = input$unif_alpha_range[2]),
                                        Normal = qnorm(p = LHS_samples$alpha_samples,
                                                       mean = input$norm_alpha_mean,
                                                       sd = input$norm_alpha_sd),
                                        Beta = qbeta(p = LHS_samples$alpha_samples,
                                                     shape1 = input$beta_alpha_shape1,
                                                     shape2 = input$beta_alpha_shape2),
                                        Binomial = qbinom(p = LHS_samples$alpha_samples,
                                                          size = input$binom_alpha_size,
                                                          prob = input$binom_alpha_prob),
                                        "Negative Binomial" = qnbinom(n = input$alpha_n,
                                                                      size = input$nbinom_alpha_size,
                                                                      prob = input$nbinom_alpha_prob),
                                        Poisson = qpois(q = input$alpha_n,
                                                        lambda = input$pois_alpha_lambda)
    )
    LHS_samples$mu_c_samples <- switch(input$mu_c_disttype,
                                       Uniform = qunif(p = LHS_samples$mu_c_samples,
                                                       min = input$unif_mu_c_range[1],
                                                       max = input$unif_mu_c_range[2]),
                                       Normal = qnorm(p = LHS_samples$mu_c_samples,
                                                      mean = input$norm_mu_c_mean,
                                                      sd = input$norm_mu_c_sd),
                                       Beta = qbeta(p = LHS_samples$mu_c_samples,
                                                    shape1 = input$beta_mu_c_shape1,
                                                    shape2 = input$beta_mu_c_shape2),
                                       Binomial = qbinom(p = LHS_samples$mu_c_samples,
                                                         size = input$binom_mu_c_size,
                                                         prob = input$binom_mu_c_prob),
                                       "Negative Binomial" = qnbinom(n = input$mu_c_n,
                                                                     size = input$nbinom_mu_c_size,
                                                                     prob = input$nbinom_mu_c_prob),
                                       Poisson = qpois(q = input$mu_c_n,
                                                       lambda = input$pois_mu_c_lambda)
    )
    LHS_samples$initC_samples <- switch(input$initC_disttype,
                                        Uniform = qunif(p = LHS_samples$initC_samples,
                                                        min = input$unif_initC_range[1],
                                                        max = input$unif_initC_range[2]),
                                        Normal = qnorm(p = LHS_samples$initC_samples,
                                                       mean = input$norm_initC_mean,
                                                       sd = input$norm_initC_sd),
                                        Beta = qbeta(p = LHS_samples$initC_samples,
                                                     shape1 = input$beta_initC_shape1,
                                                     shape2 = input$beta_initC_shape2),
                                        Binomial = qbinom(p = LHS_samples$initC_samples,
                                                          size = input$binom_initC_size,
                                                          prob = input$binom_initC_prob),
                                        "Negative Binomial" = qnbinom(n = input$initC_n,
                                                                      size = input$nbinom_initC_size,
                                                                      prob = input$nbinom_initC_prob),
                                        Poisson = qpois(q = input$initC_n,
                                                        lambda = input$pois_initC_lambda)
    )
    
    curvewise_plot <- combined_out_df %>%
      group_by(time) %>%
      curve_interval(S, .width = c(.5, .8, .95)) %>%
      ggplot(aes(x = time)) +
      geom_line(aes(y = S)) +
      geom_ribbon(aes(ymin = .lower, ymax = .upper))
    curvewise_plot
  })
}