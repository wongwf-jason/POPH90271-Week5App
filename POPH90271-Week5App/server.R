############################### Library loading ################################

# Shiny packages
library(shiny)
library(shinyWidgets)

# Visualisation packages
library(dplyr)
library(tidyr)
library(ggdist)
library(ggplot2)
library(plotly)
library(patchwork)
library(bslib)

# Mathematical packages
library(deSolve)
library(lhs)
library(distributional)

################################### Model ######################################

# Initialise inputs
initN <- 0 # Dummy N(0) value
N <- initN # Initialise N
initS <- 0 # Dummy S(0) value
initI_s <- 0 # Dummy I_s(0) value
initI_a <- 0 # Dummy I_a(0) value
initR <- N - initS - initI_s - initI_a # R(t) dependent on other states
initC <- 0 # Dummy C(0) value
initW <- 0 # Dummy W value

# Model Function
sirsc.ode <- function(t, state, parameters) {
  with(as.list(c(state, parameters)), {
    # Calculate the total population size
    N <- S + I_s + I_a + R
    # Calculate the net (instantaneous) change in each state variable
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
    # Return net changes as list
    return(list(c(S_change,
                  I_s_change,
                  I_a_change,
                  R_change,
                  C_change,
                  CholeraDead_change)))
  })
}

################################################################################
################################### SERVER #####################################
################################################################################

function(input, output) {

########################## Seed for reproducibility ############################

set.seed(1)
  
############################ Module 1 server logic #############################

### Reactive SIRSC model time series plots ###
  
# Human population plot
output$M1_out_plot_human <- renderPlotly({
  
  # Progress message 1
  withProgress(message = "Specifying initial states and parameters...", value = 0.1, {

  # States
  state <- c(
    S = input$M1_initS,
    I_s = input$M1_initI_s,
    I_a = input$M1_initI_a,
    R = initR,
    C = input$M1_initC,
    CholeraDead = 0 # Define CholeraDead compartment to monitor cholera deaths
  )
  
  # Parameters
  parameters <- c(
    ContamWaterCons_Rate = input$M1_ContamWaterCons_Rate,
    Cholera_ID50 = input$M1_Cholera_ID50,
    Asymptomatic_Proportion = input$M1_Asymptomatic_Proportion,
    CholeraDeath_Rate = input$M1_CholeraDeath_Rate,
    invRecovery_Rate = input$M1_invRecovery_Rate,
    invImmunityLoss_Rate = input$M1_invImmunityLoss_Rate,
    invBirthDeath_Rate = input$M1_invBirthDeath_Rate,
    Shedding_Rate_Unnormalised = input$M1_Shedding_Rate / input$M1_W,
    SheddingAsymptomatic_Modifier = input$M1_SheddingAsymptomatic_Modifier,
    invCholeraDecay_Rate = input$M1_invCholeraDecay_Rate
  )
  
  # Time window
  times <- seq(1, input$M1_days, by = 0.1)
  
  # Progress message 2
  incProgress(message = "Solving ordinary differential equations...",
              detail = HTML("Cholera is being transmitted \uD83E\uDDA0! This should take no longer than ~30 seconds \uD83D\uDE47"),
              amount = 0.2)
  
  # Solve model and output to matrix
  out_matrix <- ode(
    y = state,
    #times = as.numeric(times - times[1]), # Time by dates
    times = times - times[1], # Time by number of days
    func = sirsc.ode,
    parms = parameters)
  
  # Convert output matrix to data frame for plotly; add N and Total compartments for monitoring
  out_df <- out_matrix |>
    data.frame() |>
    mutate(N = S + I_s + I_a + R) |> # Define Total compartment to monitor live population
    mutate(Total = S + I_s + I_a + R + CholeraDead) |> # Define Total compartment to monitor live population + cholera deaths
    mutate(Lambda = input$M1_ContamWaterCons_Rate * C / (input$M1_Cholera_ID50 + C)) # Define Lambda column to monitor force of infection
  
  # Progress message 3
  incProgress(message = 'Rendering plots...', detail = "", amount = 0.7)
  
  # SIRSC model interactive time series
  out_plot_human <- plot_ly(data = out_df)
  out_plot_human <- out_plot_human |>
    add_trace(x = ~time, y = ~S, type = 'scatter', mode = 'lines',
              name = 'Susceptible') |>
    add_trace(x = ~time, y = ~I_s, type = 'scatter', mode = 'lines',
              name = 'Symptomatic') |>
    add_trace(x = ~time, y = ~I_a, type = 'scatter', mode = 'lines', name = 'Asymptomatic') |>
    add_trace(x = ~time, y = ~R, type = 'scatter', mode = 'lines', name = 'Recovered') |>
    add_trace(x = ~time, y = ~CholeraDead, type = 'scatter', mode = 'lines', name = 'Cholera Deaths',
              visible='legendonly') |>
    add_trace(x = ~time, y = ~N, type = 'scatter', mode = 'lines', name = 'Total Humans Alive') |>
    add_trace(x = ~time, y = ~Total, type = 'scatter', mode = 'lines', name = 'Total Humans Alive + Cholera Deaths',
              visible='legendonly') |>
    layout(xaxis = list(title = "Time (days)"),
           yaxis = list(title = "Number of individuals (persons)"))
})})

# Force of infection plot
output$M1_out_plot_lambda <- renderPlotly({
  
    # States
    state <- c(
      S = input$M1_initS,
      I_s = input$M1_initI_s,
      I_a = input$M1_initI_a,
      R = initR,
      C = input$M1_initC,
      CholeraDead = 0 # Define CholeraDead compartment to monitor cholera deaths
    )
    
    # Parameters
    parameters <- c(
      ContamWaterCons_Rate = input$M1_ContamWaterCons_Rate,
      Cholera_ID50 = input$M1_Cholera_ID50,
      Asymptomatic_Proportion = input$M1_Asymptomatic_Proportion,
      CholeraDeath_Rate = input$M1_CholeraDeath_Rate,
      invRecovery_Rate = input$M1_invRecovery_Rate,
      invImmunityLoss_Rate = input$M1_invImmunityLoss_Rate,
      invBirthDeath_Rate = input$M1_invBirthDeath_Rate,
      Shedding_Rate_Unnormalised = input$M1_Shedding_Rate / input$M1_W,
      SheddingAsymptomatic_Modifier = input$M1_SheddingAsymptomatic_Modifier,
      invCholeraDecay_Rate = input$M1_invCholeraDecay_Rate
    )
    
    # Time window
    times <- seq(1, input$M1_days, by = 0.1)
    
    # Solve model and output to matrix
    out_matrix <- ode(
      y = state,
      #times = as.numeric(times - times[1]), # Time by dates
      times = times - times[1], # Time by number of days
      func = sirsc.ode,
      parms = parameters)
    
    # Convert output matrix to data frame for plotly; add N and Total compartments for monitoring
    out_df <- out_matrix |>
      data.frame() |>
      mutate(N = S + I_s + I_a + R) |> # Define Total compartment to monitor live population
      mutate(Total = S + I_s + I_a + R + CholeraDead) |> # Define Total compartment to monitor live population + cholera deaths
      mutate(Lambda = input$M1_ContamWaterCons_Rate * C / (input$M1_Cholera_ID50 + C)) # Define Lambda column to monitor force of infection
    
    # SIRSC model interactive time series
    out_plot_lambda <- plot_ly(data = out_df)
    out_plot_lambda <- out_plot_lambda |>
      add_trace(x = ~time, y = ~Lambda, type = 'scatter', mode = 'lines', name = 'Force of Infection') |>
      layout(xaxis = list(title = "Time (days)"),
             yaxis = list(title = "\u03BB (per day)"))
})

# Cholera concentration plot
output$M1_out_plot_cholera <- renderPlotly({

    # States
    state <- c(
      S = input$M1_initS,
      I_s = input$M1_initI_s,
      I_a = input$M1_initI_a,
      R = initR,
      C = input$M1_initC,
      CholeraDead = 0 # Define CholeraDead compartment to monitor cholera deaths
    )
    
    # Parameters
    parameters <- c(
      ContamWaterCons_Rate = input$M1_ContamWaterCons_Rate,
      Cholera_ID50 = input$M1_Cholera_ID50,
      Asymptomatic_Proportion = input$M1_Asymptomatic_Proportion,
      CholeraDeath_Rate = input$M1_CholeraDeath_Rate,
      invRecovery_Rate = input$M1_invRecovery_Rate,
      invImmunityLoss_Rate = input$M1_invImmunityLoss_Rate,
      invBirthDeath_Rate = input$M1_invBirthDeath_Rate,
      Shedding_Rate_Unnormalised = input$M1_Shedding_Rate / input$M1_W,
      SheddingAsymptomatic_Modifier = input$M1_SheddingAsymptomatic_Modifier,
      invCholeraDecay_Rate = input$M1_invCholeraDecay_Rate
    )
    
    # Time window
    times <- seq(1, input$M1_days, by = 0.1)
    
    # Solve model and output to matrix
    out_matrix <- ode(
      y = state,
      #times = as.numeric(times - times[1]), # Time by dates
      times = times - times[1], # Time by number of days
      func = sirsc.ode,
      parms = parameters)
    
    # Convert output matrix to data frame for plotly; add N and Total compartments for monitoring
    out_df <- out_matrix |>
      data.frame() |>
      mutate(N = S + I_s + I_a + R) |> # Define Total compartment to monitor live population
      mutate(Total = S + I_s + I_a + R + CholeraDead) |> # Define Total compartment to monitor live population + cholera deaths
      mutate(Lambda = input$M1_ContamWaterCons_Rate * C / (input$M1_Cholera_ID50 + C)) # Define Lambda column to monitor force of infection
    
    # SIRSC model interactive time series
    out_plot_cholera <- plot_ly(data = out_df)
    out_plot_cholera <- out_plot_cholera |>
      add_trace(x = ~time, y = ~C, type = 'scatter', mode = 'lines', name = 'Cholera Concentration in Reservoir',
                colors = "#67B7D1") |>
      layout(xaxis = list(title = "Time (days)"),
             yaxis = list(title = "[Bacteria] (cells / L)"))
})

############################ Module 2 server logic #############################

### Display selected distribution type ###
observeEvent(input$M2_alpha_disttype, {
  updateTabsetPanel(inputId = "M2_disttype", selected = input$M2_alpha_disttype)
})

### Draw samples with selected distribution type ###
M2_alpha_samples <- reactive({
  # Progress message 1
  withProgress(message = "Drawing samples of \u03b1...", value = 0.1, {
    incProgress(1)
    switch(input$M2_alpha_disttype,
           Uniform = runif(n = input$M2_alpha_n,
                           min = input$M2_unif_alpha_range[1],
                           max = input$M2_unif_alpha_range[2]),
           Normal = rnorm(n = input$M2_alpha_n,
                          mean = input$M2_norm_alpha_mean,
                          sd = input$M2_norm_alpha_sd),
           Beta = rbeta(n = input$M2_alpha_n,
                        shape1 = input$M2_beta_alpha_shape1,
                        shape2 = input$M2_beta_alpha_shape2),
           Binomial = rbinom(n = input$M2_alpha_n,
                             size = input$M2_binom_alpha_size,
                             prob = input$M2_binom_alpha_prob),
           "Negative Binomial" = rnbinom(n = input$M2_alpha_n,
                                         size = input$M2_nbinom_alpha_size,
                                         prob = input$M2_nbinom_alpha_prob),
           Poisson = rpois(n = input$M2_alpha_n,
                           lambda = input$M2_pois_alpha_lambda)
    )
  })
})

### Reactive SIRSC model time series plot ###
output$M2_out_plot <- renderPlotly({
  
  # Progress message 2
  withProgress(message = "Specifying initial states and parameters...", value = 0.1, {
    # States
    state <- c(
      S = input$M2_initS,
      I_s = input$M2_initI_s,
      I_a = input$M2_initI_a,
      R = initR,
      C = input$M2_initC,
      CholeraDead = 0 # Define CholeraDead compartment to monitor cholera deaths
    )
    
    # Time window
    times <- seq(1, input$M2_days, by = 1)
    
    # Create parameter list for every sample
    parameters_list <- lapply(M2_alpha_samples(), function(ContamWaterCons_Rate_sample) {
      c(
        ContamWaterCons_Rate = ContamWaterCons_Rate_sample,
        Cholera_ID50 = input$M2_Cholera_ID50,
        Asymptomatic_Proportion = input$M2_Asymptomatic_Proportion,
        CholeraDeath_Rate = input$M2_CholeraDeath_Rate,
        invRecovery_Rate = input$M2_invRecovery_Rate,
        invImmunityLoss_Rate = input$M2_invImmunityLoss_Rate,
        invBirthDeath_Rate = input$M2_invBirthDeath_Rate,
        Shedding_Rate_Unnormalised = input$M2_Shedding_Rate / input$M2_W,
        SheddingAsymptomatic_Modifier = input$M2_SheddingAsymptomatic_Modifier,
        invCholeraDecay_Rate = input$M2_invCholeraDecay_Rate
      )
    })
    
    # Define function to solve model for each set of parameters
    solve_model <- function(parameters) {
      ode(
        y = state,
        times = times - times[1],
        func = sirsc.ode,
        parms = parameters)
    }
    
    # Progress message 3
    incProgress(message = "Solving ordinary differential equations...",
                detail = HTML("Cholera is being transmitted \uD83E\uDDA0! This should take no longer than ~30 seconds \uD83D\uDE47"),
                amount = 0.2)
    
    # Solve model for all sampled parameters and output to list
    solutions_list <- lapply(parameters_list, solve_model)
    
    # Combine all results into one data frame
    combined_out_df <- do.call(rbind, lapply(seq_along(solutions_list), function(i) {
      out_df <- data.frame(solutions_list[[i]])
      out_df$sample <- i
      out_df$N <- out_df$S + out_df$I_s + out_df$I_a + out_df$R # Define Total compartment to monitor live population
      out_df$Total <-  out_df$S + out_df$I_s + out_df$I_a + out_df$R + out_df$CholeraDead # Define Total compartment to monitor live population + cholera deaths
      # out_df$Lambda <- out_df$ContamWaterCons_Rate_sample * out_df$C / (input$M2_Cholera_ID50 + C) # Define Lambda column to monitor force of infection
      out_df
    }))
    
    # Add NA breaks in data frame to prevent plotly from joining traces of different samples
    combined_out_df <- combined_out_df |>
      group_by(sample) |>
      reframe(across(everything(), ~ c(., NA)))
    
    # Progress message 4
    incProgress(message = 'Rendering plots...', detail = "", amount = 0.7)
    
    # Plot interactive time series
    out_plot_human <- plot_ly(data = combined_out_df, x = ~time, opacity = 0.5) |>
      add_trace(y = ~S, name = 'Susceptible (S) ', type = 'scatter', mode = 'lines',
                line = list(width = ifelse(input$M2_alpha_n %in% c(1, 2, 5, 10), 0.8,
                                           ifelse(input$M2_alpha_n == 100, 0.1, 0.05)))) |>
      add_trace(y = ~I_s, name = 'Symptomatic (I<sub>s</sub>)', type = 'scatter', mode = 'lines', 
                line = list(width = ifelse(input$M2_alpha_n %in% c(1, 2, 5, 10), 0.8,
                                           ifelse(input$M2_alpha_n == 100, 0.1, 0.05)))) |>
      add_trace(y = ~I_a, name = 'Asymptomatic (I<sub>a</sub>)', type = 'scatter', mode = 'lines', 
                line = list(width = ifelse(input$M2_alpha_n %in% c(1, 2, 5, 10), 0.8,
                                           ifelse(input$M2_alpha_n == 100, 0.1, 0.05)))) |>
      add_trace(y = ~R, name = 'Recovered (R)', type = 'scatter', mode = 'lines', 
                line = list(width = ifelse(input$M2_alpha_n %in% c(1, 2, 5, 10), 0.8,
                                           ifelse(input$M2_alpha_n == 100, 0.1, 0.05)))) |>
      add_trace(y = ~N, name = 'Total Humans Alive (N)', type = 'scatter', mode = 'lines', 
                line = list(width = ifelse(input$M2_alpha_n %in% c(1, 2, 5, 10), 0.8,
                                           ifelse(input$M2_alpha_n == 100, 0.1, 0.05)))) |>
      add_trace(y = ~CholeraDead, name = 'Cholera Deaths (D<sub>c</sub>)', type = 'scatter', mode = 'lines',
                line = list(width = ifelse(input$M2_alpha_n %in% c(1, 2, 5, 10), 0.8,
                                           ifelse(input$M2_alpha_n == 100, 0.1, 0.05))),
                visible = 'legendonly') |>
      add_trace(y = ~Total, name = 'Total Humans Alive + Cholera Deaths (N + D<sub>c</sub>)', type = 'scatter', mode = 'lines',
                line = list(width = ifelse(input$M2_alpha_n %in% c(1, 2, 5, 10), 0.8,
                                           ifelse(input$M2_alpha_n == 100, 0.1, 0.05))),
                visible = 'legendonly') |>
      layout(xaxis = list(title = "Time (days)"),
             yaxis = list(title = "Number of individuals (persons)"))
})})

### Sample alpha distribution ###
# Confirmation of sample distribution parameters selected
output$M2_out_alpha_selection <- renderUI(p(HTML(paste("You have chosen to sample <b>", input$M2_alpha_n, "</b> values of \u03b1 using a <b>", input$M2_alpha_disttype, "distribution</b>. Now, <b>", input$M2_alpha_n, "iterations </b> of the SIRSC model across <b>", input$M2_days, "days </b> are plotted below."))))

# Histogram to visualise sampled alpha values
output$M2_out_alpha_dist <- renderPlotly({
  ggplot_alpha <- ggplot(data = as.data.frame(M2_alpha_samples()),
                         mapping = aes(x = M2_alpha_samples()),
                         histnorm = "probability") +
    geom_histogram(mapping = aes(y = after_stat(ncount)), bins = 30, fill = "#67B7D1") +
    geom_density(mapping = aes(y = after_stat(ndensity)), color = "#007BC2") +
    geom_rug(sides="t", color = "#007BC2", alpha = 0.1) +
    labs(x = "\u03b1 samples", y = "Relative frequency") +
    theme_bw()
  out_plot_alpha <- ggplotly(ggplot_alpha)
})

############################ Module 3 server logic #############################

# Display selected distribution type
observeEvent(input$M3_alpha_disttype, {
  updateTabsetPanel(inputId = "M3_disttype_alpha", selected = input$M3_alpha_disttype)
})
observeEvent(input$M3_mu_c_disttype, {
  updateTabsetPanel(inputId = "M3_disttype_mu_c", selected = input$M3_mu_c_disttype)
})
observeEvent(input$M3_sampling_method, {
  updateTabsetPanel(inputId = "M3_sampling_method_show", selected = input$M3_sampling_method)
})

# Draw samples with selected distribution type as matrix
# !When upgrading code so alpha and mu_c aren't the only uncertain parameters, replace '2' with count of uncertain parameters!
parameter_samples <- reactive(switch(input$M3_sampling_method,
                                     "Random Search" = matrix(data = runif(n = input$M3_sample_n * 2, min = 0, max = 1),
                                                              nrow = input$M3_sample_n,
                                                              ncol = 2),
                                     "Brute force (Grid Search)" = expand.grid(seq(0, 1, length.out = input$M3_sample_n), seq(0, 1, length.out = input$M3_sample_n)),
                                     "Latin Hypercube Sampling" = randomLHS(n = input$M3_sample_n, k = 2)
))

### Reactive SIRSC model time series plot ###
output$M3_out_plot <- renderPlotly({
  
  # Progress message 1
  withProgress(message = "Specifying initial states and parameters...", value = 0.1, {
    
  # Convert matrix sampled values into data frame
  parameter_samples <- as.data.frame(parameter_samples())
  colnames(parameter_samples) <- c("alpha_samples", "mu_c_samples")
  
  # Apply appropriate inverse distribution to sampled values
  parameter_samples$alpha_samples <- switch(input$M3_alpha_disttype,
                                            Uniform = qunif(p = parameter_samples$alpha_samples,
                                                            min = input$M3_unif_alpha_range[1],
                                                            max = input$M3_unif_alpha_range[2]),
                                            Normal = qnorm(p = parameter_samples$alpha_samples,
                                                           mean = input$M3_norm_alpha_mean,
                                                           sd = input$M3_norm_alpha_sd),
                                            Beta = qbeta(p = parameter_samples$alpha_samples,
                                                         shape1 = input$M3_beta_alpha_shape1,
                                                         shape2 = input$M3_beta_alpha_shape2),
                                            Binomial = qbinom(p = parameter_samples$alpha_samples,
                                                              size = input$M3_binom_alpha_size,
                                                              prob = input$M3_binom_alpha_prob),
                                            
                                            "Negative Binomial" = qnbinom(p = parameter_samples$alpha_samples,
                                                                          size = input$M3_nbinom_alpha_size,
                                                                          prob = input$M3_nbinom_alpha_prob),
                                            Poisson = qpois(p = parameter_samples$alpha_samples,
                                                            lambda = input$M3_pois_alpha_lambda))
  parameter_samples$mu_c_samples <- switch(input$M3_mu_c_disttype,
                                           Uniform = qunif(p = parameter_samples$mu_c_samples,
                                                           min = input$M3_unif_mu_c_range[1],
                                                           max = input$M3_unif_mu_c_range[2]),
                                           Normal = qnorm(p = parameter_samples$mu_c_samples,
                                                          mean = input$M3_norm_mu_c_mean,
                                                          sd = input$M3_norm_mu_c_sd),
                                           Beta = qbeta(p = parameter_samples$mu_c_samples,
                                                        shape1 = input$M3_beta_mu_c_shape1,
                                                        shape2 = input$M3_beta_mu_c_shape2),
                                           Binomial = qbinom(p = parameter_samples$mu_c_samples,
                                                             size = input$M3_binom_mu_c_size,
                                                             prob = input$M3_binom_mu_c_prob),
                                           "Negative Binomial" = qnbinom(p = parameter_samples$alpha_samples,
                                                                         size = input$M3_nbinom_mu_c_size,
                                                                         prob = input$M3_nbinom_mu_c_prob),
                                           Poisson = qpois(p = parameter_samples$alpha_samples,
                                                           lambda = input$M3_pois_mu_c_lambda))
  
  # Create state list for every sample
  # !When upgrading code so initial states can be uncertain, use lapply/mapply function cf. creating parameter list!
  state_list <- c(S = input$M3_initS,
                  I_s = input$M3_initI_s,
                  I_a = input$M3_initI_a,
                  R = initR,
                  C = input$M3_initC,
                  CholeraDead = 0 # Define CholeraDead compartment to monitor cholera deaths
                  )
  # Time window (by number of days)
  times <- seq(1, input$M3_days, by = 1)
      
  # Create parameter multiple list for every sample of parameter combinations
    parameters_list <- mapply(function(ContamWaterCons_Rate_sample, CholeraDeath_Rate_sample) {
      c(ContamWaterCons_Rate = ContamWaterCons_Rate_sample,
        Cholera_ID50 = input$M3_Cholera_ID50,
        Asymptomatic_Proportion = input$M3_Asymptomatic_Proportion,
        CholeraDeath_Rate = CholeraDeath_Rate_sample,
        invRecovery_Rate = input$M3_invRecovery_Rate,
        invImmunityLoss_Rate = input$M3_invImmunityLoss_Rate,
        invBirthDeath_Rate = input$M3_invBirthDeath_Rate,
        Shedding_Rate_Unnormalised = input$M3_Shedding_Rate / input$M3_W,
        SheddingAsymptomatic_Modifier = input$M3_SheddingAsymptomatic_Modifier,
        invCholeraDecay_Rate = input$M3_invCholeraDecay_Rate)
      }, parameter_samples$alpha_samples, parameter_samples$mu_c_samples, SIMPLIFY = FALSE)

  # Define function to solve model for each set of parameters
  # !When upgrading code, add additional argument for state = state_list!
  solve_model <- function(parameters) {
    ode(y = state_list,
        times = times - times[1], # Time by number of days
        func = sirsc.ode,
        parms = parameters)
    }
      
  # Progress message 2
  incProgress(message = "Solving ordinary differential equations...",
              detail = HTML("Cholera is being transmitted \uD83E\uDDA0! This should take no longer than ~30 seconds \uD83D\uDE47, unless you're using brute force!"),
              amount = 0.2)
      
  # Solve model for all sampled parameters and output to list
# solutions_list <- mapply(solve_model, state_list, parameters_list, SIMPLIFY = FALSE) # !Use this line with upgraded code for uncerain states! 
  solutions_list <- lapply(parameters_list, solve_model)

  # Combine all results into one data frame
  combined_out_df <<- do.call(rbind, lapply(seq_along(solutions_list), function(i) {
    out_df <- data.frame(solutions_list[[i]])
    out_df$sample <- i
    out_df$N <- out_df$S + out_df$I_s + out_df$I_a + out_df$R # Define Total compartment to monitor live population
    out_df$Total <-  out_df$S + out_df$I_s + out_df$I_a + out_df$R + out_df$CholeraDead # Define Total compartment to monitor live population + cholera deaths
    # out_df$Lambda <- out_df$ContamWaterCons_Rate_sample * out_df$C / (input$M3_Cholera_ID50 + C) # Define Lambda column to monitor force of infection
    out_df
    }))

  # Add NA breaks in data frame to prevent plotly from joining traces of different samples
  combined_out_df <- combined_out_df |>
    group_by(sample) |>
    reframe(across(everything(), ~ c(., NA)))

  # Progress message 3
  incProgress(message = 'Rendering plots...', detail = "", amount = 0.7)

  # Plot interactive time series
  out_plot_time_series <- plot_ly(data = combined_out_df, x = ~time, opacity = 0.5) |>
    add_trace(y = ~S, name = 'Susceptible (S) ', type = 'scatter', mode = 'lines',
              line = list(width = ifelse(max(combined_out_df$sample) <= 10, 0.8,
                                         ifelse(max(combined_out_df$sample) <= 100, 0.1, 0.05)))) |>
    add_trace(y = ~I_s, name = 'Symptomatic (I<sub>s</sub>)', type = 'scatter', mode = 'lines', 
              line = list(width = ifelse(max(combined_out_df$sample) <= 10, 0.8,
                                         ifelse(max(combined_out_df$sample) <= 100, 0.1, 0.05)))) |>
    add_trace(y = ~I_a, name = 'Asymptomatic (I<sub>a</sub>)', type = 'scatter', mode = 'lines', 
              line = list(width = ifelse(max(combined_out_df$sample) <= 10, 0.8,
                                         ifelse(max(combined_out_df$sample) <= 100, 0.1, 0.05)))) |>
    add_trace(y = ~R, name = 'Recovered (R)', type = 'scatter', mode = 'lines', 
              line = list(width = ifelse(max(combined_out_df$sample) <= 10, 0.8,
                                         ifelse(max(combined_out_df$sample) <= 100, 0.1, 0.05)))) |>
    add_trace(y = ~N, name = 'Total Humans Alive (N)', type = 'scatter', mode = 'lines', 
              line = list(width = ifelse(max(combined_out_df$sample) <= 10, 0.8,
                                         ifelse(max(combined_out_df$sample) <= 100, 0.1, 0.05)))) |>
    add_trace(y = ~CholeraDead, name = 'Cholera Deaths (D<sub>c</sub>)', type = 'scatter', mode = 'lines',
              line = list(width = ifelse(max(combined_out_df$sample) <= 10, 0.8,
                                         ifelse(max(combined_out_df$sample) <= 100, 0.1, 0.05))),
              visible = 'legendonly') |>
    layout(xaxis = list(title = "Time (days)"),
           yaxis = list(title = "Number of individuals (persons)"))
})})

### Confirmation of sampling distributions and sampling method selected ###
output$M3_out_sampling_method_selection <- renderUI(p(HTML(paste("You have chosen to sample <b>",
                                                                 switch(input$M3_sampling_method,
                                                                        "Brute force (Grid Search)" = input$M3_sample_n^2,
                                                                        "Random Search" = input$M3_sample_n,
                                                                        "Latin Hypercube Sampling" = input$M3_sample_n),
                                                                 "</b> values of the parameter space. Now, <b>", 
                                                                 switch(input$M3_sampling_method,
                                                                        "Brute force (Grid Search)" = input$M3_sample_n^2,
                                                                        "Random Search" = input$M3_sample_n,
                                                                        "Latin Hypercube Sampling" = input$M3_sample_n),
                                                                 "iterations </b> of the SIRSC model across <b>", input$M3_days, "days </b> are plotted below."))))

### Grid search help text: Confirmation of number of combinations ###
output$M3_out_grid_search_samples <- renderUI(p(HTML(paste("With <b>2</b> uncertain parameters, there are",
                                                           input$M3_sample_n, "<sup>2</sup>", "= <b>", input$M3_sample_n^2,
                                                           "</b> combinations across the parameter space."))))
  
### Reactive sampling distribution pair plots ###
  
# 2D scatter to visualise sampled parameter space
output$M3_out_parameter_space <- renderPlotly({
  parameter_samples <- as.data.frame(parameter_samples())
  colnames(parameter_samples) <- c("alpha_samples", "mu_c_samples")
  
  # Apply inverse distribution to LHS values
  parameter_samples$alpha_samples <- switch(input$M3_alpha_disttype,
                                            Uniform = qunif(p = parameter_samples$alpha_samples,
                                                            min = input$M3_unif_alpha_range[1],
                                                            max = input$M3_unif_alpha_range[2]),
                                            Normal = qnorm(p = parameter_samples$alpha_samples,
                                                           mean = input$M3_norm_alpha_mean,
                                                           sd = input$M3_norm_alpha_sd),
                                            Beta = qbeta(p = parameter_samples$alpha_samples,
                                                         shape1 = input$M3_beta_alpha_shape1,
                                                         shape2 = input$M3_beta_alpha_shape2),
                                            Binomial = qbinom(p = parameter_samples$alpha_samples,
                                                              size = input$M3_binom_alpha_size,
                                                              prob = input$M3_binom_alpha_prob),
                                            "Negative Binomial" = qnbinom(p = parameter_samples$alpha_samples,
                                                                          size = input$M3_nbinom_alpha_size,
                                                                          prob = input$M3_nbinom_alpha_prob),
                                            Poisson = qpois(p = parameter_samples$alpha_samples,
                                                            lambda = input$M3_pois_alpha_lambda)
                                            )
  parameter_samples$mu_c_samples <- switch(input$M3_mu_c_disttype,
                                           Uniform = qunif(p = parameter_samples$mu_c_samples,
                                                           min = input$M3_unif_mu_c_range[1],
                                                           max = input$M3_unif_mu_c_range[2]),
                                           Normal = qnorm(p = parameter_samples$mu_c_samples,
                                                          mean = input$M3_norm_mu_c_mean,
                                                          sd = input$M3_norm_mu_c_sd),
                                           Beta = qbeta(p = parameter_samples$mu_c_samples,
                                                        shape1 = input$M3_beta_mu_c_shape1,
                                                        shape2 = input$M3_beta_mu_c_shape2),
                                           Binomial = qbinom(p = parameter_samples$mu_c_samples,
                                                             size = input$M3_binom_mu_c_size,
                                                             prob = input$M3_binom_mu_c_prob),
                                           "Negative Binomial" = qnbinom(p = parameter_samples$alpha_samples,
                                                                         size = input$M3_nbinom_mu_c_size,
                                                                         prob = input$M3_nbinom_mu_c_prob),
                                           Poisson = qpois(p = parameter_samples$alpha_samples,
                                                           lambda = input$M3_pois_mu_c_lambda)
                                           )
  plot_ly(data = parameter_samples, x = ~alpha_samples, y = ~mu_c_samples, colors = "#67B7D1") |>
    add_markers(marker = list(size = ifelse(max(combined_out_df$sample) <= 10, 3,
                                            ifelse(max(combined_out_df$sample) <= 100, 2, 1)))) |>
    layout(xaxis = list(title = "\u03b1",
                        range = list(-0.2, 1.2)),
           yaxis = list(title = "\u03bc<sub>c</sub>",
                        range = list(-0.2, 1.2)))
  })

#[UNUSED] Curve-wise plot
#  output$M3_out_curvewise_plot <- renderPlotly({
#    parameter_samples <- as.data.frame(parameter_samples())
#    colnames(parameter_samples) <- c("alpha_samples", "mu_c_samples", "initC_samples")
#    
#    # Apply inverse distribution to LHS values
#    parameter_samples$alpha_samples <- switch(input$M3_alpha_disttype,
#                                        Uniform = qunif(p = parameter_samples$alpha_samples,
#                                                        min = input$M3_unif_alpha_range[1],
#                                                        max = input$M3_unif_alpha_range[2]),
#                                        Normal = qnorm(p = parameter_samples$alpha_samples,
#                                                       mean = input$M3_norm_alpha_mean,
#                                                       sd = input$M3_norm_alpha_sd),
#                                        Beta = qbeta(p = parameter_samples$alpha_samples,
#                                                     shape1 = input$M3_beta_alpha_shape1,
#                                                     shape2 = input$M3_beta_alpha_shape2),
#                                        Binomial = qbinom(p = parameter_samples$alpha_samples,
#                                                          size = input$M3_binom_alpha_size,
#                                                          prob = input$M3_binom_alpha_prob),
#                                        "Negative Binomial" = qnbinom(p = parameter_samples$alpha_samples,
#                                                                      size = input$M3_nbinom_alpha_size,
#                                                                      prob = input$M3_nbinom_alpha_prob),
#                                        Poisson = qpois(p = parameter_samples$alpha_samples,
#                                                        lambda = input$M3_pois_alpha_lambda)
#    )
#    parameter_samples$mu_c_samples <- switch(input$M3_mu_c_disttype,
#                                       Uniform = qunif(p = parameter_samples$mu_c_samples,
#                                                       min = input$M3_unif_mu_c_range[1],
#                                                       max = input$M3_unif_mu_c_range[2]),
#                                       Normal = qnorm(p = parameter_samples$mu_c_samples,
#                                                      mean = input$M3_norm_mu_c_mean,
#                                                      sd = input$M3_norm_mu_c_sd),
#                                       Beta = qbeta(p = parameter_samples$mu_c_samples,
#                                                    shape1 = input$M3_beta_mu_c_shape1,
#                                                    shape2 = input$M3_beta_mu_c_shape2),
#                                       Binomial = qbinom(p = parameter_samples$mu_c_samples,
#                                                         size = input$M3_binom_mu_c_size,
#                                                         prob = input$M3_binom_mu_c_prob),
#                                       "Negative Binomial" = qnbinom(p = parameter_samples$alpha_samples,
#                                                                     size = input$M3_nbinom_mu_c_size,
#                                                                     prob = input$M3_nbinom_mu_c_prob),
#                                       Poisson = qpois(p = parameter_samples$alpha_samples,
#                                                       lambda = input$M3_pois_mu_c_lambda)
#    )
#    parameter_samples$initC_samples <- switch(input$M3_initC_disttype,
#                                        Uniform = qunif(p = parameter_samples$initC_samples,
#                                                        min = input$M3_unif_initC_range[1],
#                                                        max = input$M3_unif_initC_range[2]),
#                                        Normal = qnorm(p = parameter_samples$initC_samples,
#                                                       mean = input$M3_norm_initC_mean,
#                                                       sd = input$M3_norm_initC_sd),
#                                        Beta = qbeta(p = parameter_samples$initC_samples,
#                                                     shape1 = input$M3_beta_initC_shape1,
#                                                     shape2 = input$M3_beta_initC_shape2),
#                                        Binomial = qbinom(p = parameter_samples$initC_samples,
#                                                          size = input$M3_binom_initC_size,
#                                                          prob = input$M3_binom_initC_prob),
#                                        "Negative Binomial" = qnbinom(p = parameter_samples$alpha_samples,
#                                                                      size = input$M3_nbinom_initC_size,
#                                                                      prob = input$M3_nbinom_initC_prob),
#                                        Poisson = qpois(p = parameter_samples$alpha_samples,
#                                                        lambda = input$M3_pois_initC_lambda)
#    )
#    
#    curvewise_plot <- combined_out_df %>%
#      group_by(time) %>%
#      curve_interval(S, .width = c(.5, .8, .95)) %>%
#      ggplot(aes(x = time)) +
#      geom_line(aes(y = S)) +
#      geom_ribbon(aes(ymin = .lower, ymax = .upper))
#    curvewise_plot
#  })
}