# 1 Library loading
required_packages <- (c("dplyr",
                        "deSolve",
                        #"tidyverse",
                        "plotly",
                        "ggplot2",
                        "shiny",
                        "bslib",
                        "shinyWidgets"#,
                        #"future.apply"
))
for (pkg in required_packages) {
  if (!require(pkg, character.only = TRUE)) {
    install.packages(pkg)
    library(pkg, character.only = TRUE)
  }
}

## 1.1 Parallel computing
#plan(multisession)

## 1.2 Set seed for reproducibility
set.seed(1)

################################################################################

# 2 UI
## 2.A Dynamic UI elements
disttype_tabs <- tabsetPanel(id = "disttype",
                             type = "hidden",
                             tabPanel(title = "Uniform",
                                      helpText(HTML("The Uniform distribution is a simple, equiprobable probability distribution. <br> You'll need to specify a lower bound \\( a \\), and upper bound \\( b \\).")),
                                      sliderInput(inputId = "unif_alpha_range",
                                                  label = "Feasible range for \\( \\alpha \\) i.e. \\([a, b]\\)",
                                                  min = 0, max = 2, value = c(0.1, 0.5), step = 0.01),
                             ),
                             tabPanel(title = "Normal", 
                                      helpText(HTML("The Normal distribution is a well-known probability distribution. However, it is unbounded (your samples of \\( \\alpha \\) can take any real number!). <br> You'll need to specify a mean \\( \\mu \\) and standard deviation \\( \\sigma \\).")),
                                      sliderInput(inputId = "norm_alpha_mean",
                                                  label = "\\( \\mu \\)",
                                                  min = 0, max = 2, value = 1, step = 0.01),
                                      sliderInput(inputId = "norm_alpha_sd", 
                                                  label = "\\( \\sigma \\)",
                                                  min = 0, max = 2, value = 1, step = 0.01)
                             ),
                             tabPanel(title = "Beta",
                                      helpText(HTML("The Beta distribution is a probability distribution bounded between 0 and 1, so it is useful for estimating probabilities or proportions. <br> You'll need to specify two parameters which affect its shape, \\( \\alpha_{\\text{shape}} \\) and \\( \\beta_{\\text{shape}} \\).")),
                                      sliderInput(inputId = "beta_alpha_shape1",
                                                  label = "\\( \\alpha_{\\text{shape}} \\)",
                                                  min = 0, max = 30, value = 1, step = 0.01),
                                      sliderInput(inputId = "beta_alpha_shape2", 
                                                  label = "\\( \\beta_{\\text{shape}}\\)",
                                                  min = 0, max = 30, value = 1, step = 0.01)
                             ),
                             tabPanel(title = "Binomial", 
                                      helpText(HTML("The Binomial distribution is another well-known probability distribution. However, it is discrete (your samples of \\( \\alpha \\) can only take whole numbers!). It is bounded between \\( 0 \\) and \\( n \\). <br> You'll need to specify the number of trials \\( n \\) and probability of success \\( p \\).<br>")),
                                      sliderInput(inputId = "binom_alpha_size",
                                                  label = "\\( n \\)",
                                                  min = 0, max = 5, value = 3, step = 1),
                                      sliderInput(inputId = "binom_alpha_prob", 
                                                  label = "\\( p \\)",
                                                  min = 0, max = 1, value = 0.5, step = 0.01)
                             ),
                             tabPanel(title = "Negative Binomial", 
                                      helpText(HTML("The Negative Binomial distribution is another discrete probability distribution. It can give zero or any positive whole number. <br> You'll need to specify the number of successful trials \\( r \\) and probability of success \\( p \\).")),
                                      sliderInput(inputId = "nbinom_alpha_size",
                                                  label = "\\( r \\)",
                                                  min = 0, max = 5, value = 3, step = 1),
                                      sliderInput(inputId = "nbinom_alpha_prob", 
                                                  label = "\\( p \\)",
                                                  min = 0, max = 1, value = 0.5, step = 0.01)
                             ),
                             tabPanel(title = "Poisson", 
                                      helpText(HTML("The Poisson distribution is another discrete probability distribution. It can give zero or any positive whole number. <br> It is useful in that you only need to specify one parameter, a mean \\( \\lambda \\).")),
                                      sliderInput(inputId = "pois_alpha_lambda",
                                                  label = "\\( \\lambda \\)",
                                                  min = 0, max = 5, value = 3, step = 0.01),
                             )
)

## 2.B Visible UI elements
ui <- page_sidebar(
  withMathJax(),
  inverse = T,
  title = list("Module 2: Deterministic SIRSC Model of Cholera Transmission - Uncertainty Analysis (Single Parameter Sampling) (InDev)",
               ## 2.0 Close app button
               tags$button(id = "Exit_app", type = "button", class = "btn action-button", 
                           onclick = "setTimeout(function(){window.close();}, 0);",  # Close browser
                           icon("circle-xmark"), "Exit App")),
  ## 2.1 Slider sidebar
  sidebar = sidebar(
    navset_underline(
      nav_panel("States",
                helpText("Choose the initial states of the population."),
                sliderInput(inputId = "initS",
                            label = "\\( S(0) \\): Initial Susceptible",
                            min = 0, max = 10000, value = 1000),
                sliderInput(inputId = "initI_s",
                            label = "\\( I_s(0) \\): Initial Symptomatic",
                            min = 0, max = 10000, value = 0),
                sliderInput(inputId = "initI_a",
                            label = "\\( I_a(0) \\): Initial Asymptomatic",
                            min = 0, max = 10000, value = 0),
                shinyWidgets::sliderTextInput(inputId = "initC",
                                              label = "\\( C(0) \\): Initial Cholera Reservoir Concentration (bacteria / L)",
                                              choices = c(0, 1, 10, 10^2, 10^3, 10^4, 10^5, 10^6, 10^7), selected = 10^5, grid = T),
                sliderInput(inputId = "W",
                            label = "\\( W(0) \\): Water reservoir volume per capita  (L per person)",
                            min = 1, max = 100, value = 15)),
      nav_panel("Parameters",
                tags$div("We now sample \\( \\alpha \\) using the sampling distribution set on the right."),
                helpText("Choose the values of the other parameters."),
                shinyWidgets::sliderTextInput(inputId = "Cholera_ID50",
                                              label = "\\( \\kappa \\): Concentration of cholera yielding 50% chance of infection (bacteria / L)",
                                              choices = c(0, 1, 10, 10^2, 10^3, 10^4, 10^5, 10^6, 10^7), selected = 10^5, grid = T),
                sliderInput(inputId = "Asymptomatic_Proportion",
                            label = "\\( p \\): Proportion of infections that are asymptomatic",
                            min = 0, max = 1, value = 0.79, step = 0.01),
                sliderInput(inputId = "CholeraDeath_Rate",
                            label = "\\( \\mu_c \\) Cholera-induced death rate (per day)",
                            min = 0, max = 1, value = 0.5, step = 0.01),
                shinyWidgets::sliderTextInput(inputId = "invRecovery_Rate",
                                              label = "\\( D_\\text{inf} = \\frac{1}{\\gamma} \\): Duration of infectiousness (days)",
                                              choices = c(0.001, 1:21, 1000), selected = 5, grid = T),
                shinyWidgets::sliderTextInput(inputId = "invImmunityLoss_Rate",
                                              label = "\\( D_\\text{imm} = \\frac{1}{\\omega} \\): Duration of immunity (years)",
                                              choices = c(0.001, seq(0.1, 2, 0.1), 1000), selected = 0.8, grid = T),
                sliderInput(inputId = "invBirthDeath_Rate",
                            label = "\\( L_\\text{H} = \\frac{1}{\\mu} \\): Life expectancy of humans (years)",
                            min = 30, max = 100, value = 61, step = 1),
                shinyWidgets::sliderTextInput(inputId = "Shedding_Rate",
                                              label = "\\( \\sigma \\): Rate of shedding per symptomatic individual (bacteria / day per person)",
                                              choices = c(0, 10^5, 10^6, 10^7, 10^8, 10^9, 10^10, 10^11), selected = 10^5, grid = T),
                shinyWidgets::sliderTextInput(inputId = "SheddingAsymptomatic_Modifier",
                                              label = " \\( \\epsilon \\): Shedding modifier for asymptomatic individuals",
                                              choices = c(0, 10^-4, 10^-3, 10^-2, 0.1, 0.2, 0.4, 0.6, 0.8, 1), selected = 10^-3, grid = T),
                shinyWidgets::sliderTextInput(inputId = "invCholeraDecay_Rate",
                                              label = "\\( L_\\text{C} = \\frac{1}{\\delta} \\): Life expectancy of cholera in the environment (days)",
                                              choices = c(0.001, 1:120, 1000), selected = 30, grid = T)
      )
    )
  ),
  
  ## 2.2 Top-row cards
  layout_columns(
    ## 2.A Choosing sampling distribution type, sample size and time window
    card(card_header("Distribution type, sample size and time window"),
         selectInput(inputId = "alpha_disttype",
                     label = "Probability distribution",
                     choices = c("Uniform", "Normal", "Beta", "Binomial", "Negative Binomial", "Poisson")),
         shinyWidgets::sliderTextInput(inputId = "alpha_n",
                                       label = "Number of samples of \\( \\alpha \\)",
                                       choices = c(1, 2, 5, 10, 100, 500, 1000), selected = 10, grid = T),
         shinyWidgets::sliderTextInput(inputId = "days",
                                       label = "Number of days the model will run",
                                       choices = c(10, 30, 60, 120, 365), selected = 30, grid = T)
    ),
    ## 2.B Choosing sampling distribution 
    card(card_header("Sampling distribution parameters"),
         disttype_tabs
    ),
    ## 2.C Visualising sample distribution
    card(card_header("Samples of \\( \\alpha \\) you've taken"),
         plotlyOutput("out_alpha_dist")
    )
  ),
  ## 2.3 Bottom-row cards
  card(
    ### 2.3.2 Summary of selection
    uiOutput("out_alpha_selection"),
    ### 2.3.3 Model Visualisation
    plotlyOutput("out_plot")
  ),
  ### 2.4 Footer
  card_footer("Based off Andrews, J. R., & Basu, S. (2011). Transmission dynamics and control of cholera in Haiti: an epidemic model. Lancet, 377(9773), 1248–1255. https://doi.org/10.1016/S0140-6736(11)60273-0")
)

################################################################################

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

################################################################################

# 4 Server logic
server <- function(input, output) {
  ## 4.X Close app and stop Shiny
  observe({if (input$Exit_app > 0) stopApp()})
  
  ## 4.A Display selected distribution type
  observeEvent(input$alpha_disttype, {
    updateTabsetPanel(inputId = "disttype", selected = input$alpha_disttype)
  })
  
  ## 4.B Draw samples with selected distribution type
  alpha_samples <- reactive({
    #### 4.P.1 Progress message 1
    withProgress(message = "Drawing samples of \u03b1...", value = 0.1, {
      incProgress(1)
      switch(input$alpha_disttype,
             Uniform = runif(n = input$alpha_n,
                             min = input$unif_alpha_range[1],
                             max = input$unif_alpha_range[2]),
             Normal = rnorm(n = input$alpha_n,
                            mean = input$norm_alpha_mean,
                            sd = input$norm_alpha_sd),
             Beta = rbeta(n = input$alpha_n,
                          shape1 = input$beta_alpha_shape1,
                          shape2 = input$beta_alpha_shape2),
             Binomial = rbinom(n = input$alpha_n,
                               size = input$binom_alpha_size,
                               prob = input$binom_alpha_prob),
             "Negative Binomial" = rnbinom(n = input$alpha_n,
                                           size = input$nbinom_alpha_size,
                                           prob = input$nbinom_alpha_prob),
             Poisson = rpois(n = input$alpha_n,
                             lambda = input$pois_alpha_lambda)
      )
    })
  })
  
  ## 4.1 Reactive SIRSC model
  output$out_plot <- renderPlotly({
    ### 4.P.2 Progress message 2
    withProgress(message = "Specifying initial states and parameters...", value = 0.1, {
      ### 4.1.1 States (either based on reactive inputs or pre-defined)
      state <- c(
        S = input$initS,
        I_s = input$initI_s,
        I_a = input$initI_a,
        R = initR,
        C = input$initC,
        CholeraDead = 0 # Define CholeraDead compartment to monitor cholera deaths
      )
      
      ### 4.1.2 Time window (by number of days)
      times <- seq(1, input$days, by = 1)
      
      ### 4.1.3V Create parameter list for every sample (VECTORISED)
      parameters_list <- lapply(alpha_samples(), function(ContamWaterCons_Rate_sample) {
        c(
          ContamWaterCons_Rate = ContamWaterCons_Rate_sample,
          Cholera_ID50 = input$Cholera_ID50,
          Asymptomatic_Proportion = input$Asymptomatic_Proportion,
          CholeraDeath_Rate = input$CholeraDeath_Rate,
          invRecovery_Rate = input$invRecovery_Rate,
          invImmunityLoss_Rate = input$invImmunityLoss_Rate,
          invBirthDeath_Rate = input$invBirthDeath_Rate,
          Shedding_Rate_Unnormalised = input$Shedding_Rate / input$W,
          SheddingAsymptomatic_Modifier = input$SheddingAsymptomatic_Modifier,
          invCholeraDecay_Rate = input$invCholeraDecay_Rate
        )
      })
      
      ### 4.1.4V Define function to solve model for each set of parameters
      solve_model <- function(parameters) {
        ode(
          y = state,
          #times = as.numeric(times - times[1]), # Time by dates
          times = times - times[1], # Time by number of days
          func = sirsc.ode,
          parms = parameters)
      }
      
      ### 4.P.2 Progress message 3
      incProgress(message = "Solving ordinary differential equations...", detail = HTML("Cholera is being transmitted \uD83E\uDDA0! This should take no longer than ~30 seconds \uD83D\uDE47."), amount = 0.2)
      
      ### 4.1.5V Solve model for all sampled parameters and output to list (VECTORISED)
      solutions_list <- lapply(parameters_list, solve_model)
      
      ### 4.1.6V Combine all results into one data frame (VECTORISED)
      combined_out_df <- do.call(rbind, lapply(seq_along(solutions_list), function(i) {
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
                  line = list(width = ifelse(input$alpha_n %in% c(1, 2, 5, 10), 0.8,
                                             ifelse(input$alpha_n == 100, 0.1, 0.05)))) |>
        add_trace(y = ~I_s, name = 'Symptomatic (I<sub>s</sub>)', type = 'scatter', mode = 'lines', 
                  line = list(width = ifelse(input$alpha_n %in% c(1, 2, 5, 10), 0.8,
                                             ifelse(input$alpha_n == 100, 0.1, 0.05)))) |>
        add_trace(y = ~I_a, name = 'Asymptomatic (I<sub>a</sub>)', type = 'scatter', mode = 'lines', 
                  line = list(width = ifelse(input$alpha_n %in% c(1, 2, 5, 10), 0.8,
                                             ifelse(input$alpha_n == 100, 0.1, 0.05)))) |>
        add_trace(y = ~R, name = 'Recovered (R)', type = 'scatter', mode = 'lines', 
                  line = list(width = ifelse(input$alpha_n %in% c(1, 2, 5, 10), 0.8,
                                             ifelse(input$alpha_n == 100, 0.1, 0.05)))) |>
        add_trace(y = ~N, name = 'Total Humans Alive (N)', type = 'scatter', mode = 'lines', 
                  line = list(width = ifelse(input$alpha_n %in% c(1, 2, 5, 10), 0.8,
                                             ifelse(input$alpha_n == 100, 0.1, 0.05)))) |>
        add_trace(y = ~CholeraDead, name = 'Cholera Deaths (D<sub>c</sub>)', type = 'scatter', mode = 'lines',
                  line = list(width = ifelse(input$alpha_n %in% c(1, 2, 5, 10), 0.8,
                                             ifelse(input$alpha_n == 100, 0.1, 0.05))),
                  visible = 'legendonly') |>
        add_trace(y = ~Total, name = 'Total Humans Alive + Cholera Deaths (N + D<sub>c</sub>)', type = 'scatter', mode = 'lines',
                  line = list(width = ifelse(input$alpha_n %in% c(1, 2, 5, 10), 0.8,
                                             ifelse(input$alpha_n == 100, 0.1, 0.05))),
                  visible = 'legendonly') |>
        layout(#annotations = list(text = "Human Population",
          #x = 0.5 , y = 1.05, 
          #showarrow = F, 
          #xref='paper', yref='paper', xanchor = 'center'),
          xaxis = list(title = "Time (days)"),
          yaxis = list(title = "Number of individuals (persons)"))
      #### 4.1.7.2 Cholera concentration plot
      #out_plot_cholera <- plot_ly(data = combined_out_df) |>
      #  add_trace(x = ~time, y = ~C, type = 'scatter', mode = 'lines', name = 'Cholera Concentration in Reservoir', line = list(width = 0.1)) |>
      #  layout(annotations = list(text = "Cholera Concentration in Reservoir",
      #                            x = 0.5 , y = 1.05, 
      #                            showarrow = F, 
      #                            xref='paper', yref='paper', xanchor = 'center'),
      #         xaxis = list(title = "Time (days)"),
      #         yaxis = list(title = "Concentration of Bacteria (cells / L)"))
      #### 4.1.7.3 Combining plots
      # out_plot <- subplot(out_plot_human,
      #                    out_plot_cholera,
      #                    nrows = 2, titleX = TRUE, titleY = TRUE, margin = 0.2)
    })})
  
  ## 4.2 Sample alpha distribution
  ### 4.2.1 Confirmation of sample distribution parameters selected
  output$out_alpha_selection <- renderUI(p(HTML(paste("You have chosen to sample <b>", input$alpha_n, "</b> values of \u03b1 using a <b>", input$alpha_disttype, "distribution</b>. Now, <b>", input$alpha_n, "iterations </b> of the SIRSC model across <b>", input$days, "days </b> are plotted below."))))
  ### 4.2.2 Histogram to visualise sampled alpha values
  
  output$out_alpha_dist <- renderPlotly({
    ggplot_alpha <- ggplot(data = as.data.frame(alpha_samples()),
                           mapping = aes(x = alpha_samples()),
                           histnorm = "probability") +
      geom_histogram(mapping = aes(y = after_stat(ncount)), bins = 30, fill = "#67B7D1") +
      geom_density(mapping = aes(y = after_stat(ndensity)), color = "#007BC2") +
      geom_rug(sides="t", color = "#007BC2", alpha = 0.1) +
      labs(x = "\u03b1 samples", y = "Relative frequency") +
      theme_bw()
    out_plot_alpha <- ggplotly(ggplot_alpha)
  })
}
################################################################################

# 5 App Execution
runApp(list(ui = ui, server = server), launch.browser = TRUE)