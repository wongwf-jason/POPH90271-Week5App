# 2 UI

  ## 2.1 Slider sidebar
  #sidebar = sidebar(
  #  navset_underline(
  #    nav_panel("States",
  #              helpText("Choose the initial states of the population"),
  #              sliderInput(inputId = "M1_initS",
  #                          label = "\\( S(0) \\): Initial Susceptible",
  #                          min = 0, max = 10000, value = 1000),
  #              sliderInput(inputId = "M1_initI_s",
  #                          label = "\\( I_s(0) \\): Initial Symptomatic ",
  #                          min = 0, max = 10000, value = 0),
  #              sliderInput(inputId = "M1_initI_a",
  #                          label = "\\( I_a(0) \\): Initial Asymptomatic",
  #                          min = 0, max = 10000, value = 0),
  #              shinyWidgets::sliderTextInput(inputId = "M1_initC",
  #                                            label = "\\( C(0) \\): Initial Cholera Reservoir Concentration (bacteria / L)",
  #                                            choices = c(0, 1, 10, 10^2, 10^3, 10^4, 10^5, 10^6, 10^7), selected = 10^5, grid = T),
  #              sliderInput(inputId = "M1_W",
  #                          label = "\\( W(0) \\): Water reservoir volume per capita  (L per person)",
  #                          min = 1, max = 100, value = 15)),
  #    nav_panel("Parameters",
  #              helpText("Choose the values of the parameters"),
  #              sliderInput(inputId = "M1_ContamWaterCons_Rate",
  #                          label = "\\( \\alpha \\): Rate of contaminated water consumption (per day)",
  #                          min = 0, max = 2, value = 1, step = 0.01,
  #                          animate = animationOptions(interval = 150, loop = T)),
  #              shinyWidgets::sliderTextInput(inputId = "M1_Cholera_ID50",
  #                                            label = "\\( \\kappa \\): Concentration of cholera yielding 50% chance of infection (bacteria / L)",
  #                                            choices = c(0, 1, 10, 10^2, 10^3, 10^4, 10^5, 10^6, 10^7), selected = 10^5, grid = T),
  #              sliderInput(inputId = "M1_Asymptomatic_Proportion",
  #                          label = "\\( p \\): Proportion of infections that are asymptomatic",
  #                          min = 0, max = 1, value = 0.79, step = 0.01),
  #              sliderInput(inputId = "M1_CholeraDeath_Rate",
  #                          label = "\\( \\mu_c \\) Cholera-induced death rate (per day)",
  #                          min = 0, max = 1, value = 0.5, step = 0.01),
  #              shinyWidgets::sliderTextInput(inputId = "M1_invRecovery_Rate",
  #                                            label = "\\( D_\\text{inf} = \\frac{1}{\\gamma} \\): Duration of infectiousness (days)",
  #                                            choices = c(0.001, 1:21, 1000), selected = 5, grid = T),
  #              shinyWidgets::sliderTextInput(inputId = "M1_invImmunityLoss_Rate",
  #                                            label = "\\( D_\\text{imm} = \\frac{1}{\\omega} \\): Duration of immunity (years)",
  #                                            choices = c(0.001, seq(0.1, 2, 0.1), 1000), selected = 0.8, grid = T),
  #              sliderInput(inputId = "M1_invBirthDeath_Rate",
  #                          label = "\\( L_\\text{H} = \\frac{1}{\\mu} \\): Life expectancy of humans (years)",
  #                          min = 30, max = 100, value = 61, step = 1),
  #              shinyWidgets::sliderTextInput(inputId = "M1_Shedding_Rate",
  #                                            label = "\\( \\sigma \\): Rate of shedding per symptomatic individual (bacteria / day per person)",
  #                                            choices = c(0, 10^5, 10^6, 10^7, 10^8, 10^9, 10^10, 10^11), selected = 10^5, grid = T),
  #              shinyWidgets::sliderTextInput(inputId = "M1_SheddingAsymptomatic_Modifier",
  #                                            label = " \\( \\epsilon \\): Shedding modifier for asymptomatic individuals",
  #                                            choices = c(0, 10^-4, 10^-3, 10^-2, 0.1, 0.2, 0.4, 0.6, 0.8, 1), selected = 10^-3, grid = T),
  #              shinyWidgets::sliderTextInput(inputId = "M1_invCholeraDecay_Rate",
  #                                            label = "\\( L_\\text{C} = \\frac{1}{\\delta} \\): Life expectancy of cholera in the environment (days)",
  #                                            choices = c(0.001, 1:120, 1000), selected = 30, grid = T)
  #    )
  #  )
  #),
  ## 2.2 Choose time window
  helpText("Choose the number of days for the model"),
  numericInput(inputId = "M1_days",
               label = "Number of days",
               value = 30),
  ## 2.3 Model Visualisation
  plotlyOutput("out"),
  card_footer("Based off Andrews, J. R., & Basu, S. (2011). Transmission dynamics and control of cholera in Haiti: an epidemic model. Lancet, 377(9773), 1248–1255. https://doi.org/10.1016/S0140-6736(11)60273-0")
)


## 3.2 Model Function
#sirsc.ode <- function(t, state, parameters) {
#  with(as.list(c(state, parameters)), {
#    ### 3.2.1 Calculate the total population size
#    N <- S + I_s + I_a + R
#    W <- initW
#    ### 3.2.2 Calculate the net (instantaneous) change in each state variable
#    S_change <- 1/invBirthDeath_Rate/365 * N +
#      1/invImmunityLoss_Rate/365 * R -
#      ContamWaterCons_Rate * C / (Cholera_ID50 + C) * S -
#      1/invBirthDeath_Rate/365 * S
#    I_s_change <- (1 - Asymptomatic_Proportion) * ContamWaterCons_Rate * 
#      C / (Cholera_ID50 + C) * S -
#      (1/invRecovery_Rate + CholeraDeath_Rate + 1/invBirthDeath_Rate/365) * I_s
#    I_a_change <- Asymptomatic_Proportion * ContamWaterCons_Rate * 
#      C / (Cholera_ID50 + C) * S -
#      (1/invRecovery_Rate + 1/invBirthDeath_Rate/365) * I_a
#    R_change <- 1/invRecovery_Rate * (I_s + I_a) - 
#      (1/invImmunityLoss_Rate/365 + 1/invBirthDeath_Rate/365) * R
#    C_change <- Shedding_Rate_Unnormalised / N *
#      (I_s + SheddingAsymptomatic_Modifier * I_a) -
#      1/invCholeraDecay_Rate * C
#    CholeraDead_change <- CholeraDeath_Rate * I_s
    ### 3.2.3 Return net changes as list
#    return(list(
#      c(
#        S_change,
#        I_s_change,
#        I_a_change,
#        R_change,
#        C_change,
#        CholeraDead_change
#      )
#    ))
#  })
#}

## 3.3 Time Window (by date)
#start_date <- as.Date("2024-05-01")
#end_date <- as.Date("2024-05-31")
#times <- seq(start_date, end_date, by = 1)

# 4 Server logic
server <- function(input, output) {
  ## 4.1 Reactive SIRSC model
  output$M1_out_plot <- renderPlotly({
    ### 4.1.1 States (either based on reactive inputs or pre-defined)
    state <- c(
      S = input$M1_initS,
      I_s = input$M1_initI_s,
      I_a = input$M1_initI_a,
      R = initR,
      C = input$M1_initC,
      CholeraDead = 0 # Define CholeraDead compartment to monitor cholera deaths
    )
    ### 4.1.2 Parameters (based on reactive inputs)
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
    ### 4.1.3 Time window (by number of days)
    times <- seq(1, input$M1_days, by = 0.1)
    ### 4.1.4 Solve model and output to matrix
    out_matrix <- ode(
      y = state,
      #times = as.numeric(times - times[1]), # Time by dates
      times = times - times[1], # Time by number of days
      func = sirsc.ode,
      parms = parameters)
    ### 4.1.5 Convert output matrix to data frame for plotly; add N and Total compartments for monitoring
    out_df <- out_matrix |>
      data.frame() |>
      mutate(N = S + I_s + I_a + R) |> # Define Total compartment to monitor live population
      mutate(Total = S + I_s + I_a + R + CholeraDead) |> # Define Total compartment to monitor live population + cholera deaths
      mutate(Lambda = input$M1_ContamWaterCons_Rate * C / (input$M1_Cholera_ID50 + C)) # Define Lambda column to monitor force of infection
    ### 4.1.6 SIRSC model interactive time series
    #### 4.1.6.1 Human population plot
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
      layout(annotations = list(text = "Human Population",
                                x = 0.5 , y = 1.05, 
                                showarrow = F, 
                                xref='paper', yref='paper', xanchor = 'center'),
             xaxis = list(title = "Time (days)"),
             yaxis = list(title = "Numb <- <- er of individuals (persons)"))
    #### 4.1.6.2 Force of infection plot
    out_plot_lambda <- plot_ly(data = out_df)
    out_plot_lambda <- out_plot_lambda |>
      add_trace(x = ~time, y = ~Lambda, type = 'scatter', mode = 'lines', name = 'Force of Infection') |>
      layout(annotations = list(text = "Force of Infection",
                                x = 0.5 , y = 1.05, 
                                showarrow = F, 
                                xref='paper', yref='paper', xanchor = 'center'),
             xaxis = list(title = "Time (days)"),
             yaxis = list(title = "Force of Infection (per day)"))
    #### 4.1.6.3 Cholera concentration plot
    out_plot_cholera <- plot_ly(data = out_df)
    out_plot_cholera <- out_plot_cholera |>
      add_trace(x = ~time, y = ~C, type = 'scatter', mode = 'lines', name = 'Cholera Concentration in Reservoir') |>
      layout(annotations = list(text = "Cholera Concentration in Reservoir",
                                x = 0.5 , y = 1.05, 
                                showarrow = F, 
                                xref='paper', yref='paper', xanchor = 'center'),
             xaxis = list(title = "Time (days)"),
             yaxis = list(title = "Concentration of Bacteria (cells / L)"))
    #### 4.1.6.4 Combining plots
    out_plot_bottom <- subplot(out_plot_lambda, out_plot_cholera,
                               titleX = TRUE, titleY = TRUE, margin = 0.04)
    out_plot <- subplot(out_plot_human,
                        out_plot_bottom,
                        nrows = 2, titleX = TRUE, titleY = TRUE, margin = 0.08)
  })
}