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

## Seed for reproducability
set.seed(1)

################################################################################
# 2 UI
## 2.A Dynamic UI elements
disttype_tabs_alpha <- tabsetPanel(id = "disttype_alpha",
                                   type = "hidden",
                                   tabPanel(title = "Uniform",
                                            sliderInput(inputId = "unif_alpha_range",
                                                        label = "Feasible range for \\( \\alpha \\) i.e. \\([a, b]\\)",
                                                        min = 0, max = 2, value = c(0.1, 0.5), step = 0.01)
                                   ),
                                   tabPanel(title = "Normal", 
                                            sliderInput(inputId = "norm_alpha_mean",
                                                        label = "\\( \\mu \\)",
                                                        min = 0, max = 2, value = 1, step = 0.01),
                                            sliderInput(inputId = "norm_alpha_sd", 
                                                        label = "\\( \\sigma \\)",
                                                        min = 0, max = 2, value = 1, step = 0.01)
                                   ),
                                   tabPanel(title = "Beta",
                                            sliderInput(inputId = "beta_alpha_shape1",
                                                        label = "\\( \\alpha_{\\text{shape}} \\)",
                                                        min = 0, max = 30, value = 1, step = 0.01),
                                            sliderInput(inputId = "beta_alpha_shape2", 
                                                        label = "\\( \\beta_{\\text{shape}}\\)",
                                                        min = 0, max = 30, value = 1, step = 0.01)
                                   ),
                                   tabPanel(title = "Binomial", 
                                            sliderInput(inputId = "binom_alpha_size",
                                                        label = "\\( n \\)",
                                                        min = 0, max = 5, value = 3, step = 1),
                                            sliderInput(inputId = "binom_alpha_prob", 
                                                        label = "\\( p \\)",
                                                        min = 0, max = 1, value = 0.5, step = 0.01)
                                   ),
                                   tabPanel(title = "Negative Binomial", 
                                            sliderInput(inputId = "nbinom_alpha_size",
                                                        label = "\\( r \\)",
                                                        min = 0, max = 5, value = 3, step = 1),
                                            sliderInput(inputId = "nbinom_alpha_prob", 
                                                        label = "\\( p \\)",
                                                        min = 0, max = 1, value = 0.5, step = 0.01)
                                   ),
                                   tabPanel(title = "Poisson", 
                                            sliderInput(inputId = "pois_alpha_lambda",
                                                        label = "\\( \\lambda \\)",
                                                        min = 0, max = 5, value = 3, step = 0.01),
                                   )
)

disttype_tabs_mu_c <- tabsetPanel(id = "disttype_mu_c",
                                  type = "hidden",
                                  tabPanel(title = "Uniform",
                                           sliderInput(inputId = "unif_mu_c_range",
                                                       label = "Feasible range for \\( \\mu_c \\) i.e. \\([a, b]\\)",
                                                       min = 0, max = 1, value = c(0.1, 0.5), step = 0.01)
                                  ),
                                  tabPanel(title = "Normal", 
                                           sliderInput(inputId = "norm_mu_c_mean",
                                                       label = "\\( \\mu \\)",
                                                       min = 0, max = 1, value = 1, step = 0.01),
                                           sliderInput(inputId = "norm_mu_c_sd", 
                                                       label = "\\( \\sigma \\)",
                                                       min = 0, max = 2, value = 1, step = 0.01)
                                  ),
                                  tabPanel(title = "Beta",
                                           sliderInput(inputId = "beta_mu_c_shape1",
                                                       label = "\\( \\alpha_{\\text{shape}} \\)",
                                                       min = 0, max = 30, value = 1, step = 0.01),
                                           sliderInput(inputId = "beta_mu_c_shape2", 
                                                       label = "\\( \\beta_{\\text{shape}}\\)",
                                                       min = 0, max = 30, value = 1, step = 0.01)
                                  ),
                                  tabPanel(title = "Binomial", 
                                           sliderInput(inputId = "binom_mu_c_size",
                                                       label = "\\( n \\)",
                                                       min = 0, max = 5, value = 3, step = 1),
                                           sliderInput(inputId = "binom_mu_c_prob", 
                                                       label = "\\( p \\)",
                                                       min = 0, max = 1, value = 0.5, step = 0.01)
                                  ),
                                  tabPanel(title = "Negative Binomial", 
                                           sliderInput(inputId = "nbinom_mu_c_size",
                                                       label = "\\( r \\)",
                                                       min = 0, max = 5, value = 3, step = 1),
                                           sliderInput(inputId = "nbinom_mu_c_prob", 
                                                       label = "\\( p \\)",
                                                       min = 0, max = 1, value = 0.5, step = 0.01)
                                  ),
                                  tabPanel(title = "Poisson", 
                                           sliderInput(inputId = "pois_mu_c_lambda",
                                                       label = "\\( \\lambda \\)",
                                                       min = 0, max = 5, value = 3, step = 0.01),
                                  )
)

disttype_tabs_initC <- tabsetPanel(id = "disttype_initC",
                                   type = "hidden",
                                   tabPanel(title = "Uniform",
                                            sliderInput(inputId = "unif_initC_range",
                                                        label = "Feasible range for \\( C(0) \\) i.e. \\([a, b]\\)",
                                                        min = 10^4, max = 10^6, value = c(10^5, 2*10^5), step = 10^4)
                                   ),
                                   tabPanel(title = "Normal", 
                                            sliderInput(inputId = "norm_initC_mean",
                                                        label = "\\( \\mu \\)",
                                                        min = 0, max = 10^6, value = 10^5, step = 10^4),
                                            sliderInput(inputId = "norm_initC_sd", 
                                                        label = "\\( \\sigma \\)",
                                                        min = 0, max = 10^6, value = 1, step = 10^3)
                                   ),
                                   tabPanel(title = "Beta",
                                            sliderInput(inputId = "beta_initC_shape1",
                                                        label = "\\( \\alpha_{\\text{shape}} \\)",
                                                        min = 0, max = 30, value = 1, step = 0.01),
                                            sliderInput(inputId = "beta_initC_shape2", 
                                                        label = "\\( \\beta_{\\text{shape}}\\)",
                                                        min = 0, max = 30, value = 1, step = 0.01)
                                   ),
                                   tabPanel(title = "Binomial", 
                                            sliderInput(inputId = "binom_initC_size",
                                                        label = "\\( n \\)",
                                                        min = 0, max = 10^6, value = 10^5, step = 10^4),
                                            sliderInput(inputId = "binom_initC_prob", 
                                                        label = "\\( p \\)",
                                                        min = 0, max = 1, value = 0.5, step = 0.01)
                                   ),
                                   tabPanel(title = "Negative Binomial", 
                                            sliderInput(inputId = "nbinom_initC_size",
                                                        label = "\\( r \\)",
                                                        min = 0, max = 10^6, value = 10^5, step = 10^4),
                                            sliderInput(inputId = "nbinom_initC_prob", 
                                                        label = "\\( p \\)",
                                                        min = 0, max = 1, value = 0.5, step = 0.01)
                                   ),
                                   tabPanel(title = "Poisson", 
                                            sliderInput(inputId = "pois_initC_lambda",
                                                        label = "\\( \\lambda \\)",
                                                        min = 0, max = 10^6, value = 10^5, step = 10^4),
                                   )
)
# Temporary values
alpha_n <- 100 ### IS THIS NEEDED ANYMORE?

## 2.B Visible UI elements
addResourcePath("prefix", "www") # Allow UI images to be stored in 'www/' folder
page_sidebar(
  withMathJax(),
  inverse = T,
  title = list(tags$img(src = "prefix/UoM_Logo.png", height = 50, width = 50),
               "Module 3: Deterministic SIRSC Model of Cholera Transmission - Uncertainty Analysis (Multiple Parameter Sampling) (InDev)"),
  ## 2.1 Slider sidebar
  sidebar = sidebar(
    navset_underline(
      nav_panel("States",
                helpText("Select whether to sample uncertain initial states or manually choose the their values."),
                sliderInput(inputId = "initS",
                            label = "\\( S(0) \\): Initial Susceptible",
                            min = 0, max = 10000, value = 1000),
                sliderInput(inputId = "initI_s",
                            label = "\\( I_s(0) \\): Initial Symptomatic ",
                            min = 0, max = 10000, value = 0),
                sliderInput(inputId = "initI_a",
                            label = "\\( I_a(0) \\): Initial Asymptomatic",
                            min = 0, max = 10000, value = 0),
                checkboxInput(inputId = "uncertain_initC",
                              label = "Uncertain \\( C(0) \\)?", value = TRUE),
                #shinyWidgets::sliderTextInput(inputId = "initC",
                #             label = "\\( C(0) \\): Initial Cholera Reservoir Concentration (bacteria / L)",
                #             choices = c(0, 1, 10, 10^2, 10^3, 10^4, 10^5, 10^6, 10^7), selected = 10^5, grid = T),
                sliderInput(inputId = "W",
                            label = "\\( W(0) \\): Water reservoir volume per capita  (L per person)",
                            min = 1, max = 100, value = 15)),
      nav_panel("Parameters",
                helpText("Select whether to sample uncertain parameters or manually choose their values."),
                checkboxInput(inputId = "uncertain_alpha",
                              label = "Uncertain \\( \\alpha \\)?", value = TRUE),
                shinyWidgets::sliderTextInput(inputId = "Cholera_ID50",
                                              label = "\\( \\kappa \\): Concentration of cholera yielding 50% chance of infection (bacteria / L)",
                                              choices = c(0, 1, 10, 10^2, 10^3, 10^4, 10^5, 10^6, 10^7), selected = 10^5, grid = T),
                sliderInput(inputId = "Asymptomatic_Proportion",
                            label = "\\( p \\): Proportion of infections that are asymptomatic",
                            min = 0, max = 1, value = 0.79, step = 0.01),
                checkboxInput(inputId = "uncertain_mu_c",
                              label = "Uncertain \\( \\mu_c \\)?", value = TRUE),
                #sliderInput(inputId = "CholeraDeath_Rate",
                #             label = "\\( \\mu_c \\) Cholera-induced death rate (per day)",
                #             min = 0, max = 1, value = 0.5, step = 0.01),
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
    card(card_header("Sampling distribution"),
         selectInput(inputId = "alpha_disttype",
                     label = "Probability distribution type for \\( \\alpha \\)",
                     choices = c("Uniform", "Normal", "Beta", "Binomial", "Negative Binomial", "Poisson")),
         disttype_tabs_alpha,
         selectInput(inputId = "mu_c_disttype",
                     label = "Probability distribution type for \\( \\mu_c \\)",
                     choices = c("Uniform", "Normal", "Beta", "Binomial", "Negative Binomial", "Poisson")),
         disttype_tabs_mu_c,
         selectInput(inputId = "initC_disttype",
                     label = "Probability distribution type \\( C(0) \\)",
                     choices = c("Uniform", "Normal", "Beta", "Binomial", "Negative Binomial", "Poisson")),
         disttype_tabs_initC
    ),
    ## 2.B Choosing sampling distribution 
    card(card_header("Sampling method"),
         selectInput(inputId = "sampling_method",
                     label = "Sampling method type",
                     choices = c("None (Brute force / Combinatorial)", "Random Search", "Grid Search",
                                 "Latin Hypercube Sampling", "Sobol Sequence"), selected = "Latin Hypercube Sampling"),
         shinyWidgets::sliderTextInput(inputId = "sample_n",
                                       label = "Number of samples to take from parameter space",
                                       choices = c(1, 2, 5, 10, 100, 500, 1000), selected = 10, grid = T),
         shinyWidgets::sliderTextInput(inputId = "days",
                                       label = "Number of days the model will run",
                                       choices = c(10, 30, 60, 120, 365), selected = 30, grid = T)
         
    ),
    ## 2.C Visualising sample distribution
    card(card_header("Samples in parameter space"),
         plotlyOutput("out_parameter_space")
    )
  ),
  ## 2.3 Bottom-row cards
  layout_columns(
    card(
      ### 2.3.2 Summary of selection
      uiOutput("out_sampling_method_selection"),
      ### 2.3.3 Model Visualisation
      plotlyOutput("out_plot"),
    ),
    #card(plotlyOutput("out_curvewise_plot"))
  ),
  ### 2.4 Footer
  card_footer("Based off Andrews, J. R., & Basu, S. (2011). Transmission dynamics and control of cholera in Haiti: an epidemic model. Lancet, 377(9773), 1248–1255. https://doi.org/10.1016/S0140-6736(11)60273-0")
)