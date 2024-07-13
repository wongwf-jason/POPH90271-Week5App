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

################################################################################
##################################### UI #######################################
################################################################################

####################### Dynamic UI elements for module 2 #######################

# Sampling distribution parameters for alpha
M2_disttype_tabs <- tabsetPanel(id = "M2_disttype",
                                type = "hidden",
                                tabPanel(title = "Uniform",
                                         helpText(HTML("The <i>Uniform distribution</i> is a simple, <b>equiprobable</b> probability distribution. <br> You'll need to specify a lower bound \\( a \\), and upper bound \\( b \\).")),
                                         sliderInput(inputId = "M2_unif_alpha_range",
                                                     label = "Feasible range for \\( \\alpha \\) i.e. \\([a, b]\\)",
                                                     min = 0, max = 2, value = c(0.1, 0.5), step = 0.01)),
                                tabPanel(title = "Normal", 
                                         helpText(HTML("The <i>Normal distribution</i> is a well-known probability distribution. However, it is <b>unbounded</b> (your samples of \\( \\alpha \\) can take any real number!). <br> You'll need to specify a mean \\( \\mu \\) and standard deviation \\( \\sigma \\).")),
                                         sliderInput(inputId = "M2_norm_alpha_mean",
                                                     label = "\\( \\mu \\)",
                                                     min = 0, max = 2, value = 1, step = 0.01),
                                         sliderInput(inputId = "M2_norm_alpha_sd", 
                                                     label = "\\( \\sigma \\)",
                                                     min = 0, max = 2, value = 1, step = 0.01)),
                                tabPanel(title = "Beta",
                                         helpText(HTML("The <i>Beta distribution</i> is a probability distribution <b>bounded between 0 and 1</b>, so it is useful for estimating <b>probabilities</b> or <b>proportions</b>. <br> You'll need to specify two parameters which affect its shape, \\( \\alpha_{\\text{shape}} \\) and \\( \\beta_{\\text{shape}} \\).")),
                                         sliderInput(inputId = "M2_beta_alpha_shape1",
                                                     label = "\\( \\alpha_{\\text{shape}} \\)",
                                                     min = 0, max = 30, value = 1, step = 0.01),
                                         sliderInput(inputId = "M2_beta_alpha_shape2", 
                                                     label = "\\( \\beta_{\\text{shape}}\\)",
                                                     min = 0, max = 30, value = 1, step = 0.01)),
                                tabPanel(title = "Binomial", 
                                         helpText(HTML("The <i>Binomial distribution</i> is another well-known probability distribution. However, it is <b>discrete</b> (your samples of \\( \\alpha \\) can only take whole numbers!). It is <b>bounded between \\( 0 \\) and \\( n \\)</b>. <br> You'll need to specify the number of trials \\( n \\) and probability of success \\( p \\).<br>")),
                                         sliderInput(inputId = "M2_binom_alpha_size",
                                                     label = "\\( n \\)",
                                                     min = 0, max = 5, value = 3, step = 1),
                                         sliderInput(inputId = "M2_binom_alpha_prob", 
                                                     label = "\\( p \\)",
                                                     min = 0, max = 1, value = 0.5, step = 0.01)),
                                tabPanel(title = "Negative Binomial", 
                                         helpText(HTML("The <i>Negative Binomial distribution</i> is another <b>discrete</b> probability distribution. It can give <b>zero or any positive whole number</b>. <br> You'll need to specify the number of successful trials \\( r \\) and probability of success \\( p \\).")),
                                         sliderInput(inputId = "M2_nbinom_alpha_size",
                                                     label = "\\( r \\)",
                                                     min = 0, max = 5, value = 3, step = 1),
                                         sliderInput(inputId = "M2_nbinom_alpha_prob", 
                                                     label = "\\( p \\)",
                                                     min = 0, max = 1, value = 0.5, step = 0.01)),
                                tabPanel(title = "Poisson", 
                                         helpText(HTML("The <i>Poisson distribution</i> is another <b>discrete</b> probability distribution. It can give <b>zero or any positive whole number</b>. <br> It is useful in that <b>you only need to specify one parameter</b>, a mean \\( \\lambda \\).")),
                                         sliderInput(inputId = "M2_pois_alpha_lambda",
                                                     label = "\\( \\lambda \\)",
                                                     min = 0, max = 5, value = 3, step = 0.01)))

####################### Dynamic UI elements for module 3 #######################

# Sampling distribution parameters for alpha
M3_disttype_tabs_alpha <- tabsetPanel(id = "M3_disttype_alpha",
                                   type = "hidden",
                                   tabPanel(title = "Uniform",
                                            sliderInput(inputId = "M3_unif_alpha_range",
                                                        label = "Feasible range for \\( \\alpha \\) i.e. \\([a, b]\\)",
                                                        min = 0, max = 2, value = c(0, 0.5), step = 0.01)),
                                   tabPanel(title = "Normal", 
                                            sliderInput(inputId = "M3_norm_alpha_mean",
                                                        label = "\\( \\mu \\)",
                                                        min = 0, max = 2, value = 1, step = 0.01),
                                            sliderInput(inputId = "M3_norm_alpha_sd", 
                                                        label = "\\( \\sigma \\)",
                                                        min = 0, max = 2, value = 1, step = 0.01)),
                                   tabPanel(title = "Beta",
                                            sliderInput(inputId = "M3_beta_alpha_shape1",
                                                        label = "\\( \\alpha_{\\text{shape}} \\)",
                                                        min = 0, max = 30, value = 1, step = 0.01),
                                            sliderInput(inputId = "M3_beta_alpha_shape2", 
                                                        label = "\\( \\beta_{\\text{shape}}\\)",
                                                        min = 0, max = 30, value = 1, step = 0.01)),
                                   tabPanel(title = "Binomial", 
                                            sliderInput(inputId = "M3_binom_alpha_size",
                                                        label = "\\( n \\)",
                                                        min = 0, max = 5, value = 3, step = 1),
                                            sliderInput(inputId = "M3_binom_alpha_prob", 
                                                        label = "\\( p \\)",
                                                        min = 0, max = 1, value = 0.5, step = 0.01)),
                                   tabPanel(title = "Negative Binomial", 
                                            sliderInput(inputId = "M3_nbinom_alpha_size",
                                                        label = "\\( r \\)",
                                                        min = 0, max = 5, value = 3, step = 1),
                                            sliderInput(inputId = "M3_nbinom_alpha_prob", 
                                                        label = "\\( p \\)",
                                                        min = 0, max = 1, value = 0.5, step = 0.01)),
                                   tabPanel(title = "Poisson", 
                                            sliderInput(inputId = "M3_pois_alpha_lambda",
                                                        label = "\\( \\lambda \\)",
                                                        min = 0, max = 5, value = 3, step = 0.01)))

# Sampling distribution parameters for mu_c
M3_disttype_tabs_mu_c <- tabsetPanel(id = "M3_disttype_mu_c",
                                  type = "hidden",
                                  tabPanel(title = "Uniform",
                                           sliderInput(inputId = "M3_unif_mu_c_range",
                                                       label = "Feasible range for \\( \\mu_c \\) i.e. \\([a, b]\\)",
                                                       min = 0, max = 1, value = c(0, 0.5), step = 0.01)),
                                  tabPanel(title = "Normal", 
                                           sliderInput(inputId = "M3_norm_mu_c_mean",
                                                       label = "\\( \\mu \\)",
                                                       min = 0, max = 1, value = 1, step = 0.01),
                                           sliderInput(inputId = "M3_norm_mu_c_sd", 
                                                       label = "\\( \\sigma \\)",
                                                       min = 0, max = 2, value = 1, step = 0.01)),
                                  tabPanel(title = "Beta",
                                           sliderInput(inputId = "M3_beta_mu_c_shape1",
                                                       label = "\\( \\alpha_{\\text{shape}} \\)",
                                                       min = 0, max = 30, value = 1, step = 0.01),
                                           sliderInput(inputId = "M3_beta_mu_c_shape2", 
                                                       label = "\\( \\beta_{\\text{shape}}\\)",
                                                       min = 0, max = 30, value = 1, step = 0.01)),
                                  tabPanel(title = "Binomial", 
                                           sliderInput(inputId = "M3_binom_mu_c_size",
                                                       label = "\\( n \\)",
                                                       min = 0, max = 5, value = 3, step = 1),
                                           sliderInput(inputId = "M3_binom_mu_c_prob", 
                                                       label = "\\( p \\)",
                                                       min = 0, max = 1, value = 0.5, step = 0.01)),
                                  tabPanel(title = "Negative Binomial", 
                                           sliderInput(inputId = "M3_nbinom_mu_c_size",
                                                       label = "\\( r \\)",
                                                       min = 0, max = 5, value = 3, step = 1),
                                           sliderInput(inputId = "M3_nbinom_mu_c_prob", 
                                                       label = "\\( p \\)",
                                                       min = 0, max = 1, value = 0.5, step = 0.01)),
                                  tabPanel(title = "Poisson", 
                                           sliderInput(inputId = "M3_pois_mu_c_lambda",
                                                       label = "\\( \\lambda \\)",
                                                       min = 0, max = 5, value = 3, step = 0.01)))

# [UNUSED] Sampling distribution parameters for initC
#M3_disttype_tabs_initC <- tabsetPanel(id = "M3_disttype_initC",
#                                   type = "hidden",
#                                   tabPanel(title = "Uniform",
#                                            sliderInput(inputId = "M3_unif_initC_range",
#                                                        label = "Feasible range for \\( C(0) \\) i.e. \\([a, b]\\)",
#                                                        min = 10^4, max = 10^6, value = c(10^5, 2*10^5), step = 10^4)),
#                                   tabPanel(title = "Normal", 
#                                            sliderInput(inputId = "M3_norm_initC_mean",
#                                                        label = "\\( \\mu \\)",
#                                                        min = 0, max = 10^6, value = 10^5, step = 10^4),
#                                            sliderInput(inputId = "M3_norm_initC_sd", 
#                                                        label = "\\( \\sigma \\)",
#                                                        min = 0, max = 10^6, value = 1, step = 10^3)),
#                                   tabPanel(title = "Beta",
#                                            sliderInput(inputId = "M3_beta_initC_shape1",
#                                                        label = "\\( \\alpha_{\\text{shape}} \\)",
#                                                        min = 0, max = 30, value = 1, step = 0.01),
#                                            sliderInput(inputId = "M3_beta_initC_shape2", 
#                                                        label = "\\( \\beta_{\\text{shape}}\\)",
#                                                        min = 0, max = 30, value = 1, step = 0.01)),
#                                   tabPanel(title = "Binomial", 
#                                            sliderInput(inputId = "M3_binom_initC_size",
#                                                        label = "\\( n \\)",
#                                                        min = 0, max = 10^6, value = 10^5, step = 10^4),
#                                            sliderInput(inputId = "M3_binom_initC_prob", 
#                                                        label = "\\( p \\)",
#                                                        min = 0, max = 1, value = 0.5, step = 0.01)),
#                                   tabPanel(title = "Negative Binomial", 
#                                            sliderInput(inputId = "M3_nbinom_initC_size",
#                                                        label = "\\( r \\)",
#                                                        min = 0, max = 10^6, value = 10^5, step = 10^4),
#                                            sliderInput(inputId = "M3_nbinom_initC_prob", 
#                                                        label = "\\( p \\)",
#                                                        min = 0, max = 1, value = 0.5, step = 0.01)),
#                                   tabPanel(title = "Poisson", 
#                                            sliderInput(inputId = "M3_pois_initC_lambda",
#                                                        label = "\\( \\lambda \\)",
#                                                        min = 0, max = 10^6, value = 10^5, step = 10^4)))

# Sampling methods
sampling_method_tabs <- tabsetPanel(id = "M3_sampling_method_show",
                                  type = "hidden",
                                  tabPanel(title = "Brute force (Grid Search)",
                                           helpText(HTML("<i>Grid search</i> is a simple, brute force technique that includes <b>every combination</b> of parameters across evenly-spaced points.
                                                         <br> Why would this pose a problem in simulating a model?")),
                                           shinyWidgets::sliderTextInput(inputId = "M3_sample_n",
                                                                         label = "Number of points to sample from each parameter",
                                                                         choices = c(1, 2, 5, 10, 100), selected = 10, grid = T),
                                           uiOutput("M3_out_grid_search_samples"),
                                           helpText(HTML("The number of samples equals the number of points <b>raised to the power</b> of how many uncertain parameters there are!")),
                                           shinyWidgets::sliderTextInput(inputId = "M3_days",
                                                                         label = "Number of days the model will run",
                                                                         choices = c(10, 30, 60, 120, 365), selected = 30, grid = T)),
                                  tabPanel(title = "Random Search",
                                           helpText(HTML("<i>Random search</i> is another simple technique that selects <b>random combinations</b> of parameters according to each of their <b>sampling distributions</b> (cf. Module 2).
                                                         <br> What are the benefits and drawbacks of this method?")),
                                           shinyWidgets::sliderTextInput(inputId = "M3_sample_n",
                                                                         label = "Number of samples to take from parameter space",
                                                                         choices = c(1, 2, 5, 10, 100, 500, 1000), selected = 10, grid = T),
                                           shinyWidgets::sliderTextInput(inputId = "M3_days",
                                                                         label = "Number of days the model will run",
                                                                         choices = c(10, 30, 60, 120, 365), selected = 30, grid = T)),
                                  tabPanel(title = "Latin Hypercube Sampling",
                                           helpText(HTML("<i>LHS</i> is a <b>quasi-random</b> search method that samples sections of each parameter's sampling distributions, such that there is a better spread of values chosen.
                                                         <br> Compared to random search, do you notice any differences in <i>sparsity</i> and <i>clustering</i>?
                                                         <br> In higher dimensions (i.e. with more uncertain parameters), there are likely to be more gaps and clusters in random search (i.e. greater <b>discrepancy</b>) which worsen the effectiveness of our sampling method.")),
                                           shinyWidgets::sliderTextInput(inputId = "M3_sample_n",
                                                                         label = "Number of samples to take from parameter space",
                                                                         choices = c(1, 2, 5, 10, 100, 500, 1000), selected = 10, grid = T),
                                           shinyWidgets::sliderTextInput(inputId = "M3_days",
                                                                         label = "Number of days the model will run",
                                                                         choices = c(10, 30, 60, 120, 365), selected = 30, grid = T)))

############################# Visible UI elements ##############################

addResourcePath("prefix", "www")
page_navbar(
  title = list(tags$img(src = "prefix/UoM_Logo.png", height = 50, width = 50),
               "POPH90271 IDM: Uncertainty and Sensitivity Analysis Playground"),
  bg = "#000F46",
  inverse = TRUE,
  
  #################### Welcome page ####################
  
  nav_panel(title = "Welcome",
            accordion(
              multiple = FALSE,
              accordion_panel('Introduction \uD83D\uDC4B',
                              p(HTML(paste('Hello and welcome to the <i>Infectious Disease Modelling Uncertainty and Sensitivity Analysis Playground</i>, an interactive <i>Shiny</i> app for <b>POPH90271 Infectious Diseases Modelling</b>!'))),
                              p(HTML(paste('Through the Week 5 lectures, we were introduced to the concepts of <i>Uncertainty and Sensitivity Analysis</i>.'))),
                              h4('Learning Objectives'),
                              p(HTML(paste('In this app, we will use an example from the extant literature to gain an <i>intuitive feel</i> for:'))),
                              tags$ul(tags$li(HTML(paste('How models react to changes in parameter and state inputs (<b>sensitivity</b>),'))),
                                      tags$li(HTML(paste('The different approaches with which parameter and state inputs can be sampled (<b>non-informative uncertainty</b>), and'))),
                                      tags$li(HTML(paste('How computational considerations affect feasibility of simulations (<b>implementation efficiency</b>).<br><br>')))),
                              h4('App Overview'),
                              p(HTML(paste('In <b>Module 1: Parameter Exploration</b>, we introduce a <b>cholera outbreak</b> \uD83E\uDDA0,
                                           and visualise how changing various parameters and state inputs affects the model&apos;s outputs,'))),
                              p(HTML(paste('In <b>Module 2: Single Parameter Sampling </b>, we introduce various probability distributions
                                           (<b>sampling distributions</b> \uD83D\uDCCA) with which we can use to sample a single parameter we are uncertain of.'))),
                              p(HTML(paste('In <b>Module 3: Multiple Parameters</b>, we sample more than one parameter and introduce different
                                           techniques (<b>sampling methods</b> \uD83D\uDD0D) to efficiently explore the sample space.<br><br>'))),
                              h4('Acknowledgements'),
                              p(HTML(paste('This Shiny app was created in 2024 by <a href="mailto:jaswong@unimelb.edu.au">Mr Jason Wong</a> (MD4/MPH Candidate, The University of Melbourne),
                                           under the supervision of <a href="mailto:patricia.campbell@unimelb.edu.au">Dr Trish Campbell</a> (The Peter Doherty Institute for Infection and Immunity).',
                                           '<br> It is intended to be used with Dr Campbell&apos;s second semester subject <b>POPH90271 Infectious Diseases Modelling</b>.'))),
                              p(HTML(paste('The model used in this app was adapted from Andrews, J. R., & Basu, S. (2011).
                              <i>Transmission dynamics and control of cholera in Haiti: an epidemic model</i>. Lancet, 377(9773), 1248–1255.
                              <br>You can access the original paper <a href="https://doi.org/10.1016/S0140-6736(11)60273-0">here</a>.')))),
              accordion_panel('Model specification \uD83D\uDCDD',
                              p(HTML(paste('On Tuesday 12 January 2010, a catastrophic magnitude 7.0 earthquake struck the Carribbean nation of Haiti.',
            'Devastating the country&apos;s already poor water and sewage infrastructure, cases of <b><i>Vibrio cholerae</i></b> infection were reported in the country for the first time in over a century.'))),
            p(HTML(paste('In early 2011, Andrews and Basu constructed a model to evaluate the dynamics and control of cholera in this epidemic.',
            'Consider the following <b>model diagram</b> (simplified from the original paper). <br><i>What are the compartments in this model?</i>'))),
            div(img(src = "prefix/Simplified_Model_Diagram.png", width = "50%"), style="text-align: center;"),
            h4('States'),
            p(HTML(paste('There are <b>5 compartments</b> in this model (4 human and 1 cholera)&mdash;'))),
            withMathJax('$$S: \\text{Susceptible}$$'),
            withMathJax('$$I_s: \\text{Symptomatic}$$'),
            withMathJax('$$I_a: \\text{Asymptomatic}$$'),
            withMathJax('$$R: \\text{Recovered}$$'),
            withMathJax('$$C: \\text{Concentration of cholera in reservoir}$$'),
            p(HTML(paste('with 2 additional states defined&mdash;'))),
            withMathJax('$$N = S + I_s + I_a + R: \\text{Total population}$$'),
            withMathJax('$$W: \\text{Per capita volume of water reservoir accessed}$$'),
            h4('Parameters'),
            p(HTML(paste('There are <b>10 parameters</b> in this model. The values in brackets are what Andrews and Basu used)&mdash;'))),
            withMathJax('$$\\alpha: \\text{rate of contaminated water consumption (fitted)}$$'),
            withMathJax('$$\\kappa: \\text{concentration of cholera yielding 50% chance of infection (10^5 cells)}$$'),
            withMathJax('$$p: \\text{proportion of infections that are asymptomatic (0.79)}$$'),
            withMathJax('$$\\mu_c: \\text{cholera-induced death rate (fitted)}$$'),
            withMathJax('$$\\gamma: \\text{rate of recovery (5d)}^{-1}$$'),
            withMathJax('$$\\omega: \\text{rate of loss of immunity (0.8yr)}^{-1}$$'),
            withMathJax('$$\\mu: \\text{birth and death rate (61yr)}^{-1}$$'),
            withMathJax('$$\\sigma: \\text{rate of shedding of symptomatic individual (1.3*10^11 cells/d)}$$'),
            withMathJax('$$\\epsilon: \\text{shedding modifier for asymptomatic individuals (0.001)}$$'),
            withMathJax('$$\\delta: \\text{decay of cholera in the environment (30d)}^{-1}$$'),
            p(HTML(paste('<i>While Andrews and Basu fit some of their parameters to data, how else could we assign values to those parameters?</i>'))),
            p(HTML(paste('We will explore these <b>non-informative methods</b> in Modules 2 and 3.'))),
            h4('Differential Equations'),
            p(HTML(paste('There are <b>5 ODEs</b> in this model&mdash;'))),
            withMathJax('$$\\dot{S} = \\mu N(t) + \\omega R(t) - \\alpha \\frac{C(t)}{\\kappa + C(t)} S(t) -\\mu S(t)$$'),
            withMathJax('$$\\dot{I_s} = (1 - p) \\cdot \\alpha \\frac{C(t)}{\\kappa + C(t)} S(t) - (\\gamma + \\mu_c + \\mu) I_s(t)$$'),
            withMathJax('$$\\dot{I_a} = p \\cdot \\alpha \\frac{C(t)}{\\kappa + C(t)} S(t) - (\\gamma + \\mu) I_a(t)$$'),
            withMathJax('$$\\dot{R} = \\gamma (I_s (t) + I_a(t)) - (\\omega + \\mu) R(t)$$'),
            withMathJax('$$\\dot{C} = \\frac{\\sigma (I_s(t) + \\epsilon I_a(t))}{W \\cdot N}  - \\delta C(t)$$'),
            p(HTML(paste('<i>With this system of ODEs, how should we expect the total population to change over time?</i>'))),
            p(HTML(paste('Navigate to <b>Module 1: Parameter Exploration</b> to see if your prediction is correct!'))))
            )),
  #################### Module 1 UI ####################
  
  nav_panel(title = "Module 1: Parameter Exploration",
            withMathJax(),
            layout_sidebar(
              
            # 1.1 Sidebar for parameter, state and time window sliders
              sidebar = sidebar(
                accordion(
                  multiple = FALSE,
                  accordion_panel("Time Window",
                                  helpText("Choose the number of days the model will run for."),
                                  sliderInput(inputId = "M1_days",
                                              label = "Number of days",
                                              min = 0, max = 1000, value = 30)),
                  accordion_panel("States",
                                  helpText("Choose the initial states of the population."),
                                  sliderInput(inputId = "M1_initS",
                                              label = "\\( S(0) \\): Initial Susceptible",
                                              min = 0, max = 10000, value = 1000),
                                  sliderInput(inputId = "M1_initI_s",
                                              label = "\\( I_s(0) \\): Initial Symptomatic",
                                              min = 0, max = 10000, value = 0),
                                  sliderInput(inputId = "M1_initI_a",
                                              label = "\\( I_a(0) \\): Initial Asymptomatic",
                                              min = 0, max = 10000, value = 0),
                                  shinyWidgets::sliderTextInput(inputId = "M1_initC",
                                                                label = "\\( C(0) \\): Initial Cholera Reservoir Concentration (bacteria / L)",
                                                                choices = c(0, 1, 10, 10^2, 10^3, 10^4, 10^5, 10^6, 10^7), selected = 10^5, grid = T),
                                  sliderInput(inputId = "M1_W",
                                              label = "\\( W(0) \\): Water reservoir volume per capita  (L per person)",
                                              min = 1, max = 100, value = 15)),
                  accordion_panel("Parameters",
                                  helpText("Choose the values of the parameters."),
                                  sliderInput(inputId = "M1_ContamWaterCons_Rate",
                                              label = "\\( \\alpha \\): Rate of contaminated water consumption (per day)",
                                              min = 0, max = 2, value = 1, step = 0.01,
                                              animate = animationOptions(interval = 1000, loop = T)),
                                  shinyWidgets::sliderTextInput(inputId = "M1_Cholera_ID50",
                                                                label = "\\( \\kappa \\): Concentration of cholera yielding 50% chance of infection (bacteria / L)",
                                                                choices = c(0, 1, 10, 10^2, 10^3, 10^4, 10^5, 10^6, 10^7), selected = 10^5, grid = T,
                                                                animate = animationOptions(interval = 1000, loop = T)),
                                  sliderInput(inputId = "M1_Asymptomatic_Proportion",
                                              label = "\\( p \\): Proportion of infections that are asymptomatic",
                                              min = 0, max = 1, value = 0.79, step = 0.01,
                                              animate = animationOptions(interval = 1000, loop = T)),
                                  sliderInput(inputId = "M1_CholeraDeath_Rate",
                                              label = "\\( \\mu_c \\) Cholera-induced death rate (per day)",
                                              min = 0, max = 1, value = 0.5, step = 0.01,
                                              animate = animationOptions(interval = 1000, loop = T)),
                                  shinyWidgets::sliderTextInput(inputId = "M1_invRecovery_Rate",
                                                                label = "\\( D_\\text{inf} = \\frac{1}{\\gamma} \\): Duration of infectiousness (days)",
                                                                choices = c(0.001, 1:21, 1000), selected = 5, grid = T,
                                                                animate = animationOptions(interval = 1000, loop = T)),
                                  shinyWidgets::sliderTextInput(inputId = "M1_invImmunityLoss_Rate",
                                                                label = "\\( D_\\text{imm} = \\frac{1}{\\omega} \\): Duration of immunity (years)",
                                                                choices = c(0.001, seq(0.1, 2, 0.1), 1000), selected = 0.8, grid = T,
                                                                animate = animationOptions(interval = 1000, loop = T)),
                                  sliderInput(inputId = "M1_invBirthDeath_Rate",
                                              label = "\\( L_\\text{H} = \\frac{1}{\\mu} \\): Life expectancy of humans (years)",
                                              min = 30, max = 100, value = 61, step = 1,
                                              animate = animationOptions(interval = 1000, loop = T)),
                                  shinyWidgets::sliderTextInput(inputId = "M1_Shedding_Rate",
                                                                label = "\\( \\sigma \\): Rate of shedding per symptomatic individual (bacteria / day per person)",
                                                                choices = c(0, 10^5, 10^6, 10^7, 10^8, 10^9, 10^10, 10^11), selected = 10^5, grid = T,
                                                                animate = animationOptions(interval = 1000, loop = T)),
                                  shinyWidgets::sliderTextInput(inputId = "M1_SheddingAsymptomatic_Modifier",
                                                                label = " \\( \\epsilon \\): Shedding modifier for asymptomatic individuals",
                                                                choices = c(0, 10^-4, 10^-3, 10^-2, 0.1, 0.2, 0.4, 0.6, 0.8, 1), selected = 10^-3, grid = T,
                                                                animate = animationOptions(interval = 1000, loop = T)),
                                  shinyWidgets::sliderTextInput(inputId = "M1_invCholeraDecay_Rate",
                                                                label = "\\( L_\\text{C} = \\frac{1}{\\delta} \\): Life expectancy of cholera in the environment (days)",
                                                                choices = c(0.001, 1:120, 1000), selected = 30, grid = T,
                                                                animate = animationOptions(interval = 1000, loop = T)))
              )
            ),
            
            # 1.2 Main panel cards
            layout_columns(
              # 1.2.1 Model time series visualisation for humans
              card(card_header("Human population over time"),
                   plotlyOutput("M1_out_plot_human")),
              # 1.2.2 Model time series visualisation for force of infection
              card(card_header("Force of infection (\u03BB) over time"),
                   plotlyOutput("M1_out_plot_lambda")),
              # 1.2.3 Model time series visualisation for cholera concentration
              card(card_header("Cholera concentration over time"),
                   plotlyOutput("M1_out_plot_cholera")),
            col_widths = c(12, 6, 6),
            row_heights = c(2, 1)))),
  
  #################### Module 2 UI ####################
  
  nav_panel(title = "Module 2: Single Parameter Sampling",
            withMathJax(),
            layout_sidebar(
              
            # 2.1 Sidebar for parameter, state and time window sliders
              sidebar = sidebar(
                open = "closed",
                accordion(
                  multiple = FALSE,
                  open = "Parameters",
                  accordion_panel("States",
                                  helpText("Choose the initial states of the population."),
                                  sliderInput(inputId = "M2_initS",
                                              label = "\\( S(0) \\): Initial Susceptible",
                                              min = 0, max = 10000, value = 1000),
                                  sliderInput(inputId = "M2_initI_s",
                                              label = "\\( I_s(0) \\): Initial Symptomatic",
                                              min = 0, max = 10000, value = 0),
                                  sliderInput(inputId = "M2_initI_a",
                                              label = "\\( I_a(0) \\): Initial Asymptomatic",
                                              min = 0, max = 10000, value = 0),
                                  shinyWidgets::sliderTextInput(inputId = "M2_initC",
                                                                label = "\\( C(0) \\): Initial Cholera Reservoir Concentration (bacteria / L)",
                                                                choices = c(0, 1, 10, 10^2, 10^3, 10^4, 10^5, 10^6, 10^7), selected = 10^5, grid = T),
                                  sliderInput(inputId = "M2_W",
                                              label = "\\( W(0) \\): Water reservoir volume per capita  (L per person)",
                                              min = 1, max = 100, value = 15)),
                  accordion_panel("Parameters",
                                  tags$div("We now sample \\( \\alpha \\) using the sampling distribution set on the right."),
                                  helpText("Choose the values of the other parameters."),
                                  shinyWidgets::sliderTextInput(inputId = "M2_Cholera_ID50",
                                                                label = "\\( \\kappa \\): Concentration of cholera yielding 50% chance of infection (bacteria / L)",
                                                                choices = c(0, 1, 10, 10^2, 10^3, 10^4, 10^5, 10^6, 10^7), selected = 10^5, grid = T),
                                  sliderInput(inputId = "M2_Asymptomatic_Proportion",
                                              label = "\\( p \\): Proportion of infections that are asymptomatic",
                                              min = 0, max = 1, value = 0.79, step = 0.01),
                                  sliderInput(inputId = "M2_CholeraDeath_Rate",
                                              label = "\\( \\mu_c \\) Cholera-induced death rate (per day)",
                                              min = 0, max = 1, value = 0.5, step = 0.01),
                                  shinyWidgets::sliderTextInput(inputId = "M2_invRecovery_Rate",
                                                                label = "\\( D_\\text{inf} = \\frac{1}{\\gamma} \\): Duration of infectiousness (days)",
                                                                choices = c(0.001, 1:21, 1000), selected = 5, grid = T),
                                  shinyWidgets::sliderTextInput(inputId = "M2_invImmunityLoss_Rate",
                                                                label = "\\( D_\\text{imm} = \\frac{1}{\\omega} \\): Duration of immunity (years)",
                                                                choices = c(0.001, seq(0.1, 2, 0.1), 1000), selected = 0.8, grid = T),
                                  sliderInput(inputId = "M2_invBirthDeath_Rate",
                                              label = "\\( L_\\text{H} = \\frac{1}{\\mu} \\): Life expectancy of humans (years)",
                                              min = 30, max = 100, value = 61, step = 1),
                                  shinyWidgets::sliderTextInput(inputId = "M2_Shedding_Rate",
                                                                label = "\\( \\sigma \\): Rate of shedding per symptomatic individual (bacteria / day per person)",
                                                                choices = c(0, 10^5, 10^6, 10^7, 10^8, 10^9, 10^10, 10^11), selected = 10^5, grid = T),
                                  shinyWidgets::sliderTextInput(inputId = "M2_SheddingAsymptomatic_Modifier",
                                                                label = " \\( \\epsilon \\): Shedding modifier for asymptomatic individuals",
                                                                choices = c(0, 10^-4, 10^-3, 10^-2, 0.1, 0.2, 0.4, 0.6, 0.8, 1), selected = 10^-3, grid = T),
                                  shinyWidgets::sliderTextInput(inputId = "M2_invCholeraDecay_Rate",
                                                                label = "\\( L_\\text{C} = \\frac{1}{\\delta} \\): Life expectancy of cholera in the environment (days)",
                                                                choices = c(0.001, 1:120, 1000), selected = 30, grid = T)
                  )
                )
              ),
              
            # 2.2 Main panel cards
            layout_columns(
              # 2.2.1 Choosing sampling distribution type, sample size and time window
              card(card_header("Distribution type, sample size and time window"),
                   selectInput(inputId = "M2_alpha_disttype",
                               label = "Probability distribution",
                               choices = c("Uniform", "Normal", "Beta", "Binomial", "Negative Binomial", "Poisson")),
                   shinyWidgets::sliderTextInput(inputId = "M2_alpha_n",
                                                 label = "Number of samples of \\( \\alpha \\)",
                                                 choices = c(1, 2, 5, 10, 100, 500, 1000), selected = 10, grid = T),
                   shinyWidgets::sliderTextInput(inputId = "M2_days",
                                                 label = "Number of days the model will run",
                                                 choices = c(10, 30, 60, 120, 365), selected = 30, grid = T)
              ),
              # 2.2.2 Choosing sampling distribution 
              card(card_header("Sampling distribution parameters"),
                   M2_disttype_tabs),
              # 2.2.3 Visualising sample distribution
              card(card_header("Samples of \\( \\alpha \\) you've taken"),
                   plotlyOutput("M2_out_alpha_dist")),
              # 2.2.4 Summary of selection and Model time series visualisation
              card(uiOutput("M2_out_alpha_selection"),
                plotlyOutput("M2_out_plot")),
            col_widths = c(4, 4, 4, 12),
            row_heights = c(2, 3)))),
  
  #################### Module 3 UI ####################
  
  nav_panel(title = "Module 3: Multiple Parameters",
            withMathJax(),
            layout_sidebar(
              
            # 3.1 Sidebar for parameter and state sliders
              sidebar = sidebar(
                open = "closed",
                accordion(
                  multiple = FALSE,
                  open = "Parameters",
                  accordion_panel("States",
                            helpText("Choose the initial states of the population."),
                            #checkboxInput(inputId = "M3_uncertain_initC", # !Checkboxes can be used to determine what states are uncertain!
                            #              label = "Uncertain \\( C(0) \\)?", value = TRUE),
                            sliderInput(inputId = "M3_initS",
                                        label = "\\( S(0) \\): Initial Susceptible",
                                        min = 0, max = 10000, value = 1000),
                            sliderInput(inputId = "M3_initI_s",
                                        label = "\\( I_s(0) \\): Initial Symptomatic",
                                        min = 0, max = 10000, value = 0),
                            sliderInput(inputId = "M3_initI_a",
                                        label = "\\( I_a(0) \\): Initial Asymptomatic",
                                        min = 0, max = 10000, value = 0),
                            shinyWidgets::sliderTextInput(inputId = "M3_initC",
                                         label = "\\( C(0) \\): Initial Cholera Reservoir Concentration (bacteria / L)",
                                         choices = c(0, 1, 10, 10^2, 10^3, 10^4, 10^5, 10^6, 10^7), selected = 10^5, grid = T),
                            sliderInput(inputId = "M3_W",
                                        label = "\\( W(0) \\): Water reservoir volume per capita  (L per person)",
                                        min = 1, max = 100, value = 15)),
                  accordion_panel("Parameters",
                            tags$div("We now sample combinations of \\( \\alpha \\) and \\( \\mu_c \\) using the sampling distributions and method set on the right"),
                            helpText("Choose the values of the other parameters."),
                            #checkboxInput(inputId = "M3_uncertain_alpha",
                            #              label = "Uncertain \\( \\alpha \\)?", value = TRUE),
                            #checkboxInput(inputId = "M3_uncertain_mu_c",
                            #              label = "Uncertain \\( \\mu_c \\)?", value = TRUE),
                            #sliderInput(inputId = "M3_CholeraDeath_Rate",
                            #             label = "\\( \\mu_c \\) Cholera-induced death rate (per day)",
                            #             min = 0, max = 1, value = 0.5, step = 0.01),
                            shinyWidgets::sliderTextInput(inputId = "M3_Cholera_ID50",
                                                          label = "\\( \\kappa \\): Concentration of cholera yielding 50% chance of infection (bacteria / L)",
                                                          choices = c(0, 1, 10, 10^2, 10^3, 10^4, 10^5, 10^6, 10^7), selected = 10^5, grid = T),
                            sliderInput(inputId = "M3_Asymptomatic_Proportion",
                                        label = "\\( p \\): Proportion of infections that are asymptomatic",
                                        min = 0, max = 1, value = 0.79, step = 0.01),
                            shinyWidgets::sliderTextInput(inputId = "M3_invRecovery_Rate",
                                                          label = "\\( D_\\text{inf} = \\frac{1}{\\gamma} \\): Duration of infectiousness (days)",
                                                          choices = c(0.001, 1:21, 1000), selected = 5, grid = T),
                            shinyWidgets::sliderTextInput(inputId = "M3_invImmunityLoss_Rate",
                                                          label = "\\( D_\\text{imm} = \\frac{1}{\\omega} \\): Duration of immunity (years)",
                                                          choices = c(0.001, seq(0.1, 2, 0.1), 1000), selected = 0.8, grid = T),
                            sliderInput(inputId = "M3_invBirthDeath_Rate",
                                        label = "\\( L_\\text{H} = \\frac{1}{\\mu} \\): Life expectancy of humans (years)",
                                        min = 30, max = 100, value = 61, step = 1),
                            shinyWidgets::sliderTextInput(inputId = "M3_Shedding_Rate",
                                                          label = "\\( \\sigma \\): Rate of shedding per symptomatic individual (bacteria / day per person)",
                                                          choices = c(0, 10^5, 10^6, 10^7, 10^8, 10^9, 10^10, 10^11), selected = 10^5, grid = T),
                            shinyWidgets::sliderTextInput(inputId = "M3_SheddingAsymptomatic_Modifier",
                                                          label = " \\( \\epsilon \\): Shedding modifier for asymptomatic individuals",
                                                          choices = c(0, 10^-4, 10^-3, 10^-2, 0.1, 0.2, 0.4, 0.6, 0.8, 1), selected = 10^-3, grid = T),
                            shinyWidgets::sliderTextInput(inputId = "M3_invCholeraDecay_Rate",
                                                          label = "\\( L_\\text{C} = \\frac{1}{\\delta} \\): Life expectancy of cholera in the environment (days)",
                                                          choices = c(0.001, 1:120, 1000), selected = 30, grid = T))
                )
              ),
              
              # 3.2 Main panel cards
              layout_columns(
                # 3.2.1 Choosing sampling distribution type and sample size
                card(card_header("Sampling distribution"),
                     selectInput(inputId = "M3_alpha_disttype",
                                 label = "Probability distribution type for \\( \\alpha \\)",
                                 choices = c("Uniform", "Normal", "Beta", "Binomial", "Negative Binomial", "Poisson")),
                     M3_disttype_tabs_alpha,
                     selectInput(inputId = "M3_mu_c_disttype",
                                 label = "Probability distribution type for \\( \\mu_c \\)",
                                 choices = c("Uniform", "Normal", "Beta", "Binomial", "Negative Binomial", "Poisson")),
                     M3_disttype_tabs_mu_c#,
                    #selectInput(inputId = "M3_initC_disttype",
                    #            label = "Probability distribution type \\( C(0) \\)",
                    #            choices = c("Uniform", "Normal", "Beta", "Binomial", "Negative Binomial", "Poisson")),
                    #M3_disttype_tabs_initC
                    ),
                
                # 3.2.2 Choosing sampling distribution 
                card(card_header("Sampling method"),
                     selectInput(inputId = "M3_sampling_method",
                                 label = "Sampling method type",
                                 choices = c("Brute force (Grid Search)", "Random Search",
                                             "Latin Hypercube Sampling"), selected = "Brute force (Grid Search)"),
                     sampling_method_tabs),
                # 3.2.3 Visualising sample distribution
                card(card_header("Samples in parameter space"),
                     plotlyOutput("M3_out_parameter_space")),
                
                # 3.2.4 Summary of selection and Model time series visualisation
                card(uiOutput("M3_out_sampling_method_selection"),
                     plotlyOutput("M3_out_plot")), 
                col_widths = c(3, 3, 6, 12),
                row_heights = c(2, 3))
            )
  )
)