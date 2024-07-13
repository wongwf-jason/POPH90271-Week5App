# Infectious Disease Modelling Uncertainty and Sensitivity Analysis Playground
An interactive *Shiny* app for **POPH90271 Infectious Diseases Modelling** \
Created by: Mr Jason Wong (MD4/MPH Candidate, The University of Melbourne) \
Supervisor: Dr Trish Campbell (The Peter Doherty Institute for Infection and Immunity) \
Version: 1.0 (July 2024)

## Shiny Application Learning Objectives
Using an example from the extant literature, gain an *intuitive feel* of:
- [ ] How infectious disease models react to changes in parameter and state inputs (**sensitivity**),
- [ ] The different approaches with which parameter and state inputs can be sampled (**non-informative uncertainty**), and
- [ ] How computational considerations affect feasibility of simulations (**implementation efficiency**)

## Shiny Application Features
Across three learning modules, allows students to:
1. Explore a deterministic SIRSC Cholera model, based off Andrews and Basu (2011),
2. Produce interactive times series plots for human and cholera compartments, where students can see live changes to alterations in model parameters and initial states,
3. Produce looping animated time series plots as parameters progressively change,
4. Explore single parameter sampling, comparing Uniform, Normal, Beta, Binomial, Negative Binomial, and Poisson distributions, and their effect on transmission dynamics,
5. Explore multiple parameter sampling, with Brute force (Grid Search), Random Search, and quasi-random search (Latin Hypercube Sampling), and their effect on transmission dynamics, and
6. Appreciate the benefits and drawbacks of certain sampling distributions and methods in the simulation setting.

## Changelog
- 1.0 (July 2024): Shiny app deployed to shinyapps.io for hosting

## Known issues / Future directions
- Grid search does not work with Normal, Negative binomial and Poisson distributions
- Reduce code reduplication / improve efficiency
- Improve code documentation
- More tooltips needed for unfeasible parameter distributions
- Seed not reproducing across instances ?allow user to input seed
- Stop button to terminate simulation and display computed results
- More flexibility in multi-parameter sampling
- Other visualisations for uncertainty analysis incl. curve-wise CI plots, pair plots
- PRCC and tornado plot visualisation
- Informative sampling techniques: MCMC, Bayesian sampling techniques, etc.
- Fixed model ?full Andrews and Basu model, model builder
- Stochastic models