# Week 5: Uncertainty and Sensitivity Analysis — Shiny App
**POPH90271 Infectious Diseases Modelling** \
Created by: Mr Jason Wong (MD4/MPH Candidate, The University of Melbourne) \
Supervisor: Dr Trish Campbell (The Peter Doherty Institute for Infection and Immunity) \
Version: 1.0 (July 2024)

## Shiny Application Learning Objectives
Using an example from the extant literature, gain an *intuitive feel* of:
- [ ] How infectious disease models react to changes in parameter and state inputs (**sensitivity**),
- [ ] The different approaches with which parameter and state inputs can be sampled (**non-informative uncertainty**), and
- [ ] How computational considerations affect feasibility of simulations (**implementation efficiency**)

## Shiny Application Features

## Changelog
- 1.0 (July 2024): Shiny app deployed to shinyapps.io for hosting

## Known issues / Future directions
- Combine all three modules into one dashboard app
- Grid search does not work with Normal, Negative binomial and Poisson distributions
- Reduce code reduplication / improve efficiency
- Improve code documentation
- More tooltips needed for unfeasible parameter distributions
- Seed not reproducing across instances ?allow user to input seed
- Stop button to terminate simulation and display computed results