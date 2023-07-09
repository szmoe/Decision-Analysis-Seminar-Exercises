install.packages("decisionSupport")
library(decisionSupport)
library (DiagrammeR)
library(tidyverse)
library(ggplot2)
library(plyr)
library(dplyr)
library(readr)
input_estimates <- data.frame(variable = c("veryunlikely", "unlikely", "neutral", "likely", "verylikely"),
                              lower = c(0.1, 0.3, 0.4, 0.5, 0.4),
                              median = NA,
                              upper = c(0.2, 0.5, 0.6, 0.7, 0.6),
                              distribution = c("tnorm_0_1", "tnorm_0_1", "tnorm_0_1", "tnorm_0_1", "tnorm_0_1"),
                              label = c("veryunlikely", "unlikely", "neutral", "likely", "verylikely"),
                              Description = c("veryunlikely",
                                              "unlikely",
                                              "neutral",
                                              "likely",
                                              "verylikely"))


install.packages("mcSimulation")
model_function <- function(){
  
  # Estimate the likelihood of behavioural change based on quailty of educational leaflet
  positive_response <- likely + verylikely
  negative_response <- veryunlikely + unlikely + neutral
  
  # Generate the list of outputs from the Monte Carlo simulation
  return(list(positive_response = positive_response,
              negative_response = negative_response))
}

# Run the Monte Carlo simulation using the model function
example_mc_simulation <- mcSimulation(estimate = as.estimate(input_estimates),
                                      model_function = model_function,
                                      numberOfModelRuns = 800,
                                      functionSyntax = "plainNames")
example_mc_simulation


decisionSupport::plot_distributions(mcSimulation_object = example_mc_simulation,
                   vars = c("positive_response", "negative_response"),
                   method = 'smooth_simple_overlay',
                   base_size = 12)
#Test editing
