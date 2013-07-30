# Split-duration vs. probit simulations
# Andreas Beger
# July 2013

# Setup -------------------------------------------------------------------

library(spduration)
library(survival)

# DGP ---------------------------------------------------------------------

CreateDurationData <- function(N) {
  # variables
  N <- 1000
  t.max <- 12
  cure.fraction <- 0.5
  
  # Cure equation
  cure <- rlogis(N, qlogis(cure.fraction))
  
  # Duration equation
  duration <- rweibull(1000, shape=5, scale=10)
  
  # Observed outcomes
  obs.duration <- ifelse(cure>0.5, t.max, duration)
}

ConvertDurToPanel <- function() {
  
}

# Cure coding -------------------------------------------------------------
