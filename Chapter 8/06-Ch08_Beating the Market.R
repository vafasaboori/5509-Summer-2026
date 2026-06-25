# Clearing the Environment ----
rm(list=ls())

# Set working directory ----
setwd("~/Documents/5509 Summer 2026/Chapter 8")

# Load the GA and readxl packages
library(GA) # Genetic algorithms inspired by natural selection and genetics. 
library(readxl)

# Read the data from the Excel file
data <- read_excel("market.xlsx")

# Extract the relevant columns
stock_returns <- as.matrix(data[, 3:7]) # 5 stocks in the portfolio
market_returns <- data[, 8] # only one column

# Define the objective function
# This function takes a candidate weight vector x, normalizes it into portfolio percentages, 
# then computes the quarterly portfolio returns, and returns how many of those quarters beat the market.

objective <- function(x) { # x is a vector of size 5 scores for stocks (not percentages)
  weights <- x / sum(x)  # Normalize x (sum equal to 1), now weights are percentages
  portfolio_returns <- stock_returns %*% weights  # Calculate the portfolio returns for each quarter
  # %*% is the matrix multiplication operator in R. 
  # The result is a vector of returns (24x1 matrix) for 24 quarters.(24x5)*(5x1):(24*1)
  beat_vector <- portfolio_returns > market_returns 
  # Create a logical vector: TRUE for each quarter the portfolio outperforms the market
  num_beat_market <- sum(beat_vector)
  # Sum the TRUEs to get the total number of quarters beating the market
  return(num_beat_market)
}

# Define the portfolio weights' bounds
lower_bound <- rep(0, 5) # vector of zeros with a length of 5 
upper_bound <- rep(1, 5) # vector of ones with a length of 5 

# Run the genetic algorithm optimization
result <- ga(
  type = "real-valued", # Problem type: real-valued variables
                        # portfolio weights can take any real value within bounds.
  fitness = objective,  # Objective function
  lower = lower_bound,  # Lower bounds for portfolio weights
  upper = upper_bound,  # Upper bounds for portfolio weights
  popSize = 500,  # Sets the population size
  maxiter = 5000,  # Sets the maximum number of generations
  run = 500  # Sets the number of independent runs
)

# Extract the optimal solution and objective value
optimal_weights <- result@solution[1, ] # Extract the optimal portfolio scores (first row)
                                        # @ extracts slots from S4 objects vs. 
                                        # $ extracts elements from lists/S3 objects
                        
optimal_weights <- optimal_weights / sum(optimal_weights)  
                        # Normalize the weights to sum up to 1
optimal_num_beat_market <- result@fitnessValue[1]  
                        # Extract the optimized obj fun (max No. beating the market)

# Print the optimal solution and objective value
cat("Optimal Portfolio Weights:", optimal_weights, "\n")
cat("Number of Quarters Beating the Market:", optimal_num_beat_market, "\n")

