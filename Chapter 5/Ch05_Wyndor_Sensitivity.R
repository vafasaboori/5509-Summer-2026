# Clearing the Environment ----
rm(list=ls())

# Import lpSolve package ----
library(lpSolve)

# Original Problem ----
# Algebraic Model
# Max P = 300D + 500W (Objective Function)
# Subject to (Constraints)
# 1D + 0W <= 4
# 0D + 2W <= 12
# 3D + 2W <= 18
# D >= 0
# W >= 0

# Set coefficients of the objective function
f.obj <- c(300, 500)

# Set matrix corresponding to coefficients of constraints by rows
# Do not consider the non-negativity constraint; it is automatically assumed
f.con <- matrix(c(1, 0,
                  0, 2,
                  3, 2), nrow = 3, byrow = TRUE)

# nrow: the desired number of rows.
# byrow: If FALSE the matrix is filled by columns, otherwise the filled by rows.

# Set inequality signs
f.dir <- c("<=",
           "<=",
           "<=")

# Set right hand side parameters
f.par <- c(4,
           12,
           18)

# Final value (p)
lp_result <- lp("max", f.obj, f.con, f.dir, f.par)

# Final value (p)
lp_result

# Variables final values
lp_result$solution

# Q1: What if the unit profit of one of Wyndor’s new products is inaccurate? ----
  
# Sensitivity Analysis (Objective Function Coefficients)
sensitivity_analysis <- lp("max", f.obj, f.con, f.dir, f.par, compute.sens = TRUE)

sensitivity_analysis$sens.coef.from # Lower bounds of sensitivity ranges
sensitivity_analysis$sens.coef.to # Upper bounds of sensitivity ranges

# disabling scientific notation: scientific penalty unless wider than 999 digits.
options(scipen =1)

# Reduced costs
sensitivity_analysis$duals[4:5] # Explained later

# Q2: What if the unit profits of both of Wyndor’s new products are inaccurate? ----
  # Optional: Try to implement 100% rule in R (try to automate the process)

# Q3: What if available hours (Constraint RHS) changes in one of the plants? ----

# Sensitivity Analysis (Shadow Prices, constraints first, variables next)
sensitivity_analysis$duals

# Sensitivity Analysis (Allowable RHS, constraints first, variables next)
sensitivity_analysis$duals.from
sensitivity_analysis$duals.to

# Q4: What if available hours (Constraint RHS) changes in one of the plants? ----
# Optional: Try to implement 100% rule in R (try to automate the process)

# Q5: What if the production rates of doors and windows at plant 3 are uncertain? ----
  # range of uncertainty for hours required per door is uniform (2.5–3.5)
  # range of uncertainty for the hours required per window is uniform (1.5–2.5)

# Robust Optimization
# Update the 3rd constraint row (Plant 3) with worst-case values
# Worst-case hours per door = 3.5, per window = 2.5
f.con[3, 1] <- 3.5  # HD3
f.con[3, 2] <- 2.5  # HW3

# Now solve the LP using the updated matrix
result <- lp("max", f.obj, f.con, f.dir, f.par)

# Extract and print results
solution <- result$solution
profit <- result$objval

cat("Robust Optimization Solution (worst-case scenario):\n")
cat("Doors (D) =", round(solution[1], 3), "\n")
cat("Windows (W) =", round(solution[2], 3), "\n")
cat("Profit =", round(profit, 2), "\n")

# Revised Q5: Monte Carlo Simulation (Perfect Information) ----
# Monte Carlo simulation uses random sampling to model uncertainty in complex systems.

# Repeatedly simulates random values for hours per D and W (from uniform distributions),
# Solves the LP for each scenario, and selects the solution with the highest profit.
# Note: This assumes perfect knowledge of uncertainty, so it gives an optimistic result. 
# It is useful for exploring possible outcomes, not for making fixed decisions.

# Monte Carlo Simulation:
# Number of simulations
num_simulations <- 1000

# Initialize vectors to store results
results <- vector("list", num_simulations)
# the type of vector is a list, i.e. each element can be any type of object
# This vector store the solutions from each simulation run of LP.
# "results" is a list, and each element can hold any type of R object (eg vector, data frame, etc.)

p_values <- numeric(num_simulations)
# Creates a numeric vector of length 'num_simulations', initialized with zeros.
# This vector will store the objective function values (eg profit) from each simulation run of the LP.

# Run simulations
for (i in 1:num_simulations) {
  # Simulate uncertain parameters
  hours_door <- runif(1, min = 2.5, max = 3.5) 
  hours_window <- runif(1, min = 1.5, max = 2.5)
  # runif generates random numbers from a uniform distribution.
  # The first argument, 1, specifies the number of random numbers to generate.
  
  # Update the constraint matrix with the simulated parameters
  f.con[3, 1] <- hours_door
  f.con[3, 2] <- hours_window
  
  # Print simulated values for hours required for doors and windows
  print(paste("Simulated hours_door:", hours_door, "hours_window:", hours_window))
  
  # Solve the linear program
  lp_result <- lp("max", f.obj, f.con, f.dir, f.par)
  
  # Store the results
  results[[i]] <- lp_result$solution
  # Double brackets [[ ]] are used specifically to access single elements within a list.
  # Remember! results is a list where each element is a vector.
  
  p_values[i] <- lp_result$objval
  # single brackets [ ] for extracting or assigning values to subsets of a list.
}

# Find the optimal solution based on the simulated results
best_solution <- results[[which.max(p_values)]]
# which.max() returns the index of the maximum value in the vector p_values
# It finds the position of the element with the highest value.

best_p <- max(p_values)

# Print the optimal solution
cat("Optimal Solution:\n")
cat("D =", best_solution[1], "\n")
cat("W =", best_solution[2], "\n")
cat("P =", best_p, "\n")

# Revised Q5: What if plant 3 rates uncertain + chance constraint 95% ----
  # What if the production rates of doors and windows at plant 3 are uncertain?
  # range of uncertainty for hours required per door is uniform (2.5–3.5)
  # range of uncertainty for the hours required per window is uniform (1.5–2.5)
  # 3rd constraint be satisfied at least 95% of the time (chance constraint).

# Function checks if constraint 3 is satisfied at least 95% of the time under uncertainty:
# where hd3 and hw3 are random values from uniform distributions.

check_feasibility <- function(D, W, num_simulations = 1000) {
  
  count <- 0  # Start a counter to track how many times the constraint is satisfied
  
  # Run simulations to sample different possible values for the uncertain parameters
  for (i in 1:num_simulations) {
    
    # Randomly generate one possible value for hours per door in Plant 3 (HD3)
    hd3 <- runif(1, 2.5, 3.5)  
    
    # Randomly generate one possible value for hours per window in Plant 3 (HW3)
    hw3 <- runif(1, 1.5, 2.5)
    
    # Calculate the total hours used in Plant 3 for this simulation
    total_hours <- D * hd3 + W * hw3
    
    # If the total hours are within the 18-hour limit, count it as a success
    if (total_hours <= 18) {
      count <- count + 1
    }
  }
  
  # After all simulations, return the proportion of times the constraint was satisfied
  return(count / num_simulations)  # This gives a value between 0 and 1
}

# Grid search for fixed values of Doors (D) and Windows (W)
# We are trying different combinations of D and W to find the one
# that satisfies all constraints (including the 95% chance constraint for Plant 3)
# and gives the highest profit.

best_profit <- -Inf  # Start with the lowest possible profit (negative infinity)
best_solution <- c(NA, NA)  # Placeholder to store the best values of D and W

# Try different values for D (doors) from 0 to 4, in steps of 0.1
for (D in seq(0, 4, by = 0.1)) {
  
  # Try different values for W (windows) from 0 to 6, in steps of 0.1
  for (W in seq(0, 6, by = 0.1)) {
    
    # Check the hard constraints for Plant 1 and Plant 2:
    # Plant 1: 1 hour per door, max 4 hours available → 1*D <= 4
    # Plant 2: 2 hours per window, max 12 hours available → 2*W <= 12
    if ((1 * D <= 4) && (2 * W <= 12)) {
      
      # Call the check_feasibility function to estimate the chance
      # that the Plant 3 constraint is satisfied (at least 95% of the time)
      satisfied_prob <- check_feasibility(D, W)
      
      # Only consider this D-W pair if it satisfies the Plant 3 constraint in ≥ 95% of simulations
      if (satisfied_prob >= 0.95) {
        
        # Calculate the profit for this combination
        # Profit = 300 per door + 500 per window
        profit <- 300 * D + 500 * W
        
        # If this profit is higher than the best we've seen so far, update the best
        if (profit > best_profit) {
          best_profit <- profit            # Update the best profit
          best_solution <- c(D, W)         # Save the corresponding values of D and W
        }
      }
    }
  }
}
# Print result
cat("Best solution with 95% chance constraint:\n")
cat("D =", round(best_solution[1], 3), "\n")
cat("W =", round(best_solution[2], 3), "\n")
cat("Profit =", round(best_profit, 2), "\n")

# In class Exercise: Normal Distribution -----
# hours required per door follow a normal distribution (mean=3.0, sd=0.5)
# hours required per window follow a normal distribution (mean=2.0, sd=0.5)
# 3rd constraint be satisfied at least 95% of the time (chance constraint).


# Solution for In Class Exercise ----

# This function checks how often a given combination of Doors (D) and Windows (W)
# satisfies the Plant 3 constraint when the processing times follow normal distributions.
# - HD3 ~ Normal(3.0, 0.5)
# - HW3 ~ Normal(2.0, 0.5)
# Returns the proportion of simulations where total hours used ≤ 18

check_feasibility <- function(D, W, num_simulations = 1000) {
  
  count <- 0  # Count how many times the constraint is satisfied
  
  for (i in 1:num_simulations) {
    
    # Randomly generate hours per door using normal distribution (mean = 3.0, sd = 0.5)
    hd3 <- rnorm(1, mean = 3.0, sd = 0.5)
    
    # Randomly generate hours per window using normal distribution (mean = 2.0, sd = 0.5)
    hw3 <- rnorm(1, mean = 2.0, sd = 0.5)
    
    # Calculate total hours used in Plant 3
    total_hours <- D * hd3 + W * hw3
    
    # Count if this combination fits within the 18-hour Plant 3 limit
    if (total_hours <= 18) {
      count <- count + 1
    }
  }
  
  # Return the percentage of successful outcomes (as a decimal between 0 and 1)
  return(count / num_simulations)
}

# Grid search for best fixed D and W values under normal uncertainty
# Goal: maximize profit while ensuring Plant 3 constraint holds ≥ 95% of the time

best_profit <- -Inf            # Start with the lowest possible profit
best_solution <- c(NA, NA)     # Placeholder for best D and W values

# Try D values from 0 to 4 in steps of 0.1
for (D in seq(0, 4, by = 0.1)) {
  
  # Try W values from 0 to 6 in steps of 0.1
  for (W in seq(0, 6, by = 0.1)) {
    
    # Check Plant 1 and Plant 2 hard constraints
    if ((1 * D <= 4) && (2 * W <= 12)) {
      
      # Check if Plant 3 constraint is satisfied ≥ 95% of the time under normal uncertainty
      satisfied_prob <- check_feasibility(D, W)
      
      if (satisfied_prob >= 0.95) {
        
        # Calculate the profit for this D-W combination
        profit <- 300 * D + 500 * W
        
        # If this profit is better than what we've seen so far, save it
        if (profit > best_profit) {
          best_profit <- profit
          best_solution <- c(D, W)
        }
      }
    }
  }
}

# Display the best decision found
cat("Best solution with normal uncertainty and 95% chance constraint:\n")
cat("Doors (D) =", round(best_solution[1], 3), "\n")
cat("Windows (W) =", round(best_solution[2], 3), "\n")
cat("Profit =", round(best_profit, 2), "\n")

