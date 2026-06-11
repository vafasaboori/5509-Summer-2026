# Clearing the Environment ----
rm(list=ls())

# Import lpSolve package ----
library(lpSolve)

# Algebraic Model ----
# Min C = 1TV + 2PM in Millions (Objective Function)
# Subject to (Constraints)
#  0TV + 1PM >= 3
#  3TV + 2PM >= 18
# -1TV + 4PM >= 4
# TV >= 0
# PM >= 0

# Original Problem ----
# Set coefficients of the objective function
f.obj <- c(1, 2)

# Set matrix corresponding to coefficients of constraints by rows
# Do not consider the non-negativity constraint; it is automatically assumed
f.con <- matrix(c(0, 1,
                  3, 2,
                 -1, 4), nrow = 3, byrow = TRUE)

# nrow: the desired number of rows.
# byrow: If FALSE the matrix is filled by columns, otherwise the filled by rows.

# Set inequality signs
f.dir <- c(">=",
           ">=",
           ">=")

# Set right hand side parameters
f.par <- c(3,
           18,
           4)

# Solve the linear programming problem
lp_result <- lp("min", f.obj, f.con, f.dir, f.par)

# Print the final value (p)
lp_result

# Print variable final values
lp_result$solution


# Sensitivity Analysis ----

# Perform sensitivity analysis 
sens <- lp("min", f.obj, f.con, f.dir, f.par, compute.sens = TRUE)

# Perform sensitivity analysis for objective function coefficients
round(sens$sens.coef.from, 3)
round(sens$sens.coef.to, 3)

# Perform sensitivity analysis for shadow prices
round(sens$duals, 3)

# Sensitivity Analysis (Allowable RHS, constraints first, variables next)
options(scipen = 0, digits = 3)
round(sens$duals.from, 3)
round(sens$duals.to, 3)

# Additional Steps: In Class Exercise (Chance Constraint) ----
# What if the estimates for increase in sales of liquid detergent are uncertain?
# range of uncertainty for increase in sales per TV is uniform (2.5–3.5)
# range of uncertainty for increase in sales per Print Media is uniform (1.5–2.5)
# This constraint be satisfied at least 95% of the time (chance constraint)

# In Class Exercise Solution ----

# Function checks if the 2nd constraint is satisfied at least 95% of the time under uncertainty:
# where a and b are random values from uniform distributions (TV and Print media effectiveness)
check_feasibility <- function(TV, PM, num_simulations = 1000) {
  
  count <- 0  # Start a counter to track how many times the constraint is satisfied
  
  # Run simulations to sample different possible values for the uncertain parameters
  for (i in 1:num_simulations) {
    
    # Randomly generate one value for sales increase per TV ad (a ~ Uniform[2.5, 3.5])
    a <- runif(1, 2.5, 3.5)
    
    # Randomly generate one  value for sales increase per Print ad (b ~ Uniform[1.5, 2.5])
    b <- runif(1, 1.5, 2.5)
    
    # Calculate the total expected increase in liquid detergent sales for this simulation
    total_increase <- TV * a + PM * b
    
    # If the total increase is at least 18, count it as a success
    if (total_increase >= 18) {
      count <- count + 1
    }
  }
  
  # Return the proportion of simulations where the constraint is satisfied
  return(count / num_simulations)  # This gives a value between 0 and 1
}

# Grid search for fixed values of TV and PM
# Try different combinations of TV and PM to find the one that:
# - Satisfies all deterministic constraints
# - Satisfies the chance constraint for liquid detergent (≥ 95% of the time)
# - Minimizes total advertising cost

best_cost <- Inf                 # Start with the highest possible cost (infinity)
best_solution <- c(NA, NA)       # Placeholder for the best combination of TV and PM

# Try different values for TV from 0 to 10, in increments of 0.1
for (TV in seq(0, 10, by = 0.1)) {
  
  # Try different values for PM from 0 to 10, in increments of 0.1
  for (PM in seq(0, 10, by = 0.1)) {
    
    # Check if the values satisfy the two deterministic constraints:
    # Constraint 1: PM ≥ 3 (Stain remover)
    # Constraint 3: -TV + 4*PM ≥ 4 (Powder detergent)
    if ((PM >= 3) && ((-1 * TV + 4 * PM) >= 4)) {
      
      # Check if the liquid detergent constraint is satisfied in ≥ 95% of simulations
      satisfied_prob <- check_feasibility(TV, PM)
      
      if (satisfied_prob >= 0.95) {
        
        # Compute total cost: 1M per TV + 2M per PM
        cost <- 1 * TV + 2 * PM
        
        # If this cost is the lowest so far, update the best solution
        if (cost < best_cost) {
          best_cost <- cost
          best_solution <- c(TV, PM)
        }
      }
    }
  }
}

# Display the best decision found that satisfies all constraints (including chance constraint)
cat("Best solution with 95% chance constraint:\n")
cat("TV =", best_solution[1], "\n")
cat("PM =", best_solution[2], "\n")
cat("Total Cost (in millions) =", best_cost, "\n")

