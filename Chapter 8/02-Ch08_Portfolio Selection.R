# Clear the environment
rm(list = ls())

# Load nloptr package ----
library(nloptr)

# Algebraic Model -----
# Objective function:
# Risk = (0.25S1)^2 + (0.45S2)^2 + (0.05S3)^2 
#        + 2(0.04)S1S2 + 2(-0.005)S1S3 + 2(-0.01)S2S3

# Constraints:
# 21S1 + 30S2 + 8S3 >= 18
# S1 + S2 + S3 == 1
# S1 >= 0, S2 >= 0, S3 >= 0

# General form of NLP
# Min f(x)        objective function
# such that
# g(x) <= 0       inequality constraints
# h(x) = 0        equality constraints
# xL <= x <= xU   lower and upper bounds

# Define the objective function: portfolio variance
objective <- function(x) {
  return((0.25 * x[1])^2 + (0.45 * x[2])^2 + (0.05 * x[3])^2 
         + 2 * (0.04)   * x[1] * x[2] 
         + 2 * (-0.005) * x[1] * x[3] 
         + 2 * (-0.01)  * x[2] * x[3])
}

# Define the inequality constraint: expected return must be at least 18
ineq_constraints <- function(x) {
  return(c(
    18 - (21 * x[1] + 30 * x[2] + 8 * x[3]) 
    # 21S1 + 30S2 + 8S3 >= 18 -> 18 - (21S1 + 30S2 + 8S3) <= 0
  ))
}

# Define the equality constraint: portfolio weights must add to 1
eq_constraints <- function(x) {
  return(x[1] + x[2] + x[3] - 1)
  # S1 + S2 + S3 = 1 -> # S1 + S2 + S3 - 1 <= 0
}

# Define the bounds for variables
lower_bounds <- c(0, 0, 0)
upper_bounds <- c(1, 1, 1)

# Set the optimization options
opts <- list(
  "algorithm" = "NLOPT_LN_COBYLA", # COBYLA algorithm for nonlinear programming
  "xtol_rel" = 1.0e-8,             # Stop when new solutions become almost identical
  "tol_constraints_eq" = 1.0e-8    # Tolerance for the equality constraint
)

# Solve the nonlinear programming problem
sol <- nloptr(x0 = c(0, 1, 0), # feasible starting values
              eval_f = objective,
              lb = lower_bounds,
              ub = upper_bounds,
              eval_g_ineq = ineq_constraints,
              eval_g_eq = eq_constraints,
              opts = opts)

# Extract the solution
solution <- sol$solution
objfun <- sol$objective

# Print the solution
cat("Solution (Percentages):", round(solution * 100, 2), "%\n")
cat("Objective Value (Variance):", round(objfun, 6), "\n")
cat("Objective Value (Standard Deviation):", round(sqrt(objfun) * 100, 2), "%\n")

# Perform parameter analysis replicating fig. 8.14 ----
min_return <- 10
max_return <- 30

# Initialize a data frame to store results
results <- data.frame(
  Minimum.Expected.Return = numeric(),
  Stock1 = numeric(),
  Stock2 = numeric(),
  Stock3 = numeric(),
  Variance = numeric(),
  Standard.Deviation = numeric(),
  Return = numeric()
)

for (i in seq(min_return, max_return, by = 2)) {
  
  # Define the modified inequality constraint
  modified_ineq_constraints <- function(x) {
    return(c(
      i - (21 * x[1] + 30 * x[2] + 8 * x[3])
    ))
  }
  
  # Solve the modified nonlinear programming problem
  sol <- nloptr(x0 = c(0, 1, 0),
                eval_f = objective,
                lb = lower_bounds,
                ub = upper_bounds,
                eval_g_ineq = modified_ineq_constraints,
                eval_g_eq = eq_constraints,
                opts = opts)
  
  # Extract the solution
  solution <- sol$solution
  objfun <- sol$objective
  
  # Calculate actual return
  actual_return <- 21 * solution[1] + 30 * solution[2] + 8 * solution[3]
  
  # Append the results to the data frame
  results <- rbind(
    results,
    data.frame(
      Minimum.Expected.Return = i,
      Stock1 = round(solution[1] * 100, 2),
      Stock2 = round(solution[2] * 100, 2),
      Stock3 = round(solution[3] * 100, 2),
      Variance = round(objfun, 4),
      Standard.Deviation = round(sqrt(objfun) * 100, 1),
      Return = round(actual_return, 1)
    )
  )
}

# Print the results as a table
print(results)


# Load ggplot2 package
library(ggplot2)

# Plot: risk on x-axis, return on y-axis
ggplot(results, aes(x = Standard.Deviation, y = Return)) +
  geom_line() +
  geom_point() +
  labs(
    title = "Parameter Analysis: Risk vs. Return",
    x = "Standard Deviation (%)",
    y = "Return (%)"
  )

