# Clearing the Environment ----
rm(list = ls()) 

# Define the breakdown and replacement costs
breakdown_cost   <- 11000  # Cost incurred if machine breaks down
replacement_cost <-  6000  # Cost to proactively replace the machine

# Function to simulate 1000 replacement cycles and compute average cost per day
calculate_average_cost_per_day <- function(option) {
  num_cycles  <- 1000  # Number of simulated breakdown/replacement scenarios
  total_cost  <- 0      # Accumulates costs across cycles
  total_days  <- 0      # Accumulates operational days across cycles
  
  for (i in 1:num_cycles) {
    # Randomly draw "days until breakdown": 4, 5, or 6 days with given probabilities
    days_until_breakdown <- sample(c(4, 5, 6), size = 1, prob = c(0.25, 0.5, 0.25))
    total_days <- total_days + days_until_breakdown  # Add to total days
    
    # Determine cost based on the chosen maintenance strategy
    # switch() is a compact way to pick a code paths based on a value (multi-branch if/else)
    # A short R script explaining "switch()" is provided.
        switch(option,
           "No Preventive Maintenance" = {
             # Never replace early: always incur breakdown cost
             total_cost <- total_cost + breakdown_cost
           },
           "Replace After 4 Days" = {
             # If breakdown happens after day 4, replace at day 4; otherwise pay breakdown cost
             if (days_until_breakdown > 4) {
               total_cost <- total_cost + replacement_cost
             } else {
               total_cost <- total_cost + breakdown_cost
             }
           },
           "Replace After 5 Days" = {
             # If breakdown happens after day 5, replace at day 5; otherwise pay breakdown cost
             if (days_until_breakdown > 5) {
               total_cost <- total_cost + replacement_cost
             } else {
               total_cost <- total_cost + breakdown_cost
             }
           }
    )
  }
  
  # Compute and return the average cost per operational day
  average_cost_per_day <- total_cost / total_days
  return(average_cost_per_day)
}

# Run simulations for each option
no_preventive_maintenance <- calculate_average_cost_per_day("No Preventive Maintenance")
replace_after_4_days      <- calculate_average_cost_per_day("Replace After 4 Days")
replace_after_5_days      <- calculate_average_cost_per_day("Replace After 5 Days")

# Compare and display the results
comparison <- data.frame(
  Option = c("No Preventive Maintenance", "Replace After 4 Days", "Replace After 5 Days"),
  Average_Cost_Per_Day = c(no_preventive_maintenance, replace_after_4_days, replace_after_5_days)
)
print(comparison)
