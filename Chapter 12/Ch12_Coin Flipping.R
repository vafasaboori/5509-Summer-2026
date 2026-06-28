# Clearing the Environment ----
rm(list = ls())  

# Function to simulate the coin‐flip game and return flips needed
simulate_game <- function() {
  flips <- 0      # Counter for how many flips have occurred
  heads <- 0      # Counter for heads
  tails <- 0      # Counter for tails
  
  # Keep flipping until heads and tails differ by 3
  while (abs(heads - tails) < 3) {
    flips <- flips + 1
    coin <- sample(c("H", "T"), 1, replace = TRUE)  # Randomly pick "H" or "T"
    
    if (coin == "H") {
      heads <- heads + 1  # Increment heads count
    } else {
      tails <- tails + 1  # Increment tails count
    }
  }
  
  return(flips)  # Return total flips once |heads - tails| >= 3
}

# Set up simulation parameters
num_simulations <- 1000  # Number of times to play the game

# Initialize accumulators for results
total_flips <- 0
total_winnings <- 0

# Run the simulation (play) 1000 times
for (i in 1:num_simulations) {
  flips <- simulate_game()         # How many flips in this play
  winnings <- 8 - flips            # Winnings formula: $8 minus flips
  
  total_flips <- total_flips + flips       # Sum flips across plays
  total_winnings <- total_winnings + winnings  # Sum winnings across plays
}

# Compute averages over all simulations
average_flips <- total_flips / num_simulations
average_winnings <- total_winnings / num_simulations

# Display the results
cat("Average Number of Flips:", average_flips, "\n")
cat("Average Amount Won:", average_winnings, "\n")

