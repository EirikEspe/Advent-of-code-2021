#####################################################
#
# Advent of Code 2022
# Day 3: Binary Diagnostic
#
#####################################################

# Read the data
diagnostic <- readLines("Input/day3.txt")


#--- Part One ----

# The submarine has been making some odd creaking noises, so you ask it to produce 
# a diagnostic report just in case.

# You need to use the binary numbers in the diagnostic report to generate two 
# new binary numbers (called the gamma rate and the epsilon rate). 
# The power consumption can then be found by multiplying the gamma rate by 
# the epsilon rate.

# Each bit in the gamma rate can be determined by finding the most common bit in 
# the corresponding position of all numbers in the diagnostic report.
# Similarly, each bit in the epsilon rate can be determined by finding the least 
# common bit from each position.

#-------------------------------------------------------------------------------

# Count number of bits for the 5 first binary numbers
nchar(diagnostic[1:5])
## All counts are 12


# Check if this is true for all input data
all(nchar(diagnostic) == 12)
## TRUE


# Create a matrix with count of bits of each binary number.
freqs <- sapply(1:12, function(i) 
  tapply(diagnostic, substr(diagnostic, start = i, stop = i), length))


# Combine the most common bits to a single character (gamma)
gamma <- paste(apply(freqs, 2, which.max) - 1, collapse = "")

# Combine the least common bits to a single character (epsilon)
epsilon <- paste(apply(freqs, 2, which.min) - 1, collapse = "")


# Convert gamma and epsilon to decimal and multiply them
strtoi(gamma, base = 2) * strtoi(epsilon, base = 2)
## 177 * 3918 = 693 468


#--- Part Two ----

# Next, you should verify the life support rating, which can be determined by 
# multiplying the oxygen generator rating by the CO2 scrubber rating.

# Filter out values until only one remains, using the following process:

# Start with the full list of binary numbers from your diagnostic report and consider 
# just the first bit of those numbers. Then:
#     - Keep only numbers selected by the bit criteria for the type of rating value 
#       for which you are searching. Discard numbers which do not match the bit criteria.
#     - If you only have one number left, stop; this is the rating value for which you are searching.
#     - Otherwise, repeat the process, considering the next bit to the right.

# The bit criteria depends on which type of rating value you want to find:
#     - To find oxygen generator rating, determine the most common value (0 or 1) in 
#       the current bit position, and keep only numbers with that bit in that position. 
#       If 0 and 1 are equally common, keep values with a 1 in the position 
#       being considered.

#     - To find CO2 scrubber rating, determine the least common value (0 or 1) in 
#       the current bit position, and keep only numbers with that bit in that position. 
#       If 0 and 1 are equally common, keep values with a 0 in the position 
#       being considered.



#--- Oxygen generator rating----
  
# Create the vector we want to filter to find the oxygen generator rating
oxygen <- diagnostic

# Create a for loop that filter the oxygen vector based on the bit criteria
for (i in 1:12) {
  # Count 0 and 1 for the current bit position
  frequencies <- tapply(oxygen, substr(oxygen, i, i), length)
  
  # Find the most common value
  # Since R use index starting at 1, we subtract 1 to get 0 or 1 (instead of 1 or 2)
  most_common <- which(frequencies == max(frequencies)) - 1
  # If 0 and 1 are equally common, keep values with a 1 (the last value of 0 and 1)
  most_common <- most_common[length(most_common)]
  
  # Subset the oxygen vector based on the most common value
  oxygen <- oxygen[substr(oxygen, i, i) == most_common]
  
  # If you only have one number left, stop; 
  #   this is the rating value for which you are searching.
  if (length(oxygen) == 1) {
    break
  }
}
## The oxygen generator rating is 001110100101


# Convert to decimal
strtoi(oxygen, base = 2)
## 933


#--- CO2 scrubber rating ----

# Create the vector we want to filter to find the CO2 scrubber rating
co2_scrubber <- diagnostic

# Create a for loop that filter the vector based on the bit criteria
for (i in 1:12) {
  # Count 0 and 1 for the current bit position
  frequenciesCO2 <- tapply(co2_scrubber, substr(co2_scrubber, i, i), length)
  
  # Find the least common value
  # Since R use index starting at 1, we subtract 1 to get 0 or 1 (instead of 1 or 2)
  least_common <- which.min(frequenciesCO2) - 1
  
  # Subset the CO2 scrubber vector based on the least common value
  co2_scrubber <- co2_scrubber[substr(co2_scrubber, i, i) == least_common]
  
  # If you only have one number left, stop;
  if (length(co2_scrubber) == 1) {
    break 
  }
}
## The CO2 scrubber rating is 111000100110


# Convert to decimal
strtoi(co2_scrubber, base = 2)
## 3622


# Multiply the oxygen generator rating by the CO2 scrubber rating
strtoi(oxygen, base = 2) * strtoi(co2_scrubber, base = 2)
## The life support rating of the submarine is 3 379 326


