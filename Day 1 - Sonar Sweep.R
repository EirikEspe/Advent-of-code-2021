#####################################################
#
# Advent of Code 2022
# Day 1: Sonar Sweep
#
#####################################################

# Read the data
depth <- as.integer(readLines("Input/day1.txt"))


#--- Part One ----

# The input data is measurements of the sea floor depth. As the submarine drops below 
# the surface of the ocean, it automatically performs a sonar sweep of the nearby 
# sea floor. Each line in the input data is a measurement of the sea floor depth as 
# the sweep looks further and further away from the submarine.

# The first order of business is to figure out how quickly the depth increases, 
# just so you know what you're dealing with - you never know if the keys will get 
# carried into deeper water by an ocean current or a fish or something.

# Find the number of times a depth measurement increases
sum(diff(depth) > 0)
## 1502


#--- Part Two ----

# Every single measurement isn't as useful as expected: there's just too much noise 
# in the data.

# Instead, consider sums of a three-measurement sliding window:

# 199  A                    
# 200  A B    
# 208  A B C                    A: 607 (N/A - no previous sum)
# 210    B C D                  B: 618 (increased)
# 200  E   C D                  C: 618 (no change)
# 207  E F   D                  D: 617 (decreased)
# 240  E F G                    E: 647 (increased)
# 269    F G H                  F: 716 (increased)
# 260      G H                  G: 769 (increased)
# 263        H                  H: 792 (increased)

# Your goal now is to count the number of times the sum of measurements in 
# this sliding window increases from the previous sum.



# Calculate the sum of each three-measurement window, 
# using the slide_int() function from the slider package
depth_slide <- slider::slide_int(depth, sum, .before = 2, .complete = TRUE)

# Find the number of times the three-measurement sliding windows increase
sum(diff(depth_slide) > 0, na.rm = TRUE)
## 1538
