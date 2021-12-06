#####################################################
#
# Advent of Code 2022
# Day 2: Dive!
#
#####################################################

# Read the data
directions <- readr::read_table2("Input/day2.txt", col_names = c("direction", "units"))


#--- Part One ----

# It seems like the submarine can take a series of commands like 
# forward 1, down 2, or up 3:

# forward X increases the horizontal position by X units.
# down X increases the depth by X units.
# up X decreases the depth by X units.

# Your horizontal position and depth both start at 0.
# After following the instructions, you would have a horizontal position and
# a depth. 


# Calculate horizontal position and depth after following the planned course

# Horizontal position
horizontal_position <- with(directions, sum(units[direction == "forward"]))
## 1911

# Depth
depth <- with(directions, 
              sum(units[direction == "down"]) - sum(units[direction == "up"]))
## 724

# Horizontal position multiplied by depth
horizontal_position * depth
## 1 383 564



#--- Part Two ----

# It turns out that the process is actually slightly more complicated.
# In addition to horizontal position and depth, you'll also need to track 
# a third value, aim, which also starts at 0. 
# The commands also mean something entirely different than you first thought:

#   - down X increases your aim by X units.
#   - up X decreases your aim by X units.
#   - forward X does two things:
#       - It increases your horizontal position by X units.
#       - It increases your depth by your aim multiplied by X.

# Using this new interpretation of the commands, calculate the horizontal position and 
# depth you would have after following the planned course. 
# What do you get if you multiply your final horizontal position by your final depth?

#-------------------------------------------------------------------------------

# Create a column that tracks aim
#   - forward X does not change aim 
#   - up X decreases your aim by X units.
#   - down X increases your aim by X units.
directions$aim <- with(directions, 
                       cumsum(ifelse(direction == "forward", 0, 
                                     ifelse(direction == "up", -units, units))))

# Create a column that tracks the depth
#   - forward X increases your depth by your aim multiplied by X
directions$depth <- with(directions, 
                         cumsum(ifelse(direction == "forward", aim * units, 0)))


# Multiply final horizontal position with final depth

## For the horizontal position, we can use the value from Part One
## For the depth we use the final depth from our directions dataframe 
horizontal_position * tail(directions$depth, n = 1)
## 1 488 311 643




### Dplyr solutions

# My first attempt at this puzzle was with the dplyr package.
# These alternatives can be found below:


# Load the dplyr package
library(dplyr)

#--- Part One

## First, we convert the units of 'up' to negative numbers. 
##  Remember: 'Up' decreases the depth.
## Second, we combine the 'up' and 'down' directions to a category called 'depth'.

## Then we group by direction and calculate the horizontal position and depth
directions %>%
  mutate(units = if_else(direction == "up", -units, units),
         direction = recode(direction, "down" = "depth", "up" = "depth")) %>% 
  group_by(direction) %>% 
  summarise(total = sum(units), .groups = "drop") %>% 
  mutate(product = c(NA_real_, prod(total)))


#--- Part Two

# First, we add a column that tracks changes in aim. This is the column named 'aim_change'
# Second, we add a column with the running total of the 'aim' value.
# Third, we create a variable that tracks changes in depth.
# Fourth, we create a variable with the running total of the depth.
# And fifth, we track the horizontal position.
directions %>% 
  mutate(aim_change = case_when(direction == "forward" ~ 0, 
                                direction == "up" ~ -units, 
                                TRUE ~ units), 
         cumulative_aim = cumsum(aim_change), 
         depth_change = if_else(direction == "forward", units * cumulative_aim, 0), 
         depth = cumsum(depth_change), 
         horizontal_position = cumsum(if_else(direction == "forward", units, 0))) %>% 
  # Subset the last row
  slice_tail(n = 1) %>% 
  # Multiply the final horizontal position by the final depth
  summarise(product = horizontal_position * depth)
## Horizontal position:   1 911
## Depth:               778 813
## Horizontal position * Depth = 1 488 311 643
