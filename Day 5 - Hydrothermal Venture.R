#####################################################
#
# Advent of Code 2022
# Day 5: Hydrothermal Venture
#
#####################################################

# Load the dplyr package and the ggplot2 package
library(dplyr)
library(ggplot2)

# Read the data
coordinates <- readr::read_csv("Input/day5.txt", col_names = c("x1", "y1x2", "y2"))


# Split the 'y1x2' column into y1 and x2
coordinates <- coordinates %>% 
  tidyr::separate(col = y1x2, into = c("y1", "x2"), sep = "->") %>% 
  mutate(across(y1:x2, as.numeric))


# You come across a field of hydrothermal vents on the ocean floor! These vents 
# constantly produce large, opaque clouds, so it would be best to avoid them 
# if possible.

# They tend to form in lines; the submarine helpfully produces a list of 
# nearby lines of vents for you to review. (your puzzle input)

# Each line of vents is given as a line segment in the format x1,y1 -> x2,y2 
# where x1,y1 are the coordinates of one end the line segment and x2,y2 are the 
# coordinates of the other end. These line segments include the points at both ends.

#   - An entry like 1,1 -> 1,3 covers points 1,1, 1,2, and 1,3.
#   - An entry like 9,7 -> 7,7 covers points 9,7, 8,7, and 7,7.

# To avoid the most dangerous areas, you need to determine the number of points 
# where at least two lines overlap.


#--- Part One ----

# For the first part we will only consider horizontal and vertical lines: 
# lines where either x1 = x2 or y1 = y2.


# Filter for horizontal or vertical lines, add an id column, and convert 
# to long format
coordinates_long <- coordinates %>% 
  filter(x1 == x2 | y1 == y2) %>% 
  mutate(id = row_number(), .before = 1) %>% 
  tidyr::pivot_longer(cols = -id, 
                      names_to = c(".value", "end"), 
                      names_pattern = "([xy])([12])")

# Plot the data
coordinates_long %>% 
  ggplot(aes(x = y, y = x)) + 
  geom_line(aes(group = id), position = position_dodge(width = 0.1)) + 
  theme_light() + 
  coord_flip()


# Identify all points covered by the line segments
points1 <- coordinates_long %>% 
  group_by(id) %>% 
  summarise(points_x = seq(first(x), last(x)), 
            points_y = seq(first(y), last(y)), 
            .groups = "drop")


# Drop id column
points1$id <- NULL

# Find overlaps 
sum(table(points1) > 1)
## 4655


# Alternative dplyr solution for finding overlaps
# points1 %>% 
#   count(points_x, points_y) %>% 
#   filter(n > 1) %>% 
#   count()


#--- Part Two ----

# Unfortunately, considering only horizontal and vertical lines doesn't give you 
# the full picture; you need to also consider diagonal lines.

# You still need to determine the number of points where at least two lines overlap.

# Consider all of the lines. At how many points do at least two lines overlap?

# Add an id column, and convert to long format
coord_long <- coordinates %>% 
  mutate(id = row_number(), .before = 1) %>% 
  tidyr::pivot_longer(cols = -id, 
                      names_to = c(".value", "end"), 
                      names_pattern = "([xy])([12])")

# Identify all points covered by the line segments
points2 <- coord_long %>% 
  group_by(id) %>% 
  summarise(points_x = seq(first(x), last(x)), 
            points_y = seq(first(y), last(y)), 
            .groups = "drop")


# Drop id column
points2$id <- NULL

# Find overlaps 
sum(table(points2) > 1)
## 20500
