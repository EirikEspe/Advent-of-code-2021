#####################################################
#
# Advent of Code 2022
# Day 4: Giant Squid
#
#####################################################

# Load the dplyr package
library(dplyr)

# Read the numbers data
numbers <- readLines("Input/day4.txt", n = 1)

# Create a numeric vector of the input numbers
numbers <- as.integer(
  strsplit(numbers, split = ",")[[1]])


# Read the board data
board_data <- readr::read_table2("Input/day4.txt", col_names = FALSE, skip = 1)


#--- Part One ----

# Bingo is played on a set of boards each consisting of a 5x5 grid of numbers. 
# Numbers are chosen at random, and the chosen number is marked on all boards on 
# which it appears. (Numbers may not appear on all boards.) If all numbers in 
# any row or any column of a board are marked, that board wins. (Diagonals don't 
# count.)

# The submarine has a bingo subsystem to help passengers pass the time. (Currently, 
# the passengers are you and a giant squid that has attached itself to the outside 
# of the submarine). The bingo subsystem automatically generates a random order 
# in which to draw numbers and a random set of boards


# Start by finding the sum of all unmarked numbers on the winning board. Then, 
# multiply that sum by the number that was just called when the board won.

# To guarantee victory against the giant squid, figure out which board will win first. 
# What will your final score be if you choose that board?

#-------------------------------------------------------------------------------

# Add identifier for the boards (a column with the number of the board)
board_data <- board_data %>% 
  mutate(board = rep(1:(nrow(board_data) / 5), each = 5), .before = 1)


# Create a loop that tracks bingo after i numbers are drawn
for (i in 5:length(numbers)) {
  # Check for bingo in rows
  bingo_row_df <- board_data %>%
    rowwise(board) %>% 
    summarise(totalRow = all(c_across(starts_with("X")) %in% numbers[1:i]), 
              .groups = "drop")
  
  bingo_rows <- bingo_row_df %>% 
    pull(totalRow) %>% 
    any() 
  
  # Check for bingo in columns
  bingo_col_df <- board_data %>% 
    group_by(board) %>% 
    summarise(across(starts_with("X"), ~all(.x %in% numbers[1:i])), 
              .groups = "drop") %>% 
    tidyr::pivot_longer(cols = starts_with("X"), 
                        names_to = "column", 
                        values_to = "bingo")
    
  bingo_cols <- bingo_col_df %>%  
    pull(bingo) %>% 
    any()
  
  if (bingo_rows | bingo_cols) {
    winning_board <- if (bingo_rows) {
      bingo_row_df %>% filter(totalRow) %>% pull(board) 
      } else if (bingo_cols) {
        bingo_col_df %>% filter(bingo) %>% pull(board)
        }
    
    print(paste("Win by rows:", bingo_rows))
    print(paste("Win by columns:", bingo_cols))
    print(paste("Winning board:", winning_board))
    break
  }
}
## Win by rows: FALSE
## Win by columns: TRUE
## Winning board: 70

# Sum of all umarked numbers
unmarked_sum1 <- board_data %>% 
  filter(board == winning_board) %>% 
  select(-board) %>% 
  as.matrix() %>% 
  setdiff(numbers[1:i]) %>% 
  sum()
## 788

# Last called number
numbers[i]
## 91

# Final score
unmarked_sum1 * numbers[i]
## 71 708


#--- Part Two ----

# On the other hand, it might be wise to try a different strategy: let the giant 
# squid win.

# The safe thing to do is to figure out which board will win last and choose 
# that one. That way, no matter which boards it picks, it will win for sure.

# Figure out which board will win last. Once it wins, what would its final score be?


# Create a dataframe to filter for the last winning board
last_board <- board_data


# Create a loop that will filter out winning boards.
# The loop will stop when we have 1 board left.
for (n in 1:length(numbers)) {
  # Check for bingo in rows
  bingo_row_df <- last_board %>%
    rowwise(board) %>% 
    summarise(totalRow = all(c_across(starts_with("X")) %in% numbers[1:n]), 
              .groups = "drop")
  
  bingo_rows <- bingo_row_df %>% 
    pull(totalRow) %>% 
    any()
  
  
  # Check for bingo in columns
  bingo_col_df <- last_board %>% 
    group_by(board) %>% 
    summarise(across(starts_with("X"), ~all(.x %in% numbers[1:n])), 
              .groups = "drop") %>% 
    tidyr::pivot_longer(cols = starts_with("X"), 
                        names_to = "column", 
                        values_to = "bingo")
  
  bingo_cols <- bingo_col_df %>%  
    pull(bingo) %>% 
    any()
  
  if (bingo_rows) {
    winning_board <- bingo_row_df %>% filter(totalRow) %>% pull(board)
    
    last_board <- last_board %>% filter(!(board %in% winning_board))
    
    if (length(unique(last_board$board)) == 1) {
      break 
    }
  }
  
  if (bingo_cols) {
    winning_board <- bingo_col_df %>% filter(bingo) %>% pull(board)
    
    last_board <- last_board %>% filter(!(board %in% winning_board))
    
    if (length(unique(last_board$board)) == 1) {
      break 
    }
  }
}


# Next, we create a loop that will find when the last winning board will win
for (t in (n+1):length(numbers)) {
  # Check for bingo in rows
  bingo_row_df <- last_board %>%
    rowwise(board) %>% 
    summarise(totalRow = all(c_across(starts_with("X")) %in% numbers[1:t]), 
              .groups = "drop")
  
  bingo_rows <- bingo_row_df %>% 
    pull(totalRow) %>% 
    any() 
  
  # Check for bingo in columns
  bingo_col_df <- last_board %>% 
    group_by(board) %>% 
    summarise(across(starts_with("X"), ~all(.x %in% numbers[1:t])), 
              .groups = "drop") %>% 
    tidyr::pivot_longer(cols = starts_with("X"), 
                        names_to = "column", 
                        values_to = "bingo")
  
  bingo_cols <- bingo_col_df %>%  
    pull(bingo) %>% 
    any()
  
  if (bingo_rows | bingo_cols) {
    last_winning <- if (bingo_rows) {
      bingo_row_df %>% filter(totalRow) %>% pull(board) 
    } else if (bingo_cols) {
      bingo_col_df %>% filter(bingo) %>% pull(board)
    }
    
    print(paste("Last draw:", numbers[t]))
    print(paste("Last winning board:", last_winning))
    break
  }
}
## Last draw: 97
## Last winning board: 10


# Sum of all umarked numbers
unmarked_sum2 <- last_board %>% 
  select(-board) %>% 
  as.matrix() %>% 
  setdiff(numbers[1:t]) %>% 
  sum()
## 358


# Final score
unmarked_sum2 * numbers[t]
## 34 726
