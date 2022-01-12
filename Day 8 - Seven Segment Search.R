#####################################################
#
# Advent of Code 2022
# Day 8: Seven Segment Search
#
#####################################################

# Load the dplyr package
library(dplyr)

# Read the data
notes <- readr::read_delim("Input/day8.txt", "|", 
                           escape_double = FALSE, col_names = c("input", "output"), 
                           trim_ws = TRUE)


# You barely reach the safety of the cave when the whale smashes into the cave mouth, 
# collapsing it. Sensors indicate another exit to this cave at a much greater depth, 
# so you have no choice but to press on.

# As your submarine slowly makes its way through the cave system, you notice that 
# the four-digit seven-segment displays in your submarine are malfunctioning; 
# they must have been damaged during the escape. You'll be in a lot of trouble 
# without them, so you'd better figure out what's wrong.

# Each digit of a seven-segment display is rendered by turning on or off any of 
# seven segments named a through g:

#      0:         1:        2:        3:        4:
#     aaaa      ....      aaaa      aaaa      ....
#    b    c    .    c    .    c    .    c    b    c
#    b    c    .    c    .    c    .    c    b    c
#     ....      ....      dddd      dddd      dddd
#    e    f    .    f    e    .    .    f    .    f
#    e    f    .    f    e    .    .    f    .    f
#     gggg      ....      gggg      gggg      ....

#       5:        6:       7:        8:        9:
#     aaaa      aaaa      aaaa      aaaa      aaaa
#    b    .    b    .    .    c    b    c    b    c
#    b    .    b    .    .    c    b    c    b    c
#     dddd      dddd      ....      dddd      dddd
#    .    f    e    f    .    f    e    f    .    f
#    .    f    e    f    .    f    e    f    .    f
#     gggg      gggg      ....      gggg      gggg

# So, to render a 1, only segments c and f would be turned on; the rest would be off. 
# To render a 7, only segments a, c, and f would be turned on.


#--- Part One ----

# In the output values, how many times do digits 1, 4, 7, or 8 appear?
notes %>% 
  tidyr::separate_rows(output) %>% 
  filter(nchar(output) %in% c(2, 3, 4, 7)) %>% 
  summarise(count_output_criteria = length(output))
## 245


#--- Part Two ----

# Through a little deduction, you should now be able to determine the remaining digits.
# For each entry, determine all of the wire/segment connections and decode 
# the four-digit output values. What do you get if you add up all of the output values?


#--- Decode the input ----

# Create a dataframe for decoding the input
input_notes <- notes %>% 
  select(input) %>% 
  # Add an id column for each entry
  mutate(input_id = row_number(), .before = 1) %>% 
  tidyr::separate_rows(input) %>% 
  # Add an id column for each element in the ten unique signal patterns
  mutate(signal_pattern_id = row_number(), .before = input) %>%
  # Count letters of the signal patterns
  mutate(input_length = nchar(input))


# Split the signal patterns into individual letters
input_notes <- input_notes %>% 
  mutate(input = strsplit(input, split = "")) %>% 
  tidyr::unnest(input)


# Create helper columns to render 0, 1, 3, 4, 6, 7, 8 and 9
# These digits can be identified by counting the length of the signal pattern and 
# checking whether the digits contain all the segments required to render a 1 or a 4
input_notes <- input_notes %>% 
  group_by(input_id) %>% 
  # Create two columns to check if the individual elements of the signal pattern is 
  # included in segments required to render a 1 or a 4
  mutate(input_in_segments_1 = input %in% input[input_length == 2], 
         input_in_segments_4 = input %in% input[input_length == 4]) %>% 
  
  # Group the data by signal pattern
  group_by(signal_pattern_id) %>%
  # Check that all the segments required to render a 1 or a 4 is present in 
  # the signal patterns, and drop the previous helper columns
  mutate(incl_segments_1 = sum(input_in_segments_1) == 2, 
         incl_segments_4 = sum(input_in_segments_4) == 4, 
         input_in_segments_1 = NULL, 
         input_in_segments_4 = NULL) %>% 
  ungroup()


# Identify digits 0, 1, 3, 4, 6, 7, 8 and 9 based on the length of 
# the signal pattern and the helper columns
input_notes <- input_notes %>%  
  mutate(digit = case_when(input_length == 2 ~ 1, 
                           input_length == 3 ~ 7, 
                           input_length == 4 ~ 4, 
                           input_length == 7 ~ 8, 
                           input_length == 5 & incl_segments_1 ~ 3, 
                           input_length == 6 & incl_segments_1 == FALSE ~ 6, 
                           input_length == 6 & incl_segments_4 ~ 9, 
                           input_length == 6 & incl_segments_4 == FALSE ~ 0))


# Next, we have to find digit 2 and 5
# Looking at the difference between 2 and 5 we can use the two segments that are
# the difference between 4 and 1 (|_). Both of these two segments are present in 5, 
# but only the horizontal part is present in 2. 
input_notes <- input_notes %>% 
  group_by(input_id) %>%
  mutate(diff_4_1 = toString(setdiff(input[digit == 4], input[digit == 1]))) %>% 
  group_by(signal_pattern_id) %>% 
  mutate(digit = case_when(is.na(digit) & 
                             all(strsplit(diff_4_1, split = ", ")[[1]] %in% input) ~ 5, 
                           is.na(digit) ~ 2, 
                           TRUE ~ digit), 
         diff_4_1 = NULL) %>% 
  ungroup()


# Sort and combine the letters of the signal patterns
input_notes <- input_notes %>% 
  group_by(signal_pattern_id) %>% 
  mutate(input = stringr::str_sort(input), 
         input = stringr::str_flatten(input)) %>% 
  ungroup() %>% 
  filter(!duplicated(signal_pattern_id))
# (Since we used mutate and not summarise, we have some duplicated signal_pattern_ids.
#  The last line removes these. Mutate is chosen in order to keep the variables 
#  input_id, digit etc.)


#--- Decode the output ----

# Create a dataframe for the outputs in a similar way as for the inputs
output_notes <- notes %>% 
  select(output) %>% 
  mutate(output_id = row_number(), .before = 1) %>% 
  tidyr::separate_rows(output) %>% 
  mutate(signal_pattern_id = row_number(), .before = output)


# Split the signal patterns into individual letters
output_notes <- output_notes %>% 
  mutate(output = strsplit(output, split = "")) %>% 
  tidyr::unnest(output)


# Sort the output segments and combine the letters back to a sorted signal pattern)
output_notes <- output_notes %>% 
  group_by(signal_pattern_id) %>% 
  mutate(output = stringr::str_sort(output), 
         output = stringr::str_flatten(output)) %>% 
  ungroup() %>% 
  filter(!duplicated(signal_pattern_id))


# Find the output digits from the decoded inputs and sum the digits
output_notes %>% 
  inner_join(input_notes %>% select(input_id, input, digit), 
             by = c("output_id" = "input_id", "output" = "input")) %>% 
  group_by(output_id) %>%
  # Combine the individual digits into output values
  summarise(output_value = as.numeric(stringr::str_flatten(digit)), .groups = "drop") %>% 
  summarise(total_output_value = sum(output_value))
