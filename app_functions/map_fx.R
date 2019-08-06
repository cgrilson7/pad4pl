# Functions to map signals between 384- and 96-well plates

# Dependencies:
library(tidyverse)
# library(readxl)

# Helper Functions --------------------------------------------------------

## odd: check if x is odd
odd <- function(x) {
  return(x %% 2 == 1)
}

## even: check if x is even
even <- function(x) {
  return(x %% 2 == 0)
}

## is.blank: a blanket check for NULL/NA/NaN, especially useful to check values passed to functions (like map_384_to_96).
is.blank <- function(x, false.triggers = FALSE) {
  if (is.function(x))
    return(FALSE) # Some of the tests below trigger warnings when used on functions
  return(is.null(x) ||
           length(x) == 0 ||
           all(is.na(x)) ||
           all(x == "") ||
           (false.triggers && all(!x)))
}

## row2num: converts an uppercase row indicator ("A"-"P") to an integer (1-16)
row2num <- function(x) {
  result = utf8ToInt(x) - utf8ToInt("A") + 1L
  if (is.blank(result) | result < 1 | result > 26) {
    return(NULL)
  } else {
    return(result)
  }
}

## num2row: converts back!
num2row <- function(x) {
  if (is.blank(x) | x < 1 | x > 26)
    return(NULL)
  else
    return(intToUtf8(x + 64))
}

## stamp: returns the 96-well plate number, mod 16.
stamp <- function(row, col) {
  if (odd(row) & odd(col))
    return(1)
  else if (even(row) & odd(col))
    return(2)
  else if (odd(row) & even(col))
    return(3)
  else
    return(4)
}


# Map 384- to 96-well plates ----------------------------------------------

# Main function: map384_96
  # Description:
    # Vectorized function that maps the readout from a position on a given 384-well plate to the source 96-well plate and position.

# Arguments:
  # num384: plate number assigned to the input 384-well plate (1-16)
  # pos384: the 384-well position; e.g. "A1", "H15", "P24"
    # Can either pass pos384, or the decomposed row and column numbers, row384 and col384:
      # row384: the letter of the 384-well position converted to numeric position in the alphabet ("A" = 1, "B"= 2,..., "P" = 16)
      # col384: the number of the 384-well position

map384_96 <- function(num384, pos384, row384 = NULL, col384 = NULL) {
  # Check for necessary inputs
  if (is.blank(num384)) { # must have number of 384-well plate the signal came from
    cat("Missing 384-well plate number.")
    return(NULL)
  }
  if (is.blank(pos384)) { # if missing pos384, we must have row384 and col384
    if (is.blank(row384 | col384)) {
      cat("Missing 384-well plate position.")
      return(NULL)
    }
  } else { # convert pos384 to numeric row and numeric column, and set them equal to row384 and col384
    row384 = row2num(str_extract(pos384, "[A-P]+"))
    col384 = as.integer(str_extract(pos384, "[0-9]+"))
  }
  
  # To get pos96, just need to divide by 2 and take the ceiling of the result for each given row384 & col384
  row96 <- ceiling(row384 / 2)
  col96 <- ceiling(col384 / 2)
  pos96 <- paste0(num2row(row96), col96) # to convert to "A1" format, concatenate the letter representation of the row96 and the number of col96
  
  # We have 96-well plate position... now just need to find the plate number (num96) based on num384
  if (num384 %% 4 == 1) { # then num96 one of 1-4. Take the result from stamp
    num96 <- stamp(row384, col384)
  } else if (num384 %% 4 == 2) { # then num96 one of 5-8.  Add 4 to the result of stamp
    num96 <- stamp(row384, col384) + 4
  } else if (num384 %% 4 == 3) { # then num96 one of 9-12. Add 8.
    num96 <- stamp(row384, col384) + 8
  } else { # num96 one of 13-16. Add 12.
    num96 <- stamp(row384, col384) + 12
  }
  
  # Function complete. Return (as a list) the 96-well plate number and the 96-well plate position (in alphanumeric form and numeric [row,col] form)
  
  return(list(num96=num96, pos96=pos96, row96=row96, col96=col96))
}


# Map 96- to 384-well plates -----------------------------------------------

# Main function: map96_384
  # Description:
    # Vectorized function that maps 96-well plate and positions to 384-well signals.
  # Arguments:
    # num96: plate number assigned to the input 96-well plate (1-16)
    # pos96: the 96-well position; e.g. "A1", "H15", "P24"
      # Can either pass pos96, or the decomposed row and column numbers, row96 and col96:
       # row96: the letter of the 96-well position converted to numeric position in the alphabet ("A" = 1, "B"= 2,..., "P" = 16)
       # col96: the number of the 96-well position

map96_384 <- function(sk_design, num96, pos96, row96 = NULL, col96 = NULL) {
  # Check for necessary inputs
  if(is.blank(sk_design)){
    cat("Missing sk_design (need this to differentiate between 384-well plates).")
    return(NULL)
  }
  if (is.blank(num96)) { # must have number of 96-well plate
    cat("Missing 96-well plate number.")
    return(NULL)
  }
  
  if (is.blank(pos96)) { # if missing pos96, we must have row96 and col96
    if (is.blank(row96 | col96)) {
      cat("Missing 96-well plate position.")
      return(NULL)
    }
  } else { # convert pos96 to numeric row and numeric column, and set them equal to row96 and col96
    row96 = row2num(str_extract(pos96, "[A-H]+"))
    col96 = as.integer(str_extract(pos96, "[0-9]+"))
  }
  
  # Three calculations to identify what 384-well plate and in what well the 96-well experiment is in:
  # 1) num384
  # The experiment will sit in 
  # 384-well #1 if from 96-well #1-4,
  # 384-well #2 if from 96-well #5-8,
  # 384-well #3 if from 96-well #9-12,
  # 384-well #4 if from 96-well #13-16.
  # To get this, just divide sk_design by 384 and take the ceiling.
  num384 = ceiling(sk_design/384)
  
  # 2) row384
  
  # According to the stamping pattern:
  # [i] [k] = [1 | 5 |  9 | 13]   [3 | 7 | 11 | 15]
  # [j] [l] = [2 | 6 | 10 | 14]   [4 | 8 | 12 | 16]
  
  # The 384-well row number in which a given 96-well experiment resides is
  # just the 96-well row number doubled, minus 1 if the 96-well plate number is 
  # divisible by two (the lower two rows of the stamping pattern). 
  # In other words:
  row384 = row96*2-(num96%%2)
  
  # 3) col384
  # The 384-well column number '' '' '' '' 
  # is the 96-well column number doubled, minus 1 if the 96-well plate number
  # divided by 4 is either 1 or 2 (the left two columns of the stamping pattern)
  col384 = col96*2-ifelse(num96%%4 %in% c(1,2), 1, 0)
  
  # Concatenate the row number (converted to character) and column number to form pos384
  pos384 = paste0(num2row(row384), col384)
  
  # Function complete. Return (as a list) the 384-well plate number and the 384-well plate position (in alphanumeric form and numeric [row,col] form)
  
  return(list(num384=num384, pos384=pos384, row384=row384, col384=col384))
  # ^ The reason for these equalities is that we are naming the components in the list,
  # so that later we can access or manipulate them more easily. The name to the left of
  # the equals could be anything; to the right are the values we calculated.
}

# # Example usage -----------------------------------------------------------
# 
# # Ex. #1
#   # map96_384()
#     # Mapping design file (96-well plate numbers and positions) to 384-well plates
#     # Look to the final 3 columns for the 384-well information!
# 
# # Read in design file
# design <- read_excel("data/designs/HT0030_design.xlsx") %>%
#   # Call the map96_384 function. purrr::pmap() is a variant of purrr::map() that iterates over multiple arguments simultaneously
#   mutate(info_384 = purrr::pmap(list(sk_design, P96, TPOS), map96_384)) %>%
#   # Split apart the info_384 list-column into separate columns
#   mutate(info_384 = purrr::map(info_384, as_tibble)) %>% unnest()
# 
# # Ex. #2
#   # map384_96()
#     # Mapping signal file (384-well plate numbers and positions) to 96-well plates
#     # Look to the final 3 columns for the 96-well information!
# 
# # Read in example signal data:
# example_signals <- read_csv("data/tests/20170311 TAB Triage Summary Data.csv") %>%
#   # Get 384-well plate number from 'PlateName' column
#   mutate(num384 = PlateName - 1000000000) %>%
#   # Call the map384_96 function.
#   mutate(info_96 = purrr::pmap(list(num384, Well), map384_96)) %>%
#   # Split the info_96 list-column into separate columns
#   mutate(info_96 = purrr::map(info_96, as_tibble)) %>% unnest()