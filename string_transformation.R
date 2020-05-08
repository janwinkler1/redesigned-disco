# Loading the packages
library(tidyverse)


# Setting seed for reproducable results
set.seed(123)

# Generating df with dummy data
df <- tibble(
  "ProjectID" = c(1:5), "ProjectName" = c("Project A", "Project B", "Project C", "Project D", "Project E"),
  "ProjectMainCatecory" = as.factor(sample(LETTERS, 5, FALSE)), "ProjectValue" = c("2.0 - 3.0 Mio USD", "300.0 Mio USD", "", "0.1 - 0.4 Mio USD", NA)
)
# Check output in console
df
# Alternatively use glimpse()

# Definition of base cases
case_1 <- "2.0 - 3.0 Mio USD"
case_2 <- "300.0 Mio USD"
case_3 <- ""
case_4 <- NA

# Replace NA functionality of tidyr
no_empties <- replace_na(case_4, "")

# Checking functionality of stringr functions
del_space <- str_replace_all(case_1, "[:space:]", "")
del_space
del_char <- str_replace_all(del_space, "[:alpha:]", "")
del_char

# Definiton of cost transform function
cost_transform <- function(string) {
  # Treat NAs as empty strings ("")
  no_empties <- replace_na(string, "")
  # Insert string clean functionality
  del_space <- str_replace_all(no_empties, "[:space:]", "")
  del_char <- str_replace_all(del_space, "[:alpha:]", "")
  # If-else clause definition
  # If no "-" is found return the cost as numeric type
  if (!str_detect(del_char, "-")) {
    cost <- as.numeric(del_char)
    return(cost)
    # If "-" is found return the average of the two values
  } else if (str_detect(del_char, "-")) {
    split <- str_split(del_char, pattern = "-", simplify = TRUE)
    avg_cost <- (as.numeric(split[1]) + as.numeric(split[2])) / 2
    return(avg_cost)
    # If the string is empty, return NA_real_
  } else if (string == "") {
    return(NA_real_)
  }
}

# Testing function on all test cases
test_p1 <- cost_transform(case_1)
test_p2 <- cost_transform(case_2)
test_p3 <- cost_transform(case_3)
test_p4 <- cost_transform(case_4)

# Check output
test_p1
test_p2
test_p3
test_p4


# See how function works within data wrangling pipeline
df_pipeline <- df %>%
  # Select some columns
  select(ProjectID, ProjectMainCatecory, ProjectValue) %>%
  mutate(
    # Apply map function
    # (only renaming to get direct comparison)
    ProjectValueNew = map_dbl(ProjectValue, cost_transform)
  )

# Look at output (glimpse())
df_pipeline



