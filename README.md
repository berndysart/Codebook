# Sample Codebook Assignment

## Introduction

This was an assignment for SIS-750 Data Analysis at American University in Washington DC. The project was to create a codebook for a cleaned data set of our choosing or for the professors research project. With interest in contributing to a larger research topic, I was given two data sets to clean and merge for the professor and provide a codebook outlying and exemplifying all of the variables that were included in the cleaned data set. 

## Research Topic
The data was contributing to a research project evaluating the impact of foreign influence on campaigns supporting democracies

## Data
The data that was merged and cleaned was the World Values Survey (WVS) and the European Values Survey (EVS). See codebook methodology section for more details. 

## Approach
- Wrangling Data
  - Merged and created source id for each individual row
  - Transformed variable S003 in both EVS and WVS and turned into a numeric variable
  - Filtered and mutated in names of countries into merged data set with id code provided by Dr Austin Hart
  - Created 15-17 variables from existing variables in each new data set created from WSV and ESV
- Cleaning Data
  - Mutated across the columns to switch labels to factor labels
  - Mutated the month variable to show the month name rather than the number
  - Relocated source id to designated spot
  - Removed NA's from each variable individually
  - Adjusted the function code to do row.number() instead of as.numeric

# Interesting Code Chunk
- Added a function variable for the purpose of displaying the rows when kabling in the codebook
  - Can be seen in the latter half of the codebook when visualising tables and variables

```
cbfactor = function(.data, x){
  x = enquo(x)
  count(.data, !!x) |>
    mutate(
      values = row_number(!!x),
      labels = as_factor(!!x),
      freq = n,
      perc = n / sum(n) * 100,
      .keep = 'unused'
    ) |>
    knitr::kable(format = 'pipe', digits = 1L)
}
```

