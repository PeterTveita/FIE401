options(scipen = 999) #Display numbers without scientific notation

load("CAR_M&A.Rdata")

str(CAR_MA)

colSums(is.na(CAR_MA)) #Checking for missing values

continous_vars <- names(CAR_MA)[
  sapply(CAR_MA, function(x) is.numeric(x) && length(unique(x)) > 2) #Choose only continuous numeric variables with more than two unique values (drop dummy variables)
]

extreme_summary <- sapply(CAR_MA[continous_vars], function(x){ #Calculate extreme values
  c(
    min = min(x, na.rm = TRUE),
    p1 = quantile(x, 0.01, na.rm = TRUE),
    median = median(x, na.rm = TRUE),
    p99 = quantile(x, 0.99, na.rm = TRUE),
    max = max(x, na.rm = TRUE)
  )
})

t(extreme_summary) #Transposing the array

table(CAR_MA$private, CAR_MA$public) #Checking that no entity is both private and public