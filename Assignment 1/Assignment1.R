options(scipen = 999) #Display numbers without scientific notation

require(dplyr)
require(stargazer)
require(DescTools)

#======================
#Cleaning data
#======================

load("CAR_M&A.Rdata")
str(CAR_MA)

colSums(is.na(CAR_MA)) #Checking for missing values
table(CAR_MA$private, CAR_MA$public) #Checking that no entity is both private and public

continuous_vars <- names(CAR_MA)[
  sapply(CAR_MA, function(x) is.numeric(x) && length(unique(x)) > 2) #Choose only continuous numeric variables with more than two unique values (drop dummy variables)
]

continuous_summary <- sapply(CAR_MA[continuous_vars], function(x){ #Summary of continuous variables, with 1% and 99%
  c(
    min = min(x),
    p1 = quantile(x, 0.01),
    median = median(x),
    p99 = quantile(x, 0.99),
    max = max(x)
  )
})

t(continuous_summary) #Transposing the array

continuous_vars <- setdiff(continuous_vars, c("yyyy", "yyyymmdd")) #Remove year and date, as i don't want to winsorize them

for (v in continuous_vars) { #Winsorizing at 1% and 99%
  CAR_MA[[paste0(v, "_win")]] <- Winsorize(CAR_MA[[v]], val = quantile(CAR_MA[[v]], probs = c(0.01, 0.99)))
}

summary(CAR_MA$carbidder) #Checking carbidder before winsorizing
summary(CAR_MA$carbidder_win) #Checking carbidder after winsorizing

#======================
#Table 1
#======================

Table1 <- CAR_MA %>%                      #Creating table with wanted variables
  group_by(yyyy) %>%                      #Grouping by year
  summarise(                              #Summarizing to show one value per variable per year
    avg_deal_value = mean(deal_value_win),
    avg_car = mean(carbidder_win),
    share_private = mean(private),
    share_stock = mean(all_stock),
    observations = n()
  )

Table1 <- Table1 %>%                      #Couldn't make digits = 3 work with stargazer, so rounded the entire table
  mutate(across(-yyyy, ~ round(., 3)))    #Round all columns but year
  
stargazer(Table1,
          type = "latex",
          title = "Statistics for CAR by year",
          summary = FALSE,
          rownames = FALSE)               #Formatting for latex
