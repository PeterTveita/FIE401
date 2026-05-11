#Solution for Term Paper FIE401
#===================================================================================================

#1.1 Loading required packages
#===================================================================================================
require(dplyr)
require(DescTools)
require(psych)
require(plm)
require(lmtest)
require(sandwich)
require(stargazer)
require(car)
#===================================================================================================


#======================================= Part 1 - Cleaning data =====================================
#Working directory set to same folder as this R file
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#Loading data
isins     <- read.csv("FIE401 EXAM attach 3 isins.csv",    stringsAsFactors = FALSE)
chf_eur   <- read.csv("FIE401 exam attach 1 chf_eur.csv",  stringsAsFactors = FALSE)
firm_info <- read.csv("FIE401 exam attach 2 firm_info.csv", stringsAsFactors = FALSE)

#Parse dates
chf_eur$date   <- as.Date(chf_eur$date,   format = "%d/%m/%Y")
firm_info$date <- as.Date(firm_info$date, format = "%d/%m/%Y")

isins <- isins[isins$Index %in% c("SMI", "CAC40", "DAX30"), ]

#Merging data frames
df <- merge(firm_info, isins[, c("ISIN", "Index")], by = "ISIN")
df <- merge(df, chf_eur, by = "date", all.x = TRUE)

#Cleaning data
df <- df[!is.na(df$volume) & df$volume > 0, ] #Remove rows with missing or zero volume
df$ask[df$prc < 0] <- NA #Remove negative ask prices
df$bid[df$prc < 0] <- NA #Remove negative bid prices
df$prc <- abs(df$prc) #Convert negative prices to positive (assuming they are errors in the data)

#Removing rows with less than 100 observations and average price less than 1
stock_filter <- df %>% group_by(ISIN) %>%
  summarise(N = n(), avg_prc = mean(prc)) %>%
  filter(N >= 100, avg_prc >= 1)

df <- df[df$ISIN %in% stock_filter$ISIN, ]

#Converting from CHF (swiss franc) to EUR (euro)
df$prc_eur <- ifelse(df$Index == "SMI", df$prc / df$chf_eur, df$prc)

#Calculating the bid-ask spread
df$spread <- (df$ask - df$bid) / ((df$ask + df$bid) / 2)

#Calculating log market capitalization, turnover, book-to-market ratio and leverage
df$log_mcap <- log(df$prc_eur * df$shrout * 1000 / 1e9)
df$turnover <- (df$volume / df$shrout) * 252
df$btm      <- df$book_val_per_share / df$prc
df$btm[df$btm < 0] <- NA
df$leverage <- df$tot_debt_prop

#Difference-in-differences variables
df$SMI      <- ifelse(df$Index == "SMI", 1, 0)
df$post     <- ifelse(df$date >= as.Date("2019-07-01"), 1, 0)
df$SMI_post <- df$SMI * df$post

#Winsorizing the variables to mitigate the influence of outliers (at the 0.5% and 99.5% quantiles)
wins <- function(x) {
  DescTools::Winsorize(x, val = quantile(x, probs = c(0.005, 0.995), na.rm = TRUE))
}
df$spread   <- wins(df$spread)
df$log_mcap <- wins(df$log_mcap)
df$turnover <- wins(df$turnover)
df$btm      <- wins(df$btm)
df$leverage <- wins(df$leverage)

#======================================= Part 2 - Pre period =========================================

#2.1 Table 1 - Summary Statistics
#===================================================================================================
#Variables
tab1_vars <- c("spread", "log_mcap", "turnover", "btm", "leverage")

#Summary of statistics for SMI and control group
stats_smi <- describe(df[df$SMI == 1, tab1_vars])[, c("n","mean","median","sd","min","max")]
stats_ctl <- describe(df[df$SMI == 0, tab1_vars])[, c("n","mean","median","sd","min","max")]

#Print to console
stargazer(as.data.frame(round(stats_smi, 3)),
          type = "latex", summary = FALSE,
          out = "Latex/Inputs/Table1A.tex",
          title = "Table 1A: Summary Statistics - SMI")

stargazer(as.data.frame(round(stats_ctl, 3)),
          type = "latex", summary = FALSE,
          out = "Latex/Inputs/Table1B.tex",
          title = "Table 1B: Summary Statistics - CAC40/DAX30")


#2.2 Table 2 - Pre-period comparison (SMI vs CAC40/DAX30)
#===================================================================================================

#DF for pre period
df_pre <- df[df$post == 0, ]

#Variables for pre period
tab2_vars <- c("spread", "log_mcap", "turnover", "btm", "leverage")

#OLS regressions for pre period
models_pre <- lapply(tab2_vars, function(v) {
  lm(reformulate("SMI", response = v), data = df_pre)
})

#Statistics for pre period regressions
ses_pre <- lapply(models_pre, function(m) {
  coeftest(m, vcov = vcovCL(m, cluster = df_pre$ISIN))[, 2]
})

#Print output
stargazer(models_pre,
          coef         = lapply(models_pre, coef),
          se           = ses_pre,
          type         = "latex",
          out          = "Latex/Inputs/Table2.tex",
          title        = "Table 2: Pre-period Comparison --- SMI vs. CAC40/DAX30",
          dep.var.labels   = tab2_vars,
          covariate.labels = "SMI",
          keep             = "SMI",
          omit.stat        = c("f", "ser"),
          add.lines = list(
            c("Pre-period", rep("Yes", length(tab2_vars))),
            c("Cluster SE", rep("By stock", length(tab2_vars)))
          ),
          notes.append = FALSE,
          notes = "$^{*}$p$<$0.1; $^{**}$p$<$0.05; $^{***}$p$<$0.01")

#======================================= Part 3 - DiD Regression =========================================

#DF for DiD regression
df_panel <- pdata.frame(df, index = c("ISIN", "date"))

#DiD regression with two-way fixed effects, for FE-firm and FE-time
#Model 1: Only treatment variable
fit_1 <- plm(spread ~ SMI_post,
             data = df_panel, model = "within", effect = "twoways")
se_fit_1 <- coeftest(fit_1, vcov = vcovHC(fit_1, cluster = "group", type = "sss"))[, 2]

#Model 2: Treatment variable + controls
fit_2 <- plm(spread ~ SMI_post + log_mcap + turnover + btm + leverage,
             data = df_panel, model = "within", effect = "twoways")
se_fit_2 <- coeftest(fit_2, vcov = vcovHC(fit_2, cluster = "group", type = "sss"))[, 2]

#Print output
stargazer(list(fit_1, fit_2),
          coef = list(fit_1$coefficients, fit_2$coefficients),
          se   = list(se_fit_1, se_fit_2),
          type = "latex",
          out  = "Latex/Inputs/Table3.tex",
          title            = "Table 3: Effect of Market Consolidation on Stock Liquidity",
          dep.var.labels   = "Bid-Ask Spread",
          covariate.labels = c("SMI $\\times$ Post", "log(Mcap)", "Turnover", "BTM", "Leverage"),
          keep.stat = c("adj.rsq"),
          report    = "vc*t",
          add.lines = list(
            c("Observations", length(fit_1$residuals), length(fit_2$residuals)),
            c("Stock FE",   "Yes", "Yes"),
            c("Day FE",     "Yes", "Yes"),
            c("Cluster SE", "By stock", "By stock")
          ),
          notes.append = FALSE,
          notes = "$^{*}$p$<$0.1; $^{**}$p$<$0.05; $^{***}$p$<$0.01")