#AI Declaration: AI has been used to debug and optimize the code. In addition, AI has been used to check for potential errors and spelling mistakes in the code and comments.

#========================================== Solution for Term Paper FIE401 ===========================================

#============================================= Loading required packages =============================================
require(dplyr)
require(DescTools)
require(psych)
require(plm)
require(lmtest)
require(sandwich)
require(stargazer)
require(car)
require(ggplot2)


#============================================== Part 1 - Cleaning data ===============================================
#Working directory set to same folder as this R file
if (rstudioapi::isAvailable()) {
  setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
}

#Loading data
isins     <- read.csv("FIE401 EXAM attach 3 isins.csv",    stringsAsFactors = FALSE)
chf_eur   <- read.csv("FIE401 exam attach 1 chf_eur.csv",  stringsAsFactors = FALSE)
firm_info <- read.csv("FIE401 exam attach 2 firm_info.csv", stringsAsFactors = FALSE)

#Parse dates
chf_eur$date   <- as.Date(chf_eur$date,   format = "%d/%m/%Y")
firm_info$date <- as.Date(firm_info$date, format = "%d/%m/%Y")

isins <- isins[isins$Index %in% c("SMI", "CAC40", "DAX30"), ]

#Drop Airbus as it is duplicated for both CAC40 and DAX30
isins <- isins[isins$ISIN != "NL0000235190", ]

#Merging data frames
df <- merge(firm_info, isins[, c("ISIN", "Index")], by = "ISIN")
df <- merge(df, chf_eur, by = "date", all.x = TRUE)

#Removing duplicates
df <- df[!duplicated(df[, c("ISIN", "date")]), ]

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

#Correcting DAX30 volume: reported volume is understated by factor of 10
df$volume[df$Index == "DAX30"] <- df$volume[df$Index == "DAX30"] * 10

#========================================== Part 1.2 - Variable construction ==========================================
#Converting from CHF (swiss franc) to EUR (euro)
df$prc_eur <- ifelse(df$Index == "SMI", df$prc / df$chf_eur, df$prc)

#Calculating Amihud illiquidity as absolute return divided by euro trading volume
df <- df %>%
  arrange(ISIN, date) %>%
  group_by(ISIN) %>%
  mutate(ret = return_index / dplyr::lag(return_index) - 1) %>%
  ungroup()

df$eur_volume <- df$prc_eur * df$volume * 1000
df$amihud     <- abs(df$ret) / df$eur_volume * 1e9
df$amihud[!is.finite(df$amihud) | df$amihud < 0] <- NA

#Calculating the bid-ask spread
df$spread <- (df$ask - df$bid) / ((df$ask + df$bid) / 2)

#Spread filter to remove missing and negative spreads
df <- df[!is.na(df$spread) & df$spread >= 0, ]
df$spread <- df$spread * 10000 #Convert to basis points

#Calculating log market capitalization, turnover, book-to-market ratio and leverage
df$log_mcap <- log(df$prc_eur * df$shrout * 1000 / 1e9)
df$turnover <- (df$volume / df$shrout) * 252
df$btm      <- df$book_val_per_share / df$prc
df$btm[df$btm < 0] <- NA
df$leverage <- df$tot_debt_prop / 100

#Difference-in-differences variables
df$SMI          <- ifelse(df$Index == "SMI", 1, 0)
df$consolidated <- as.integer(df$date >= as.Date("2019-07-01") & df$date <  as.Date("2021-02-08"))
df$refragmented <- as.integer(df$date >= as.Date("2021-02-08"))
df$SMI_cons     <- df$SMI * df$consolidated
df$SMI_refrag   <- df$SMI * df$refragmented

#Keep only rows with complete data on all analysis variables
df <- df[complete.cases(df[, c("spread", "log_mcap", "turnover", "btm", "leverage")]), ]

#Winsorizing the variables to mitigate the influence of outliers (at the 0.5% and 99.5% quantiles)
wins <- function(x) {DescTools::Winsorize(x, val = quantile(x, probs = c(0.005, 0.995), na.rm = TRUE))}

df$spread   <- wins(df$spread)
df$amihud   <- wins(df$amihud)
df$log_mcap <- wins(df$log_mcap)
df$turnover <- wins(df$turnover)
df$btm      <- wins(df$btm)
df$leverage <- wins(df$leverage)

#================================================ Part 2 - Pre period ================================================

#=========================================== Part 2.1 - Summary statistics ===========================================
#Variables
tab1_vars <- c("spread", "log_mcap", "turnover", "btm", "leverage")
var_labels <- c("Bid-Ask Spread", "log(Mcap)", "Turnover", "BTM", "Leverage (D/E)")

#Summary of statistics for SMI and control group
stats_smi <- round(describe(df[df$SMI == 1, tab1_vars])[, c("n","mean","median","sd","min","max")], 3)
stats_ctl <- round(describe(df[df$SMI == 0, tab1_vars])[, c("n","mean","median","sd","min","max")], 3)

n_smi <- formatC(stats_smi$n[1], format = "d", big.mark = ",")
n_ctl <- formatC(stats_ctl$n[1], format = "d", big.mark = ",")

fmt_row <- function(label, stats, i) {
  paste0(label, " & ", stats$mean[i], " & ", stats$median[i], " & ",
         stats$sd[i], " & ", stats$min[i], " & ", stats$max[i], " \\\\")
}

writeLines(c(
  "\\begin{table}[H] \\centering",
  "  \\caption{Summary Statistics}",
  "  \\label{tab:summary}",
  "\\small",
  "\\begin{tabular}{@{\\extracolsep{5pt}}lcccccc}",
  "\\\\[-1.8ex]\\hline",
  "\\hline \\\\[-1.8ex]",
  " & Mean & Median & SD & Min & Max \\\\",
  "\\hline \\\\[-1.8ex]",
  paste0("\\multicolumn{6}{l}{\\textit{Panel A: SMI (N = ", n_smi, ")}} \\\\[3pt]"),
  sapply(seq_along(var_labels), function(i) fmt_row(var_labels[i], stats_smi, i)),
  "\\hline \\\\[-1.8ex]",
  paste0("\\multicolumn{6}{l}{\\textit{Panel B: CAC40/DAX30 (N = ", n_ctl, ")}} \\\\[3pt]"),
  sapply(seq_along(var_labels), function(i) fmt_row(var_labels[i], stats_ctl, i)),
  "\\hline",
  "\\hline \\\\[-1.8ex]",
  "\\end{tabular}",
  "\\end{table}"
), "Latex/Inputs/Table1.tex")

#========================================= Part 2.2 - Pre-period comparison ==========================================

#DF for pre period
df_pre <- df[df$consolidated == 0 & df$refragmented == 0, ]
rownames(df_pre) <- NULL

#Variables for pre period
tab2_vars <- c("spread", "log_mcap", "turnover", "btm", "leverage")

#OLS regressions for pre period
models_pre <- lapply(tab2_vars, function(v) {
  lm(reformulate("SMI", response = v), data = df_pre)
})

#Statistics for pre period regressions
ses_pre <- lapply(models_pre, function(m) {
  idx <- as.integer(names(m$residuals))
  coeftest(m, vcov = vcovCL(m, cluster = df_pre$ISIN[idx]))[, 2]
})

#Print output
stargazer(models_pre,
          coef                   = lapply(models_pre, coef),
          se                     = ses_pre,
          type                   = "latex",
          out                    = "Latex/Inputs/Table2.tex",
          label                  = "tab:pre-period",
          title                  = "Pre-period Comparison --- SMI vs. CAC40/DAX30",
          dep.var.caption        = "",
          dep.var.labels.include = FALSE,
          column.labels          = c("Bid-Ask Spread", "log(Mcap)", "Turnover", "BTM", "Leverage (D/E)"),
          covariate.labels       = "SMI",
          keep                   = "SMI",
          omit.stat              = c("f", "ser", "rsq"),
          report                 = "vc*t",
          add.lines              = list(c("Pre-period", rep("Yes", length(tab2_vars))),
                                        c("Cluster SE", rep("By stock", length(tab2_vars)))),
          notes.append           = FALSE,
          notes                  = "*p<0.1; **p<0.05; ***p<0.01")

#======================================== Part 2.3 - Parallel trends figure ========================================

df_plot <- df %>%
  mutate(
    group    = ifelse(SMI == 1, "SMI", "CAC40/DAX30"),
    year_mon = as.Date(format(date, "%Y-%m-01"))
  ) %>%
  group_by(year_mon, group) %>%
  summarise(mean_spread = mean(spread, na.rm = TRUE), .groups = "drop")

consolidation_label  <- as.Date("2019-07-01")
refragmentation_label <- as.Date("2021-02-08")
y_top <- max(df_plot$mean_spread) * 0.97

fig1 <- ggplot(df_plot, aes(x = year_mon, y = mean_spread,
                             color = group, linetype = group)) +
  geom_line(linewidth = 0.7) +
  geom_vline(xintercept = consolidation_label,  linetype = "dashed", color = "black") +
  geom_vline(xintercept = refragmentation_label, linetype = "dotted", color = "black") +
  annotate("text", x = consolidation_label,  y = y_top,
           label = "Consolidation", hjust = -0.05, size = 3) +
  annotate("text", x = refragmentation_label, y = y_top,
           label = "Re-fragmentation", hjust = -0.05, size = 3) +
  scale_color_manual(values = c("SMI" = "steelblue", "CAC40/DAX30" = "firebrick")) +
  labs(x = NULL, y = "Average Bid-Ask Spread (bps)", color = NULL, linetype = NULL) +
  theme_bw() +
  theme(legend.position = "bottom")

ggsave("Latex/Inputs/Figure1.pdf", fig1, width = 7, height = 3.5)


#============================================== Part 3 - DiD regression ==============================================

#============================================= Part 3.1 - Model estimation =============================================
#Event dates
consolidation_date <- as.Date("2019-07-01")
refragmentation_date <- as.Date("2021-02-08")

#Main DiD regression: before consolidation + consolidation period
df_main <- df[df$date < refragmentation_date, ]

df_main$post     <- ifelse(df_main$date >= consolidation_date, 1, 0)
df_main$SMI_post <- df_main$SMI * df_main$post

df_main_panel <- pdata.frame(df_main, index = c("ISIN", "date"))
df_main_amihud <- df_main[complete.cases(df_main[, c("amihud", "SMI_post", "log_mcap", "turnover", "btm", "leverage")]), ]
df_main_amihud_panel <- pdata.frame(df_main_amihud, index = c("ISIN", "date"))

#Model 1: Main DiD without controls
fit_1 <- plm(spread ~ SMI_post,
             data = df_main_panel, model = "within", effect = "twoways")

se_fit_1 <- coeftest(fit_1,
                     vcov = vcovHC(fit_1, cluster = "group", type = "sss"))[, 2]

#Model 2: Main DiD with controls
fit_2 <- plm(spread ~ SMI_post + log_mcap + turnover + btm + leverage,
             data = df_main_panel, model = "within", effect = "twoways")

se_fit_2 <- coeftest(fit_2,
                     vcov = vcovHC(fit_2, cluster = "group", type = "sss"))[, 2]

#Robustness / regime model: full sample with separate re-fragmentation period
df_panel <- pdata.frame(df, index = c("ISIN", "date"))

#Model 3: Regime model without controls
fit_3 <- plm(spread ~ SMI_cons + SMI_refrag,
             data = df_panel, model = "within", effect = "twoways")

se_fit_3 <- coeftest(fit_3,
                     vcov = vcovHC(fit_3, cluster = "group", type = "sss"))[, 2]

#Model 4: Regime model with controls
fit_4 <- plm(spread ~ SMI_cons + SMI_refrag +
               log_mcap + turnover + btm + leverage,
             data = df_panel, model = "within", effect = "twoways")

se_fit_4 <- coeftest(fit_4,
                     vcov = vcovHC(fit_4, cluster = "group", type = "sss"))[, 2]

#Model 5: Main DiD using Amihud illiquidity without controls
fit_5 <- plm(amihud ~ SMI_post,
             data = df_main_amihud_panel, model = "within", effect = "twoways")

se_fit_5 <- coeftest(fit_5,
                     vcov = vcovHC(fit_5, cluster = "group", type = "sss"))[, 2]

#Model 6: Main DiD using Amihud illiquidity with controls
fit_6 <- plm(amihud ~ SMI_post + log_mcap + turnover + btm + leverage,
             data = df_main_amihud_panel, model = "within", effect = "twoways")

se_fit_6 <- coeftest(fit_6,
                     vcov = vcovHC(fit_6, cluster = "group", type = "sss"))[, 2]


#================================================= Part 3.2 - Output =================================================
#Print output
stargazer(list(fit_1, fit_2, fit_3, fit_4, fit_5, fit_6),
          coef             = list(fit_1$coefficients, fit_2$coefficients, fit_3$coefficients, fit_4$coefficients, fit_5$coefficients, fit_6$coefficients),
          se               = list(se_fit_1, se_fit_2, se_fit_3, se_fit_4, se_fit_5, se_fit_6),
          type             = "latex",
          out              = "Latex/Inputs/Table3.tex",
          title            = "Table 3: Effect of Market Consolidation and Re-fragmentation on Stock Liquidity",
          font.size        = "scriptsize",
          table.placement  = "H",
          label            = "tab:did",
          dep.var.labels   = c("Bid-Ask Spread", "Amihud Illiquidity"),
          covariate.labels = c("SMI x Post",
                              "SMI x Consolidated",
                              "SMI x Re-fragmented",
                              "log(Mcap)", "Turnover", 
                              "BTM", 
                              "Leverage (D/E)"),
          order            = c("SMI_post", 
                              "SMI_cons", 
                              "SMI_refrag",
                              "log_mcap", 
                              "turnover", 
                              "btm", 
                              "leverage"),
          keep.stat        = c("adj.rsq"),
          omit.stat        = c("f", "ser"),
          report           = "vc*t",
          add.lines        = list(c("Sample", "Pre+Cons.", "Pre+Cons.", "Full", "Full", "Pre+Cons.", "Pre+Cons."),
                                  c("Controls", "No", "Yes", "No", "Yes", "No", "Yes"),
                                  c("Observations", length(fit_1$residuals), length(fit_2$residuals), length(fit_3$residuals), length(fit_4$residuals), length(fit_5$residuals), length(fit_6$residuals)),
                                  c("Stock FE", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes"),
                                  c("Day FE", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes"),
                                  c("Cluster SE", "By stock", "By stock", "By stock", "By stock", "By stock", "By stock")),
          notes.append     = FALSE,
          notes            = "*p<0.1; **p<0.05; ***p<0.01")