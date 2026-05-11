rm(list = ls())
setwd("M:/Projects/FIE401/exam/Exam 2019/Data2019")

# packages
require(DescTools)
require(tibble)
require(dplyr)
require(knitr)
require(MatchIt)
require(lmtest)
require(sandwich)
require(car)
require(mfx)
require(stargazer)
require(AER)

# load the data
calls <- read.csv("calls.txt", stringsAsFactors = F)
compustat <- read.csv("compustat.csv", stringsAsFactors = F, sep = ";")
load("industry.returns.daily.RData")
merger <- read.csv("merger.csv", sep = "\t")

# Winsorize
calls$beta_MKT_short_window <- Winsorize(calls$beta_MKT_short_window, probs = c(0.005, 0.995), na.rm=T)
calls$beta_HML_short_window <- Winsorize(calls$beta_HML_short_window, probs = c(0.005, 0.995), na.rm=T)
calls$beta_SMB_short_window <- Winsorize(calls$beta_SMB_short_window, probs = c(0.005, 0.995), na.rm=T)

# make variables

# announcement return: I take two days
calls$announcement_return <- (calls$RET.t0 - 
                                calls$beta_MKT_short_window*calls$MKT.RF.t0 - 
                                calls$beta_HML_short_window*calls$HML.t0 -
                                calls$beta_SMB_short_window*calls$SMB.t0) + 
  (calls$RET.f1 - 
     calls$beta_MKT_short_window*calls$MKT.RF.f1 - 
     calls$beta_HML_short_window*calls$HML.f1 -
     calls$beta_SMB_short_window*calls$SMB.f1) 
calls$announcement_return <- calls$announcement_return*100

# abnormal volume
calls$abnormal.volume <- Winsorize(as.vector((calls$VOL.t0 + calls$VOL.f1)/2 / calls$av_daily_vol_short_window ),
                                   probs = c(0.005, 0.995), na.rm=T)

# vola
calls$sqrd.ann.ret <- Winsorize(calls$announcement_return^2, probs = c(0.005, 0.995), na.rm=T)

# compustat variables
ix <- match(paste(calls$lpermno, calls$date, "%Y%m"),
            paste(compustat$lpermno, compustat$announcement.date, "%Y%m"))

calls$assets <- Winsorize(compustat$assets[ix],  probs = c(0.005, 0.995), na.rm=T)
calls$sales.to.assets <- Winsorize(compustat$sales[ix] / calls$assets,  probs = c(0.005, 0.995), na.rm=T)
calls$income.to.assets <- Winsorize(compustat$income[ix] / calls$assets,  probs = c(0.005, 0.995), na.rm=T)
rm(compustat)


# number of mergers / volume of mergers
temp <- aggregate(merger$total.size, 
                  by = list(merger$announcement.date), 
                  sum)
calls$total.size.merger.ann.date <- temp$x[match(calls$date, temp$Group.1)]
calls$total.size.merger.ann.date[is.na(calls$total.size.merger.ann.date)] <- 0
temp <- aggregate(rep(1, length(merger$total.size)), 
                  by = list(merger$announcement.date), 
                  sum)
calls$nr.merger.ann.date <- temp$x[match(calls$date, temp$Group.1)]
calls$nr.merger.ann.date[is.na(calls$nr.merger.ann.date)] <- 0
rm(merger)

# industry returns
industry.returns.daily <- cbind(rep(industry.returns.daily$date, 49), 
                                stack(industry.returns.daily[,2:50]))
colnames(industry.returns.daily) <- c("date", "ret", "FF.49")

# return of the industry today
ix <- match(paste(calls$date, calls$FF.49), 
            paste(industry.returns.daily$date, industry.returns.daily$FF.49))
calls$industry.return.last.day <- industry.returns.daily$ret[ix-1] / 100
calls$industry.return.last.two.days <- (industry.returns.daily$ret[ix-1] + industry.returns.daily$ret[ix-2]) / 100
calls$industry.return.last.five.days <- (industry.returns.daily$ret[ix-5] + industry.returns.daily$ret[ix-1] + industry.returns.daily$ret[ix-2] + industry.returns.daily$ret[ix-3] +industry.returns.daily$ret[ix-4]) / 100 

# return differentials
calls$industry.return.last.day.diff <- calls$industry.return.last.day - calls$RET.l1
calls$industry.return.last.two.days.diff <- calls$industry.return.last.two.days - calls$RET.l2 - calls$RET.l1



# many calls on the same day in same industry / same time in general
calls$date.ind <- paste(calls$date, calls$FF.49)
table.calls.date.ind <- table(calls$date.ind)
calls$nr.calls.same.day.same.industry <- table.calls.date.ind[match(calls$date.ind, names(table.calls.date.ind))] -1

calls$date.hour <- paste(calls$date, calls$hour)
table.calls.date.hour <- table(calls$date.hour)
calls$nr.calls.same.day.and.hour <- table.calls.date.hour[match(calls$date.hour, names(table.calls.date.hour))] -1


# timing
calls$year <- substr(calls$date, 1, 4)
calls$year.quarter <- paste0(substr(calls$date, 1, 4), "Q", ceiling(as.numeric(substr(calls$date, 6, 7))/3))


# delete everything excepts calls
rm(list = ls()[ls()!="calls"])


# summary stats
sum_stat <- function(x, n){
  print_dec(c(mean(x, na.rm =T),
              sd(x , na.rm=T),
              min(x, na.rm=T), 
              max(x, na.rm=T), 
              sum(!is.na(x))), n)
}
print_dec <- function(r,n){
  res <- format(round(r, n), nsmall=n, big.mark = ",")
  res[is.na(r)] <- ""
  res}


# limit sample

calls %>% as_tibble %>% 
  filter(!is.na(institution_participates) & 
           !is.na(announcement_return) & 
           !is.na(sqrd.ann.ret) & 
           !is.na(abnormal.volume) & 
           !is.na(participants) & 
           !is.na(mktv_end_of_last_month) & 
           !is.na(prc_end_of_last_month) & 
           !is.na(sales.to.assets) & 
           !is.na(nr.merger.ann.date) & 
           !is.na(industry.return.last.day) &
           !is.na(nr.calls.same.day.same.industry) &
           !is.na(nr.calls.same.day.and.hour) &
           !is.na(SUE)) %>% 
  dplyr::select(institution_participates, announcement_return, sqrd.ann.ret,
                abnormal.volume, participants, mktv_end_of_last_month, 
                prc_end_of_last_month, sales.to.assets, nr.merger.ann.date, SUE, 
                industry.return.last.day, nr.calls.same.day.same.industry, 
                nr.calls.same.day.and.hour, lpermno, date, FF.49, year, year.quarter) -> calls




# SUMMARY STAT
ix <- rep(T, nrow(calls))

temp <- rbind(
  sum_stat(calls$institution_participates[ix], 3),
  sum_stat(calls$announcement_return[ix], 3),
  sum_stat(log(calls$sqrd.ann.ret[ix]), 3),
  sum_stat(log(calls$abnormal.volume[ix]), 3),
  
  sum_stat(calls$participants[ix], 3),
  sum_stat(log(calls$mktv_end_of_last_month[ix]), 3),
  sum_stat(log(calls$prc_end_of_last_month[ix]), 3),
  #sum_stat(log(calls$assets[ix]), 3),
  sum_stat((calls$sales.to.assets[ix]), 3),
  sum_stat((calls$SUE[ix]), 3),
  
  sum_stat(calls$nr.merger.ann.date[ix], 3),
  sum_stat(calls$industry.return.last.day[ix], 3),
  sum_stat(calls$nr.calls.same.day.same.industry[ix], 3),
  sum_stat(calls$nr.calls.same.day.and.hour[ix], 3))
rownames(temp) <- c("institution participates", 
                    "announcement return", 
                    "sqrd ann ret", 
                    "abnormal volume", 
                    "nr participants", 
                    "mktv",
                    "prc", 
                    #"assets",
                    "sales to assets", 
                    "SUE",
                    "nr mergers ann date", 
                    "industry return last day",
                    "nr calls same day and industry", 
                    "nr calls same day and hour")

colnames(temp) <- c("mean", "sd", "min", "max", "nr obs")

temp.all <- temp



ix <- calls$institution_participates

temp <- rbind(
  sum_stat(calls$institution_participates[ix], 3),
  sum_stat(calls$announcement_return[ix], 3),
  sum_stat(log(calls$sqrd.ann.ret[ix]), 3),
  sum_stat(log(calls$abnormal.volume[ix]), 3),
  
  sum_stat(calls$participants[ix], 3),
  sum_stat(log(calls$mktv_end_of_last_month[ix]), 3),
  sum_stat(log(calls$prc_end_of_last_month[ix]), 3),
  #sum_stat(log(calls$assets[ix]), 3),
  sum_stat((calls$sales.to.assets[ix]), 3),
  sum_stat((calls$SUE[ix]), 3),
  
  sum_stat(calls$nr.merger.ann.date[ix], 3),
  sum_stat(calls$industry.return.last.day[ix], 3),
  sum_stat(calls$nr.calls.same.day.same.industry[ix], 3),
  sum_stat(calls$nr.calls.same.day.and.hour[ix], 3))
rownames(temp) <- c("institution participates", 
                    "announcement return", 
                    "sqrd ann ret", 
                    "abnormal volume", 
                    "nr participants", 
                    "mktv",
                    "prc", 
                    #"assets",
                    "sales to assets", 
                    "SUE",
                    "nr mergers ann date", 
                    "industry return last day",
                    "nr calls same day and industry", 
                    "nr calls same day and hour")

colnames(temp) <- c("mean", "sd", "min", "max", "nr obs")

temp.part <- temp


ix <- !calls$institution_participates

temp <- rbind(
  sum_stat(calls$institution_participates[ix], 3),
  sum_stat(calls$announcement_return[ix], 3),
  sum_stat(log(calls$sqrd.ann.ret[ix]), 3),
  sum_stat(log(calls$abnormal.volume[ix]), 3),
  
  sum_stat(calls$participants[ix], 3),
  sum_stat(log(calls$mktv_end_of_last_month[ix]), 3),
  sum_stat(log(calls$prc_end_of_last_month[ix]), 3),
  #sum_stat(log(calls$assets[ix]), 3),
  sum_stat((calls$sales.to.assets[ix]), 3),
  sum_stat((calls$SUE[ix]), 3),
  
  sum_stat(calls$nr.merger.ann.date[ix], 3),
  sum_stat(calls$industry.return.last.day[ix], 3),
  sum_stat(calls$nr.calls.same.day.same.industry[ix], 3),
  sum_stat(calls$nr.calls.same.day.and.hour[ix], 3))
rownames(temp) <- c("institution participates", 
                    "announcement return", 
                    "sqrd ann ret", 
                    "abnormal volume", 
                    "nr participants", 
                    "mktv",
                    "prc", 
                    #"assets",
                    "sales to assets", 
                    "SUE",
                    "nr mergers ann date", 
                    "industry return last day",
                    "nr calls same day and industry", 
                    "nr calls same day and hour")

colnames(temp) <- c("mean", "sd", "min", "max", "nr obs")

temp.no <- temp


# make table
library(xtable)
temp.all2=data.frame(temp.all)
temp.part2=data.frame(temp.part)
temp.no2=data.frame(temp.no)

xtable(temp.all2)
xtable(temp.part2)
xtable(temp.no2)


# propensity score matched
matching <- matchit(institution_participates ~  
                      + log(participants)
                    + log(mktv_end_of_last_month)
                    + log(prc_end_of_last_month)
                    + sales.to.assets
                    + SUE
                    
                    # include or not? 
                    #+ factor(FF.49)
                    + factor(year)
                    , data = calls
                    ,
                    method = "nearest", distance = "logit", ratio = 1)

# define matched sample
calls$matched.sample <- calls$institution_participates | (1:nrow(calls) %in% as.numeric(matching$match.matrix))

# LPM model
m1 <- lm(
  institution_participates ~
    # main controls
    + log(participants)
  + log(mktv_end_of_last_month)
  + log(prc_end_of_last_month)
  + sales.to.assets
  + SUE
  # potential IVs
  #+ nr.merger.ann.date
  #+ industry.return.last.day
  #+ nr.calls.same.day.same.industry
  #+ nr.calls.same.day.and.hour
  
  #+ factor(FF.49)
  + factor(year)
  , data = calls #%>% filter(matched.sample)
)
m1.se <- coeftest(m1, vcov = vcovHC(m1))[,2]

m2 <- lm(
  institution_participates ~
    # main controls
    + log(participants)
  + log(mktv_end_of_last_month)
  + log(prc_end_of_last_month)
  + sales.to.assets
  + SUE
  # potential IVs
  + nr.merger.ann.date
  #+ industry.return.last.day
  + nr.calls.same.day.same.industry
  #+ nr.calls.same.day.and.hour
  
  #+ factor(FF.49)
  + factor(year)
  , data = calls #%>% filter(matched.sample)
)
m2.se <- coeftest(m2, vcov = vcovHC(m2))[,2]

m3 <- lm(
  institution_participates ~
    # main controls
    + log(participants)
  + log(mktv_end_of_last_month)
  + log(prc_end_of_last_month)
  + sales.to.assets
  + SUE
  # potential IVs
  + nr.merger.ann.date
  #+ industry.return.last.day
  + nr.calls.same.day.same.industry
  #+ nr.calls.same.day.and.hour
  
  #+ factor(FF.49)
  + factor(year)
  , data = calls %>% filter(matched.sample)
)
m3.se <- coeftest(m3, vcov = vcovHC(m3))[,2]

# as logit
m4 <- (logitmfx(institution_participates ~
                  # main controls
                  + log(participants)
                + log(mktv_end_of_last_month)
                + log(prc_end_of_last_month)
                + sales.to.assets
                + SUE
                # potential IVs
                #+ nr.merger.ann.date
                #+ industry.return.last.day
                #+ nr.calls.same.day.same.industry
                #+ nr.calls.same.day.and.hour
                
                #+ factor(FF.49)
                + factor(year)
                , data = calls , 
                atmean = F))

m5 <- (logitmfx(institution_participates ~
                  # main controls
                  + log(participants)
                + log(mktv_end_of_last_month)
                + log(prc_end_of_last_month)
                + sales.to.assets
                + SUE
                # potential IVs
                + nr.merger.ann.date
                #+ industry.return.last.day
                + nr.calls.same.day.same.industry
                #+ nr.calls.same.day.and.hour
                
                #+ factor(FF.49)
                + factor(year)
                , data = calls , 
                atmean = F))

m6 <- (logitmfx(institution_participates ~
                  # main controls
                  + log(participants)
                + log(mktv_end_of_last_month)
                + log(prc_end_of_last_month)
                + sales.to.assets
                + SUE
                # potential IVs
                + nr.merger.ann.date
                #+ industry.return.last.day
                + nr.calls.same.day.same.industry
                #+ nr.calls.same.day.and.hour
                
                #+ factor(FF.49)
                + factor(year)
                , data = calls %>% filter(matched.sample), 
                atmean = F))


# stargazer
stargazer(list(m1, m2, m3, m1, m2, m3),
          coef = list(NULL, NULL, NULL, m4$mfxest[,1], m5$mfxest[,1], m6$mfxest[,1]),
          se = list(m1.se, m2.se, m3.se, m4$mfxest[,2], m5$mfxest[,2], m6$mfxest[,2]),
          type="text", keep.stat=c("n"), report=('vc*t'),
          omit = c("year"),
          add.lines = list(
            c("Sample", "full", "full", "match","full", "full", "match"),
            c("Model", "OLS","OLS","OLS","logit ME","logit ME","logit ME"),
            c("Year FE", "yes","yes","yes","yes","yes","yes")
          ))


# FTEST if wished
myH0 <- c("nr.merger.ann.date=0",
          "nr.calls.same.day.same.industry=0")

print("F-test of instruments for model 2")
linearHypothesis( m2, myH0)

print("F-test of instruments for model 3")
linearHypothesis( m3, myH0)


# FINAL MODELS

# not IV on matched sample
fit_1 <- lm(
  #log(abnormal.volume) ~
  announcement_return ~ #sample versus control sample matters
    #  log(sqrd.ann.ret) ~
    
    # main independent variable
    institution_participates
  # main controls
  + log(participants)
  + log(mktv_end_of_last_month)
  + log(prc_end_of_last_month)
  + sales.to.assets
  + SUE
  + factor(year)
  , data = calls %>% filter(matched.sample)
)
# robust standard errors
se_fit_1 <- coeftest(fit_1,vcov=vcovHC(fit_1))[,2]

# IV on full sample
fit_iv_1 <- AER::ivreg(
  #log(abnormal.volume) ~
  announcement_return ~ #sample versus control sample matters
    #  log(sqrd.ann.ret) ~
    
    # main independent variable
    institution_participates
  # main controls
  + log(participants)
  + log(mktv_end_of_last_month)
  + log(prc_end_of_last_month)
  + sales.to.assets
  + SUE
  + factor(year)
  |
    # potential IVs
    + nr.merger.ann.date
  + nr.calls.same.day.same.industry
  # controls  
  + log(participants)
  + log(mktv_end_of_last_month)
  + log(prc_end_of_last_month)
  + sales.to.assets
  + SUE
  + factor(year)
  
  
  , data = calls #%>% filter(matched.sample)
)
# robust standard errors
se_fit_iv_1 <- coeftest(fit_iv_1,vcov=vcovHC(fit_iv_1, type = c("HC0")))[,2]

# IV on matched sample
fit_iv_2 <- ivreg(
  #log(abnormal.volume) ~
  announcement_return ~ #sample versus control sample matters
    #  log(sqrd.ann.ret) ~
    
    # main independent variable
    institution_participates
  # main controls
  + log(participants)
  + log(mktv_end_of_last_month)
  + log(prc_end_of_last_month)
  + sales.to.assets
  + SUE
  + factor(year)
  |
    # potential IVs
    + nr.merger.ann.date
  + nr.calls.same.day.same.industry
  # controls  
  + log(participants)
  + log(mktv_end_of_last_month)
  + log(prc_end_of_last_month)
  + sales.to.assets
  + SUE
  + factor(year)
  
  
  , data = calls %>% filter(matched.sample)
)
# robust standard errors
se_fit_iv_2 <- coeftest(fit_iv_2,vcov=vcovHC(fit_iv_2, type = c("HC0")))[,2]



# not IV on matched sample
fit_2 <- lm(
  log(abnormal.volume) ~
    #  announcement_return ~ #sample versus control sample matters
    #  log(sqrd.ann.ret) ~
    
    # main independent variable
    institution_participates
  # main controls
  + log(participants)
  + log(mktv_end_of_last_month)
  + log(prc_end_of_last_month)
  + sales.to.assets
  + SUE
  + factor(year)
  , data = calls %>% filter(matched.sample)
)
# robust standard errors
se_fit_2 <- coeftest(fit_2,vcov=vcovHC(fit_2))[,2]

# IV on full sample
fit_iv_3 <- ivreg(
  log(abnormal.volume) ~
    #  announcement_return ~ #sample versus control sample matters
    #  log(sqrd.ann.ret) ~
    
    # main independent variable
    institution_participates
  # main controls
  + log(participants)
  + log(mktv_end_of_last_month)
  + log(prc_end_of_last_month)
  + sales.to.assets
  + SUE
  + factor(year)
  |
    # potential IVs
    + nr.merger.ann.date
  + nr.calls.same.day.same.industry
  # controls  
  + log(participants)
  + log(mktv_end_of_last_month)
  + log(prc_end_of_last_month)
  + sales.to.assets
  + SUE
  + factor(year)
  
  
  , data = calls #%>% filter(matched.sample)
)
# robust standard errors
se_fit_iv_3 <- coeftest(fit_iv_3,vcov=vcovHC(fit_iv_3, type = c("HC0")))[,2]


# IV on matched sample
fit_iv_4 <- ivreg(
  log(abnormal.volume) ~
    #  announcement_return ~ #sample versus control sample matters
    #  log(sqrd.ann.ret) ~
    
    # main independent variable
    institution_participates
  # main controls
  + log(participants)
  + log(mktv_end_of_last_month)
  + log(prc_end_of_last_month)
  + sales.to.assets
  + SUE
  + factor(year)
  |
    # potential IVs
    + nr.merger.ann.date
  + nr.calls.same.day.same.industry
  # controls  
  + log(participants)
  + log(mktv_end_of_last_month)
  + log(prc_end_of_last_month)
  + sales.to.assets
  + SUE
  + factor(year)
  
  
  , data = calls %>% filter(matched.sample)
)
# robust standard errors
se_fit_iv_4 <- coeftest(fit_iv_4,vcov=vcovHC(fit_iv_4, type = c("HC0")))[,2]


# not IV on matched sample
fit_3 <- lm(
  #log(abnormal.volume) ~
  #  announcement_return ~ #sample versus control sample matters
  log(sqrd.ann.ret) ~
    
    # main independent variable
    institution_participates
  # main controls
  + log(participants)
  + log(mktv_end_of_last_month)
  + log(prc_end_of_last_month)
  + sales.to.assets
  + SUE
  + factor(year)
  , data = calls %>% filter(matched.sample)
)
# robust standard errors
se_fit_3 <- coeftest(fit_3,vcov=vcovHC(fit_3))[,2]

# IV on full sample
fit_iv_5 <- ivreg(
  #log(abnormal.volume) ~
  #  announcement_return ~ #sample versus control sample matters
  log(sqrd.ann.ret) ~
    
    # main independent variable
    institution_participates
  # main controls
  + log(participants)
  + log(mktv_end_of_last_month)
  + log(prc_end_of_last_month)
  + sales.to.assets
  + SUE
  + factor(year)
  |
    # potential IVs
    + nr.merger.ann.date
  + nr.calls.same.day.same.industry
  # controls  
  + log(participants)
  + log(mktv_end_of_last_month)
  + log(prc_end_of_last_month)
  + sales.to.assets
  + SUE
  + factor(year)
  
  
  , data = calls #%>% filter(matched.sample)
)
# robust standard errors
se_fit_iv_5 <- coeftest(fit_iv_5,vcov=vcovHC(fit_iv_5, type = c("HC0")))[,2]

# IV on matched sample
fit_iv_6 <- ivreg(
  #log(abnormal.volume) ~
  #  announcement_return ~ #sample versus control sample matters
  log(sqrd.ann.ret) ~
    
    # main independent variable
    institution_participates
  # main controls
  + log(participants)
  + log(mktv_end_of_last_month)
  + log(prc_end_of_last_month)
  + sales.to.assets
  + SUE
  + factor(year)
  |
    # potential IVs
    + nr.merger.ann.date
  + nr.calls.same.day.same.industry
  # controls  
  + log(participants)
  + log(mktv_end_of_last_month)
  + log(prc_end_of_last_month)
  + sales.to.assets
  + SUE
  + factor(year)
  
  
  , data = calls %>% filter(matched.sample)
)
# robust standard errors
se_fit_iv_6 <- coeftest(fit_iv_6,vcov=vcovHC(fit_iv_6, type = c("HC0")))[,2]

# stargazer
stargazer(list(fit_1, fit_1, fit_1,fit_2, fit_2, fit_2,fit_3, fit_3, fit_3),
          coef = list(NULL, fit_iv_1$coefficients, fit_iv_2$coefficients, NULL, fit_iv_3$coefficients, fit_iv_4$coefficients, NULL, fit_iv_5$coefficients, fit_iv_6$coefficients),
          se = list(se_fit_1, se_fit_iv_1, se_fit_iv_2, se_fit_2, se_fit_iv_3, se_fit_iv_4, se_fit_3, se_fit_iv_5, se_fit_iv_6 ),
          type="text", 
          keep.stat=c("adj.rsq"), 
          omit = c("year"),
          report=('vc*t'),
          add.lines = list(
            c("Observations", 
              length(fit_1$residuals), length(fit_iv_1$residuals), length(fit_iv_2$residuals),
              length(fit_2$residuals), length(fit_iv_3$residuals), length(fit_iv_4$residuals),
              length(fit_3$residuals), length(fit_iv_5$residuals), length(fit_iv_6$residuals)),
            c("Sample", "match", "full", "match","match", "full", "match","match", "full", "match"),
            c("Model", "OLS", "IV", "IV", "OLS", "IV", "IV", "OLS", "IV", "IV"),
            c("Year FE", "yes","yes","yes","yes","yes","yes","yes","yes","yes")
          ))

