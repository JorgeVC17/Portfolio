library(rio)
library(patchwork)
library(lubridate)
library(lfe)
library(rstanarm)
library(tidybayes)
library(lme4)
library(broom)
library(stargazer)
library(tidyverse)

# store the median date for each data collection wave
wavesndates <- tribble(~wave, ~wavedate,
                       "1", "2020-04-19",
                       "2", "2020-06-15",
                       "3", "2020-12-12") %>% 
        mutate(wavedate = ymd(wavedate))

# parse rounded mean and SD into character for tables
mean.sd <- function(x){
        avg <-round(mean(x, na.rm = T), 2)
        sd <- sprintf("%.2f", round(sd(x, na.rm = T), 2))
        paste(avg, " (", sd, ")", sep = '')
}


# a function to plug in the DVs into fixed effects regression models
my.fe <- function(data, 
                  DV = "value", 
                  IV, 
                  twoway = TRUE, 
                  unittime = FALSE,
                  unittimepool = FALSE,
                  onewayresp = FALSE,
                  onewaytime = FALSE,
                  onewayresppool = FALSE,
                  onewaytimepool = FALSE,
                  controlgovt = FALSE,
                  lead = FALSE, 
                  # lag = FALSE,
                  weights = TRUE){
        
        # we have many types of fixed effect structures. 
        # twoway = classical 2wfe model with respondent and time FE 
        if(twoway){
                myFE <-  "| country_wave + ID"
        # unittime = individual time-trend, for robustness checks
        } else if(unittime){
                myFE <-  "| ID_f + ID_f:wave"    
        # unittimepool = individual time-trend for pooled, for robustness checks
        } else if(unittimepool){
                myFE <-  "| ID_f + ID_f:wave + name"   
                
        # onewayresp[pool] = include only unit fixed effects (with or without pooling vars)
        } else if(onewayresp){
                myFE <-  "| ID"   
        } else if(onewayresppool){
                myFE <-  "| ID + name"   
                
        # onewaytime[pool] = include only time fixed effects  (with or without pooling vars)
        } else if(onewaytime){
                myFE <-  "| country_wave"   
        } else if(onewaytimepool){
                myFE <-  "| country_wave + name"   
        
        # finally, an aggregator model which pools 4 DVs within groups, 
                # by adding "name" as 3rd FE
        } else {
                myFE <-  "| country_wave + ID + name"    
        } 
        
        # include leads? (for testing parallel trends) 
        if(lead){
                myIV <- paste(IV, "+", paste0(IV, "_lead"))
        
        # include "support for government" as a control variable?
        } else if(controlgovt) {
                myIV <- paste(IV, "+", "gov")
        } else  {
                myIV <- IV
        } 
        # browser()
        # combine all components from above to get formula of the regression 
        # temp_formula <- formula(paste(DV, "~", myIV, myFE))
        temp_formula <- formula(paste(DV, "~", myIV, myFE, "| 0 | ID"))
        
        # use weights? 
        if(weights){
                
                # this bit fits the FE regression with felm()
                #       and cleans ups output with tidy()
                broom::tidy(felm(temp_formula, data = data,
                                 weights = data$FE_weight),
                            conf.int = TRUE)
        } else {
                # same, but without weights
                broom::tidy(felm(temp_formula, data = data),
                            conf.int = TRUE)
        } 
}


# Import all survey data (imbalanced panel)
df_full <- readRDS("B-analysis-data/yougov_clean_202204122.rds")

# create balanced panel by keeping only people who did not drop out
#       (ie participated in all 3 waves)
df <- df_full %>% filter(dropped == 0)

# import a lookup table with nice variable labels for plots
var_lookup <- readRDS("B-analysis-data/varlookup.rds")


# Run 2way fixed effect (2WFE) models --------------------------------------------------------

# first group outcome variables 
system_dvs <- c("system_satisfaction", "level_democracy", "supp_demcy",
                "proudcitizen")
soc_dvs <- c("solidarity","tolerate_immigrants", "soctrust", "no_surveillance")
extreme_dvs <- c("nfc", "misinfo", "populism")


# regress all outcomes on covid burden scale (cbs)
# lapply loops through the vector out outcomes, runs my.fe() on each and 
#       stores outputs in a list
#       do.call(rbind, ) then loops through list and binds each row into a
#       data.frame, where we also add DV and category names, labels
# N.B. I'm using the very same procedure many times below

cbs_2wfe <- do.call(rbind, lapply(c(system_dvs, soc_dvs, extreme_dvs),
                                 function(x) my.fe(df, x, "cbs"))) %>% 
        # add dv, iv and category names
        mutate(dv = c(system_dvs, soc_dvs, extreme_dvs),
               category = case_when(dv %in% soc_dvs ~ "Social solidarity",
                                    dv %in% system_dvs ~ "System support",
                                    dv %in% extreme_dvs ~ "Extreme discontent"),
               IV = "Burden of COVID-19") %>% 
        # attach labels for plotting
        left_join(var_lookup) %>% 
        # arrange by size for manual inspection
        arrange(desc(abs(estimate))) %>% 
        glimpse

# regress all outcomes on anomie
anomie_2wfe <- do.call(rbind, lapply(c(system_dvs, soc_dvs, extreme_dvs),
                                  function(x) my.fe(df, x, "anomie"))) %>% 
        mutate(dv = c(system_dvs, soc_dvs, extreme_dvs),
               category = case_when(dv %in% soc_dvs ~ "Social solidarity",
                                    dv %in% system_dvs ~ "System support",
                                    dv %in% extreme_dvs ~ "Extreme discontent"),
               IV = "Anomie") %>% 
        glimpse

# ~ Pooled estimates (aka "3WFE") --------------------------------------------------------

# we want to average across the 3*4 outcomes. 
# so first we stack the data such that each resp has 4 rows, and the responses to
#       the 4 outcomes are grouped under value, and the name of the outcome is under
#        name. first for system support
df_sys <- df %>% 
        dplyr::select(all_of(system_dvs), cbs, anomie, nochoice, cbs_full,
                      cbs_lead, anomie_lead, cbs_lag, anomie_lag,
                      cbs_health, cbs_finance, cbs_sociallife, cbs_anomie,
                      ID, ID_f, country, wave, country_wave, FE_weight, 
                      age, male, education, gov) %>% 
        pivot_longer(all_of(system_dvs)) %>% 
        glimpse

# we repeat this for each group. here for social variables
df_soc <- df %>% 
        dplyr::select(all_of(soc_dvs), cbs, anomie, nochoice, cbs_full,
                      cbs_lead, anomie_lead, cbs_lag, anomie_lag,
                      cbs_health, cbs_finance, cbs_sociallife, cbs_anomie,
                      ID, ID_f, country, wave, country_wave, FE_weight, 
                      age, male, education) %>% 
        pivot_longer(all_of(soc_dvs)) %>% 
        glimpse

# and finally for extreme discontent
df_ext <- df %>% 
        dplyr::select(all_of(extreme_dvs), cbs, anomie, nochoice, cbs_full,
                      cbs_lead, anomie_lead, cbs_lag, anomie_lag,
                      cbs_health, cbs_finance, cbs_sociallife, cbs_anomie,
                      ID, ID_f, country, wave, country_wave, FE_weight, 
                      age, male, education, gov) %>% 
        pivot_longer(all_of(extreme_dvs)) %>% 
        glimpse



# now we run fixed effects models not just with ID and wave, but outcome indicators
#       both with CBS and anomie
pooled <- bind_rows(my.fe(df_sys, IV = "cbs", twoway = F),
                    my.fe(df_sys, IV = "anomie", twoway = F),
                    my.fe(df_soc, IV = "cbs", twoway = F),
                    my.fe(df_soc, IV = "anomie", twoway = F),
                    my.fe(df_ext, IV = "cbs", twoway = F),
                    my.fe(df_ext, IV = "anomie", twoway = F)) %>% 
        # a bit of wrangling for nice lables
        mutate(category = rep(c("System support","Social solidarity", "Extreme discontent"), 
                              each = 2),
               dv = "Pooled",
               IV = rep(c("Burden of COVID-19", "Anomie"), 3)) 

# we combine the 2wfe models and the pooled estimates

# merge all 3 dfs, wrangle levels add labels
results <- bind_rows(select(cbs_2wfe, -label), 
                     anomie_2wfe, pooled) %>% 
        # more wrangling for nice label orders
        mutate(dv = fct_relevel(dv, "Pooled", as.character(cbs_2wfe$dv)), 
               IV = fct_relevel(IV, "Burden of COVID-19"), 
               category = fct_relevel(category, "Social solidarity", "System support"), 
               pooled = ifelse(dv == "Pooled", "1", "0")) %>% 
        left_join(var_lookup, by = "dv") %>% 
        mutate(label = fct_relevel(label, "Pooled", as.character(cbs_2wfe$label)))
 


# plot coefficient estimates
# this is not reported in the main text, b
ggplot(results, aes(x = fct_rev(label), y = estimate, color = pooled )) + 
        geom_point(aes( y=estimate)) + 
        geom_errorbar(aes(ymin = conf.low, ymax = conf.high),
                      width = 0) +
        geom_hline(aes(yintercept = 0))  +
        scale_color_manual(guide=NULL, values = c("grey", "black")) + 
        coord_flip() + 
        xlab("") + ylab("Coefficient estimates (95% CI)") +
        facet_grid(category ~ IV, scales = "free_y") +
        theme_bw() + 
        theme(axis.text.y = element_text(face = c(rep("plain", 4), "bold")))


# Scale to realistic change in CBS and Anomie ----------------------------------

# we are relying exclusively on within-unit variance in these FE models.
#       therefore it's prudent to scale coefficient estimates to realistic 
#       w/i unit (counterfactual) changes in independent variables. 
#       following Mummolo and Peterson 2018 PSRM recommendations:

# residualise CBS and anomie 
cbs.res <- felm(cbs ~ 1 | ID + country_wave, df)
anomie.res <- felm(anomie ~ 1 | ID + country_wave, df)
cbs.id <- length(unique(cbs.res$fe$ID)) #store number of unique respondents in model

cbs.id.list <- cbs.res$fe$ID
cbs.wave.list <- cbs.res$fe$country_wave

# combine residuals and IDs into a single df 
res.df <- tibble(cbs.res = cbs.res$residuals[, 1],
        anomie.res = anomie.res$residuals[, 1],
        ID = cbs.id.list,
        wave = cbs.wave.list) %>% 
        separate(wave, c("country", "wave"), sep = "_") %>% 
        arrange(ID, wave)

# calculate the within individual SD in the two indep vars.
res.df %>% 
        group_by(ID) %>% 
        summarise(cbs = sd(cbs.res), 
                  anomie = sd(anomie.res)) %>% 
        ungroup() %>% 
        # these look reasonable similar to simply taking the SD of all residuals
        summarise(cbs=  mean(cbs),
                  anomie = mean(anomie))

# calculate the largest within-unit change in the two IVs for each respondent. 
res.range <- res.df %>% 
        group_by(ID) %>% 
        summarise(cbs = max(cbs.res) - min(cbs.res),
                  anomie = max(anomie.res) - min(anomie.res)
                  ) %>% 
        ungroup()

# now store the values
cbs.m <- mean(res.range$cbs) #sample avg of max within unit change
anomie.m <- mean(res.range$anomie) 
range(res.range$cbs) # range of max within unit changes
range(res.range$anomie)
cbs.q <- quantile(res.range$cbs, .95)  # a realistic large within unit change
anomie.q <- quantile(res.range$anomie, .95)

# reported in the text (end of methods section!):
round(cbs.q, 2)
round(anomie.q, 2)
round(cbs.m, 2)
round(anomie.m, 2)
round(max(res.range$cbs), 2)
round(max(res.range$anomie), 2)

# create distribution plot on maximal within unit changes 
# This plot is reported as Figure OA5. 

ggplot(res.range, aes(x = cbs)) +
        geom_density()+
        # geom_histogram()+
        geom_vline(aes(xintercept = mean(cbs))) +
        geom_vline(aes(xintercept = quantile(cbs, .95)), linetype = "dashed") +
        xlab("Burden of COVID-19") +  ylab("") +
        annotate("segment", xend = cbs.m, x = cbs.m + .5,
                 y = 1.4, yend = 1.4, colour = "black", size=.3,
                 alpha=.8, arrow=arrow()) +
        annotate(geom = "label",label="Average", x=cbs.m + .6, y=1.4,
                 size = 4) +
        annotate("segment", xend = cbs.q, x = cbs.q + .4,
                 y = 1.2, yend = 1.2, colour = "black", size=.3,
                 alpha=.8, arrow=arrow()) +
        annotate(geom = "label",label="95th perc.", x=cbs.q + .7, y=1.2,
                 size = 4) +
        ylim(0, 1.6) + 
        theme_bw()  +
        
        ggplot(res.range, aes(x = anomie)) +
        geom_density()+
        geom_vline(aes(xintercept = mean(anomie))) + 
        geom_vline(aes(xintercept = quantile(anomie, .95)), linetype = "dashed") + 
        xlab("Anomie") + ylab("") + 
        annotate("segment", xend = anomie.m, x = anomie.m + .5,
                 y = 1.4, yend = 1.4, colour = "black", 
                 size=.3, alpha=.8, arrow=arrow()) +
        annotate(geom = "label",label="Average", x=anomie.m + 1, y=1.4,
                 size = 4) + 
        annotate("segment", xend = anomie.q, x = anomie.q + .7,
                 y = 1.2, yend = 1.2, colour = "black", size=.3, 
                 alpha=.8, arrow=arrow()) +
        annotate(geom = "label",label="95th perc.", x=anomie.q + 1.2,
                 y=1.2, size = 4) +
        ylim(0, 1.6) + 
        theme_bw()
ggsave("D-documents/figures/within-unit-change_density.jpg", height = 5)


# update values in coef plot with a large but realistic within-unit change in IVs
results_scaled_q95 <- results %>% 
        mutate(estimate = ifelse(term == "cbs", 
                                 estimate * quantile(res.range$cbs, .95),
                                 estimate *  quantile(res.range$anomie, .95)),
               conf.low = ifelse(term == "cbs", 
                                 conf.low *  quantile(res.range$cbs, .95),
                                 conf.low * quantile(res.range$anomie, .95)),
               conf.high = ifelse(term == "cbs", 
                                  conf.high *  quantile(res.range$cbs, .95),
                                  conf.high * quantile(res.range$anomie, .95)))

# for those who want to see coefs scaled to average within unit change
# results_scaled <- results %>% 
#         mutate(estimate = ifelse(term == "cbs", 
#                                  estimate * mean(res.range$cbs),
#                                  estimate * mean(res.range$anomie)),
#                conf.low = ifelse(term == "cbs", 
#                                  conf.low * mean(res.range$cbs),
#                                  conf.low * mean(res.range$anomie)),
#                conf.high = ifelse(term == "cbs", 
#                                   conf.high * mean(res.range$cbs),
#                                   conf.high * mean(res.range$anomie)))

# Figure 3. 2FE regression coefficients -----------------------------------
ggplot(results_scaled_q95, aes(x = fct_rev(label), y = estimate, color = pooled)) + 
        geom_point() +
        geom_errorbar(aes(ymin = conf.low, ymax = conf.high),
                      width = 0) + 
        geom_hline(aes(yintercept = 0))  +
        scale_color_manual(guide=NULL, values = c("darkgrey", "black")) + 
        coord_flip() +
        xlab("") + ylab("Coefficient estimates (95% CI)") +
        facet_grid(category ~ IV, scales = "free_y") +
        theme_minimal()

ggsave("D-documents/figures/main_fe_coefplot.jpg", height = 4.2, width = 7)

# values reported in the text 

results_scaled_q95 %>% 
        filter(dv == "Pooled" & category == "Social solidarity") %>% 
        select(term, estimate, conf.low, conf.high) %>% 
        mutate(across(estimate:conf.high, ~round(.x,2)))
               
results_scaled_q95 %>% 
        filter(dv == "Pooled" & category == "System support")%>% 
        select(term, estimate, conf.low, conf.high) %>% 
        mutate(across(estimate:conf.high, ~round(.x,2)))

results_scaled_q95 %>% 
        filter(dv == "Pooled" & category == "Extreme discontent")%>% 
        select(term, estimate, conf.low, conf.high) %>% 
        mutate(across(estimate:conf.high, ~round(.x,2)))

results_scaled_q95 %>% 
        filter(dv == "nfc" )%>% 
        select(term, estimate, conf.low, conf.high) %>% 
        mutate(across(estimate:conf.high, ~round(.x,2)))


# Export tables -----------------------------------------------------------

# A lean function to rerun models in Fig 3 and export full regression tables.
export.fe.tab <- function(data, 
                  DV = "value", 
                  IV, 
                  pooled = FALSE){
        
        # specify if we need only 2 (country_wave and respondent FE)
        #       or also one for DV (if it's a pooled model)
        if(pooled){
                myFE <-  "| country_wave + ID + name | 0 | ID"
        } else {
                myFE <-  "| country_wave + ID | 0 | ID"    
        } 
        
        # combine vars into one regression formula
        temp_formula <- formula(paste(DV, "~", IV, myFE))
        
        # run regression and store output
        fit <- felm(temp_formula, data = data,
                                 weights = data$FE_weight)
        fit
}

# create empty lists to store fitted models in
s_t <- list(NA) # system tables. stupid short names bc stargazer bug
soct <- list(NA)
ex_t <- list(NA)

# system support and social solidarity have 4 outcomes each. loop through them 
for(i in 1:4){
        s_t[[i]] <- export.fe.tab(df, DV = system_dvs[i], IV = "cbs")
        s_t[[i+4]] <- export.fe.tab(df, DV = system_dvs[i], IV = "anomie")
        soct[[i]] <- export.fe.tab(df, DV = soc_dvs[i], IV = "cbs")
        soct[[i+4]] <- export.fe.tab(df, DV = soc_dvs[i], IV = "anomie")
}

# extreme discontent is only 3 outcomes, so it needs it's own little loop
for(i in 1:3){
        ex_t[[i]] <- export.fe.tab(df, DV = extreme_dvs[i], IV = "cbs")
        ex_t[[i+3]] <- export.fe.tab(df, DV = extreme_dvs[i], IV = "anomie")
}

# Finally pooled outcomes are stored directly. 
s_t[[9]] <- export.fe.tab(df_sys, IV = "cbs")
s_t[[10]] <- export.fe.tab(df_sys, IV = "anomie")
soct[[9]] <- export.fe.tab(df_soc, IV = "cbs")
soct[[10]] <- export.fe.tab(df_soc, IV = "anomie")
ex_t[[9]] <-  export.fe.tab(df_ext, IV = "cbs")
ex_t[[10]] <- export.fe.tab(df_ext, IV = "anomie")

# Export well formatted regression tables. 
# First for system support and cbs 
stargazer(s_t[[9]],
          s_t[[1]],
          s_t[[2]],
          s_t[[3]],
          s_t[[4]],
          digits = 2, omit.stat = c("rsq", "ser"),
          covariate.labels = "Covid Burden",
          dep.var.labels = c("Pooled", "System Stsf.", "Lvl of Demcy", 
                             "Supp Demcy", "Proud Cit"),
          title = "Fixed effects regressions of system support on burden of COVID-19",
          label = "tab:sys_cbs",
          out = "D-documents/tables/cbs_system_FE.tex",
          type = "text")

# system support and anomie
stargazer(s_t[[10]],
          s_t[[5]],
          s_t[[6]],
          s_t[[7]],
          s_t[[8]],
          digits = 2, omit.stat = c("rsq", "ser"),
          covariate.labels = "Anomie",
          dep.var.labels = c("Pooled", "System Stsf.", "Lvl of Demcy", 
                             "Supp Demcy", "Proud Cit"),
          title = "Fixed effects regressions of system support on Anomie",
          label = "tab:sys_anomie",
          out = "D-documents/tables/anomie_system_FE.tex",
          type = "text")

# Social solidarity and CBS 
stargazer(soct[[9]],
          soct[[1]],
          soct[[2]],
          soct[[3]],
          soct[[4]],
          digits = 2, omit.stat = c("rsq", "ser"),
          covariate.labels = "Covid Burden",
          dep.var.labels = c("Pooled", "Supp for Redistr", "Toler Immig",
                             "Soc Trst", "Reject Surv"),
          title = "Fixed effects regressions of social solidarity on burden of COVID-19",
          label = "tab:soc_cbs",
          out = "D-documents/tables/cbs_soc_FE.tex",
          type = "text")

# Social solidarity and Anomie 
stargazer(soct[[10]],
          soct[[5]],
          soct[[6]],
          soct[[7]],
          soct[[8]],
          digits = 2, omit.stat = c("rsq", "ser"),
          covariate.labels = "Anomie",
          dep.var.labels = c("Pooled", "Supp for Redistr", "Toler Immig",
                             "Soc Trst", "Reject Surv"),
          title = "Fixed effects regressions of social solidarity on anomie",
          label = "tab:soc_anomie",
          out = "D-documents/tables/anomie_soc_FE.tex",
          type = "text")

# Extreme discontent and CBS

# extremeial solidarity and CBS 
stargazer(ex_t[[9]],
          ex_t[[1]],
          ex_t[[2]],
          ex_t[[3]],
          digits = 2, omit.stat = c("rsq", "ser"),
          covariate.labels = "Covid Burden",
          dep.var.labels = c("Pooled", "Need for Chaos", "Misinfo",
                             "Populism"),
          title = "Fixed effects regressions of extreme discontent on burden of COVID-19",
          label = "tab:extreme_cbs",
          out = "D-documents/tables/cbs_extreme_FE.tex",
          type = "text")

# extremeial solidarity and Anomie 
stargazer(ex_t[[10]],
          ex_t[[4]],
          ex_t[[5]],
          ex_t[[6]],
          digits = 2, omit.stat = c("rsq", "ser"),
          covariate.labels = "Anomie",
          dep.var.labels = c("Pooled", "Need for Chaos", "Misinfo",
                             "Populism"),
          title = "Fixed effects regressions of extreme discontent on anomie",
          label = "tab:extreme_anomie",
          out = "D-documents/tables/anomie_extreme_FE.tex",
          type = "text")

# Country-by-country splits -----------------------------------------------

cbs_country <- list()
anomie_country <- list()
pooled_country <- list()

# Loop through the four countries and rerun all models from above 
for(i in 1:4){ 
        mycountry <- c("Hungary", "Denmark", "USA", "Italy")[i]
        cbs_country[[i]] <- do.call(rbind, 
                                    lapply(c(system_dvs, soc_dvs, extreme_dvs),
                                           function(x) my.fe(filter(df, country == mycountry),
                                                             x, 
                                                             "cbs"))) %>% 
                mutate(dv = c(system_dvs, soc_dvs, extreme_dvs),
                       category = case_when(dv %in% soc_dvs ~ "Social solidarity",
                                            dv %in% system_dvs ~ "System support",
                                            dv %in% extreme_dvs ~ "Extreme discontent"),
                       IV = "Burden of COVID-19",
                       country = mycountry) 
        
        anomie_country[[i]] <- do.call(rbind, 
                                    lapply(c(system_dvs, soc_dvs, extreme_dvs),
                                           function(x) my.fe(filter(df, country == mycountry),
                                                             x, 
                                                             "anomie"))) %>% 
                mutate(dv = c(system_dvs, soc_dvs, extreme_dvs),
                       category = case_when(dv %in% soc_dvs ~ "Social solidarity",
                                            dv %in% system_dvs ~ "System support",
                                            dv %in% extreme_dvs ~ "Extreme discontent"),
                       IV = "Anomie",
                       country = mycountry) 
        
        pooled_country[[i]] <- bind_rows(my.fe(filter(df_sys, country == mycountry), 
                                          IV = "cbs", twoway = F),
                                    my.fe(filter(df_sys, country == mycountry), 
                                          IV = "anomie", twoway = F),
                                    my.fe(filter(df_soc, country == mycountry), 
                                          IV = "cbs", twoway = F),
                                    my.fe(filter(df_soc, country == mycountry), 
                                          IV = "anomie", twoway = F),
                                    my.fe(filter(df_ext, country == mycountry), 
                                          IV = "cbs", twoway = F),
                                    my.fe(filter(df_ext, country == mycountry), 
                                          IV = "anomie", twoway = F)) %>% 
                # a bit of wrangling for nice lables
                mutate(category = rep(c("System support","Social solidarity", "Extreme discontent"), each = 2),
                       dv = "Pooled",
                       IV = rep(c("Burden of COVID-19", "Anomie"), 3),
                       country = mycountry) 
        print(mycountry)
}

# now  combine all results in neat dfs
cbs_country <- do.call(rbind, cbs_country)
anomie_country <- do.call(rbind, anomie_country)
pooled_country <- do.call(rbind, pooled_country)

# clean up for plotting
country_results <- bind_rows(cbs_country, anomie_country, pooled_country) %>% 
        left_join(var_lookup) %>% 
        mutate(label = fct_relevel(label, "Pooled", as.character(cbs_2wfe$label)),
               IV = fct_relevel(IV, "Burden of COVID-19"), 
               category = fct_relevel(category, "Social solidarity", "System support"))

# Plot 
# This figure is reported as Figure OA7 in section C4 of the OA
ggplot(country_results, aes(x = fct_rev(label), y = estimate, color = country)) + 
        geom_point(position = position_dodge(.4)) +
        geom_errorbar(aes(ymin = conf.low, ymax = conf.high),
                      width = 0,position = position_dodge(.4)) + 
        geom_hline(aes(yintercept = 0))  +
        scale_color_viridis_d(name = NULL)+
        coord_flip() +
        xlab("") + ylab("Coefficient estimates (95% CI)") +
        facet_grid(category ~ IV, scales = "free_y") +
        theme_bw() +
        theme(legend.position = "bottom")

ggsave("D-documents/figures/fe_bycountry.jpg", height = 5, width = 8)



# Lead --------------------------------------------------------------------

cbs_lead <- do.call(rbind, lapply(c(system_dvs, soc_dvs, extreme_dvs),
                                              function(x) my.fe(df, x, "cbs", 
                                                                lead = T))) %>% 
        mutate(dv = rep(c(system_dvs, soc_dvs, extreme_dvs), each = 2),
               category = case_when(dv %in% soc_dvs ~ "Social solidarity",
                                    dv %in% system_dvs ~ "System support",
                                    dv %in% extreme_dvs ~ "Extreme discontent"),
               IV = "Burden of COVID-19") %>% 
        separate(term, c("term", "lead"), sep = "_") %>% 
        mutate(lead = ifelse(is.na(lead), "with_lead", lead)) 

anomie_lead <- do.call(rbind, lapply(c(system_dvs, soc_dvs, extreme_dvs),
                                  function(x) my.fe(df, x, "anomie", 
                                                    lead = T))) %>% 
        mutate(dv = rep(c(system_dvs, soc_dvs, extreme_dvs), each = 2),
               category = case_when(dv %in% soc_dvs ~ "Social solidarity",
                                    dv %in% system_dvs ~ "System support",
                                    dv %in% extreme_dvs ~ "Extreme discontent"),
               IV = "Anomie") %>% 
        separate(term, c("term", "lead"), sep = "_") %>% 
        mutate(lead = ifelse(is.na(lead), "with_lead", lead)) 


pooled_lead <- bind_rows(my.fe(df_sys, IV = "cbs", twoway = F, lead = T),
                    my.fe(df_sys, IV = "anomie", twoway = F, lead = T),
                    my.fe(df_soc, IV = "cbs", twoway = F, lead = T),
                    my.fe(df_soc, IV = "anomie", twoway = F, lead = T),
                    my.fe(df_ext, IV = "cbs", twoway = F, lead = T),
                    my.fe(df_ext, IV = "anomie", twoway = F, lead = T)) %>% 
        separate(term, c("term", "lead"), sep = "_") %>% 
        # a bit of wrangling for nice lables
        mutate(lead = ifelse(is.na(lead), "with_lead", lead),
               category = rep(c("System support","Social solidarity", "Extreme discontent"), each = 4),
               dv = "Pooled",
               IV = rep(c("Burden of COVID-19", 
                          "Burden of COVID-19",
                          "Anomie","Anomie"), 3)) 
        

results_lead <-  bind_rows(cbs_lead, anomie_lead, pooled_lead) %>% 
        left_join(var_lookup) %>% 
        bind_rows(mutate(results, lead = "no_lead")) %>% 
        mutate(dv = fct_relevel(dv, "Pooled", as.character(cbs_2wfe$dv)), 
               IV = fct_relevel(IV, "Burden of COVID-19"), 
               category = fct_relevel(category, "Social solidarity", "System support"),
               label = fct_relevel(label, "Pooled", as.character(cbs_2wfe$label)), 
               lead = fct_relevel(lead, "no_lead", "with_lead"))

# This figure is reported as Figure OA13 in section D2 of the OA
ggplot(results_lead, aes(x = fct_rev(label), y = estimate, color = fct_rev(lead))) + 
        geom_point(aes( y=estimate), position=position_dodge(.4)) + 
        geom_errorbar(aes(ymin = conf.low, ymax = conf.high),
                      width = 0, position=position_dodge(.4)) + 
        geom_hline(aes(yintercept = 0))  +
        scale_color_discrete(name = NULL, guide = guide_legend(reverse = TRUE)) +
        coord_flip() + 
        xlab("") + ylab("Coefficient estimates (95% CI)") +
        facet_grid(category ~ IV, scales = "free_y") +
        theme_bw() 
ggsave("D-documents/figures/lead_fe.jpg", height = 4.2, width = 7)



# Unit-specific time trends -----------------------------------------------
# AKA as respondent-specific time trends

cbs_unittime <- do.call(rbind, lapply(c(system_dvs, soc_dvs, extreme_dvs),
                                  function(x) my.fe(df, x, "cbs", 
                                                    twoway = F,
                                                    unittime = T))) %>% 
        mutate(dv = c(system_dvs, soc_dvs, extreme_dvs),
               category = case_when(dv %in% soc_dvs ~ "Social solidarity",
                                    dv %in% system_dvs ~ "System support",
                                    dv %in% extreme_dvs ~ "Extreme discontent"),
               IV = "Burden of COVID-19") 
        
anomie_unittime <- do.call(rbind, lapply(c(system_dvs, soc_dvs, extreme_dvs),
                                     function(x) my.fe(df, x, "anomie",  
                                                       twoway = F,
                                                       unittime = T))) %>% 
        mutate(dv = c(system_dvs, soc_dvs, extreme_dvs),
               category = case_when(dv %in% soc_dvs ~ "Social solidarity",
                                    dv %in% system_dvs ~ "System support",
                                    dv %in% extreme_dvs ~ "Extreme discontent"),
               IV = "Anomie")

pooled_unittime <- bind_rows(my.fe(df_sys, IV = "cbs", twoway = F, unittimepool = T),
                         my.fe(df_sys, IV = "anomie", twoway = F, unittimepool = T),
                         my.fe(df_soc, IV = "cbs", twoway = F, unittimepool = T),
                         my.fe(df_soc, IV = "anomie", twoway = F, unittimepool = T),
                         my.fe(df_ext, IV = "cbs", twoway = F, unittimepool = T),
                         my.fe(df_ext, IV = "anomie", twoway = F, unittimepool = T)) %>% 
        # a bit of wrangling for nice lables
        mutate(category = rep(c("System support","Social solidarity", 
                                "Extreme discontent"), each = 2),
               dv = "Pooled",
               IV = rep(c("Burden of COVID-19", 
                          "Anomie"), 3)) 

results_unittime <- bind_rows(cbs_unittime, anomie_unittime, pooled_unittime) %>% 
        mutate(unittime = "with_unittime") %>% 
        left_join(var_lookup) %>% 
        bind_rows(mutate(results, unittime = "no_unittime")) %>% 
        mutate(dv = fct_relevel(dv, "Pooled", as.character(cbs_2wfe$dv)), 
               IV = fct_relevel(IV, "Burden of COVID-19"), 
               category = fct_relevel(category, "Social solidarity", "System support"),
               label = fct_relevel(label, "Pooled", as.character(cbs_2wfe$label)), 
               unittime = fct_relevel(unittime, "no_unittime", "with_unittime")) %>% 
        glimpse()


# This figure is reported as Figure OA14 in section D2 of the OA
ggplot(results_unittime, aes(x = fct_rev(label), y = estimate, color = fct_rev(unittime))) + 
        geom_point(aes( y=estimate), position=position_dodge(.4)) + 
        geom_errorbar(aes(ymin = conf.low, ymax = conf.high),
                      width = 0, position=position_dodge(.4)) + 
        geom_hline(aes(yintercept = 0))  +
        scale_color_discrete(name = NULL, guide = guide_legend(reverse = TRUE)) +
        coord_flip() + 
        xlab("") + ylab("Coefficient estimates (95% CI)") +
        facet_grid(category ~ IV, scales = "free_y") +
        theme_bw() + 
        theme(axis.text.y = element_text(face = c(rep("plain", 4), "bold")))

ggsave("D-documents/figures/unittime_fe.jpg", height = 4.2, width = 7)



# 1 way fixed effects models  ---------------------------------------------
# Some say 2FE models are uninterpretable and it's usually preferable to 
#       report 1FE models (zooming in on within respondent or within time 
#       analyses). Here we contrast estimates across these 3 models

# ~ respondent fixed effects ----------------------------------------------


cbs_onewayresp <- do.call(rbind, lapply(c(system_dvs, soc_dvs, extreme_dvs),
                                      function(x) my.fe(df, x, "cbs", 
                                                        twoway = F,
                                                        onewayresp = T))) %>% 
        mutate(dv = c(system_dvs, soc_dvs, extreme_dvs),
               category = case_when(dv %in% soc_dvs ~ "Social solidarity",
                                    dv %in% system_dvs ~ "System support",
                                    dv %in% extreme_dvs ~ "Extreme discontent"),
               IV = "Burden of COVID-19") 

anomie_onewayresp <- do.call(rbind, lapply(c(system_dvs, soc_dvs, extreme_dvs),
                                         function(x) my.fe(df, x, "anomie",  
                                                           twoway = F,
                                                           onewayresp = T))) %>% 
        mutate(dv = c(system_dvs, soc_dvs, extreme_dvs),
               category = case_when(dv %in% soc_dvs ~ "Social solidarity",
                                    dv %in% system_dvs ~ "System support",
                                    dv %in% extreme_dvs ~ "Extreme discontent"),
               IV = "Anomie")

pooled_onewayresp <- bind_rows(my.fe(df_sys, IV = "cbs", twoway = F, onewayresppool = T),
                             my.fe(df_sys, IV = "anomie", twoway = F, onewayresppool = T),
                             my.fe(df_soc, IV = "cbs", twoway = F, onewayresppool = T),
                             my.fe(df_soc, IV = "anomie", twoway = F, onewayresppool = T),
                             my.fe(df_ext, IV = "cbs", twoway = F, onewayresppool = T),
                             my.fe(df_ext, IV = "anomie", twoway = F, onewayresppool = T)) %>% 
        # a bit of wrangling for nice labels
        mutate(category = rep(c("System support","Social solidarity", 
                                "Extreme discontent"), each = 2),
               dv = "Pooled",
               IV = rep(c("Burden of COVID-19", 
                          "Anomie"), 3)) 


results_onewayresp <- bind_rows(cbs_onewayresp, anomie_onewayresp, pooled_onewayresp) %>% 
        mutate(onetwoway = "oneway_resp") %>% 
        left_join(var_lookup) 


# ~ time fixed effects ----------------------------------------------------
my.timefe <-  function(data, 
                       DV = "value", 
                       IV,
                       pooled = F){
        
        myIV <- paste(IV, "+", "age + male + education")
        
        if(pooled){
                myFE <- "| country_wave + name"
        } else {
                myFE <- "| country_wave"
        }
        
        # combine formula of the regression 
        temp_formula <- formula(paste(DV, "~", myIV, "| country_wave"))
        
        # use weights? 
        broom::tidy(felm(temp_formula, data = data,
                         weights = data$FE_weight),
                    conf.int = TRUE) %>% 
                filter(term == IV)
}

cbs_onewaytime <- do.call(rbind, lapply(c(system_dvs, soc_dvs, extreme_dvs),
                                        function(x) my.timefe(df, x, "cbs"))) %>% 
        mutate(dv = c(system_dvs, soc_dvs, extreme_dvs),
               category = case_when(dv %in% soc_dvs ~ "Social solidarity",
                                    dv %in% system_dvs ~ "System support",
                                    dv %in% extreme_dvs ~ "Extreme discontent"),
               IV = "Burden of COVID-19") 

anomie_onewaytime <- do.call(rbind, lapply(c(system_dvs, soc_dvs, extreme_dvs),
                                           function(x) my.timefe(df, x, "anomie"))) %>% 
        mutate(dv = c(system_dvs, soc_dvs, extreme_dvs),
               category = case_when(dv %in% soc_dvs ~ "Social solidarity",
                                    dv %in% system_dvs ~ "System support",
                                    dv %in% extreme_dvs ~ "Extreme discontent"),
               IV = "Anomie")

pooled_onewaytime <- bind_rows(my.timefe(df_sys, IV = "cbs", pooled = T),
                               my.timefe(df_sys, IV = "anomie", pooled = T),
                               my.timefe(df_soc, IV = "cbs", pooled = T),
                               my.timefe(df_soc, IV = "anomie", pooled = T),
                               my.timefe(df_ext, IV = "cbs", pooled = T),
                               my.timefe(df_ext, IV = "anomie", pooled = T)) %>% 
        # a bit of wrangling for nice lables
        mutate(category = rep(c("System support","Social solidarity", 
                                "Extreme discontent"), each = 2),
               dv = "Pooled",
               IV = rep(c("Burden of COVID-19", 
                          "Anomie"), 3)) 



results_onewaytime <- bind_rows(cbs_onewaytime, anomie_onewaytime,
                                pooled_onewaytime) %>% 
        mutate(onetwoway = "oneway_time") %>% 
        left_join(var_lookup) 

results_onetwoway <- bind_rows(results_onewayresp, results_onewaytime,
                                  mutate(results, onetwoway = "twowayfe")) %>% 
        mutate(dv = fct_relevel(dv, "Pooled", as.character(cbs_2wfe$dv)), 
               IV = fct_relevel(IV, "Burden of COVID-19"), 
               category = fct_relevel(category, "Social solidarity", "System support"),
               label = fct_relevel(label, "Pooled", as.character(cbs_2wfe$label)), 
               onewayresp = fct_relevel(onetwoway, "twowayfe", "oneway_resp")) %>% 
        glimpse()

# This figure is reported as Figure OA16 in section D4 of the OA
ggplot(results_onetwoway, aes(x = fct_rev(label), 
                              y = estimate, 
                              color = fct_rev(onewayresp))) + 
        geom_point(aes( y=estimate), position=position_dodge(.4)) + 
        geom_errorbar(aes(ymin = conf.low, ymax = conf.high),
                      width = 0, position=position_dodge(.4)) + 
        geom_hline(aes(yintercept = 0))  +
        scale_color_discrete(name = NULL, guide = guide_legend(reverse = TRUE)) +
        coord_flip() + 
        xlab("") + ylab("Coefficient estimates (95% CI)") +
        facet_grid(category ~ IV, scales = "free_y") +
        theme_bw() + 
        theme(axis.text.y = element_text(face = c(rep("plain", 4), "bold")))

ggsave("D-documents/figures/onetwoway_fe.jpg", height = 4.2, width = 7)


# reported in the text: 
results_onewaytime %>% 
        filter(term == "cbs" & dv == "soctrust") %>% 
        mutate(across(is.numeric, ~round(.x, 2))) %>% 
        select(estimate, conf.high,conf.low)

results_onewaytime %>% 
        filter(term == "cbs" & dv == "solidarity") %>% 
        mutate(across(is.numeric, ~round(.x, 2))) %>% 
        select(estimate, conf.high,conf.low)

# calculate the proporiton and percentage point difference for each estimate
#       across the two models
onetwoway_differences <- results_onetwoway %>% 
        filter(onetwoway != "oneway_resp") %>%
        mutate(dv = ifelse(dv == "Pooled", paste0(dv,category), as.character(dv))) %>% 
        select(term, dv, category, estimate, onetwoway) %>% 
        pivot_wider(values_from = estimate, names_from = onetwoway) %>% 
        mutate(prop = round(`oneway_time` / `twowayfe`, 2), 
               diff = round(`oneway_time` - `twowayfe`, 2)) %>% 
        arrange(prop) 

# report range in the text
onetwoway_differences %>% 
        filter(category != "Social solidarity") %>% 
        group_by(category) %>% 
        summarise(across(c(prop, diff), list(min = min, max = max)))

# Control for government support ------------------------------------------
df %>% 
        select(gov,all_of(system_dvs)) %>% 
        psych::corr.test()
round(cor(df_sys$value, df_sys$gov), 2)

govcontr <- bind_rows(
        do.call(rbind, lapply(system_dvs, function(x) 
                my.fe(df, x, "cbs", controlgovt = T))),
        my.fe(df_sys, IV = "cbs", twoway = F, controlgovt = T),
        do.call(rbind, lapply(system_dvs, function(x) 
                my.fe(df, x, "anomie", controlgovt = T))),
        my.fe(df_sys, IV = "anomie", twoway = F, controlgovt = T)
) %>%
        filter(term != "gov") %>% 
        mutate(dv = rep(c(system_dvs, "Pooled"), 2), 
               IV = rep(c("Burden of COVID-19", "Anomie"), each = 5), 
               control = "with control")  %>% 
        left_join(var_lookup) %>% 
        bind_rows(
                mutate(filter(results, category == "System support"), 
                       control = "without control")
        ) %>% 
        mutate(IV = fct_relevel(IV, "Burden of COVID-19"),
               label = fct_relevel(label, "Pooled"))

# reported in the text:
# size of association with and without control
govcontr %>% 
        filter(dv == "Pooled") %>% 
        select(term, estimate, control) %>% 
        pivot_wider(values_from = estimate, names_from = control) %>% 
        mutate(diff = round(`with control` / `without control`, 2))

# This figure is reported as Figure OA15 in section D3 of the OA        
ggplot(govcontr, aes(x = fct_rev(label), y = estimate, color = fct_rev(control))) +
        geom_point(aes( y=estimate), position=position_dodge(.4)) + 
        geom_errorbar(aes(ymin = conf.low, ymax = conf.high),
                      width = 0, position=position_dodge(.4)) + 
        geom_hline(aes(yintercept = 0))  +
        scale_color_discrete(name = NULL, guide = guide_legend(reverse = TRUE)) +
        coord_flip() + 
        xlab("") + ylab("Coefficient estimates (95% CI)") +
        facet_grid(. ~ IV, scales = "free_y") +
        theme_bw()

ggsave("D-documents/figures/govcontr.jpg")


# Drop weights  -----------------------------------------------------------


# regress all outcomes on covid burden scale (cbs)
cbs_noweights <- do.call(rbind, lapply(c(system_dvs, soc_dvs, extreme_dvs),
                                  function(x) my.fe(df, x, "cbs", weights = F))) %>% 
        mutate(dv = c(system_dvs, soc_dvs, extreme_dvs),
               category = case_when(dv %in% soc_dvs ~ "Social solidarity",
                                    dv %in% system_dvs ~ "System support",
                                    dv %in% extreme_dvs ~ "Extreme discontent"),
               IV = "Burden of COVID-19") %>% 
        left_join(var_lookup) %>% 
        arrange(desc(abs(estimate))) %>% 
        glimpse

# regress all outcomes on anomie
anomie_noweights <- do.call(rbind, lapply(c(system_dvs, soc_dvs, extreme_dvs),
                                     function(x) my.fe(df, x, "anomie", weights = F))) %>% 
        mutate(dv = c(system_dvs, soc_dvs, extreme_dvs),
               category = case_when(dv %in% soc_dvs ~ "Social solidarity",
                                    dv %in% system_dvs ~ "System support",
                                    dv %in% extreme_dvs ~ "Extreme discontent"),
               IV = "Anomie") %>% 
        glimpse

pooled_noweights <- bind_rows(my.fe(df_sys, IV = "cbs", twoway = F, weights = F),
                              my.fe(df_sys, IV = "anomie", twoway = F, weights = F),
                              my.fe(df_soc, IV = "cbs", twoway = F, weights = F),
                              my.fe(df_soc, IV = "anomie", twoway = F, weights = F),
                              my.fe(df_ext, IV = "cbs", twoway = F, weights = F),
                              my.fe(df_ext, IV = "anomie", twoway = F, weights = F)) %>% 
        # a bit of wrangling for nice lables
        mutate(category = rep(c("System support","Social solidarity", "Extreme discontent"), 
                              each = 2),
               dv = "Pooled",
               IV = rep(c("Burden of COVID-19", "Anomie"), 3)) 

results_noweights <- bind_rows(select(cbs_noweights, -label), 
                     anomie_noweights, pooled_noweights) %>% 
        # more wrangling for nice label orders
        mutate(dv = fct_relevel(dv, "Pooled", as.character(cbs_2wfe$dv)), 
               IV = fct_relevel(IV, "Burden of COVID-19"), 
               category = fct_relevel(category, "Social solidarity", "System support"), 
               pooled = ifelse(dv == "Pooled", "1", "0")) %>% 
        left_join(var_lookup, by = "dv") %>% 
        mutate(label = fct_relevel(label, "Pooled", as.character(cbs_2wfe$label)))


# This figure is reported as Figure OA17 in section D5 of the OA
bind_rows(list("weights" = results, "noweights" = results_noweights), .id = "weights") %>% 

ggplot(., aes(x = fct_rev(label), y = estimate, color = weights )) + 
        geom_point(aes( y=estimate), position = position_dodge(.3)) + 
        geom_errorbar(aes(ymin = conf.low, ymax = conf.high),
                      width = 0,position = position_dodge(.3)) + 
        geom_hline(aes(yintercept = 0))  +
        coord_flip() + 
        xlab("") + ylab("Coefficient estimates (95% CI)") +
        facet_grid(category ~ IV, scales = "free_y") +
        theme_bw() + 
        theme(axis.text.y = element_text(face = c(rep("plain", 4), "bold")))

ggsave("D-documents/figures/rob_noweights.jpg", height = 4.2, width = 7.7)



# No choice - short anomie scale  -----------------------------------------

# regress all outcomes on covid burden scale (cbs)

# regress all outcomes on anomie
anomie_nochoice <- do.call(rbind, lapply(c(system_dvs, soc_dvs, extreme_dvs),
                                          function(x) my.fe(df, x, "nochoice"))) %>% 
        mutate(dv = c(system_dvs, soc_dvs, extreme_dvs),
               category = case_when(dv %in% soc_dvs ~ "Social solidarity",
                                    dv %in% system_dvs ~ "System support",
                                    dv %in% extreme_dvs ~ "Extreme discontent"),
               IV = "Anomie (short)") %>% 
        glimpse

pooled_nochoice <- bind_rows(my.fe(df_sys, IV = "nochoice", twoway = F),
                              my.fe(df_soc, IV = "nochoice", twoway = F),
                              my.fe(df_ext, IV = "nochoice", twoway = F)) %>% 
        # a bit of wrangling for nice lables
        mutate(category = c("System support","Social solidarity", "Extreme discontent"),
               dv = "Pooled",
               IV = "Anomie (short)") 

results_nochoice <- bind_rows(anomie_nochoice, pooled_nochoice) %>% 
        # more wrangling for nice label orders
        mutate(dv = fct_relevel(dv, "Pooled", as.character(cbs_2wfe$dv)), 
               category = fct_relevel(category, "Social solidarity", "System support"), 
               pooled = ifelse(dv == "Pooled", "1", "0")) %>% 
        left_join(var_lookup) %>% 
        mutate(label = fct_relevel(label, "Pooled", as.character(cbs_2wfe$label)))

# This figure is reported as Figure OA11 in section C8 of the OA
bind_rows(list("Long" = filter(results, term == "anomie"), 
               "Short" = results_nochoice), .id = "Anomie") %>% 
        
        ggplot(., aes(x = fct_rev(label), y = estimate, color = Anomie )) + 
        geom_point(aes( y=estimate), position = position_dodge(.3)) + 
        geom_errorbar(aes(ymin = conf.low, ymax = conf.high),
                      width = 0,position = position_dodge(.3)) + 
        geom_hline(aes(yintercept = 0))  +
        coord_flip() + 
        xlab("") + ylab("Coefficient estimates (95% CI)") +
        facet_grid(category ~ ., scales = "free_y") +
        theme_bw()

ggsave("D-documents/figures/rob_nochoice.jpg", height = 4.2, width =6)

# Additional outcomes  ----------------------------------------------------

bonus_dvs <- c("gov", "polarized_abs", "finance")

bonus_2wfe <- rbind(do.call(rbind, lapply(bonus_dvs, 
                                          function(x) my.fe(df, x, "cbs"))),
                do.call(rbind, lapply(bonus_dvs, 
                                      function(x) my.fe(df, x, "anomie")))) %>% 
        mutate(dv = rep(c("Government support", 
                          "Affective polarization", "Financial prospects"), 2),
               IV = rep(c("Burden of COVID-19", "Anomie"), 
                        each = 3),
               IV = fct_relevel(IV, "Burden of COVID-19"))%>% 
        glimpse

# This figure is reported as Figure OA8 in section C5 of the OA
ggplot(bonus_2wfe, aes(x = dv, y = estimate)) + 
        geom_point(aes( y=estimate)) + 
        geom_errorbar(aes(ymin = conf.low, ymax = conf.high),
                      width = 0) + 
        geom_hline(aes(yintercept = 0))  +
        coord_flip() + 
        xlab("") + ylab("Coefficient estimates (95% CI)") +
        facet_grid(. ~ IV, scales = "free_y") +
        # facet_grid(category ~ IV, scales = "fixed") + 
        theme_bw()
ggsave("D-documents/figures/bonus_DVs.jpg", height = 3, width = 7)



# CBS Full scale ----------------------------------------------------------

cbs_full <- do.call(rbind, lapply(c(system_dvs, soc_dvs, extreme_dvs),
                                    function(x) my.fe(df, x, "cbs_full"))) %>% 
        mutate(dv = c(system_dvs, soc_dvs, extreme_dvs),
               category = case_when(dv %in% soc_dvs ~ "Social solidarity",
                                    dv %in% system_dvs ~ "System support",
                                    dv %in% extreme_dvs ~ "Extreme discontent"),
               IV = "Full Scale") %>% 
        left_join(var_lookup) %>%
        arrange(desc(abs(estimate))) %>% 
        glimpse


pooled_full <- bind_rows(my.fe(df_sys, IV = "cbs_full", twoway = F),
                            my.fe(df_soc, IV = "cbs_full", twoway = F),
                            my.fe(df_ext, IV = "cbs_full", twoway = F)) %>% 
        # a bit of wrangling for nice labels
        mutate(category = c("System support","Social solidarity", "Extreme discontent"),
               dv = "Pooled",
               IV = "Full Scale") %>% 
        left_join(var_lookup) %>%
        glimpse

cbs_full_pooled <- bind_rows(cbs_full, pooled_full,
                             mutate(filter(pooled, IV == "Burden of COVID-19"), 
                                    label = "Pooled"),
                             cbs_2wfe) %>% 
        mutate(category = fct_relevel(category, "Social solidarity", "System support"),
               label = fct_relevel(label, "Pooled", as.character(cbs_2wfe$label)))


# This figure is reported as Figure OA12 in section D1 of the OA
ggplot(cbs_full_pooled, aes(x = fct_rev(label), y = estimate, color = IV)) + 
        geom_point(position = position_dodge(.4)) +
        geom_errorbar(aes(ymin = conf.low, ymax = conf.high),
                      width = 0, position = position_dodge(.4)) + 
        geom_hline(aes(yintercept = 0))  +
        scale_color_discrete(name = NULL)+
        coord_flip() +
        xlab("") + ylab("Coefficient estimates (95% CI)") +
        facet_grid(category ~ ., scales = "free_y") +
        theme_bw() +
        theme(legend.position = "bottom")
ggsave("D-documents/figures/cbs_full.jpg", height = 5, width = 6)

# CBS factors -------------------------------------------------------------


cbs_health <- do.call(rbind, lapply(c(system_dvs, soc_dvs, extreme_dvs),
                                  function(x) my.fe(df, x, "cbs_health"))) %>% 
        mutate(dv = c(system_dvs, soc_dvs, extreme_dvs),
               category = case_when(dv %in% soc_dvs ~ "Social solidarity",
                                    dv %in% system_dvs ~ "System support",
                                    dv %in% extreme_dvs ~ "Extreme discontent"),
               IV = "Health") %>% 
        arrange(desc(abs(estimate))) %>% 
        glimpse

cbs_finance <- do.call(rbind, lapply(c(system_dvs, soc_dvs, extreme_dvs),
                                    function(x) my.fe(df, x, "cbs_finance"))) %>% 
        mutate(dv = c(system_dvs, soc_dvs, extreme_dvs),
               category = case_when(dv %in% soc_dvs ~ "Social solidarity",
                                    dv %in% system_dvs ~ "System support",
                                    dv %in% extreme_dvs ~ "Extreme discontent"),
               IV = "Finance") %>% 
        arrange(desc(abs(estimate))) %>% 
        glimpse

cbs_sociallife <- do.call(rbind, lapply(c(system_dvs, soc_dvs, extreme_dvs),
                                     function(x) my.fe(df, x, "cbs_sociallife"))) %>% 
        mutate(dv = c(system_dvs, soc_dvs, extreme_dvs),
               category = case_when(dv %in% soc_dvs ~ "Social solidarity",
                                    dv %in% system_dvs ~ "System support",
                                    dv %in% extreme_dvs ~ "Extreme discontent"),
               IV = "Social Life") %>% 
        arrange(desc(abs(estimate))) %>% 
        glimpse

cbs_anomie <- do.call(rbind, lapply(c(system_dvs, soc_dvs, extreme_dvs),
                                        function(x) my.fe(df, x, "cbs_anomie"))) %>% 
        mutate(dv = c(system_dvs, soc_dvs, extreme_dvs),
               category = case_when(dv %in% soc_dvs ~ "Social solidarity",
                                    dv %in% system_dvs ~ "System support",
                                    dv %in% extreme_dvs ~ "Extreme discontent"),
               IV = "Anomie") %>% 
        arrange(desc(abs(estimate))) %>% 
        glimpse

pooled_factors <- bind_rows(my.fe(df_sys, IV = "cbs_health", twoway = F),
                            my.fe(df_sys, IV = "cbs_finance", twoway = F),
                            my.fe(df_sys, IV = "cbs_sociallife", twoway = F),
                            my.fe(df_sys, IV = "cbs_anomie", twoway = F),
                            my.fe(df_soc, IV = "cbs_health", twoway = F),
                            my.fe(df_soc, IV = "cbs_finance", twoway = F),
                            my.fe(df_soc, IV = "cbs_sociallife", twoway = F),
                            my.fe(df_soc, IV = "cbs_anomie", twoway = F),
                            my.fe(df_ext, IV = "cbs_health", twoway = F),
                            my.fe(df_ext, IV = "cbs_finance", twoway = F),
                            my.fe(df_ext, IV = "cbs_sociallife", twoway = F),
                            my.fe(df_ext, IV = "cbs_anomie", twoway = F)
                            ) %>% 
        # a bit of wrangling for nice labels
        mutate(category = rep(c("System support","Social solidarity", "Extreme discontent"), each = 4),
               dv = "Pooled",
               IV = rep(c("Health", "Finance", "Social Life", "Anomie"), 3))

cbs_factors <- bind_rows(cbs_health, cbs_finance, cbs_sociallife, cbs_anomie, pooled_factors) %>%
        left_join(var_lookup) %>% 
        mutate(category = fct_relevel(category, "Social solidarity", "System support"),
               label = fct_relevel(label, "Pooled", as.character(cbs_2wfe$label)))

# This figure is reported as Figure OA9 in section C6 of the OA
ggplot(cbs_factors, aes(x = fct_rev(label), y = estimate, color = IV)) + 
        geom_point(position = position_dodge(.4)) +
        geom_errorbar(aes(ymin = conf.low, ymax = conf.high),
                      width = 0, position = position_dodge(.4)) + 
        geom_hline(aes(yintercept = 0))  +
        scale_color_viridis_d(name = NULL)+
        coord_flip() +
        xlab("") + ylab("Coefficient estimates (95% CI)") +
        facet_grid(category ~ ., scales = "free_y") +
        theme_bw() +
        theme(legend.position = "bottom")
ggsave("D-documents/figures/cbs_byfactor.jpg", height = 6, width = 8)

# CBS and Anomie explained ------------------------------------------------

# ~ CBS -------------------------------------------------------------------

# Build model --------------------- -  --------------------- - 
# start with only country random intercepts
cbs1 <- lmer(cbs ~ age_c + male + education + 
                     (1 | country), df)

# add wave random intercepts
cbs2 <- lmer(cbs ~ age_c + male + education + 
                     (1 | country) + 
                     (1 | wave), df)
anova(cbs1, cbs2) # model fit improved! 

# add an interaction between country and wave as random intercept
cbs3 <- lmer(cbs ~ age_c + male + education + 
                       (1 | country) + 
                       (1 | wave) + 
                       (1 | country:wave), df)
anova(cbs2, cbs3) # model fit improved! 

# add varying slopes by countryfor age. 
cbs4 <- lmer(cbs ~  1 + age_c + male + education + 
                       (1 + age_c | country) + 
                       (1 | wave) + 
                       (1 | country:wave), df)
anova(cbs3, cbs4)  # model fit improved! 

# add varying slopes by country for education 
cbs5 <- lmer(cbs ~  1 + age_c + male + education + 
                       (1 + age_c + education | country) + 
                       (1 | wave) + 
                       (1 | country:wave), df)
anova(cbs4, cbs5)  # model fit improved! 

# add varying slopes by country for gender
cbs6 <- lmer(cbs ~  1 + age_c + male + education + 
                       (1 + age_c + male + education | country) + 
                       (1 | wave) + 
                       (1 | country:wave), df)
anova(cbs5, cbs6)  # no improvement

# add varying slopes by wave for age 
cbs7 <- lmer(cbs ~  1 + age_c + male + education + 
                       (1 + age_c + education | country) + 
                       (1 + age_c  | wave) + 
                       (1 | country:wave), df)
anova(cbs5, cbs7) # no improvement 

# add varying slopes by country:wave for age.
cbs8 <- lmer(cbs ~  1 + age_c + male + education + 
                       (1 + age_c + education | country) + 
                       (1  | wave) + 
                       (1 + age_c  | country:wave), df)
anova(cbs5, cbs8) # no improvement

# add varying slopes by wave for education
cbs9 <- lmer(cbs ~  1 + age_c + male + education + 
                     (1 + age_c + education | country) + 
                     (1 + education  | wave) + 
                     (1 | country:wave), df)
anova(cbs5, cbs9)

# our best model is having troubles converging using maximum likelihood 
#       let's rerun in bayes with default weakly informative priors.
#       this takes 30mins to run so I'll comment it out
# cbs5.stan <- stan_lmer(cbs ~  1 + age_c + male + education + 
#                                (1 + age_c + education | country) + 
#                                (1 | wave) + 
#                                (1 | country:wave), df, cores = 4)
# saveRDS(cbs5.stan, "B-analysis-data/models/cbs5.rds")
cbs5.stan <- readRDS("B-analysis-data/models/cbs5.rds")

summary(cbs5.stan, regex = c("^age_c", "^education"), 
        digits =2)

# look at the posterior distribution of coef estimates 
cbs_age <- cbs5.stan %>% 
        spread_draws(age_c, b[term, group]) %>% 
        filter(term == "age_c") %>% 
        median_qi(b = age_c + b, .width = c(.67, .89, .97)) %>% print

cbs_edu2 <- cbs5.stan %>% 
        spread_draws(education2, b[term, group]) %>% 
        filter(term == "education2") %>% 
        median_qi(b = education2 + b, .width = c(.67, .89, .97)) %>% print

cbs_edu3 <- cbs5.stan %>% 
        spread_draws(education3, b[term, group]) %>% 
        filter(term == "education3") %>% 
        median_qi(b = education3 + b, .width = c(.67, .89, .97)) %>% print

cbs_all <- bind_rows(cbs_age, cbs_edu2, cbs_edu3) %>% 
        ungroup() %>% 
        mutate(group = gsub("country:", "", group), 
               label = fct_recode(term, 
                                  Age = "age_c", 
                                  `Secondary ed.` = "education2",
                                  `Higher ed.` = "education3"))


# ~ Anomie ----------------------------------------------------------------

# Build model --------------------- -  --------------------- - 
# start with only country random intercepts
anomie1 <- lmer(anomie ~ age_c + male + education + 
                     (1 | country), df)

# add wave random intercepts
anomie2 <- lmer(anomie ~ age_c + male + education + 
                     (1 | country) + 
                     (1 | wave), df)
anova(anomie1, anomie2) # model fit improved! 

# add an interaction between country and wave as random intercept
anomie3 <- lmer(anomie ~ age_c + male + education + 
                     (1 | country) + 
                     (1 | wave) + 
                     (1 | country:wave), df)
anova(anomie2, anomie3) # no improvement

# add varying slopes by countryfor age. 
anomie4 <- lmer(anomie ~  1 + age_c + male + education + 
                     (1 + age_c | country) + 
                     (1 | wave), df)
anova(anomie2, anomie4)  # model fit improved! 

# add varying slopes by country for education 
anomie5 <- lmer(anomie ~  1 + age_c + male + education + 
                     (1 + age_c + education | country) + 
                     (1 | wave) , df)
anova(anomie4, anomie5)  # model fit improved! 

# add varying slopes by country for gender
anomie6 <- lmer(anomie ~  1 + age_c + male + education + 
                     (1 + age_c + male + education | country) + 
                     (1 | wave), df)
anova(anomie5, anomie6)  # no improvement

# add varying slopes by wave for age 
anomie7 <- lmer(anomie ~  1 + age_c + male + education + 
                     (1 + age_c + education | country) + 
                     (1 + age_c  | wave), df)
anova(anomie5, anomie7) # no improvement 

# add varying slopes by wave for education
anomie8 <- lmer(anomie ~  1 + age_c + male + education + 
                        (1 + age_c + education | country) + 
                        (1 + education  | wave), df)
anova(anomie5, anomie8) # no improvement

# our best model is having troubles converging using maximum likelihood 
#       let's rerun in bayes with default weakly informative priors.
#       this takes 30mins to run so I'll comment it out
# anomie5.stan <- stan_lmer(anomie ~  1 + age_c + male + education + 
#                                (1 + age_c + education | country) + 
#                                (1 | wave), df, cores = 4)
# saveRDS(anomie5.stan, "B-analysis-data/models/anomie5.rds")
anomie5.stan <- readRDS("B-analysis-data/models/anomie5.rds")
# interpret model results 

summary(anomie5.stan, regex = c("^age_c", "^education"), 
        digits =2)

# posteriors on coefs  --------------------- -  --------------------- - 

anomie_age <- anomie5.stan %>% 
        spread_draws(age_c, b[term, group]) %>% 
        filter(term == "age_c") %>% 
        median_qi(b = age_c + b, .width = c(.67, .89, .97)) %>% print

anomie_edu2 <- anomie5.stan %>% 
        spread_draws(education2, b[term, group]) %>% 
        filter(term == "education2") %>% 
        median_qi(b = education2 + b, .width = c(.67, .89, .97)) %>% print

anomie_edu3 <- anomie5.stan %>% 
        spread_draws(education3, b[term, group]) %>% 
        filter(term == "education3") %>% 
        median_qi(b = education3 + b, .width = c(.67, .89, .97)) %>% print

anomie_all <- bind_rows(anomie_age, anomie_edu2, anomie_edu3) %>% 
        ungroup() %>% 
        mutate(group = gsub("country:", "", group), 
               label = fct_recode(term, 
                                  Age = "age_c", 
                                  `Secondary ed.` = "education2",
                                  `Higher ed.` = "education3"))
# Combine all estimates into a single DF
burden_all <- bind_rows(cbs_age, cbs_edu2, cbs_edu3,
                        anomie_age, anomie_edu2, anomie_edu3) %>% 
        ungroup() %>% 
        mutate(group = gsub("country:", "", group), 
               label = fct_recode(term, 
                                  Age = "age_c", 
                                  `Secondary ed.` = "education2",
                                  `Higher ed.` = "education3"),
               dv = rep(c("Burden of COVID-19", "Anomie"),
                        each= 36), 
               dv = fct_relevel(dv, "Burden of COVID-19"))

# This figure is reported as Figure OA4 in section C1 of the OA
ggplot(burden_all, aes(y = label, x = b, xmin = .lower, xmax = .upper)) +
        geom_vline(aes(xintercept = 0)) +
        facet_grid(dv ~ group) +
        geom_pointinterval() +
        xlab("Coefficient estimates") + 
        ylab("") + 
        coord_flip()+ 
        theme_bw() +
        theme(axis.text.x = element_text(size = 7))
ggsave("D-documents/figures/burden_coefs.jpg", width = 9)


# Test-retest -------------------------------------------------------------

# nifty function calculating the test-retest correlations between all waves 
#        for a given variable
tr.corr <- function(var){
        df %>%  
                dplyr::select(country,wave, ID, !!var) %>% 
                pivot_wider(values_from = !!var, names_from = wave,
                            names_prefix="wave") %>% 
                group_by(country) %>% 
                summarise(cor12 = cor(wave1, wave2, use = "complete.obs"),
                          cor23 = cor(wave2, wave3, use = "complete.obs"),
                          cor13 = cor(wave1, wave3, use = "complete.obs")) 
}


# store name of all variables
allvars <- c(system_dvs, soc_dvs, extreme_dvs, bonus_dvs, "cbs", "anomie")


# loop through all variables and calculate correlations
tr.results <- do.call(rbind, lapply(allvars, function(x)tr.corr(quo(x)))) %>% 
        mutate(dv = rep(allvars, each = 4)) %>% # add variable names
        left_join(var_lookup)


tr.results %>% 
        pivot_longer(cor12:cor13, names_to = "wave", values_to = "cor") %>% 
        mutate(wave = fct_recode(wave, 
                                 `Waves 1-2` = "cor12",
                                 `Waves 1-3` = "cor13",
                                 `Waves 2-3` = "cor23"
                                 )) %>% 
# This plot is reported as Figure OA2 in Section B4 of the OA         
ggplot(aes(y = fct_reorder(label, cor), x = cor, color = wave)) + 
        geom_vline(aes(xintercept = 0)) + 
        geom_point(position = position_dodge(.3)) +
        facet_grid(~country) + 
        scale_color_discrete(name = NULL) +
        xlab("Test-retest correlations") +
        ylab("") +
        theme_bw() + 
        theme(legend.position="bottom")
ggsave("D-documents/figures/long_testretest_indices.jpg")

# Alpha reliability -------------------------------------------------------

nameslist <- readRDS("B-analysis-data/nameslist.rds")[-1]
alpha.frame <- expand.grid(country = c("Denmark", "Italy", "Hungary", "USA"),
                           wave = 1:3, 
                           variable = as.character(names(nameslist))) %>% 
        # ais and ris were not measured in wave 1. delete these rows
        filter(!(wave == 1 & variable %in% c("ais", "ris")))


for(i in 1:nrow(alpha.frame)){
        temp.df <- df %>% 
                filter(country == alpha.frame$country[i] & 
                               wave == alpha.frame$wave[i])
        temp_vars <- unlist(nameslist[as.character(alpha.frame$variable[i])])
        alpha.frame$alpha[i] <- psych::alpha(dplyr::select(temp.df, all_of(temp_vars)),
                                             warnings=FALSE)$total$raw_alpha
}


# This plot is reported as Figure OA3 in Section B4 of the OA
alpha.frame %>% 
        left_join(var_lookup, by = c("variable" = "dv")) %>% 
ggplot(., aes(y = fct_reorder(label, alpha),
                        x = alpha, color = as.factor(wave))) +
        geom_point(position = position_dodge(.3)) +
        facet_grid(~country) +
        scale_color_discrete(name = "Wave:") +
        xlab("Alpha reliability") + ylab("") +
        theme_bw() +
        theme(legend.position = "bottom")
ggsave("D-documents/figures/long_alpha.jpg",
       width = 7, height = 4)

alpha.pooled <-  data_frame(variable = as.character(names(nameslist)))

for(i in 1:nrow(alpha.pooled)){
        temp_vars <- unlist(nameslist[as.character(alpha.pooled$variable[i])])
        alpha.pooled$alpha[i] <- psych::alpha(dplyr::select(df, all_of(temp_vars)),
                                             warnings=FALSE)$total$raw_alpha
}


# Demographics ------------------------------------------------------------

library(patchwork)
library(fastDummies)

# calculate sample size in total and balanced panel for each country
country_n <- df_full %>% 
        group_by(country) %>% 
        summarise(fulltotal = n(), 
                  balancedtotal = sum(dropped == 0))


# type in population benchmarks for all 4 countries
pop.prop <- tribble(
        ~country, ~levels, ~pop.prop, 
        "Denmark", "Primary", .268,
        "Denmark", "Secondary",  .411,
        "Denmark", "Tertiary", .321,
        "Denmark", "Male",  .494,
        "Denmark", "Female",.506, 
        "Denmark", "Capital Region",.315, 
        "Denmark", "Zealand",.145, 
        "Denmark", "Southern", .211, 
        "Denmark", "Central",.225, 
        "Denmark", "North", .103, 
        "Denmark", "18-34",.270,
        "Denmark", "35-54", .337,
        "Denmark", "55-69", .226,
        "Denmark", "70+", .166,
        
        "Hungary", "Male",.483,
        "Hungary", "Female",.517,
        "Hungary", "18-34",.293,
        "Hungary", "35-54",.384,
        "Hungary", "55+",.323,
        "Hungary", "Primary",.208,
        "Hungary", "Secondary",.573,
        "Hungary", "Tertiary",.219,
        "Hungary", "Northern",.114,
        "Hungary", "Northern Great Plain",.146,
        "Hungary", "Southern Great Plain",.127,
        "Hungary", "Pest",.129,
        "Hungary", "Budapest",.182,
        "Hungary", "Central Transdanubia",.109,
        "Hungary", "Western Transdanubia",.102,
        "Hungary", "Southern Transdanubia",.090,
        
        "Italy","Male",.476,
        "Italy","Female",.524,
        "Italy","18-24", .081,
        "Italy","25-34", .134,
        "Italy","35-44", .172,
        "Italy","45-54", .195,
        "Italy","55+",.418,
        "Italy","Primary",.433,
        "Italy","Secondary",.403,
        "Italy","Tertiary",.165,
        "Italy","Northwest",.265,
        "Italy","Northeast", .192,
        "Italy","Centre",.199,
        "Italy","South",.233,
        "Italy","Islands",.111,
        
        "USA","Male",.486,
        "USA","Female",.514,
        "USA","18-29", .220,
        "USA","30-44", .261,
        "USA","45-64", .347,
        "USA","65+", .172,
        "USA","Primary",.116,
        "USA","Secondary",.458,
        "USA","Tertiary",.426,
        "USA","Northeast", .169,
        "USA","Midwest", .211,
        "USA","South",.380,
        "USA","West",.240
)

# this function calculates the proportion of respondents across demographic groups
#       (factor vars) for each country and in each dataframe (balanced / unbalanced;
#       weighted/unweighted) panel
demog <- function(var){
        df_full %>% 
                group_by(country,levels = !!var) %>% 
                summarise(fulln = n(), 
                  balancedn = sum(dropped == 0),
                  weighted.n = sum(weight), 
                  balancedweighted.n = sum(FE_weight, na.rm = T)) %>% 
                left_join(country_n) %>% 
                mutate(full.prop = fulln / fulltotal,
                       weighted.prop = weighted.n / fulltotal,
                       balanced.prop = balancedn / balancedtotal, 
                       balancedweighted.prop = balancedweighted.n / balancedtotal,
                       var = rlang::as_name(var),
                       levels = as_factor(levels))
}

# calculate props for all demographic vars and all countries
dem_all <- rbind(
        demog(quo(education)),
        demog(quo(male)),
        demog(quo(region)),
        demog(quo(age_f))) %>% 
        # and combine everything into neatly ordered df
        mutate(var = fct_recode(var, 
                                Education = "education", 
                                Age = "age_f", 
                                Region = "region", 
                                Sex = "male"),
               levels = case_when(var == "Education" & levels == 1 ~ "Primary",
                                  var == "Education" & levels == 2 ~ "Secondary",
                                  var == "Education" & levels == 3 ~ "Tertiary",
                                  var == "Sex" & levels == 0 ~ "Female",
                                  var == "Sex" & levels == 1 ~ "Male",
                                  TRUE ~ as.character(levels)),
               var = fct_relevel(var, "Sex", "Education")) %>% 
        # merge with census benchmarks
        left_join(pop.prop) %>% 
        glimpse

# make sure all proportions for all dem variable add up to 4 (n of countries)
# dem_all %>% group_by(var) %>% summarise(pop = sum(pop.prop),
#                                         full.sample = sum(full.prop), 
#                                         balanced.sample = sum(balanced.prop), 
#                                         full.weighted = sum(weighted.prop), 
#                                         balanced.weighed = sum(balancedweighted.prop))


# Plot everything 
# We need to do this country by country and then patchwork them together
dem_all %>% 
        filter(country == "Hungary") %>% 
ggplot(., aes(x = levels)) +
        geom_point(aes(y = pop.prop), shape = 4, size = 3) +
        geom_point(aes(y = full.prop),  
                   position = position_nudge(x = -0.1),
                   color = "red") +
        geom_point(aes(y = weighted.prop), 
                   position = position_nudge(x = -0.1),
                   color = "darkgreen") +
        geom_point(aes(y = balanced.prop),  
                   shape = 2,
                   position = position_nudge(x = 0.1),
                   color = "red") +
        geom_point(aes(y = balancedweighted.prop), 
                   shape = 2,
                   position = position_nudge(x = 0.1),
                   color = "darkgreen") +
        scale_y_continuous(labels = scales::percent,
                           limits = c(0, .65)) +
        coord_flip() +
        ggtitle("Hungary") + 
        ggforce::facet_col(var~., scales = "free_y", space = "free") + 
        xlab("") + ylab("") +
        theme_bw()  |
        
dem_all %>% 
        filter(country == "Denmark") %>% 
ggplot(., aes(x = levels)) +
        geom_point(aes(y = pop.prop), shape = 4, size = 3) +
        geom_point(aes(y = full.prop),  
                   position = position_nudge(x = -0.1),
                   color = "red") +
        geom_point(aes(y = weighted.prop), 
                   position = position_nudge(x = -0.1),
                   color = "darkgreen") +
        geom_point(aes(y = balanced.prop),  
                   shape = 2,
                   position = position_nudge(x = 0.1),
                   color = "red") +
        geom_point(aes(y = balancedweighted.prop), 
                   shape = 2,
                   position = position_nudge(x = 0.1),
                   color = "darkgreen") +
        scale_y_continuous(labels = scales::percent,
                           limits = c(0, .65)) +
        ggtitle("Denmark") + 
        coord_flip() +
        ggforce::facet_col(var~., scales = "free_y", space = "free") + 
        xlab("") + ylab("") +
        theme_bw() |
        
dem_all %>% 
        filter(country == "Italy") %>% 
ggplot(., aes(x = levels)) +
        geom_point(aes(y = pop.prop), shape = 4, size = 3) +
        geom_point(aes(y = full.prop),  
                   position = position_nudge(x = -0.1),
                   color = "red") +
        geom_point(aes(y = weighted.prop), 
                   position = position_nudge(x = -0.1),
                   color = "darkgreen") +
        geom_point(aes(y = balanced.prop),  
                   shape = 2,
                   position = position_nudge(x = 0.1),
                   color = "red") +
        geom_point(aes(y = balancedweighted.prop), 
                   shape = 2,
                   position = position_nudge(x = 0.1),
                   color = "darkgreen") +
        scale_y_continuous(labels = scales::percent,
                           limits = c(0, .65)) +
        ggtitle("Italy") + 
        coord_flip() +
        ggforce::facet_col(var~., scales = "free_y", space = "free") + 
        xlab("") + ylab("") +
        theme_bw() | 
        
dem_all %>% 
        filter(country == "USA") %>% 
ggplot(., aes(x = levels)) +
        geom_point(aes(y = pop.prop), shape = 4, size = 3) +
        geom_point(aes(y = full.prop),  
                   position = position_nudge(x = -0.1),
                   color = "red") +
        geom_point(aes(y = weighted.prop), 
                   position = position_nudge(x = -0.1),
                   color = "darkgreen") +
        geom_point(aes(y = balanced.prop),  
                   shape = 2,
                   position = position_nudge(x = 0.1),
                   color = "red") +
        geom_point(aes(y = balancedweighted.prop), 
                   shape = 2,
                   position = position_nudge(x = 0.1),
                   color = "darkgreen") +
        scale_y_continuous(labels = scales::percent,
                           limits = c(0, .65)) +
        ggtitle("USA") + 
        coord_flip() +
        ggforce::facet_col(var~., scales = "free_y", space = "free") + 
        xlab("") + ylab("") +
        theme_bw()

# This plot is reported as Figure OA1 in Section B2 of the OA
ggsave("D-documents/figures/demographics.jpg", width =11, height =7)


# Sample descriptives ----------------------------------------------------

# These two tables are reported as Tables OA2 and OA3 in Section B3 of the OA
df_full %>% 
        filter(country %in% c("Denmark", "Hungary")) %>% 
        group_by(countrywave = paste(country, wave, sep = "_")) %>% 
        summarise(across(all_of(c("cbs.01", "anomie.01", 
                                  soc_dvs, system_dvs, extreme_dvs, bonus_dvs)), 
                         mean.sd)) %>% 
        pivot_longer(-countrywave, names_to = "dv") %>% 
        mutate(dv = gsub(".01", "", dv)) %>% 
        pivot_wider(names_from = countrywave) %>% 
        left_join(select(var_lookup, -bench_variables)) %>% 
        dplyr::select(label, contains("_"), -dv) %>% 
        stargazer(type = "text", summary= F, rownames = F, 
                  out = "D-documents/tables/sample_descr_DENHUN.tex")


df %>% 
        filter(country %in% c("Italy", "USA")) %>% 
        group_by(countrywave = paste(country, wave, sep = "_")) %>% 
        summarise(across(all_of(c("cbs.01", "anomie.01", 
                                  soc_dvs, system_dvs, extreme_dvs, bonus_dvs)), 
                         mean.sd)) %>% 
        pivot_longer(-countrywave, names_to = "dv") %>%
        mutate(dv = gsub(".01", "", dv)) %>% 
        pivot_wider(names_from = countrywave) %>% 
        left_join(select(var_lookup, -bench_variables)) %>% 
        dplyr::select(label, contains("_"), -dv) %>% 
        stargazer(type = "text", summary= F, rownames = F, 
                  out = "D-documents/tables/sample_descr_ITAUSA.tex")



# Replicate AIS and RIS ---------------------------------------------------
# COVID-19 Burden, Anomie and Antigovernment Behavior Intentions -- 
#       Replicating and extending Bartusevicius et al 2021.

my.fe(filter(df, wave >= 2), DV = "ais", IV = "cbs")
my.fe(filter(df, wave >= 2), DV = "ris", IV = "cbs")
my.fe(filter(df, wave >= 2), DV = "ais", IV = "anomie")
my.fe(filter(df, wave >= 2), DV = "ris", IV = "anomie")


ais1 <- felm(ais ~ cbs | ID + country_wave,
             weights = df$FE_weight[df$wave >= 2],
             data =filter(df, wave >= 2))

ais2 <- felm(ais ~ anomie | ID + country_wave,
             weights = df$FE_weight[df$wave >= 2],
             data =filter(df, wave >= 2))

ris1 <- felm(ris ~ cbs | ID + country_wave,
             weights = df$FE_weight[df$wave >= 2],
             data =filter(df, wave >= 2))

ris2 <- felm(ris ~ anomie | ID + country_wave, 
             weights = df$FE_weight[df$wave >= 2],
             data =filter(df, wave >= 2))


# This table is reported as Table OA4 in Section C7 of the OA
stargazer(ais1, ais2, ris1, ris2, 
          dep.var.labels = c("Activism Intentions", 
                             "Radicalism Intentions"),
          covariate.labels = c("Covid-19 Burden", "Anomie"),
          title = "2FE models of Activism and Radicalism Intentions",
          label = "tab:aisris",
          out="D-documents/tables/aisris.tex",
          type= "text")
