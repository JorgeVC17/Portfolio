library(rio)
library(tidyverse)
library(lubridate)
library(Hmisc)
library(patchwork)
library(weights)
library(stargazer)

# a function to calculate quantile for a specific CI width
myquantile <- function(ci){
         alpha = (100-ci)/2/100
         qnorm(1-alpha)
}


# custom functions for calculating weighted mean and SE which work inside functions 
mymean <- function(x,w) {wtd.mean(x = x, weights = w, na.rm  = T)}
myse <- function(x,w) {sqrt(wtd.var(x, weights = w, na.rm = T))/sqrt(length(x))}


# Import data -------------------------------------------------------------

df_full <- readRDS("B-analysis-data/yougov_clean_202204122.rds")
var_lookup <- readRDS("B-analysis-data/varlookup.rds")



# Figure 1. Benchmarked changes in Solidarity and System support ------------------------------
# ~ Crunch Systemic variables  -----------------------------------------------------

system_dvs <- c("system_satisfaction.z", "level_democracy.z", "supp_demcy.z",
                "proudcitizen.z")

# 4*3 country-wave means for individual outcomes
system_4c <- df_full %>% 
        # subset to relevant vars for system support
        dplyr::select(country, weight, wavedate, all_of(system_dvs)) %>% 
        # calculate weighted mean by country-waves
        pivot_longer(all_of(system_dvs), names_to = "dv") %>% 
        group_by(country, wavedate, dv) %>% 
        summarise(mean = mymean(value, weight), 
                  se = myse(value, weight)) %>% glimpse

# reported in the text:
system_4c %>% 
        group_by(country) %>% 
        summarise(sum(mean > 0))

# reported in the text:
system_4c %>% 
        filter(country %in% c("Denmark")) %>% 
        filter(dv %in% c("level_democracy.z") & 
                       wavedate == ymd("2020-12-12")) 

# country trends pooling across four outcomes
system_countrypool <-  df_full %>% 
        dplyr::select(country, weight, wavedate, all_of(system_dvs)) %>% 
        # calculate weighted mean by country-waves
        pivot_longer(all_of(system_dvs)) %>% 
        group_by(country, wavedate) %>% 
        summarise(mean = mymean(value, weight),
                  se = myse(value, weight)) %>% 
        mutate(dv = "Pooled") %>% 
        glimpse

# outcome trends pooling across the four countries
system_varpool <- df_full %>% 
        # subset to relevant vars for system support
        dplyr::select(country, weight, wavedate, all_of(system_dvs)) %>% 
        # calculate weighted mean by country-waves
        pivot_longer(all_of(system_dvs), names_to = "dv") %>% 
        group_by(wavedate, dv) %>% 
        summarise(mean = mymean(value, weight), 
                  se = myse(value, weight)) %>% 
        mutate(country = "Pooled") %>% 
        glimpse

# pool everything. The trend in system support across countries and vars.
system_pooled <- df_full %>% 
        dplyr::select(wavedate, weight, all_of(system_dvs)) %>% 
        pivot_longer(all_of(system_dvs)) %>% 
        group_by(wavedate) %>%
        # group_by(wave) %>% 
        summarise(mean = mymean(value, weight), 
                  se = myse(value, weight)) %>% 
        mutate(country = "Pooled", 
               dv = "Pooled") %>% 
        glimpse

# reported in the text: 
system_pooled %>% 
        mutate(mean = round(mean, 2))

# combine things into a single DF
system_plotdata <- bind_rows(system_4c, system_varpool) %>% 
        bind_rows(system_countrypool, system_pooled) %>% 
        ungroup() %>% 
        left_join(var_lookup) %>% 
        mutate(dv = fct_relevel(dv, "Pooled"), 
               country = fct_relevel(country, "Pooled"))

# ~ Draw system plot ---------------------------------------------------------
# ~ Panel A of Figure 1

system_facet <- ggplot(system_plotdata, aes( x =  wavedate, y = mean, color = dv)) +
        geom_rect(data = filter(system_plotdata, country == "Pooled"), 
                  fill = NA, color = "black",
                  xmin = -Inf, xmax = Inf, 
                  ymin = -Inf, ymax = Inf) + 
        geom_hline(aes(yintercept = 0)) + 
        geom_point(size = 1, alpha = .5) +
        geom_line(aes(group = dv), alpha = .7) +
        # add extra width to pooled lines
        geom_line(data = filter(system_plotdata,dv == "Pooled"), size = 1) + 
        # add errorbars only to pooled estimates
        geom_errorbar(data = filter(system_plotdata,dv == "Pooled"),
                      aes(ymin = mean - myquantile(95)*se, 
                          ymax = mean + myquantile(95)*se),
                      size = .6, width = 0) +
        xlab("") + ylab("") + 
        scale_color_manual(name = NULL,
                           values = c("#000000", viridisLite::viridis(4)),
                              labels = c("Pooled", 
                                         "\nLevel of \nDemocracy",
                                         "Proud Citizen",
                                         "\nSupport for \nDemocracy",
                                         "\nSatisf. with \nPol. System")
                              ) +
        scale_x_date(breaks = ymd(c("2020-04-20","2020-06-15","2020-12-12")), 
                     limits = ymd(c("2020-04-01", "2020-12-31")),
                     date_labels = "%b") +  
        ggtitle("System Support") + 
        facet_grid(~country) + 
        scale_y_continuous(limits = c(-1, .2)) + 
        theme_minimal() +
        theme(panel.grid.minor = element_blank())

system_facet


# ~ Crunch Social variables --------------------------------------------------------

soc_dvs <- c("solidarity.z","tolerate_immigrants.z", 
             "soctrust.z", "no_surveillance.z")

# 4*3 country-wave means for individual outcomes
soc_4c <- df_full %>% 
        # subset to relevant vars for soc support
        dplyr::select(country, weight, wavedate, all_of(soc_dvs)) %>% 
        # calculate weighted mean by country-waves
        pivot_longer(all_of(soc_dvs), names_to = "dv") %>% 
        group_by(country, wavedate, dv) %>% 
        summarise(mean = mymean(value, weight), 
                  se = myse(value, weight)) %>% glimpse

# country trends pooling across four outcomes
soc_countrypool <-  df_full %>% 
        dplyr::select(country, weight, wavedate, all_of(soc_dvs)) %>% 
        # calculate weighted mean by country-waves
        pivot_longer(all_of(soc_dvs)) %>% 
        group_by(country, wavedate) %>% 
        summarise(mean = mymean(value, weight),
                  se = myse(value, weight)) %>% 
        mutate(dv = "Pooled") %>% 
        glimpse

# outcome trends pooling across the four countries
soc_varpool <- df_full %>% 
        # subset to relevant vars for soc support
        dplyr::select(country, weight, wavedate, all_of(soc_dvs)) %>% 
        # calculate weighted mean by country-waves
        pivot_longer(all_of(soc_dvs), names_to = "dv") %>% 
        group_by(wavedate, dv) %>% 
        summarise(mean = mymean(value, weight), 
                  se = myse(value, weight)) %>% 
        mutate(country = "Pooled") %>% 
        glimpse

# pool everything. The trend in soc support across countries and vars.
soc_pooled <- df_full %>% 
        dplyr::select(wavedate, weight, all_of(soc_dvs)) %>% 
        pivot_longer(all_of(soc_dvs)) %>% 
        group_by(wavedate) %>%
        # group_by(wave) %>% 
        summarise(mean = mymean(value, weight), 
                  se = myse(value, weight)) %>% 
        mutate(country = "Pooled", 
               dv = "Pooled") %>% 
        glimpse

# combine everything into a single DF
soc_plotdata <- bind_rows(soc_4c, soc_varpool, soc_countrypool, soc_pooled) %>% 
        ungroup() %>% 
        left_join(var_lookup) %>% 
        mutate(dv = fct_relevel(dv, "Pooled"), 
               country = fct_relevel(country, "Pooled"))


# reported in the text:
# decreased tolerance for immigrants in US and DK
soc_4c %>% 
        filter(country %in% c("Denmark", "USA")) %>% 
        filter(dv %in% "tolerate_immigrants.z") %>% 
        mutate(mean = round(mean, 2))

# and redistribution in the US
soc_4c %>% 
        filter(country == "USA" & dv == "solidarity.z") %>% 
        mutate(mean = round(mean, 2))

# Overall country level changes
soc_countrypool %>% 
        mutate(mean = round(mean, 2))

# ~ Draw socplot ---------------------------------------------------------
# ~ Panel B of Figure 1
soc_facet <- ggplot(soc_plotdata, aes( x =  wavedate, y = mean, color = dv)) +
        geom_rect(data = filter(soc_plotdata, country == "Pooled"), 
                  fill = NA, color = "black",
                  # fill = "mistyrose", alpha = .02, color = NA,
                  xmin = -Inf, xmax = Inf, 
                  ymin = -Inf, ymax = Inf) + 
        geom_hline(aes(yintercept = 0)) + 
        geom_point(size = 1, alpha = .5) + 
        geom_line(aes(group = dv), alpha = .7) +
        # add extra width to pooled lines
        geom_line(data = filter(soc_plotdata,dv == "Pooled"), size = 1) + 
        # add errorbars only to pooled estimates
        geom_errorbar(data = filter(soc_plotdata,dv == "Pooled"),
                      aes(ymin = mean - myquantile(95)*se, 
                          ymax = mean + myquantile(95)*se),
                      size = .6, width = 0) +
        facet_grid(~country) + 
        xlab("") + ylab("") + 
        scale_color_manual(name = NULL,
                           values = c("#000000", 
                                      viridisLite::viridis(4, direction = -1)),
                           labels = c("Pooled",
                                      "\nReject \nSurveillance",
                                      "Social Trust",
                                      "\nSupport \nRedistribution",
                                      "\nTolerate \nImmigrants")
        ) +
        scale_x_date(breaks = ymd(c("2020-04-20","2020-06-15","2020-12-12")), 
                     limits = ymd(c("2020-04-01", "2020-12-31")),
                     date_labels = "%b") +  
        scale_y_continuous(limits = c(-.6, .6), 
                           breaks = seq(-.5, .5, by = .25)) +
        ggtitle("Social Solidarity") + 
        theme_minimal() +
        theme(panel.grid.minor = element_blank())

soc_facet


# ~ Combine Systemic and Social vars --------------------------------------
# Reported as Figure 1 in the main text 

soc_facet / system_facet + plot_annotation(tag_levels = 'A')
ggsave(file= "D-documents/figures/longtrend_benchmarked_20211216.pdf", 
       height = 9, width = 8.4)


# Extreme discontent ------------------------------------------------------

extreme_dvs <- c("nfc",  "misinfo", "populism")

# 4*3 country-wave means for individual outcomes
extreme_4c <- df_full %>% 
        # subset to relevant vars for extreme support
        dplyr::select(country, weight, wavedate, all_of(extreme_dvs)) %>% 
        # calculate weighted mean by country-waves
        pivot_longer(all_of(extreme_dvs), names_to = "dv") %>% 
        group_by(country, wavedate, dv) %>% 
        summarise(mean = mymean(value, weight), 
                  se = myse(value, weight)) %>% glimpse

# country trends pooling across four outcomes
extreme_countrypool <-  df_full %>% 
        dplyr::select(country, weight, wavedate, all_of(extreme_dvs)) %>% 
        # calculate weighted mean by country-waves
        pivot_longer(all_of(extreme_dvs)) %>% 
        group_by(country, wavedate) %>% 
        summarise(mean = mymean(value, weight),
                  se = myse(value, weight)) %>% 
        mutate(dv = "Pooled") %>% 
        glimpse

# outcome trends pooling across the four countries
extreme_varpool <- df_full %>% 
        # subset to relevant vars for extreme support
        dplyr::select(country, weight, wavedate, all_of(extreme_dvs)) %>% 
        # calculate weighted mean by country-waves
        pivot_longer(all_of(extreme_dvs), names_to = "dv") %>% 
        group_by(wavedate, dv) %>% 
        summarise(mean = mymean(value, weight), 
                  se = myse(value, weight)) %>% 
        mutate(country = "Pooled") %>% 
        glimpse

# pool everything. The trend in extreme support across countries and vars.
extreme_pooled <- df_full %>% 
        dplyr::select(wavedate, weight, all_of(extreme_dvs)) %>% 
        pivot_longer(all_of(extreme_dvs)) %>% 
        group_by(wavedate) %>%
        summarise(mean = mymean(value, weight), 
                  se = myse(value, weight)) %>% 
        mutate(country = "Pooled", 
               dv = "Pooled") %>% 
        glimpse

# combine everything into single DF
extreme_plotdata <- bind_rows(extreme_4c, extreme_varpool, extreme_countrypool, 
                              extreme_pooled) %>% 
        ungroup() %>% 
        left_join(var_lookup) %>% 
        # tweak factors so they are ordered nicely on the plot
        mutate(dv = fct_relevel(dv, "Pooled"), 
               country = fct_relevel(country, "Pooled"))

# ~ Draw extreme plot ---------------------------------------------------------

extreme_facet <- ggplot(extreme_plotdata, aes( x =  wavedate, y = mean, color = dv)) +
        geom_rect(data = filter(extreme_plotdata, country == "Pooled"), 
                  fill = NA, color = "black",
                  # fill = "mistyrose", alpha = .02, color = NA,
                  xmin = -Inf, xmax = Inf, 
                  ymin = -Inf, ymax = Inf) + 
        geom_point(size = 1, alpha = .5) + 
        geom_line(aes(group = dv), alpha = .5) +
        geom_errorbar(data = filter(extreme_plotdata,dv == "Pooled"),
                      size = .6,
                      aes(ymin = mean - 2*se,
                          ymax = mean + 2*se),
                      width = 0) +
        # add extra width to pooled lines
        geom_line(data = filter(extreme_plotdata,dv == "Pooled"), size = 1) + 
        facet_grid(~country) + 
        xlab("") + ylab("") + 
        scale_color_manual(name = NULL,
                           values = c("#000000",
                                      viridisLite::viridis(3)),
                           labels = c("Pooled",
                                      "Believe, Share Misinfo",
                                      "Need for Chaos",
                                      "Populism")) +
        scale_x_date(breaks = ymd(c("2020-04-20","2020-06-15","2020-12-12")), 
                     limits = ymd(c("2020-04-01", "2020-12-31")),
                     date_labels = "%b") +  
        ggtitle("Extreme Discontent") + 
        theme_minimal() +
        theme(
                # plot.title = element_text(hjust = 0.5), 
                legend.position = "bottom") 

extreme_facet

# Reported as Figure OA6.
ggsave("D-documents/figures/exteme_trend_20211210.pdf", width = 8)




# Fig 1 with CI-s ---------------------------------------------------------

ggplot(system_plotdata, aes( x =  wavedate, y = mean, color = dv)) +
        # geom_rect(data = filter(system_plotdata, country == "Pooled"), 
        #           fill = NA, color = "black",
        #           xmin = -Inf, xmax = Inf, 
        #           ymin = -Inf, ymax = Inf) + 
        geom_hline(aes(yintercept = 0)) + 
        geom_point(size = 1, alpha = .5) +
        geom_line(aes(group = dv), alpha = .7) +
        # add extra width to pooled lines
        # geom_line(data = filter(system_plotdata,dv == "Pooled"), size = 1) + 
        # add errorbars only to pooled estimates
        geom_errorbar(aes(ymin = mean - myquantile(95)*se, 
                          ymax = mean + myquantile(95)*se),
                      size = .6, width = 0) +
        xlab("") + ylab("") + 
        scale_color_manual(name = NULL,
                           values = c("#000000", viridisLite::viridis(4)),
                           labels = c("Pooled", 
                                      "\nLevel of \nDemocracy",
                                      "Proud Citizen",
                                      "\nSupport for \nDemocracy",
                                      "\nSatisf. with \nPol. System")
        ) +
        scale_x_date(breaks = ymd(c("2020-04-20","2020-06-15","2020-12-12")), 
                     limits = ymd(c("2020-04-01", "2020-12-31")),
                     date_labels = "%b") +  
        ggtitle("System Support with CI-s") + 
        facet_grid(dv~country, scales = "free") + 
        theme_minimal() +
        theme(panel.grid.minor = element_blank(), 
              strip.text.y = element_blank()
        )

ggsave("D-documents/figures/fig1_syssupp_CI_s.pdf")


ggplot(soc_plotdata, aes( x =  wavedate, y = mean, color = dv)) +
        geom_hline(aes(yintercept = 0)) + 
        geom_point(size = 1, alpha = .5) + 
        geom_line(aes(group = dv), alpha = .7) +
        # add extra width to pooled lines
        # geom_line(data = filter(soc_plotdata,dv == "Pooled"), size = 1) + 
        # add errorbars only to pooled estimates
        geom_errorbar(aes(ymin = mean - myquantile(95)*se, 
                          ymax = mean + myquantile(95)*se),
                      size = .6, width = 0) +
        facet_grid(dv~country, scales = "free") + 
        xlab("") + ylab("") + 
        scale_color_manual(name = NULL,
                           values = c("#000000", 
                                      viridisLite::viridis(4, direction = -1)),
                           labels = c("Pooled",
                                      "\nReject \nSurveillance",
                                      "Social Trust",
                                      "\nSupport \nRedistribution",
                                      "\nTolerate \nImmigrants")
        ) +
        scale_x_date(breaks = ymd(c("2020-04-20","2020-06-15","2020-12-12")), 
                     limits = ymd(c("2020-04-01", "2020-12-31")),
                     date_labels = "%b") +  
        # scale_y_continuous(limits = c(-.6, .6), 
        #                    breaks = seq(-.5, .5, by = .25)) +
        ggtitle("Social Solidarity") + 
        theme_minimal() +
        theme(panel.grid.minor = element_blank(), 
              strip.text.y = element_blank())

ggsave("D-documents/figures/fig1_solidar_CI_s.pdf")



# Regressions on significance ---------------------------------------------

df_plot <- df_full %>% 
        mutate(sys_avg = rowMeans(select(., all_of(system_dvs)), na.rm = T), 
               sol_avg = rowMeans(select(., all_of(soc_dvs)), na.rm = T), 
               wave_ = as.factor(wave))

fit11 <- lm(system_satisfaction.z ~ wave_, df_plot)
fit12 <- lm(level_democracy.z ~ wave_, df_plot)
fit13 <- lm(supp_demcy.z ~ wave_, df_plot)
fit14 <- lm(proudcitizen.z ~ wave_, df_plot)

fit21 <- lm(sys_avg ~ wave_, df_plot, 
            subset = df_plot$country == "Denmark")

fit22 <- lm(sys_avg ~ wave_, df_plot, 
            subset = df_plot$country == "Hungary")

fit23 <- lm(sys_avg ~ wave_, df_plot, 
            subset = df_plot$country == "Italy")

fit24 <- lm(sys_avg ~ wave_, df_plot, 
            subset = df_plot$country == "USA")

stargazer(fit11, fit12, fit13, fit14, 
          type = "text", order = c("Constant"), 
          title = "Time trends system support variables pooling across countries",
          label = "tab:reg_sys_1",
          digits = 2, keep.stat = c("n" , "adj.rsq"),
          dep.var.labels = c("Sys.stfctn", 
                             "Level.of.demcy", 
                             "Supp.demcy", 
                             "Proud.ctzn"),
          out= "D-documents/tables/reg_sys_1.tex",
          star.cutoffs = c(0.05, 0.01, 0.001))


stargazer(fit21, fit22, fit23, fit24, 
          type = "text", order = c("Constant"), 
          digits = 2, keep.stat = c("n" , "adj.rsq"),
          title = "Time trends in pooled system support across countries",
          label = "tab:reg_sys_2",
          dep.var.labels = "Pooled System Support", 
          column.labels =  c("Denmark", 
                             "Hungary", 
                             "Italy", 
                             "USA"),
          # dep.var.caption = "DV = Pool System Support",
          out= "D-documents/tables/reg_sys_2.tex",
          star.cutoffs = c(0.05, 0.01, 0.001))


## ~ Solidarity

fit31 <- lm(solidarity.z ~ wave_, df_plot)
fit32 <- lm(tolerate_immigrants.z ~ wave_, df_plot)
fit33 <- lm(soctrust.z ~ wave_, df_plot)
fit34 <- lm(no_surveillance.z ~ wave_, df_plot)

fit41 <- lm(sol_avg ~ wave_, df_plot, 
            subset = df_plot$country == "Denmark")

fit42 <- lm(sol_avg ~ wave_, df_plot, 
            subset = df_plot$country == "Hungary")

fit43 <- lm(sol_avg ~ wave_, df_plot, 
            subset = df_plot$country == "Italy")

fit44 <- lm(sol_avg ~ wave_, df_plot, 
            subset = df_plot$country == "USA")

stargazer(fit31, fit32, fit33, fit34, 
          type = "text", order = c("Constant"), 
          digits = 2, keep.stat = c("n" , "adj.rsq"),
          title = "Time trends social solidarity variables pooling across countries",
          label = "tab:reg_sol_1",
          dep.var.labels = c("Soc.solid",
                             "Toler.immig",
                             "Soc.trust",
                             "No.Surveillance"),
          out= "D-documents/tables/reg_sol_1.tex",
          star.cutoffs = c(0.05, 0.01, 0.001))


stargazer(fit41, fit42, fit43, fit44, 
          type = "text", order = c("Constant"), 
          digits = 2, keep.stat = c("n" , "adj.rsq"),
          title = "Time trends in pooled social solidarity across countries",
          label = "tab:reg_sol_2",
          dep.var.labels = "Pooled Social Solidarity", 
          column.labels =  c("Denmark", 
                             "Hungary", 
                             "Italy", 
                             "USA"),
          # dep.var.caption = "DV = Pool System Support",
          out= "D-documents/tables/reg_sol_2.tex",
          star.cutoffs = c(0.05, 0.01, 0.001))


# No choice benchmarked ---------------------------------------------------

# follows the same basic procedure as above. 
# calculate means for each country-wave
nochoice_countrypool <-  df_full %>% 
        dplyr::select(country, weight, wavedate, nochoice) %>% 
        # calculate weighted mean by country-waves
        group_by(country, wavedate) %>% 
        summarise(mean = mymean(nochoice, weight),
                  se = myse(nochoice, weight)) %>% 
        glimpse


# calculate means for waves (pooling across countries)
nochoice_pool <-  df_full %>% 
        dplyr::select(weight, wavedate, nochoice) %>% 
        # calculate weighted mean by country-waves
        group_by(wavedate) %>% 
        summarise(mean = mymean(nochoice, weight),
                  se = myse(nochoice, weight)) %>% 
        mutate(country = "Pooled") %>% 
        glimpse

# combine averages into single DF
nochoice_plot <- bind_rows(nochoice_countrypool, nochoice_pool) %>% 
        ungroup() %>% 
        mutate(country = fct_relevel(country, "Pooled")) %>% 
         glimpse

# Draw plot
ggplot(nochoice_plot, aes( x =  wavedate, y = mean)) +
        geom_hline(aes(yintercept = 0)) + 
        geom_rect(data = filter(nochoice_plot, country == "Pooled"), 
                  fill = NA, color = "black",
                  # fill = "mistyrose", alpha = .02, color = NA,
                  xmin = -Inf, xmax = Inf, 
                  ymin = -Inf, ymax = Inf) + 
        geom_point(size = 1, alpha = .5) + 
        geom_line(alpha = .5) +
        geom_errorbar(size = .6,
                      aes(ymin = mean - 2*se,
                          ymax = mean + 2*se),
                      width = 0) +
        # add extra width to pooled lines
        facet_grid(~country) + 
        xlab("") + ylab("") + 
        scale_x_date(breaks = ymd(c("2020-04-20","2020-06-15","2020-12-12")), 
                     limits = ymd(c("2020-04-01", "2020-12-31")),
                     date_labels = "%b") +  
        ggtitle("No Choice and Control Over Life") + 
        theme_minimal() 

# Reported as Figure OA10. 
ggsave("D-documents/figures/nochoice.pdf")

#Fig 2. Stringency, Death and Burden---------------------------------------------

# ~ Stingency ------------------------------------
# import data
stringency_data <- import("B-analysis-data/stringency.rds")
# draw trends in stringency 
string <-  ggplot(stringency_data, aes(x= date, y = stringency)) +
        geom_line(aes(group = 1)) +
        facet_grid(~ country) + 
        xlab("") + ylab("Policy Stringency") +
        # scale_y_continuous(breaks = c(0, 100, 200)) +
        scale_x_date(breaks = ymd(c("2020-02-09","2020-04-20","2020-06-15","2020-12-12")),
                     limits = ymd(c("2020-02-01", "2020-12-31")),
                     labels = c("", "Apr", "Jun", "Dec")) +
        theme_minimal() + 
        theme(panel.spacing = unit(1.1, "lines"),
              strip.text.x = element_blank(),
              panel.grid.minor = element_blank(),
              plot.margin = margin(2, 2, 0, 2))
string


# ~ Death rate ------------------------------------------------------------
# import data
infection_data <- readRDS("B-analysis-data/infections.rds") %>% 
        mutate(country = fct_relevel(country, "Hungary", "Italy", "USA"))
# draw plot
inf <- ggplot(infection_data, aes(x= date, y = rate_14_day)) +
        geom_line(aes(group = 1)) + 
        facet_grid(~ country) + 
        xlab("") + ylab("14-day death rate") +
        scale_x_date(breaks = ymd(c("2020-02-09","2020-04-20","2020-06-15","2020-12-12")),
                     limits = ymd(c("2020-02-01", "2020-12-31")),
                     labels = c("", "Apr", "Jun", "Dec")) +
        theme_minimal() + 
        theme(panel.spacing = unit(1.1, "lines"),
              panel.grid.minor = element_blank(),
              plot.margin = margin(2, 2, 0, 2))
inf


# ~ Trends in Independent vars --------------------------------------------
# aggregate to country waves
countrywave_trend <- df_full %>% 
        group_by(country, wavedate) %>% 
        summarise(across(c("anomie.01", "cbs.01", "nochoice"), 
                         .fns = list(mean = ~mymean(.x, FE_weight),
                                     se = ~myse(.x, FE_weight)))) %>% 
        pivot_longer(anomie.01_mean : nochoice_se) %>% 
        separate(name, into = c("variable", "stat"), sep = "_") %>% 
        pivot_wider(names_from = stat, values_from = value) %>% 
        ungroup() %>% 
        mutate(country = fct_relevel(country, "Hungary", "Italy", "USA")) %>%
        glimpse



# calculate baseline for no choice 
# this lone IV has been included in pre-pandemic benchmark surveys
evs_bench <- import("B-analysis-data/evs_aggregates.csv")
wvs_bench <- import("B-analysis-data/wvs_aggregates.csv") 
nochoice_bench <- bind_rows(evs_bench, wvs_bench) %>%  
        filter(variables == "Q8B") %>% 
        select(country, mean, standard_error) %>% 
        #we turn 1-10 scale to a flipped 0-1.
        transmute(country = country,
                mean = (((mean-1)/9)-1)*-1, 
               se = standard_error/9, 
               wavedate = ymd("2020-02-09"), 
               variable = "nochoice") %>% 
        glimpse()

# combine everything into single DF 
countrywave_data <- bind_rows(countrywave_trend, nochoice_bench) %>% 
        ungroup() %>% 
        # end tweak things for plotting
        mutate(variable = fct_relevel(variable, "cbs.01"),
               line = ifelse(wavedate != ymd("2020-02-09"), 1, 0), 
               dashed = ifelse(wavedate < ymd("2020-04-30"), 1, 0),
               country = fct_relevel(country, "Hungary", "Italy", "USA"))

# draw panel with IV trends
cw <- ggplot(countrywave_data, aes(x = wavedate, y = mean, color = variable)) +
        geom_point(position= position_dodge(25)) + 
        geom_errorbar(aes(ymin = mean - 2*se,
                          ymax = mean + 2*se),
                      width = 0, position= position_dodge(25)) + 
        geom_line(aes(group = interaction(line, variable)),
                  position= position_dodge(25)) + 
        geom_line(aes(group = interaction(dashed, variable)),
                  position= position_dodge(25), linetype = 2) +
        scale_color_viridis_d(name = NULL, direction = -1,
                              labels = c("Burden of COVID-19",
                                         "Anomie (long)", 
                                         "Anomie (short)"),
                              guide = guide_legend(reverse = F)) +
        scale_x_date(breaks = ymd(c("2020-02-09","2020-04-20","2020-06-15",
                                    "2020-12-12")),
                     limits = ymd(c("2020-02-01", "2020-12-31")),
                     labels = c("Pre", "Apr", "Jun", "Dec")) +
        facet_grid(~country) + 
        xlab("") + ylab("Mean attitudes") + 
        # ylim(.3, .6) +
        theme_minimal()+ 
        theme(legend.position = "bottom", 
              strip.background = element_blank(),
              strip.text.x = element_blank(),
              panel.spacing = unit(1, "lines"),
              panel.grid.minor = element_blank(),
              plot.margin = margin(0, 2, 2, 2)
        )
cw

# combine 3 panels
# This is reported as Figure 2 in the main text
inf/string/cw + plot_layout(heights = c(1, 1, 2))
ggsave("D-documents/figures/trends_20211213.jpg", height = 7, width = 7)

