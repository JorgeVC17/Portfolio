library(rio)
library(lubridate)
library(weights)
library(tidyverse)


# nifty function which handles the variable typical variable transformations
r.l <- function(x,
                na.val = 988, 
                flip = F, flip2 = F, zero.one = F,
                max = NULL, min = NULL){
        
        # replace na values with NA
        x <- ifelse(x %in% na.val, NA, x)
        
        if(is.null(max)){
                maximum <- max(x, na.rm = T)
        } else maximum <- max
        
        if(is.null(min)){
                minimum <- min(x, na.rm = T)
        }  else minimum <- min
        
        # this flips scales with arbitrary min and max values
        #       min will become max and vice versa
        if(flip == T)
                x <- (((x - maximum) * -1 ) + minimum)
        
        # for z-scored vars, we want 0 to remain 0, but flipping signs otherwise
        if(flip2 == T)
                x <- x * -1
        
        # this rescales a scale with arbitrary min&max to 0-1 range
        if(zero.one == T) {
                x <- (x - minimum) / (maximum - minimum)}
        x
        
} 



# create a lookup table with nice labels for all DVs
var_lookup <- tribble(
        ~dv, ~label, ~bench_variables,
        "level_democracy", "Level of Democracy", "Q26", 
        "system_satisfaction", "Satisfaction with pol. system", "Q25",
        "nfc", "Need for chaos", NA,
        "misinfo", "Believe and share misinfo", NA,
        "proudcitizen","Proud citizen", "Q23",
        "dishonesty_hum", "Low honesty-humility", NA,
        "supp_demcy", "Support for democracy", NA,
        "dem_nostrong", "Support. No strong leader", "Q28_1",
        "dem_experts", "Support. Not experts", "Q28_2",
        "dem_noarmy", "Support. No army", "Q28_3",
        "dem_democracy", "Support. Democracy", "Q28_4", 
        "populism", "Populism", NA,
        "no_surveillance", "Reject surveillance", NA,
        "no_surveillance1", "Surveillance. CCTV" ,"Q32_1",
        "no_surveillance2", "Surveillance. Online" ,"Q32_2",
        "no_surveillance3", "Surveillance. Information" ,"Q32_3", 
        "tolerate_immigrants", "Tolerate immigrants", NA,
        "immig_dontdiscriminate","Immigrants. Dont discriminate","Q16",
        "immig_goodimpact" ,"Immigrants. Good impact","Q17",
        "immig_donttakeawayjobs" ,"Immigrants. Dont take away jobs","Q19",
        "immig_dontmakecrimeworse" ,"Immigrants. Dont make crime worse","Q20",
        "immig_dontstrainwelfare" ,"Immigrants. Dont strain welfare","Q21",
        "immig_shouldntassimilate" ,"Immigrants. Need not assimilate","Q22",
        "soctrust", "Social trust", "Q18",
        "solidarity", "Support for redistribution", NA,
        "solidarity_stateresp", "Redistrib. State's responsibility", "Q13", 
        "solidarity_refusework", "Redistrib. Can refuse work", "Q14", 
        "solidarity_equalincome", "Redistrib. More equal income", "Q15", 
        "no_choice", "No choice", "Q8B",
        "cbs", "Subjective Burden of COVID-19", NA,
        "cbs_full", "Subjective Burden of COVID-19 \n(incl. Dem. factor)", NA,
        "anomie", "Anomie", NA,
        "sdrt", "Status Driven Risk Taking", NA,
        "tol_im_short", "Tolerating immigrants (short)", NA, 
        "gov", "Support for the Government", NA, 
        "polarized_abs", "Affective polarization", NA,
        "finance", "Financial prospects", NA,
        "hate_right", "Disliking the Right", NA, 
        "hate_left", "Disliking the Left", NA,
        "ais", "Activism Intention Scale", NA, 
        "ris", "Radicalism Intention Scale", NA,
        "Pooled", "Pooled",NA
)

saveRDS(var_lookup, "B-analysis-data/varlookup.rds")

# create a list with variable names group by index they belong to
#       this has to be run before indices are created. 
#       it's best commented out, to avoid confusion later.

nameslist <- list()
nameslist$finance <- c("fin_findjob", "fin_v_child", "fin_v_parent")
nameslist$cbs_full <- paste0("cbs", 1:10)
nameslist$cbs <- paste0("cbs", c(1:4, 7:10)) #exckude democratic concerns factor
nameslist$anomie <- paste0("anomie", 1:17)
nameslist$solidarity <- c("solidarity_stateresp", "solidarity_refusework",
                          "solidarity_equalincome")
nameslist$tolerate_immigrants <- c("immig_dontdiscriminate","immig_goodimpact" ,
                                   "immig_donttakeawayjobs","immig_dontmakecrimeworse" ,
                                   "immig_dontstrainwelfare","immig_shouldntassimilate")
nameslist$tol_im_short <- c("immig_dontdiscriminate","immig_goodimpact")

nameslist$gov <- c("gov_notharmsecon","gov_posecon", "gov_necessarypol",
                   "gov_nottooextreme","gov_united")
nameslist$nfc <- paste0("nfc", 1:8)
nameslist$hate_right <- paste0("hate_right", 1:6)
nameslist$hate_left <- paste0("hate_left", 1:6)
nameslist$no_surveillance <- paste0("no_surveillance", 1:3)
nameslist$populism <- paste0("populism", 1:6)
nameslist$supp_demcy <- c("dem_nostrong", "dem_noarmy", "dem_democracy")
nameslist$ais <- paste0("ais", 1:4)
nameslist$ris <- paste0("ris", 1:4)
saveRDS(nameslist, "B-analysis-data/nameslist.rds")
nameslist <- readRDS("B-analysis-data/nameslist.rds")


# import raw survey panel data ###################################################
rawraw <- import("A-original-data/2021-06-17_longsurvey_individual_level_both_weights.dta")

raw <- rawraw %>% 
        # minimal tweaks to avoid doing it multiple times
        rename(ID = caseid_W1, wave = wave_long) %>% # sensible varnames for ID&Wave
        # sensible labels for countries
        mutate(country = case_when(country == 1 ~ "USA", 
                                   country == 2 ~ "Denmark", 
                                   country == 3 ~ "Italy", 
                                   country == 4 ~ "Hungary")) %>% 
        # drop variables not used in the current paper 
        select(-contains("Q34"), -contains("Q35"), -contains("Q36"), 
               -contains("Q36"),
               -contains("Q38"), -contains("Q39"),
               -contains("Q40")) %>%
        # 988 is a general code for NA. replace them with NA
        mutate(across(where(is.double), ~na_if(., 988))) %>% 
        # replace values of Denmark wave 1 Q13, which were messed up due to a coding error
        mutate(Q13 = ifelse(country == "Denmark" & wave == 1,
                                             NA, 
                                             Q13), 
        # 3 questions on finance have custom Dontknow values. replace them w/ NA.
               Q3 = na_if(Q3, 4), 
               Q4 = na_if(Q4, 6), 
               Q5 = na_if(Q5, 6)) %>% 
        glimpse
        

# Z-score outcomes --------------------------------------------------------

# create a vector for names of benchmarked variable labels
bench_vars <- unique(var_lookup$bench_variables)[!is.na(unique(var_lookup$bench_variables))]
# and for non benchmarked var names too
non_bench_vars <- raw %>% 
        select(contains("Q", ignore.case = F)) %>% 
        select(-all_of(bench_vars), -contains("Q37")) %>% 
        names(.) 

# helpers with weighted mean and sd with na.rm = T 
mymean <- function(x,w) {wtd.mean(x = x, weights = w, na.rm  = T)}
myse <- function(x,w) {sqrt(wtd.var(x, weights = w, na.rm = T))/sqrt(length(x))}
mysd <- function(x,w) {sqrt(wtd.var(x, weights = w, na.rm = T))}


# estimate mean and SD for non-benchmarked vars
hope_bench <- raw %>% 
        # limit to wave 1.
        filter(wave == 1) %>% 
        # select all non-benchmarkd vars, and id and counctry
        select(all_of(non_bench_vars), ID, country, weight_global) %>% 
        group_by(country) %>% 
        summarise(across(c(-ID,-weight_global),
                         list(mean = ~mymean(., w = weight_global),
                              stdv = ~mysd(., w = weight_global)),
                         .names = "{col}.{fn}")) %>%  
        ungroup() %>% 
        pivot_longer(-country) %>% 
        separate(name, c("name", "stat"), sep = "\\.") %>%
        pivot_wider(names_from = stat) %>% 
        glimpse()


# import evs and wvs summary stats. these were preprocessed by an RA in STATA
evs_bench <- import("B-analysis-data/evs_aggregates.csv")
wvs_bench <- import("B-analysis-data/wvs_aggregates.csv") 

# combine evs and wvs descriptives and subset to focal vars.
baseline <- bind_rows(evs_bench, wvs_bench) %>%  
        rename(name = variables, 
               stdv = standard_deviation) %>% 
        filter(name %in% bench_vars) %>% 
        select(country, mean, stdv, name) %>% 
        # merge in benchmarks from own wave 1 survey
        bind_rows(hope_bench) %>%
        glimpse()


# Transform responses into z-scores 
zraw <- raw %>%
        # subset to focal vars + country and wave info and respondent ID
        select(all_of(c(bench_vars, non_bench_vars)), country, wave, ID) %>% 
        # now pivot to megalong df, with all variables in a single column
        pivot_longer(all_of(c(bench_vars, non_bench_vars)), 
                     names_to = "name", 
                     values_to = "response") %>% 
        # this allows us to attach baseline mean and SD for each var and respondent
        left_join(baseline) %>% 
        # we transform responses into z-scores
        mutate(response = (response - mean)/stdv) %>% 
        # remove mean and SD, we don't need them anymore, but they mess up wrangling
        select(-mean, -stdv) %>% 
        # and now pivot to wide form where each variable has its own column again
        pivot_wider(names_from = "name", values_from = "response") %>% 
        glimpse()

# let's double check that we didn't lose any respondents along the way.
#       raw data should have same N as z-scored data
stopifnot(nrow(raw) == nrow(zraw))

# let's skim through the non-benchmarked vars and make sure that in wave 1 they
        # have a SD close to 1
# zraw %>% 
#         filter(wave == 1) %>%  
#         select(all_of(non_bench_vars)) %>% 
#         summarise(across(where(is.double), 
#                          list(
#                                  sd= function(x)(round(sd(x, na.rm = T), 1)),
#                                  mean= function(x)(round(mean(x, na.rm = T), 1))
#                                  ))) %>% t()



# Tidy demographic and background variables -------------------------------

df_demographics <- raw %>% 
        transmute(country = country, 
                  wave = wave,
                  ID = ID,
                  ID_f = as.factor(ID),
                  country_wave = paste(country, wave, sep = "_"),
                  
                  date = lubridate::as_date(endtime),
                  
                  age = recode(as.character(profile_age5), 
                               `1` = 20L,  `2` = 25L, `3` = 30L, `4` = 35L, 
                               `5` = 40L,  `6` = 45L, `7` = 50L, `8` = 55L, 
                               `9` = 60L, `10` = 65L, `11` = 70L, `12` = 75L,
                               `13` = 80L, `14` = 86L, `15` = 91L),
                  
                  age_US = recode(weight_var_age_US, 
                                  `1` = "18-29", `2` = "30-44", 
                                  `3` = "45-64", `4` = "65+"),
                  age_DK = recode(weight_var_age_DK, 
                                  `1` = "18-34", `2` = "35-54", 
                                  `3` = "55-69", `4` = "70+"),
                  age_HU = recode(weight_var_age_HU, 
                                  `1` = "18-34", `2` = "35-54", 
                                  `3` = "55+"),
                  age_IT = recode(weight_var_age_IT, 
                                  `1` = "18-24", `2` = "25-34", 
                                  `3` = "35-44", `4` = "45-54", 
                                  `5` = "55+"),
                  
                  age_f = case_when(country == "USA" ~    as.character(age_US),
                                    country == "Italy" ~  as.character(age_IT),
                                    country == "Hungary" ~as.character(age_HU),
                                    country == "Denmark" ~as.character(age_DK),
                                    TRUE ~ NA_character_),
                  
                  education = as.factor(weight_var_education_rc),
                  male = weight_var_sex,
                  region_US = fct_recode(as.factor(weight_var_region_US), 
                                         Northeast = "1",
                                         Midwest = "2", 
                                         South = "3", 
                                         West = "4"),
                  region_DK = fct_recode(as.factor(weight_var_region_DK), 
                                         `Capital Region` = "1",
                                         Zealand = "2", 
                                         Southern = "3", 
                                         Central = "4", 
                                         North = "5"),
                  region_HU = fct_recode(as.factor(weight_var_region_HU), 
                                         Northern = "1",
                                         `Northern Great Plain` = "2", 
                                         `Southern Great Plain` = "3", 
                                         Pest = "4",
                                         Budapest = "5",
                                         `Central Transdanubia` = "6", 
                                         `Western Transdanubia` = "7", 
                                         `Southern Transdanubia` = "8"),
                  region_IT = fct_recode(as.factor(weight_var_region_IT), 
                                         Northwest = "1",
                                         Northeast = "2", 
                                         Centre = "3", 
                                         South = "4",
                                         Islands = "5"),
                  
                  region = case_when(country == "USA" ~    as.character(region_US),
                                     country == "Italy" ~  as.character(region_IT),
                                     country == "Hungary" ~as.character(region_HU),
                                     country == "Denmark" ~as.character(region_DK),
                                     TRUE ~ NA_character_),
                  
                  employed = ifelse(Q6 >= 1 & Q6 <= 3, 1, 0), 
                  
                  wavedate = ymd(case_when(wave == 1 ~ "2020-04-20",
                                           wave == 2 ~ "2020-06-15",
                                           wave == 3 ~ "2020-12-12")),
                 
                  weight = weight_global,
                  FE_weight = FE_weight_global, 
                  dropped = ifelse(is.na(FE_weight), 1, 0)) %>% 
        select(-contains("region_"), -age_US:-age_IT) %>% 
        group_by(country) %>%
        mutate(age_c = arm::rescale(age)) %>%
        ungroup() %>%
        glimpse()


        

# Tidy with 0-1 scales-----------------------------------------
df_01 <- raw %>% 
        transmute(ID = ID, # ID and Wave needed for merging
                wave = wave,
                
                my_health = r.l(Q1, flip = T, zero.one = T), 
                got_covid = ifelse(Q2 == 1, 1, 0), 
                fin_findjob = r.l(Q3, zero.one = T),
                fin_v_parent = r.l(Q4, flip = T, zero.one = T),
                fin_v_child = r.l(Q5, flip = T, zero.one = T), 
                
                cbs1 = r.l(Q7_1 , flip = F, zero.one = T), #health
                cbs2 = r.l(Q7_2 , flip = T, zero.one = T), 
                cbs3 = r.l(Q7_3 , flip = T, zero.one = T), #finance
                cbs4 = r.l(Q7_4 , flip = F, zero.one = T),
                cbs5 = r.l(Q7_5 , flip = T, zero.one = T), #democracy
                cbs6 = r.l(Q7_6 , flip = F, zero.one = T),
                cbs7 = r.l(Q7_7 , flip = T, zero.one = T), #social life
                cbs8 = r.l(Q7_8 , flip = F, zero.one = T),
                cbs9 = r.l(Q7_9 , flip = T, zero.one = T), #anomie 
                cbs10 = r.l(Q7_10, flip = F, zero.one = T),
                
                anomie1  = r.l(Q8_1 , flip = T, zero.one = T),
                anomie2  = r.l(Q8_2 , flip = T, zero.one = T),
                anomie3  = r.l(Q8_3 , flip = T, zero.one = T),
                anomie4  = r.l(Q8_4 , flip = T, zero.one = T),
                anomie5  = r.l(Q8_5 , flip = T, zero.one = T),
                anomie6  = r.l(Q8_6 , flip = T, zero.one = T),
                anomie7  = r.l(Q8_7 , flip = T, zero.one = T),
                anomie8  = r.l(Q8_8 , flip = F, zero.one = T), #our lonely reverse coded item
                anomie9  = r.l(Q8_9 , flip = T, zero.one = T),
                anomie10 = r.l(Q8_10, flip = T, zero.one = T),
                anomie11 = r.l(Q8_11, flip = T, zero.one = T),
                anomie12 = r.l(Q8_12, flip = T, zero.one = T),
                anomie13 = r.l(Q8_13, flip = T, zero.one = T),
                anomie14 = r.l(Q8_14, flip = T, zero.one = T),
                anomie15 = r.l(Q8_15, flip = T, zero.one = T),
                anomie16 = r.l(Q8_16, flip = T, zero.one = T),
                anomie17 = r.l(Q8_17, flip = T, zero.one = T), 
                
                nochoice = r.l(Q8B, flip = T, zero.one = T),
                
                socladder = r.l(Q9, zero.one = T),
                
                ideology  = r.l(Q10, zero.one = T), 
                
                hohu1 = r.l(Q11_1, flip = F, zero.one = T), #high values = lower Honesty
                hohu2 = r.l(Q11_2, flip = T, zero.one = T),
                hohu3 = r.l(Q11_3, flip = T, zero.one = T),
                hohu4 = r.l(Q11_4, flip = T, zero.one = T),
                
                sdrt1  = r.l(Q12_1, zero.one = T),
                sdrt2  = r.l(Q12_2, flip = T, zero.one = T),
                sdrt3  = r.l(Q12_3, flip = T, zero.one = T),
                sdrt4  = r.l(Q12_4, flip = T, zero.one = T),
                sdrt5  = r.l(Q12_5, flip = T, zero.one = T),
                sdrt6  = r.l(Q12_6, flip = T, zero.one = T),
                sdrt7  = r.l(Q12_7, zero.one = T),
                sdrt8  = r.l(Q12_8, zero.one = T),
                sdrt9  = r.l(Q12_9, flip = T, zero.one = T),
                sdrt10 = r.l(Q12_10, flip = T, zero.one = T),
                sdrt11 = r.l(Q12_11, flip = T, zero.one = T),
                sdrt12 = r.l(Q12_12, zero.one = T),
                sdrt13 = r.l(Q12_13, flip = T, zero.one = T),
                sdrt14 = r.l(Q12_14, flip = T, zero.one = T),
                
                solidarity_stateresp = r.l(Q13, zero.one = T), 
                solidarity_refusework = r.l(Q14, zero.one = T), 
                solidarity_equalincome = r.l(Q15, flip = T, zero.one = T), 
                
                immig_dontdiscriminate = r.l(Q16, flip = F, zero.one = T), 
                immig_goodimpact = r.l(Q17, flip = T, zero.one = T),
                immig_donttakeawayjobs = r.l(Q19, flip = F, zero.one = T), 
                immig_dontmakecrimeworse = r.l(Q20, flip = F, zero.one = T), 
                immig_dontstrainwelfare = r.l(Q21, flip = F, zero.one = T),
                immig_shouldntassimilate = r.l(Q22, flip = T, zero.one = T),
                
                soctrust= r.l(Q18, flip = T, zero.one = T),
                
                proudcitizen = r.l(Q23, flip = T, zero.one = T),
                
                gov_notharmsecon = r.l(Q24_1, zero.one = T), 
                gov_posecon = r.l(Q24_2, flip = T, zero.one = T),
                gov_necessarypol = r.l(Q24_3, flip = T, zero.one = T), 
                gov_nottooextreme = r.l(Q24_4, zero.one = T), 
                gov_united = r.l(Q24_5, flip = T, zero.one = T),
                
                system_satisfaction = r.l(Q25, zero.one = T), 
                level_democracy = r.l(Q26, zero.one = T), 
                
                nfc1  = r.l(Q27_1 , flip = T, zero.one = T),
                nfc2  = r.l(Q27_2 , flip = T, zero.one = T),
                nfc3  = r.l(Q27_3 , flip = T, zero.one = T),
                nfc4  = r.l(Q27_4 , flip = T, zero.one = T),
                nfc5  = r.l(Q27_5 , flip = T, zero.one = T),
                nfc6  = r.l(Q27_6 , flip = T, zero.one = T),
                nfc7  = r.l(Q27_7 , flip = T, zero.one = T),
                nfc8  = r.l(Q27_8 , flip = T, zero.one = T), 
                
                dem_nostrong = r.l(Q28_1, zero.one = T),
                dem_experts = r.l(Q28_2, zero.one = T),
                dem_noarmy = r.l(Q28_3, zero.one = T),
                dem_democracy = r.l(Q28_4, flip = T, zero.one = T), 
                
                misinfo_belief =  r.l(Q29_1, flip = T, zero.one = T),
                misinfo_share =  r.l(Q29_2, flip = T, zero.one = T),
                
                hate_right1 = r.l(Q30_1, flip = T, zero.one = T), #angry
                hate_right2 = r.l(Q30_2, flip = T, zero.one = T), #frustrated
                hate_right3 = r.l(Q30_3, flip = T, zero.one = T), #afraid
                hate_right4 = r.l(Q30_4, zero.one = T),           #hopeful
                hate_right5 = r.l(Q30_5, zero.one = T),           #enthusiasitc
                hate_right6 = r.l(Q30_6, zero.one = T),           #proud
                
                hate_left1 = r.l(Q31_1, flip = T, zero.one = T),
                hate_left2 = r.l(Q31_2, flip = T, zero.one = T),
                hate_left3 = r.l(Q31_3, flip = T, zero.one = T),
                hate_left4 = r.l(Q31_4, zero.one = T),
                hate_left5 = r.l(Q31_5, zero.one = T),
                hate_left6 = r.l(Q31_6, zero.one = T),
                
                no_surveillance1 = r.l(Q32_1, flip = F, zero.one = T),
                no_surveillance2 = r.l(Q32_2, flip = F, zero.one = T),
                no_surveillance3 = r.l(Q32_3, flip = F, zero.one = T),
                
                populism1 = r.l(Q33_1, flip = T, zero.one = T),
                populism2 = r.l(Q33_2, zero.one = T),
                populism3 = r.l(Q33_3, flip = T, zero.one = T),
                populism4 = r.l(Q33_4, zero.one = T),
                populism5 = r.l(Q33_5, flip = T, zero.one = T),
                populism6 = r.l(Q33_6, zero.one = T), 
                
                ais1 = r.l(Q37_1, zero.one = T),
                ais2 = r.l(Q37_2, zero.one = T),
                ais3 = r.l(Q37_3, zero.one = T),
                ais4 = r.l(Q37_4, zero.one = T),
                
                ris1 = r.l(Q37_5, zero.one = T),
                ris2 = r.l(Q37_6, zero.one = T),
                ris3 = r.l(Q37_7, zero.one = T),
                ris4 = r.l(Q37_8, zero.one = T)
                
                ) %>%  
        # now form scales
        mutate(finance = rowMeans(select(.,  nameslist$finance)),
               anomie = rowMeans(select(.,  nameslist$anomie)),
               nfc = rowMeans(select(.,  nameslist$nfc)),
               sdrt = rowMeans(select(.,  nameslist$sdrt)),
               tolerate_immigrants = rowMeans(select(.,  nameslist$tolerate_immigrants), na.rm = T),
               tol_im_short = rowMeans(select(.,  nameslist$tol_im_short)),
               gov = rowMeans(select(.,  nameslist$gov)),
               hate_right = rowMeans(select(.,  nameslist$hate_right)),
               hate_left = rowMeans(select(.,  nameslist$hate_left)),
               no_surveillance = rowMeans(select(.,  nameslist$no_surveillance)),
               cbs = rowMeans(select(.,  nameslist$cbs)),
               cbs_full = rowMeans(select(.,  nameslist$cbs_full)),
               cbs_health = rowMeans(select(.,  c("cbs1", "cbs2"))),
               cbs_finance =  rowMeans(select(.,  c("cbs3", "cbs4"))),
               cbs_sociallife =  rowMeans(select(.,  c("cbs7", "cbs8"))),
               cbs_anomie =  rowMeans(select(.,  c("cbs9", "cbs10"))),
               dishonesty_hum = rowMeans(select(.,  nameslist$dishonesty_hum)),
               populism = rowMeans(select(.,  nameslist$populism)),
               supp_demcy = rowMeans(select(.,  nameslist$supp_demcy)),
               solidarity = rowMeans(select(.,  nameslist$solidarity), na.rm = T),
               misinfo = rowMeans(select(.,  c("misinfo_belief", "misinfo_share")), na.rm = T),
               ingroup_love = case_when(ideology < 0.5 ~ r.l(hate_left, flip = T),
                                         ideology > 0.5 ~ r.l(hate_right, flip = T)),
               outgroup_love = case_when(ideology < 0.5 ~ r.l(hate_right, flip = T),
                                          ideology > 0.5 ~ r.l(hate_left, flip = T)),
               polarized_abs = abs(ingroup_love - outgroup_love),
               ais = rowMeans(select(.,  nameslist$ais), na.rm = T),
               ris = rowMeans(select(.,  nameslist$ris), na.rm = T)) %>% 
        glimpse


string_01 <- c(grep("cbs", names(df_01)), grep("anomie", names(df_01)))
names(df_01)[string_01] <- paste(names(df_01)[string_01], "01", sep = ".")


# verify that indeed all scales have a range between 0 and 1
# df_01 %>% 
#         summarise(across(c(-ID, -wave), min, na.rm = T))
# 
# df_01 %>% 
#         summarise(across(c(-ID, -wave), max, na.rm = T))


# Tidy with Z-scores -----------------------------------------
# this is identical code from above, with 3 diffs: 
        # 1. uses zraw instead of raw
        # 2. uses flip2 instead of flip 
        # 3. does not have zero.one = T
df_z <- zraw %>% 
        transmute(ID = ID, # ID and Wave needed for merging
                  wave = wave,
                  
                  my_health = r.l(Q1, flip2 = T), 
                  got_covid = ifelse(Q2 == 1, 1, 0), 
                  fin_findjob = r.l(Q3),
                  fin_v_parent = r.l(Q4, flip2 = T),
                  fin_v_child = r.l(Q5, flip2 = T), 
                  
                  cbs1 = r.l(Q7_1 , flip2 = F), #health
                  cbs2 = r.l(Q7_2 , flip2 = T), 
                  cbs3 = r.l(Q7_3 , flip2 = T), #finance
                  cbs4 = r.l(Q7_4 , flip2 = F),
                  cbs5 = r.l(Q7_5 , flip2 = T), #democracy
                  cbs6 = r.l(Q7_6 , flip2 = F),
                  cbs7 = r.l(Q7_7 , flip2 = T), #social life
                  cbs8 = r.l(Q7_8 , flip2 = F),
                  cbs9 = r.l(Q7_9 , flip2 = T), #anomie 
                  cbs10 = r.l(Q7_10, flip2 = F),
                  
                  anomie1  = r.l(Q8_1 , flip2 = T),
                  anomie2  = r.l(Q8_2 , flip2 = T),
                  anomie3  = r.l(Q8_3 , flip2 = T),
                  anomie4  = r.l(Q8_4 , flip2 = T),
                  anomie5  = r.l(Q8_5 , flip2 = T),
                  anomie6  = r.l(Q8_6 , flip2 = T),
                  anomie7  = r.l(Q8_7 , flip2 = T),
                  anomie8  = r.l(Q8_8 , flip2 = F), #our lonely reverse coded item
                  anomie9  = r.l(Q8_9 , flip2 = T),
                  anomie10 = r.l(Q8_10, flip2 = T),
                  anomie11 = r.l(Q8_11, flip2 = T),
                  anomie12 = r.l(Q8_12, flip2 = T),
                  anomie13 = r.l(Q8_13, flip2 = T),
                  anomie14 = r.l(Q8_14, flip2 = T),
                  anomie15 = r.l(Q8_15, flip2 = T),
                  anomie16 = r.l(Q8_16, flip2 = T),
                  anomie17 = r.l(Q8_17, flip2 = T), 
                  
                  nochoice = r.l(Q8B, flip2 = T),
                  
                  socladder = r.l(Q9),
                  
                  ideology  = r.l(Q10), 
                  
                  hohu1 = r.l(Q11_1, flip2 = F), #high values = lower Honesty
                  hohu2 = r.l(Q11_2, flip2 = T),
                  hohu3 = r.l(Q11_3, flip2 = T),
                  hohu4 = r.l(Q11_4, flip2 = T),
                  
                  sdrt1  = r.l(Q12_1),
                  sdrt2  = r.l(Q12_2, flip2 = T),
                  sdrt3  = r.l(Q12_3, flip2 = T),
                  sdrt4  = r.l(Q12_4, flip2 = T),
                  sdrt5  = r.l(Q12_5, flip2 = T),
                  sdrt6  = r.l(Q12_6, flip2 = T),
                  sdrt7  = r.l(Q12_7),
                  sdrt8  = r.l(Q12_8),
                  sdrt9  = r.l(Q12_9, flip2 = T),
                  sdrt10 = r.l(Q12_10, flip2 = T),
                  sdrt11 = r.l(Q12_11, flip2 = T),
                  sdrt12 = r.l(Q12_12),
                  sdrt13 = r.l(Q12_13, flip2 = T),
                  sdrt14 = r.l(Q12_14, flip2 = T),
                  
                  solidarity_stateresp = r.l(Q13), 
                  solidarity_refusework = r.l(Q14), 
                  solidarity_equalincome = r.l(Q15, flip2 = T), 
                  
                  immig_dontdiscriminate = r.l(Q16, flip2 = F), 
                  immig_goodimpact = r.l(Q17, flip2 = T),
                  immig_donttakeawayjobs = r.l(Q19, flip2 = F), 
                  immig_dontmakecrimeworse = r.l(Q20, flip2 = F), 
                  immig_dontstrainwelfare = r.l(Q21, flip2 = F),
                  immig_shouldntassimilate = r.l(Q22, flip2 = T),
                  
                  soctrust= r.l(Q18, flip2 = T),
                  
                  proudcitizen = r.l(Q23, flip2 = T),
                  
                  gov_notharmsecon = r.l(Q24_1), 
                  gov_posecon = r.l(Q24_2, flip2 = T),
                  gov_necessarypol = r.l(Q24_3, flip2 = T), 
                  gov_nottooextreme = r.l(Q24_4), 
                  gov_united = r.l(Q24_5, flip2 = T),
                  
                  system_satisfaction = r.l(Q25), 
                  level_democracy = r.l(Q26), 
                  
                  nfc1  = r.l(Q27_1 , flip2 = T),
                  nfc2  = r.l(Q27_2 , flip2 = T),
                  nfc3  = r.l(Q27_3 , flip2 = T),
                  nfc4  = r.l(Q27_4 , flip2 = T),
                  nfc5  = r.l(Q27_5 , flip2 = T),
                  nfc6  = r.l(Q27_6 , flip2 = T),
                  nfc7  = r.l(Q27_7 , flip2 = T),
                  nfc8  = r.l(Q27_8 , flip2 = T), 
                  
                  dem_nostrong = r.l(Q28_1),
                  dem_experts = r.l(Q28_2),
                  dem_noarmy = r.l(Q28_3),
                  dem_democracy = r.l(Q28_4, flip2 = T), 
                  
                  misinfo_belief =  r.l(Q29_1, flip2 = T),
                  misinfo_share =  r.l(Q29_2, flip2 = T),
                  
                  hate_right1 = r.l(Q30_1, flip2 = T), #angry
                  hate_right2 = r.l(Q30_2, flip2 = T), #frustrated
                  hate_right3 = r.l(Q30_3, flip2 = T), #afraid
                  hate_right4 = r.l(Q30_4),           #hopeful
                  hate_right5 = r.l(Q30_5),           #enthusiasitc
                  hate_right6 = r.l(Q30_6),           #proud
                  
                  hate_left1 = r.l(Q31_1, flip2 = T),
                  hate_left2 = r.l(Q31_2, flip2 = T),
                  hate_left3 = r.l(Q31_3, flip2 = T),
                  hate_left4 = r.l(Q31_4),
                  hate_left5 = r.l(Q31_5),
                  hate_left6 = r.l(Q31_6),
                  
                  no_surveillance1 = r.l(Q32_1, flip2 = F),
                  no_surveillance2 = r.l(Q32_2, flip2 = F),
                  no_surveillance3 = r.l(Q32_3, flip2 = F),
                  
                  populism1 = r.l(Q33_1, flip2 = T),
                  populism2 = r.l(Q33_2),
                  populism3 = r.l(Q33_3, flip2 = T),
                  populism4 = r.l(Q33_4),
                  populism5 = r.l(Q33_5, flip2 = T),
                  populism6 = r.l(Q33_6)) %>%  
        # now form scales
        mutate(finance = rowMeans(select(.,  nameslist$finance)),
               anomie = rowMeans(select(.,  nameslist$anomie)),
               nfc = rowMeans(select(.,  nameslist$nfc)),
               sdrt = rowMeans(select(.,  nameslist$sdrt)),
               tolerate_immigrants = rowMeans(select(.,  nameslist$tolerate_immigrants), na.rm = T),
               tol_im_short = rowMeans(select(.,  nameslist$tol_im_short)),
               gov = rowMeans(select(.,  nameslist$gov)),
               hate_right = rowMeans(select(.,  nameslist$hate_right)),
               hate_left = rowMeans(select(.,  nameslist$hate_left)),
               no_surveillance = rowMeans(select(.,  nameslist$no_surveillance)),
               cbs = rowMeans(select(.,  nameslist$cbs)),
               cbs_full = rowMeans(select(.,  nameslist$cbs_full)),
               cbs_health = rowMeans(select(.,  c("cbs1", "cbs2"))),
               cbs_finance =  rowMeans(select(.,  c("cbs3", "cbs4"))),
               cbs_sociallife =  rowMeans(select(.,  c("cbs7", "cbs8"))),
               cbs_anomie =  rowMeans(select(.,  c("cbs9", "cbs10"))),
               dishonesty_hum = rowMeans(select(.,  nameslist$dishonesty_hum)),
               populism = rowMeans(select(.,  nameslist$populism)),
               supp_demcy = rowMeans(select(.,  nameslist$supp_demcy)),
               solidarity = rowMeans(select(.,  nameslist$solidarity), na.rm = T),
               misinfo = rowMeans(select(.,  c("misinfo_belief", "misinfo_share")), na.rm = T),
               ingroup_love = case_when(ideology < 0.5 ~ r.l(hate_left, flip = T),
                                        ideology > 0.5 ~ r.l(hate_right, flip = T)),
               outgroup_love = case_when(ideology < 0.5 ~ r.l(hate_right, flip = T),
                                         ideology > 0.5 ~ r.l(hate_left, flip = T)),
               polarized_abs = abs(ingroup_love - outgroup_love)) %>% 
        # add leaded and lagged predictors
        group_by(ID) %>%  # first group by respondents 
        arrange(wave) %>% # and arrange in the order of waves 
        # add leaded and lagged versions of the two IVs
        mutate(cbs_lead =lead(cbs),
               anomie_lead = lead(anomie),
               cbs_lag = lag(cbs),
               anomie_lag = lag(anomie)) %>%
        ungroup()

# add .z to variable names. these are used primarily for benchmarkplot

# define variables not to be renamed. country + wave + all cbs and anomie vars.
z_string <- c(-1:-2, -grep("cbs", names(df_z)), 
              -grep("anomie", names(df_z)))

names(df_z)[z_string] <- paste(names(df_z)[z_string], "z", sep = ".")



# Combine Demographics + 0-1 + Z-scored DFs.------------------------------------ 

df <- left_join(df_demographics, df_z) %>% 
        left_join(df_01)

saveRDS(df, "B-analysis-data/yougov_clean_20211210.rds")
# df <- readRDS("B-analysis-data/yougov_clean_20211210.rds")


# Attrition and wave dates ---------------------------------------------------------------

# calculate the median data collection date in each country-wave
wavesndates <- df %>% 
        group_by(wave) %>% 
        # group_by(country, wave) %>% 
        summarise(wavedate = median(date)) %>% 
        # select(-wave) %>% 
        print

# Infection data ----------------------------------------------------------

# source 
# https://www.ecdc.europa.eu/en/publications-data/data-national-14-day-notification-rate-covid-19
# retreived on 2021-06-25
raw.inf <- import("A-original-data/ecdc_deaths.csv")
        
# Loading data and shaping for plots
infection_data <-raw.inf %>% 
        # mutate(date = lubridate::parse_date_time(year_week, "%Y-%W")) %>% glimpse
        # mutate(date = ISOweek2date(year_week)) %>% glimpse
        mutate(year_week_date = paste(year_week, "01", sep="-"),
               date = as.Date(year_week_date, "%Y-%U-%w")) %>% 
        # separate(year_week, c("year", "week"), sep = "-", remove = F) %>% 
        filter(country %in% c("Hungary", "Denmark", "Italy", 
                              "United States Of America") &
                      indicator == "deaths" &
                      # year == "2020" &
                      #  as.numeric(week) >= 14) %>% 
                      date >= ymd("2020-03-30") & 
                       date <= ymd("2021-01-01")) %>% 
        mutate(country = fct_recode(country, USA = "United States Of America"),
               country = fct_relevel(country, "Hungary", "Italy", "USA"))%>% 
        glimpse

saveRDS(infection_data, "B-analysis-data/infections.rds")

# Stringency data ---------------------------------------------------------

raw.string <- import("A-original-data/OxCGRT_20211116.csv") %>% 
        glimpse

stringency_data <- raw.string %>% 
        filter(CountryName %in% c("Denmark", "Hungary", "Italy", "United States") & 
                       Jurisdiction == "NAT_TOTAL") %>% 
        filter(ymd(Date) >= ymd(c("2020-03-30")) & 
                                        ymd(Date) <= ymd(c("2021-01-01"))) %>% 
        transmute(country = fct_recode(CountryName, USA = "United States"),
                  country = fct_relevel(country, "Hungary", "Italy", "USA"),
                  date = ymd(Date),
                  stringency = StringencyIndex) %>% 
        glimpse()
saveRDS(stringency_data, "B-analysis-data/stringency.rds")

