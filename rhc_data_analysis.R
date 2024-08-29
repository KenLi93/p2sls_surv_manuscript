###############################
### DATA ANALYSIS FOR PAPER
############ data process ####################
rm(list=ls())
library(dplyr)
library(lubridate)
library(haven)
library(pci2s)
print(getwd())

rhc <- read.csv("application/rhc.csv") %>%
  mutate(
    ## change the dates to the Date format
    sadmdte = as.Date(sadmdte, origin = "1960-1-1"),
    dschdte = as.Date(dschdte, origin = "1960-1-1"),
    dthdte = as.Date(dthdte, origin = "1960-1-1"),
    lstctdte = as.Date(lstctdte, origin = "1960-1-1"),
    ## dichotomize variables
    death = as.numeric(death == "Yes"), 
    sex = as.numeric(sex == "Female"),
    raceblack = as.numeric(race == "black"),
    raceother = as.numeric(race == "other"),
    ## income: under $11k is reference
    income1 = as.numeric(income == "$11-$25k"),
    income2 = as.numeric(income == "$25-$50k"),
    income3 = as.numeric(income == "> $50k"),
    ## insurance type: private is reference
    ins_care = as.numeric(ninsclas == "Medicare"),
    ins_pcare = as.numeric(ninsclas == "Private & Medicare"),
    ins_caid = as.numeric(ninsclas == "Medicaid"),
    ins_no = as.numeric(ninsclas == "No insurance"),
    ins_carecaid = as.numeric(ninsclas == "Medicare & Medicaid"),
    ## primary disease category: ARF is reference
    cat1_copd = as.numeric(cat1 == "COPD"),
    cat1_mosfsep = as.numeric(cat1 == "MOSF w/Sepsis"),
    cat1_mosfmal = as.numeric(cat1 == "MOSF w/Malignancy"),
    cat1_chf = as.numeric(cat1 == "CHF"),
    cat1_coma = as.numeric(cat1 == "Coma"),
    cat1_cirr = as.numeric(cat1 == "Cirrhosis"),
    cat1_lung = as.numeric(cat1 == "Lung Cancer"),
    cat1_colon = as.numeric(cat1 == "Colon Cancer"),
    ## secondary disease category: NA is reference
    cat2_mosfsep = as.numeric(cat2 == "MOSF w/Sepsis" & !is.na(cat2)),
    cat2_coma = as.numeric(cat2 == "Coma" & !is.na(cat2)),
    cat2_mosfmal = as.numeric(cat2 == "MOSF w/Malignancy" & !is.na(cat2)),
    cat2_lung = as.numeric(cat2 == "Lung Cancer" & !is.na(cat2)),
    cat2_cirr = as.numeric(cat2 == "Cirrhosis" & !is.na(cat2)),
    cat2_colon = as.numeric(cat2 == "Colon Cancer" & !is.na(cat2)),
    ## respiratory diagnosis
    resp = as.numeric(resp == "Yes"),
    ## cardiovascular diagnosis
    card = as.numeric(card == "Yes"),
    ## Neurological diagnosis
    neuro = as.numeric(neuro == "Yes"),
    ## Gastrointestinal diagnosis
    gastr = as.numeric(gastr == "Yes"),
    ## Renal diagnosis
    renal = as.numeric(renal == "Yes"),
    ## Metabolic diagnosis
    meta = as.numeric(meta == "Yes"),
    ## Hematologic diagnosis
    hema = as.numeric(hema == "Yes"),
    ## Sepsis diagnosis
    seps = as.numeric(seps == "Yes"),
    ## Trauma diagnosis
    trauma = as.numeric(trauma == "Yes"),
    ## Orthopedic diagnosis
    ortho = as.numeric(ortho == "Yes"),
    ## ADL
    MISSadld3p = as.numeric(is.na(adld3p)),
    ## DNR status on day 1
    dnr1 = as.numeric(dnr1 == "Yes"),
    ## cancer: yes/no/metastatic. No is reference
    ca_yes = as.numeric(ca == "Yes"),
    ca_meta = as.numeric(ca == "Metastatic"),
    # das2d3pc    Duke Activity Status Index, numeric         
    # surv2md1    SUPPORT model estimated survival probability for 2 months
    # aps1        APACHE score, numeric
    # scoma1      Glasgow coma score, numeric
    # wtkilo1     weight in kilo, numeric                                              
    wt0 = as.numeric(wtkilo1 == 0), ## a lot of zero measurements: make extra indicator saying this is 0
    # temp1       temperature, numeric      
    # meanbp1     blood pressure, numeric
    # resp1       respiratory rate, numeric
    # hrt1        heart rate, numeric
    # pafi1       PaO2/FIO2 ratio, numeric
    # paco21      PaCo2, numeric
    # ph1         PH, numeric
    # wblc1       WBC, numeric
    # hema1       Hematocrit, numeric
    # sod1        Sodium, numeric
    # pot1        Potassium, numeric
    # crea1       Creatinine, numeric
    # bili1       Bilirubin, numeric
    # alb1        Albumin, numeric
    # urin1       Urine output, numeric                                                              
    MISSurin1 = as.numeric(is.na(urin1)),  ## ubducatir fir nussubg urine output
    # cardiohx    yes/no, already in 0/1
    # chfhx       yes/no, already in 0/1
    # dementhx    yes/no, already in 0/1
    # psychhx     yes/no, already in 0/1
    # chrpulhx    yes/no, already in 0/1
    # renalhx     yes/no, already in 0/1
    # liverhx     yes/no, already in 0/1
    # gibledhx    yes/no, already in 0/1
    # malighx     yes/no, already in 0/1
    # immunhx     yes/no, already in 0/1
    # transhx     yes/no, already in 0/1
    # amihx       yes/no, already in 0/1
    
    ## Treatment variable
    # swang1      RHC or NO RHC               CATEGORICAL: MAKE BINARY INDICATOR (1)
    swang1 = as.numeric(swang1 == "RHC"),   # 1 indicates RHC was given, 0 means NO RHC
    
    ## time from admission to death
    t2dth = time_length(dthdte - sadmdte, unit = "year"),
    
    ## time from last contact to death
    t2lstct = time_length(lstctdte - sadmdte, unit = "year"),
    # t2event = ifelse(death == 1, t2dth, t2lstct) + rnorm(length(t2dth)) * 1e-8 # break tie by adding a small random number
    t2event = ifelse(death == 1, t2dth, t2lstct)
  )

Y <- rhc[, "t2event"]
D <- rhc[, "death"]
A <- rhc[, "swang1"]
W <- rhc[, c("ph1", "hema1")]
Z <- rhc[, c("pafi1", "paco21")]
X <- rhc[, c("age", "sex", "raceblack", "raceother", "edu", "income1", "income2", "income3",
             "ins_care", "ins_pcare", "ins_caid", "ins_no", "ins_carecaid", "cat1_copd",
             "cat1_mosfsep", "cat1_mosfmal", "cat1_chf", "cat1_coma", "cat1_cirr",
             "cat1_lung", "cat1_colon", "cat2_mosfsep", "cat2_coma", "cat2_mosfmal",
             "cat2_lung", "cat2_cirr", "cat2_colon", "resp", "card", "neuro", "gastr",
             "renal", "meta", "seps", "trauma", "ortho", "das2d3pc", "dnr1", "ca_yes", 
             "ca_meta", "surv2md1", "aps1", "scoma1", "wtkilo1", "temp1", "meanbp1", "resp1",
             "hrt1", "wblc1", "sod1", "pot1", "crea1", "bili1", "alb1", "cardiohx", "chfhx",
             "dementhx", "psychhx", "chrpulhx", "renalhx", "liverhx", "gibledhx", "malighx",
             "immunhx", "transhx", "amihx", "wt0")]
#=====================================
#==============Analysis===============
#=====================================
## unadjusted ah model
unadjusted_ah <- lin_ah(time = Y,
                        event = D,
                        covariates = A)
with(unadjusted_ah, c(ESTIMATE, SE[1]))
with(unadjusted_ah, c(ESTIMATE, ESTIMATE + qnorm(c(0.025, 0.975)) * SE[1]))
exp(-with(unadjusted_ah, c(ESTIMATE, ESTIMATE + qnorm(c(0.025, 0.975)) * SE[1])) * 30/365.25)
#  0.196934 0.046378 CI: 0.1060346 0.2878326

## ah model adjusted for X


adjusted_ah1 <- lin_ah(time = Y, event = D, covariates = cbind(A, X))
with(adjusted_ah1, c(ESTIMATE[1], SE[1]))

with(adjusted_ah1, c(ESTIMATE[1], ESTIMATE[1] + qnorm(c(0.025, 0.975)) * SE[1]))
exp(-with(adjusted_ah1, c(ESTIMATE[1], ESTIMATE[1] + qnorm(c(0.025, 0.975)) * SE[1])) * 30/365.25)


#  0.1882299 0.0601987 0.3162611
## ah model adjusted for X, Z and W
adjusted_ah2 <- lin_ah(time = Y, event = D, covariates = cbind(A, X, Z, W))
with(adjusted_ah2, c(ESTIMATE[1], ESTIMATE[1] + qnorm(c(0.025, 0.975)) * SE[1]))
exp(-with(adjusted_ah2, c(ESTIMATE[1], ESTIMATE[1] + qnorm(c(0.025, 0.975)) * SE[1])) * 30/365.25)

# 0.19596989 0.06912863 0.32281115

pci_result <- p2sls.ah(Y = Y, D = D, A = A, 
                     Z = Z, W = W, X = X, variance = T)
with(pci_result, c(ESTIMATE[1], ESTIMATE[1] + qnorm(c(0.025, 0.975)) * SE[1]))
exp(-with(pci_result, c(ESTIMATE[1], ESTIMATE[1] + qnorm(c(0.025, 0.975)) * SE[1])) * 30/365.25)

with(pci_result, c(ESTIMATE[1], ESTIMATE[1] + qnorm(c(0.025, 0.975)) * SE[1]))
# 0.16114209 0.02547685 0.29680733
save(pci_result, file = "rhc_results.RData")
