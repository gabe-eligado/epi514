# EPI 514 Project
# Comparing Prevalence of Coronary Heart Disease and Angina in the United States
# in Individuals Under the Poverty Line Based on Insurance Coverage
#
# Data source: BRFSS 2023
#
# Written by: Gabe Eligado
#
# Last updated on: April 29, 2025 @ 2:30 PM





#----------------------------IMPORT AND CLEAN BRFSS-----------------------------

rm(list = ls())

#Setup
library(tidyverse)
library(haven)
library(survey)
setwd("/Users/gabe/Desktop/R Projects/EPI 514")

#Import BRFSS
brfss <- read_xpt("LLCP2023.XPT")
brfss <- type.convert(brfss)

#Rename variables
names(brfss) <- tolower(names(brfss))
names(brfss) <- gsub('._', 'X_', names(brfss), fixed = TRUE)

#Convert exposure & outcome variables to labelled factors
brfss$primins1_f <- factor(brfss$primins1,
                             levels = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 77, 88, 99),
                             labels = c("Employer/Union",
                                        "Self-purchased",
                                        "Medicare",
                                        "Medigap",
                                        "Medicaid",
                                        "Childrens' Health",
                                        "TRICARE, VA, CHAMPUS",
                                        "Indian Health Service",
                                        "State-sponsored",
                                        "Other gov. plan",
                                        "Don't Know",
                                        "None",
                                        "Refused"
                                        )
                           )

brfss$cvdcrhd4_f <- factor(brfss$cvdcrhd4,
                         levels = c(1, 2, 7, 9),
                         labels = c("Yes", "No", "Don't Know", "Refused"))

#Select variables for analysis
brfss_clean <- brfss %>% select(#Survey weights
                                "_psu", "_ststr", "_llcpwt",
                                
                                #Poverty variables
                                "hhadult", "children", "numadult","income3",
                                
                                #Exposure & Outcome variables
                                "primins1", "primins1_f", "cvdcrhd4", "cvdcrhd4_f",
                                
                                #Confounders
                                "_age65yr", "diabete4", "chckdny2",
                                
                                #Effect Modifiers
                                "sexvar", "_smoker3",
                                
                                #Other
                                "_imprace"
                                )
              
#Remove rows with no info on adults in household
which(is.na(brfss_clean$hhadult) & is.na(brfss_clean$numadult))
brfss_clean <- brfss_clean %>% filter(!(is.na(hhadult) & is.na(numadult)))
which(is.na(brfss_clean$hhadult) & is.na(brfss_clean$numadult))





#---------------------------DETERMINE POVERTY STATUS----------------------------

#Set "Don't know" / "Refused" / NA to 0
brfss_clean$hhadult[brfss_clean$hhadult == 77] <- 0
brfss_clean$hhadult[brfss_clean$hhadult == 99] <- 0
brfss_clean$hhadult[is.na(brfss_clean$hhadult)] <- 0

brfss_clean$children[brfss_clean$children == 88] <- 0
brfss_clean$children[brfss_clean$children == 99] <- 0
brfss_clean$children[is.na(brfss_clean$children)] <- 0

brfss_clean$numadult[brfss_clean$numadult == 77] <- 0
brfss_clean$numadult[brfss_clean$numadult == 99] <- 0
brfss_clean$numadult[is.na(brfss_clean$numadult)] <- 0


#Create total household variable
brfss_clean$household <- NA
brfss_clean <- brfss_clean %>% mutate(household = hhadult + numadult + children)
brfss_clean %>% select(hhadult, numadult, children, household)


#Determine Poverty status
brfss_clean$poverty <- "Above Poverty Line"
brfss_clean <- brfss_clean %>%
  mutate(
    poverty = case_when(
      household == 1 & income3 <= 2 ~ "At or Below Poverty Line",
      household == 2 & income3 <= 3 ~ "At or Below Poverty Line",
      household == 3 & income3 <= 4 ~ "At or Below Poverty Line",
      household == 4 & income3 <= 4 ~ "At or Below Poverty Line",
      household == 5 & income3 <= 5 ~ "At or Below Poverty Line",
      household == 6 & income3 <= 5 ~ "At or Below Poverty Line",
      household == 7 & income3 <= 5 ~ "At or Below Poverty Line",
      household == 8 & income3 <= 6 ~ "At or Below Poverty Line",
      household == 9 & income3 <= 6 ~ "At or Below Poverty Line",
      household == 10 & income3 <= 6 ~ "At or Below Poverty Line",
      household == 11 & income3 <= 6 ~ "At or Below Poverty Line",
      household == 12 & income3 <= 6 ~ "At or Below Poverty Line",
      household == 13 & income3 <= 7 ~ "At or Below Poverty Line",
      household == 14 & income3 <= 7 ~ "At or Below Poverty Line",
      household == 15 & income3 <= 7 ~ "At or Below Poverty Line",
      household == 16 & income3 <= 7 ~ "At or Below Poverty Line",
      household == 17 & income3 <= 7 ~ "At or Below Poverty Line",
      household == 18 & income3 <= 8 ~ "At or Below Poverty Line",
      TRUE ~ "Above Poverty Line"
    )
  )


#Subset data to only individuals living under 100% poverty line
pov <- brfss_clean %>% filter(poverty == "At or Below Poverty Line")





#----------------------------DETERMINE SAMPLE SIZE------------------------------

#Survey design
options(survey.lonely.psu = "adjust")
pvdesign <- svydesign(data = pov, 
                      id = ~`_psu`, 
                      strata = ~`_ststr`, 
                      weights = ~`_llcpwt`,
                      nest = TRUE)


#Cross-tab
svytable(~primins1_f + cvdcrhd4_f, design = pvdesign)
svytable(~primins1_f, design = pvdesign)





#-----------------------TABLE 1 (NEED TO CLEAN UP AFTER)------------------------
#Convert insurnace into binary (0 = uninsured, 1 = insured[employer, private, m-aid, m-care, IHS])
pov$ins01[pov$primins1 == 88] <- 0
pov$ins01[pov$primins1 == 1] <- 1
pov$ins01[pov$primins1 == 2] <- 1
pov$ins01[pov$primins1 == 3] <- 1
pov$ins01[pov$primins1 == 5] <- 1
pov$ins01[pov$primins1 == 8] <- 1


#Subset data to only uninsured individuals
uninsured <- pov %>% filter(ins01 == 0)
options(survey.lonely.psu = "adjust")
uninsured_design <- svydesign(data = uninsured, 
                      id = ~`_psu`, 
                      strata = ~`_ststr`, 
                      weights = ~`_llcpwt`,
                      nest = TRUE)

#Subset data to only insured individuals
insured <- pov %>% filter(ins01 == 1)
options(survey.lonely.psu = "adjust")
insured_design <- svydesign(data = insured, 
                              id = ~`_psu`, 
                              strata = ~`_ststr`, 
                              weights = ~`_llcpwt`,
                              nest = TRUE)


#Svytable of demographics (UNINSURED)
unins_age <- svytable(~`_age65yr`, design = uninsured_design)
prop.table(unins_age)

(unins_sex <-svytable(~sexvar, design = uninsured_design))
prop.table(unins_sex)

(unins_diabetes <-svytable(~diabete4, design = uninsured_design))
prop.table(unins_diabetes)

(unins_ckd <-svytable(~chckdny2, design = uninsured_design))
prop.table(unins_ckd)

(unins_smoke <-svytable(~`_smoker3`, design = uninsured_design))
prop.table(unins_smoke)

(unins_race <-svytable(~`_imprace`, design = uninsured_design))
prop.table(unins_race)


#Svytable of demographics (INSURED)
(ins_age <- svytable(~`_age65yr`, design = insured_design))
prop.table(ins_age)

(ins_sex <-svytable(~sexvar, design = insured_design))
prop.table(ins_sex)

(ins_diabetes <-svytable(~diabete4, design = insured_design))
prop.table(ins_diabetes)

(ins_ckd <-svytable(~chckdny2, design = insured_design))
prop.table(ins_ckd)
format(prop.table(ins_ckd), scientific = FALSE)

(ins_smoke <-svytable(~`_smoker3`, design = insured_design))
prop.table(ins_smoke)

(ins_race <-svytable(~`_imprace`, design = insured_design))
prop.table(ins_race)

svytable(~ins01, design = pvdesign)

