# Initial script

# 27/7/2015


# Eligible if: 
# age 22 to 25 in wave A or wave B

# Subgroups: do you have a full driving licence? (latest response)
# wDRIVER in wINDRESP for w in a, b

# wCARUSE in wINDRESP for w from c onwards
# 1 : yes
# 2: no
# 3: does not drive




# And whether this affects health, ghq, etc

#wHLGHQ1-2 in wINDRESP : GHQ score

rm(list = ls())

require(readr)

require(stringr)
require(tidyr)
require(car)

require(Zelig)

require(ggplot2)
require(lattice)

require(plyr)
require(dplyr)



source("scripts/manage_data.R")

source("scripts/create_graphs.R")


# Modelling/descriptive stats ---------------------------------------------




# How does the probability of B learning to drive have on A l --------

# Find all rows in egoalt where alter is partner

partners <- all_egoalts %>% 
  filter(simple_relation =="partner") %>% 
  select(-relation, -pno, -opno, -simple_relation)

# Find partner age, partner gender, partner driving status

tmp <- all_inds_drvs %>% select(pid, wave, sex, age, drives, highqual) %>% 
  inner_join(partners, by = c("pid", "wave")) %>%
  rename(ego_drives = drives , ego_highqual = highqual, ego_age = age, ego_sex = sex) 
  
tmp2 <- all_inds_drvs %>% select(opid = pid, wave, sex, age, drives, highqual) %>% 
  inner_join(partners, by = c("opid", "wave")) %>% 
  rename(alter_drives = drives, alter_highqual = highqual, alter_age = age, alter_sex = sex)

partners <- tmp2 %>% select(-hid, -pid) %>% inner_join(tmp)
rm(tmp, tmp2)

# Want to find households with partners who both do not drive

# Identify the PIDS of people who were nondrivers in wave 1 and their partners were nondrivers in wave 1

nondriver_wave1_pids <- partners %>% 
    mutate(
      firstwave = wave == 1,
      ego_drives_firstwave = firstwave & ego_drives == "no",
      alter_drives_firstwave = firstwave & alter_drives == "no"
      ) %>% 
  filter(ego_drives_firstwave & alter_drives_firstwave) %>%
  .$pid 


nondriver_wave8_pids <- partners %>% 
  mutate(
    firstwave = wave == 8,
    ego_drives_firstwave = firstwave & ego_drives == "no",
    alter_drives_firstwave = firstwave & alter_drives == "no"
  ) %>% 
  filter(ego_drives_firstwave & alter_drives_firstwave) %>%
  .$pid 


nondrivers_wave1 <- partners %>% 
  filter(pid %in% nondriver_wave1_pids) %>% 
  mutate(
    ego_drives = recode(ego_drives, "'yes' = 1; 'no' = 0; else = NA"), 
    alter_drives = recode(alter_drives, "'yes' = 1; 'no' = 0; else = NA")
    )


nondrivers_wave1$alter_sex <- as.factor(nondrivers_wave1$alter_sex) 
nondrivers_wave1$ego_sex <- as.factor(nondrivers_wave1$ego_sex)
nondrivers_wave1$ego_highqual <- as.factor(nondrivers_wave1$ego_highqual)
nondrivers_wave1$alter_highqual <- as.factor(nondrivers_wave1$alter_highqual)

nondrivers_wave1 <- nondrivers_wave1 %>%  
  dplyr::select(wave, alter_sex, alter_age, alter_drives, alter_highqual, ego_sex, ego_age, ego_drives, ego_highqual)

class(nondrivers_wave1) <- "data.frame" # this seems to make all the difference


# zelig model
z01 <- zelig(ego_drives ~ wave, model = "logit", data = nondrivers_wave1)
z02 <- zelig(ego_drives ~ wave + ego_sex, model = "logit", data = nondrivers_wave1)
z03 <- zelig(ego_drives ~ wave + ego_sex + ego_age, model= "logit", data = nondrivers_wave1)
AIC(z01, z02, z03)
# Each model adds enough additional precision to be worth the additional model compelxity

z04 <- zelig(ego_drives ~ wave + ego_sex + ego_age + wave:ego_age, model = "logit", data = nondrivers_wave1)

AIC(z03, z04)
# The interaction term is also worth adding

z05 <- zelig(ego_drives ~ wave + ego_sex + poly(ego_age, degree =3), model = "logit", data = nondrivers_wave1)
# 3rd degree polynomial not enough of an improvement

z06 <- zelig(ego_drives ~ wave + ego_sex*poly(ego_age, degree = 2), model = "logit", data = nondrivers_wave1)
# also not an improvement 

z07 <- zelig(ego_drives ~ poly(wave, 3)*poly(ego_age, 3)*ego_sex, model ="logit", data = nondrivers_wave1)
# this is complex, but has the lowest fit by far. It indicates the importance of taking into 
# account age, peroid and cohort effects in the model. 

z08 <- zelig(ego_drives ~ poly(wave, 2)*poly(ego_age, 2)*ego_sex, model ="logit", data = nondrivers_wave1)
# also too complex

z09 <- zelig(ego_drives ~ poly(wave, 2)*poly(ego_age, 2) + ego_sex, model ="logit", data = nondrivers_wave1)


z10 <- zelig(ego_drives ~ poly(wave, 3)*poly(ego_age, 3)*ego_sex + ego_highqual, model ="logit", data = nondrivers_wave1)

# This has a still lower AIC 
z11 <- zelig(ego_drives ~ poly(wave, 3)*poly(ego_age, 3)*ego_sex* ego_highqual, model ="logit", data = nondrivers_wave1)
# This has a lower AIC still than z10, despite three times as many params

# Now, finally, to add the influence of alter learning to drive 

z12 <- zelig(ego_drives ~ poly(wave, 3)*poly(ego_age, 3)*ego_sex* ego_highqual + alter_drives, 
             model ="logit", data = nondrivers_wave1)

# This greatly reduces the AIC, but in what direction?

summary(z12)
# Overall the effect is positive and highly statistically significant

# But what about if this is added as an interaction to all the others?

z13 <- zelig(ego_drives ~ poly(wave, 3)*poly(ego_age, 3)*ego_sex* ego_highqual * alter_drives, 
             model ="logit", data = nondrivers_wave1)
# this has the lowest fit of all, despite being extremely complex

summary(z13)

# Now no single variable is stat significant despite the AIC being lower, as the effect of any single variable 
# operates through its interaction with many other variables 

# This is why simulation is so important for understanding the model


# What if simulations  ----------------------------------------------------


# hypothetical: first wave, 25 year olds, male ego
# first dif is alter drives vs alter does not drive

x13_nodrive <- setx(
  z13,
wave = 1, ego_sex = "male", alter_sex = "female", ego_age = 25
)


x04_nodrive <- setx(
  z04,
  wave = 1, ego_sex = "male", alter_sex = "female", ego_age = 25
)

x04_nodrive1 <- setx(
  z04,
  wave = 7, ego_sex = "male", alter_sex = "female", ego_age = 25
)


s_out <- sim(obj =z04, x=x04_nodrive, x1 = x04_nodrive1)


x07_nodrive <- setx(
  z07,
  wave = 1, ego_sex = "male", alter_sex = "female", ego_age = 25
)

x07_nodrive1 <- setx(
  z07,
  wave = 7, ego_sex = "male", alter_sex = "female", ego_age = 25
)


s_out <- sim(obj =z07, x=x07_nodrive, x1 = x07_nodrive1)



x06_nodrive <- setx(
  z06,
  wave = 1, ego_sex = "male", alter_sex = "female", ego_age = 25
)

x06_nodrive1 <- setx(
  z06,
  wave = 7, ego_sex = "male", alter_sex = "female", ego_age = 25
)


s_out <- sim(obj =z06, x=x06_nodrive, x1 = x06_nodrive1)
