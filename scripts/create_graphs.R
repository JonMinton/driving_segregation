# Graphs - individual level -----------------------------------------------

# prop who like neighbourhood by wave, sex and driver status
all_inds_drvs %>% 
  filter(!is.na(neigh) & !is.na(dlo)) %>% 
  group_by(sex, wave, dlo, neigh) %>% 
  tally %>% 
  spread(neigh, n) %>% 
  mutate(like_prop = yes/ (mixed + no + yes)) %>% 
  ggplot(., mapping = aes(x = factor(wave), y = like_prop, colour = dlo, group = dlo)) +
  geom_line() + geom_point() + 
  facet_wrap(~sex)

#prop who like neighbourhood by wave, age group, sex and driver status
all_inds_drvs %>% 
  filter(!is.na(neigh) & !is.na(dlo)) %>% 
  mutate(age_grp = 
           cut(age, 
               breaks = c(17, 21, 26, 35, 50, 65, 80), 
               include.lowest = T, 
               labels = c("17-20", "21-25", "26-35", "36-49", "50-64", "65-79")
           )
  ) %>% 
  filter(!is.na(age_grp)) %>% 
  group_by(age_grp, sex, wave, dlo, neigh) %>% 
  tally %>% 
  spread(neigh, n) %>% 
  mutate(like_prop = yes/ (mixed + no + yes)) %>% 
  ggplot(., mapping = aes(x = factor(wave), y = like_prop, colour = dlo, group = dlo)) +
  geom_line() + geom_point() + 
  facet_grid(sex ~age_grp) + 
  labs(x = "BPHS Wave", y = "proportion of respondents who like their neighbourhood")

ggsave("figures/prop_who_like_neighbourhood.png", dpi = 300, height = 15, width = 15, units = "cm")
# This suggests there's a particularly high level of neighbourhood dissatisfaction amongst 
# Males aged 26-35 who do not drive, particularly in wave 8 (H)



all_inds_drvs %>% mutate(
  age_grp = ntile(age, 10)
) %>% 
  filter(!is.na(sex) & !is.na(age_grp)) %>% 
  group_by(sex, age_grp, dlo) %>% 
  summarise(mn_ghq = mean(ghq, na.rm=T)) %>%
  filter(!is.na(dlo)) %>% 
  ggplot(data = ., 
         mapping =     
           aes(
             x = factor(age_grp), y = mn_ghq, 
             group = sex, colour = sex, fill = sex
           )
  ) + 
  geom_line() + geom_point() + 
  facet_wrap(~ dlo) + 
  labs(
    x = "Age group decile", y = "Mean GHQ Score (lower is better)",
    title = "Mean GHQ score by decile of age, sex, and whether has driving licence"
  )


# Proportion who drive by age, all years (so double counting etc)
all_inds_drvs %>%   
  filter(!is.na(sex)) %>% 
  arrange(sex, age) %>% 
  group_by(sex, age) %>% 
  mutate(does_drive = recode(dlo, "'yes' = 1; 'no' = '0'; else = NA")) %>% 
  summarise(driv_prop = mean(does_drive, na.rm=T)) %>% 
  ggplot(. ) +
  geom_line(aes(x = age, y = driv_prop, colour = sex, group = sex))

# Proportion who drive by age, faceted by wave 
all_inds_drvs %>%   
  filter(!is.na(sex)) %>% 
  arrange(sex, age) %>% 
  mutate(wave_year = 1990 + wave) %>% 
  group_by(wave_year, sex, age) %>%
  filter(age <= 80) %>% 
  mutate(
    does_drive = recode(dlo, "'yes' = 1; 'no' = '0'; else = NA")
  ) %>% 
  summarise(driv_prop = mean(does_drive, na.rm=T)) %>% 
  ggplot(. ) +
  geom_line(aes(x = age, y = driv_prop, colour = sex, group = sex)) + 
  facet_wrap( ~ wave_year) + 
  labs(
    title = "Proportion with driving licence by BHPS wave, age and sex",
    x = "Age (years)", 
    y = "Proportion holding driving licence"
  )
ggsave("figures/prop_drive_facet_by_waveyear.png", height = 30, width = 30, units = "cm", dpi = 300)

# This suggests a much higher proportion of males who drive from mid 20s onwards


all_inds_drvs %>%   
  filter(!is.na(sex)) %>% 
  arrange(sex, age) %>% 
  group_by(wave, sex, age) %>% 
  mutate(does_drive = recode(dlo, "'yes' = 1; 'no' = '0'; else = NA")) %>% 
  summarise(driv_prop = mean(does_drive, na.rm=T)) %>% 
  ggplot(. ) +
  geom_line(aes(x = age, y = driv_prop)) + 
  facet_grid(wave ~ sex)

all_inds_drvs %>%   
  filter(!is.na(sex)) %>% 
  arrange(sex, age) %>% 
  group_by(wave, sex, age) %>% 
  mutate(does_drive = recode(dlo, "'yes' = 1; 'no' = '0'; else = NA")) %>% 
  summarise(driv_prop = mean(does_drive, na.rm=T)) %>% 
  ggplot(. ) +
  geom_line(
    aes(
      x = age, 
      y = driv_prop, 
      colour = factor(wave), 
      group = factor(wave)
    )) + 
  facet_wrap( ~ sex)



# proportion who drive by groups of wave
all_inds_drvs %>%   
  filter(!is.na(sex)) %>% 
  mutate(wave_grp = 
           cut(wave, 
               breaks = c(1, 4, 9, 14, 20), 
               include.lowest = T, 
               labels = c("early 90s", "late 90s", "early 2000s", "late 2000s")
           )
  ) %>% 
  filter(age <= 80) %>% 
  arrange(wave_grp, sex, age) %>% 
  group_by(wave_grp, sex, age) %>% 
  mutate(does_drive = recode(dlo, "'yes' = 1; 'no' = '0'; else = NA")) %>% 
  summarise(driv_prop = mean(does_drive, na.rm=T)) %>% 
  ggplot(. ) +
  geom_line(
    aes(
      x = age, 
      y = driv_prop, 
      colour = wave_grp, 
      group = wave_grp
    )) + 
  facet_wrap( ~ sex) + 
  labs(
    title = "Proportion with driving licence by group of waves, sex, and year",
    x = "Age (years)",
    y = "Proportion holding driving licence"
  )

ggsave("figures/propdrive_wavegrp_sex_age.png", height = 12, width = 15, dpi = 300, units = "cm")


# Now to look at cohorts

all_inds_drvs %>% 
  mutate(
    year = wave + 1990,
    birth_cohort = year - age,
    brth_chrt_grp = cut(
      birth_cohort, 
      breaks= c(1930, 1940, 1950, 1960, 1970, 1980, 1990), 
      include.lowest = T, 
      labels = c("1930s", "1940s", "1950s", "1960s", "1970s", "1980s")                        )
  ) %>% 
  filter(!is.na(brth_chrt_grp) & !is.na(sex)) %>% 
  arrange(brth_chrt_grp, sex, age) %>% 
  group_by(brth_chrt_grp, sex, age) %>% 
  mutate(does_drive = recode(dlo, "'yes' = 1; 'no' = '0'; else = NA")) %>% 
  summarise(driv_prop = mean(does_drive, na.rm=T)) %>% 
  ggplot(. ) +
  geom_line(
    aes(
      x = age, 
      y = driv_prop, 
      colour = brth_chrt_grp, 
      group = brth_chrt_grp
    )) + 
  facet_wrap( ~ sex) +
  labs(
    title = "Proportion holding driving licence by decadal birth cohort",
    x = "Age of cohort members (year)",
    y = "Proportion holding driving licence"
  )
ggsave("figures/propdrive_cohortgrp_sex_age.png", height = 12, width = 15, dpi = 300, units = "cm")


# Data in Lexis compatible format


all_inds_drvs %>% 
  mutate(
    year = wave + 1990
  ) %>% 
  filter(!is.na(sex) & !is.na(age) & !is.na(year) & !is.na(dlo)) %>% 
  select(sex, age, year, dlo) %>% 
  group_by(sex, age, year, dlo) %>% 
  tally %>% 
  spread(dlo, n) %>% 
  mutate(no = ifelse(is.na(no), 0, no),
         yes = ifelse(is.na(yes), 0, yes),
         prop_driving = yes / (yes + no)
  ) %>% 
  filter(age < 80 & age > 17) %>% 
  ggplot(.) +
  geom_tile(mapping=aes(x=year, y = age, fill = prop_driving)) + 
  scale_fill_gradientn(colours = rainbow(7)) + 
  theme_minimal() + 
  labs(
    title = "Level plot of proportions driving by age and year",
    x = "Year",
    y = "Age in years"
  )
ggsave("figures/levelplot_propdrive_overall.png", height = 25, width = 25, dpi = 300, units = "cm")




# Change in how the question is asked leads to increasing proportion stating
# they own a driving licence - once the question was asked that assumed 
# that people drive, a larger proportion stated that they drive 

# Some evidence that from the 2000s onwards, average age when most people 
# drive has increased, but too early to say whether this has contributed to a cohort
# more indifferent to driving.


all_inds_drvs %>% 
  mutate(
    year = wave + 1990
  ) %>% 
  filter(!is.na(sex) & !is.na(age) & !is.na(year) & !is.na(dlo)) %>% 
  select(sex, age, year, dlo) %>% 
  group_by(sex, age, year, dlo) %>% 
  tally %>% 
  spread(dlo, n) %>% 
  mutate(no = ifelse(is.na(no), 0, no),
         yes = ifelse(is.na(yes), 0, yes),
         prop_driving = yes / (yes + no)
  ) %>% 
  filter(age < 80 & age > 17) %>% 
  ggplot(.) +
  theme_minimal() + 
  geom_tile(mapping=aes(x=year, y = age, fill = prop_driving)) + 
  facet_wrap(~ sex) + 
  scale_fill_gradientn(colours = rainbow(7)) + 
  labs(
    title = "Proportion holding driving licence by sex, age and year",
    x = "Year",
    y = "Age in years"
  )
ggsave("figures/levelplot_propdrive_sex.png", height = 25, width = 25, dpi = 300, units = "cm")

# Cohort effects much stronger when looking at genders separately
# Suggestion that female cohorts learned to drive less from an 
# earlier birth cohort, perhaps from around 1978-9.
# For males, it may have been since around 1980-1982

all_inds_drvs %>% 
  mutate(
    year = wave + 1990
  ) %>% 
  filter(!is.na(sex) & !is.na(age) & !is.na(year) & !is.na(dlo) & !is.na(highqual)) %>% 
  select(highqual, sex, age, year, dlo) %>% 
  group_by(highqual, sex, age, year, dlo) %>% 
  tally %>% 
  spread(dlo, n) %>% 
  mutate(no = ifelse(is.na(no), 0, no),
         yes = ifelse(is.na(yes), 0, yes),
         prop_driving = yes / (yes + no)
  ) %>% 
  filter(age < 80 & age > 17) %>% 
  ggplot(.) +
  geom_tile(mapping=aes(x=year, y = age, fill = prop_driving)) + 
  facet_grid(highqual~ sex) + 
  scale_fill_gradientn(colours = rainbow(7)) + 
  labs(
    title = "Proportions holding licence by age and year, tiled by sex \nand highest educational qualification",
    x = "Year",
    y = "Age in years"
  )
# This seems very informative: very large qualifications effect 
# and sex*gender interaction. Possibly a recent cohoort effect 
ggsave("figures/levelplot_propdrive_sex_qual.png", height = 30, width = 30, dpi = 300, units = "cm")



# Something similar, but with GHQ score?
all_inds_drvs %>% 
  mutate(
    year = wave + 1990
  ) %>% 
  filter(!is.na(sex) & !is.na(age) & !is.na(year) & !is.na(ghq)) %>% 
  mutate(sad = ifelse(ghq > 0, "yes", "no")) %>% 
  select(sex, age, year, sad) %>% 
  group_by(sex, age, year, sad) %>%
  tally %>% spread(sad, n) %>% 
  mutate(
    no = ifelse(is.na(no), 0, no),
    yes = ifelse(is.na(yes), 0, yes),
    prop_sad = yes / (yes + no)
  ) %>%  
  filter(age < 80 & age > 17) %>% 
  ggplot(.) +
  geom_tile(mapping=aes(x=year, y = age, fill = prop_sad)) + 
  facet_wrap(~ sex) + 
  scale_fill_gradientn(colours = rainbow(7), limits = c(0, 1)) + 
  labs(
    title = "Proportion with GHQ scores > 0", 
    x = "Year",
    y = "Age in years"
  )

ggsave("figures/ghq_overall.png", height = 25, width = 25, units = "cm", dpi = 300)


all_inds_drvs %>% 
  mutate(
    year = wave + 1990
  ) %>% 
  filter(!is.na(sex) & !is.na(age) & !is.na(year) & !is.na(ghq)) %>% 
  mutate( sad = ifelse(ghq > 0, "yes", "no")) %>% 
  select(highqual, sex, age, year, sad) %>% 
  filter(!is.na(highqual)) %>% 
  group_by(highqual, sex, age, year, sad) %>%
  tally %>% spread(sad, n) %>% 
  mutate(
    no = ifelse(is.na(no), 0, no),
    yes = ifelse(is.na(yes), 0, yes),
    prop_sad = yes / (yes + no)
  ) %>%  
  filter(age < 80 & age > 17) %>% 
  ggplot(.) +
  geom_tile(mapping=aes(x=year, y = age, fill = prop_sad)) + 
  facet_grid(highqual ~ sex) + 
  scale_fill_gradientn(colours = rainbow(7), limits = c(0, 1)) + 
  labs(
    title = "Proportion with GHQ scores > 0, by sex and \nhighest educational qualification", 
    x = "Year",
    y = "Age in years"
  )

ggsave("figures/ghq_bysex_highqual.png", height = 30, width = 30, units = "cm", dpi = 300)


# Graphs - household  -----------------------------------------------------



# Proportion who own by age and qualification
all_inds_drvs %>% 
  mutate(
    year = wave + 1990
  ) %>% 
  filter(
    !is.na(sex) & !is.na(age) & 
      !is.na(year) & !is.na(ghq) & 
      !is.na(simpletenure) & !is.na(highqual)) %>% 
  mutate(
    homeowner = ifelse(simpletenure == "owner", "yes", "no")
  ) %>% 
  select(highqual, sex, age, year, homeowner) %>% 
  group_by(highqual, sex, age, year, homeowner) %>% 
  tally %>% 
  spread(homeowner, n) %>% 
  mutate(
    no = ifelse(is.na(no), 0, no),
    yes = ifelse(is.na(yes), 0, yes),
    prop_own = yes / (yes + no)
  ) %>% 
  filter(age < 80 & age >= 20) %>% 
  ggplot(.) +
  geom_tile(mapping=aes(x=year, y = age, fill = prop_own)) + 
  facet_grid(highqual ~ sex) + 
  scale_fill_gradientn(colours = rainbow(7), limits = c(0, 1)) + 
  labs(
    title = "Proportion of homeowners, by sex and \nhighest educational qualification", 
    x = "Year",
    y = "Age in years"
  )

ggsave("figures/homeown_bysex_highqual.png", height = 30, width = 30, units = "cm", dpi = 300)



# Proportion who are private renters, by age and qualification
all_inds_drvs %>% 
  mutate(
    year = wave + 1990
  ) %>% 
  filter(
    !is.na(sex) & !is.na(age) & 
      !is.na(year) & !is.na(ghq) & 
      !is.na(simpletenure) & !is.na(highqual)) %>% 
  mutate(
    privrenter = ifelse(simpletenure == "private renter", "yes", "no")
  ) %>% 
  select(highqual, sex, age, year, privrenter) %>% 
  group_by(highqual, sex, age, year, privrenter) %>% 
  tally %>% 
  spread(privrenter, n) %>% 
  mutate(
    no = ifelse(is.na(no), 0, no),
    yes = ifelse(is.na(yes), 0, yes),
    prop_privrent = yes / (yes + no)
  ) %>% 
  filter(age < 80 & age >= 20) %>% 
  ggplot(.) +
  geom_tile(mapping=aes(x=year, y = age, fill = prop_privrent)) + 
  facet_grid(highqual ~ sex) + 
  scale_fill_gradientn(colours = rainbow(7), limits = c(0, 1)) + 
  labs(
    title = "Proportion of private renters, by sex and \nhighest educational qualification", 
    x = "Year",
    y = "Age in years"
  )

ggsave("figures/privrent_bysex_highqual.png", height = 30, width = 30, units = "cm", dpi = 300)


# Proportion who are social renters, by age and qualification
all_inds_drvs %>% 
  mutate(
    year = wave + 1990
  ) %>% 
  filter(
    !is.na(sex) & !is.na(age) & 
      !is.na(year) & !is.na(ghq) & 
      !is.na(simpletenure) & !is.na(highqual)) %>% 
  mutate(
    socrenter = ifelse(simpletenure == "social renter", "yes", "no")
  ) %>% 
  select(highqual, sex, age, year, socrenter) %>% 
  group_by(highqual, sex, age, year, socrenter) %>% 
  tally %>% 
  spread(socrenter, n) %>% 
  mutate(
    no = ifelse(is.na(no), 0, no),
    yes = ifelse(is.na(yes), 0, yes),
    prop_socrenter = yes / (yes + no)
  ) %>% 
  filter(age < 80 & age >= 20) %>% 
  ggplot(.) +
  geom_tile(mapping=aes(x=year, y = age, fill = prop_socrenter)) + 
  facet_grid(highqual ~ sex) + 
  scale_fill_gradientn(colours = rainbow(7), limits = c(0, 1)) + 
  labs(
    title = "Proportion of social renters, by sex and \nhighest educational qualification", 
    x = "Year",
    y = "Age in years"
  )

ggsave("figures/socialrent_bysex_highqual.png", height = 30, width = 30, units = "cm", dpi = 300)


#  Lexis surface of urban rural classification by sex

all_inds_drvs %>%
  filter(region !="wales" & region != "scotland") %>% 
  mutate(
    year = wave + 1990
  ) %>% 
  filter(!is.na(sex) & !is.na(age) & !is.na(year) & !is.na(dlo) & !is.na(ur_group)) %>% 
  select(ur_group, sex, age, year, dlo) %>% 
  group_by(ur_group, sex, age, year, dlo) %>% 
  tally %>% 
  spread(dlo, n) %>% 
  mutate(no = ifelse(is.na(no), 0, no),
         yes = ifelse(is.na(yes), 0, yes),
         prop_driving = yes / (yes + no)
  ) %>% 
  filter(age < 80 & age > 17) %>% 
  ggplot(.) +
  geom_tile(mapping=aes(x=year, y = age, fill = prop_driving)) + 
  facet_grid(ur_group ~ sex) + 
  scale_fill_gradientn(colours = rainbow(7)) + 
  labs(
    title = "Proportions holding licence by age and year, tiled by sex \nand urban rural classification",
    x = "Year",
    y = "Age in years"
  )

ggsave("figures/drivelicence_bysex_ur_group.png", height = 30, width = 30, units = "cm", dpi = 300)
# Rural seems too sparse to be worth including
# Gender differences, especially for urban, seem even stronger in this case

# As before, but excluding rural category
all_inds_drvs %>% 
  mutate(
    year = wave + 1990
  ) %>% 
  filter(!is.na(sex) & !is.na(age) & !is.na(year) & !is.na(dlo) & !is.na(ur_group)) %>%
  filter(ur_group != "rural") %>% 
  select(ur_group, sex, age, year, dlo) %>% 
  group_by(ur_group, sex, age, year, dlo) %>% 
  tally %>% 
  spread(dlo, n) %>% 
  mutate(no = ifelse(is.na(no), 0, no),
         yes = ifelse(is.na(yes), 0, yes),
         prop_driving = yes / (yes + no)
  ) %>% 
  filter(age < 80 & age > 17) %>% 
  ggplot(.) +
  geom_tile(mapping=aes(x=year, y = age, fill = prop_driving)) + 
  facet_grid(ur_group ~ sex) + 
  scale_fill_gradientn(colours = rainbow(7)) + 
  labs(
    title = "Proportions holding licence by age and year, tiled by sex \nand urban rural classification",
    x = "Year",
    y = "Age in years"
  )

ggsave("figures/drivelicence_bysex_urban_suburban_group.png", height = 30, width = 30, units = "cm", dpi = 300)


# urban/suburban by educational qualification, males only 

all_inds_drvs %>% 
  mutate(
    year = wave + 1990
  ) %>% 
  filter(!is.na(sex) & !is.na(age) & !is.na(year) & 
           !is.na(dlo) & !is.na(highqual) & !is.na(ur_group)
  ) %>%
  filter(ur_group != "rural" & sex == "male") %>% 
  select(ur_group, highqual, age, year, dlo) %>% 
  group_by(ur_group, highqual, age, year, dlo) %>% 
  tally %>% 
  spread(dlo, n) %>% 
  mutate(no = ifelse(is.na(no), 0, no),
         yes = ifelse(is.na(yes), 0, yes),
         prop_driving = yes / (yes + no)
  ) %>% 
  filter(age < 80 & age > 17) %>% 
  ggplot(.) +
  geom_tile(mapping=aes(x=year, y = age, fill = prop_driving)) + 
  facet_grid(ur_group ~ highqual) + 
  scale_fill_gradientn(colours = rainbow(7)) + 
  labs(
    title = "Proportions of males holding licence by age and year, 
    tiled by highest qualification \nand urban rural classification",
    x = "Year",
    y = "Age in years"
  )

ggsave("figures/drivelicence_males_urgroup_by_highqual.png", height = 30, width = 30, units = "cm", dpi = 300)

# urban/suburban by educational qualification, females only 

all_inds_drvs %>% 
  mutate(
    year = wave + 1990
  ) %>% 
  filter(!is.na(sex) & !is.na(age) & !is.na(year) & 
           !is.na(dlo) & !is.na(highqual) & !is.na(ur_group)
  ) %>%
  filter(ur_group != "rural" & sex == "female") %>% 
  select(ur_group, highqual, age, year, dlo) %>% 
  group_by(ur_group, highqual, age, year, dlo) %>% 
  tally %>% 
  spread(dlo, n) %>% 
  mutate(no = ifelse(is.na(no), 0, no),
         yes = ifelse(is.na(yes), 0, yes),
         prop_driving = yes / (yes + no)
  ) %>% 
  filter(age < 80 & age > 17) %>% 
  ggplot(.) +
  geom_tile(mapping=aes(x=year, y = age, fill = prop_driving)) + 
  facet_grid(ur_group ~ highqual) + 
  scale_fill_gradientn(colours = rainbow(7)) + 
  labs(
    title = 
      "Proportions of females holding licence by age and year, tiled by 
    highest qualification and urban rural classification",
    x = "Year",
    y = "Age in years"
  )

ggsave("figures/drivelicence_females_urgroup_by_highqual.png", height = 30, width = 30, units = "cm", dpi = 300)



# driving by tertile of household income before housings costs 

all_inds_drvs %>% 
  mutate(
    year = wave + 1990
  ) %>% 
  filter(!is.na(sex) & !is.na(age) & !is.na(year) & 
           !is.na(dlo) & !is.na(hh_income_before_hcosts)
  ) %>%
  rename(hhincome = hh_income_before_hcosts) %>% 
  select(hhincome, highqual, sex, age, year, dlo) %>%
  mutate(hhincome_tertile = ntile(hhincome, 3)) %>% 
  group_by(hhincome_tertile, sex, age, year, dlo) %>%
  tally %>% 
  spread(dlo, n) %>% 
  mutate(no = ifelse(is.na(no), 0, no),
         yes = ifelse(is.na(yes), 0, yes),
         prop_driving = yes / (yes + no)
  ) %>% 
  filter(age < 80 & age > 17) %>% 
  ggplot(.) +
  geom_tile(mapping=aes(x=year, y = age, fill = prop_driving)) + 
  facet_grid(hhincome_tertile ~ sex) + 
  scale_fill_gradientn(colours = rainbow(7)) + 
  labs(
    title = 
      "Driving licence holders by household income tertile and sex",
    x = "Year",
    y = "Age in years"
  )

# tertile of household income after housing costs 
all_inds_drvs %>% 
  mutate(
    year = wave + 1990
  ) %>% 
  filter(!is.na(sex) & !is.na(age) & !is.na(year) & 
           !is.na(dlo) & !is.na(hh_income_after_hcosts)
  ) %>%
  rename(hhincome = hh_income_after_hcosts) %>% 
  select(hhincome, highqual, sex, age, year, dlo) %>%
  mutate(hhincome_tertile = ntile(hhincome, 3)) %>% 
  group_by(hhincome_tertile, sex, age, year, dlo) %>%
  tally %>% 
  spread(dlo, n) %>% 
  mutate(no = ifelse(is.na(no), 0, no),
         yes = ifelse(is.na(yes), 0, yes),
         prop_driving = yes / (yes + no)
  ) %>% 
  filter(age < 80 & age > 17) %>% 
  ggplot(.) +
  geom_tile(mapping=aes(x=year, y = age, fill = prop_driving)) + 
  facet_grid(hhincome_tertile ~ sex) + 
  scale_fill_gradientn(colours = rainbow(7)) + 
  labs(
    title = 
      "Driving licence holders by household income tertile and sex",
    x = "Year",
    y = "Age in years"
  )
# Income after housing costs seems less reliable/fewer observations
# Better to use before housing costs, but substantively there is little difference


# males only, tertile of household income by highest educational qualification
all_inds_drvs %>% 
  mutate(
    year = wave + 1990
  ) %>% 
  filter(!is.na(sex) & !is.na(age) & !is.na(year) & 
           !is.na(dlo) & !is.na(hh_income_before_hcosts) & 
           !is.na(highqual)
  ) %>%
  filter(sex =="male") %>% 
  rename(hhincome = hh_income_after_hcosts) %>% 
  select(hhincome, highqual, age, year, dlo) %>%
  mutate(hhincome_tertile = ntile(hhincome, 3)) %>% 
  group_by(hhincome_tertile, highqual, age, year, dlo) %>%
  tally %>% 
  spread(dlo, n) %>% 
  mutate(no = ifelse(is.na(no), 0, no),
         yes = ifelse(is.na(yes), 0, yes),
         prop_driving = yes / (yes + no)
  ) %>% 
  filter(age < 80 & age > 17) %>% 
  ggplot(.) +
  geom_tile(mapping=aes(x=year, y = age, fill = prop_driving)) + 
  facet_grid(hhincome_tertile ~ highqual) + 
  scale_fill_gradientn(colours = rainbow(7)) + 
  labs(
    title = 
      "male: driving licence holders by household income tertile and highest qualification",
    x = "Year",
    y = "Age in years"
  )

# females only, tertile of household income by highest educational qualification
all_inds_drvs %>% 
  mutate(
    year = wave + 1990
  ) %>% 
  filter(!is.na(sex) & !is.na(age) & !is.na(year) & 
           !is.na(dlo) & !is.na(hh_income_before_hcosts) & 
           !is.na(highqual)
  ) %>%
  filter(sex =="female") %>% 
  rename(hhincome = hh_income_after_hcosts) %>% 
  select(hhincome, highqual, age, year, dlo) %>%
  mutate(hhincome_tertile = ntile(hhincome, 3)) %>% 
  group_by(hhincome_tertile, highqual, age, year, dlo) %>%
  tally %>% 
  spread(dlo, n) %>% 
  mutate(no = ifelse(is.na(no), 0, no),
         yes = ifelse(is.na(yes), 0, yes),
         prop_driving = yes / (yes + no)
  ) %>% 
  filter(age < 80 & age > 17) %>% 
  ggplot(.) +
  geom_tile(mapping=aes(x=year, y = age, fill = prop_driving)) + 
  facet_grid(hhincome_tertile ~ highqual) + 
  scale_fill_gradientn(colours = rainbow(7)) + 
  labs(
    title = 
      "female: driving licence holders by household income tertile and highest qualification",
    x = "Year",
    y = "Age in years"
  )

