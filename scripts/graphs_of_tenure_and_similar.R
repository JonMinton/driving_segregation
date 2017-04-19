


# Graphs - household  -----------------------------------------------------

all_inds_drvs <- read_csv("data/derived/bhps_driver.csv")

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


# Urban/nonurban differences by qualification

all_inds_drvs %>%
  filter(region !="wales" & region != "scotland") %>% 
  mutate(
    year = wave + 1990
  ) %>% 
  filter(!is.na(age) & !is.na(highqual) & !is.na(year) & !is.na(dlo) & !is.na(ur_group)) %>% 
  select(highqual, ur_group, age, year, dlo) %>% 
  group_by(highqual, ur_group, age, year, dlo) %>% 
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
    title = "Proportions holding licence by age and year, tiled by highest qual \nand urban rural classification",
    x = "Year",
    y = "Age in years"
  )

# males only 
all_inds_drvs %>%
  filter(region !="wales" & region != "scotland") %>%
  filter(sex == "male") %>% 
  mutate(
    year = wave + 1990
  ) %>% 
  filter(!is.na(age) & !is.na(highqual) & !is.na(year) & !is.na(dlo) & !is.na(ur_group)) %>% 
  select(highqual, ur_group, age, year, dlo) %>% 
  group_by(highqual, ur_group, age, year, dlo) %>% 
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
    title = "Proportions holding licence by age and year, tiled by highest qual \nand urban rural classification, males only",
    x = "Year",
    y = "Age in years"
  )

# females only 
all_inds_drvs %>%
  filter(region !="wales" & region != "scotland") %>%
  filter(sex == "female") %>% 
  mutate(
    year = wave + 1990
  ) %>% 
  filter(!is.na(age) & !is.na(highqual) & !is.na(year) & !is.na(dlo) & !is.na(ur_group)) %>% 
  select(highqual, ur_group, age, year, dlo) %>% 
  group_by(highqual, ur_group, age, year, dlo) %>% 
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
    title = "Proportions holding licence by age and year, tiled by highest qual \nand urban rural classification, females only",
    x = "Year",
    y = "Age in years"
  )

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




# Lexis plot of mean number of children in household by age, year  --------

all_inds_drvs %>% 
  filter(!is.na(num_children)) %>% 
  mutate(year = wave + 1991) %>% 
  select(year, age, num_children) %>% 
  group_by(year, age) %>% 
  summarise(mean_num_children = mean(num_children)) %>% 
  filter(age < 80 & age > 17) %>% 
  ggplot(.) +
  geom_tile(mapping=aes(x=year, y = age, fill = mean_num_children)) + 
  scale_fill_gradientn(colours = rainbow(7)) + 
  labs(
    title = 
      "mean number of children in household by age and year",
    x = "Year",
    y = "Age in years"
  )


# Proportion of households with no children
all_inds_drvs %>% 
  filter(!is.na(num_children)) %>% 
  mutate(year = wave + 1990) %>% 
  select(year, age, num_children) %>%
  mutate(has_child = ifelse(num_children > 0, 1, 0)) %>% 
  select(year, age, has_child) %>% 
  group_by(year, age, has_child) %>% 
  tally %>% 
  spread(has_child, n, fill = 0) %>% 
  
  mutate(prop_with_child = `1` / (`1` + `0`)) %>% 
  ggplot(.) +
  geom_tile(mapping=aes(x=year, y = age, fill = prop_with_child)) + 
  scale_fill_gradientn(colours = rainbow(7)) + 
  labs(
    title = 
      "proportion of households with children",
    x = "Year",
    y = "Age in years"
  )


# driving licence ownership by whether household contains a child
tmp1 <- all_inds_drvs %>% 
  filter(!is.na(num_children)) %>% 
  mutate(year = wave + 1990) %>% 
  select(year, age, dlo, num_children) %>%
  mutate(has_child = ifelse(num_children > 0, "yes", "no")) %>% 
  select(year, age, dlo, has_child) %>% 
  group_by(age, year, has_child) %>% 
  tally %>% 
  spread(has_child, n, fill = 0) %>%
  mutate(prop_with_child = yes / (yes + no)) %>% 
  select(age, year, prop_with_child)


tmp2 <- all_inds_drvs %>% 
  filter(!is.na(dlo)) %>% 
  mutate(year = wave + 1990) %>% 
  select(year, age, dlo) %>% 
  group_by(year, age, dlo) %>% 
  tally %>% 
  spread(dlo, n, fill = 0) %>% 
  mutate(prop_with_dl = yes / (yes + no)) %>% 
  select(year, age, prop_with_dl)


tmp3 <- tmp1 %>% inner_join(tmp2)

tmp3 %>%  
  gather(variable, value, prop_with_child:prop_with_dl) %>% 
  ggplot(.) +
  geom_tile(mapping=aes(x=year, y = age, fill = value)) +
  facet_wrap(~variable) +
  scale_fill_gradientn(colours = rainbow(7)) + 
  labs(
    title = 
      "dlos by whether household includes a child",
    x = "Year",
    y = "Age in years"
  )

# This is fairly informative. Note especially the falling levels of households 
# with a child amongst under 25s

# Let's look at years 1995, 2000, 2005 and 2008

tmp3 %>% 
  filter(year %in% c(1995, 2000, 2005, 2008)) %>% 
  ggplot(., aes(x = age, y = prop_with_child, group = factor(year), colour = factor(year))) + 
  geom_line()

tmp3 %>% 
  filter(year %in% c(1995, 2000, 2005, 2008)) %>% 
  filter(age < 40) %>% 
  ggplot(., aes(x = age, y = prop_with_child, group = factor(year), colour = factor(year))) + 
  geom_line()



tmp1 <-   group_by(highqual, age, year, dlo) %>%
  tally %>% 
  spread(dlo, n) %>% 
  mutate(no = ifelse(is.na(no), 0, no),
         yes = ifelse(is.na(yes), 0, yes),
         prop_driving = yes / (yes + no)
  ) %>% 
  
  
  
  
  
  group_by(year, age) %>% 
  
  summarise(mean_num_children = mean(num_children)) %>% 
  filter(age < 80 & age > 17) %>% 
  ggplot(.) +
  geom_tile(mapping=aes(x=year, y = age, fill = mean_num_children)) + 
  scale_fill_gradientn(colours = rainbow(7)) + 
  labs(
    title = 
      "mean number of children in household by age and year",
    x = "Year",
    y = "Age in years"
  )



#Tenure levelplots 
png(filename = "figures/tenure_overall_prop private renters - by age, year, sex.png", width = 18, height = 35, units = "cm", res = 300)

all_inds_drvs %>% 
  mutate(
    year = wave + 1990
  ) %>% 
  filter(!is.na(sex) & !is.na(age) & !is.na(year) & !is.na(simpletenure)) %>% 
  select(age, year, sex, simpletenure) %>% 
  group_by(age, year, sex) %>% 
  filter(age <= 80) %>% 
  summarise(prop_rent = length(simpletenure[simpletenure == "private renter"]) / length(simpletenure)) %>% 
  levelplot(
    prop_rent ~ year * age | sex, 
    data=., 
    aspect = "iso",
    strip=strip.custom(par.strip.text=list(cex=1.4, fontface="bold"), bg="grey"),
    ylab=list(label="Age in years", cex=1.4),
    xlab=list(label="Year", cex=1.4),
    cex=1.4,
    col.regions= rev(colorRampPalette(brewer.pal(12, "Paired"))(200)),
    main=NULL,
    colorkey = list(labels= list(cex = 1.4)),
    at = seq(0, 1, by = 0.02),
    labels=list(cex=1.2),
    col="black",
    scales=list(
      x=list(cex=1.1, rot = 90), 
      y=list(cex=1.1),
      alternating=3
    )
  ) 
dev.off()

#Tenure levelplots 
png(filename = "figures/tenure_overall_prop home owners - by age, year, sex.png", width = 18, height = 35, units = "cm", res = 300)

all_inds_drvs %>% 
  mutate(
    year = wave + 1990
  ) %>% 
  filter(!is.na(sex) & !is.na(age) & !is.na(year) & !is.na(simpletenure)) %>% 
  select(age, year, sex, simpletenure) %>% 
  group_by(age, year, sex) %>% 
  filter(age <= 80) %>% 
  summarise(prop_rent = length(simpletenure[simpletenure == "owner"]) / length(simpletenure)) %>% 
  levelplot(
    prop_rent ~ year * age | sex, 
    data=., 
    aspect = "iso",
    strip=strip.custom(par.strip.text=list(cex=1.4, fontface="bold"), bg="grey"),
    ylab=list(label="Age in years", cex=1.4),
    xlab=list(label="Year", cex=1.4),
    cex=1.4,
    col.regions= rev(colorRampPalette(brewer.pal(12, "Paired"))(200)),
    main=NULL,
    colorkey = list(labels= list(cex = 1.4)),
    at = seq(0, 1, by = 0.02),
    labels=list(cex=1.2),
    col="black",
    scales=list(
      x=list(cex=1.1, rot = 90), 
      y=list(cex=1.1),
      alternating=3
    )
  ) 
dev.off()

png(filename = "figures/tenure_overall_prop social renters - by age, year, sex.png", width = 18, height = 35, units = "cm", res = 300)

all_inds_drvs %>% 
  mutate(
    year = wave + 1990
  ) %>% 
  filter(!is.na(sex) & !is.na(age) & !is.na(year) & !is.na(simpletenure)) %>% 
  select(age, year, sex, simpletenure) %>% 
  group_by(age, year, sex) %>% 
  filter(age <= 80) %>% 
  summarise(prop_rent = length(simpletenure[simpletenure == "social renter"]) / length(simpletenure)) %>% 
  levelplot(
    prop_rent ~ year * age | sex, 
    data=., 
    aspect = "iso",
    strip=strip.custom(par.strip.text=list(cex=1.4, fontface="bold"), bg="grey"),
    ylab=list(label="Age in years", cex=1.4),
    xlab=list(label="Year", cex=1.4),
    cex=1.4,
    col.regions= rev(colorRampPalette(brewer.pal(12, "Paired"))(200)),
    main=NULL,
    colorkey = list(labels= list(cex = 1.4)),
    at = seq(0, 1, by = 0.02),
    labels=list(cex=1.2),
    col="black",
    scales=list(
      x=list(cex=1.1, rot = 90), 
      y=list(cex=1.1),
      alternating=3
    )
  ) 
dev.off()



# Graphs of tenure --------------------------------------------------------


png(filename = "figures/tenure_overall_prop social renters - by age, year, sex, highqual.png", width = 18, height = 35, units = "cm", res = 300)

all_inds_drvs %>% 
  mutate(
    year = wave + 1990
  ) %>% 
  filter(!is.na(sex) & !is.na(age) & !is.na(year) & 
           !is.na(simpletenure) & 
           !is.na(highqual)
  ) %>% 
  mutate(highqual = recode(
    highqual, 
    "
    'further non-vocational' = 'High';
    'further vocational' = 'Med';
    'no further' = 'Low'
    ",
    levels = c("Low", "Med", "High"), as.factor.result =T
  )) %>% 
  select(age, year, sex, highqual, simpletenure) %>% 
  group_by(age, year, sex, highqual) %>% 
  filter(age <= 80) %>% 
  summarise(prop_rent = length(simpletenure[simpletenure == "social renter"]) / length(simpletenure)) %>% 
  levelplot(
    prop_rent ~ year * age | sex * highqual, 
    data=., 
    aspect = "iso",
    strip=strip.custom(par.strip.text=list(cex=1.4, fontface="bold"), bg="grey"),
    ylab=list(label="Age in years", cex=1.4),
    xlab=list(label="Year", cex=1.4),
    cex=1.4,
    col.regions= rev(colorRampPalette(brewer.pal(12, "Paired"))(200)),
    main=NULL,
    colorkey = list(labels= list(cex = 1.4)),
    at = seq(0, 1, by = 0.02),
    labels=list(cex=1.2),
    col="black",
    scales=list(
      x=list(cex=1.1, rot = 90), 
      y=list(cex=1.1),
      alternating=3
    )
  ) 
dev.off()



png(filename = "figures/tenure_overall_prop private renters - by age, year, sex, highqual.png", width = 18, height = 35, units = "cm", res = 300)

all_inds_drvs %>% 
  mutate(
    year = wave + 1990
  ) %>% 
  filter(!is.na(sex) & !is.na(age) & !is.na(year) & 
           !is.na(simpletenure) & 
           !is.na(highqual)
  ) %>% 
  mutate(highqual = recode(
    highqual, 
    "
    'further non-vocational' = 'High';
    'further vocational' = 'Med';
    'no further' = 'Low'
    ",
    levels = c("Low", "Med", "High"), as.factor.result =T
  )) %>% 
  select(age, year, sex, highqual, simpletenure) %>% 
  group_by(age, year, sex, highqual) %>% 
  filter(age <= 80) %>% 
  summarise(prop_rent = length(simpletenure[simpletenure == "private renter"]) / length(simpletenure)) %>% 
  levelplot(
    prop_rent ~ year * age | sex * highqual, 
    data=., 
    aspect = "iso",
    strip=strip.custom(par.strip.text=list(cex=1.4, fontface="bold"), bg="grey"),
    ylab=list(label="Age in years", cex=1.4),
    xlab=list(label="Year", cex=1.4),
    cex=1.4,
    col.regions= rev(colorRampPalette(brewer.pal(12, "Paired"))(200)),
    main=NULL,
    colorkey = list(labels= list(cex = 1.4)),
    at = seq(0, 1, by = 0.02),
    labels=list(cex=1.2),
    col="black",
    scales=list(
      x=list(cex=1.1, rot = 90), 
      y=list(cex=1.1),
      alternating=3
    )
  ) 
dev.off()


png(filename = "figures/tenure_overall_prop home owners - by age, year, sex, highqual.png", width = 18, height = 35, units = "cm", res = 300)

all_inds_drvs %>% 
  mutate(
    year = wave + 1990
  ) %>% 
  filter(!is.na(sex) & !is.na(age) & !is.na(year) & 
           !is.na(simpletenure) & 
           !is.na(highqual)
  ) %>% 
  mutate(highqual = recode(
    highqual, 
    "
    'further non-vocational' = 'High';
    'further vocational' = 'Med';
    'no further' = 'Low'
    ",
    levels = c("Low", "Med", "High"), as.factor.result =T
  )) %>% 
  select(age, year, sex, highqual, simpletenure) %>% 
  group_by(age, year, sex, highqual) %>% 
  filter(age <= 80) %>% 
  summarise(prop_rent = length(simpletenure[simpletenure == "owner"]) / length(simpletenure)) %>% 
  levelplot(
    prop_rent ~ year * age | sex * highqual, 
    data=., 
    aspect = "iso",
    strip=strip.custom(par.strip.text=list(cex=1.4, fontface="bold"), bg="grey"),
    ylab=list(label="Age in years", cex=1.4),
    xlab=list(label="Year", cex=1.4),
    cex=1.4,
    col.regions= rev(colorRampPalette(brewer.pal(12, "Paired"))(200)),
    main=NULL,
    colorkey = list(labels= list(cex = 1.4)),
    at = seq(0, 1, by = 0.02),
    labels=list(cex=1.2),
    col="black",
    scales=list(
      x=list(cex=1.1, rot = 90), 
      y=list(cex=1.1),
      alternating=3
    )
  ) 
dev.off()


# Gender seems less important than for the earlier graphs, so instead I'm going to aggregate


png(filename = "figures/tenure_overall_prop social renters - by age, year, highqual.png", width = 40, height = 40, units = "cm", res = 300)

all_inds_drvs %>% 
  mutate(
    year = wave + 1990
  ) %>% 
  filter(!is.na(sex) & !is.na(age) & !is.na(year) & 
           !is.na(simpletenure) & 
           !is.na(highqual)
  ) %>% 
  mutate(highqual = recode(
    highqual, 
    "
    'further non-vocational' = 'High';
    'further vocational' = 'Med';
    'no further' = 'Low'
    ",
    levels = c("Low", "Med", "High"), as.factor.result =T
  )) %>% 
  select(age, year, highqual, simpletenure) %>% 
  group_by(age, year, highqual) %>% 
  filter(age <= 80) %>% 
  summarise(prop_rent = length(simpletenure[simpletenure == "social renter"]) / length(simpletenure)) %>% 
  levelplot(
    prop_rent ~ year * age | highqual, 
    data=., 
    aspect = "iso",
    strip=strip.custom(par.strip.text=list(cex=1.4, fontface="bold"), bg="grey"),
    ylab=list(label="Age in years", cex=1.4),
    xlab=list(label="Year", cex=1.4),
    cex=1.4,
    col.regions= rev(colorRampPalette(brewer.pal(12, "Paired"))(200)),
    main=NULL,
    colorkey = list(labels= list(cex = 1.4)),
    at = seq(0, 1, by = 0.02),
    labels=list(cex=1.2),
    col="black",
    scales=list(
      x=list(cex=1.1, rot = 90), 
      y=list(cex=1.1),
      alternating=3
    )
  ) 
dev.off()



png(filename = "figures/tenure_overall_prop private renters - by age, year, highqual.png", width = 40, height = 40, units = "cm", res = 300)

all_inds_drvs %>% 
  mutate(
    year = wave + 1990
  ) %>% 
  filter(!is.na(age) & !is.na(year) & 
           !is.na(simpletenure) & 
           !is.na(highqual)
  ) %>% 
  mutate(highqual = recode(
    highqual, 
    "
    'further non-vocational' = 'High';
    'further vocational' = 'Med';
    'no further' = 'Low'
    ",
    levels = c("Low", "Med", "High"), as.factor.result =T
  )) %>% 
  select(age, year, highqual, simpletenure) %>% 
  group_by(age, year, highqual) %>% 
  filter(age <= 80) %>% 
  summarise(prop_rent = length(simpletenure[simpletenure == "private renter"]) / length(simpletenure)) %>% 
  levelplot(
    prop_rent ~ year * age | highqual, 
    data=., 
    aspect = "iso",
    strip=strip.custom(par.strip.text=list(cex=1.4, fontface="bold"), bg="grey"),
    ylab=list(label="Age in years", cex=1.4),
    xlab=list(label="Year", cex=1.4),
    cex=1.4,
    col.regions= rev(colorRampPalette(brewer.pal(12, "Paired"))(200)),
    main=NULL,
    colorkey = list(labels= list(cex = 1.4)),
    at = seq(0, 1, by = 0.02),
    labels=list(cex=1.2),
    col="black",
    scales=list(
      x=list(cex=1.1, rot = 90), 
      y=list(cex=1.1),
      alternating=3
    )
  ) 
dev.off()


png(filename = "figures/tenure_overall_prop home owners - by age, year, highqual.png", width = 40, height = 40, units = "cm", res = 300)

all_inds_drvs %>% 
  mutate(
    year = wave + 1990
  ) %>% 
  filter(!is.na(age) & !is.na(year) & 
           !is.na(simpletenure) & 
           !is.na(highqual)
  ) %>% 
  mutate(highqual = recode(
    highqual, 
    "
    'further non-vocational' = 'High';
    'further vocational' = 'Med';
    'no further' = 'Low'
    ",
    levels = c("Low", "Med", "High"), as.factor.result =T
  )) %>% 
  select(age, year, highqual, simpletenure) %>% 
  group_by(age, year, highqual) %>% 
  filter(age <= 80) %>% 
  summarise(prop_rent = length(simpletenure[simpletenure == "owner"]) / length(simpletenure)) %>% 
  levelplot(
    prop_rent ~ year * age | highqual, 
    data=., 
    aspect = "iso",
    strip=strip.custom(par.strip.text=list(cex=1.4, fontface="bold"), bg="grey"),
    ylab=list(label="Age in years", cex=1.4),
    xlab=list(label="Year", cex=1.4),
    cex=1.4,
    col.regions= rev(colorRampPalette(brewer.pal(12, "Paired"))(200)),
    main=NULL,
    colorkey = list(labels= list(cex = 1.4)),
    at = seq(0, 1, by = 0.02),
    labels=list(cex=1.2),
    col="black",
    scales=list(
      x=list(cex=1.1, rot = 90), 
      y=list(cex=1.1),
      alternating=3
    )
  ) 
dev.off()


# Household composition ---------------------------------------------------


# A straight division into one, two, or more people households


