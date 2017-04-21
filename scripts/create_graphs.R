

# all_inds_drvs <- read_csv("data/derived/bhps_plus_usosm.csv")

all_inds_drvs <- read_csv("data/derived/bhps_driver.csv")

# Graphs to produce 

# Standard graphs 

# 01 - proportion of dlos  - by sex, age group, and period
# 02 - proportion of dlos  - by sex, age, urban rural class, and two periods
# 03 - proportion of dlos  - by sex, age, highqual, and two periods
# 04 - proportion of drivers driving - by sex, age, and period 
# 05 - proportion of drivers driving - by sex, age, urban rural class, and two periods 
# 06 - proportion of drivers driving - by sex, age, highest qual, and two periods 

# Levelplots 

# 07 - proportion of dlos  - by age, year, sex
# 08 - proportion of dlos  - by age, year, sex, urclass
# 09 - proportion of dlos  - by age, year, sex, highqual 
# 10 - proportion of drivers driving - by age, year, sex
# 11 - proportion of drivers driving - by age, year, sex, urclass
# 12 - proportion of drivers driving - by age, year, sex, highqual


# Standard plots  ---------------------------------------------------------



# Graphs to produce 

# Standard graphs 

# 01 - proportion of dlos  - by sex, age group, and period

all_inds_drvs %>% 
  mutate(
    year = wave + 1990
  ) %>% 
  mutate(
    age_grp = 
      cut(
        age, 
        c(0, 20, 25, 35, 50, 60, 70, 999), 
        labels = c("<20", "20-25", "26-35", "36-50", "51-60", "61-70", ">70")
      )
  ) %>% 
  filter(!is.na(sex) & !is.na(age) & !is.na(year) & !is.na(dlo)) %>% 
  filter(year %in% c(1995, 2000, 2005, 2010)) %>% # Something clearly not right
  select(sex, age_grp, year,  dlo) %>% 
  group_by(sex, age_grp, year, dlo) %>% 
  tally %>% 
  spread(dlo, n, fill = 0) %>% 
  mutate(
    prop_dlo = yes / (yes + no),
    se_dlo = (prop_dlo * (1 - prop_dlo) / (yes + no)) ^ 0.5,
    sex_age = paste(sex, age_grp, sep = "_")
  ) %>% 
  ggplot(., aes(
    x = age_grp, 
    y = prop_dlo, group = factor(year), colour = factor(year), shape = factor(year))) +
  geom_line() + geom_point(aes(shape = factor(year)), size = 3) + 
  geom_linerange(
    aes(ymax = prop_dlo + 2 * se_dlo, ymin = prop_dlo - 2 * se_dlo)
  ) + 
  labs(
    x = "Age Group", 
    y = "Proportion with driving licence", 
    colour = "Year", group = "Year", shape = "Year") + 
  scale_y_continuous(limits = c(0, 1)) + 
  facet_wrap(~sex)

ggsave("figures/final_deck/01_- proportion of dlos  - by sex, age group, and period.png", width = 25, height =20, units = "cm", dpi = 300)

# 02 - proportion of dlos  - by sex, age, urban rural class, and two periods
all_inds_drvs %>% 
  mutate(
    year = wave + 1990
  ) %>% 
  mutate(
    age_grp = 
      cut(
        age, 
        c(0, 20, 25, 35, 50, 60, 70, 999), 
        labels = c("<20", "20-25", "26-35", "36-50", "51-60", "61-70", ">70")
      )
  ) %>% 
  filter(!is.na(sex) & !is.na(age) & !is.na(year) & !is.na(dlo) & !is.na(ur_group)) %>% 
  filter(year %in% c(1995, 2005)) %>%
  select(sex, ur_group, age_grp, year,  dlo) %>% 
  group_by(sex, ur_group, age_grp, year, dlo) %>% 
  tally %>% 
  spread(dlo, n, fill = 0) %>% 
  mutate(
    prop_dlo = yes / (yes + no),
    se_dlo = (prop_dlo * (1 - prop_dlo) / (yes + no)) ^ 0.5,
    ur_year = paste(ur_group, year, sep = "_")
  ) %>% 
  ggplot(., aes(
    x = age_grp, 
    y = prop_dlo, group = ur_year, colour = ur_group, shape = ur_group)) +
  geom_line(aes(linetype = factor(year))) + geom_point(size = 3) + 
  geom_linerange(
    aes(ymax = prop_dlo + 2 * se_dlo, ymin = prop_dlo - 2 * se_dlo)
  ) + 
  labs(
    x = "Age Group", 
    y = "Proportion with driving licence", 
    colour = "Urban Rural Class", shape = "Urban Rural Class", linetype = "Year") + 
  scale_y_continuous(limits = c(0, 1)) + 
  facet_wrap(~sex)

ggsave("figures/final_deck/02 - proportion of dlos  - by sex, age, urban rural class, and two periods.png", width = 25, height =20, units = "cm", dpi = 300)

# 03 - proportion of dlos  - by sex, age, highqual, and two periods

all_inds_drvs %>% 
  mutate(
    year = wave + 1990
  ) %>% 
  mutate(
    age_grp = 
      cut(
        age, 
        c(0, 20, 25, 35, 50, 60, 70, 999), 
        labels = c("<20", "20-25", "26-35", "36-50", "51-60", "61-70", ">70")
      )
  ) %>% 
  filter(!is.na(sex) & !is.na(age) & !is.na(year) & !is.na(dlo) & !is.na(highqual)) %>% 
  filter(year %in% c(1995, 2005)) %>%
  select(sex, highqual, age_grp, year,  dlo) %>% 
  group_by(sex, highqual, age_grp, year, dlo) %>% 
  tally %>% 
  spread(dlo, n, fill = 0) %>% 
  mutate(
    prop_dlo = yes / (yes + no),
    se_dlo = (prop_dlo * (1 - prop_dlo) / (yes + no)) ^ 0.5,
    hq_year = paste(highqual, year, sep = "_")
  ) %>% 
  ggplot(., aes(
    x = age_grp, 
    y = prop_dlo, group = hq_year, colour = highqual)) +
  geom_line(aes(linetype = factor(year))) + geom_point(aes(shape = highqual), size = 3) + 
  geom_linerange(
    aes(ymax = prop_dlo + 2 * se_dlo, ymin = prop_dlo - 2 * se_dlo)
  ) + 
  labs(
    x = "Age Group", 
    y = "Proportion with driving licence", 
    colour = "Highest Qualification", shape = "Highest Qualification", linetype = "Year") + 
  scale_y_continuous(limits = c(0, 1)) + 
  facet_wrap(~sex)

ggsave("figures/final_deck/03 - proportion of dlos  - by sex, age, highqual, and two periods.png", width = 25, height =20, units = "cm", dpi = 300)

# 03a - proportion of dlos  - by sex, age, highqual, and two periods - double faceted

all_inds_drvs %>% 
  mutate(
    year = wave + 1990
  ) %>% 
  mutate(
    age_grp = 
      cut(
        age, 
        c(0, 20, 25, 35, 50, 60, 70, 999), 
        labels = c("<20", "20-25", "26-35", "36-50", "51-60", "61-70", ">70")
      )
  ) %>% 
  filter(!is.na(sex) & !is.na(age) & !is.na(year) & !is.na(dlo) & !is.na(highqual)) %>% 
  filter(year %in% c(1995, 2005)) %>%
  select(sex, highqual, age_grp, year,  dlo) %>% 
  group_by(sex, highqual, age_grp, year, dlo) %>% 
  tally %>% 
  spread(dlo, n, fill = 0) %>% 
  mutate(
    prop_dlo = yes / (yes + no),
    se_dlo = (prop_dlo * (1 - prop_dlo) / (yes + no)) ^ 0.5,
    hq_year = paste(highqual, year, sep = "_")
  ) %>% 
  ggplot(., aes(
    x = age_grp, 
    y = prop_dlo, group = hq_year, colour = highqual)) +
  geom_line(aes(linetype = factor(year))) + geom_point(aes(shape = highqual), size = 3) + 
  geom_linerange(
    aes(ymax = prop_dlo + 2 * se_dlo, ymin = prop_dlo - 2 * se_dlo)
  ) + 
  labs(
    x = "Age Group", 
    y = "Proportion with driving licence", 
    colour = "Highest Qualification", shape = "Highest Qualification", linetype = "Year") + 
  scale_y_continuous(limits = c(0, 1)) + 
  facet_grid(highqual ~ sex)

ggsave("figures/final_deck/03a - proportion of dlos  - by sex, age, highqual, and two periods.png", width = 30, height =25, units = "cm", dpi = 300)

# 04 - proportion of drivers driving - by sex, age, and period 
all_inds_drvs %>% 
  mutate(
    year = wave + 1990
  ) %>% 
  mutate(
    age_grp = 
      cut(
        age, 
        c(0, 20, 25, 35, 50, 60, 70, 999), 
        labels = c("<20", "20-25", "26-35", "36-50", "51-60", "61-70", ">70")
      )
  ) %>% 
  filter(!is.na(sex) & !is.na(age) & !is.na(year) & !is.na(dlo) & !is.na(cu)) %>% 
  filter(year %in% c(1995, 2000, 2005)) %>%
  filter(dlo == "yes") %>% 
  select(sex, age_grp, year,  cu) %>% 
  group_by(sex, age_grp, year, cu) %>% 
  tally %>% 
  spread(cu, n, fill = 0) %>% 
  mutate(
    drivers_driving = yes / (yes + no),
    se_drdr = (drivers_driving * (1 - drivers_driving) / (yes + no)) ^ 0.5
  ) %>% 
  ggplot(., aes(
    x = age_grp, 
    y = drivers_driving, group = factor(year), colour = factor(year), shape = factor(year))) +
  geom_line() + geom_point(aes(shape = factor(year)), size = 3) + 
  geom_linerange(
    aes(ymax = drivers_driving + 2 * se_drdr, ymin = drivers_driving - 2 * se_drdr)
  ) + 
  labs(
    x = "Age Group", 
    y = "Proportion of drivers driving", 
    colour = "Year", group = "Year", shape = "Year") + 
  scale_y_continuous(limits = c(0, 1)) + 
  facet_wrap(~sex)
ggsave("figures/final_deck/04 - proportion of drivers driving - by sex, age, and period.png", width = 25, height =20, units = "cm", dpi = 300)

# 05 - proportion of drivers driving - by sex, age, urban rural class, and two periods 
all_inds_drvs %>% 
  mutate(
    year = wave + 1990
  ) %>% 
  mutate(
    age_grp = 
      cut(
        age, 
        c(0, 20, 25, 35, 50, 60, 70, 999), 
        labels = c("<20", "20-25", "26-35", "36-50", "51-60", "61-70", ">70")
      )
  ) %>% 
  filter(!is.na(sex) & !is.na(age) & !is.na(year) & !is.na(dlo) & !is.na(cu) & !is.na(ur_group)) %>% 
  filter(year %in% c(1995, 2005)) %>%
  filter(dlo == "yes") %>% 
  select(sex, ur_group, age_grp, year,  cu) %>% 
  group_by(sex, ur_group, age_grp, year, cu) %>% 
  tally %>% 
  spread(cu, n, fill = 0) %>% 
  mutate(
    drivers_driving = yes / (yes + no),
    se_drdr = (drivers_driving * (1 - drivers_driving) / (yes + no)) ^ 0.5,
    ur_year = paste(ur_group, year, sep = "_")
  ) %>% 
  ggplot(., aes(
    x = age_grp, 
    y = drivers_driving, group = ur_year, colour = ur_group)
    ) +
  geom_line(aes(linetype = factor(year))) + geom_point(aes(shape = ur_group), size = 3) +
  geom_linerange(
    aes(ymax = drivers_driving + 2 * se_drdr, ymin = drivers_driving - 2 * se_drdr)
  ) + 
  labs(
    x = "Age Group", 
    y = "Proportion of drivers driving", 
    colour = "Urban Rural Class", group = "Urban Rural Class", shape = "Urban Rural Class",
    linetype = "Year") + 
  scale_y_continuous(limits = c(0, 1)) + 
  facet_wrap(~sex)
ggsave("figures/final_deck/05 - proportion of drivers driving - by sex, age, urban rural class, and two periods.png", width = 25, height =20, units = "cm", dpi = 300)

# 06 - proportion of drivers driving - by sex, age, highest qual, and two periods 

all_inds_drvs %>% 
  mutate(
    year = wave + 1990
  ) %>% 
  mutate(
    age_grp = 
      cut(
        age, 
        c(0, 20, 25, 35, 50, 60, 70, 999), 
        labels = c("<20", "20-25", "26-35", "36-50", "51-60", "61-70", ">70")
      )
  ) %>% 
  filter(!is.na(sex) & !is.na(age) & !is.na(year) & !is.na(dlo) & !is.na(cu) & !is.na(highqual)) %>% 
  filter(year %in% c(1995, 2005)) %>%
  filter(dlo == "yes") %>% 
  select(sex, highqual, age_grp, year,  cu) %>% 
  group_by(sex, highqual, age_grp, year, cu) %>% 
  tally %>% 
  spread(cu, n, fill = 0) %>% 
  mutate(
    drivers_driving = yes / (yes + no),
    se_drdr = (drivers_driving * (1 - drivers_driving) / (yes + no)) ^ 0.5,
    hq_year = paste(highqual, year, sep = "_")
  ) %>% 
  ggplot(., aes(
    x = age_grp, 
    y = drivers_driving, group = hq_year, colour = highqual)
  ) +
  geom_line(aes(linetype = factor(year))) + geom_point(aes(shape = highqual), size = 3) +
  geom_linerange(
    aes(ymax = drivers_driving + 2 * se_drdr, ymin = drivers_driving - 2 * se_drdr)
  ) + 
  labs(
    x = "Age Group", 
    y = "Proportion of drivers driving", 
    colour = "Highest Qualification", group = "Highest Qualification", shape = "Highest Qualification",
    linetype = "Year") + 
  scale_y_continuous(limits = c(0, 1)) + 
  facet_wrap(~sex)
ggsave("figures/final_deck/06 - proportion of drivers driving - by sex, age, highest qual, and two periods.png", width = 25, height =20, units = "cm", dpi = 300)

# 06a - proportion of drivers driving - by sex, age, highest qual, and two periods - faceted

all_inds_drvs %>% 
  mutate(
    year = wave + 1990
  ) %>% 
  mutate(
    age_grp = 
      cut(
        age, 
        c(0, 20, 25, 35, 50, 60, 70, 999), 
        labels = c("<20", "20-25", "26-35", "36-50", "51-60", "61-70", ">70")
      )
  ) %>% 
  filter(!is.na(sex) & !is.na(age) & !is.na(year) & !is.na(dlo) & !is.na(cu) & !is.na(highqual)) %>% 
  filter(year %in% c(1995, 2005)) %>%
  filter(dlo == "yes") %>% 
  select(sex, highqual, age_grp, year,  cu) %>% 
  group_by(sex, highqual, age_grp, year, cu) %>% 
  tally %>% 
  spread(cu, n, fill = 0) %>% 
  mutate(
    drivers_driving = yes / (yes + no),
    se_drdr = (drivers_driving * (1 - drivers_driving) / (yes + no)) ^ 0.5,
    hq_year = paste(highqual, year, sep = "_")
  ) %>% 
  ggplot(., aes(
    x = age_grp, 
    y = drivers_driving, group = hq_year, colour = highqual)
  ) +
  geom_line(aes(linetype = factor(year))) + geom_point(aes(shape = highqual), size = 3) +
  geom_linerange(
    aes(ymax = drivers_driving + 2 * se_drdr, ymin = drivers_driving - 2 * se_drdr)
  ) + 
  labs(
    x = "Age Group", 
    y = "Proportion of drivers driving", 
    colour = "Highest Qualification", group = "Highest Qualification", shape = "Highest Qualification",
    linetype = "Year") + 
  scale_y_continuous(limits = c(0, 1)) + 
  facet_grid(highqual ~ sex)
ggsave("figures/final_deck/06a - proportion of drivers driving - by sex, age, highest qual, and two periods - faceted.png", width = 25, height =20, units = "cm", dpi = 300)



# Levelplots 

# 07 - proportion of dlos  - by age, year, sex


all_inds_drvs %>% 
  mutate(
    year = wave + 1990
  ) %>% 
  filter(!is.na(sex) & !is.na(age) & !is.na(year) & !is.na(dlo) & !is.na(ur_group)) %>% 
  filter( year > 1992) %>% 
  select(year, age, sex,  dlo) %>% 
  arrange(year, sex, age) %>% 
  group_by(year, sex, age) %>% 
  mutate(does_drive = car::recode(dlo, "'yes' = 1; 'no' = '0'; else = NA")) %>% 
  summarise(drive_prop = mean(does_drive, na.rm=T)) %>% 
  filter(age <= 80 & age >= 17) %>% 
  levelplot(
    drive_prop ~ year * age | sex, 
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
  ) -> p
png(filename = "figures/final_deck/07 - proportion of dlos  - by age, year, sex.png", width = 18, height = 35, units = "cm", res = 300)
print(p)
dev.off()

# Greyscale version of 07


all_inds_drvs %>% 
  mutate(
    year = wave + 1990
  ) %>% 
  filter(!is.na(sex) & !is.na(age) & !is.na(year) & !is.na(dlo) & !is.na(ur_group)) %>% 
  filter( year > 1992) %>% 
  select(year, age, sex,  dlo) %>% 
  arrange(year, sex, age) %>% 
  group_by(year, sex, age) %>% 
  mutate(does_drive = car::recode(dlo, "'yes' = 1; 'no' = '0'; else = NA")) %>% 
  summarise(drive_prop = mean(does_drive, na.rm=T)) %>% 
  filter(age <= 80 & age >= 17) %>% 
  levelplot(
    drive_prop ~ year * age | sex, 
    data=., 
    aspect = "iso",
    strip=strip.custom(par.strip.text=list(cex=1.4, fontface="bold"), bg="grey"),
    ylab=list(label="Age in years", cex=1.4),
    xlab=list(label="Year", cex=1.4),
    cex=1.4,
    col.regions= rev(grey(seq(0, 1, by = 0.1))),
    main=NULL,
    colorkey = list(labels= list(cex = 1.4)),
    at = c(0, seq(0.5, 1., by = 0.05)),
    labels=list(cex=1.2),
    col="black",
    scales=list(
      x=list(cex=1.1, rot = 90), 
      y=list(cex=1.1),
      alternating=3
    )
  ) ->
png(filename = "figures/final_deck/07g - proportion of dlos  - by age, year, sex.png", width = 18, height = 35, units = "cm", res = 300)
print(p)
dev.off()


# 08 - proportion of dlos  - by age, year, sex, urclass


all_inds_drvs %>% 
  mutate(
    year = wave + 1990
  ) %>% 
  filter(!is.na(sex) & !is.na(age) & !is.na(year) & !is.na(dlo) & !is.na(ur_group)) %>% 
  filter( year > 1992) %>% 
  mutate(ur_group = car::recode(
    ur_group, 
    "
    'urban' = 'U';
    'nonurban' = 'NU'
    ",
    as.factor.result =T
  ),
  sex = car::recode(sex, "'male' = 'M'; 'female' = 'F'")
  ) %>%  
  select(year, age, sex, ur_group, dlo) %>% 
  arrange(year, sex, ur_group, age) %>% 
  group_by(year, sex, ur_group, age) %>% 
  mutate(does_drive = car::recode(dlo, "'yes' = 1; 'no' = '0'; else = NA")) %>% 
  summarise(drive_prop = mean(does_drive, na.rm=T)) %>% 
  filter(age <= 80 & age >= 17) %>% 
  levelplot(
    drive_prop ~ year * age | sex + ur_group, 
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
  ) -> p

png(filename = "figures/final_deck/08 - proportion of dlos  - by age, year, sex, urclass.png", width = 18, height = 35, units = "cm", res = 300)
print(p)
dev.off()


# 08 - proportion of dlos  - by age, year, sex, urclass


all_inds_drvs %>% 
  mutate(
    year = wave + 1990
  ) %>% 
  filter(!is.na(sex) & !is.na(age) & !is.na(year) & !is.na(dlo) & !is.na(ur_group)) %>% 
  filter( year > 1992) %>% 
  mutate(ur_group = car::recode(
    ur_group, 
    "
    'urban' = 'U';
    'nonurban' = 'NU'
    ",
    as.factor.result =T
  ),
  sex = car::recode(sex, "'male' = 'M'; 'female' = 'F'")
  ) %>%  
  select(year, age, sex, ur_group, dlo) %>% 
  arrange(year, sex, ur_group, age) %>% 
  group_by(year, sex, ur_group, age) %>% 
  mutate(does_drive = car::recode(dlo, "'yes' = 1; 'no' = '0'; else = NA")) %>% 
  summarise(drive_prop = mean(does_drive, na.rm=T)) %>% 
  filter(age <= 80 & age >= 17) %>% 
  levelplot(
    drive_prop ~ year * age | sex + ur_group, 
    data=., 
    aspect = "iso",
    strip=strip.custom(par.strip.text=list(cex=1.4, fontface="bold"), bg="grey"),
    ylab=list(label="Age in years", cex=1.4),
    xlab=list(label="Year", cex=1.4),
    cex=1.4,
    col.regions= rev(grey(seq(0, 1, by = 0.1))),
    main=NULL,
    colorkey = list(labels= list(cex = 1.4)),
    at = c(0, seq(0.5, 1., by = 0.05)),
    labels=list(cex=1.2),
    col="black",
    scales=list(
      x=list(cex=1.1, rot = 90), 
      y=list(cex=1.1),
      alternating=3
    )
  ) -> p
png(filename = "figures/final_deck/08g - proportion of dlos  - by age, year, sex, urclass.png", width = 18, height = 35, units = "cm", res = 300)
print(p)
dev.off()


# 09g - proportion of dlos  - by age, year, sex, highqual 

all_inds_drvs %>% 
  mutate(
    year = wave + 1990
  ) %>% 
  filter(!is.na(sex) & !is.na(age) & !is.na(year) & !is.na(dlo) & !is.na(highqual)) %>% 
  filter( year > 1992) %>% 
  mutate(highqual = car::recode(
    highqual, 
    "
    'further non-vocational' = 'High';
    'further vocational' = 'Med';
    'no further' = 'Low'
    ",
    levels = c("Low", "Med", "High"), as.factor.result =T
  ),
  sex = car::recode(sex, "'male' = 'M'; 'female' = 'F'")
  ) %>% 
  select(year, age, sex, highqual, dlo) %>% 
  arrange(year, sex, highqual, age) %>% 
  group_by(year, sex, highqual, age) %>% 
  mutate(does_drive = car::recode(dlo, "'yes' = 1; 'no' = '0'; else = NA")) %>% 
  summarise(drive_prop = mean(does_drive, na.rm=T)) %>% 
  filter(age <= 80 & age >= 17) %>% 
  levelplot(
    drive_prop ~ year * age | sex + highqual, 
    data=., 
    aspect = "iso",
    strip=strip.custom(par.strip.text=list(cex=1.4, fontface="bold"), bg="grey"),
    ylab=list(label="Age in years", cex=1.4),
    xlab=list(label="Year", cex=1.4),
    cex=1.4,
    col.regions= rev(grey(seq(0, 1, by = 0.1))),
    main=NULL,
    colorkey = list(labels= list(cex = 1.4)),
    at = c(0, seq(0.5, 1., by = 0.05)),
    labels=list(cex=1.2),
    col="black",
    scales=list(
      x=list(cex=1.1, rot = 90), 
      y=list(cex=1.1),
      alternating=3
    )
  ) -> p
png(filename = "figures/final_deck/09g - proportion of dlos  - by age, year, sex, highqual.png", width = 18, height = 35, units = "cm", res = 300)
print(p)
dev.off()


# 09 - proportion of dlos  - by age, year, sex, highqual 

all_inds_drvs %>% 
  mutate(
    year = wave + 1990
  ) %>% 
  filter(!is.na(sex) & !is.na(age) & !is.na(year) & !is.na(dlo) & !is.na(highqual)) %>% 
  filter( year > 1992) %>% 
  mutate(highqual = car::recode(
    highqual, 
    "
    'further non-vocational' = 'High';
    'further vocational' = 'Med';
    'no further' = 'Low'
    ",
    levels = c("Low", "Med", "High"), as.factor.result =T
  ),
  sex = car::recode(sex, "'male' = 'M'; 'female' = 'F'")
  ) %>% 
  select(year, age, sex, highqual, dlo) %>% 
  arrange(year, sex, highqual, age) %>% 
  group_by(year, sex, highqual, age) %>% 
  mutate(does_drive = car::recode(dlo, "'yes' = 1; 'no' = '0'; else = NA")) %>% 
  summarise(drive_prop = mean(does_drive, na.rm=T)) %>% 
  filter(age <= 80 & age >= 17) %>% 
  levelplot(
    drive_prop ~ year * age | sex + highqual, 
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
  ) -> p

png(filename = "figures/final_deck/09 - proportion of dlos  - by age, year, sex, highqual.png", width = 18, height = 35, units = "cm", res = 300)
print(p)
dev.off()

# 10 - proportion of drivers driving - by age, year, sex


all_inds_drvs %>% 
  mutate(
    year = wave + 1990
  ) %>% 
  filter(!is.na(sex) & !is.na(age) & !is.na(year) & !is.na(dlo)  & !is.na(cu)) %>% 
  filter(dlo == "yes") %>% 
  filter( year > 1992) %>% 
  select(age, year, sex, cu) %>% 
  group_by(age, year, sex, cu) %>% 
  tally %>% 
  spread(cu, n, fill = 0) %>% 
  mutate(
    drivers_driving = yes / (yes + no)
  ) %>% 
  filter(age <= 80 & age >= 17) %>% 
  levelplot(
    drivers_driving ~ year * age | sex, 
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
  ) -> p

png(filename = "figures/final_deck/10 - proportion of drivers driving - by age, year, sex.png", width = 18, height = 35, units = "cm", res = 300)
print(p)
dev.off()

# 10g - proportion of drivers driving - by age, year, sex


all_inds_drvs %>% 
  mutate(
    year = wave + 1990
  ) %>% 
  filter(!is.na(sex) & !is.na(age) & !is.na(year) & !is.na(dlo)  & !is.na(cu)) %>% 
  filter(dlo == "yes") %>% 
  filter( year > 1992) %>% 
  select(age, year, sex, cu) %>% 
  group_by(age, year, sex, cu) %>% 
  tally %>% 
  spread(cu, n, fill = 0) %>% 
  mutate(
    drivers_driving = yes / (yes + no)
  ) %>% 
  filter(age <= 80 & age >= 17) %>% 
  levelplot(
    drivers_driving ~ year * age | sex, 
    data=., 
    aspect = "iso",
    strip=strip.custom(par.strip.text=list(cex=1.4, fontface="bold"), bg="grey"),
    ylab=list(label="Age in years", cex=1.4),
    xlab=list(label="Year", cex=1.4),
    cex=1.4,
    col.regions= rev(grey(seq(0, 1, by = 0.1))),
    main=NULL,
    colorkey = list(labels= list(cex = 1.4)),
    at = c(0, seq(0.5, 1., by = 0.05)),
    labels=list(cex=1.2),
    col="black",
    scales=list(
      x=list(cex=1.1, rot = 90), 
      y=list(cex=1.1),
      alternating=3
    )
  ) -> p

png(filename = "figures/final_deck/10g - proportion of drivers driving - by age, year, sex.png", width = 18, height = 35, units = "cm", res = 300)
print(p)
dev.off()

# 11 - proportion of drivers driving - by age, year, sex, urclass


all_inds_drvs %>% 
  mutate(
    year = wave + 1990
  ) %>% 
  filter(!is.na(sex) & !is.na(age) & !is.na(year) & !is.na(dlo) & !is.na(ur_group) & !is.na(cu)) %>% 
  filter(dlo == "yes") %>% 
  filter( year > 1992) %>% 
  mutate(ur_group = car::recode(
    ur_group, 
    "
    'urban' = 'Urb';
    'nonurban' = 'Non Urb'
    ",
    as.factor.result =T
    ),
  sex = car::recode(sex, "'male' = 'M'; 'female' = 'F'")
  ) %>%  
  select(age, year, sex, ur_group,  cu) %>% 
  group_by(age, year, sex, ur_group,  cu) %>% 
  tally %>% 
  spread(cu, n, fill = 0) %>% 
  mutate(
    drivers_driving = yes / (yes + no)
  ) %>% 
  filter(age <= 80 & age >= 17) %>% 
  levelplot(
    drivers_driving ~ year * age | sex + ur_group, 
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
  ) -> p

png(filename = "figures/final_deck/11 - proportion of drivers driving - by age, year, sex, urclass.png", width = 18, height = 35, units = "cm", res = 300)
print(p)
dev.off()

# 11g - proportion of drivers driving - by age, year, sex, urclass


all_inds_drvs %>% 
  mutate(
    year = wave + 1990
  ) %>% 
  filter(!is.na(sex) & !is.na(age) & !is.na(year) & !is.na(dlo) & !is.na(ur_group) & !is.na(cu)) %>% 
  filter(dlo == "yes") %>% 
  filter( year > 1992) %>% 
  mutate(ur_group = car::recode(
    ur_group, 
    "
    'urban' = 'U';
    'nonurban' = 'NU'
    ",
    as.factor.result =T
  ),
  sex = car::recode(sex, "'male' = 'M'; 'female' = 'F'")
  ) %>%  
  select(age, year, sex, ur_group,  cu) %>% 
  group_by(age, year, sex, ur_group,  cu) %>% 
  tally %>% 
  spread(cu, n, fill = 0) %>% 
  mutate(
    drivers_driving = yes / (yes + no)
  ) %>% 
  filter(age <= 80 & age >= 17) %>% 
  levelplot(
    drivers_driving ~ year * age | sex + ur_group, 
    data=., 
    aspect = "iso",
    strip=strip.custom(par.strip.text=list(cex=1.4, fontface="bold"), bg="grey"),
    ylab=list(label="Age in years", cex=1.4),
    xlab=list(label="Year", cex=1.4),
    cex=1.4,
    col.regions= rev(grey(seq(0, 1, by = 0.1))),
    main=NULL,
    colorkey = list(labels= list(cex = 1.4)),
    at = c(0, seq(0.5, 1., by = 0.05)),
    labels=list(cex=1.2),
    col="black",
    scales=list(
      x=list(cex=1.1, rot = 90), 
      y=list(cex=1.1),
      alternating=3
    )
  ) -> p

png(filename = "figures/final_deck/11g - proportion of drivers driving - by age, year, sex, urclass.png", width = 18, height = 35, units = "cm", res = 300)
print(p)
dev.off()

# 12 - proportion of drivers driving - by age, year, sex, highqual

all_inds_drvs %>% 
  mutate(
    year = wave + 1990
  ) %>% 
  filter( year > 1992) %>% 
  filter(!is.na(sex) & !is.na(age) & !is.na(year) & !is.na(dlo) & !is.na(highqual) & !is.na(cu)) %>% 
  filter(dlo == "yes") %>% 
  mutate(highqual = car::recode(
    highqual, 
    "
    'further non-vocational' = 'High';
    'further vocational' = 'Med';
    'no further' = 'Low'
    ",
    levels = c("Low", "Med", "High"), as.factor.result =T
  ),
  sex = car::recode(sex, "'male' = 'M'; 'female' = 'F'")
  ) %>% 
  select(age, year, sex, highqual,  cu) %>% 
  group_by(age, year, sex, highqual,  cu) %>% 
  tally %>% 
  spread(cu, n, fill = 0) %>% 
  mutate(
    drivers_driving = yes / (yes + no)
  ) %>% 
  filter(age <= 80 & age >= 17) %>% 
  levelplot(
    drivers_driving ~ year * age | sex + highqual, 
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
  ) -> p

png(filename = "figures/final_deck/12 - proportion of drivers driving - by age, year, sex, highqual.png", width = 18, height = 35, units = "cm", res = 300)
print(p)
dev.off()


# 12g - proportion of drivers driving - by age, year, sex, highqual

all_inds_drvs %>% 
  mutate(
    year = wave + 1990
  ) %>% 
  filter( year > 1992) %>% 
  filter(!is.na(sex) & !is.na(age) & !is.na(year) & !is.na(dlo) & !is.na(highqual) & !is.na(cu)) %>% 
  filter(dlo == "yes") %>% 
  mutate(highqual = car::recode(
    highqual, 
    "
    'further non-vocational' = 'High';
    'further vocational' = 'Med';
    'no further' = 'Low'
    ",
    levels = c("Low", "Med", "High"), as.factor.result =T
  ),
  sex = car::recode(sex, "'male' = 'M'; 'female' = 'F'")
  ) %>% 
  select(age, year, sex, highqual,  cu) %>% 
  group_by(age, year, sex, highqual,  cu) %>% 
  tally %>% 
  spread(cu, n, fill = 0) %>% 
  mutate(
    drivers_driving = yes / (yes + no)
  ) %>% 
  filter(age <= 80 & age >= 17) %>% 
  levelplot(
    drivers_driving ~ year * age | sex + highqual, 
    data=., 
    aspect = "iso",
    strip=strip.custom(par.strip.text=list(cex=1.4, fontface="bold"), bg="grey"),
    ylab=list(label="Age in years", cex=1.4),
    xlab=list(label="Year", cex=1.4),
    cex=1.4,
    col.regions= rev(grey(seq(0, 1, by = 0.1))),
    main=NULL,
    colorkey = list(labels= list(cex = 1.4)),
    at = c(0, seq(0.5, 1., by = 0.05)),
    labels=list(cex=1.2),
    col="black",
    scales=list(
      x=list(cex=1.1, rot = 90), 
      y=list(cex=1.1),
      alternating=3
    )
  ) -> p

png(filename = "figures/final_deck/12g - proportion of drivers driving - by age, year, sex, highqual.png", width = 18, height = 35, units = "cm", res = 300)
print(p)
dev.off()


# Tables by cohort 

# What do I want? 

# For birth cohorts in five year groupings, 
# 1950-54, 1955-59, 1960-64, 1965-69, 1970-74, 1975-79, 1980-84, 1985-89

# For ages 
# 25-40

# all_inds_drvs %>% 
#   mutate(year = wave + 1990) %>% 
#   mutate(birth_year = year - age) %>% 
#   mutate(
#     birth_year_group = cut(
#       birth_year, 
#       breaks = c(0, seq(1950, 1990, by = 5))
#     )
#   ) %>% 
#   filter(age %in% c(25, 30, 35, 40)) %>% 
#   select(sex, birth_year_group, age, dlo) %>%
#   group_by(sex, birth_year_group, age) %>% 
#   summarise(
#     has_dlo = length(dlo[dlo == "yes"]),
#     no_dlo = length(dlo[dlo == "no"]),
#     prop_dlo = has_dlo / (has_dlo + no_dlo)
#   ) %>% 
#   arrange(sex, birth_year_group, age) %>% 
#   mutate(summaries = paste0(has_dlo, "/", no_dlo, " (", round(prop_dlo, 2), ")")) %>% 
#   select(sex, birth_year_group, age, summaries) %>% 
#   spread(age, summaries) %>% 
#   write_csv("tables/birth_year_props.csv")
# 
# 
# 
# 
# 


# Graphs - individual level -----------------------------------------------

# prop who like neighbourhood by wave, sex and driver status
all_inds_drvs %>% 
  mutate(wave_year = 1990 + wave) %>%
  filter(!is.na(neigh) & !is.na(dlo)) %>% 
  group_by(sex, wave_year, dlo, neigh) %>% 
  tally %>% 
  spread(neigh, n) %>% 
  mutate(total = mixed + no + yes) %>% 
  mutate(mean_like_prop = yes / (total)) %>%
  mutate(se_like_prop = (mean_like_prop * (1 - mean_like_prop) / total)^0.5) %>% 
  ggplot(., mapping = aes(x = factor(wave_year), y = mean_like_prop, colour = dlo, group = dlo)) +
  geom_line() + 
  geom_point(aes(shape = dlo)) + 
  geom_linerange(aes(ymax = mean_like_prop + 2 * se_like_prop, ymin = mean_like_prop - 2 * se_like_prop)) +
  facet_wrap(~sex) + 
  labs(x = "Year", y = "Proportion liking neighbourhood")

#prop who like neighbourhood by wave, age group, sex and driver status
all_inds_drvs %>% 
  filter(!is.na(neigh) & !is.na(dlo)) %>% 
  mutate(wave_year = 1990 + wave) %>%
  mutate(age_grp = 
           cut(age, 
               breaks = c(17, 21, 26, 35, 50, 65, 80), 
               include.lowest = T, 
               labels = c("17-20", "21-25", "26-35", "36-49", "50-64", "65-79")
           )
  ) %>% 
  filter(!is.na(age_grp)) %>% 
  group_by(age_grp, sex, wave_year, dlo, neigh) %>% 
  tally %>% 
  spread(neigh, n, fill = 0) %>% 
  mutate(total = mixed + no + yes) %>% 
  mutate(mean_like_prop = yes / (total)) %>%
  mutate(se_like_prop = (mean_like_prop * (1 - mean_like_prop) / total)^0.5) %>% 
  ggplot(., mapping = aes(x = factor(wave_year), y = mean_like_prop, colour = dlo, group = dlo)) +
  geom_line() + 
  geom_point(aes(shape = dlo), size = 4) + 
  geom_linerange(aes(ymax = mean_like_prop + 2 * se_like_prop, ymin = mean_like_prop - 2 * se_like_prop)) +
  facet_grid(sex ~age_grp) + 
  labs(x = "Year", y = "proportion of respondents who like their neighbourhood")

ggsave("figures/prop_who_like_neighbourhood.png", dpi = 300, height = 15, width = 25, units = "cm")
# This suggests there's a particularly high level of neighbourhood dissatisfaction amongst 
# Males aged 26-35 who do not drive, particularly in wave 8 (H)


all_inds_drvs %>% 
  mutate(wave_year = 1990 + wave) %>%
  mutate(wave_year_group = cut(wave_year, c(0, 2000, Inf), labels = c("1990s", "2000s"))) %>% 
  mutate(
  age_grp = 
    cut(
      age, 
      c(0, 20, 25, 35, 50, 60, 70, 999), 
      labels = c("<20", "20-25", "26-35", "36-50", "51-60", "61-70", ">70")
      )
  ) %>% 
  filter(!is.na(sex) & !is.na(age_grp)) %>% 
  group_by(sex, age_grp, wave_year_group, dlo) %>% 
  summarise(
    mn_ghq = mean(ghq, na.rm=T),
    se_ghq = sd(ghq, na.rm = T) / length(ghq[!is.na(ghq)]) ^ 0.5 
    ) %>%
  filter(!is.na(dlo)) %>% 
  ggplot(data = ., 
         mapping =     
           aes(
             x = factor(age_grp), y = mn_ghq, 
             group = dlo, colour = dlo, fill = dlo
           )
  ) + 
  geom_line() + geom_point(aes(shape = dlo), size = 4) +
  geom_linerange(
    aes(
      ymax = mn_ghq + 2 * se_ghq, 
      ymin = mn_ghq - 2 * se_ghq
      )
    ) + 
  facet_grid(wave_year_group~ sex) + 
  labs(
    x = "Age group", y = "Mean (SE) GHQ Score (lower is better)",
    title = "Mean GHQ score by Age group, sex, and whether has driving licence"
  )
ggsave("figures/ghq_by_dlo.png", dpi = 300, height = 25, width = 25, units = "cm")

# GHQ by highest qualification and age group

all_inds_drvs %>% 
  mutate(wave_year = 1990 + wave) %>%
  mutate(wave_year_group = cut(wave_year, c(0, 2000, Inf), labels = c("1990s", "2000s"))) %>% 
  mutate(
    age_grp = 
      cut(
        age, 
        c(0, 20, 25, 35, 50, 60, 70, 999), 
        labels = c("<20", "20-25", "26-35", "36-50", "51-60", "61-70", ">70")
      )
  ) %>% 
  filter(!is.na(sex) & !is.na(age_grp) & !is.na(highqual)) %>% 
  group_by(sex, age_grp, highqual, dlo) %>% 
  summarise(
    mn_ghq = mean(ghq, na.rm=T),
    se_ghq = sd(ghq, na.rm = T) / length(ghq[!is.na(ghq)]) ^ 0.5 
  ) %>%
  filter(!is.na(dlo)) %>% 
  ggplot(data = ., 
         mapping =     
           aes(
             x = factor(age_grp), y = mn_ghq, 
             group = dlo, colour = dlo, fill = dlo
           )
  ) + 
  geom_line() + geom_point(aes(shape = dlo), size = 4) +
  geom_linerange(
    aes(
      ymax = mn_ghq + 2 * se_ghq, 
      ymin = mn_ghq - 2 * se_ghq
    )
  ) + 
  facet_grid(highqual ~ sex) + 
  labs(
    x = "Age group", y = "Mean (SE) GHQ Score (lower is better)",
    title = "Mean GHQ score by Age group, sex, and whether has driving licence"
  ) + 
  coord_cartesian(ylim = c(0, 4))
ggsave("figures/ghq_by_dlo_highqual_agegroup.png", dpi = 300, height = 25, width = 25, units = "cm")

# Proportion with high GHQ (GHQ > 2) , by sex
all_inds_drvs %>% 
  mutate(wave_year = 1990 + wave) %>%
  mutate(wave_year_group = cut(wave_year, c(0, 2000, Inf), labels = c("1990s", "2000s"))) %>% 
  mutate(
    age_grp = 
      cut(
        age, 
        c(0, 20, 25, 35, 50, 60, 70, 999), 
        labels = c("<20", "20-25", "26-35", "36-50", "51-60", "61-70", ">70")
      )
  ) %>% 
  filter(!is.na(sex) & !is.na(age_grp) & !is.na(ghq) & !is.na(dlo)) %>%
  select(sex, age_grp, dlo, ghq) %>% 
  mutate(highghq = cut(ghq, c(-1, 2, 999), labels = c("no", "yes"))) %>%
  group_by(sex, age_grp, dlo, highghq) %>%
  select(-ghq) %>% 
  tally() %>%
  spread(highghq, n) %>% 
  mutate(
    prop_distress = yes / (yes + no),
    se_p = ((prop_distress * ( 1 - prop_distress) ) / (yes + no)) ^ 0.5
  ) %>% 
  ggplot(data = ., 
         mapping =     
           aes(
             x = factor(age_grp), y = prop_distress, 
             group = dlo, colour = dlo, fill = dlo
           )
  ) + 
  geom_line() + geom_point(aes(shape = dlo), size = 4) +
  geom_linerange(
    aes(
      ymax = prop_distress + 2 * se_p, 
      ymin = prop_distress - 2 * se_p
    )
  ) + 
  facet_wrap( ~ sex) + 
  labs(
    x = "Age group", y = "Proportion with GHQ > 2",
    title = "High GHQ prop by Age group, sex, and whether has driving licence"
  ) + 
  coord_cartesian(ylim = c(0, 0.75))
ggsave("figures/ghqdistress_by_dlo_highqual_agegroup.png", dpi = 300, height = 15, width = 25, units = "cm")

# Proportion with high GHQ (3 or more) - by sex and qual

all_inds_drvs %>% 
  mutate(wave_year = 1990 + wave) %>%
  mutate(wave_year_group = cut(wave_year, c(0, 2000, Inf), labels = c("1990s", "2000s"))) %>% 
  mutate(
    age_grp = 
      cut(
        age, 
        c(0, 20, 25, 35, 50, 60, 70, 999), 
        labels = c("<20", "20-25", "26-35", "36-50", "51-60", "61-70", ">70")
      )
  ) %>% 
  filter(!is.na(sex) & !is.na(age_grp) & !is.na(highqual) & !is.na(ghq) & !is.na(dlo)) %>%
  select(sex, age_grp, highqual, dlo, ghq) %>% 
  mutate(highghq = cut(ghq, c(-1, 2, 999), labels = c("no", "yes"))) %>%
  group_by(sex, age_grp, highqual, dlo, highghq) %>%
  select(-ghq) %>% 
  tally() %>%
  spread(highghq, n) %>% 
  mutate(
    prop_distress = yes / (yes + no),
    se_p = ((prop_distress * ( 1 - prop_distress) ) / (yes + no)) ^ 0.5
  ) %>% 
  ggplot(data = ., 
         mapping =     
           aes(
             x = factor(age_grp), y = prop_distress, 
             group = dlo, colour = dlo, fill = dlo
           )
  ) + 
  geom_line() + geom_point(aes(shape = dlo), size = 4) +
  geom_linerange(
    aes(
      ymax = prop_distress + 2 * se_p, 
      ymin = prop_distress - 2 * se_p
    )
  ) + 
  facet_grid(highqual ~ sex) + 
  labs(
    x = "Age group", y = "Proportion with GHQ > 2",
    title = "High GHQ prop by Age group, sex, and whether has driving licence"
  ) + 
  coord_cartesian(ylim = c(0, 0.75))

ggsave("figures/ghqdistress_by_dlo_highqual_agegroup.png", dpi = 300, height = 15, width = 25, units = "cm")

# Proportion dlo by age, faceted by wave 

all_inds_drvs %>%   
  filter(!is.na(sex)) %>% 
  arrange(sex, age) %>% 
  mutate(wave_year = 1990 + wave) %>%
  filter(wave_year %in% c(1991, 1995, 2000, 2005)) %>% 
  group_by(wave_year, sex, age) %>%
  filter(age <= 80) %>% 
  mutate(
    does_drive = recode(dlo, "'yes' = 1; 'no' = '0'; else = NA")
  ) %>% 
  summarise(driv_prop = mean(does_drive, na.rm=T)) %>% 
  ggplot(. , aes(x = age, y = driv_prop, colour = sex, group = sex)) +
  geom_point(alpha = 0.2) +
  stat_smooth() + 
  facet_wrap( ~ wave_year) + 
  labs(
    title = "Proportion with driving licence by BHPS wave, age and sex",
    x = "Age (years)", 
    y = "Proportion holding driving licence"
  )
#ggsave("figures/prop_drive_facet_by_waveyear.png", height = 30, width = 30, units = "cm", dpi = 300)



# Proportion of driving license owners (DLOs) who drive ------------


all_inds_drvs %>% 
  mutate(
    year = wave + 1990
  ) %>% 
  filter(!is.na(sex) & !is.na(age) & !is.na(year) & !is.na(dlo) & !is.na(cu)) %>% 
  filter( year > 1992) %>% 
  filter(dlo == "yes") %>% 
  select(age, year,  cu) %>% 
  group_by(age, year, cu) %>% 
  tally %>% 
  spread(cu, n, fill = 0) %>% 
  mutate(
         drivers_driving = yes / (yes + no)
  ) %>% 
  filter(age < 80 & age > 17) %>% 
  ggplot(.) +
  geom_raster(mapping=aes(x=year, y = age, fill = drivers_driving)) + 
  scale_fill_gradientn(limits = c(0, 1), breaks = c(0, 0.2, 0.4, 0.6, 0.8, 1), colours = rainbow(7)) + 
  theme_minimal() + 
  labs(
    title = "Proportions of drivers driving",
    x = "Year",
    y = "Age in years",
    fill = "Drivers driving"
  )

ggsave("figures/prop_dlo_driving.png", height = 15, width =15, dpi = 300, units = "cm")




# Proportion of drivers driving by sex
all_inds_drvs %>% 
  mutate(
    year = wave + 1990
  ) %>% 
  mutate(
    age_grp = 
      cut(
        age, 
        c(0, 20, 25, 35, 50, 60, 70, 999), 
        labels = c("<20", "20-25", "26-35", "36-50", "51-60", "61-70", ">70")
      )
  ) %>% 
  filter(!is.na(sex) & !is.na(age) & !is.na(year) & !is.na(dlo) & !is.na(cu)) %>% 
  filter(year %in% c(1995, 2000, 2005)) %>%
  filter(dlo == "yes") %>% 
  select(age_grp, sex, year,  cu) %>% 
  group_by(age_grp, sex, year, cu) %>% 
  tally %>% 
  spread(cu, n, fill = 0) %>% 
  mutate(
    drivers_driving = yes / (yes + no),
    se_drdr = (drivers_driving * (1 - drivers_driving) / (yes + no)) ^ 0.5
  ) %>% 
  ggplot(., aes(
    x = age_grp, 
    y = drivers_driving, group = factor(year), colour = factor(year), shape = factor(year))) +
  geom_line() + geom_point() + 
  geom_linerange(
    aes(ymax = drivers_driving + 2 * se_drdr, ymin = drivers_driving - 2 * se_drdr)
  ) + 
  labs(
    x = "Age Group", 
    y = "Proportion of drivers driving", 
    colour = "Year", group = "Year", shape = "Year") + 
  scale_y_continuous(limits = c(0, 1)) + 
  facet_wrap(~ sex)


# Proportion of drivers driving by sex and highest qualification
all_inds_drvs %>% 
  mutate(
    year = wave + 1990
  ) %>% 
  mutate(
    age_grp = 
      cut(
        age, 
        c(0, 20, 25, 35, 50, 60, 70, 999), 
        labels = c("<20", "20-25", "26-35", "36-50", "51-60", "61-70", ">70")
      )
  ) %>% 
  filter(!is.na(sex) & !is.na(age) & !is.na(year) & !is.na(dlo) & !is.na(highqual) & !is.na(cu)) %>% 
  filter(year %in% c(1995, 2000, 2005)) %>%
  filter(dlo == "yes") %>% 
  select(age_grp, sex, year,  highqual, cu) %>% 
  group_by(age_grp, sex, year, highqual, cu) %>% 
  tally %>% 
  spread(cu, n, fill = 0) %>% 
  mutate(
    drivers_driving = yes / (yes + no),
    se_drdr = (drivers_driving * (1 - drivers_driving) / (yes + no)) ^ 0.5
  ) %>% 
  ggplot(., aes(
    x = age_grp, 
    y = drivers_driving, group = factor(year), colour = factor(year), shape = factor(year))) +
  geom_line() + geom_point() + 
  geom_linerange(
    aes(ymax = drivers_driving + 2 * se_drdr, ymin = drivers_driving - 2 * se_drdr)
  ) + 
  labs(
    x = "Age Group", 
    y = "Proportion of drivers driving", 
    colour = "Year", group = "Year", shape = "Year") + 
  scale_y_continuous(limits = c(0, 1)) + 
  facet_grid(highqual ~ sex)

# Proportions driving by age and urban rural class

all_inds_drvs %>% 
  mutate(
    year = wave + 1990
  ) %>% 
  mutate(
    age_grp = 
      cut(
        age, 
        c(0, 20, 25, 35, 50, 60, 70, 999), 
        labels = c("<20", "20-25", "26-35", "36-50", "51-60", "61-70", ">70")
      )
  ) %>% 
  filter(!is.na(sex) & !is.na(age) & !is.na(year) & !is.na(dlo) & !is.na(ur_group) & !is.na(cu)) %>% 
  filter(year %in% c(1995, 2000, 2005)) %>%
  filter(dlo == "yes") %>% 
  select(age_grp, sex, year,  ur_group, cu) %>% 
  group_by(age_grp, sex, year, ur_group, cu) %>% 
  tally %>% 
  spread(cu, n, fill = 0) %>% 
  mutate(
    drivers_driving = yes / (yes + no),
    se_drdr = (drivers_driving * (1 - drivers_driving) / (yes + no)) ^ 0.5
  ) %>% 
  ggplot(., aes(
    x = age_grp, 
    y = drivers_driving, group = factor(ur_group), colour = factor(ur_group), shape = factor(ur_group))) +
  geom_line() + geom_point() + 
  geom_linerange(
    aes(ymax = drivers_driving + 2 * se_drdr, ymin = drivers_driving - 2 * se_drdr)
  ) + 
  labs(
    x = "Age Group", 
    y = "Proportion of drivers driving", 
    colour = "Year", group = "Year", shape = "Year") + 
  scale_y_continuous(limits = c(0, 1)) + 
  facet_grid(year ~ sex)


# Proportions with driving licences  --------------------------------------


# lattice levelplot version - proportion of drivers driving overall
all_inds_drvs %>% 
  mutate(
    year = wave + 1990
  ) %>% 
  filter(!is.na(sex) & !is.na(age) & !is.na(year) & !is.na(dlo) & !is.na(cu)) %>% 
  filter( year > 1992) %>% 
  select(age, year,  dlo) %>% 
  group_by(age, year, dlo) %>% 
  tally %>% 
  spread(dlo, n, fill = 0) %>% 
  mutate(
    has_driving_licence = yes / (yes + no)
  ) %>% 
  filter(age < 80 & age > 17) %>% 
  levelplot(
    has_driving_licence ~ year * age , 
    data=., 
    aspect = "iso",
    strip=strip.custom(par.strip.text=list(cex=1.4, fontface="bold"), bg="grey"),
    ylab=list(label="Age in years", cex=1.4),
    xlab=list(label="Year", cex=1.4),
    cex=1.4,
    col.regions= rev(colorRampPalette(brewer.pal(12, "Paired"))(200)),
    main=NULL,
    at = seq(0, 1, by = 0.01),
    labels=list(cex=1.2),
    col="black",
    scales=list(
      x=list(cex=1.4), 
      y=list(cex=1.4),
      alternating=3
    )
  ) 
# dev.off()

# lattice levelplot version - proportion of drivers driving  - by sex

all_inds_drvs %>% 
  mutate(
    year = wave + 1990
  ) %>% 
  filter(!is.na(sex) & !is.na(age) & !is.na(year) & !is.na(dlo) & !is.na(cu)) %>% 
  filter( year > 1992) %>% 
  filter(dlo == "yes") %>% 
  select(age, year,  sex, cu) %>% 
  group_by(age, year, sex, cu) %>% 
  tally %>% 
  spread(cu, n, fill = 0) %>% 
  mutate(
    drivers_driving = yes / (yes + no)
  ) %>% 
  filter(age < 80 & age > 17) %>% 
  levelplot(
    drivers_driving ~ year * age | sex, 
    data=., 
    aspect = "iso",
    strip=strip.custom(par.strip.text=list(cex=1.4, fontface="bold"), bg="grey"),
    ylab=list(label="Age in years", cex=1.4),
    xlab=list(label="Year", cex=1.4),
    cex=1.4,
    col.regions= rev(colorRampPalette(brewer.pal(12, "Paired"))(200)),
    main=NULL,
    at = seq(0, 1, by = 0.01),
    labels=list(cex=1.2),
    col="black",
    scales=list(
      x=list(cex=1.4), 
      y=list(cex=1.4),
      alternating=3
    )
  ) 

# proportion of drivers driving, by sex and highest qual
all_inds_drvs %>% 
  mutate(
    year = wave + 1990
  ) %>% 
  filter(!is.na(sex) & !is.na(age) & !is.na(year) & !is.na(dlo) & !is.na(cu) & !is.na(highqual)) %>% 
  filter( year > 1992) %>% 
  mutate(highqual = recode(
    highqual, 
    "
    'further non-vocational' = 'H';
    'further vocational' = 'M';
    'no further' = 'L'
    ",
    levels = c("L", "M", "H"), as.factor.result =T
  ),
  sex = recode(sex, "'male' = 'M'; 'female' = 'F'")
  ) %>%  
  select(age, year, sex, highqual, cu) %>% 
  group_by(age, year, sex, highqual, cu) %>% 
  tally %>% 
  spread(cu, n, fill = 0) %>% 
  mutate(
    drivers_driving = yes / (yes + no)
  ) %>% 
  filter(age < 80 & age > 17) %>% 
  levelplot(
    drivers_driving ~ year * age | sex + highqual, 
    data=., 
    aspect = "iso",
    strip=strip.custom(par.strip.text=list(cex=1.4, fontface="bold"), bg="grey"),
    ylab=list(label="Age in years", cex=1.4),
    xlab=list(label="Year", cex=1.4),
    cex=1.4,
    col.regions= rev(colorRampPalette(brewer.pal(12, "Paired"))(200)),
    main=NULL,
    at = seq(0, 1, by = 0.02),
    labels=list(cex=1.2),
    col="black",
    scales=list(
      x=list(cex=1.4), 
      y=list(cex=1.4),
      alternating=3
    )
  ) 



# lattice levelplot version - proportion of drivers driving overall
all_inds_drvs %>% 
  mutate(
    year = wave + 1990
  ) %>% 
  filter(!is.na(sex) & !is.na(age) & !is.na(year) & !is.na(dlo)) %>% 
  filter( year > 1992) %>% 
  select(year, age, dlo) %>% 
  arrange(year, age) %>% 
  group_by(year, age) %>% 
  mutate(does_drive = recode(dlo, "'yes' = 1; 'no' = '0'; else = NA")) %>% 
  summarise(drive_prop = mean(does_drive, na.rm=T)) %>% 
  filter(age < 80 & age > 17) %>% 
  levelplot(
    drive_prop ~ year * age , 
    data=., 
    aspect = "iso",
    strip=strip.custom(par.strip.text=list(cex=1.4, fontface="bold"), bg="grey"),
    ylab=list(label="Age in years", cex=1.4),
    xlab=list(label="Year", cex=1.4),
    cex=1.4,
    col.regions= rev(colorRampPalette(brewer.pal(12, "Paired"))(200)),
    main=NULL,
    at = seq(0, 1, by = 0.01),
    labels=list(cex=1.2),
    col="black",
    scales=list(
      x=list(cex=1.4), 
      y=list(cex=1.4),
      alternating=3
    )
  ) 
# dev.off()

# lattice levelplot version - proportion of drivers driving  - by sex
all_inds_drvs %>% 
  mutate(
    year = wave + 1990
  ) %>% 
  filter(!is.na(sex) & !is.na(age) & !is.na(year) & !is.na(dlo)) %>% 
  filter( year > 1992) %>% 
  select(year, age, sex, dlo) %>% 
  arrange(year, sex, age) %>% 
  group_by(year, sex, age) %>% 
  mutate(does_drive = recode(dlo, "'yes' = 1; 'no' = '0'; else = NA")) %>% 
  summarise(drive_prop = mean(does_drive, na.rm=T)) %>% 
  filter(age < 80 & age > 17) %>% 
  levelplot(
    drive_prop ~ year * age | sex , 
    data=., 
    aspect = "iso",
    strip=strip.custom(par.strip.text=list(cex=1.4, fontface="bold"), bg="grey"),
    ylab=list(label="Age in years", cex=1.4),
    xlab=list(label="Year", cex=1.4),
    cex=1.4,
    col.regions= rev(colorRampPalette(brewer.pal(12, "Paired"))(200)),
    main=NULL,
    at = seq(0, 1, by = 0.01),
    labels=list(cex=1.2),
    col="black",
    scales=list(
      x=list(cex=1.4), 
      y=list(cex=1.4),
      alternating=3
    )
  ) 

# proportion of drivers driving, by sex and highest qual
all_inds_drvs %>% 
  mutate(
    year = wave + 1990
  ) %>% 
  filter(!is.na(sex) & !is.na(age) & !is.na(year) & !is.na(dlo) & !is.na(highqual)) %>% 
  filter( year > 1992) %>% 
  mutate(highqual = recode(
    highqual, 
    "
      'further non-vocational' = 'H';
      'further vocational' = 'M';
      'no further' = 'L'
    ",
    levels = c("L", "M", "H"), as.factor.result =T
  ),
  sex = recode(sex, "'male' = 'M'; 'female' = 'F'")
  ) %>%  
  select(year, age, sex, highqual, dlo) %>% 
  arrange(year, sex, highqual, age) %>% 
  group_by(year, sex, highqual, age) %>% 
  mutate(does_drive = recode(dlo, "'yes' = 1; 'no' = '0'; else = NA")) %>% 
  summarise(drive_prop = mean(does_drive, na.rm=T)) %>% 
  filter(age < 80 & age > 17) %>% 
  levelplot(
    drive_prop ~ year * age | sex + highqual, 
    data=., 
    aspect = "iso",
    strip=strip.custom(par.strip.text=list(cex=1.4, fontface="bold"), bg="grey"),
    ylab=list(label="Age in years", cex=1.4),
    xlab=list(label="Year", cex=1.4),
    cex=1.4,
    col.regions= rev(colorRampPalette(brewer.pal(12, "Paired"))(200)),
    main=NULL,
    at = seq(0, 1, by = 0.02),
    labels=list(cex=1.2),
    col="black",
    scales=list(
      x=list(cex=1.1, rot = 90), 
      y=list(cex=1.1),
      alternating=3
    )
  ) 



# Focus on 20-35 year olds 

# proportion of drivers driving, by sex and highest qual
all_inds_drvs %>% 
  mutate(
    year = wave + 1990
  ) %>% 
  filter(!is.na(sex) & !is.na(age) & !is.na(year) & !is.na(dlo) & !is.na(highqual)) %>% 
  filter( year > 1992) %>% 
  mutate(highqual = recode(
    highqual, 
    "
    'further non-vocational' = 'H';
    'further vocational' = 'M';
    'no further' = 'L'
    ",
    levels = c("L", "M", "H"), as.factor.result =T
  ),
  sex = recode(sex, "'male' = 'M'; 'female' = 'F'")
  ) %>%  
  select(year, age, sex, highqual, dlo) %>% 
  arrange(year, sex, highqual, age) %>% 
  group_by(year, sex, highqual, age) %>% 
  mutate(does_drive = recode(dlo, "'yes' = 1; 'no' = '0'; else = NA")) %>% 
  summarise(drive_prop = mean(does_drive, na.rm=T)) %>% 
  filter(age <= 35 & age >= 20) %>% 
  levelplot(
    drive_prop ~ year * age | sex + highqual, 
    data=., 
    aspect = "iso",
    strip=strip.custom(par.strip.text=list(cex=1.4, fontface="bold"), bg="grey"),
    ylab=list(label="Age in years", cex=1.4),
    xlab=list(label="Year", cex=1.4),
    cex=1.4,
    col.regions= rev(colorRampPalette(brewer.pal(12, "Paired"))(200)),
    main=NULL,
    at = seq(0, 1, by = 0.02),
    labels=list(cex=1.2),
    col="black",
    scales=list(
      x=list(cex=1.1, rot = 90), 
      y=list(cex=1.1),
      alternating=3
    )
  ) 


# Proportion with driving licence, by urban/rural class
all_inds_drvs %>% 
  mutate(
    year = wave + 1990
  ) %>% 
  filter(!is.na(sex) & !is.na(age) & !is.na(year) & !is.na(dlo) & !is.na(ur_group)) %>% 
  filter( year > 1992) %>% 
  mutate(ur_group = recode(
    ur_group, 
    "
    'urban' = 'U';
    'nonurban' = 'NU'
    ",
    as.factor.result =T
  ),
  sex = recode(sex, "'male' = 'M'; 'female' = 'F'")
  ) %>%  
  select(year, age, sex, ur_group, dlo) %>% 
  arrange(year, sex, ur_group, age) %>% 
  group_by(year, sex, ur_group, age) %>% 
  mutate(does_drive = recode(dlo, "'yes' = 1; 'no' = '0'; else = NA")) %>% 
  summarise(drive_prop = mean(does_drive, na.rm=T)) %>% 
  filter(age <= 80 & age >= 17) %>% 
  levelplot(
    drive_prop ~ year * age | sex + ur_group, 
    data=., 
    aspect = "iso",
    strip=strip.custom(par.strip.text=list(cex=1.4, fontface="bold"), bg="grey"),
    ylab=list(label="Age in years", cex=1.4),
    xlab=list(label="Year", cex=1.4),
    cex=1.4,
    col.regions= rev(colorRampPalette(brewer.pal(12, "Paired"))(200)),
    main=NULL,
    at = seq(0, 1, by = 0.02),
    labels=list(cex=1.2),
    col="black",
    scales=list(
      x=list(cex=1.1, rot = 90), 
      y=list(cex=1.1),
      alternating=3
    )
  ) 


# Proportion with GHQ > 2, by sex

all_inds_drvs %>% 
  mutate(
    year = wave + 1990
  ) %>% 
  filter(!is.na(sex) & !is.na(age) & !is.na(year) & !is.na(ghq)) %>%
  select(sex, age, year, ghq) %>% 
  mutate(highghq = cut(ghq, c(-Inf, 2, Inf), labels = c("low", "high"), ordered_result = T)) %>% 
  filter( year > 1992) %>%
  select(-ghq) %>% 
  group_by(sex, age, year, highghq) %>% 
  tally() %>% 
  spread(highghq, n) %>% 
  mutate(prop_hq = high / (low + high)) %>% 
  filter(age <= 80 & age >= 17) %>% 
  levelplot(
    prop_hq ~ year * age | sex, 
    data=., 
    aspect = "iso",
    strip=strip.custom(par.strip.text=list(cex=1.4, fontface="bold"), bg="grey"),
    ylab=list(label="Age in years", cex=1.4),
    xlab=list(label="Year", cex=1.4),
    cex=1.4,
    col.regions= rev(colorRampPalette(brewer.pal(12, "Paired"))(200)),
    main=NULL,
    at = seq(0, 1, by = 0.02),
    labels=list(cex=1.2),
    col="black",
    scales=list(
      x=list(cex=1.1, rot = 90), 
      y=list(cex=1.1),
      alternating=3
    )
  ) 

# Proportion with GHQ > 2, by sex and highqual
all_inds_drvs %>% 
  mutate(
    year = wave + 1990
  ) %>% 
  filter(!is.na(sex) & !is.na(age) & !is.na(year) & !is.na(ghq) & !is.na(highqual)) %>%
  select(sex, age, year, highqual, ghq) %>%
  mutate(highqual = recode(
    highqual, 
    "
    'further non-vocational' = 'H';
    'further vocational' = 'M';
    'no further' = 'L'
    ",
    levels = c("L", "M", "H"), as.factor.result =T
    ),
    sex = recode(sex, "'male' = 'M'; 'female' = 'F'")
  ) %>% 
  mutate(highghq = cut(ghq, c(-Inf, 2, Inf), labels = c("low", "high"), ordered_result = T)) %>% 
  filter( year > 1992) %>%
  select(-ghq) %>% 
  group_by(sex, age, year, highqual, highghq) %>% 
  tally() %>% 
  spread(highghq, n) %>% 
  mutate(prop_hq = high / (low + high)) %>% 
  filter(age <= 80 & age >= 17) %>% 
  levelplot(
    prop_hq ~ year * age | sex + highqual, 
    data=., 
    aspect = "iso",
    strip=strip.custom(par.strip.text=list(cex=1.4, fontface="bold"), bg="grey"),
    ylab=list(label="Age in years", cex=1.4),
    xlab=list(label="Year", cex=1.4),
    cex=1.4,
    col.regions= rev(colorRampPalette(brewer.pal(12, "Paired"))(200)),
    main=NULL,
    at = seq(0, 1, by = 0.02),
    labels=list(cex=1.2),
    col="black",
    scales=list(
      x=list(cex=1.1, rot = 90), 
      y=list(cex=1.1),
      alternating=3
    )
  ) 


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
  filter(year >=1993) %>% 
  filter(age >= 20) %>% 
  arrange(brth_chrt_grp, sex, age) %>% 
  group_by(brth_chrt_grp, sex, age) %>% 
  mutate(does_drive = car::recode(dlo, "'yes' = 1; 'no' = '0'; else = NA")) %>% 
  summarise(driv_prop = mean(does_drive, na.rm=T)) %>% 
  ggplot(. ) +
  geom_line(
    aes(
      x = age, 
      y = driv_prop, 
      colour = brth_chrt_grp, 
      group = brth_chrt_grp,
      linetype = brth_chrt_grp,
      size = brth_chrt_grp
    )) + 
  facet_wrap( ~ sex) +
  labs(
    title = "Proportion holding driving licence by decadal birth cohort",
    x = "Age of cohort members (year)",
    y = "Proportion holding driving licence"
  ) +
  scale_color_discrete(name = "Birth Cohort") + 
  scale_size_manual(name = "Birth Cohort", values = c(1,1.5, 1, 1.5,1,1.5)) + 
  scale_linetype_manual(name = "Birth Cohort", values = c(1, 1, 2, 2, 3,3)) +
  scale_x_continuous(breaks = seq(20, 80, 5)) + 
  scale_y_continuous(breaks = seq(0.6, 1, 0.05)) + 
  theme_minimal()



ggsave("figures/propdrive_cohortgrp_sex_age.png", height = 12, width = 15, dpi = 300, units = "cm")

# From this a quick table showing the proportion of those aged 25 who drive by birth cohort decade
# looks a good idea

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
  filter(year >=1993) %>% 
  filter(age >= 20) %>% 
  arrange(brth_chrt_grp, sex, age) %>% 
  group_by(brth_chrt_grp, sex, age) %>% 
  select(brth_chrt_grp, wave, sex, age, highqual, dlo, cu) %>% 
  filter(age == 25) -> status_at_age_25


xtabs( ~ brth_chrt_grp +sex + dlo, data = status_at_age_25,
       drop.unused.levels = T) %>% 
  as.data.frame() %>% 
  spread(dlo, Freq) %>% 
  mutate(proportion = yes / (yes + no)) %>% 
  mutate(se_prop = ((proportion)* (1 - proportion) / (yes + no))^0.5) -> props_dlo_sex

props_dlo_sex %>% 
  ggplot(., aes(x = brth_chrt_grp, group = sex, linetype = sex, colour = sex, shape = sex)) + 
  geom_point(aes(y = proportion)) + 
  geom_linerange(
    aes(ymax = proportion + 2 * se_prop, ymin = proportion - 2 * se_prop)
  ) +
  labs(x = "Birth cohort (decade)", y = "Proportion with licence at age 25")
ggsave("figures/prop_licence_at_age25_sex.png", width = 15, height = 15, units = "cm", dpi = 300 )

xtabs( ~ brth_chrt_grp +sex + cu, data = status_at_age_25,
       drop.unused.levels = T) %>% 
  as.data.frame() %>% 
  spread(cu, Freq) %>% 
  mutate(proportion = yes / (yes + no)) %>% 
  mutate(se_prop = ((proportion)* (1 - proportion) / (yes + no))^0.5) -> props_cu_sex

props_cu_sex %>% 
  ggplot(., aes(x = brth_chrt_grp, group = sex, linetype = sex, colour = sex, shape = sex)) + 
  geom_point(aes(y = proportion)) + 
  geom_linerange(
    aes(ymax = proportion + 2 * se_prop, ymin = proportion - 2 * se_prop)
  ) +
  labs(x = "Birth cohort (decade)", y = "Proportion who drive at age 25")
ggsave("figures/prop_drives_at_age25_sex.png", width = 15, height = 15, units = "cm", dpi = 300 )

# By educational status

xtabs( ~ brth_chrt_grp +sex + highqual + dlo, data = status_at_age_25,
       drop.unused.levels = T) %>% 
  as.data.frame() %>% 
  spread(dlo, Freq) %>% 
  mutate(proportion = yes / (yes + no)) %>% 
  mutate(se_prop = ((proportion)* (1 - proportion) / (yes + no))^0.5) -> props_cu_sex_qual

props_dlo_sex_qual %>% 
  mutate(highqual = car::recode(highqual, "'no further' = 'low'; 'further vocational' = 'med'; 'further non-vocational' = 'high'")) %>% 
  mutate(highqual = factor(highqual, levels = c("low", "med", "high"), 
                           ordered = T)
         ) %>% 
  ggplot(., aes(x = brth_chrt_grp, group = sex, linetype = sex, colour = sex, shape = sex)) + 
  geom_point(aes(y = proportion)) + 
  geom_linerange(
    aes(ymax = proportion + 2 * se_prop, ymin = proportion - 2 * se_prop)
  ) +
  facet_wrap(~highqual) + 
  labs(x = "Birth cohort (decade)", y = "Proportion with licence at age 25")
ggsave("figures/prop_licence_at_age25_sex_qual.png", width = 20, height = 15, units = "cm", dpi = 300 )


xtabs( ~ brth_chrt_grp +sex + highqual + cu, data = status_at_age_25,
       drop.unused.levels = T) %>% 
  as.data.frame() %>% 
  spread(cu, Freq) %>% 
  mutate(proportion = yes / (yes + no)) %>% 
  mutate(se_prop = ((proportion)* (1 - proportion) / (yes + no))^0.5) -> props_cu_sex_qual

props_cu_sex_qual %>% 
  mutate(highqual = car::recode(highqual, "'no further' = 'low'; 'further vocational' = 'med'; 'further non-vocational' = 'high'")) %>% 
  mutate(highqual = factor(highqual, levels = c("low", "med", "high"), 
                           ordered = T)
  ) %>% 
  ggplot(., aes(x = brth_chrt_grp, group = sex, linetype = sex, colour = sex, shape = sex)) + 
  geom_point(aes(y = proportion)) + 
  geom_linerange(
    aes(ymax = proportion + 2 * se_prop, ymin = proportion - 2 * se_prop)
  ) +
  facet_wrap(~highqual) + 
  labs(x = "Birth cohort (decade)", y = "Proportion driving at age 25")
ggsave("figures/prop_drives_at_age25_sex_qual.png", width = 20, height = 15, units = "cm", dpi = 300 )





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
  
  filter(age < 80 & age >= 20) %>% 
  levelplot(
    prop_driving ~ year * age | sex, 
    data=., 
    region=T, 
    strip=strip.custom(par.strip.text=list(cex=1.4, fontface="bold"), bg="grey"),
    ylab=list(label="Age in years", cex=1.4),
    xlab=list(label="Year", cex=1.4),
    cex=1.4,
    col.regions= colorRampPalette(brewer.pal(6, "Spectral"))(200),
    main=NULL,
    at = seq(0, 1, by = 0.025),
    labels=list(cex=1.2),
    col="black",
    scales=list(
      x=list(cex=1.4), 
      y=list(cex=1.4),
      alternating=3
    )
  ) 

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
# and sex*gender interaction. Possibly a recent cohort effect 
ggsave("figures/levelplot_propdrive_sex_qual.png", height = 30, width = 30, dpi = 300, units = "cm")


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
  levelplot(
    prop_driving ~ year * age | sex + highqual , 
    data=., 
    region=T, 
    strip=strip.custom(par.strip.text=list(cex=1.4, fontface="bold"), bg="grey"),
    ylab=list(label="Age in years", cex=1.4),
    xlab=list(label="Year", cex=1.4),
    cex=1.4,
    col.regions= colorRampPalette(brewer.pal(6, "Spectral"))(200),
    main=NULL,
    at = seq(0, 1, by = 0.025),
    labels=list(cex=1.2),
    col="black",
    scales=list(
      x=list(cex=1.4), 
      y=list(cex=1.4),
      alternating=3
    )
  ) 


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

