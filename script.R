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

require(plyr)
require(stringr)
require(tidyr)
require(car)

require(dplyr)

require(ggplot2)
require(lattice)


# Data - individual  ------------------------------------------------------



dta_path <- "E:/data/bhps/unzipped/UKDA-5151-tab/tab/"
dta_files <- list.files(path = "E:/data/bhps/unzipped/UKDA-5151-tab/tab/", pattern = "indresp\\.tab")

all_inds <- llply(
  paste0(dta_path, dta_files), 
  read_delim,
  delim = "\t"
  )
  
fn <- function(x){
  nms <- names(x)
  
  selection <- str_detect(
    nms ,
    pattern = "^PID$"
    ) | str_detect(
      nms , 
      pattern = "^[A-Z]{1}SEX"
    ) | str_detect(
      nms , 
      pattern = "^[A-Z]{1}HID"
    ) | str_detect(
      nms , 
      "^[A-Z]{1}DRIVER"
    ) | str_detect(
      nms , 
      "^[A-Z]{1}CARUSE"
    ) | str_detect(
      nms , 
      "^[A-Z]{1}HLGHQ2"
    ) | str_detect(
      nms , 
      "^[A-Z]{1}AGE$"
    ) | str_detect(
      nms , 
      "^[A-Z]{1}FEEND$" # Further education leaving age
    ) | str_detect(
      nms , 
      "^[A-Z]{1}FENOW$" # Still in further education
    ) | str_detect(
      nms , 
      "^[A-Z]{1}ISCED$" # ISCED level (highest qualification)
    ) | str_detect(
      nms , 
      "^[A-Z]{1}PLBORNC$" # Country of birth
    ) | str_detect(
      nms , 
      "^[A-Z]{1}RACE$" # ethnic group membership
    ) | str_detect(
      nms , 
      "^[A-Z]{1}RACEL$" # ethnic group membership (long version)
    ) | str_detect(
      nms , 
      "^[A-Z]{1}NEIGH$" # neighbourhood good place to live
    )  | str_detect(
      nms , 
      "^[A-Z]{1}OPNGBH[A-H]{1}$" # see below
    )
  
    
#   OPNGBHA # feels belongs to neighbourhood
#   OPNGBHB # local friends mean a lot
#   OPNGBHC # advice obtanable locally
#   OPNGBHD # can borrow things from neighbours
#   OPNGBHE # willing to improve neighbourhood
#   OPNGBHF # plan to stay in neighbourhood
#   OPNGBHG # am similar to others in neighbourhood
#   OPNGBHH # talk regularly to neighbourhood
  

  
  out <- x[,selection]
  tmp <- names(out)
  WAVE <- tmp[str_detect(tmp, pattern = "^[A-Z]{1}SEX")]  %>% str_replace(., "SEX", "")
  PID <- out$PID
  out <- out %>% select(-PID)
  names(out) <- names(out) %>% str_replace_all("^[A-Z]{1}", "")
  out <- data.frame(PID = PID, WAVE = WAVE, out)
  return(out)
}

all_inds_ss <- llply(
  all_inds,
  fn
)


# variable with pid, wave, sex, car_driver (derived), age, ghq

fn <- function(x){
  out <- x %>% select(pid = PID, hid = HID, sex = SEX, age = AGE, ghq = HLGHQ2)
  out <- out %>% mutate(
    sex = recode(sex, "1 = 'male'; 2 = 'female'; else = NA"),
    ghq = ifelse(ghq < 0, NA, ghq),
    age = ifelse(age < 0, NA, age)
  )
  out$neigh <- NA
  if ("NEIGH" %in% names(x)){
    out$neigh <- recode(
      x$NEIGH, 
      "
      1 = 'yes';
      2 = 'no'; 
      3 = 'mixed';
      else = NA
      ")
  }
  out$isced <- recode(
    x$ISCED, 
    "
      0 = 'not defined';
      1 = 'primary'; 
      2 = 'low secondary';
      3 = 'low sec-voc';
      4 = 'hisec mivoc';
      5 = 'higher voc';
      6 = 'first degree';
      7 = 'higher degree';
      else = NA
      "
    )
  
  out$highqual <- recode(
    out$isced,
    "
    c('not defined', 'primary', 'secondary') = 'no further';
    c('low sec-voc', 'hisec mivoc', 'higher voc') = 'further vocational';
    c('first degree', 'higher degree') = 'further non-vocational';
    else = NA
    "
  )
  
  out$drives <- NA
  if (x$WAVE[1] %in% c("A", "B")){
    out$drives[x$DRIVER==1] <- "yes"
    out$drives[x$DRIVER==2] <- "no"
  } else {
    out$drives[x$CARUSE==3] <- "no"
    out$drives[x$CARUSE==1 | x$CARUSE == 2] <- "yes"
  }
  out$wave <- which(LETTERS %in% x$WAVE)
  out <- out %>% select(pid, hid, wave, sex, age, drives, ghq, neigh, isced, highqual)
  return(out)
}

all_inds_drvs <- ldply(all_inds_ss, fn) %>% tbl_df

# Vis of individual level covariates --------------------------------------

# prop who like neighbourhood by wave, sex and driver status
all_inds_drvs %>% 
  filter(!is.na(neigh) & !is.na(drives)) %>% 
  group_by(sex, wave, drives, neigh) %>% 
  tally %>% 
  spread(neigh, n) %>% 
  mutate(like_prop = yes/ (mixed + no + yes)) %>% 
  ggplot(., mapping = aes(x = factor(wave), y = like_prop, colour = drives, group = drives)) +
  geom_line() + geom_point() + 
  facet_wrap(~sex)

#prop who like neighbourhood by wave, age group, sex and driver status
all_inds_drvs %>% 
  filter(!is.na(neigh) & !is.na(drives)) %>% 
  mutate(age_grp = 
           cut(age, 
               breaks = c(17, 21, 26, 35, 50, 65, 80), 
               include.lowest = T, 
               labels = c("17-20", "21-25", "26-35", "36-49", "50-64", "65-79")
           )
  ) %>% 
  filter(!is.na(age_grp)) %>% 
  group_by(age_grp, sex, wave, drives, neigh) %>% 
  tally %>% 
  spread(neigh, n) %>% 
  mutate(like_prop = yes/ (mixed + no + yes)) %>% 
  ggplot(., mapping = aes(x = factor(wave), y = like_prop, colour = drives, group = drives)) +
  geom_line() + geom_point() + 
  facet_grid(sex ~age_grp) + 
  labs(x = "BPHS Wave", y = "proportion of respondents who like their neighbourhood")

# This suggests there's a particularly high level of neighbourhood dissatisfaction amongst 
# Males aged 26-35 who do not drive, particularly in wave 8 (H)


all_inds_drvs %>% mutate(
  age_grp = ntile(age, 10)
  ) %>% 
  filter(!is.na(sex)) %>% 
  group_by(sex, age_grp, drives) %>% 
  summarise(mn_ghq = mean(ghq, na.rm=T)) %>% 
  ggplot(.) + 
  geom_bar(
    aes(x = age_grp, y = mn_ghq, group = sex, colour = sex, fill = sex), 
    position = "dodge", stat= "identity"
    ) + facet_wrap(~ drives)


# Proportion who drive by age, all years (so double counting etx)
all_inds_drvs %>%   
  filter(!is.na(sex)) %>% 
  arrange(sex, age) %>% 
  group_by(sex, age) %>% 
  mutate(does_drive = recode(drives, "'yes' = 1; 'no' = '0'; else = NA")) %>% 
  summarise(driv_prop = mean(does_drive, na.rm=T)) %>% 
  ggplot(. ) +
  geom_line(aes(x = age, y = driv_prop, colour = sex, group = sex))

# Proportion who drive by age, faceted by wave 
all_inds_drvs %>%   
  filter(!is.na(sex)) %>% 
  arrange(sex, age) %>% 
  group_by(wave, sex, age) %>%
  filter(age <= 80) %>% 
  mutate(does_drive = recode(drives, "'yes' = 1; 'no' = '0'; else = NA")) %>% 
  summarise(driv_prop = mean(does_drive, na.rm=T)) %>% 
  ggplot(. ) +
  geom_line(aes(x = age, y = driv_prop, colour = sex, group = sex)) + 
  facet_wrap( ~ wave)
# This suggests a much higher proportion of males who drive from mid 20s onwards


all_inds_drvs %>%   
  filter(!is.na(sex)) %>% 
  arrange(sex, age) %>% 
  group_by(wave, sex, age) %>% 
  mutate(does_drive = recode(drives, "'yes' = 1; 'no' = '0'; else = NA")) %>% 
  summarise(driv_prop = mean(does_drive, na.rm=T)) %>% 
  ggplot(. ) +
  geom_line(aes(x = age, y = driv_prop)) + 
  facet_grid(wave ~ sex)

all_inds_drvs %>%   
  filter(!is.na(sex)) %>% 
  arrange(sex, age) %>% 
  group_by(wave, sex, age) %>% 
  mutate(does_drive = recode(drives, "'yes' = 1; 'no' = '0'; else = NA")) %>% 
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
  arrange(wave_grp, sex, age) %>% 
  group_by(wave_grp, sex, age) %>% 
  mutate(does_drive = recode(drives, "'yes' = 1; 'no' = '0'; else = NA")) %>% 
  summarise(driv_prop = mean(does_drive, na.rm=T)) %>% 
  ggplot(. ) +
  geom_line(
    aes(
      x = age, 
      y = driv_prop, 
      colour = wave_grp, 
      group = wave_grp
    )) + 
  facet_wrap( ~ sex)


# Now to look at cohorts

all_inds_drvs %>% 
  mutate(
    year = wave + 1990,
    birth_cohort = year - age,
    brth_chrt_grp = cut(
      birth_cohort, 
      breaks= c(1930, 1940, 1950, 1960, 1970, 1980, 1990), 
      include.lowest = T, 
      labels = c("1930s", "1940s", "1950s", "1960s", "1970s", "1980s")
                        )
  ) %>% 
  filter(!is.na(brth_chrt_grp) & !is.na(sex)) %>% 
  arrange(brth_chrt_grp, sex, age) %>% 
  group_by(brth_chrt_grp, sex, age) %>% 
  mutate(does_drive = recode(drives, "'yes' = 1; 'no' = '0'; else = NA")) %>% 
  summarise(driv_prop = mean(does_drive, na.rm=T)) %>% 
  ggplot(. ) +
  geom_line(
    aes(
      x = age, 
      y = driv_prop, 
      colour = brth_chrt_grp, 
      group = brth_chrt_grp
    )) + 
  facet_wrap( ~ sex)


# Data in Lexis compatible format


all_inds_drvs %>% 
  mutate(
    year = wave + 1990
  ) %>% 
  filter(!is.na(sex) & !is.na(age) & !is.na(year) & !is.na(drives)) %>% 
  select(sex, age, year, drives) %>% 
  group_by(sex, age, year, drives) %>% 
  tally %>% 
  spread(drives, n) %>% 
  mutate(no = ifelse(is.na(no), 0, no),
        yes = ifelse(is.na(yes), 0, yes),
        prop_driving = yes / (yes + no)
  ) %>% 
  filter(age < 80 & age > 17) %>% 
  ggplot(.) +
  geom_tile(mapping=aes(x=year, y = age, fill = prop_driving)) + 
  scale_fill_gradientn(colours = rainbow(7))

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
  filter(!is.na(sex) & !is.na(age) & !is.na(year) & !is.na(drives)) %>% 
  select(sex, age, year, drives) %>% 
  group_by(sex, age, year, drives) %>% 
  tally %>% 
  spread(drives, n) %>% 
  mutate(no = ifelse(is.na(no), 0, no),
         yes = ifelse(is.na(yes), 0, yes),
         prop_driving = yes / (yes + no)
  ) %>% 
  filter(age < 80 & age > 17) %>% 
  ggplot(.) +
  geom_tile(mapping=aes(x=year, y = age, fill = prop_driving)) + 
  facet_wrap(~ sex) + 
  scale_fill_gradientn(colours = rainbow(7))

# Cohort effects much stronger when looing age genders separately
# Suggestion that female cohorts learned to drive less from an 
# earlier birth cohort, perhaps from around 1978-9.
# For males, it may have been since around 1980-1982

all_inds_drvs %>% 
  mutate(
    year = wave + 1990
  ) %>% 
  filter(!is.na(sex) & !is.na(age) & !is.na(year) & !is.na(drives) & !is.na(highqual)) %>% 
  select(highqual, sex, age, year, drives) %>% 
  group_by(highqual, sex, age, year, drives) %>% 
  tally %>% 
  spread(drives, n) %>% 
  mutate(no = ifelse(is.na(no), 0, no),
         yes = ifelse(is.na(yes), 0, yes),
         prop_driving = yes / (yes + no)
  ) %>% 
  filter(age < 80 & age > 17) %>% 
  ggplot(.) +
  geom_tile(mapping=aes(x=year, y = age, fill = prop_driving)) + 
  facet_grid(highqual~ sex) + 
  scale_fill_gradientn(colours = rainbow(7))
# This seems very informative: very large qualifications effect 
# and sex*gender interaction. Possibly a recent cohoort effect 

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
  scale_fill_gradientn(colours = rainbow(7))



 
# Now onto household characteristics 



# Data - households -------------------------------------------------------




dta_path <- "E:/data/bhps/unzipped/UKDA-5151-tab/tab/"
dta_files <- list.files(path = "E:/data/bhps/unzipped/UKDA-5151-tab/tab/", pattern = "hhresp\\.tab")

all_hhlds <- llply(
  paste0(dta_path, dta_files), 
  read_delim,
  delim = "\t"
)

fn <- function(x){
  nms <- names(x)
  
  selection <- str_detect(
    nms , 
    pattern = "^[A-Z]{1}HID"
  ) | str_detect(
    nms , 
    "^[A-Z]{1}REGION$"
  ) | str_detect(
    nms , 
    "^[A-Z]{1}LADIST"
  ) | str_detect(
    nms , 
    "^[A-Z]{1}TENURE$"
  ) | str_detect(
    nms , 
    "^[A-Z]{1}HSFLOOR$"
  ) | str_detect(
    nms, 
    "^[A-Z]{1}HSTYPE$"
  ) | str_detect(
    nms, 
    "^[A-Z]{1}HSROOM$"
  ) | str_detect(
    nms, 
    "^[A-Z]{1}HSGDN$" # accomm has terrace /garden
  ) | str_detect(
    nms, 
    "^[A-Z]{1}HSPRBH$" # noise from neighbours
  ) | str_detect(
    nms, 
    "^[A-Z]{1}HSPRBI$" # street noise 
  ) | str_detect(
    nms, 
    "^[A-Z]{1}HSPRBP$" # pollution and enviornmental problems 
  ) | str_detect(
    nms, 
    "^[A-Z]{1}HSPRBQ$" # vandalism or crime 
  ) | str_detect(
    nms, 
    "^[A-Z]{1}HSCTAX$" # council tax band 
  )

  out <- x[,selection]
  tmp <- names(out)
  WAVE <- tmp[str_detect(tmp, pattern = "^[A-Z]{1}HID")]  %>% str_replace(., "HID", "")
  names(out) <- names(out) %>% str_replace_all("^[A-Z]{1}", "")
  out <- data.frame(WAVE = WAVE, out)
  return(out)
}

all_hhlds_ss <- llply(
  all_hhlds,
  fn
)


# variable with pid, wave, sex, car_driver (derived), age, ghq

fn <- function(x){
  out <- x %>% select(hid = HID, hstype =HSTYPE, region = REGION, tenure = TENURE)
  
  out <- out %>% mutate(
    hstype = recode(
      hstype,
      "
      0 = 'other';
      1 = 'detd house or bungalow';
      2 = 'semi detd house or bungalow';
      3 = 'end terraced house';
      4 = 'terraced house';
      5 = 'purpose built flat';
      6 = 'converted flat';
      7 = 'includes business premis';
      8 = 'bedsit multi occup';
      9 = 'bedsit other';
      else = NA
      "              
    ),
    region = recode(
      region, 
      "
      1 = 'inner london';
      2 = 'outer london'; 
      3 = 'rest of south east';
      4 = 'south west';
      5 = 'east anglia';
      6 = 'east midlands';
      7 = 'west midlands conurb';
      8 = 'rest of west midlands';
      9 = 'greater manchester';
      10 = 'merseyside';
      11 = 'rest of north west';
      12 = 'south yorkshire';
      13 = 'west yorkshire';
      14 = 'rest of yorkshire and humberside';
      15 = 'tyne and wear';
      16 = 'rest of north';
      17 = 'wales';
      18 = 'scotland';
      else = NA
      "
    ),
    tenure = recode(
      tenure, 
      "
      1 = 'owned outright';
      2 = 'owned with mortgage';
      3 = 'local authority rent';
      4 = 'housing assoc rent';
      5 = 'rented from employer';
      6 = 'rented private unfurnished';
      7 = 'rented private furnished';
      8 = 'rented other';
      else = NA
      "
    )
  )

  out$wave <- which(LETTERS %in% x$WAVE)
  out <- out %>% select(hid, wave, hstype, region, tenure)
  return(out)
}

all_hhlds <- ldply(all_hhlds_ss, fn) %>% tbl_df

all_inds_drvs <- all_inds_drvs %>% join(all_hhlds) %>% tbl_df


  