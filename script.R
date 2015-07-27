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
    )
  
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
  out$drives <- NA
  if (x$WAVE[1] %in% c("A", "B")){
    out$drives[x$DRIVER==1] <- "yes"
    out$drives[x$DRIVER==2] <- "no"
  } else {
    out$drives[x$CARUSE==3] <- "no"
    out$drives[x$CARUSE==1 | x$CARUSE == 2] <- "yes"
  }
  out$wave <- which(LETTERS %in% x$WAVE)
  out <- out %>% select(pid, hid, wave, sex, age, drives, ghq)
  return(out)
}

all_inds_drvs <- ldply(all_inds_ss, fn) %>% tbl_df



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



# cumulative proportions by age who drive

all_inds_drvs %>%   
  filter(!is.na(sex)) %>% 
  arrange(sex, age) %>% 
  group_by(sex, age) %>% 
  mutate(does_drive = recode(drives, "'yes' = 1; 'no' = '0'; else = NA")) %>% 
  summarise(driv_prop = mean(does_drive, na.rm=T)) %>% 
  ggplot(. ) +
  geom_line(aes(x = age, y = driv_prop, colour = sex, group = sex))


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


  