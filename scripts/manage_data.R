

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
  out <- out %>% select_(quote(-PID))
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
  out <- x %>% select_(pid = ~PID, hid = ~HID, sex = ~SEX, age = ~AGE, ghq = ~HLGHQ2)
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
  
  # dlo: driving licence ownership
  # co: car ownership
  
  
  out$dlo <- NA
  if (x$WAVE[1] %in% c("A", "B")){
    out$dlo[x$DRIVER==1] <- "yes"
    out$dlo[x$DRIVER==2] <- "no"
  } else {
    out$dlo[x$CARUSE==3] <- "no"
    out$dlo[x$CARUSE==1 | x$CARUSE == 2] <- "yes"
  }
  
  out$cu <- NA  # car use
  if (x$WAVE[1] %in% c("A", "B")){
    out$cu[x$CARUSE==1] <- "yes"
    out$cu[x$CARUSE==3] <- "no"
  } else {
    out$cu[x$CARUSE==1] <- "yes"
    out$cu[x$CARUSE==2] <- "no"
    
  }
  
  out$wave <- which(LETTERS %in% x$WAVE)
  out <- out %>% select_(~pid, ~hid, ~wave, ~sex, ~age, ~dlo, ~cu, ~ghq, ~neigh, ~isced, ~highqual)
  return(out)
  }

all_inds_drvs <- ldply(all_inds_ss, fn) %>% tbl_df


# Data - egoalt files -----------------------------------------------------


dta_path <- "E:/data/bhps/unzipped/ukda-5151-tab/tab/"
dta_files <- list.files(path = dta_path, pattern = "egoalt\\.tab")


rel_lookup <- read_csv("label_lookups/egoalt_rel.csv")
rel_lookup$label <- str_trim(rel_lookup$label)
rel_lookup2 <- rel_lookup$label
names(rel_lookup2) <- rel_lookup$value
simple_rel_lookup <- rel_lookup$simple_label
names(simple_rel_lookup) <- rel_lookup$value

rm(rel_lookup)
rel_lookup <- rel_lookup2
rm(rel_lookup2)



all_egoalts <- llply(
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
    "PNO$"
  ) | str_detect(
    nms , 
    "REL$"
  ) | str_detect(
    nms , 
    "PID$"
  ) 
  
  out <- x[,selection]
  PID <- out$PID
  out <- out %>% select_(quote(-PID))
  tmp <- names(out)
  WAVE <- tmp[str_detect(tmp, pattern = "^[A-Z]{1}HID")]  %>% str_replace(., "HID", "")
  names(out) <- names(out) %>% str_replace_all("^[A-Z]{1}", "")
  out <- data.frame(WAVE = WAVE, PID = PID, out)
  return(out)
}

all_egoalts_ss <- llply(
  all_egoalts,
  fn
)

# Variable with relationships defined
fn <- function(x){
  out <- x %>% select_(
    hid = ~HID, pid = ~PID, pno = ~PNO, opno = ~OPNO, opid = ~OPID
  )
  
  out$rel <- rel_lookup[as.character(x$REL)]
  
  out$rel_simple <- simple_rel_lookup[as.character(x$REL)]  
  
  
  out$wave <- which(LETTERS %in% x$WAVE)
  out <- out %>% select_(~hid, ~wave, ~pid, ~pno, ~opno, ~opid,
                        relation = ~rel,
                        simple_relation = ~rel_simple
  )
  return(out)
}

all_egoalts <- ldply(all_egoalts_ss, fn) %>% tbl_df

rm(all_egoalts_ss, rel_lookup, simple_rel_lookup)



# Household composition ---------------------------------------------------


hh_composition <- all_egoalts %>% 
  group_by(hid, wave) %>% 
  summarise(
    num_hh_members = length(unique(pid)), 
    num_children = length(unique(pid[simple_relation == "child"]))
  ) %>% 
  mutate(num_adults = num_hh_members - num_children)





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
  ) | str_detect(
    nms, 
    "^[A-Z]{1}FIEQFCB$" # Equivalised household income before housing costs 
  ) | str_detect(
    nms, 
    "^[A-Z]{1}FIEQFCA$" # Equivalised household income after housing costs 
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
  out <- x %>% select_(
    hid = ~HID, hstype = ~HSTYPE, region = ~REGION, tenure = ~TENURE,
    hh_income_before_hcosts = ~FIEQFCB,
    hh_income_after_hcosts = ~FIEQFCA
  )
  
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
    ),
    simpletenure = recode(
      tenure, 
      "
      c('owned outright', 'owned with mortgage') = 'owner';
      c('rented from employer', 'rented private unfurnished', 'rented private furnished') = 'private renter';
      c('local authority rent', 'housing assoc rent') = 'social renter';
      else = NA
      "
    ),
    simplehstype = recode(
      hstype, 
      "
      c('end terraced house','terraced house') = 'terraced';
      c(
      'detd house or bungalow',
      'semi detd house or bungalow'
      ) = 'detached';
      c(
      'purpose built flat',
      'converted flat',
      'bedsit multi occup',
      'bedsit other'
      ) = 'flat_hmo';
      'includes business premis' = 'mixed';
      else = NA
      "              
      
    )
    )
  
  out$wave <- which(LETTERS %in% x$WAVE)
  out <- out %>% select_(~hid, ~wave, ~hstype, ~simplehstype, ~region, ~tenure, ~simpletenure,
                        ~hh_income_before_hcosts,
                        ~hh_income_after_hcosts
  )
  return(out)
}

all_hhlds <- ldply(all_hhlds_ss, fn) %>% tbl_df



# Data - urban/rural indicator household link -------------------------------------------------------


dta_path <- "E:/data/bhps/urban_rural/UKDA-6032-tab/tab/"
dta_files <- list.files(path = dta_path, pattern = "[a-z]{1}ur01ind_protect\\.tab")

all_urbrur <- llply(
  paste0(dta_path, dta_files), 
  read_delim,
  delim = "\t",
  col_types = "ic"
)

fn <- function(x){
  nms <- names(x)
  
  selection <- str_detect(
    nms , 
    pattern = "^[a-z]{1}hid"
  ) | str_detect(
    nms , 
    "^[a-z]{1}ur01ind$"
  ) 
  out <- x[,selection]
  tmp <- names(out)
  wave <- tmp[str_detect(tmp, pattern = "^[a-z]{1}hid")]  %>% str_replace(., "hid", "")
  wave <- which(letters %in% wave)
  names(out) <- names(out) %>% str_replace_all("^[a-z]{1}", "")
  out <- data.frame(wave, out)
  return(out)
}

all_urbrur <- ldply(
  all_urbrur,
  fn
)


# join urbrur to all_hhlds 

all_hhlds <- all_hhlds %>% left_join(all_urbrur)


# recode to three states 

fn <- function(x){
  out <- x
  out$ur_group <- NA
  
  ur_scot <- recode(
    x$ur01ind,
    "
    c(1, 2) = 'urban';
    c(3, 4, 5, 6, 7, 8) = 'nonurban';
    else = NA
    "
  )
  
  ur_enw <- recode(
    x$ur01ind,
    "
    5 = 'urban';
    c(1, 2, 3, 4, 6, 7, 8) = 'nonurban';
    else = NA
    "
  )
  
  is_scot <- which(out$region == "scotland")
  is_engwales <- which(out$region != "scotland"  & !is.na(out$region))
  out$ur_group[is_scot] <- ur_scot[is_scot]
  out$ur_group[is_engwales] <- ur_enw[is_engwales]
  
  out$ur01ind <- NULL
  
  return(out)
}


all_hhlds <- fn(all_hhlds)
all_inds_drvs <- all_inds_drvs %>% join(all_hhlds) %>% tbl_df

all_inds_drvs <- all_inds_drvs %>% left_join(hh_composition)
