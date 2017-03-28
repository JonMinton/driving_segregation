
# Using UKHLS
# Data - individual  ------------------------------------------------------

# Starting from wave 2 as this is when BHPS OSMs first included

dta_path <- "E:/Dropbox/Data/ukhls/6614TAB_7B010178BD0798C37F8215AE82667F56/UKDA-6614-tab/tab/"
dta_files <- list.files(path = dta_path, pattern = "[b-z]_indresp\\.tab")

# AIM should be to do more in a single function, even if effectively the function should 
# be broken into clearly defined parts 

tidy_select_and_lengthen <- function(
  file_location,
  variable_patterns
  ){
  
  out <- read_delim(file_location, delim = "\t")
  
  nms <- names(out)
  
  selection <- str_detect(
    nms,
    pattern = search_patterns
  )
  
  out <- out[,selection]
  tmp <- names(out)

  wave_code <- str_subset(tmp, "^[a-z]_") %>% str_extract("^[a-z]{1}")
  if(length(unique(wave_code)) != 1) {stop("More than one possible wave code identified") }
  wave <- wave_code[1]
  
  pid <- out$pid
  
  out <- out %>% select_(quote(-pid))
  names(out) <- names(out) %>% str_replace_all("^[a-z]{1}_", "")
  out <- data.frame(pid = pid, wave = wave, out)
  out
}



search_patterns <- c(
  "^pid$", # WAVE 2 onwards - BHPS original sample members
  "^[a-z]{1}_sex$", # Moved to other characteristics file 
  "^[a-z]{1}_hidp",
  "^[a-z]{1}_drive", # Respondent has driving licence
  "^[a-z]{1}_caruse", # Has use of car or van
  "^[a-z]{1}_scghq1_dv",
  "^[a-z]{1}_age_cr$", # Age corrected 
  "^[a-z]{1}_feend$", # Further education leaving age
  "^[a-z]{1}_fenow$", # Still in further education
  "^[a-z]{1}_qfhigh_dv$", # highest qualification (derived variable)
  "^[a-z]{1}_plbornc$", # Country of birth
  "^[a-z]{1}_racel$", # ethnic group membership (long version)
  #  "^[A-Z]{1}NEIGH$", # neighbourhood good place to live
  "^[A-Z]{1}scopngbh[a-h]{1}$" # see below
  #   OPNGBHA # feels belongs to neighbourhood
  #   OPNGBHB # local friends mean a lot
  #   OPNGBHC # advice obtanable locally
  #   OPNGBHD # can borrow things from neighbours
  #   OPNGBHE # willing to improve neighbourhood
  #   OPNGBHF # plan to stay in neighbourhood
  #   OPNGBHG # am similar to others in neighbourhood
  #   OPNGBHH # talk regularly to neighbourhood
) %>% paste( collapse = "|")

#debug(tidy_select_and_lengthen)

all_inds_ss <- map(
  paste0(dta_path, dta_files), 
  tidy_select_and_lengthen, 
  variable_patterns = search_patterns
) %>% 
  reduce(bind_rows) %>% 
  as_data_frame

all_inds_ss %>% 
  mutate(
    sex = car::recode(
      sex, 
      "1 = 'male'; 2 = 'female'; else = NA"
    ),
#    ghq = ifelse(ghq < 0, NA, ghq),
    age = ifelse(age_cr < 0, NA, age_cr),
    isced = car::recode(
      qfhigh_dv,
      "
        1 = 'degree';
        2 = 'other higher degree';
        3 = 'a level etc';
        4 = 'gsce etc';
        5 = 'other qualification';
        9 = 'no qualification';
        else = NA
        "
    ),
    highqual = car::recode(
      isced,
    "
      c('no qualification') = 'no further';
      c('gsce etc',  'other qualification') = 'further vocational';
      c('degree', 'a level etc') = 'further non-vocational';
      else = NA
    "
  )
)




# variable with pid, wave, sex, car_driver (derived), age, ghq

  # dlo: driving licence ownership
  # co: car ownership
# 
# 
#   out$dlo <- NA
#   if (x$WAVE[1] %in% c("A", "B")){
#     out$dlo[x$DRIVER==1] <- "yes"
#     out$dlo[x$DRIVER==2] <- "no"
#   } else {
#     out$dlo[x$CARUSE==3] <- "no"
#     out$dlo[x$CARUSE==1 | x$CARUSE == 2] <- "yes"
#   }
# 
#   out$cu <- NA  # car use
#   if (x$WAVE[1] %in% c("A", "B")){
#     out$cu[x$CARUSE==1] <- "yes"
#     out$cu[x$CARUSE==3] <- "no"
#   } else {
#     out$cu[x$CARUSE==1] <- "yes"
#     out$cu[x$CARUSE==2] <- "no"
# 
#   }
# 
#   out$wave <- which(LETTERS %in% x$WAVE)
#   out <- out %>% 
#     select_(~pid, ~hid, ~wave, ~sex, ~age, ~dlo, ~cu, ~ghq, ~neigh, ~isced, ~highqual) %>% 
#     as_data_frame
#   return(out)
  }

all_inds_drvs <- map_df(all_inds_ss, fn) 


# # Variation of above for looking at neighbourhood characteristics ---------
# 
# # variable with pid, wave, sex, car_driver (derived), age, ghq
# 
# fn <- function(x){
#   out <- x %>% select_(
#     pid = ~PID, hid = ~HID, sex = ~SEX, age = ~AGE, ghq = ~HLGHQ2
#                        ) %>% 
#     mutate(
#       opngbha = NA, 
#       opngbhb = NA,
#       opngbhc = NA,
#       opngbhd = NA,
#       opngbhe = NA,
#       opngbhf = NA,
#       opngbhg = NA,
#       opngbhh = NA
#       
#            )
#   
#   if ("OPNGBHA" %in% names(x)) {out$opngbha = x$OPNGBHA}
#   if ("OPNGBHB" %in% names(x)) {out$opngbhb = x$OPNGBHB}
#   if ("OPNGBHC" %in% names(x)) {out$opngbhc = x$OPNGBHC}
#   if ("OPNGBHD" %in% names(x)) {out$opngbhd = x$OPNGBHD}
#   if ("OPNGBHE" %in% names(x)) {out$opngbhe = x$OPNGBHE}
#   if ("OPNGBHF" %in% names(x)) {out$opngbhf = x$OPNGBHF}
#   if ("OPNGBHG" %in% names(x)) {out$opngbhg = x$OPNGBHG}
#   if ("OPNGBHH" %in% names(x)) {out$opngbhh = x$OPNGBHH}
#   
# #   ~OPNGBHA, # feels belongs to neighbourhood
# #   ~OPNGBHB, # local friends mean a lot
# #   ~OPNGBHC, # advice obtanable locally
# #   ~OPNGBHD, # can borrow things from neighbours
# #   ~OPNGBHE, # willing to improve neighbourhood
# #   ~OPNGBHF, # plan to stay in neighbourhood
# #   ~OPNGBHG, # am similar to others in neighbourhood
# #   ~OPNGBHH # talk regularly to neighbourhood
#   
#   
#   out <- out %>% mutate(
#     sex = recode(sex, "1 = 'male'; 2 = 'female'; else = NA"),
#     ghq = ifelse(ghq < 0, NA, ghq),
#     age = ifelse(age < 0, NA, age)
#   )
#   out$neigh <- NA
#   if ("NEIGH" %in% names(x)){
#     out$neigh <- recode(
#       x$NEIGH, 
#       "
#       1 = 'yes';
#       2 = 'no'; 
#       3 = 'mixed';
#       else = NA
#       ")
#   }
#   out$isced <- recode(
#     x$ISCED, 
#     "
#     0 = 'not defined';
#     1 = 'primary'; 
#     2 = 'low secondary';
#     3 = 'low sec-voc';
#     4 = 'hisec mivoc';
#     5 = 'higher voc';
#     6 = 'first degree';
#     7 = 'higher degree';
#     else = NA
#     "
#   )
#   
#   out$highqual <- recode(
#     out$isced,
#     "
#     c('not defined', 'primary', 'secondary') = 'no further';
#     c('low sec-voc', 'hisec mivoc', 'higher voc') = 'further vocational';
#     c('first degree', 'higher degree') = 'further non-vocational';
#     else = NA
#     "
#   )
#   
# 
#   
#   out$wave <- which(LETTERS %in% x$WAVE)
#   return(out)
#   }
# 
# all_inds_nhds <- ldply(all_inds_ss, fn) %>% tbl_df
# 
# all_inds_nhds <- all_inds_nhds %>% filter(wave %in% c(8, 13, 18))
# 
# 
# write_csv(x = all_inds_nhds, path = "nhd_bhps_for_johanna.csv")

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



all_egoalts <- map(
  paste0(dta_path, dta_files),
  read_delim,
  delim = "\t"
)

search_patterns <- c(
  "^[A-Z]{1}HID",
  "PNO$",
  "REL$",
  "PID$"
) %>% paste(collapse = "|")

fn <- function(x){
  nms <- names(x)
  
  selection <- str_detect(
    nms , 
    search_patterns
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

all_egoalts_ss <- map(
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
  ) %>% as_data_frame
  return(out)
}

all_egoalts <- map_df(all_egoalts_ss, fn) 

rm(all_egoalts_ss, rel_lookup, simple_rel_lookup)



# Household composition ---------------------------------------------------


hh_composition <- all_egoalts %>% 
  group_by(hid, wave) %>% 
  summarise(
    num_hh_members = length(unique(pid)), 
    num_children = length(unique(pid[simple_relation == "child"]))
  ) %T>% print %>%  # Tee operator, operates but returns its input
  mutate(num_adults = num_hh_members - num_children)





# Data - households -------------------------------------------------------




dta_path <- "E:/data/bhps/unzipped/UKDA-5151-tab/tab/"
dta_files <- list.files(path = "E:/data/bhps/unzipped/UKDA-5151-tab/tab/", pattern = "hhresp\\.tab")

all_hhlds <- map(
  paste0(dta_path, dta_files), 
  read_delim,
  delim = "\t"
)


search_patterns <- c(
  "^[A-Z]{1}HID",
  "^[A-Z]{1}REGION$",
  "^[A-Z]{1}LADIST",
  "^[A-Z]{1}TENURE$",
  "^[A-Z]{1}HSFLOOR$",
  "^[A-Z]{1}HSTYPE$",
  "^[A-Z]{1}HSROOM$",
  "^[A-Z]{1}HSGDN$", # accomm has terrace /garden
  "^[A-Z]{1}HSPRBH$", # noise from neighbours
  "^[A-Z]{1}HSPRBI$", # street noise 
  "^[A-Z]{1}HSPRBP$", # pollution and enviornmental problems 
  "^[A-Z]{1}HSPRBQ$", # vandalism or crime 
  "^[A-Z]{1}HSCTAX$", # council tax band 
  "^[A-Z]{1}FIEQFCB$", # Equivalised household income before housing costs 
  "^[A-Z]{1}FIEQFCA$" # Equivalised household income after housing costs 
) %>% paste(collapse = "|")

fn <- function(x){
  nms <- names(x)
  
  selection <- str_detect(
    nms , 
    pattern = search_patterns
  )
  
  out <- x[,selection]
  tmp <- names(out)
  WAVE <- tmp[str_detect(tmp, pattern = "^[A-Z]{1}HID")]  %>% str_replace(., "HID", "")
  names(out) <- names(out) %>% str_replace_all("^[A-Z]{1}", "")
  out <- data.frame(WAVE = WAVE, out)
  return(out)
}

all_hhlds_ss <- map(
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
    hstype = car::recode(
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
    region = car::recode(
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
    tenure = car::recode(
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
    simpletenure = car::recode(
      tenure, 
      "
      c('owned outright', 'owned with mortgage') = 'owner';
      c('rented from employer', 'rented private unfurnished', 'rented private furnished') = 'private renter';
      c('local authority rent', 'housing assoc rent') = 'social renter';
      else = NA
      "
    ),
    simplehstype = car::recode(
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
  ) %>% as_data_frame
  return(out)
}

all_hhlds <- map_df(all_hhlds_ss, fn)



# Data - urban/rural indicator household link -------------------------------------------------------


dta_path <- "E:/data/bhps/urban_rural/UKDA-6032-tab/tab/"
dta_files <- list.files(path = dta_path, pattern = "[a-z]{1}ur01ind_protect\\.tab")

all_urbrur <- map(
  paste0(dta_path, dta_files), 
  read_delim,
  delim = "\t",
  col_types = "ic"
)
search_patterns <- c(
  "^[a-z]{1}hid",
  "^[a-z]{1}ur01ind$"
) %>% paste(collapse = "|")

fn <- function(x){
  nms <- names(x)
  
  selection <- str_detect(
    nms , 
    search_patterns
  )
  
  out <- x[,selection]
  tmp <- names(out)
  wave <- tmp[str_detect(tmp, pattern = "^[a-z]{1}hid")]  %>% str_replace(., "hid", "")
  wave <- which(letters %in% wave)
  names(out) <- names(out) %>% str_replace_all("^[a-z]{1}", "")
  out <- data.frame(wave, out)
  return(out)
}

all_urbrur <- map_df(
  all_urbrur,
  fn
)


# join urbrur to all_hhlds 

all_hhlds <- all_hhlds %>% left_join(all_urbrur)


# recode to three states 

fn <- function(x){
  out <- x
  out$ur_group <- NA
  
  ur_scot <- car::recode(
    x$ur01ind,
    "
    c(1, 2) = 'urban';
    c(3, 4, 5, 6, 7, 8) = 'nonurban';
    else = NA
    "
  )
  
  ur_enw <- car::recode(
    x$ur01ind,
    "
    c(1, 5) = 'urban';
    c(2, 3, 4, 6, 7, 8) = 'nonurban';
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
