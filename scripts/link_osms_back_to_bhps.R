# The aim of this is to link BHPS and UKHLS data together

# 
# NOTE: The attributes of OSMs are retained from the last observed BHPS wave

bhps_dta <- read_csv("data/derived/bhps_driver.csv")

ukhls_dta <- read_csv("data/derived/ukhls_osms_drivers_simplified.csv")

# For UKHLS, want to turn waves into numbers

lkup <- 1:26
names(lkup) <- letters

ukhls_wave <- ukhls_dta$wave

ukhls_dta %>% 
  mutate(wave = lkup[ukhls_wave] + 17) -> ukhls_dta 
#  select(wave, wave_num) %>% sample_n(20)
rm(lkup, ukhls_wave)  



bhps_dta %>% 
  select(pid, wave, isced, highqual, simpletenure, ur_group, num_hh_members, num_children, num_adults) %>% 
  group_by(pid) %>% 
  filter(wave == max(wave)) %>% 
#  group_by(wave) %>% tally()
  select(-wave) %>% 
  right_join(ukhls_dta, by = "pid") %>% 
  select(pid, wave, sex, age, isced, highqual, simpletenure, ur_group, dlo, cu) -> osm_simplified

bhps_dta %>% 
  select(pid, wave, sex, age, isced, highqual, simpletenure, ur_group, dlo, cu) %>% 
  bind_rows(osm_simplified) -> bhps_plus_osm_combined_simplified

# Want to know how the number of OSMs varies between waves 

# bhps_plus_osm_combined_simplified %>%
#   group_by(wave) %>%
#   tally() %>%
#   ggplot(., aes(x = wave, y = n)) +
#   geom_point()


write_csv(bhps_plus_osm_combined_simplified, "data/derived/bhps_plus_usosm.csv")


