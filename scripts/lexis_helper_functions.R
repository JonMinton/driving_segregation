# Lexis surface helper functions

# Lexis surface helper functions

plot_lgcmr <- function(
  DTA,
  COL = "black",
  COLS_TO_USE =colorRampPalette(brewer.pal(9, "Greens"))(100),
  ASPECT = "iso",
  CUTS = 15,
  CORRECTION = 0
){
  DTA <- DTA %>% 
    mutate(cmr = (death_count + CORRECTION)/ (population_count + CORRECTION),
           lg_cmr = log(cmr, base = 10)
    )
  
  output <- DTA %>% contourplot(
    lg_cmr ~ year * age | sex, 
    data=., 
    region=T, 
    strip=strip.custom(par.strip.text=list(cex=1.4, fontface="bold"), bg="grey"),
    ylab=list(label="Age in years", cex=1.4),
    xlab=list(label="Year", cex=1.4),
    cex=1.4,
    cuts=CUTS,
    col.regions=COLS_TO_USE,
    main=NULL,
    labels=list(cex=1.2),
    col=COL,
    scales=list(
      x=list(cex=1.4), 
      y=list(cex=1.4),
      alternating=3
    )
    
  )
  
  return(output)  
}



plot_smoothed_lgcmr <- function(
  DTA,
  COL = "black",
  COLS_TO_USE =colorRampPalette(brewer.pal(9, "Greens"))(100),
  ASPECT = "iso",
  CUTS = 15,
  SMOOTH_PAR = 1.3,
  EDGE = 2,
  CORRECTION = 0
){
  DTA <- DTA %>% 
    mutate(cmr = (death_count + CORRECTION) / (population_count + CORRECTION),
           lg_cmr = log(cmr, base = 10)
    )
  
  output <- DTA %>% contourplot(
    lg_cmr ~ year * age | sex, 
    data=., 
    region=T, 
    strip=strip.custom(par.strip.text=list(cex=1.4, fontface="bold"), bg="grey"),
    ylab=list(label="Age in years", cex=1.4),
    xlab=list(label="Year", cex=1.4),
    cex=1.4,
    cuts=CUTS,
    col.regions=COLS_TO_USE,
    main=NULL,
    labels=list(cex=1.2),
    col=COL,
    scales=list(
      x=list(cex=1.4), 
      y=list(cex=1.4),
      alternating=3
    )
    
  )
  DTA <- DTA %>% 
    mutate(cmr = death_count / population_count,
           lg_cmr = log(cmr, base = 10)
    )
  
  dta_smoothed <- DTA %>% 
    smooth_var(. , 
               group_vars = "sex", smooth_var = "lg_cmr", 
               smooth_par= SMOOTH_PAR)
  
  level_part <- DTA %>% levelplot(
    lg_cmr ~ year * age | sex, 
    data=., 
    strip=strip.custom(par.strip.text=list(cex=1.4, fontface="bold"), bg="grey"),
    ylab=list(label="Age in years", cex=1.4),
    xlab=list(label="Year", cex=1.4),
    cex=1.4,
    cuts=CUTS,
    col.regions=COLS_TO_USE,
    main=NULL,
    labels=list(cex=1.2),
    aspect = ASPECT,
    scales=list(
      x=list(cex=1.4), 
      y=list(cex=1.4),
      alternating=3
    )
  )
  
  contour_part <- dta_smoothed %>% 
    filter(year >= min(.$year) + EDGE &
             year <= max(.$year) - EDGE &
             age >= min(.$age) + EDGE & 
             age <= max(.$age) - EDGE
    ) %>% 
    contourplot(
      lg_cmr ~ year * age | sex, 
      data=., 
      region=F,
      strip=strip.custom(par.strip.text=list(cex=1.4, fontface="bold"), bg="grey"),
      ylab="", 
      xlab="",
      cex=1.4,
      cuts=CUTS,
      main=NULL,
      aspect = ASPECT,
      labels=list(cex=1.2),
      scales=list(
        x=list(cex=1.4), 
        y=list(cex=1.4),
        alternating=3
      )
    )
  
  output <- level_part + contour_part
  
  return(output)  
}


plot_region_lgcmr <- function(
  DTA,
  COL = "black",
  COLS_TO_USE =colorRampPalette(brewer.pal(9, "Greens"))(100),
  ASPECT = "iso",
  SMOOTH_PAR = 1.3,
  CUTS = 15,
  CORRECTION = 0
){
  DTA <- DTA %>% 
    mutate(cmr = (death_count + CORRECTION)/ (population_count + CORRECTION),
           lg_cmr = log(cmr, base = 10)
    )
  
  output <- DTA %>% contourplot(
    lg_cmr ~ year * age | region + sex, 
    data=., 
    region=T, 
    strip=strip.custom(par.strip.text=list(cex=1.4, fontface="bold"), bg="grey"),
    ylab=list(label="Age in years", cex=1.4),
    xlab=list(label="Year", cex=1.4),
    cex=1.4,
    cuts=CUTS,
    col.regions=COLS_TO_USE,
    main=NULL,
    labels=list(cex=1.2),
    col=COL,
    scales=list(
      x=list(cex=1.4), 
      y=list(cex=1.4),
      alternating=3
    )
    
  )
  
  return(output)  
}




plot_smoothed_region_lgcmr <- function(
  DTA,
  COL = "black",
  COLS_TO_USE =colorRampPalette(brewer.pal(9, "Greens"))(100),
  ASPECT = "iso",
  SMOOTH_PAR = 1.3, 
  EDGE = 2,
  CUTS = 15,
  CORRECTION = 0
){
  DTA <- DTA %>% 
    mutate(cmr = (death_count + CORRECTION) / (population_count + CORRECTION),
           lg_cmr = log(cmr, base = 10)
    )
  
  dta_smoothed <- DTA %>% 
    smooth_var(. , 
               group_vars = c("region", "sex"), smooth_var = "lg_cmr", 
               smooth_par= SMOOTH_PAR)
  
  level_part <- DTA %>% levelplot(
    lg_cmr ~ year * age | region + sex, 
    data=., 
    strip=strip.custom(par.strip.text=list(cex=1.4, fontface="bold"), bg="grey"),
    ylab=list(label="Age in years", cex=1.4),
    xlab=list(label="Year", cex=1.4),
    cex=1.4,
    cuts=CUTS,
    col.regions=COLS_TO_USE,
    main=NULL,
    labels=list(cex=1.2),
    aspect = ASPECT,
    scales=list(
      x=list(cex=1.4), 
      y=list(cex=1.4),
      alternating=3
    )
  )
  
  contour_part <- dta_smoothed %>% 
    filter(year >= min(.$year) + EDGE &
             year <= max(.$year) - EDGE &
             age >= min(.$age) + EDGE & 
             age <= max(.$age) - EDGE
    ) %>% 
    contourplot(
      lg_cmr ~ year * age | region + sex, 
      data=., 
      region=F,
      strip=strip.custom(par.strip.text=list(cex=1.4, fontface="bold"), bg="grey"),
      ylab="", 
      xlab="",
      cex=1.4,
      cuts=CUTS,
      main=NULL,
      aspect = ASPECT,
      labels=list(cex=1.2),
      scales=list(
        x=list(cex=1.4), 
        y=list(cex=1.4),
        alternating=3
      )
    )
  
  output <- level_part + contour_part
  
  return(output)  
}