### Functions ###

# Writing own scale function to use within dplyr
scale_this <- function(x){
  (x - mean(x, na.rm=TRUE)) / sd(x, na.rm=TRUE)
}

# Rounding Factors
round_factor = function(x){
  x = round(x)
  y = as.factor(x)
  y = factor(y, levels = c(1,2,3,4,5))
  return(y)
}

# Atmospheric Sound Attenuation
att_coef <- function(f, T_cel, h_rel, Pa=101.325){
  #f=frequency, d = distance in meters, T_cel = temperature in celcius
  #h_rel = Relative humidity (percent - "70" = 70% humidity)
  #Pa = ambient pressure (101.325 is standard, 1 atmosphere)
  
  Pr = 101.325  #reference pressure in kPa
  P_rel = Pa/Pr #relative pressure
  Kelvin = 273.15 #standard temp in Kelvin
  T_kel = T_cel + Kelvin #Temperature in K
  T_ref <- 293.15 #reference Temp K
  T_rel <- T_kel/T_ref #relative temperature
  T_01 = Kelvin + 0.01 #triple-point isotherm temperature in °K
  
  P_sat_over_P_ref = 10^((-6.8346 * (T_01 / T_kel)^1.261) + 4.6151)
  H = h_rel * (P_sat_over_P_ref/P_rel) #Molecular Concentration of water vapour
  Fro = P_rel * (24 + 40400 * H * (0.02 + H) / (0.391 + H)) 
  Frn = P_rel / sqrt(T_rel) * (9 + 280 * H * exp(-4.17 * (T_rel^(-1/3) - 1)))
  
  Xc = 0.0000000000184 / P_rel * sqrt(T_rel)
  Xo = 0.01275 * exp(-2239.1 / T_kel) * (Fro + (f^2 / Fro))^-1
  Xn = 0.1068 * exp(-3352 / T_kel) * (Frn + (f^2 / Frn))^-1
  Alpha = 8.68589 * f^2 * (Xc + T_rel^(-5/2) * (Xo + Xn))
  return(Alpha)
}

determine_sig = function(p.value){
  sig. = if_else(p.value < 0.1 & p.value >= 0.05, ".", 
         if_else(p.value < 0.05 & p.value >= 0.01, "*",
         if_else(p.value < 0.01 & p.value >= 0.001, "**",
         if_else(p.value < 0.001, "***", " "))))
return(sig.)
}

csv_table_mas = function(contrast_table){
  df = contrast_table %>%
    data.frame(stringsAsFactors = FALSE) %>%
    dplyr::mutate(estimate = round(estimate, 3),
                  SE = round(SE, 3),
                  t.ratio = round(t.ratio, 3),
                  p.value = round(p.value, 3)) %>%
    dplyr::mutate(sig. = determine_sig(p.value)) %>%
    dplyr::mutate(p.value = as.factor(p.value)) %>%
    dplyr::mutate(p.value = dplyr::recode(p.value, 
                                          "0" = "<0.001")) %>%
    dplyr::rename("Est." = estimate,
                  "t ratio" = t.ratio,
                  "p-value" = p.value)
    # dplyr::select(-mas_bin)
  write.csv(df, "arid_mas_table.csv", row.names = FALSE)
  return(df)
}


add_stars <- function(x, decimals = 3) {
  # create vector of stars
  x_stars <-
    dplyr::case_when(
      x >= 0.05 ~ "",
      x >= 0.01 ~ "*",
      x >= 0.001 ~ "**",
      TRUE ~ "***"
    )
}

# Convert wind speed at 10m to wind speed at 2m
ws10m_to_ws2m = function(ws10m){
  ws2m = (ws10m*4.87)/(log((67.8*10)-5.42))
  return(ws2m)
}
