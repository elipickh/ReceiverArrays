soundspeed_seawater_mackenzie = function(temp, salinity, depth) {
  # Calculate the speed of sound in sea water by Mackenzie's formula
  #
  # Args:
  #   temp: Temperature in celsius.
  #   salinity: Salinity in parts per thousand.
  #   depth: Depth in meters.
  #
  # Returns:
  #   Numeric value.  
  
  # This equation has a standard error of 0.070 m/s for salinity between 25 and 40 ppt
  # Range of validity: temperature 2-30 °C; salinity 25-40 ppt; depth 0-8000 m
  # References: K.V. Mackenzie, Nine-term equation for the sound speed in the oceans (1981) J. Acoust. Soc. Am. 70(3), pp 807-812
  # L. Bjørnø, M.J. Buckingham, Applied Underwater Acoustics, 2017, p.35.
  
  1448.96 + 
  (4.591 * temp) - (5.304e-2 * temp^2) + (2.374e-4 * temp^3) + 
  1.340 * (salinity - 35) + 
  (1.630e-2 * depth) + (1.675e-7 * depth^2) - 
  (1.025e-2 * temp * (salinity - 35)) -
  (7.139e-13 * temp * depth^3)
  
}
