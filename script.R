library(taskscheduleR)
library(fredr)

# Set FRED API key
fredr_set_key('f31374c525421a4b2158ade45fea6a85')

# Set CPI series ID
cpi_series_id <- 'CPALTT01USM657N'

# Retrieve CPI data
cpi_data <- fredr_series_observations(cpi_series_id, observation_start = as.Date("1960-01-01"))

# Save data to CSV file
write.csv(cpi_data, 'cpi_1.csv')

