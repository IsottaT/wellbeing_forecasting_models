library(readr)

# Define a list of wave filenames
wave_files <- c("data/wave_4.csv", "data/wave_5.csv", "data/wave_6.csv", "data/wave_7.csv")

# Initialize a list to store wave dataframes
waves <- list()

# Read all wave files into the 'waves' list
for (wave_file in wave_files) {
  wave_data <- read_csv(wave_file)
  waves[[wave_file]] <- wave_data
}

# Find the subject IDs present in all waves
ids_intersection <- Reduce(intersect, lapply(waves, function(wave) wave$subject_id))
waves <- lapply(waves, function(wave) wave[wave$subject_id %in% ids_intersection, ])

# Filter waves to keep IDs with no missing values in CASP_12
waves <- lapply(waves, function(wave) wave[!is.na(wave$CASP_12), ])
ids_intersection2 <- Reduce(intersect, lapply(waves, function(wave) wave$subject_id))
waves <- lapply(waves, function(wave) wave[wave$subject_id %in% ids_intersection2, ])

# Filter wave 4 to keep only IDs aged >= 50 in wave 4
wave_4 <- waves[[1]]
wave_4_ids <- wave_4$subject_id[wave_4$age >= 50]
waves <- lapply(waves, function(wave) wave[wave$subject_id %in% wave_4_ids, ])

# Filter waves to keep only complete cases
wave_4 <- waves[[1]]
wave_4_ids <- wave_4$subject_id[complete.cases(wave_4)]
wave_6 <- waves[[3]]
wave_6_ids <- wave_6$subject_id[complete.cases(wave_6)]
ids_intersection3 <- intersect(wave_4_ids, wave_6_ids)

waves <- lapply(waves, function(wave) wave[wave$subject_id %in% ids_intersection3, ])

# Save the filtered dataframes to CSV files
for (i in 1:length(wave_files)) {
  filename <- paste("filtered_wave_", i, ".csv", sep = "")
  write_csv(waves[[i]], filename)
}
