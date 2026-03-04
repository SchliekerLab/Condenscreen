args <- commandArgs(trailingOnly = TRUE)

library(data.table)

# Parse arguments
start_idx      <- as.numeric(args[1])
end_idx        <- as.numeric(args[2])
batch_number   <- as.numeric(args[3])
file_paths_rds <- args[4]
file_Location  <- args[5]

# Load file paths
file_paths <- readRDS(file_paths_rds)

cat("Processing batch", batch_number, "files", start_idx, "to", end_idx, "\n")

# Guard against out-of-range indices
end_idx <- min(end_idx, length(file_paths))
batch_paths <- file_paths[start_idx:end_idx]
batch_paths <- batch_paths[!is.na(batch_paths)]

desired_cols <- c(
  "ImageNumber",
  "ObjectNumber",
  "FileName_GFP",
  "AreaShape_Area",
  "Intensity_IntegratedIntensity_tdTomato",
  "Children_GFP_foci_Count"
)

numeric_optional <- c("AreaShape_Area", "Intensity_IntegratedIntensity_tdTomato")

batch_data <- lapply(batch_paths, function(path) {
  header_cols  <- names(fread(path, nrows = 0))
  cols_to_read <- intersect(desired_cols, header_cols)
  
  dt <- fread(path, select = cols_to_read, header = TRUE)
  
  # Ensure optional numeric columns exist and are numeric
  for (col in numeric_optional) {
    if (col %in% names(dt)) {
      # If fread inferred logical/character due to NAs, force numeric
      dt[, (col) := suppressWarnings(as.numeric(get(col)))]
    } else {
      dt[, (col) := NA_real_]   # IMPORTANT: numeric NA, not logical NA
    }
  }
  
  # Add any other missing desired columns as plain NA (ok for chr/int etc.)
  missing_other <- setdiff(desired_cols, names(dt))
  missing_other <- setdiff(missing_other, numeric_optional)
  if (length(missing_other) > 0) dt[, (missing_other) := NA]
  
  # Consistent order
  dt <- dt[, ..desired_cols]
  dt
})

combined <- rbindlist(batch_data, fill = TRUE)

# Keep only rows with required IDs (do this first)
required <- c("ImageNumber", "ObjectNumber", "Children_GFP_foci_Count", "FileName_GFP")
combined <- combined[complete.cases(combined[, ..required])]

# NOW drop columns that are all NA after filtering
all_na_cols <- names(combined)[vapply(combined, function(x) all(is.na(x)), logical(1))]
if (length(all_na_cols) > 0) combined[, (all_na_cols) := NULL]

output_file <- file.path(file_Location, paste0("batch_", batch_number, ".csv"))
fwrite(combined, output_file, na = "")