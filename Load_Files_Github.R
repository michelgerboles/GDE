# Load necessary library
if (!requireNamespace("digest", quietly = TRUE)) {
    install.packages("digest")
}
library(digest)

# Function to calculate the hash of a file
calculate_hash <- function(file_path) {
    if (file.exists(file_path)) {
        return(digest::digest(file_path, algo = "md5", file = TRUE))
    }
    return(NULL)
}

################################################
# URLs and file paths for "Functions4ASE.R" ####
################################################
file_url <- "https://raw.githubusercontent.com/ec-jrc/airsenseur-calibration/master/Functions4ASE.R"
destination_file <- file.path(WD,"Functions4ASE.R")

# Calculate the hash of the local file
local_hash <- calculate_hash(destination_file)

# Download the remote file to a temporary location
temp_file <- tempfile()
download.file(file_url, temp_file, method = "auto")

# Calculate the hash of the remote file
remote_hash <- calculate_hash(temp_file)

# Compare hashes and download if different
if (is.null(local_hash) || local_hash != remote_hash) {
    file.copy(temp_file, destination_file, overwrite = TRUE)
    message(paste0(destination_file," downloaded and updated."))
} else {
    message(paste0(destination_file, " is up-to-date. No download needed."))
}

# Clean up by deleting the temporary file
unlink(temp_file)

################################################
# URLs and file paths for "Functions4ASE.R"
################################################
file_url <- "https://raw.githubusercontent.com/ec-jrc/airsenseur-calibration/refs/heads/master/151016%20Sensor_Toolbox.R"
destination_file <- file.path(WD,"151016 Sensor_Toolbox.R")

# Calculate the hash of the local file
local_hash <- calculate_hash(destination_file)

# Download the remote file to a temporary location
temp_file <- tempfile()
download.file(file_url, temp_file, method = "auto")

# Calculate the hash of the remote file
remote_hash <- calculate_hash(temp_file)

# Compare hashes and download if different
if (is.null(local_hash) || local_hash != remote_hash) {
    file.copy(temp_file, destination_file, overwrite = TRUE)
    message(paste0(destination_file, " downloaded and updated."))
} else {
    message(paste0(destination_file, " is up-to-date. No download needed."))
}

# Clean up by deleting the temporary file
unlink(temp_file)
