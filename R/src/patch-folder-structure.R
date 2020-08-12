library(fs)

# Input directories
dir.create(fs::dir_create("input", "UKTUS-Data"), showWarnings = FALSE)

# Work directories
dir.create(fs::dir_create("work"), showWarnings = FALSE)
dir.create(fs::dir_create("work", "caches"), showWarnings = FALSE)
dir.create(fs::dir_create("work", "checkpoints"), showWarnings = FALSE)

# Output data
dir.create(fs::dir_create("output", "final-processed-data"), showWarnings = FALSE)
