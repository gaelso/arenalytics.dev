# Create OLAP_Shiny_demo_corrupted.zip
#
# Produces a ZIP with the same 10 files as OLAP_Shiny_demo.zip, but with two
# intentionally corrupted entries for testing fct_readzip2() error handling:
#
#   categories.rds  — replaced with random bytes (readRDS fails:
#                     "unknown input format")
#   OLAP_tree.csv   — replaced with random high bytes (read.csv fails:
#                     "invalid multibyte string")
#
# All other files are copied from the good demo ZIP unchanged.
#
# Run this script from the package root:
#   source("inst/extdata/create_corruptedzip.R")

## Manually add data-raw/ to .Rbuidlignore: ^data-raw$

set.seed(8831)

extdata_dir <- "inst/extdata"
good_zip    <- file.path(extdata_dir, "OLAP_Shiny_demo.zip")
out_zip     <- file.path(extdata_dir, "OLAP_Shiny_demo_corrupted.zip")

stopifnot(file.exists(good_zip))

## -- 1. Extract good ZIP to a temp directory ----------------------------------
tmp <- tempfile("arenalytics_corrupt_")
dir.create(tmp)
utils::unzip(good_zip, exdir = tmp)

## -- 2. Corrupt categories.rds ------------------------------------------------
## Random bytes in the range 0x80-0xFF: invalid as both gzip and RDS format.
## readRDS(gzcon(unz(...))) will error immediately with "unknown input format".
writeBin(
  as.raw(sample(128:255, 512, replace = TRUE)),
  file.path(tmp, "categories.rds")
)

## -- 3. Corrupt OLAP_tree.csv -------------------------------------------------
## Same random high bytes: invalid UTF-8, so read.csv() first warns
## "incomplete final line" then errors with "invalid multibyte string".
writeBin(
  as.raw(sample(128:255, 512, replace = TRUE)),
  file.path(tmp, "OLAP_tree.csv")
)

## -- 4. Repack into the output ZIP --------------------------------------------
## zip::zip() requires an absolute path for zipfile; relative paths cause it to
## fail silently in the underlying C code. file.path(getwd(), ...) is used here
## rather than normalizePath() since the output file does not exist yet.
## The root parameter sets the archive's working directory so entries are stored
## without any directory prefix (same flat layout as the source ZIP).
all_files   <- list.files(tmp, full.names = FALSE)
out_zip_abs <- file.path(getwd(), out_zip)
zip::zip(zipfile = out_zip_abs, files = all_files, root = tmp)

## -- 5. Clean up --------------------------------------------------------------
unlink(tmp, recursive = TRUE)

## -- 6. Verify ----------------------------------------------------------------
stopifnot(file.exists(out_zip))

entries <- zip::zip_list(out_zip)$filename
stopifnot(length(entries) == 10L)

cat("Created:", out_zip, "\n")
cat("Entries:\n")
cat(paste0("  ", entries), sep = "\n")

cat("\nVerifying corrupted files error as expected:\n")
cat("  categories.rds valid RDS: ",
    tryCatch({ readRDS(unz(out_zip, "categories.rds")); TRUE }, error = function(e) FALSE), "\n")
cat("  OLAP_tree.csv  valid CSV: ",
    tryCatch({ utils::read.csv(unz(out_zip, "OLAP_tree.csv")); TRUE }, error = function(e) FALSE), "\n")
