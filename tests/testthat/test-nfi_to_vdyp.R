# Testing
library(testthat)
test_that("nfi_to_vdyp runs and produces output files", {
  output_dir <- file.path(tempdir(), "vdyp_test_output")

  # Ensure output directory is clean
  if (dir.exists(output_dir)) unlink(output_dir, recursive = TRUE)
  dir.create(output_dir, recursive = TRUE)

  # Run the function

  expect_message(
    nfi_to_vdyp(
      nfi_folder = "C:/Vini/Github_projects/nfitovdyp/development/nfi_data",
      output_path = output_dir,
      remeasurement_number = NULL
    ),
    "Preparing INPUT_POLY..."
  )


  # Check if files were created
  expect_true(file.exists(file.path(output_dir, "INPUT_POLY.csv")))
  expect_true(file.exists(file.path(output_dir, "INPUT_LAYER.csv")))

  # Optional: Check files are not empty
  expect_gt(file.size(file.path(output_dir, "INPUT_POLY.csv")), 0)
  expect_gt(file.size(file.path(output_dir, "INPUT_LAYER.csv")), 0)
})
