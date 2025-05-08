# nfitovdyp

[![R build status](https://github.com/vmanvailer/nfitovdyp/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/vmanvailer/nfitovdyp/actions)

**nfitovdyp** is an R package to convert National Forestry Inventory (NFI) ground plot data into the input format required by the Variable Density Yield Projection (VDYP) software, developed by the Government of British Columbia.

This package streamlines data preparation for forest growth and yield modeling workflows using NFI datasets, aligning input formats to VDYP's specifications.

⚠️ **Important:** This package does not ship with NFI data. Users must obtain NFI datasets under the appropriate data agreements.

---

## ✨ Features

✅ Converts NFI data to VDYP’s `INPUT_POLY.csv` and `INPUT_LAYER.csv` formats  
✅ Supports mapping of variables, species code harmonization, and per-hectare calculations  
✅ Handles UTM-to-lat/lon conversion and BEC zone assignment  
✅ Processes small and large tree plots according to NFI sampling protocols  
✅ Outputs are ready to load into VDYP7

---

## 📦 Installation

Install the development version from GitHub:

```{r}
# install.packages("devtools")
devtools::install_github("vmanvailer/nfitovdyp")
```

## 📝 Example usage
```{r, eval = FALSE}
library(nfitovdyp)

# Set your local NFI data folder
nfi_data_path <- "path/to/your/nfi_data"

# Set output directory
output_dir <- tempdir()

# Run the conversion
nfi_to_vdyp(
  nfi_folder = nfi_data_path,
  output_path = output_dir
)

# Check output files
list.files(output_dir)
```

## 📚 Documentation
A package vignette is available and can be accessed via:
```{r}
browseVignettes("nfitovdyp")
```

## 🗂️ Output files
Running nfi_to_vdyp() will generate:

  INPUT_POLY.csv → polygon-level inputs for VDYP
  INPUT_LAYER.csv → layer-level inputs for VDYP

These files follow the required VDYP7 schema.

### 📄 License
This package is currently unlicensed.

### 🤝 Acknowledgements
Developed by Vinicius Manvailer
Contributions by Derek Settler.

### 📝 Disclaimer
Use of this package requires access to NFI data under the applicable agreements. Users are responsible for acquiring the data and respecting its usage terms.
