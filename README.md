# nfitovdyp

[![R build status](https://github.com/vmanvailer/nfitovdyp/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/vmanvailer/nfitovdyp/actions)

**nfitovdyp** is an R package to convert National Forestry Inventory (NFI) ground plot data into the input format required by the Variable Density Yield Projection (VDYP) software, developed by the Government of British Columbia.

This package streamlines data preparation for forest growth and yield modeling workflows using NFI datasets, aligning input formats to VDYP's specifications.

⚠️ **Important:** This package does not ship with NFI data. Users must obtain NFI datasets under the appropriate data agreements.

---

## Features

* Converts NFI data to VDYP’s `INPUT_POLY.csv` and `INPUT_LAYER.csv` formats  
* Supports mapping of variables, species code harmonization, and per-hectare calculations  
* Handles UTM-to-lat/lon conversion and BEC zone assignment  
* Processes small and large tree plots according to NFI sampling protocols  
* Outputs are ready to load into VDYP7

---

## Installation

Install the development version from GitHub:

```{r}
# install.packages("devtools")
devtools::install_github("vmanvailer/nfitovdyp")
```

## Example usage
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

## Documentation
A package vignette is available and can be accessed via:
```{r}
browseVignettes("nfitovdyp")
```

## Output files
Running nfi_to_vdyp() will generate:

  INPUT_POLY.csv → polygon-level inputs for VDYP
  INPUT_LAYER.csv → layer-level inputs for VDYP

These files follow the required VDYP7 schema.

## Disclaimer
Use of this package requires access to NFI data under the applicable agreements. Users are responsible for acquiring the data and respecting its usage terms.

## Acknowledgements
Developed by Vinicius Manvailer
Contributions by Derek Settler.

## Open Source Licensing and Attribution

### Copyright
Copyright (c) 2026 His Majesty the King in Right of Canada, as represented by the Minister of Natural Resources.

### License
This data conversion tool is open-source software maintained by the Canadian Forestry Service. It is licensed under the **Apache License, Version 2.0** (the "License"). You may not use this tool except in compliance with the License. 

You can view the full terms and conditions online at the [Apache Software Foundation Website](http://apache.org).

### Government of Canada Disclaimer
Unless explicitly stated otherwise, this project is considered an experimental utility. 
* **Support:** This code is provided "as-is." Support is limited and provided on a best-effort basis.
* **Contributions:** By submitting a pull request, you agree to license your contributions under the same Apache 2.0 terms.

---

## Licence et attribution de source ouverte

### Droit d'auteur
Droit d'auteur (c) 2026 Sa Majesté le Roi du chef du Canada, représenté par le ministre des Ressources naturelles.

### Licence
Cet outil de conversion de données est un logiciel libre maintenu par le Service canadien des forêts. Il est sous licence **Apache License, Version 2.0** (la « Licence »). Vous ne pouvez pas utiliser cet outil sauf en conformité avec la Licence.

Vous pouvez consulter l'intégralité des conditions en ligne sur le [site Web de la Apache Software Foundation](http://apache.org).
