# onpe-perra-vida

Small R workflow to download and compile ONPE second-round election results by UBIGEO.

The scripts query ONPE’s public results endpoints for Peru and foreign voting locations (`Extranjero`), save individual reference files, and produce concatenated result tables for:

* `totales`
* `participantes`

## Use

Open the R project and run:

```r
source("onpe.R")
```

Outputs are written to:

```text
results/
```

## Notes

The ONPE endpoint may require browser headers/cookies copied from a working browser request. See the script comments for the expected `curl` workflow.

This repository is a lightweight data-retrieval workflow, not an official ONPE product.
