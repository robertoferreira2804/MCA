# R programs for "Monitoring the Weibull process mean with Shewhart control charts: A Markov chain approach"

This repository contains the R codes used in the paper:

> Ferreira, R.C., Ho, L.L., Quinino, V.B., Quinino, R.C.  
> *Monitoring the Weibull process mean with Shewhart control charts: A Markov chain approach*, Journal of Process Control, 2025 (submitted).

## Structure

- `R/Program_A_num_integr_limits.R`: Computes LCL and UCL for the Weibull process using n-dimensional numerical integration.
- `R/Program_B_num_integr_ARL.R`: Computes ARL1 under out-of-control conditions using deterministic integration.
- `R/Program_C_MCS.R`: Monte Carlo simulation for comparison with the deterministic results.
- `R/Program_D_Ly.R`: 
- `R/Program_E_MCA.R`: Implements the Markov Chain Approach (MCA) to obtain control limits and ARL values.

## Requirements

- R (version X.Y.Z or later)
- Packages: `expm`, `pracma`, `stats`, `graphics` (and others used in the scripts).

## How to run

1. Clone or download this repository.
2. Open the desired R script in RStudio.
3. Set the working directory to the root of the repository.
4. Source the script or run it section by section as indicated in the comments.

