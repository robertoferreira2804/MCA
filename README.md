# R programs for "Control Chart Design for Weibull Processes via a Markov Chain Approach in the Absence of a Closed-Form Sum Distribution"

This repository contains the R codes used in the paper:

> Ferreira, R.C., Ho, L.L., Quinino, V.B., Quinino, R.C.  
> *Control Chart Design for Weibull Processes via a Markov Chain Approach in the Absence of a Closed-Form Sum Distribution*, IEEE, 2026 (submitted).

## Structure

- **R/Program_A_num_int_CLs.R**: Computes lower and upper control limits (LCL and UCL) for the Weibull process using n-dimensional deterministic numerical integration.

- **R/Program_B_num_int_ARL.R**: Computes ARL₁ under out-of-control (or ARL₀ under in-control) conditions  using deterministic integration.

- **R/Program_C_MCS.R**: Monte Carlo simulation for validation and comparison with deterministic results
- **R/Program_D_Ly.R**:  Computes the upper truncation bound \( L_Y \) used in the discretization of the sum distribution.
- **R/Program_E_MCA.R** : Implements the proposed Markov Chain Approach (MCA) to obtain:
  - Control limits (CLs)
  - In-control ARL (ARL₀)
  - Out-of-control ARL (ARL₁)

- **R/Program_F_BE.R**: Implements the Brook–Evans (BE) discretization method for Control limits (CLs) and ARL computation.  
  
## Requirements

- R (version 4.2.0 or later)
- Packages: `expm`, `stats`, `graphics` (and others used in the scripts).

## How to run

1. Clone or download this repository.
2. Open the desired R script in RStudio.
3. Set the working directory to the root of the repository.
4. Source the script or run it section by section as indicated in the comments.

## Notes

- Programs A and B provide deterministic numerical benchmarks for small sample sizes.
- Program C provides stochastic validation via Monte Carlo simulation.
- Programs D and E contains the main contribution of the paper (MCA).
- Program F implements the classical Brook–Evans approach for comparison purposes.
