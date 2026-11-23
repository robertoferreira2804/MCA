# R programs for "Control Chart Design for Weibull Processes via a Markov Chain Approach in the Absence of a Closed-Form Sum Distribution"

This repository contains the R codes used in the paper:

> Ferreira, R.C., Ho, L.L., Quinino, V.B., Quinino, R.C.  
> *Control Chart Design for Weibull Processes via a Markov Chain Approach in the Absence of a Closed-Form Sum Distribution*, IEEE, 2026 (submitted).

## Structure

- **R/Program_A_num_int_CLs.R**: Computes lower and upper control limits (LCL and UCL) for the Weibull process using n-dimensional deterministic numerical integration.

- **R/Program_B_num_int_ARL.R**: Computes in-control or out-of-control average run length (ARL₀ or ARL₁) by deterministic numerical integration, using the control limits from Program A.

- **R/Program_C_MCS.R**: Uses Monte Carlo simulation to estimate control limits and ARL for comparison with the deterministic methods.
  
- **R/Program_D_Ly.R**:  Computes candidate upper truncation bounds for the Weibull sample sum and reports the value of (L_y) used to define the discretization grid.
 
- **R/Program_E_MCA_R.R** : Implements the recursive Markov chain approach (MCA-R). It reconstructs the sample-sum distribution by successively applying the row-normalized transition matrix, then computes control limits, ARL₀, and optionally ARL₁.
 
- **R/Program_F_BE.R**:  Implements the Brook–Evans (B&E) method to obtain control limits and ARL₀ for comparison with MCA.
 
- **R/Program_G_MCA_FFT.R** — Implements MCA-FFT. It reconstructs the Weibull sample-sum distribution by FFT convolution of the kernel from the first row of (M^*), retains the first (d) states, and renormalizes their probabilities. It computes control limits, in-control ARL (ARL₀), and, optionally, out-of-control ARL (ARL₁).

- **Program_H_PhaseI_ARL0.R** — Uses MCA-FFT to evaluate how estimating Weibull parameters from Phase I samples affects the control limits and the conditional in-control ARL (CARL₀). It summarizes the results across simulated Phase I samples.
  
## Requirements

- R (The scripts were checked with R 4.6.0 on Windows.)
- Packages: `expm`, `stats`, `graphics`, `spatstat.geom`, `pracma`

## How to run

1. Clone or download this repository.
2. Open the desired R script in RStudio.
3. Review the inputs at the top of the script and select the parameters for your scenario.
4. Set the working directory to the root of the repository.
5. Source the script or run it section by section as indicated in the comments.
6. For Program B, first run Program A in the same R session or enter the control limits directly in B. For Programs E and G, use Program D to recalculate (L_y) when changing the subgroup size or Weibull parameters.
