# mci-testing

A reproducible pipeline for evaluating how landscape connectivity indicators respond
to habitat amount and configuration, using simulated landscapes generated with
[NLMR](https://github.com/ropensci/NLMR), following the modified random clusters
method of Saura & Martínez-Millán (2000). The workflow builds a set of binary landscape scenarios (non-binary
scenarios forthcoming), computes a suite of established connectivity metrics as a
reference baseline, and extends that comparison to a novel resistance-surface-based
indicator computed in Julia. Results are summarised and visualised across a
gradient of habitat amount, patch clumping, and dispersal distance.

This project builds on and extends the simulation and evaluation framework from:

> Oehri, J., Wood, S.L.R., Touratier, E., Leung, B., Gonzalez, A. Rapid evaluation of habitat
> connectivity change to safeguard multispecies persistence in human-transformed
> landscapes. *Biodivers Conserv* 33, 4043–4071 (2024).
> https://doi.org/10.1007/s10531-024-02938-2
>
> Saura, S., Martínez-Millán, J. Landscape patterns simulation with a modified
> random clusters method. *Landscape Ecology* 15, 661–678 (2000).
> https://doi.org/10.1023/A:1008107902848
```
mci-testing/
├── landscapes/
│   ├── binary/
│   │   ├── simulated-landscapes-tif/
│   │   └── resistance-tif/
│   └── non-binary/                    
│       ├── simulated-landscapes-tif/
│       └── resistance-tif/
├── results/
│   ├── binary/
│   │   ├── omniscape-output/
│   │   ├── connectivity-reference-indicators.csv
│   │   ├── connectivity-with-mci.csv
│   │   └── figures/
│   └── non-binary/                    
├── scripts/
│   ├── parameters.R
│   ├── functions/
│   │   └──compute-connectivity.R
│   ├── binary/
│   │   ├── 01-simulate-landscapes.R
│   │   ├── 02-compute-reference-connectivity.R
│   │   ├── 03-convert-to-resistance.R
│   │   ├── 04-run-mci.jl
│   │   ├── 04-run-mci.ps1
│   │   ├── 05-summarise-mci.R
│   │   └── 06-plot-connectivity-indicators.R
│   └── non-binary/                    
└── README.md
```
