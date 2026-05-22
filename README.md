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
