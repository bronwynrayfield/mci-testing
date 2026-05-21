################################################################
## run-mci.ps1
## Run MCI analysis for all resistance rasters across all
## dispersal distances.
##
## Usage: .\scripts\binary\run-mci.ps1
## Run from: anywhere (paths are set below)
##
## Update PROJECT_ROOT and MCI_ROOT for your machine before running.
################################################################

# ---- Paths (update for your machine) ----------
$PROJECT_ROOT = "C:\github\mci-testing"
$MCI_ROOT     = "C:\github\MerriamConnectivityIndicator"

# ---- Run ----------
Set-Location $MCI_ROOT
julia -t 8 --project=. $PROJECT_ROOT\scripts\binary\04-run-mci.jl
