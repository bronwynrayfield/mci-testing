################################################################
## 04-run-mci.jl
## Run MCI (Merriam Connectivity Indicator) analysis for all
## resistance rasters across all dispersal distances.
##
## Inputs:  landscapes/binary/resistance-tif/*.tif
## Outputs: results/binary/omniscape-output/<key>_r<radius>.tif
##          results/binary/mci-runtime-log.csv
##
## Run from: cd C:\github\MerriamConnectivityIndicator
##           julia -t 8 --project=. C:\github\mci-testing\scripts\binary\04-run-mci.jl
################################################################

using MerriamConnectivityIndicator
import MerriamConnectivityIndicator: init_cfg, update_cfg!, read_raster, write_raster
using Dates
using CSV
using DataFrames

# ---- Paths ----------
PROJECT_ROOT   = raw"C:\github\mci-testing"
RESISTANCE_DIR = joinpath(PROJECT_ROOT, "landscapes", "binary", "resistance-tif")
OUTPUT_DIR     = joinpath(PROJECT_ROOT, "results", "binary", "omniscape-output")
LOG_PATH       = joinpath(PROJECT_ROOT, "results", "binary", "mci-runtime-log.csv")

# ---- Create output directories ----------
mkpath(OUTPUT_DIR)
mkpath(dirname(LOG_PATH))

# ---- MCI parameters ----------
RADII           = [7]
MODE            = "pairwise-direct"
NUM_SPOKES      = 16
BLOCK_SIZE      = 1
SOURCE_FROM_RES = "true"
SPOKE_AGG       = "mean"

# ---- Build base config ----------
cfg = init_cfg()
update_cfg!(cfg, Dict(
    "mode"                   => MODE,
    "num_spokes"             => string(NUM_SPOKES),
    "block_size"             => string(BLOCK_SIZE),
    "source_from_resistance" => SOURCE_FROM_RES,
    "spoke_aggregation"      => SPOKE_AGG,
    "write_as_tif"           => "false"   # we handle writing ourselves
))

# ---- Get resistance rasters ----------
tif_files = sort(filter(f -> endswith(lowercase(f), ".tif"),
                        readdir(RESISTANCE_DIR, join=true)))

println("Found $(length(tif_files)) resistance rasters")
println("Running $(length(RADII)) dispersal distances: $(RADII)")
println("Total runs: $(length(tif_files) * length(RADII))")

# ---- Runtime log ----------
runtime_df = DataFrame(
    key             = String[],
    radius          = Int[],
    start_time      = String[],
    end_time        = String[],
    runtime_seconds = Float64[],
    runtime_minutes = Float64[],
    status          = String[],
    error_message   = String[]
)

# ---- Run MCI ----------
for tif in tif_files
    key = splitext(basename(tif))[1]

    # Read resistance raster once per landscape (reuse across radii)
    resistance, wkt, transform = read_raster(tif, Float64)

    for radius in RADII

        out_path = joinpath(OUTPUT_DIR, "$(key)_r$(radius).tif")

        # Skip if already exists
        if isfile(out_path)
            println("Skipping $(key) r=$(radius) (already exists)")
            continue
        end

        println("\nRunning: $(key) | radius=$(radius)")
        start_dt = now()
        status   = "success"
        err_msg  = ""
        elapsed  = NaN

        try
            # Update radius in config
            run_cfg = copy(cfg)
            run_cfg["search_radius"] = string(radius)

            # Run MCI and time it
            start_time = time()
            result     = run_mci(run_cfg, resistance, wkt, transform)
            elapsed    = time() - start_time

            # Write output directly to flat file
            write_raster(out_path, result, wkt, transform)

        catch e
            status  = "failed"
            err_msg = sprint(showerror, e)
            println("Error: ", err_msg)
        end

        end_dt = now()

        push!(runtime_df, (
            key,
            radius,
            string(start_dt),
            string(end_dt),
            elapsed,
            elapsed / 60,
            status,
            err_msg
        ))

        # Save log after every run
        CSV.write(LOG_PATH, runtime_df)

        if status == "success"
            println("Runtime: $(round(elapsed, digits=2)) seconds")
        end
    end
end

println("\nAll runs complete. Log saved to: $(LOG_PATH)")
