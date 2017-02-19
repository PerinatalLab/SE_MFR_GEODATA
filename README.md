# SE_MFR_GEODATA

Project done by Sarah Murray in collaboration with Bo Jacobsson's group, on the summer of 2016. aka "postal codes".

Scripts here provide the entire pipeline for data cleaning, analysis, and map generation, as used in the SMFM Pregnancy Meeting in January 2017 and submitted to AJOG in March 2017.

## Required data files & dirs:
1. Special MFR from the postal_codes project, containing the lan_kom variable
2. SCB datasets with education levels, 1 file per year, original formatting
3. A working directory with `tmp/` and `plots/` folders.

## Code:
`process_scb_files.sh` extracts education and some other optional data from SCB files.
`initial_mfr_cleaning_optimized.R` merges MFR with education, and does some essential cleaning that is necessary for any plotting or analysis.
`analysis.R` then runs a regression to produce adjusted PTD rates, and does all the pre- and post- processing for that (variable categorization, summary tables). It also produces colorized maps of Sweden, using `mapping_helper.R`.
`mapping_helper.R` is flexible and can be used to plot any quantitative variable, with optional greying out of NA values. It requires the cartographical data provided here in the `KommunRT90/` folder.
