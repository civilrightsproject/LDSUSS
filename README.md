# Longitudinal Demographic Dataset of US Schools (LDDUSS)

This project aims to create the Longitudinal Demographic Dataset of US Schools (LDDUSS) to enable detailed analyses of school segregation from the 1960s (and earlier) to the present.

Desegregation of schools is associated with positive educational, health, and economic outcomes, yet schools are resegregating. A significant challenge in evaluating and reaping the benefits of school desegregation stems from the absence of a national data collection system for most of the 20th century. This project will create the Longitudinal Demographic Datasets of US Schools (LDDUSS), which will offer a publicly accessible, comprehensive series of datasets that enable detailed analyses of school segregation from the 1950s to the present. By filling gaps in missing data from critical and often misinterpreted periods, the LDDUSS will serve as a foundational resource for studies aimed at deepening our understanding of America's schools—the nation’s largest public institutions—and the civil rights reforms that have shaped racial progress.

## Technical Details

The R scripts in this repo download and merge hundreds of datasets, including:

- Early Civil Rights Data Collection (CRDC) (starting in 1968)
- Common Core of Data (CCD) (starting around 1988)

Some data files can be accessed programmatically through public APIs, while others must be downloaded manually—see the scripts for details. The scripts are numbered for sequential execution, but they are still in an early stage and may require adjustments to generate meaningful results.

## Next Steps & Collaboration

The next big challenge is matching schools and districts between 1968 and 1988. Future work could merge data from 1968 to 2023 and explore even earlier datasets (e.g., Project Talent). It would be great to have lots of validation -- finding other sources (e.g., US Census) and checking to see whether relevant stats from the LDDUSS are consistent.

A promising approach involves fuzzy matching on school names, IDs, and location data. We need help refining this process! If you’re interested, please submit an issue to start a conversation about how you can contribute.
