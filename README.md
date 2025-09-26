# Environmental Impacts from European Food Consumption Can Be Reduced with Carbon Pricing or a Value-Added Tax Reform
Summary:
This is the R and Stata code for Plinke et al. (2024). _Environmental Impacts from European Food Consumption Can Be Reduced with Carbon Pricing or a Value-Added Tax Reform_. Submitted to Nature Food. The code was developed by C. Plinke and M. Sureth.

## Prerequites
- Software: The analysis was conducted using RStudio Version 2023.12.1+402, R Version 4.3.2, and Stata SE Version 18.0
- Data: To run the analysis, the following data need to be obtained and placed in the directory `/source/00_data/`. The respective file and path names that need adhered to can be found in the file `/source/01_data_management/easi_data-preparation.R`.
	- Eurostat Household Budget Survey (HBS) 2010 (Version 1.0: 2016/09/02) and 2015 (Version 1.3: 2022/01/19)
	- Destatis Einkommens- and Verbrauchsstichprobe (EVS) 2018, Grundfile 4 (Version 1; DOI: 10.21242/63231.2018.00.00.3.1.0)
	- Statistik Austria Konsumerhebung 2014/15 (KE14) (Version February 2017)

## How to run the analysis

1. Define settings in config_default.do:
- General settings
	- `procdatapath`: Path name. Can be kept empty as this is set automatically in `/source/05_functions/preparations.R`.
	- `statapath`: Path name. Specify the path to Stata(console) on your machine (stata-se).
	- `stataversion`: Integer. Specify your stata version (e.g., "18").
	- `categorization`: The only allowed value is "cat10". However, the code is flexible enough to allow for other categorizations than the ten food categories this analysis is based on. The categorization is retrieved from the file `/source/00_data/manual_input/categorization_fooditems.xlsx`. The value of `categorization` corresponds to the sheet name of `categorization_fooditems.xlsx`.
	- `carbprice_upperlim`: Allowed values are any positive number. In an iterative computation, the GHG emission price level that leads to the same reduction in GHG emissions as the VAT reform is searched for in the interval from zero to `carbonprice_upperlim`. Lower values reduce computing time but increase the risk of excluding the true price level from the search interval. "100" is the default.
- Alternative configurations of the demand system estimation (used for robustness analyses)
	- `dse`: Allowed values are "partial", "incomplete", and "partialnoDE". A partial demand systems rests on the assumption of weak separability between food and non-food consumption; only food-related expenditures are considered in the demand system estimation. An incomplete demand system specifies a residual good category such that all expenditures are considered and no weak separability is assumed. The value "incomplete" leads to the exclusion of Germany from the analysis due to data availability issues. Thus, there is the third specification "partialnoDE" which is comparable to the "incomplete" specification (whereas the specification "partial" is not). "partial" is the default.
	- `equations`: Allowed values are "nmin1" and "alln". This corresponds to whether n-1 equations are estimated with parameters for the nth equation being recovered using theoretical restrictions or all n equations are estimated with the adding-up restriction being ignored (this is only relevant for the censored demand system specification). "alln" is the default.
	- `nprice`: "10" is the only allowed value. Specifies the number of food categories (must be consistent with `categorization`). Required for the Stata do-file `/source/02_analysis/2_stata_restricted_easi.do`.
	- `prices`: Allowed values are "uvadj" and "uvnonadj". This corresponds to whether a quality adjustment of the unit values (UVs) according to Cox & Wohlgenant (1986) is conducted or not. "uvadj" is the default.
	- `p_to_norm`: Allowed values are any integer between 1 and `nprice` (e.g., "1"). This corresponds to the category that is used to normalize prices. Only relevat if `equations` is set to "nmin1". Otherwise, total food expenditures are automatically used for normalization.
	- `cens_yesno`: Allowed values are "censored" and "uncensored". This corresponds to whether the censored distribution of expenditure observations should be accounted for. If a censored demand system is estimated, the two-step censoring correction proceedure by Shonkwiler & Yen (1999) is implemented. "censored" is the default.
	- `weight_yesno`: Allowed values are "weighted" and "unweighted". This corresponds to whether the seemingly unrelated regression (SUR) estimation should be weighted with household weights or not. "weighted" is the default.
	- `npowers`: The only allowed value is "4". This corresponds to the number of y-polynomials in the LA/EASI estimation.
	- `ndem`: The only allowed values is "5". This corresponds to the number of demand shifters in the demand system (z-variables / demographic characteristics in the code).
	- `y`: Allowed values are "ystone" and "ytilda". The "ystone" EASI specification approximates the real expenditure measure y with nominal expenditures deflated by a household-specific Stone price index. This introduces endogeneity in the estimation which, however, according to Lewbel & Pendakur (2008) is numerically irrelevant. The "ytilda" EASI specification approximates real expenditures using a common Stone price deflator for all households circumventing endogeneity but loosing idiosyncratic information. See Lewbel & Pendakur (2008) for details. "ystone" is the default.
	- `pzint`: Allowed values are "pzintno" and "pzintyes". This corresponds to whether the interaction term between prices, p, and socio-demographic demand shifters, z, is considered or not. "pzintno" significantly speeds up the estimation. "pzintno" is the default.
	- `ycentered_yesno`: Allowed values are "ycentered" and "yuncentered". This corresponds to whether the log of the Stone index-deflated nominal expenditures (y_stone or y_tilda) is centered with the weighted country-mean. "yuncentered" is the default.
	- `shares`: Allowed values are "actual" and "expected". Not relevant to this analysis. "actual" is the default.
- Alternative multi-regional input-output (MRIO) data (used for robustness analyses)
	- `IorP`: Allowed values are "pxp" and "ixi". Refers to the categorisation of the technical coefficient matrix by product (pxp) or by industry (ixi). The respective data must be available in the directory `/source/00_data/MRIO/`. Not relevant to this analysis. "pxp" is the default.
	- `year_io`: In principle, every year for which EXIOBASE data tables are available specified as an integer are allowed. This corresponds to the EXIOBASE base year used in the analysis. The respective data must be available in the directory `/source/00_data/MRIO/`. In this analysis, "2011" and "2019" are compared. "2019" is the default.
- Bootstrapping related settings (allow for the flexibel and piecewise collection of bootstrapping samples)
	- `nbootstart`: Any positive integer. "0" is the default.
	- `nbootend`: Any positive integer. "100" is the default.

2. Source the file `/source/mainfile.R` from within an R session in the terminal/console **OR** run code in `/source/mainfile.R` interactively in RStudio.
- If you source the mainfile in an R session, make sure to set the working directory beforehand to the directory `/source/`.
- If you run the code in the mainfile interactively, the working directory is set programmatically.

3. Output is written to the directory `/build/`
- `/build/data/` contains all data.
- `/build/figures/` contains all figures as pdf-files.
- `/build/tables/` contains latex code for all tables.

## Authors and acknowledgment
Authors:
- Charlotte Plinke, cplinke@pik-potsdam.de, Potsdam Institute for Climate Impact Research, Member of the Leibniz Association, Telegrafenberg A 31, 14473 Potsdam, Germany
- Michael Sureth, michael.sureth@pik-potsdam.de, Potsdam Institute for Climate Impact Research, Member of the Leibniz Association, Telegrafenberg A 31, 14473 Potsdam, Germany

Citation: Plinke, C. and Sureth, M. (2024). _Code for Environmental Impacts from European Food Consumption Can Be Reduced with Carbon Pricing or a Value-Added Tax Reform_. Submitted to Nature Food.

Parts of our code build on the work contained in the R-package [easi](https://cran.r-project.org/src/contrib/Archive/easi/) by Hoareau, S. et al. (2012) as well as on code and input data kindly shared by Benedikt Bruckner to whom we would like to express our sincere gratitude.

## License
GNU GPLv3

In addition to the software license, we kindly urge fellow scientists who use this code to prioritize reproducibility **and** open access in their own work. We encourage users to make their code openly accessible by ensuring that their code is publicly hosted and can be easily implemented on major platforms, without necessitating extensive reverse-engineering efforts. We believe this is vital for fostering and maintaining a culture of collaboration and transparency within the academic community.
