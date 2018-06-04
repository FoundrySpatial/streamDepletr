## Package Style and Conventions

# Useful notes on creating an R package
- https://support.rstudio.com/hc/en-us/articles/200486488-Developing-Packages-with-RStudio
- https://hilaryparker.com/2014/04/29/writing-an-r-package-from-scratch/
- https://github.com/ThinkR-open/prepare-for-cran
- http://www.masalmon.eu/2017/12/11/goodrpackages/
- http://r-pkgs.had.co.nz/vignettes.html

# Style
Goal: common style among all functions within the package. 
Solution: Use Hadley's guide http://r-pkgs.had.co.nz/style.html

When calling a function from a different package, make sure to explicitly
define the package (e.g. dplyr::summarize).

# Function names
Most of these functions are introduced in a paper and that is how they are typically
referred to in the literature (e.g. the Hunt solution). We will follow that convention
for function names.

# Variables
Use these variable names, which are consistent with the hydrogeological literature:
- `Qw`   = well pumping rate [L3/T], = `Qf/Qs`
- `Qf`   = capture fraction, [-], = `Qw/Qs`
- `Qs`   = streamflow depletion, [L3/T], = `Qw*Qf`
- `d`    = distance from well to stream [L]
- `S`    = aquifer storage coefficient [-] (specific yield for unconfined aquifer, storativity for confined aquifer)
- `Kh`   = aquifer horizontal hydraulic conductivity [L/T]
- `Kv`   = aquifer vertical hydraulic conductivity [L/T]
- `b`    = aquifer saturated thickness [L]
- `Tr`   = aquifer transmissivity [L2/T], = `Kh*b`
- `t`    = time since pumping started [T]
- `Kriv` = riverbed hydraulic conductivity [L/T]
- `briv` = thickness of riverbed semipervious layer [L]
- `w`    = stream width [L]
- `beff` = effective transmissivity [L]. Reeves et al. (2009) use the distance from bottom of stream to top of well screen
- `lmda` = streambed conductance [L/T]
