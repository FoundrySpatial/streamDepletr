## Package Style and Conventions

# Useful notes on creating an R package
https://support.rstudio.com/hc/en-us/articles/200486488-Developing-Packages-with-RStudio
https://hilaryparker.com/2014/04/29/writing-an-r-package-from-scratch/

# Style
Goal: common style among all functions within the package. 
Solution: Use Hadley's guide http://r-pkgs.had.co.nz/style.html

# Function names
Most of these functions are introduced in a paper and that is how they are typically
referred to in the literature (e.g. the Hunt solution). We will follow that convention
for function names.

# Variables
Use these variable names, which are consistent with the hydrogeological literature:
-Qw   = well pumping rate [L3/T]
-Qf   = capture fraction, [-] (=streamflow reduction as a proportion of pumping rate)
-d    = distance from well to stream [L]
-S    = aquifer storage coefficient [-] (specific yield for unconfined, storativity for confined)
-Kh   = aquifer horizontal hydraulic conductivity [L/T]
-Kv   = aquifer vertical hydraulic conductivity [L/T]
-b    = aquifer saturated thickness [L]
-Tr   = aquifer transmissivity [L2/T] (=Kh*b)
-t    = time since pumping started [T]
-Kriv = riverbed hydraulic conductivity [L/T]
-briv = thickness of riverbed semipervious layer [L]
-w    = stream width [L]
-beff = effective transmissivity [L] (Reeves et al., 2009, use distance from bottom of stream to top of well screen)
-lmda = streambed conductance [L/T]
