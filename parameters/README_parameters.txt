1. parameters_default.txt

The tree parameters that appear three times, e.g. TREEDENS0(1:3), will later be used to distinghuish service trees, fruit trees and timber trees, in that order. However, fruit trees are not defined yet in the current model version, and the values that are currently assigned to these parameters in value-column 1 (column B when opened in Excel) are for poro, poro and Terminalia, respectively.

2. parameters_BC_[].txt

These files define the prior distribution for the calibration parameters. The files have five columns.
- Column 1: Parameter name,
- Column 2: Minimum value of the parameter in the prior,
- Column 3: Mode,
- Column 4: Maximum,
- Column 5: Site numbers.
The site numbers in column 5 show for which sites the values in columns 2-4 are intended. "nSites" is the number of sites in the calibration (so if there is only one site, then nSites=1). Parameters can be generic or site-specific. For generic parameters, we can write "1:nSites" in column 5. For each site-specific parameter, there must be more than one line in the file, and in column 5 we then indicate to which site(s) the values in column 2-4 apply.

But what do we do with site-specific parameters for which the prior is the same in different sites although the posterior can be different)? Say we have two sites, and a site-specific parameter "P". Let's further assume that we want to have the same prior for P at both sites (although of course the posterior for P will differ). Say, the {min,mode,max} are {0,50,100}.Then we have to specify two lines for P, which are identical in all columns except the last:
  P 0 50 100 1
  P 0 50 100 2
