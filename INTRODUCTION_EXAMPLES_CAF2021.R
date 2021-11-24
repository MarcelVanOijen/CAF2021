#                     CAF2021: INTRODUCTION AND USER GUIDE
#                        Marcel van Oijen, 2021-11-24


################################################################################
### 0. Introduction to CAF2021
################################################################################

# CAF2021 is a process-based model for the biogeochemistry of coffee
# agroforestry systems. We give a short description here, focusing on how the
# model differs from its predecessor models CAF2007 and CAF2014.

# CAF2007 was introduced in the following two papers:
# Van Oijen, M. et al. (2009). Plot-scale modelling of coffee agroforestry
#    systems in Central America. In: Rapidel, B. et al. (Eds), Modelling
#    Agroforestry Systems. Workshop Proceedings, CATIE, Costa Rica, 25-29 Feb
#    2008. CATIE, Turrialba: 191-202.
# Van Oijen, M. et al. (2010). Coffee agroforestry systems in Central America:
#    II. Development of a simple process-based model and preliminary results.
#    Agroforestry Systems 80: 361-378.
# CAF2014 was introduced in the following paper:
# Ovalle-Rivera, O. et al. (2020). Assessing the accuracy and robustness of a
#    process-based model for coffee agroforestry systems in Central America.
#    Agroforestry Systems 94: 2033-2051.

# CAF2021 simulates a coffee agroforestry system where the coffee plants are
# shaded by up to 3 tree species. The model allows for two lower-layer tree
# species, but no more than one upper-layer tree species. In typical use of the
# model the upper layer would be occupied by timber trees and the lower layer by
# service and fruit trees. The simulated tree species can have different initial
# planting densities and thinning regimes. The model simulates the
# biogeochemistry - flows of carbon, nitrogen and water - of the
# soil-coffee-trees system. State variables are pools of carbon, nitrogen and
# water as well as coffee developmental stage. Height and crown area growth are
# simulated for each of the tree species. The model uses a daily time step and
# therefore requires daily weather data (light, temperature, rain, wind,
# humidity) as input. Further required inputs are atmospheric CO2, soil
# properties (slope, water retention parameters, initial contents of C and N)
# and management calendars for fertilisation, pruning and thinning. The pruning
# and thinning calendars can be specified separately for each plant species.

# The outputs from the model are time series with daily values of state
# variables and processes including ecosystem services such as productivity of
# coffee and trees, N-leaching, N-emission, erosion in the form of soil organic
# matter loss (C & N) in run-off, C-sequestration in the soil.

# The model is implemented in FORTRAN but called from a user-interface written
# in R.

# CAF2021 differs from its predecessor models in the following respects:
# - There can be three tree species present rather than just one. All tree
#   variables are implemented as dynamic arrays whose length equals the number
#   of tree species. Users with experience in programming can therefore
#   relatively easily extend the number of shade tree species beyond three.
# - Fruit production can be simulated.
# - Lower-layer trees do not overlap, so the number of ground-cover conditions
#   (vertical vegetation profiles) in any simulated field is at most six: coffee
#   shaded by (1) no trees, (2) service trees, (3) fruit trees, (4) timber
#   trees, (5) service + timber trees, (6) fruit + timber trees. The model keeps
#   track of the fractional ground area covered by each of these six categories
#   (see Supplementary Information S1 for an example).
# - The relative heights, LAI values and light extinction coefficients of the
#   two tree species within ground cover categories (5) and (6) determine their
#   access to light.
# - Morphology is simulated using allometric equations but for each species a
#   genetic or management-induced maximum height can be specified.
# - Shade management can be fully specified using calendars for dates and
#   intensities of pruning and thinning. Alternatively, it may be simulated as
#   being goal-directed toward a specified shade level.

# This file shows how to run the CAF2021 model (v. 2021-11-23), inspect output,
# change parameters, and do a Bayesian calibration (BC).
# If this file is opened in R, you can run all lines of code (i.e. lines that
# do not begin with '#' which are comment lines) straight away.

# The model is operated from R, but the CAF2021-code itself is in FORTRAN.
# Parameters, weather and management (fertilisation, pruning of coffee and
# trees, thinning of trees) are all defined in R and then they are given as
# input to the main model subroutine called 'CAF2021', which we find in file
# 'CAF2021.f90'.

# Several R-functions have been defined that make working with the model easy.
# The most important function is 'run_model()', which is the R-function that
# runs CAF2021 itself. Examples are given in section 1. below. But there are
# several other routines (for plotting, making tables, sensitivity analysis)
# that we'll explain below as well.

# One important thing to realise is that at the beginning of an R-session,
# R begins with a clean slate, i.e. it does not know the modelling functions
# that we defined. So we must always begin by running a so-called
# 'initialisation file' which will make the functions known to R. We include
# examples of such site-specific initialisation files, with names like
# 'initialise_CAF2021_Turrialba_01_E_AC.R' which can be used for that purpose,
# but you may want to add files for your own sites.

# Each of the site-specific initialisation files includes a call to the R-script
# 'initialisation_CAF2021_general.R' where the modelling functions are defined.
# The site-specific initialisation files are also the place where we set the
# parameter values, weather data and management regimes for the runs that we are
# going to do.

# So, the standard way of working with CAF2021 involves at least two steps:
#   1. run an initialisation file in R,
#   2. use R-functions to run the model and do analyses.

# The rest of this file gives examples of working with this set-up.


################################################################################
### 1. Initialise and run the model for a site
################################################################################

# We use a pre-defined R-file to initialise the model for a coffee agroforestry
# system with one shade tree species (Erythrina poeppigiana) in Turrialba
# (Costa Rica) 2000-2017:

  source("initialisation/initialise_CAF2021_Turrialba_01_E_AC.R")

# Running this statement will define the seven inputs to the model:
#   (1) parameter values,
#   (2) weather data,
#   (3) calendar for fertilisation,
#   (4) calendar for pruning of coffee,
#   (5) calendars for pruning of trees (one for each tree species),
#   (6) calendar for thinning of trees (one for each tree species),
#   (7) length of simulation period.
# You can check the values of these inputs in your R-session (or by inspecting
# the initialisation file of course):

  print( cbind( names_params, params ) )
  print( head( matrix_weather ) )
  print( head( calendar_fert  ) )
  print( head( calendar_prunC ) )
  print( head( calendar_prunT ) )
  print( head( calendar_thinT ) )
  print( NDAYS )
  
# We run CAF2021 by calling the function 'run_model'.
# We run the model with the above seven inputs, and put the results in "output":

  output <- run_model( p     = params,
                       w     = matrix_weather,
          		  			 calf  = calendar_fert,
			  	          	 calpC = calendar_prunC,
				  	           calpT = calendar_prunT,
					             caltT = calendar_thinT,
                       n     = NDAYS )
  
# If we do not change the order of the inputs, the "p=" etc. are not needed:

  output <- run_model( params, matrix_weather, calendar_fert, calendar_prunC,
	            				 calendar_prunT, calendar_thinT, NDAYS )

# By default, the 'run_model' function expects arguments called 'params',
# 'matrix_weather', 'calendar_fert' ... 'NDAYS', so in this case we could
# have left those out too, and still got the same output!

  output <- run_model()

# We can plot output from CAF2021 on screen. The variables we can plot are
# listed in the vector 'outputNames' and their units are in 'outputUnits':

  print( cbind( outputNames, outputUnits ) )
  
# Let's choose some variables from this list to plot
  
  plot_output( output, vars=c("Ac(1)","Ac(2)","harvDM_f_hay") )

# We can also refer the variables by number. The next statement is equivalent to
# the last one:
  
  plot_output( output, vars=outputNames[ c(4:5,32) ] )

# We can send the plot to a pdf-file instead of the screen as follows.
# [We'll use the filename "output.pdf" but any other name is OK too]

  pdf( "output.pdf" )
    plot_output( vars=outputNames[ c(4:5,32) ] )
  dev.off()

# We can also request a txt-file with some tabulated results:

  table_output( vars=outputNames[ c(4:5,32) ] )

# That command will place a new file in the root directory, named output[..].txt,
# which you can examine in EXCEL for example.

  
################################################################################
### 2. Comparing the output from multiple runs
################################################################################

# If you run the model multiple times, e.g. with different parameter values or
# harvest times, then the outputs from the different runs can easily be compared
# with each other.

  source("initialisation/initialise_CAF2021_Turrialba_01_E_AC.R")
  output_shade   <- run_model()
  source("initialisation/initialise_CAF2021_Turrialba_19_PS_AC.R")
  output_noshade <- run_model()
  plot_output( list(output_shade,output_noshade),
               vars=outputNames[c(4:5,32)] )

# The example shows that if you put the outputs from different runs in one
# list, then the 'plot_output()' function knows how to plot the results in one
# graph.

  
################################################################################
### 3. Selecting a different set of model output variables
################################################################################

# A common change is adding new output variables to the model. The list of
# output variables produced by the model can be seen in two different files:
  
#   'initialisation/initialisation_CAF2021_general.R'
#   'model/CAF2021.f90'.
  
# MAKE SURE THAT THOSE TWO FILES STAY CONSISTENT!
  
# And remember to recompile the model if you change the two files, because the
# second file is a FORTRAN file. Recompiling is done simply by running the file
# 'compile_CAF2021.bat' (on a Windows machine) or 'compile_CAF2021.command' (on
# a Mac). That will create a new 'CAF2021.DLL'.


################################################################################
### 4. Changing things in the model: how do we do that in general?
################################################################################

# The general idea is always to copy and modify one of the existing files to
# do anything different. If you change any of the FORTRAN files (the ones with
# extension ".f90"), the model needs to be compiled again.

  
################################################################################
### 5. Changing parameter values
################################################################################

# Parameters can be changed in different ways. First, you can change or add a
# column in 'parameters/parameters_default.txt'. Once you've added a column,
# you can refer to that column in the initialisation. See how different
# parameter columns (variable 'parcol') are specified in the following two
# files:
  
#   'initialisation/set_CAF2021_Turrialba.R',
#   'initialisation/set_CAF2021_Turrialba_MODIFIED.R'.
  
# These two files are called respectively from the two initialisation files:
  
#   'initialise_CAF2021_Turrialba_01_E_AC.R' and
#   'initialise_CAF2021_Turrialba_01_E_AC_MODIFIED.R'.
  
# A second way to change parameters would be to initialise the model as normal,
# using an initialisation file and then modify 'params' in R-Studio itself.
# We now show how that can be done. Let's begin as always by running an
# initialisation file.

  source("initialisation/initialise_CAF2021_Turrialba_01_E_AC.R")

# That command sets all parameter values, which we can check as before:

  print( cbind( names_params, params ) )

# Say we want to vary parameter 'KEXT'. We see from the list KEXT is the 10th
# parameter. So the current value of KEXT is stored as 'params[10]'.
# We can now, for example, run the model with the default value of KEXT and half
# that value:

  output_default <- run_model()
  params[10]     <- 0.5 * params[10]
  output_KEXT_50 <- run_model()

# The two sets of model outputs can now be compared:

  plot_output( list( output_default, output_KEXT_50 ),
               vars=outputNames[c(4:5,32)] )
  
# A simpler way of changing parameter values is using the function 'set_par()'.
# Here are examples of changing a single parameter or multiple ones:
  
  params <- set_par( 'KEXT', 0.9 )
  params <- set_par( c('KEXT','YG' ), c( 0.9, 0.7 ) )
  
# You can inspect the vector 'params' yourself to check that these commands
# make the required parameter changes.
  
# The fourth and simplest way of studying parameter changes is to
# use the 'sensitivity analysis' function SA(), which is explained in the
# following section.

  
################################################################################
### 6. Parameter sensitivity analysis
################################################################################

  source("initialisation/initialise_CAF2021_Turrialba_01_E_AC.R")

# If we want to see the impact of changing a single parameter, we can
# use the 'SA' function. We tell SA by how much the parameter should be
# multiplied in the different runs. For example:

  SA( "RAINMULT", pmult=c(1,0.5,0.25), vars=outputNames[c(4:5,32)] )

# The results of this sensitivity analysis are written to two files,
# one with plots (called 'SA_[].pdf'), and one with a table ('SA_[].txt').

# A full specification of the 'SA()' function, using all the defaults (so it
# gives the same results as above), is as follows:

  pmult     <- 2^(-1:1)
  vars      <- outputNames[c(4:5,32)]
  nrow_plot <- ceiling( sqrt((length(vars)+1) * 8/11) )
  SA( parname_SA = "KEXT",
      pmult      = pmult,
      vars       = vars,
      leg_title  = parname_SA,
      nrow_plot  = nrow_plot,
      ncol_plot  = ceiling( (length(vars)+1)/nrow_plot ),
      lty        = rep(1,length(pmult)),
      lwd        = rep(3,length(pmult)),
      file_init  = "initialisation/initialise_CAF2021_Turrialba_01_E_AC.R" )

# So the SA-function can take nine arguments, which we can change as we see fit.

  
################################################################################
### 7. BUILDING UP A LIBRARY OF INFORMATION ON PARAMETERS & SITES
################################################################################

# The file 'parameters_default.txt', together with the directory
# 'initialisation', can be seen as the heart of the system, where you can
# gradually store more and more information about different sites and/or
# treatments. Columns can be added to the parameters file, for example if
# a new shade tree species is introduced.
  
# Each initialisation file points to one column in parameters_default.txt but
# also includes other (non-parameter) information, e.g. about management.


################################################################################
### 8. How to add parameters
################################################################################

# If you change the model and include new parameters, the model needs to be told
# the names and values of the parameters. Three files need to be changed:
# 1. 'declare_parameters.f90': include the new parameter(s) where you want
# 2. 'set_params.f90'        : add the parameter(s) at the bottom
# 3. 'parameters_default.txt': add the parameter(s) at the bottom
# And remember to recompile the model, because you have changed Fortran files.


################################################################################
### 9. Bayesian Calibration (BC)
################################################################################

# BC is carried out by means of Markov Chain Monte Carlo sampling (MCMC), using
# the Metropolis algorithm. We only give a brief introduction here to the
# relevant files.

# MCMC involves running the model many times for different parameterisations. 
# We begin with an example of using MCMC to carry out a single-site BC. Usually
# a chain of more than 10^4 model runs is required. That is very time-consuming,
# so if you just want to see a short first test of BC in action, make sure that
# 'nChain' (which is the number of iterations in the MCMC) is set to a low
# number like 500. You set the value of nChain in BC-control file like
# 'BC_CAF2021_Turrialba_01_E_AC.R'), which you can then run:

  source("BC_CAF2021_Turrialba_01_E_AC.R")

# The results of the BC are written to five files:
# 1. 'CAF2021_parModes_[].txt': parameter values (all statistics, ASCII-format)
# 2. 'CAF2021_parMAP_[].rds': parameter values (only MAP, R-format)
# 3. 'BC_parameters_traceplots_[].pdf': parameter trace plots
# 4. 'BC_parameters_priorbeta_histograms_[].pdf': parameter distributions
# 5. 'BC_outputs_data_[].pdf': model outputs and the calibration data.

# Multi-site BC can be done as in the following example:

  source("BC_CAF2021_Turrialba_01-15-19.R")

# Note that it is now possible for every parameter to specify its own degree of
# 'site-specificity'. That is done in the last column in the file
# 'parameters_BC_[].txt'. There you can specify whether the parameter should be
# calibrated generically for all of the sites, or that it should get different
# values for different subsets of the sites.

