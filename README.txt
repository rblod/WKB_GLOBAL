Overall software strucured in 2 steps:
-------------------------------------
- Preprocessing in Python (bathy and wave forcing) : PREPRO directory
- The model sources in Fortran: WKB_MODEL directory 


Preprocessing step:
-------------------
2 scripts in PREPRO directory
- genebathy.py : generate the profiles from cvs files, type 
       ./genebathy.py -h
for help.
=> generates netcdf bathy files to be used by wkb, bathy_p*.nc, with * the number of the profile

- genedata.py : generate the waves conditions from cvs files, type 
       ./genedata.py -h
for help
=> generates text files with wkb_amp,wkm_per,wkb_dir, namelist_p*_t#, with * the number of the profile, # the time window
   (it use the file namelist.template as a template)  

Model step:
-----------
The model needs to be compiled once for all, unless the sources are modified, to generate a binary file

- Compile WKB (once for all)
in directory WKB_OFFLINE, create a make.inc files with compilation options
based on example in WKB_OFFLINE/Config, typically in a terminal:
    cp Config/make.gfortran make.inc
Then check make.inc to check the options 
You're ready to compile, in a terminal:
 	gmake

In WKB_OFFLINE directory, an executable is created called  wbk.exe. 

- Run the model
This executable takes a text file as an input. Typically the is an example of text file in WKB_OFFLINE called namelist.
This text files contains several informations, such as wkb forcing values, the name of the bathy file etc
To run the model, in a terminal
    ./wkb.exe inputfile   (inputfile doesn't have to be called inputfile...)

Example of a whole sequence:
----------------------------
In the main directory WKB_OFFLINE 
   ./PREPRO/genebathy.py   (with proper options)  => 10 bathy files created by default
   ./PREPRO/genedata.py    (with proper options)  => 5 time forcing for 10 bathy files by default, 50 namelist_ created
   ./wekb.exe namelist_p00001_t00001   => run the first profile, first data
   ./wekb.exe namelist_p00001_t00002   => run the first profile, second data
   .....
   ./wekb.exe namelist_p00002_t00001   => run the second profile, first data
  

Other:
------
in WKB_OFFLINE :
  - run_all.sh : main script chaining preprocessing and execution, looping on a number of profiles
  - clean.sh   : small script to remove all the namelist and bathy files

Requirements:
-------------
Preprocessing: python modules mandatory 
 - numpy 
 - netCDF4 
 - csv
 - matplotlib
 - sys
 - os
 - docopt 
Model : a fortran compiler, netcdf library
