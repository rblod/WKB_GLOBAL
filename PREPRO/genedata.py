#!/usr/bin/env python
"""
Script to process data from CSV files and create NetCDF files.

Usage:
    ./script.py [--datadir <DATADIR>] [--topo <TOPO_FILE>] [--length <LENGTH_FILE>] [--orient <ORIENT_FILE>] 
    [--hs <HS_FILE>] [--tp <TP_FILE>] [--dir <DIR_FILE>] 
    [--nstart <NSTART>] [--nend <NEND>]
    [--tstart <TSTART>] [--tend <TEND>]

Options:
    -h --help               Show this help message and exit.
    --datadir <DATADIR>     Data directory [default: /Users/rblod/202302_WAGNER/Wagner_to_Rachid/]
    --topo <TOPO_FILE>      Topographic file [default: topo_bathy.csv]
    --length <LENGTH_FILE>  Length of the profile [default: length_profile.csv]
    --orient <ORIENT_FILE>  Orientation of the profile [default: dir_profile.csv]
    --hs <HS_FILE>          Hs file [default: waves_ERA5_hs.csv]
    --tp <TP_FILE>          Tp file [default: waves_ERA5_tp.csv]
    --dir <DIR_FILE>        Dir file [default: waves_ERA5_dir.csv]    
    --nstart <NSTART>       Start index for profile loop [default: 1]
    --nend <NEND>           End index for profile loop [default: 10]
    --tstart <TSTART>       Start index for time loop [default: 1]
    --tend <TEND>           End index for time loop [default: 10]

The script reads data from CSV files containing topographic and length profiles,
processes the data, and creates NetCDF files.

Author:
    Rachid Benshila
"""

import numpy as np
from netCDF4 import Dataset
import csv
import sys
import os
from docopt import docopt

def ncf_get_var(fname, var):
    """
    Retrieve a variable from a NetCDF file.

    Args:
        fname (str): Filename of the NetCDF file.
        var (str): Name of the variable to retrieve.

    Returns:
        numpy.ndarray: Retrieved variable data.
    """
    nc = Dataset(fname, 'r')
    myvar = nc.variables[var][:]
    nc.close()
    return myvar

def read_line_from_csv(csvfile, niter):
    """
    Read a specific line from a CSV file.

    Args:
        csvfile (str): Filename of the CSV file.
        niter (int): Line number to read.

    Returns:
        numpy.ndarray: Data from the specified line.
    """
    with open(csvfile, newline='') as csvfile:
        csv_reader = csv.reader(csvfile)
        for index, line in enumerate(csv_reader, start=1):
            if index == niter:
                myvar = np.array(line, dtype=float)
                break
    return myvar

if __name__ == '__main__':
    arguments = docopt(__doc__)

    # Parse arguments
    DATADIR = arguments['--datadir']
    topo_file = os.path.join(DATADIR, arguments['--topo'])
    length_file = os.path.join(DATADIR, arguments['--length'])
    orient_file = os.path.join(DATADIR, arguments['--orient'])
    hs_file = os.path.join(DATADIR, arguments['--hs'])
    tp_file = os.path.join(DATADIR, arguments['--tp'])
    dir_file = os.path.join(DATADIR, arguments['--dir'])

    # Space and time window
    nstart = int(arguments['--nstart'])
    nend = int(arguments['--nend'])
    tstart = int(arguments['--tstart'])
    tend = int(arguments['--tend'])

    # Iterate over profiles
    for niter in range(nstart, nend + 1):
        print('Data for profile:', niter)
        
        # Read profile-specific data from CSV files
        topo = read_line_from_csv(topo_file, niter)
        length = read_line_from_csv(length_file, niter)
        orient = read_line_from_csv(orient_file, niter)
        orient[orient < 0] = np.mod(orient[orient < 0], 360) - 180


        # Read time series data (waves) from CSV files
        hs_line = read_line_from_csv(hs_file, niter)
        tp_line = read_line_from_csv(tp_file, niter)
        dir_line = read_line_from_csv(dir_file, niter)
        
        # Iterate over time steps
        for titer in range(tstart - 1, tend): # Python index starts at 0
            print('   At time:', titer+1)
            hs = hs_line[titer]
            tp = tp_line[titer]
            dr = dir_line[titer]

            # Calculate WKB parameters
            wkb_amp = (np.sqrt(2) / 4) * hs
            wkb_per = tp
            
            # Determine wave direction
            if dr >= np.mod(orient[0] + 90, 360) and dr < np.mod(orient[0] + 180, 360):
                wkb_dir = 90.  
            elif dr < np.mod(orient[0] - 90, 360) and dr > np.mod(orient[0] - 180, 360):
                wkb_dir = -90.
            else:
                wkb_dir = orient[0] - dr  # Wave direction (°) relative to crosshore 

          #  print('hs =', hs, 'tp =', tp, 'dir =', dr,'orient =',orient[0] )
          #  print('wkb_amp =', wkb_amp, 'wkb_per =', wkb_per, 'wkb_dir =', wkb_dir)
          #  print('')
            
            # Lire le contenu du fichier namelist.template
            script_path = os.path.abspath(__file__)
           # Répertoire du script
            script_directory = os.path.dirname(script_path)
            with open(script_directory+'/namelist.template', 'r') as f:
                template_content = f.read()
            file_in = 'bathy_p' + str(niter).zfill(5) + '.nc'  
            file_out= 'output_p' + str(niter).zfill(5) + '_t' + str(titer+1).zfill(5)+ '.nc' # check the max number of samples for the digits
            logfile = 'wkb.output_p' + str(niter).zfill(5) + '_t' + str(titer+1).zfill(5)            
            modified_content = template_content.replace('<FILE_IN>', file_in)
            modified_content = modified_content.replace('<FILE_OUT>', file_out)
            modified_content = modified_content.replace('<LOGFILE>', logfile)
            modified_content = modified_content.replace('<WKB_AMP>', str(wkb_amp))
            modified_content = modified_content.replace('<WKB_PRD>', str(wkb_per))
            modified_content = modified_content.replace('<WKB_DIR>', str(wkb_dir))
           
            namelist_out='namelist_p' + str(niter).zfill(5) + '_t' + str(titer+1).zfill(5)
            with open(namelist_out, 'w') as f:
                f.write(modified_content)

