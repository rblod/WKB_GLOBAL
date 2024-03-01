#!/usr/bin/env python
"""
Script to process data from CSV files and create NetCDF files.

Usage:
    ./genebathy.py [--datadir <DATADIR>] [--topo <TOPO_FILE>] [--length <LENGTH_FILE>] [--nstart <NSTART>] [--nend <NEND>] [--doplot <DOPLOT>]

Options:
    -h --help       Show this help message and exit.
    --datadir <DATADIR>     Data directory [default: /Users/rblod/202302_WAGNER/Wagner_to_Rachid/]
    --topo <TOPO_FILE>      Topographic file [default: topo_bathy.csv]
    --length <LENGTH_FILE>  Length profile file [default: length_profile.csv]
    --nstart <NSTART>       Start index for loop [default: 1]
    --nend <NEND>           End index for loop [default: 10]
    --doplot <DOPLOT>       Plot the bathy [default: 0] 

The script reads data from CSV files containing topographic and length profiles,
processes the data, and creates NetCDF files.

Author:
    Rachid Benshila
"""

import numpy as np
from netCDF4 import Dataset
import csv
import matplotlib.pyplot as plt
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

def ncf_create_topofile(fname, x, y, z):
    """
    Create a NetCDF file from data.

    Args:
        fname (str): Filename of the NetCDF file to create.
        x (numpy.ndarray): Data for the xi_rho dimension.
        y (numpy.ndarray): Data for the eta_rho dimension.
        z (numpy.ndarray): Data for the h variable.

    Returns:
        None
    """
    ncfile = Dataset(fname, 'w', format='NETCDF4')
    xi_rho_dim = ncfile.createDimension('xi_rho', size=len(x))
    eta_rho_dim = ncfile.createDimension('eta_rho', size=len(y))
    xi_rho_var = ncfile.createVariable('xi_rho', 'f4', ('xi_rho',))
    eta_rho_var = ncfile.createVariable('eta_rho', 'f4', ('eta_rho',))
    h_var = ncfile.createVariable('h', 'f4', ('eta_rho', 'xi_rho'))
    xi_rho_var[:] = x
    eta_rho_var[:] = y
    h_var[:] = z
    ncfile.close()

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
    nstart = int(arguments['--nstart'])
    nend = int(arguments['--nend'])
    DOPLOT=int(arguments['--doplot'])

    for niter in range(nstart, nend + 1):
        print('profile :', niter)  # Display the current iteration

        # Read data from CSV files
        topo = read_line_from_csv(topo_file, niter)
        if DOPLOT:
        	plt.plot(topo)  # Plot topographic data
        	plt.show()

        length = read_line_from_csv(length_file, niter)
        dx = length / (len(topo) - 1)
        xi = np.linspace(0, length, len(topo))  # Generate xi vector
        eta = np.arange(0., 3 * dx, dx)  # Generate eta vector

        # Create a NetCDF file from the data
        fileout = 'bathy_p' + str(niter).zfill(5) + '.nc'
        ncf_create_topofile(fileout, xi, eta, topo)
