# Running-Bear

FORTRAN utility programs by
Allan Wylie

RIVELEV reads a MODFLOW river file looking for river cells
anomalously higher than its neighbors. It then lowers the elevation of the
offending cell to the elevation of its highest neighbor. There is an option to
enter a tolerance, if the nominated tolerance is 0, then cell i,j can not be
any higher than any of it’s neighbors. 


MKRIVTPL makes a PEST template
file from a MODFLOW river file and an integer array that identifies the river
reaches for riverbed conductance parameters. After reading in the integer array
and MODFLOW river file, MKRIVTPL reads an ASCII text file called a ReachID
file, that identifies reach number (from the integer array) and PEST parameter
name. The first line in the ReachID file contains the number of stress periods
in the model, the second contains the number of reaches, subsequent lines
contain 2 items, 1) integer number for reach (corresponding to the number used
to identify the reach in the integer array), and 2) parameter name
(corresponding to the parameter name used in the PEST Control file).

Example of ReachID file
193   # Number of stress periods in the MODFLOW river file
21    # Number of reaches identified in integer array
1    WR_nKe_Hul  # Integer for reach and riverbed conductance parameter name
2    WR_Hul_Ket  # Integer for reach and riverbed conductance parameter name
3    WR_Ket_Gim  # Integer for reach and riverbed conductance parameter name

