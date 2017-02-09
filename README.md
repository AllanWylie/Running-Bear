# Running-Bear

FORTRAN utility programs by
Allan Wylie

PDTRIB calculates the impact of curtailing
groundwater rights in basins tributary to the ESPA. Two input files are
required. A POD file listing number of PODs in the file on line 1, the water
right number, priority date, irrigation cfs, basin number (does not need to be
a formal basin number), and EXCEL serial date for the priority date (days since
1/0/1900 to the priority date is an EXCEL serial date) on line 2 through the
number of PODs. The following Figure illustrates the format.
The second file is the
consumptive use file. The first line contains the number of basins represented
in the file, the second line contains basin number (must correspond to the
basin number in the POD file), basin name, groundwater irrigated acres in the
basin, and consumptive use per acre per year for that basin. 
The basins need to be numbered sequentially beginning
with number 1. The following Figure shows the proper format.
PDTRIB asks for a POD File, a
Consumptive use file, and a priority date, it then calculates the benefit of
curtailing to the nominated priority date based on the acres and consumptive
use contained in the Consumptive use file and the priority dates and permitted
irrigation cfs in the POD File.


RIVDRYBED reads an integer array identifying the
dry river reach. The user then identifies a MODFLOW river file that needs
updating and ASCII file identifying the total number of stress periods and the
stress periods for which the river stage is dry. The river stage will be set
equal to the river bottom for these stress periods. RIVDRYBED then loops
through the MODFLOW river file and makes the river stage equal to the river
bottom in the nominated river reach during the nominated stress periods.

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

RIVELEV reads a MODFLOW river file looking for river cellsanomalously higher 
than its neighbors. It then lowers the elevation of theoffending cell to the elevation 
of its highest neighbor. There is an option toenter a tolerance, if the nominated 
tolerance is 0, then cell i,j can not beany higher than any of it’s neighbors. 

RIVSTAGE makes a transient MODFLOW river file from a steady state
river file, an integer array, and a reach list file. The reach list file
consists of a row of text identifying the numerical variables in the second row
(number of stress period, number of reaches, reach name, integer id for the
reach, row where the upriver gage resides, and row where the downriver gage
resides. The program interpolates between upriver change in stage and downriver
change in stage. The below figure shows a typical Reach List File

