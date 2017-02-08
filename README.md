# Running-Bear

FORTRAN utility programs by
Allan Wylie

ADDHEAD reads in a 2D array (ASCII or binary) and outputs an ASCII
2D array with a header.  The header can
consist of two integers representing the number of columns and rows or an
ARCview compatible ASCII grid header. 
ADDHEAD requires a PEST grid
specification file.

ADDWELL modified from John
Doherty program of the same name to output in free format.  Program ADDWELL requires three items of
information. These are:

·     
the name of an existing well package MODFLOW
input or supplementary file,

·     
the name of a “well addition file”, the data
which will be added to the already existing well package input or supplementary
file, and

·     
the name for a new well package input or
supplementary file which it will create; this file will contain all data
included in the original well package file, together with the data contained in
the well addition file.

Note that a MODFLOW package “supplementary file” is a
MODFLOW input file containing only stress period data (ie. data normally read
by the “…RP” subroutine pertaining to the relevant package). MODFLOW formatting
requirements are relaxed so that each line of data may be supplied in tab,
comma or space-delimited format.




 
  
  
  1 21 7   7580.0000 
  1    36
  1 21 7   7580.0000 
  90   130
  1 20 8   7580.0000 
  40   87
  1 20 8   7580.0000 
  135  145
  1 20 17  5322.0000 
  1    145
  1 20 18  5322.0000 
  1    145
  1 19 18  5322.0000 
  1    145
  1 19 17  5322.0000 
  1    145
   
  
  
 




The figure below shows a well addition file.

A Well addition file.

Each line of a well addition file contains 6 items. The
first three items are the layer, row and column number of a particular cell.
Then follows the injection rate pertaining to that cell (negative values
indicate extraction). The next two items are the numbers of the first and last stress
periods over which this cell injection rate applies.

In operation, ADDWELL simply
transcribes the extraction data listed in the existing well package MODFLOW
input or supplementary file to a new well package MODFLOW input or
supplementary file. However for any period during which an additional well is
operating, pertinent data from the well addition file are added to the well
package input or supplementary file generated by ADDWELL. In the latter file
the header for each stress period (listing the number of wells for which data
is provided in that stress period), is incremented by the number of added
wells. Be careful that this value does
not exceed MXWELL, the MODFLOW variable (found on the first line of a MODFLOW
well package input file [and not a well package supplementary file]),
denoting the maximum number of wells operative at any one time during the
simulation. If translating a MODFLOW input file, it is also important to
check the output file generated by ADDWELL to ensure that well rates are represented
with proper precision. (If there are any problems, the output formatting used
by ADDWELL may need to be adjusted and ADDWELL re-compiled.) In contrast, when
creating a MODFLOW supplementary file, well injection/extraction rates are
represented with full precision as MODFLOW’s strict formatting requirements
need not be met.

ADJFHB reads in a MODFLOW Flow
and Head Boundary (FHB) file and also reads in a list of scalars. The FHB
fluxes (variable FLWRAT) are multiplied by the scalars and a new FHB file is
written containing adjusted FLWRAT variables. 

ADJRIV reads a template file for
a MODFLOW river file and adjusts Rbot and Stage in dry reservoir cells such
that Stage and Rbot are equal, preventing seepage from the river cell but
allowing seepage from the aquifer into the river cell. The program requires a
fixed format for the river file with the columns for layer, row, and column 10
spaces each, stage 16 spaces, conductance 19 spaces (contains PEST identifier),
river bottom 17 spaces. The user enters the parameter ID the user wants checked
and the riverbed thickness. AdjRiv assumes 343 stress periods.

ADJTRIB read in a MODFLOW well
file and a tributary basin inflow adjustment file. The well file must be in
standard MODFLOW well file format and the inflow adjustment file must consist
of layer, row, column and a scalar. ADJTRIB reads the tributary basin inflow
adjustment file into memory and then works its way through the well file line
by line. For each well in the well file ADJTRIB compares the layer, row, and
column with the layer, row, and column in the adjustment file. When AJFTRIB
finds a match, it multiplies the pumping rate in the well file by the scalar
from the adjustment file. ADJTRIB writes a new well file consisting of adjusted
and not adjusted (if any) wells. “# Trib” is appended to any line in which the
pumping rate was adjusted.

ADJWELLAY reads a MODFLOW             well file and an integer array and
adjust the layer the well file pumps from within a specified zone identified in
the integer array. The user is prompted to identify the layer number to be
changed and the zone number within which the user does not want pumping wells.
9999 is appended to the end of the each line for which the well layer is
adjusted.

ADJWELL reads in a MODFLOW well
file and a well adjustment file. The well file must be in standard MODFLOW well
file format and the adjustment file consists of layer, row, column, and scalar.
ADJWELL reads the well adjustment file into memory and then works its way
through the well file line by line. For each well in the well file ADJWELL
compares the layer, row, and column with the layer, row, and column in the
adjustment file. When ADJWELL finds a match, it multiplies the pumping rate in
the well file by the scalar from the well adjustment file. ADJWELL writes a new
well file consisting of adjusted and not adjusted (if any) wells. “# Adjusted”
is appended to any line in which the pumping rate was adjusted.

AFACTOR reads in an array and
applies a user nominated factor to produce a new array modified by a constant.

ARRAYCOMP
reads in a PEST stylie grid
specification file and two 2D arrays and compares the number of columns and
rows in the with the grid specification file. If the number of columns and rows
match, the ArrayComp subtracts
the two arrays, takes the absolute value of the difference and sums the
difference.

 

ARRAY2GRD reads in PEST
compatible 2D arrays and outputs ArcMap compatible files.  The program requires a PEST style grid
specification file.  In a PEST grid
specification file, the x,y coordinates are for the upper left corner of the
grid. ArcMap needs the coordinates for the lower left corner of the grid and
ARRAY2GRD makes the conversion and will optionally do unit conversions for the
values in the 2D array. The user can import the file generated by ARRAY2GRD
into ArcMap using the “ASCII to Raster” utility in the ArcMap Conversion Tools
toolbox.

ARRAYAV reads in MODFLOW arrays
and averages a specified number of them. 
The program can optionally read and write binary arrays readable by MODFLOW.

AVSMP averages data in a site
sample file.  The user supplies a date
and AVSMP collects observations up to that date and computes an average.  Thus if the date for the averaging window
were 03/01/2000 then AVSMP would collect observations up until that date and
average them and save the data in a new site sample file and begin collecting
observations again.

AvMoSmp reads in a PEST site sample file, computes monthly
averages, and outputs a new stie sample file consisting of monthly averages.

BUDDIFF calculates differences
between the values in two site sample files (format: name, date, time,
value).  The site sample files must have
the same time series.  Useful for
differencing two different model runs to evaluate the difference between
various management options.

CELLINT reads in a tabular file consisting of MODFLOW layer, row,
column, and value (either 4 column data {well file} or 6 column data {river or
drain} and outputs a file consisting of layer, row, column, cellid, and value
(1 value for well files or 3 values for river or drain files).  Cellid is an integer consisting of layer in
the millions, row in the thousands, and column in the hundreds.  This is useful when plotting results using
GIS.

CHKLST reads a MODFLOW list file looking to see if MODFLOW
converged.  If MODFLOW converged CHKLST
prepares an output file, if MODFLOW failed to converge, CHKLST does not create
the output file.  This utility is useful
when working with problematic models and automated conversion tools.  A batch file can be programmed to look for
the output file, and if it can’t find the output file, run the model again
using different settings to try and achieve convergence.

CHKTRANRIV The user nominates a MODFLOW transient river file and a
river cell and CHKTRANRIV reads through the river file and prints the stage for
every occurrence of the river file to a user nominated output file.

CHECKRIVFILE reads an integer file that identifies river reaches
and MODFLOW river file and checks for places where river stage is not higher than
the river bottom. It counts the number of occurrences of the river stage not
being higher than the river bottom and prints the stress period and number
of  “dry” river cells and changes the
stage to -9999.99 in reach one (1) and -999.99 in reach two (2).

COMBWELL reads in a MODFLOW well file and outputs a well file,
often shorter, with all wells assigned to the same cell combined. As it is
currently written, it can accommodate up to a three layer model. When COMBWELL
reads in a well file, it adds each well to a 2D array, and at the end of well
term for each stress period, it outputs a well for each non-zero cell in each
array.  

COMPBA6
reads the ibound arrays in two different MODFLOW basic files and compares them.
The comparison is done by subtracting the two arrays and summing the absolute
value of the difference. This simple analysis allows the user to quickly determine
if the ibound arrays have changed between model updates.

CORRELATE1 reads in two site
sample files and computes the correlation coefficient, the probability that the
null hypothesis is true (probability that there is no correlation) and
Fissher’z Z statistic.

CORSMP read a file containing
data logger, or transducer, data and a file containing manual measurements and
linearly adjusts the transducer data to match the manual measurements. Both the
logger data and the manual measurements must be in PEST site sample format
(SiteName date(mm/dd/yyyy), time(hh:mm:ss), and value.

COUNTARR reads in an integer
array and counts the number of positive nonzero integers.

COUNTOBS counts the number of
observations per site in a PEST Site Sample File. 

DAYMAX reads a standard PEST
Site Sample File and selects the maximum daily value and saves the value to
save to an output file in Site Sample Format.

DAYMIN reads a standard PEST
Site Sample File and selects the minimum daily value and saves the value to
save to an output file in Site Sample Format.

DRISCOLLT takes pumping test
data available on driller logs and calculates transmissivity using a formula
found on pg 1021 in Groundwater and Wells by F Driscoll (1986), published by
Johnson Division, Minneapolis, MN.

DRNRIVS reads in drain and river
flux data from MODFLOW96 or MODFLOW2000 list files.  This data is then sorted into river reaches
identified in an integer array file.  The
reach gain/loss data is then output to a text file.  The program will work with or without drains.

DXFCONV was modified from a John
Doherty program DXFM2F.  It reads in a
dxf file and converts the coordinate system from feet to meters or from meters
to feet.

FHBAVERAGE reads in a MODFLOW
Flow and Head Boundary File (FHB1) and averages the parameters (flow or head)
to produce a new file with averaged parameters in the first period.  FHBAVERAGE asks if you want to skip some
stress periods before averaging and how many periods to average.

FILTSMP reads in a site sample
file and outputs an abbreviated site sample file.  Many filtering techniques are possible.  You can retain only samples collected on a
specified day of the month.  For example
only observations collected on the 1st of the month or only
observations collected on the 15th of the month.  You can retain only the first observation
collected per month.  You can retain only
the observations collected during a given window of the year.  You can average samples collected at each
sample location.  Site sample files must
be formatted in 20 character columns or less.

FLOW2CELLINT reads in a MODFLOW
generated stream flow file, searches for the stress period of interest, and
prepares a GIS compatible file.  It
computes a CELLID integer consisting of layer number in the millions, row
number in the thousands, and column number in the hundreds.  The resulting file can be joined in GIS to a
model grid shape file and used to map stream flow, leakage, steam, depth, etc.

FMTPPT reads in a PEST pilot point file and outputs an ASCII text
file readable by ArcMap consisting of easting, northing, value, and name.

FMTTRANS reads in a file written by John Doherty’s TRANSRES
program and formats the data so ESRI’s ArcView can read it.

GAI2SMP
reads in a *.GAI file (output from IDWR reach gain program) and outputs a PEST
site sample file. The program assumes that the *.gai file is in the format: reach
no, year, Oct, Nov, Dec, Jan, Feb, Mar, Apr, May, Jun, Jul, Aug, Sep for 14
numeric columns.

 

HDSAVG reads in a PEST bore data
file consisting of:  name, time,
observation, weight, and observation name. 
HDSAVG then computes the average head for each well in the file.  The date is changed to 1 and the weight is changed
to the number of observations.  This
program can be used to develop a steady state data file.

LINREG computes the linear regression equation ‘y = m (x) + b’ and
computes the R squared statistic and the standard error for the y intercept
(b).  The computed values are printed to
the screen.  It requires an ASCII table
of values consisting of space delimited x and y data.

MKCSV reads in a PEST site
sample file and outputs a csv file. At this point in time the columns are not
concatenated, the comma is inserted before the next column.

MKDRNTPLLAY makes a PEST
template file from a MODFLOW drain file and an integer array that identifies
the drain reaches for drain conductance parameters. After reading in the
integer array and MODFLOW drain file, MKDRNTPLLAY reads an ASCII text file
called a ReachID file, that identifies reach number (from the integer array)
and PEST parameter name. The first line in the ReachID file contains the number
of stress periods in the model, the second line identifies the number of
reaches, and the third contains the number of layers and zones. Subsequent
lines contain 3 items, 1) integer number for the layer number, 2) the drain
reach zone (corresponding to the integer used to identify the reach in the
integer array), and 3) parameter name (corresponding the parameter name used in
the PEST Control file). 

 

Example of reach id file:

193  # num stress periods

4    # num of reaches

3 
2 # 3 layers, 2 zones

1 
1 Drn_SiCr_L1 # layer 1, zone 1, ParamNam

1  2 Drn_StCr_L1 #
layer 1, zone 2, ParamNam

 

MAKRSPBCF2 reads in a MODFLOW96
bcf file and a pilot points real array (with row and column header) and
prepares and MODRSP bcf file.  The
program asks for an aquifer thickness. 
If the aquifer is confined, enter 1.

MEANDIFF reads in data from two
time series and computes the sample means. 
It then calculates the probability that the two data sets have the same
mean (the change in procedure introduces no bias).

MKMF2KOCL makes a MODFLOW-2000 output control file.  The unit numbers for the head save file,
budget save file, and drawdown save file must be specified in the name
file.  The budget unit number is also
specified in the various modules, such as the river and drain module.  Saving head, drawdown, and cell-by-cell
budget information to binary files is optional. 
Printing the volumetric budget info in the list file is also optional.

MKMF2KRCH makes a MODFLOW-2000 recharge file that reads a binary
recharge array in unit 61.  The binary
recharge array must be mentioned in the MODFLOW-2000 name file.  The recharge option refers to the MODFLOW
recharge options, 1) recharge is only to the top grid; 2) vertical distribution
of recharge is specified in layer variable IRCH; 3) recharge is applied to the
highest active cell.

MKREGUL reads in a parameter file,
one parameter per line, 9 character parameter name, no spaces.  The program then outputs a file where each
parameter is set equal to each other. 
This output file is in the format required by PEST
in regularization mode.  Each line is in
the format of:  priorlbl
1.0 * log(PAR1) - 1.0 * log(PAR2) = 0.0 
1.0  regul 
Where priorlbl is the regularization group and PAR1 and PAR2 are
parameters from the parameter file.

MKRIV makes a MODFLOW river file
based on the contents of "ptingrid" output file.  Ptingrid is used along with an easting,
northing, and elevation file to determine which cells are in which grids.  One takes a digitized xyz file of a river and
runs that through John Doherty’s ptingrid program using the elevations as well
names in a well coordinates file and a well listing file and the ibound array
for the intended model.  The ptingrid
output is used in MKRIV to make a MODFLOW river file.  The river elevations are placed in the well
id column. The elevation of the base of the riverbed sediments is in the 2nd
column, the third and forth columns are model row and column.  The last column is the model layer
identifier.  The first line of column
labels must be deleted before being used with MKRIV to make a MODFLOW river
file.  

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

193   “Number of stress periods in the MODFLOW
river file”

21     “Number of reaches identified in integer
array”

1    WR_nKe_Hul  
“Integer for reach and riverbed conductance parameter name”

2    WR_Hul_Ket    “Integer for reach and riverbed conductance
parameter name”

3    WR_Ket_Gim    “Integer for reach and riverbed conductance
parameter name”

 

RIVELEV reads in a MODFLOW river
file and checks for anomalously high river a. If it finds a stage higher than
its neighbors, it sets the stage and rbot equal to the highest neighbor stage
and rbot. RIVELEV will work with both transient and steady state river files.

MKRSPRIV reads in an integer array and MODFLOW river and
(optional) drain files and makes a response function river file.  The integer array is to be used to filter out
river reaches that are not hydraulically connected to the aquifer.  If the integer array value is greater than 0
then the river data for that cell will not be written to the response function
river file

MKSFRTPL makes a PEST template file from a MODFLOW Streamflow Routing file
and an integer array that points to river reaches for riverbed
conductance.  In its current state,
MKSFRTPL assumes data parts 3 and 4 in the Streamflow Routing file are not
present.  As best I can tell, these are
only used when MODFLOW2000 is operated in parameter estimation mode.  If you are using PEST,
you probably won’t be using MODFLOW2000 in parameter estimation mode.

MKWEL reads in a list of cells
(layer, row, col, pumping rate) and makes a transient or steady state well
file.  Useful along with VOLCALC.EXE and
RFCALC.EXE for computing transient response functions.

MKWELMFUSG reads in a
MODFLOW-2000 well file and formats it for use in MODFLOW-USG. MODFLOW-USG
requires the number of adjustable parameter (NP) for each stress period in the
well file, MODFLOW-2000 does not.

MODARRY reads in an array and modifies the array by capping the
minimum or maximum value in the array.

MODWELAV.  This program reads in a MODFLOW well file and
averages the pumping rates to produce a new well file with averaged pumping
rates for longer stress periods.  The
program asks for a multiplier.  If you do
not intend to do any units conversion, set the multiplier to 1.  

MODWELTRM.  Reads in a MODFLOW well file and trims out
unwanted stress periods.

OBSSENS  This program reads in a PEST
case.seo file and outputs an ArcView
compatible data file format containing well name, average sensitivity,
variance, standard deviation, skew, kurtosis, and number of occurrences.  This file can be joined in ArcGIS with any
shape file that shares a common naming convention.  For example, OBSSENS output could be joined
to a well shape file to display average sensitivity of all wells used in the
inversion.

PDTRIB calculates the impact of
curtailing groundwater rights in basins tributary to the ESPA. Two input files
are required. A POD file listing number of PODs in the file on line 1, the
water right number, priority date, irrigation cfs, basin number (does not need
to be a formal basin number), and EXCEL serial date for the priority date (days
since 1/0/1900 to the priority date is an EXCEL serial date) on line 2 through
the number of PODs. The following Figure illustrates the format.



 

The second file is the
consumptive use file. The first line contains the number of basins represented
in the file, the second line contains basin number (must correspond to the
basin number in the POD file), basin name, groundwater irrigated acres in the
basin, and consumptive use per acre per year for that basin. 

The basins need to be numbered
sequentially beginning with number 1. The following Figure shows the proper
format. 



PDTRIB asks for a POD File, a
Consumptive use file, and a priority date, it then calculates the benefit of
curtailing to the nominated priority date based on the acres and consumptive
use contained in the Consumptive use file and the priority dates and permitted
irrigation cfs in the POD File.

PairUp reads in two user nominated site sample files.  The first file should contain the most
complete sample time series.  The program
trims lines from the first file that do not have corresponding month and year
entries in the second file.

PredWell reads in a MODFLOW well file and a list of prediction
well locations. The header for the prediction well file contains two numbers,
the number of predictions and the number of wells used in the predictions. The
subsequent lines are typical MODFLOW well file, layer, row, column, and pumping
rate. PredWell reads the first stress period in the existing MODFLOW well file
and appends the predictions wells to produce a new MODFLOW well file.

PREDWELTR is much like PREDWEL, it reads in a MODFLOW well file
and an external file. PREDWELTR then makes a transient well file containing an
extra well during user selected stress periods. The external file contains two
lines. The first line contains the number of stress periods, the stress period
to start adding the extra well, and the last stress period that will include
the added well. The second line contains the layer, row, column, pumping rate
(- is extraction) and id. The following lines are an example of a PREDWELTR
external file.

24  2  13   ←
number of stress periods, starting stress period, ending stress period

3 458  119  30000 10 ← layer, row, column, pumping rate,
id for added well

RATIO calculates the ratios of the means in an output file from
SMPSTAT.EXE the user nominates one of the rows as the base and ratio calculates
the ratio of all the other entries using the nominated entry.

RATIO2 is similar to RATIO except that it calculates the ratios
based on the total gains for the larger reach. 

READLST reads in a MODFOW list
file and extracts data for a nominated category from the volumetric water
budget.  The user can request that the
data be extracted from the cumulative volumetric table or the rate per time
step table.  The information is extracted
for each stress period unless the user instructs MODFLOW to print out the water
budget more frequently.  Inflows are
subtracted from outflows.  This information
is stored in an ASCII file that can be loaded into a spreadsheet.  

READRMR reads in output from a PEST run management record file and computes model run
times for all model runs in iteration. 
As it sits now, you must first replace all spaces ‘: ‘in the time
columns with zeros ‘:0’

REAL2GRD reads in a real array
and builds an ARCview compatible ASCII grid file (ASCII raster).  REAL2GRD expects the input array to contain a
header consisting of ncol nrow; it also requires a PEST
grid specification file.

REAL2INT reads in a real array
and outputs an integer array

RESPMAX reads in a steady state
response function file and produces a new file containing 0 for all reaches
except the reach with the maximum response. 
In its current form, it is hardwired to 13 columns of data.  1-cellid, 2-resp reach 1, 3-resp reach2,
4-resp reach 3, etc 13-total of response functions.

RFCALC modified program from
DCM, opens BUD2SMP output and the *.wel file (or a well addition file [see
ADDWELL]), extracts the pumping rate and the reach gain/loss, normalizes the
gain/loss and writes the output to the steady state response function
file.  Modified to make RFCALC more
flexible by making the real arrays allocatable and having the program ask the
user for file names, the number of stress periods and the number of river reaches.

 

RIVDRYBED reads an integer array identifying the dry river reach.
The user then identifies a MODFLOW river file that needs updating and ASCII
file identifying the total number of stress periods and the stress periods for
which the river stage is dry. The river stage will be set equal to the river
bottom for these stress periods. RIVDRYBED then loops through the MODFLOW river
file and makes the river stage equal to the river bottom in the nominated river
reach during the nominated stress periods.

RIVELEV reads a MODFLOW river file looking for river cells
anomalously higher than its neighbors. It then lowers the elevation of the
offending cell to the elevation of its highest neighbor. There is an option to
enter a tolerance, if the nominated tolerance is 0, then cell i,j can not be
any higher than any of it’s neighbors. 

RIVSTAGE makes a transient MODFLOW river file from a steady state
river file, an integer array, and a reach list file. The reach list file
consists of a row of text identifying the numerical variables in the second row
(number of stress period, number of reaches, reach name, integer id for the
reach, row where the upriver gage resides, and row where the downriver gage
resides. The program interpolates between upriver change in stage and downriver
change in stage. The below figure shows a typical Reach List File



 

RSP2SRF reads in a MODRSP response function file and outputs
SURFER compatible grid files that facilitate visualization.  RSP2SRF needs to read MODRSP river and well
files and an integer array file that identifies river reaches (1-8) that you
want to view response functions for.  The
user is able to make response functions for up to 8 river reaches.  MODRSP needs to be modified to not print the
header for the response function file or you need to delete it.

SCI2NUM read in output from John Doherty’s REAL2TAB program
consisting of 3 columns of numbers (row, column, value) were value can
frequently be expressed in scientific notation. SCI2NUM read in the 3 columns
and outputs 3 columns consisting of numbers.

SFR2CELLINT reads in a MODFLOW Stream Flow Routing file and
outputs a file consisting of layer, row, column, cellid, riverbed hydraulic
conductivity, bed thickness, bed elevation, width, and depth.  Cellid is an integer consisting of layer in
the millions, row in the thousands, and column in the hundreds.  This is useful when plotting results using
GIS.

SFRAVERAGE reads in a MODFLOW Stream Flow Routing File
(SFR1) and averages the flow parameters (thick, elev, width, depth) to produce
a new abbreviated file with averaged flow parameters.  SFRAVERAGE asks if you want to skip some
stress periods before averaging and how many periods to average.

SFRSELECT reads in a MODFLOW Stream Flow Routing output file
(SFR1) and selects flow parameters from nominated stream segments.

SFRWIDHT adjusts stream widths in
the MODFLOW Streamflow-Routing (SFR1) Package. 
SFRWIDTH reads a SFR1 file and a river cell file.  The river cell file identifies cells requiring
width adjustment (identified using the SFR1 nseg variable), river depth standard
in the cell, and an adjustment factor. 
The river cell file must be space delimited but does not need to be
formatted.  SFRWIDTH then identifies the
targeted cells, calculates a 'depth ratio' (time step depth/standard river
depth), and then multiplies the depth ratio by the adjustment factor to yield a
width adjustment factor.  The current
river width is then multiplied by the width adjustment factor to yield a new
river width. 

 

SMPMOAV reads a PEST Site Sample File (PEST Bore Sample File) and
outputs a PEST Site Sample File containing monthly average. The average monthly
value is assigned an average monthly date and a midnight time stamp. At this
point SMPMOAV is limited to no more than 125 observations per month which
translates to 4 observations per day for most months (4*31=124).

 

SMPMOMID reads a PEST Site Sample File (PEST Bore Sample File) and
outputs a PEST Site Sample File containing the Xth monthly percentile. The
monthly percentile retains its date and time stamp. 

Assuming one desires the monthly median, or 50th
percentile for an array of field observations, then enter 0.5 when asked for
the desired percentile. The median value is most properly defined as the middle
value. Thus, if the string consists of an even number of elements, the median
is the average of the two middle values. SMPMOMID does not strictly follow this
pedantic definition of the median, in order to retain the sampling date and
time it finds the value at location INT(n*frac) where “n” is the number of
elements in the array and “frac” is the desired percentile (0.5 = 50% or
median, 0.25 = 25%, ect). If “n*frac” is not an integer SMPMOMID will round up
if the decimal value is greater or equal to 0.5. Thus, assuming 31 values are
in the array (31*0.5 = 15.5) then SMPMOMID will find the 16th
smallest value in the array. 

SMPSTAT calculates statistics for samples collected at the various
sample locations included in a site sample file.  The statistics recorded are sample location
name, mean, variance, standard deviation, and number of observations.  SMPSTAT can also calculate annual statistics
for each sample location in a site sample file.

SMPSTAT2 similar to SMPSTAT except that it also calculates skew
and kurtosis.  It does not calculate
annual statistics, it only records statistics based on sample location.

SMPSUM sums two PEST site sample files. The user enters the names
of the two site sample files to be added together and then enters the name of
the output file. At this point the program is pretty rough, the only error
checking is that it counts the lines in both site sample files to make sure
that they have the same number of lines.

SORTWELLLOG Used to sort data from Steve Bakers Well Log data base
that he posted on the ESHMC WEB page. 
You must first replace ',,'  with ',_,' and
'Nearby, Pumping' with 'Nearby,Pumping', 
'Foreign Substance' with 'ForeignSubstance',  'Recently Pumped' with 'RecentlyPumped', 'Recently
Flowing' with 'RecentlyFlowing', 'Nearby, Flowing' with 'Nearby,Flowing'.  This is still not bullet proof because occasionally
the measuring agency is left blank.

SS2TRNWEL reads in a MODFLOW steady state well file and produces a
transient well file.  SSTRANWEL assumes
you are pumping for one stress period and not pumping for rest of the model
run.

STRIPDRY reads a PEST site sample file and removes the
‘dry_or_inactive’ designation from the elevation column and replaces it with
0.00.

STRIPMOD2OBS is much like
STRIPRSD except it works with John Doherty’s MOD2OBS rather than MODBORE, and
as such does not need a well list file. 
It will do unit conversions on the coordinates if necessary.  It will also strip out underscores places in
the steady state head observations by PESTPREP.

STRIPRSD reads in a well
coordinates file, a well listing file, and a PEST
residuals file.  It then asks for the
name of up to 4 PEST head observation
groups.  The program will then output an
ArcMap/SURFER compatible data file consisting of easting, northing,
observation, modeled value, residual, PEST
well name, and observation group for all observations weighted greater than
zero.  It will optionally convert the X
and Y coordinates from feet to meters or meters to feet.

SUMARRAY reads in a PEST/PMWIN
compatible array and sums the array. 
Useful for comparing differences in model output.  Use twoarray to difference the arrays and
then sum the array representing the difference.

SumReturns reads in a *.rfl file
(output from MkMod5.pl) and sums selected entity returns.  Currently entity selection is hard wired. The
output file is in pseudo site sample format (‘entity name’ ‘stress period
number’ ‘mm/yyyy’ ‘rate’).

SvRpBud
reads in a cell listing file consisting of cell id and total, and a cell budget
file consisting of a cell id and a rate for that time step.  SvRpBud compares the cell ids and if the ids
equal, it converts the rate to a volume and adds the result to the total,
creating a new list file

 

VARDIFF based on numerical
recipes in FORTRAN pg 613.  Given the
arrays z and zz, this routine returns the value of F and its significance as
prob.  Small values of prob indicate that
the two arrays have significantly different variances.  Prob represents the probability of obtaining
the observed sample variances if the underlying population variances were in
fact identical.

VOLCALC reads in a site sample
file containing flow rates and generates another site sample file consisting of
flow volumes.  VOLCALC calculates flow
volumes using the time increment between recorded time steps.  It linearly interpolates between the rate
from the previous time step and the rate at the end of the current time
step.  Thus the recorded volume for the
first sample in a file will always be between 0 and the rate at the end of the
time step.

WELCHEK reads a MODFLOW well file and checks for duplicate wells. 
If it finds a duplicate well it will sum the two and remove the duplicate entry
from the file.

WELLCOMP compares a MODFLOW well file with an ibound array and retains
only those wells within the active portion of the grid.

WTDRESID This program reads in a PEST case.res file and outputs an ArcView compatible data file format
containing well name, average sensitivity, variance, standard deviation, skew,
kurtosis, and number of occurrences. 
This file can be joined in ArcGIS with any shape file that shares a
common naming convention.  For example,
OBSSENS output could be joined to a well shape file to display average
sensitivity of all wells used in the inversion.

WTSERIES read in a PEST sample
file, requests the date corresponding to the end of the warm-up (or spin-up)
period, request weights to assign to the warm-up and calibration periods. The output
file is in PEST sample file format with an added column of weights. If the
input file was used in preparing for a PEST calibration run or produced as a
result of a PEST run, the weights can be copied and pasted into a PEST control
file to assign different weights to the observations collected during the
warm-up period than the observations collected during the calibration period.

ZSTAT2ARRAY reads an ASCII version of a zonal statistics
calculation produced by ArcMap and a grid specification file and produces an
array.  The zonal statistics table
consists of 10 columns, the first of which consists of a cell id.  This cell id consists of 8 characters, the
first two are letters, 3 through 5 are row number, and 6 through 8 are column
number.  The eighth column in the table
consists of mean value calculated for the cell. 
Zstat2array parses the cell id out into row and column numbers and loads
the mean value into an array.


