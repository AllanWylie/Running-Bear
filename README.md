# Running-Bear

FORTRAN utility programs by
Allan Wylie

RIVELEV reads a MODFLOW river file looking for river cells
anomalously higher than its neighbors. It then lowers the elevation of the
offending cell to the elevation of its highest neighbor. There is an option to
enter a tolerance, if the nominated tolerance is 0, then cell i,j can not be
any higher than any of it’s neighbors. 

