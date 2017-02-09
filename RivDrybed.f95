program RivDrybed
    !   program RIVDRYBED reads an integer array identifying the dry river reach. The user then 
    !   identifies a MODFLOW river file that needs updating and an ASCII file identifying the 
    !   stress period which need adjusting. RIVDRYBED then loops through the MODFLOW river file and
    !   makes the river stage equal to the river bottom in the reach identified in the integer
    !   array during the nominated stress periods

    !   by Allan Wylie Nov 2014

    !   by Allan Wylie Aug 2015
    !   adjusted to account for comment lines identified by a "#" in row 1 at the beginning of the river file.

    implicit none
    integer mxrivr,itmp,irivcb,np,i,j,nstress,ncol,nrow,ilay,irow,icol,ireach,drystress,rid
    real stage,cond,rbot
    integer, allocatable:: iarray(:,:)
    character*20 array,rivfile,strsper,outfile
    character*80 cline
    character*1 lb

    ! open the integer array identifies the seasonally dry river reach
100 write(6,105)
105 format(' Enter the name of the integer array: ',$)
    read(5,'(a)')array
    open(unit=10,file=array,status='old',err=100)

    !   read the integer array
    read(10,*)ncol,nrow
    allocate(iarray(ncol,nrow))
    do irow=1,nrow
        read(10,*,err=990,end=990) (iarray(icol,irow),icol=1,ncol)
    end do
    close(unit=10)

    ! open the MODFLOW river file
110 write(6,115)
115 format(' Enter name of the MODFLOW river file: ',$)
    read(5,'(a)')rivfile
    open(unit=10,file=rivfile,status='old',err=110)

    !   read file identifying total number of stress periods and the stress periods in which the drybed should be dry
130 write(6,135)
135 format(' The file identifying the stress periods when the drybed is dry: ',$)
    read(5,'(a)')strsper
    open(unit=15,file=strsper,status='old',err=130)

    !   now read the number of stress periods in the river file and the first stress period in which the drybed is dry
    read(15,*)nstress
    read(15,*)drystress

    !   open the output MODFLOW river file
140 write(6,145)
145 format(' Enter the name for the new river file: ',$)
    read(5,'(a)')outfile
    open(unit=20,file=outfile,status='unknown',err=140)
	write(6,'( )')
    write(6,'( )')

    !   these next lines allow comment lines at the beginning of the river file to be passed to the template file
146 read(10,'(a)')lb
    if(lb.eq.'#')then
        backspace(10)
        read(10,'(a)')cline
        write(20,'(a)')cline
        go to 146
    else
        backspace(10)
        !   just reading mxriver and irivcb to make usre we know where we are, if not we crash.
        read(10,*,err=997)mxrivr,irivcb
        backspace(10)
    end if
    !   end of lines used to pass initial comments to the template file

    write(20,147)
147 format('# adjusted for periods when the Drybed is known to be dry ')
    read(10,'(a)')cline
    write(20,'(a)')cline
    read(10,*)itmp,np
    backspace(10)
    read(10,'(a)')cline
    write(20,'(a)')cline

    !   compare the river cells to the integer array and adjust stage in target reach
150 do i=1,nstress
        do j=1,itmp
            !   if drybed is dry in this stress period and the river cell is in the drybed reach set stage=rbot
            if(i.eq.drystress)then
                read(10,*,err=997,end=999)ilay,irow,icol,stage,cond,rbot,rid
                ireach=iarray(icol,irow)
                if(ireach.gt.1)then
                    write(20,200)ilay,irow,icol,rbot,cond,rbot,rid
200                 format(3i10,3f20.8,i5)
                else
                    write(20,210)ilay,irow,icol,stage,cond,rbot,rid
210                 format(3i10,3f20.8,i5)
                end if
            !   if drybed in not dry in this stress period do not change river stage and rbot
            else
                read(10,*,err=997,end=999)ilay,irow,icol,stage,cond,rbot,rid
                write(20,220)ilay,irow,icol,stage,cond,rbot,rid
220             format(3i10,3f20.8,i5)
            end if
        end do
        !   read the number of river cells in the next stress period and right the data to the new
        !   river file
        read(10,*,end=900)itmp,np
        backspace(10)
        read(10,'(a)')cline
        write(20,'(a)')cline
        !   if the last stress period was a stress period in which the drybed was dry, read the
        !   next stress period in which the drybed is dry.
        if(i.eq.drystress)then
            read(15,*,err=995,end=150)drystress
        end if
    end do
    
    !   print message saying everything went ok
900 write(6,905) trim(outfile)
905 format(' ** file ',a,' written ok. **')
    pause
    go to 999

    ! a little error trapping
990 write(6,991)trim(array)
991 format('--Problem encountered in ',a,'.--')
    pause
    go to 999

995 write(6,996)trim(strsper)
996 format('--Problem encounter in ',a,'.--')
    pause
    go to 999

997 write(6,998)trim(rivfile)
998 format('--Problem encounterd in ',a,'.--')
    pause
    go to 999

    ! goodby
999 close(unit=10)
    close(unit=15)
    close(unit=20)
    deallocate(iarray)

    end program RivDrybed
