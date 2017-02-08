program MkRivTpl

!   program MKRIVTPL reads in a MODFLOW river file and an integer array identifying
!   river reaches and prepares a PEST template file.

!   by Allan Wylie Nov 2014

!   modified by Allan Wylie January 2015 to read in a Reach ID file rather than have the 
!   user type in the reaches every time the program is used.

!   modified by Allan Wylie May 2015 to include id parameters used during post processing

    implicit none
    integer itmp,np,irow,icol,i,k,rlay,rrow,rcol,rid,nrow,ncol,nrch,ireach,nstress
    real cond,rstage,rcond,rbot
    integer, allocatable:: iarray(:,:)
    character*20 afile,outfile,rechid,parfile
    character*1 lb
    character*100 cline
    character*20, allocatable:: rchid(:)

!   open the integer array that identifyes the river reaches
100 write(6,110)
110 format(' Enter name of the integer array identifying the reaches: ',$)
    read(5,'(a)') afile
    open(unit=10,file=afile,status='old',err=100)

!   read the integer array
    read(10,*)ncol,nrow
    allocate(iarray(ncol,nrow))
    do irow=1,nrow
        read(10,*,err=997,end=997) (iarray(icol,irow),icol=1,ncol)
    end do
    close(unit=10)

!   open the MODFLOW river file
140 write(6,150)
150 format(' Enter name of the MODFLOW river file: ',$)
    read(5,'(a)')afile
    open(unit=10,file=afile,status='old',err=140)
    
!   open the output file
160 write(6,170)
170 format(' Enter the name of the new PEST template file: ',$)
    read(5,'(a)')outfile
    open(unit=20,file=outfile,status='unknown',err=160)

!   write the headder of the new template file
    write(20,'(a)')'ptf $'
!   these next lines allow comment lines at the beginning of the river file to be passed to the template file
174 read(10,'(a)')lb
    if(lb.eq.'#')then
        backspace(10)
        read(10,'(a)')cline
        write(20,'(a)')cline
        go to 174
    else
        backspace(10)
        read(10,*,err=997)itmp,np
    end if
!   end of lines used to pass initial comments to the template file
176 backspace(10)
    read(10,'(a)')cline
    continue
    write(20,'(a)')cline

!   read the file containing the reach ids
180 write(6,185)
185 format(' Enter the name for the file containing the reach ids: ',$)
    read(5,'(a)')parfile
    open(unit=30,file=parfile,status='old',err=180)

!   read the number of stress period in the model
    read(30,*)nstress
!	read the reach ids
    read(30,*)nrch
    allocate(rchid(nrch))
	do i=1,nrch
        read(30,190,end=992)k,rechid
190     format(i5,a15)
        if(rechid.eq.' ') go to 992
        rchid(k)=rechid
    end do
    close(unit=30)

!   compare the river cells to the integer array and assign the river cells to reaches
    do k=1,nstress
        read(10,*,err=997)itmp,np
        backspace(10)
        read(10,'(a)')cline
        continue
        write(20,'(a)')cline
        do i=1,itmp
            read(10,*,err=997,end=999)rlay,rrow,rcol,rstage,rcond,rbot,rid
            ireach=iarray(rcol,rrow)
            if(ireach.ge.1)then
                rechid=rchid(ireach)
                write(20,200)rlay,rrow,rcol,rstage,rechid,rbot,rid
200             format(3i10.0,f12.4,2x,'$'a11'$',2x,f12.4,i4)
            else
                write(20,210)rlay,rrow,rcol,rstage,rcond,rbot,rid
210             format(3i10.0,3f12.4,i4)
            end if
        end do
    end do

!   print message saying everything went ok
	write(6,'( )')
	write(6,'( )')
    write(6,900) trim(outfile)
900 format(' ** file ',a,' written ok. **')
    go to 999

! now a little error trapping
992 write(6,993)trim(parfile)
993 format('--Problem encountered in ',a,'.--')
    go to 999

997 write(6,998)trim(afile)
998 format('--Problem encounterd in ',a,'.--')
    go to 999

!   close all open files and deallocate arrays
999 close(unit=10)
    close(unit=20)
    deallocate(iarray,rchid)

    end program mkrivtpl

