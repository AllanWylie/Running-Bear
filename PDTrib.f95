program PDTrib

    !   Program PDTrib calculates the ratio of junior cfs to total cfs for an ESPA tributary basin and assumes
    !   that same ratio applies to the irrigated acres in the tributary basin. It then applies an estimated consumptive
    !   use for the basin to the curtailed acres to estimate the impact of curtailing the junior acres.

    !   by Allan Wylie May 2016

    !   dimension variables
    implicit none
    integer PodData,NBasin,i,j,ifail,ls(5),rs(5),numdays,pm,pd,pyyy,om,od,oyyy,pnume,basin,TribNo,PodDate
    real acres,IrrigationCfs,ratio,acft
    real, allocatable:: BasinAc(:,:)
    character*20 PodFile,CuFile,OutFile,PriorityDate,text
    character*15, allocatable:: BasinNam(:)
    character*100 cline

    !   open the POD file
10  write(6,11)
11  format(' Enter the name of the POD file: ',$)
    read(5,'(a)')PodFile
    open(unit=10,file=PodFile,status='old',err=10)
    write(6,15)
15  format(' ')
    !   i=1 for error trapping
    i=1
    read(10,*,err=930)PodData

    !   open the consumptive use file
40  write(6,41)
41  format(' Enter the name of the consumptive use file: ',$)
    read(5,'(a)')CuFile
    open(unit=20,file=CuFile,status='old',err=40)
    write(6,15)
    !   i=1 for error trapping
    i=1
    read(20,*,err=920)NBasin
    allocate(BasinAc(5,NBasin))
    allocate(BasinNam(NBasin))

    !   initialize the array to hold the consumptive use data
    do i=1,NBasin
        do j=1,5
            BasinAc(j,i)=0
        end do
    end do
    !   load the consumptive use data into the array
    do i=1,NBasin
        read(20,'(a)')cline
        call linesplit(ifail,4,ls,rs,cline)
        if(ifail.ne.0)go to 920
        !   the first column of the consumptive use file contains the basin number
        text=cline(ls(1):rs(1))
        !   store the basin number in column 1 of the basin acre array
        read(text,*,err=930)BasinAc(1,i)
        !   check to make sure that the basins in the CU file are numbered sequentially
        BasinNam(i)=cline(ls(2):rs(2))      
        !   the 3rd column of the consumptive use file contains the irrigated acres in the basin
        text=cline(ls(3):rs(3))
        !   store the irrigated aqcres in the 2nd column of the basin acre array
        read(text,*,err=920)BasinAc(2,i)
        !   the 4th column of the consumptive use file contain the average annual consumptive use in feet
        text=cline(ls(4):rs(4))
        !   store the consumptive use in the 3rd column of the basin acre array
        read(text,*,err=920)BasinAc(3,i)
    end do
    close(unit=20)

70  write(6,71)
71  format(' Enter the priority date (mm/dd/yyyy): ',$)
    read(5,'(a)',err=70)PriorityDate
    !   call sub getdate2 to convert PriorityDate text to mm dd yyyy
    !   pm,pd,pyyy = priority month, priority day, priority year
    call getdate2(ifail,PriorityDate,pm,pd,pyyy)
    if(ifail.ne.0)go to 70
    write(6,15)
    !   call function numdays to convert priority date to an EXCEL serial date
    !   od,om,oyyy = origin day, origin month, origin year
    !   the origin day is 1/0/1900 in mm/dd/yyyy format, see comments below.
    od=0
    om=1
    oyyy=1900
    pnume=numdays(od,om,oyyy,pd,pm,pyyy)
    !   A propagation of errors diatribe:
    !   EXCEL serial dates start 1/0/1900 (not a valid date) and assume that 1900 was a leap year (it wasn't), 
    !   EXCEL chose this date scheme in 1987 to be compatible with Lotus 123. So we start with non date of 
    !   od=0,om=1,oyyy=1900 see above.
    !
    !   Now we add 1 to the pnume variable if the date is more recent than 28feb1900 to account for the fact that 
    !   1900 is not a leap year (Jan 1900 had 31 days and Feb 1900 had 28 days, 31+28 = 59).
    if(pnume.gt.59)then
        pnume=pnume+1
    end if
    !   So we start at the non date of 1/0/1900 and add 1 for all dates after 2/28/1900 to to account for the non 
    !   leap year (1900), to be compatible with EXCEL, who did it to be compatible with Lotus 123, who made the 
    !   original bug.
    !   End of tirade.

    !   open the output file and print the headder
80  write(6,81)
81  format(' Enter a name for the output file: ',$)
    read(5,'(a)')OutFile
    open(unit=30,status='unknown',file=OutFile,err=80)
    write(6,15)
    !   print the priority date and the EXCEL serial date to the output file
    write(30,82)trim(PriorityDate),pnume
82  format('Priority date=',a,', the corresponding serial date in EXCEL is 'i6'. ')
    !   print the output file headder
    write(30,85)
85  format(1x,'Num',2x,'Name',5x,'Total_Ac',1x,'Cons_Use',1x,'Total_cfs',1x,'Jr_cfs's,2x,'Ratio',5x,'Jr_Ac',6x,'Ac_ft')
    write(6,15)

    !   read the POD file and determine which water rights are junior
    do i=1,PodData
        read(10,'(a)',end=930)cline
        call linesplit(ifail,5,ls,rs,cline)
        text=cline(ls(3):rs(3))
        read(text,*,err=930)IrrigationCfs
        text=cline(ls(4):rs(4))
        read(text,*,err=930)TribNo
        text=cline(ls(5):rs(5))
        read(text,*,err=930)PodDate
        !   cycle through the basins to locate the basin this POD is in
        do j=1,NBasin
            if(TribNo.eq.BasinAc(1,j))then
                !   found the correct basin so add the irrigation cfs to the total basin irrigation cfs
                BasinAc(4,j)=BasinAc(4,j)+IrrigationCfs
                !   if the priority date is junior then add the irrigation cfs to the total basin junior cfs
                if(pnume.lt.PodDate)then
                    BasinAc(5,j)=BasinAc(5,j)+IrrigationCfs
                end if
                continue
            end if
            continue
        end do
        continue
    end do
    close(unit=10)

    !   calculate the junior cfs ratio for each basin then calculate curtailed acres and ac-ft/yr benefit
    do i=1,NBasin
        !   don't divide by 0! If the total acres are 0 there is no groundwater irrigation to curtail
        if(BasinAc(4,i).eq.0)then
            ratio=0
            acres=0
            acft=0
        !   if the irrigated acres are greater than 0 calculate the benefit
        else
            ratio=BasinAc(5,i)/BasinAc(4,i)
            acres=ratio*BasinAc(2,i)
            acft=acres*BasinAc(3,i)
        end if
        !   write the results to the output file
        write(30,100)BasinAc(1,i),BasinNam(i),BasinAc(2,i),BasinAc(3,i),BasinAc(4,i),BasinAc(5,i),ratio,acres,acft
100     format(f4.0,1x,a11,f8.2,1x,f5.2,1x,f8.2,1x,f8.2,1x,f8.4,1x,f8.2,1x,f10.2)
    end do
    go to 990

    !   Now for a little error trapping
920 write(6,921)trim(CuFile),i
921 format(' There is something wrong with file 'a' at line 'i4'. ')
    go to 999

930 write(6,931)trim(PodFile),i
931 format(' There is something wrong with file 'a' at line 'i5'. ')
    go to 999

    ! Goodbye
990 write(6,991)trim(OutFile)
991 format(' File ',a,' written ok. ')

999 close(unit=30)
    deallocate(BasinAc,BasinNam)

     ! pause
    
    end program PDTrib

          SUBROUTINE LINESPLIT(IFAIL,NUM,LW,RW,CLINE)
 
!     Subroutine LINESPLIT splits a string into blank-delimited fragments.
!     num = number of colums in the file, lw and rw are 2d arrays identifying
!     the left and right ends of each colum

      INTEGER IFAIL,NW,NBLC,J,I
      INTEGER NUM
      INTEGER LW(NUM),RW(NUM)
      CHARACTER*(*) CLINE
 
      IFAIL=0
      NW=0
      nblc=nblank(CLINE)
      IF(NBLC.EQ.0) THEN
      IFAIL=-1
      RETURN
      END IF
      J=0
5     IF(NW.EQ.NUM) RETURN
      DO 10 I=J+1,NBLC
        IF((CLINE(I:I).NE.' ').AND.(CLINE(I:I).NE.',').AND.(ICHAR(CLINE(I:I)).NE.9)) GO TO 20
10    CONTINUE
      IFAIL=1
      RETURN
20    NW=NW+1
      LW(NW)=I
      DO 30 I=LW(NW)+1,NBLC
        IF((CLINE(I:I).EQ.' ').OR.(CLINE(I:I).EQ.',').OR.(ICHAR(CLINE(I:I)).EQ.9)) GO TO 40
30    CONTINUE
      RW(NW)=NBLC
      IF(NW.LT.NUM) IFAIL=1
        RETURN
40    RW(NW)=I-1
      J=RW(NW)
      GO TO 5
      END
 
      subroutine getdate2(ifail,adate,mm,dd,yyyy)

!     subroutine getdate2 is like getdate; but it does not destroy the adate string.
      logical leap
      integer function nblank,lencar
      integer ifail,mm,dd,yyyy,n
      character*(*) adate
      character*(15) tdate

      ifail=0
      adate=adjustl(adate)
      tdate=adate(1:min(15,len(adate)))
      tdate=adjustl(tdate)
      n=index(tdate,'/')
      if(n.eq.0) go to 9999
      if(n.eq.1) go to 9999
      if(n.eq.2)then
        read(tdate(1:1),'(i1)',err=9999) mm
      else if(n.eq.3)then
        read(tdate(1:2),'(i2)',err=9999) mm
      else
        go to 9999
      end if
      
      tdate=tdate(n+1:)
      n=index(tdate,'/')
      if(n.eq.0) go to 9999
      if(n.eq.1) go to 9999
      if(n.eq.2) then
        read(tdate(1:1),'(i1)',err=9999) dd
      else if(n.eq.3) then
        read(tdate(1:2),'(i2)',err=9999) dd
      else
        go to 9999
      end if
      
      tdate=tdate(n+1:)
      n=lencar(tdate)
      if(n.ne.4) go to 9999
        read(tdate(1:4),'(i4)',err=9999) yyyy
        if((mm.lt.1).or.(mm.gt.12)) go to 9999
        if((mm.eq.1).or.(mm.eq.3).or.(mm.eq.5).or.(mm.eq.7).or.(mm.eq.8).or.(mm.eq.10).or.(mm.eq.12))then
        if(dd.gt.31) go to 9999
        else if((mm.eq.4).or.(mm.eq.6).or.(mm.eq.9).or.(mm.eq.11))then
        if(dd.gt.30) go to 9999
        else
            if(leap(yyyy)) then
            if(dd.gt.29) go to 9999
        else
            if(dd.gt.28) go to 9999
        end if
      end if
      return

9999  ifail=1
      return
      end

      subroutine gettime(ifail,atime,hh,mm,ss)

!     subroutine gettime reads a time; note that it does not destroy the time string
      integer ifail,hh,mm,ss,n,i,lencar
      character*15 tatime
      character*(*) atime

      if(atime.eq.' ') go to 9999
      do i=1,len(atime)
        if(atime(i:i).ne.' ') go to 10
      end do
10    tatime=atime(i:)

      ifail=0
      tatime=adjustl(tatime)
      n=index(tatime,':')
      if(n.eq.0) then
        n=index(tatime,'.')
        if(n.eq.0) go to 9999
      end if
      if(n.eq.1) go to 9999
      if(n.eq.2)then
        read(tatime(1:1),'(i1)',err=9999) hh
      else if(n.eq.3)then
        read(tatime(1:2),'(i2)',err=9999) hh
      else
        go to 9999
      end if
      tatime=tatime(n+1:)
      n=index(tatime,':')
      if(n.eq.0)then
        n=index(tatime,'.')
        if(n.eq.0) go to 9999
      end if
      if(n.eq.0) go to 9999
      if(n.eq.1) go to 9999
      if(n.eq.2) then
        read(tatime(1:1),'(i1)',err=9999) mm
      else if(n.eq.3) then
        read(tatime(1:2),'(i2)',err=9999) mm
      else
        go to 9999
      end if
      tatime=tatime(n+1:)
      n=lencar(tatime)
      if(n.gt.2) go to 9999
      if(n.eq.1)then
        read(tatime(1:n),'(i1)',err=9999) ss
      else
        read(tatime(1:n),'(i2)',err=9999) ss
      end if

      if((mm.lt.0).or.(mm.gt.59)) go to 9999
      if((hh.lt.0).or.(hh.gt.23)) go to 9999
      if((ss.lt.0).or.(ss.gt.59)) go to 9999

      return

9999  ifail=1
      return
      end

      integer function numdays(DR,MR,YR,D,M,Y)

      implicit none

! --  Function numdays calculates the number of days between dates
!     D-M-Y and DR-MR-YR. If the former preceeds the latter the answer is
!     negative.

! --  Arguments are as follows:-
!     dr,mr,yr:     days, months and years of first date
!     d,m,y:        days, months and years of second date
!     numdays returns the number of elapsed days

! --  Revision history:-
!     22 July 1994:  version 1
!     13 September 1995:  modified for Groundwater Data Utilities

      integer dr,mr,yr,d,m,y
      INTEGER FLAG,I,J,DA(12),YE,ME,DE,YL,ML,DL
      logical leap

      DATA DA /31,28,31,30,31,30,31,31,30,31,30,31/

! --  THE SMALLER OF THE TWO DATES IS NOW CHOSEN TO DO THE COUNTING FROM.
      IF(Y.LT.YR)GO TO 10
      IF((Y.EQ.YR).AND.(M.LT.MR)) GO TO 10
      IF((Y.EQ.YR).AND.(M.EQ.MR).AND.(D.LT.DR)) GO TO 10
      FLAG=0
      YE=YR
      ME=MR
      DE=DR
      YL=Y
      ML=M
      DL=D
      GO TO 20
10    FLAG=1
      YE=Y
      ME=M
      DE=D
      YL=YR
      ML=MR
      DL=DR
! --  IN THE ABOVE THE POSTSCRIPT "E" STANDS FOR EARLIER DATE, WHILE
!     "L" STANDS FOR THE LATER DATE.

20    numdays=0
      IF((ME.EQ.ML).AND.(YL.EQ.YE))THEN
        numdays=DL-DE
        IF(FLAG.EQ.1) numdays=-numdays
            RETURN
      END IF

      DO 30 J=ME,12
        IF((ML.EQ.J).AND.(YE.EQ.YL))GOTO 40
        numdays=numdays+DA(J)
        IF((J.EQ.2).AND.(leap(ye)))numdays=numdays+1
30    CONTINUE
      GO TO 50
40    numdays=numdays+DL-DE
      IF(FLAG.EQ.1)numdays=-numdays
      RETURN

50    DO 60 I=YE+1,YL
        DO 70 J=1,12
            IF((YL.EQ.I).AND.(ML.EQ.J))GO TO 80
            numdays=numdays+DA(J)
            IF((J.EQ.2).AND.(leap(i))) numdays=numdays+1
70      CONTINUE
60    CONTINUE
      write(6,65)
65    format(/,' Error in subroutine NUMDAYS')
      stop
      RETURN

80    numdays=numdays+DL-DE
      IF(FLAG.EQ.1) numdays=-numdays

      RETURN
      end

      logical function leap(year)

! --  Function LEAP returns .true. if a year is a leap year.

! --  Revision history:-
!     June-November, 1995: version 1.

      integer year

      leap = ( mod(year,4).eq.0 .and. mod(year,100).ne.0 ).or.( mod(year,400).eq.0 .and. year.ne.0 )

      return
      end

      subroutine captrans(aa)

      integer*4 i,ii
      character*(*) aa

      do 10 i=1,len(aa)
        ii=ichar(aa(i:i))
        if((ii.ge.97).and.(ii.le.122)) aa(i:i)=char(ii-32)
10    continue

      return
      end

      integer function lencar(x)

!     this function counts the number of characters in a character string
!     allan wylie dec 2003

      implicit none
      integer i
      character*(*) x

      lencar=0
      do i=1,len(x)
        if(x(i:i).ne.' ')lencar=lencar+1
      end do

      return
      end
