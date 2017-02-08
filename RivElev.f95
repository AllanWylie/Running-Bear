program RivElev

    !   This program reads a MODFLOW river file looking for river cells anamously higher than its 
    !   neighbors. It then lowers the offending cell to the ellevation of its highest neighbor.

    !   by Allan Wylie Oct 2014

    !   modified by Allan Wylie Dec 2016 to process transient river files
    
    !   define variables
    implicit none
    integer MaxRiv
    parameter(MaxRiv=4000)
    integer nriv,np,i,j,m,count,CountAdj,CountRiv,CountStress,CountComment,nstress,ilay,irow,icol,irchid
    integer alay(MaxRiv),arow(MaxRiv),acol(MaxRiv),archid(MaxRiv)
    real ElevTol,stage,cond,rbot,MaxStage,MaxRbot,StageDif,astage(MaxRiv),acond(MaxRiv),arbot(MaxRiv)
    character*1 lb,ans
    character*20 RivFile1,RivFile2
    character*100 cline

    !   open the river file
100 write(6,101)
101 format(' Enter the name of the river file: ',$)
    read(5,'(a)')RivFile1
    open(unit=10,file=RivFile1,status='old',err=100)

    !   It is easier for me to remember the number of stress periods than the name of the *.dis file
105 write(6,106)
106 format(' Enter the number of stress periods represented in the river file: ',$)
    read(5,*,err=105)nstress

    !   open the output file
110 write(6,111)
111 format(' Enter the name of the output file: '$)
    read(5,'(a)')RivFile2
    open(unit=20,file=RivFile2,status='unknown',err=110)

    !   debuging information
112 write(6,113)
113 format(' Do you want to include debuging information in the output file (y/n)? ',$)
    read(5,'(a)')ans
    !   convert the answer to lower case, then make sure it is either "y" or "n"
    call LOWCASE(ans)
    if(ans.ne."y".and.ans.ne."n")go to 112

    !   river stage tollerance
115 write(6,116) 
116 format(' Enter the elevation difference tolerance for river stage: '$)
    read(5,*,err=115)ElevTol
    write(6,*)

    !   add comment to new river file indicating that the file has been checked for elevation anomolies
    write(20,118)ElevTol
118 format('# Adjusted to filter anamously high river cells. Elevation difference tolerance = ',f6.4,'.')


    !   check for comment lines and pass them to the new river file
    CountComment=0
120 read(10,'(a)',err=9020)lb
    if(lb.eq.'#')then
        CountComment=CountComment+1
        backspace(10)
        read(10,'(a)',err=9020)cline
        write(20,'(a)')cline
        go to 120
    else
        backspace(10)
        read(10,'(a)',err=9020)cline
        write(20,'(a)')cline
    end if
    CountAdj=0
    CountStress=1
    CountRiv=0

    !   loop through each stress period looking for anomolies
    do m=1,nstress
        read(10,*,err=9020)nriv,np
        write(20,130)nriv,np,CountStress
130     format(2i10,'  # stress period ='i4)
        CountStress=CountStress+1

        !   check to see if nriv is greater than MaxRiv, if so MaxRiv will need to be redimensioned
        if(nriv.gt.MaxRiv)then
            write(6,140)
140         format(/,' Redimension MaxRiv ',/)
            go to 9999
        end if
        !   end of check on MaxRiv
        
        !   read the river file for this stress period into memory
        !   all river cells need to be in memory to allow the 3X3 search in the next do loop
        do i=1,nriv
            CountRiv=CountRiv+1
            read(10,*,err=9020)ilay,irow,icol,stage,cond,rbot,irchid
            alay(i)=ilay
            arow(i)=irow
            acol(i)=icol
            astage(i)=stage
            acond(i)=cond
            arbot(i)=rbot
            archid(i)=irchid
        end do

        !   now we work our way through this stress period looking for outliers.
        do i=1,nriv
            MaxStage=0
            count=0

            !   look at the stage in the adjacent river cells
            !   Note: icol+1 is not equal to acol(i+1), but acol(i)+1 is
            !   The upstream cell should have the highest stage elevation not the cell in the middle of the 3X3 block
            do j=1,nriv
                !   check the stage for the 8 cells adjacent to river cell j,j
                !   Note: if an adjacent cell is not a river cell, there will be no associated river stage
                ! map of river stage search
                ! O X X
                ! X   X
                ! X X X
                if(arow(j).eq.arow(i)-1.and.acol(j).eq.acol(i)-1)then
                    count=count+1
                    if(astage(j).gt.MaxStage+ElevTol)then
                    MaxStage=astage(j)
                    MaxRbot=arbot(j)
                    end if
                end if
                ! map of river stage search
                ! X O X
                ! X   X
                ! X X X
                if(arow(j).eq.arow(i)-1.and.acol(j).eq.acol(i))then
                    count=count+1
                    if(astage(j).gt.MaxStage+ElevTol)then
                    MaxStage=astage(j)
                    MaxRbot=arbot(j)
                    end if
                end if
                ! map of river stage search
                ! X X O
                ! X   X
                ! X X X
                if(arow(j).eq.arow(i)-1.and.acol(j).eq.acol(i)+1)then
                    count=count+1
                    if(astage(j).gt.MaxStage+ElevTol)then
                    MaxStage=astage(j)
                    MaxRbot=arbot(j)
                    end if
                end if
                ! map of river stage search
                ! X X X
                ! O   X
                ! X X X
                if(arow(j).eq.arow(i).and.acol(j).eq.acol(i)-1)then
                    count=count+1
                    if(astage(j).gt.MaxStage+ElevTol)then
                    MaxStage=astage(j)
                    MaxRbot=arbot(j)
                    end if
                end if
                ! map of river stage search
                ! X X X
                ! X   O
                ! X X X
                if(arow(j).eq.arow(i).and.acol(j).eq.acol(i)+1)then
                    count=count+1
                    if(astage(j).gt.MaxStage+ElevTol)then
                    MaxStage=astage(j)
                    MaxRbot=arbot(j)
                    end if
                end if
                ! map of river stage search
                ! X X X
                ! X   X
                ! O X X
                if(arow(j).eq.arow(i)+1.and.acol(j).eq.acol(i)-1)then
                    count=count+1
                    if(astage(j).gt.MaxStage+ElevTol)then
                    MaxStage=astage(j)
                    MaxRbot=arbot(j)
                    end if
                end if
                ! map of river stage search
                ! X X X
                ! X   X
                ! X O X
                if(arow(j).eq.arow(i)+1.and.acol(j).eq.acol(i))then
                    count=count+1
                    if(astage(j).gt.MaxStage+ElevTol)then
                    MaxStage=astage(j)
                    MaxRbot=arbot(j)
                    end if
                end if
                ! map of river stage search
                ! X X X
                ! X   X
                ! X X O
                if(arow(j).eq.arow(i)+1.and.acol(j).eq.acol(i)+1)then
                    count=count+1
                    if(astage(j).gt.MaxStage+ElevTol)then
                    MaxStage=astage(j)
                    MaxRbot=arbot(j)
                    end if
                end if
            end do
    
            !   compare the stage in this cell with the adjacent cells and write it to the new file
            if(count.gt.0)then
                if(astage(i).gt.MaxStage+ElevTol)then
                    StageDif=astage(i)-MaxStage
                    if(ans.eq."y")then
                        write(20,150)alay(i),arow(i),acol(i),MaxStage,acond(i),MaxRbot,archid(i),count,StageDif
150                     format(3i5,3f15.5,i4,i3'   # adjusted'f7.4)
                    else
                        write(20,152)alay(i),arow(i),acol(i),MaxStage,acond(i),MaxRbot,archid(i)
152                     format(3i5,3f15.5,i4)
                    end if
                    CountAdj=CountAdj+1
                else
                    if(ans.eq."y")then
                        write(20,155)alay(i),arow(i),acol(i),astage(i),acond(i),arbot(i),archid(i),count
155                     format(3i5,3f15.5,i4,i3)
                    else
                        write(20,152)alay(i),arow(i),acol(i),astage(i),acond(i),arbot(i),archid(i)
                    end if
                end if
            else
                write(20,155)alay(i),arow(i),acol(i),astage(i),acond(i),arbot(i),archid(i)
            end if

        !   go back and check the next river cell in this stress period
        end do

    !   go back to the start of this loop and begin work on the next stress period
    end do

    !   So long and thanks for all the fish    
    write(6,192)trim(RivFile2)
192 format(' - File ',a,' written ok. - ')
    write(6,193)CountAdj,CountRiv
193 format(' - ',i5,' cells adjusted out of ',i6,'. - ')
    go to 9999

    ! a little error trapping
9020  write(6,9030)trim(RivFile1),CountRiv+CountStress+CountComment+1
9030  format(' *** Something is wrong with file ',a,' at line ',i6,'! *** ')
      go to 9999

9999  close(unit=10)
      close(unit=20)

      !pause

      end program RivElev

        SUBROUTINE LOWCASE(ASTRNG)
 
!C -- Subroutine LOWCASE converts a string to lower case.

!c	subroutine by John Doherty
!c	converts upper case to lower case
!c	tables taped to the back cover of "ten statement fortran plus fortran iv" text
!c	explain how this works.  ASCII characters 65 through 90 are upper case and
!c	characters 61 through 122 are lower case.
!c
!c	calling lowcase - call lowcase(variable)
 
        INTEGER I,J
        CHARACTER*(*) ASTRNG
 
        DO 10 I=1,NBLaNK(ASTRNG)
        J=ICHAR(ASTRNG(I:I))
        IF((J.GE.65).AND.(J.LE.90)) ASTRNG(I:I)=CHAR(J+32)
10      CONTINUE
        RETURN
        END
