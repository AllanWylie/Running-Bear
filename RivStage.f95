program RivStage

!   program RIVSTAGE makes a transient river file where the river stage varies seasonaly based
!   on stage data collected at river gaging stations.

!   there are 298 rows between gage1 and gage2, slope = (gage2-gage1)/298. change in gage divided
!   by the number of model cells between the gages. That is change in slope per model row.

    !   by Allan Wylie January 2015

    !   modified by Allan Wylie January 2017

    implicit none
    !   nstress=number stress periods, nreach=number reachs,     
    integer i,j,k,ilay,nrow,ncol,irow,icol,itmp,np,ireach,nstress,nreach,indicator,nline,dist,ThisStressP
    !   reach identifyers
    integer KetHaiRch,HaiGlenRch,GlenWoodRch,WoodStanRch,SpringRch,SportPicRch
    !   indicate how the reach is to be interpolated 1=interpolate this reach, 2=set stage to RBot
    integer KetHaiInd,HaiGlenInd,GlenWoodInd,WoodStanInd,SpringInd,SportPicInd
    !   the row number the gage is located on
    integer KetRow,HaiRow,GlenRow,WoodRow,StanRow,SportRow,PicabRow
    integer, allocatable:: iarray(:,:)
    !   these variables are for the stage data
    real Ketchum,Hailey,Glendale,WoodRR,StantonX,Sportsman,Picabo
    !   variables for the slope or interpolation factors
    real stage,cond,rbot,slope,slopeKH,slopeHG,slopeGS,SlopeSS,SlopeSP
    !   lb is used to check for comment lines
    character*1 lb
    !   these are file names
    character*20 ReachLst,rivfilein,rivfileout,arrayfile
    !   cline is used to read and write a line of characters
    character*100 cline

    !   open reach list file
100 write(6,105)
105 format(' Enter the name of the reach list file: ',$)
    read(5,'(a)')ReachLst
    open(unit=10,file=ReachLst,status='old',err=100)
    !   The first line contains text identifying the variables in the second line, so skip line 1
    read(10,'(a)')
    !   this line contains lots of variables, so read them in
    read(10,*,err=910)nstress,nreach,KetHaiRch,KetHaiInd,KetRow,HaiRow,HaiGlenRch,HaiGlenInd,HaiRow,GlenRow,GlenWoodRch,GlenWoodInd,GlenRow,WoodRow,WoodStanRch,WoodStanInd,WoodRow,StanRow,SpringRch,SpringInd,SportPicRch,SportPicInd,SportRow,PicabRow
    !   the next line contains column lables, so we want to skip it
    read(10,'(a)')
 
    !   open the integer array that identifyes which river cells are in which reach
110 write(6,115)
115 format(' Enter name of the integer array identifying the reaches: ',$)
    read(5,'(a)')arrayfile
    open(unit=20,file=arrayfile,status='old',err=110)

    !   read the integer array
    read(20,*)ncol,nrow
    allocate(iarray(nrow,ncol))
    do irow=1,nrow
        read(20,*,err=981,end=981) (iarray(irow,icol),icol=1,ncol)
    end do
    close(unit=20)

    !   open the MODFLOW river file
140 write(6,145)
145 format(' Enter name of the steady state river file: ',$)
    read(5,'(a)')rivfilein
    open(unit=30,file=rivfilein,status='old',err=140)

    !   nline is used to track the number of comment lines in the steady state river file
    nline=0

    !   open the output file
160 write(6,165)
165 format(' Enter the name for the new transinet river file: ',$)
    read(5,'(a)')rivfileout
    open(unit=40,file=rivfileout,status='unknown',err=160)
    write(6,*)

    !   add comment lines identifying files used to calculate river stage
    write(40,166)
166 format('# Transient river stage calculations conducted using program RivStage.exe')
    write(40,167)trim(ReachLst),trim(arrayfile),trim(rivfilein)
167 format('# Input files for river stage calculations = ',a,', ',a,', ',a,'.')


    !   check for comment lines in the steady state river file and pass them to the new river file
170 read(30,'(a)')lb
    if(lb.eq.'#')then
        nline=nline+1
        backspace(30)
        read(30,'(a)')cline
        write(40,'(a)')cline
        go to 170
    else
        backspace(30)
        nline=nline+1
        read(30,'(a)')cline
        write(40,'(a)')cline
    end if
    
    !   the first stress period in the new river file is steady state, so copy this file over
    !   write itmp, np and stress period to the new river file
    read(30,*)itmp,np
    ThisStressP=1
    write(40,180)itmp,np,ThisStressP
180 format(2i10,'  stress period ='i4)

    do i=1,itmp
        read(30,*,err=984)ilay,irow,icol,stage,cond,rbot,indicator
        write(40,190)ilay,irow,icol,stage,cond,rbot,indicator
190     format(3i10,3f12.4,i4)
    end do

    !   move back to the top of the steady state river file and skip past the initial comment lines
    rewind(30)
    do k=1,nline
        read(30,'(a)')
    end do
 
    !   Just printed out the initial steady state stress period so subtract this from nstress
    nstress=nstress-1
    
    !   read the files containing the river stage data and calculate the interpolation factors
   do i=1,nstress
       !   read in the gage data for this stress period
       read(10,*,err=910)Ketchum,Hailey,Glendale,WoodRR,StantonX,Sportsman,Picabo 
        !   calculate the interpolation factors for this stress period
        if(KetHaiInd.eq.1)then
            call interpolate(Ketchum,KetRow,Hailey,HaiRow,slopeKH)
        end if
        if(HaiGlenInd.eq.1)then
            call interpolate(Hailey,HaiRow,Glendale,GlenRow,slopeHG)
        end if
        if(GlenWoodInd.eq.1)then
            call interpolate(Glendale,GlenRow,WoodRR,WoodRow,slopeGS)
        end if
        if(WoodStanInd.eq.1)then
            call interpolate(WoodRR,WoodRow,StantonX,StanRow,SlopeSS)
        end if
        if(SpringInd.eq.1)then
            call interpolate(WoodRR,WoodRow,StantonX,StanRow,SlopeSS)
        end if
        if(SportPicInd.eq.1)then
            call interpolate(Sportsman,SportRow,Picabo,PicabRow,SlopeSP)
        end if

        !   write itmp, np and stress period number to the new river file
        read(30,*)itmp,np
       ThisStressP=ThisStressP+1
        write(40,180)itmp,np,ThisStressP

        !   compare the river cells to the integer array and adjust stage
        do j=1,itmp
            read(30,*,err=984)ilay,irow,icol,stage,cond,rbot,indicator
            !   check to see which reach this cell belongs in
            ireach=iarray(irow,icol)
            !   this is the nr Ketchum-Hailey reach
            if(ireach.eq.1)then
                dist=irow-KetRow
                stage=stage+Ketchum+slopeKH*dist
            end if
            if(ireach.eq.2)then
                !   This is the Hailey-Glendale Rd reach
                dist=irow-HaiRow
                stage=stage+Hailey+slopeHG*dist
            end if
            if(ireach.eq.3)then
                !   this is the Glendale Rd to Wood River Ranch reach
                dist=irow-GlenRow
                stage=stage+Glendale+slopeGS*dist
            end if
            if(ireach.eq.4)then
                !   this is the Wood River Ranch to Stanton Crossing reach
                dist=irow-WoodRow
                stage=stage+WoodRR+SlopeSS*dist
            end if
            if(ireach.eq.5)then
                !   This is in a spring reache, so stage = river bottom
                stage=rbot
            end if
            if(ireach.eq.6)then
                !   this is Silver Cr below Sportsmans Access
                dist=irow-SportRow
                stage=stage+Sportsman+SlopeSP*dist
            end if
            !   write cell data and adjusted stage to new river file
            write(40,190)ilay,irow,icol,stage,cond,rbot,indicator
        end do

        !   move back to the top of the steady state river file and start on the next stress period
        rewind(30)
        do k=1,nline
            read(30,'(a)')
        end do
        continue
   end do
    
    !   so long and thanks for all the fish
    write(6,900) trim(rivfileout)
900 format(' ** file ',a,' written ok. **')
    go to 999

    !   now a little error trapping
910 write(6,915)trim(ReachLst)
915 format('--Problem encountered in ',a,'.--')
    go to 999

981 write(6,982)iarray
982 format('--Problem encountered in ',a,'.--')
    go to 999

984 write(6,985)trim(rivfilein)
985 format('--Problem encounterd in ',a,'.--')
    go to 999

    !   close all open files and deallocate arrays
999 close(unit=10)
    close(unit=30)
    close(unit=40)
    deallocate(iarray)
    pause

    end program RivStage

    subroutine interpolate (UpStage,UpRow,DwnStage,DwnRow,slope)

        !   declare variables
        integer UpRow,DwnRow
        real    UpStage,DwnStage

        !   the calculations
        slope=(DwnStage-UpStage)/((DwnRow+1)-UpRow)

        !   the end
    end