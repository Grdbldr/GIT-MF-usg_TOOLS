module MUT  !### Modflow-USG Tools
    use GeneralRoutines
    use MUSG
    use tecplot

    implicit none

    !integer, parameter :: maxnn=1000000,maxne=1000000,maxnef=2000000
    character(60) :: MUT_CMD="none"
    character(60) :: ProcessModflowUSG_CMD="modflow_usg"

    character(256) :: FileNameMUT
    integer :: FnumMUT
    integer :: FnumUserMUT
    character(40)       :: prefix = ''
    integer				:: l_prfx  = 0

    integer :: FNum
    character(MAXLBL) :: DirName ! directory name

    contains

    subroutine Header
        call date_and_time(DateSTR, TIME = TimeSTR, ZONE = TimezoneSTR)
        call Msg( '@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ')
        call Msg( '@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ')
        call Msg( '@@                                                                      @@ ')
        call Msg( '@@                    MUT         '//MUTVersion//'                                 @@ ')
        call Msg( '@@                    Run date '//DateStr//'                             @@ ')
        call Msg( '@@                                                                      @@ ')
        call Msg( '@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ')
        call Msg( '@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ')
    end subroutine  header

    subroutine OpenMUT  !--- Modflow user tools  .mut

        write(*,'(a)')  'MUT version '//MUTVersion

        ! open the user MUT input file
        call EnterPrefix(prefix,l_prfx,FnumUserMUT,'mut')

        ! open a file called prefix.eco, if it exists, overwrite it with MUT header
        FNameEco=prefix(:l_prfx)//'o.eco'
        call openascii(FnumEco,FNameEco)
        call header
        call Msg ('Echo file: '//FNameEco)
        call Msg ('User input file: '//prefix(:l_prfx)//'.mut')
        ErrFNum=FnumEco


        ! Create one processed input file
        FNameInput=prefix(:l_prfx)//'o.input'
        call openascii(FnumMUT,FNameInput)

        ! strip out blanks and comments and concatenate included files
        call StripComments(FnumUserMUT,FnumMUT)
	    call freeunit(FnumUserMUT)
	


    end subroutine OpenMUT



    subroutine ProcessMUT !--- Command processor for Modflow-USG Tools (.mut file extension)

        type (ModflowProject) MyMUSG_Project
        do
            read(FnumMUT,'(a)',iostat=status,end=10) MUT_CMD
            call LwrCse(MUT_CMD)
            call Msg('!-----------------------')
            call Msg('MUT:'//MUT_CMD)


            if(status/=0) then
 		        write(ErrStr,'(a)') 'File: a.mut'
		        l1=len_trim(ErrStr)
		        write(ErrStr,'(a)') ErrStr(:l1)//New_line(a)//'Error reading file'
			    call ErrMsg(ErrStr)
           endif

            if(index(MUT_CMD, StopWatch_CMD) /= 0) then
                read(FnumMUT,*) l1
                read(FnumMUT,'(a)') TmpSTR
                call StopWatch(l1,TmpSTR(:len_trim(TmpSTR)))
            else if(index(MUT_CMD, SplitTime_CMD) /= 0) then
                read(FnumMUT,*) l1
                call SplitTime(l1)
            else if(index(MUT_CMD, ElapsedTime_CMD) /= 0) then
                read(FnumMUT,*) l1
                call ElapsedTime(l1)


            else if(index(MUT_CMD, ProcessModflowUSG_CMD) /= 0) then
                call ProcessModflowUSG(FnumMUT,MyMUSG_Project,prefix)


            else
                call ErrMsg('MUT?:'//MUT_CMD)
            end if
        end do

        10 continue
    end subroutine ProcessMUT


    subroutine CloseMUT !--- Modflow-USG Tools .mut
       call Msg('!-----------------------')
       call Msg('MUT: Normal exit')
       call FreeUnit(FnumMUT)
    end subroutine CloseMUT



end module MUT
