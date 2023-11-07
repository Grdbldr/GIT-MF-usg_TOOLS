module MUSG !
    use GeneralRoutines
    !use ProcessCSV
    use fem
    use tecplot
    implicit none

    character(256) :: MUSG_CMD
    
    character(60) :: MUSG_ProcessFiles_CMD='process modflow files'
    
    character(60) :: MUSG_ReadGSF_CMD='read gsf'
    character(60) :: MUSG_ReadBinaryHeadFile_CMD='read binary head file'
    character(60) :: MUSG_ReadBinaryDrawdownFile_CMD='read binary drawdown file'
    character(60) :: MUSG_GWF_ToTecplot_CMD='modflow_usg to tecplot'

    character(60) :: MUSG_ReadBinaryCbbFile_CMD='read binary cell by cell flow file'
    character(60) :: MUSG_CbbToTecplot_CMD='cbb to tecplot'

    character(60) :: MUSG_ReadClnFile_CMD='read cln file'
    character(60) :: MUSG_ClnToTecplot_CMD='cln to tecplot'

    character(60) :: MUSG_WBudgetToTecplot_CMD='water budget to tecplot'
    !character(60) :: MUSG_Lexicon_CMD='build lexicon'
    
    

    
    character(60) :: MUSG_ReadAsciiHeadFile_CMD='read usgbin2tab_h head file'
    character(60) :: MUSG_ReadAsciiKxFile_CMD='read ascii kx file'
    character(60) :: MUSG_ReadAsciiSsFile_CMD='read ascii ss file'
    character(60) :: MUSG_ReadAsciiSyFile_CMD='read ascii sy file'
    character(60) :: MUSG_ReadAsciiVanisFile_CMD='read ascii vanis file'
    character(60) :: MUSG_ReadWellConstructionCSVFile_CMD='read well construction csv file'
    character(60) :: MUSG_Read_EIWellCSVFile_CMD='read ei well construction csv file'
    character(60) :: MUSG_ReadRiverFlowsAsciiFile_CMD='read river cell flows'
    character(60) :: MUSG_RiverFlowsToTecplot_CMD='modflow-usg river flows to tecplot'
    character(60) :: MUSG_ReadHeadCalibrationAsciiFile_CMD='read head calibration data'
    character(60) :: MUSG_HeadCalibrationToTecplot_CMD='modflow-usg head calibration to tecplot'
    character(60) :: MUSG_RiverConductanceUpdate_CMD='river conductance update'
    character(60) :: MUSG_PEST_WellRatePenalties_CMD='pest well rate penalties'
    character(60) :: MUSG_PEST_UpdateWellRatePenalties_CMD='update well rate penalties'
    character(60) :: MUSG_PEST_FlowSourceCapture_CMD='flowsource capture'
    !character(60) :: MUSG_PEST_CLNFileCalculations_CMD='cln file calculations'
    character(60) :: MUSG_PEST_EIWellCLNFileUpdate_CMD='ei well cln file update'
    character(60) :: MUSG_PEST_CountParticlesToWells_CMD='count particles to wells'
    character(60) :: MUSG_PEST_ExternalCodeExecute_CMD='external code execute'
    character(60) :: MUSG_PEST_RTWellOptimization_CMD='rt well optimization'

    ! Modflow 2005 and Modpath 5 version for Nicole (requested by Argha Namhata, Perth)   
    character(60) :: M2005_PEST_CountParticlesToWells_CMD='count particles to wells modflow2005 modpath5'

    character(60) :: MUSG_End_CMD=	'end'
    
    character(MAXLBL) :: fname
    character(MAXSTRING) :: line
    
    
    
    ! Added for Modflow-USG Tools
    

    type MUSG_Mesh
        character(128) :: meshtype
        integer :: nCell   ! is the number of cells (elements in HGS) in the mesh
        integer :: nLay    ! is the number of layers in the model (if specific to gwf mesh should move to type project MUSG_Project? )

        integer :: iz      ! is 1 if the elevations of node and mesh elements vertices are supplied; 0 otherwise
        integer :: ic      ! is 1 if the cell specifications associated with each node are supplied; 0 otherwise
            
            
        ! arrays of size nCell
        real(dr), allocatable :: xCell(:)      ! cell x coordinate
        real(dr), allocatable :: yCell(:)      ! cell y coordinate
        real(dr), allocatable :: zCell(:)      ! cell z coordinate
        integer, allocatable :: lay(:)      ! cell layer number
        integer :: m

        ! of size nCell, m
        integer, allocatable :: ivertex(:,:)

        integer :: nvertex ! is the number of element vertex definitions to follow (my nn)
        ! of size nvertex
        real(dr), allocatable :: x(:) 
        real(dr), allocatable :: y(:)
        real(dr), allocatable :: z(:)
        
        logical :: have_mesh=.false.
            
    end type MUSG_Mesh
 

    type MUSG_Project
 
        type(MUSG_Mesh) gwf
        type(MUSG_Mesh) cln
        type(MUSG_Mesh) swf
        
        character(128) :: MUTPrefix
        character(128) :: Prefix

        ! GSF file required for grid dimensions but not listed in NAM file
        character(128) :: FNameGSF
        integer :: iGSF
        
        ! CLNGSF file required for grid dimensions but not listed in NAM file
        character(128) :: FNameCLNGSF
        integer :: iCLNGSF
        
        ! SWFGSF file required for grid dimensions but not listed in NAM file
        character(128) :: FNameSWFGSF
        integer :: iSWFGSF
        
        ! NAM file
        character(128) :: FNameNAM
        integer :: iNAM
        
        ! DISU file
        character(128) :: FNameDISU
        integer :: iDISU

        ! LIST file
        character(128) :: FNameLIST
        integer :: iLIST
        character(10) :: TUnits
        character(10) :: LUnits
        
        ! BAS6 file
        character(128) :: FNameBAS6
        integer :: iBAS6
        logical :: UNSTRUCTURED=.false.
        logical :: FREE=.false.  
        logical :: RICHARDS=.false.

        ! SMS file
        character(128) :: FNameSMS
        integer :: iSMS

        ! OC file
        character(128) :: FNameOC
        integer :: iOC
        integer :: ntime = 0
        real, allocatable :: totim(:)
        
        ! RCH file
        character(128) :: FNameRCH
        integer :: iRCH
        
        ! RIV file
        character(128) :: FNameRIV
        integer :: iRIV
        
        ! WEL file
        character(128) :: FNameWEL
        integer :: iWEL
        
        ! CHD file
        character(128) :: FNameCHD
        integer :: iCHD
        
        ! EVT file
        character(128) :: FNameEVT
        integer :: iEVT
        
        ! DRN file
        character(128) :: FNameDRN
        integer :: iDRN
 
        ! LPF file
        character(128) :: FNameLPF
        integer :: iLPF

        ! CLN file
        character(128) :: FNameCLN
        integer :: iCLN

        ! GNC file
        character(128) :: FNameGNC
        integer :: iGNC


        
        ! DATA(BINARY) files
        ! HDS file
        character(128) :: FNameHDS
        integer :: iHDS
	    real, allocatable :: Head(:,:)
	    real, allocatable :: clnHead(:,:)
        
        ! DDN file
        character(128) :: FNameDDN
        integer :: iDDN
	    real, allocatable :: Sat(:,:)
	    real, allocatable :: clnSat(:,:)
        
        ! CBB file
        character(128) :: FNameCBB
        integer :: iCBB
	    real, allocatable :: Cbb_STORAGE(:,:)
	    real, allocatable :: Cbb_CONSTANT_HEAD(:,:)
	    real, allocatable :: Cbb_RECHARGE(:,:)
	    real, allocatable :: Cbb_DRAINS(:,:)
	    real, allocatable :: Cbb_ja(:,:)

        ! CBCCLN file
        character(128) :: FNameCBCCLN
        integer :: iCBCCLN
	    real, allocatable :: CBCCLN_STORAGE(:,:)
	    real, allocatable :: CBCCLN_CONSTANT_HEAD(:,:)
	    real, allocatable :: CBCCLN_RECHARGE(:,:)
	    real, allocatable :: CBCCLN_DRAINS(:,:)
	    real, allocatable :: CBCCLN_ja(:,:)
        
        ! Scan file
        integer :: nDim=10000
        integer :: nKeyWord
        character(MAXSTRING), ALLOCATABLE :: KeyWord(:) ! read buffer for location data
        character(128) :: FNameLEX
        integer :: iLEX

        character(128) :: Name
        integer :: LengthName
        logical :: Exists=.false.
	    integer :: Unit

        logical :: blockel


        logical,allocatable :: nchosen(:)

        integer :: iz
        integer :: ic

	    real(dr), allocatable :: Kx(:)
	    real(dr), allocatable :: Thick(:)
	    real(dr), allocatable :: T(:)
	    real(dr), allocatable :: Ky(:)
	    real(dr), allocatable :: Kz(:)
	    real(dr), allocatable :: Ss(:)					
	    real(dr), allocatable :: Sy(:)
	    real(dr), allocatable :: Vanis(:)
        
        ! River Flows
        integer :: nlines
	    integer, allocatable :: StressPeriod(:)
	    integer, allocatable :: RiverCell(:)
	    real(dr), allocatable :: RiverFlow(:)
	    real(dr), allocatable :: RiverHead(:)
	    real(dr), allocatable :: RiverElev(:)
	    real(dr), allocatable :: RiverCond(:)
	    real(dr), allocatable :: RiverConc(:)

        ! Head Calibration
        ! StressPeriod,WellName,X83_ft,Y89_ft,Zmin,Zmax,ZMidpoint,Observed_ft,Simulated_ft,Residual_ft,Residual_ft_Jeff
        integer :: nlinesHead
	    integer, allocatable :: StressPeriodHead(:)
	    character(30), allocatable :: WellNameHead(:)
	    real(dr), allocatable :: Xhead(:)
	    real(dr), allocatable :: YHead(:)
	    real(dr), allocatable :: ZminHead(:)
	    real(dr), allocatable :: ZmaxHead(:)
	    real(dr), allocatable :: ZMidpointHead(:)
 	    real(dr), allocatable :: Observed_ft(:)
 	    real(dr), allocatable :: Simulated_ft(:)
 	    real(dr), allocatable :: Residual_ft(:)
        
        ! Well Construction
        ! Name	X	Y	Bottom_elevation_ft	Top_elevation_ft	casing_radius_ft	Well_on	Well_off
        integer :: nWellConst
	    character(30), allocatable :: NameWellConst(:)
	    real(dr), allocatable :: XWellConst(:)
	    real(dr), allocatable :: YWellConst(:)
	    real(dr), allocatable :: BotElevWellConst(:)
	    real(dr), allocatable :: TopElevWellConst(:)
	    real(dr), allocatable :: CasingRadiusWellConst(:)
	    character(30), allocatable :: TonWellConst(:)
	    character(30), allocatable :: ToffWellConst(:)


        ! EI Well Construction
        ! Name	X	Y	Top_elevation_ft	L1   Offset  L2
        integer :: n_EIWell
	    character(30), allocatable :: Name_EIWell(:)
	    real(dr), allocatable :: X_EIWell(:)
	    real(dr), allocatable :: Y_EIWell(:)
	    real(dr), allocatable :: TopElev_EIWell(:)
	    real(dr), allocatable :: ScreenALength_EIWell(:)
	    real(dr), allocatable :: ScreenBOffset_EIWell(:)
	    real(dr), allocatable :: ScreenBLength_EIWell(:)
	    real(dr), allocatable :: ScreenCOffset_EIWell(:)
	    real(dr), allocatable :: ScreenCLength_EIWell(:)
        
        ! cln file
        
      ! URWORD        
      integer :: linlen
      integer :: ncode, icol, iout, in
      integer :: istart
      real :: r
      integer :: istop,n

        

    end type MUSG_Project

    ! other local variables
    integer, Parameter :: MAXCLN=10000  ! assuming never more than 10000 CLN's
    integer, Parameter :: MAXSTRESS=10000  ! assuming never more than 10000 Stress Periods

    
    contains

   subroutine ProcessModflowUSG(FnumTG, musg_l,prefix) !--- Process MUSG instructions for this data structure  musg_l
        implicit none

        integer :: FnumTG
        character(*) :: prefix
        type (MUSG_Project) musg_l
        musg_l.MUTPrefix=prefix

        do
            read(FnumTG,'(a)',iostat=status,end=10) MUSG_CMD
            call Msg('!-----------------------')
            call Msg('MUSG:'//MUSG_CMD)

            if(status/=0) then
 		        write(ErrStr,'(a)') 'File: a.MUSG'
		        l1=len_trim(ErrStr)
		        write(ErrStr,'(a)') ErrStr(:l1)//New_line(a)//'Error reading file'
			    call ErrMsg(ErrStr)
            endif
            
            

            if(index(MUSG_CMD, MUSG_ProcessFiles_CMD)  /= 0) then
                call MUSG_ProcessFiles(FnumTG, musg_l)

            !else if(index(MUSG_CMD, MUSG_ReadGSF_CMD)  /= 0) then
            !    call MUSG_ReadGSF(FnumTG, musg_l)

            !else if(index(MUSG_CMD, MUSG_ReadBinaryHeadFile_CMD)  /= 0) then
            !    call MUSG_ReadBinaryHeadFile(FnumTG, musg_l)
            
            !else if(index(MUSG_CMD, MUSG_ReadBinaryDrawdownFile_CMD)  /= 0) then
            !    call MUSG_ReadBinaryDrawdownFile(FnumTG, musg_l)

            !else if(index(MUSG_CMD, MUSG_GWF_ToTecplot_CMD)  /= 0) then
            !    call MUSG_GWF_ToTecplot(FnumTG, musg_l)
                

            !else if(index(MUSG_CMD, MUSG_ReadBinaryCbbFile_CMD)  /= 0) then
            !    call MUSG_ReadBinaryCbbFile(FnumTG, musg_l)

            !else if(index(MUSG_CMD, MUSG_CbbToTecplot_CMD)  /= 0) then
            !    call MUSG_CbbToTecplot(FnumTG, musg_l)
            !

            
            else if(index(MUSG_CMD, MUSG_ReadAsciiHeadFile_CMD)  /= 0) then
                call MUSG_ReadAsciiHeadFile(FnumTG, musg_l)


            else if(index(MUSG_CMD, MUSG_ReadAsciiKxFile_CMD)  /= 0) then
                call MUSG_ReadAsciiKxFile(FnumTG, musg_l)

            else if(index(MUSG_CMD, MUSG_ReadAsciiSsFile_CMD)  /= 0) then
                call MUSG_ReadAsciiSsFile(FnumTG, musg_l)

            else if(index(MUSG_CMD, MUSG_ReadAsciiSyFile_CMD)  /= 0) then
                call MUSG_ReadAsciiSyFile(FnumTG, musg_l)

            else if(index(MUSG_CMD, MUSG_ReadAsciiVanisFile_CMD)  /= 0) then
                call MUSG_ReadAsciiVanisFile(FnumTG, musg_l)


            else if(index(MUSG_CMD, MUSG_ReadRiverFlowsAsciiFile_CMD)  /= 0) then
                call MUSG_ReadRiverFlowsAsciiFile(FnumTG, musg_l)

            else if(index(MUSG_CMD, MUSG_RiverFlowsToTecplot_CMD)  /= 0) then
                call MUSG_RiverFlowsToTecplot(FnumTG, musg_l)
            
            else if(index(MUSG_CMD, MUSG_ReadHeadCalibrationAsciiFile_CMD)  /= 0) then
                call MUSG_ReadHeadCalibrationAsciiFile(FnumTG, musg_l)

            else if(index(MUSG_CMD, MUSG_HeadCalibrationToTecplot_CMD)  /= 0) then
                call MUSG_HeadCalibrationToTecplot(FnumTG, musg_l)
            
            !else if(index(MUSG_CMD, MUSG_WBudgetToTecplot_CMD)  /= 0) then
            !    call MUSG_WBudgetToTecplot(FnumTG)
                
            !else if(index(MUSG_CMD, MUSG_Lexicon_CMD)  /= 0) then
            !    call MUSG_BuildLexicon(FnumTG, musg_l)

            else if(index(MUSG_CMD, MUSG_RiverConductanceUpdate_CMD)  /= 0) then
                call MUSG_RiverConductanceUpdate(FnumTG)

            else if(index(MUSG_CMD, MUSG_PEST_WellRatePenalties_CMD)  /= 0) then
                call MUSG_PEST_WellRatePenalties(FnumTG)

            else if(index(MUSG_CMD, MUSG_PEST_UpdateWellRatePenalties_CMD)  /= 0) then
                call MUSG_PEST_UpdateWellRatePenalties(FnumTG)
            
            else if(index(MUSG_CMD, MUSG_PEST_FlowSourceCapture_CMD)  /= 0) then
                call MUSG_PEST_FlowSourceCapture(FnumTG)

            !else if(index(MUSG_CMD, MUSG_PEST_CLNFileCalculations_CMD)  /= 0) then
            !    call MUSG_PEST_CLNFileCalculations(FnumTG, musg_l)

            else if(index(MUSG_CMD, MUSG_PEST_EIWellCLNFileUpdate_CMD)  /= 0) then
                call MUSG_PEST_EIWellCLNFileUpdate(FnumTG, musg_l)

            else if(index(MUSG_CMD, MUSG_PEST_CountParticlesToWells_CMD)  /= 0) then
                call MUSG_PEST_CountParticlesToWells(FnumTG)
            
            else if(index(MUSG_CMD, M2005_PEST_CountParticlesToWells_CMD)  /= 0) then
                call M2005_PEST_CountParticlesToWells(FnumTG)

                
            else if(index(MUSG_CMD, MUSG_ReadWellConstructionCSVFile_CMD)  /= 0) then
                call MUSG_ReadWellConstructionCSVFile(FnumTG, musg_l)

            else if(index(MUSG_CMD, MUSG_Read_EIWellCSVFile_CMD)  /= 0) then
                call MUSG_Read_EIWellCSVFile(FnumTG, musg_l)

            else if(index(MUSG_CMD, MUSG_PEST_ExternalCodeExecute_CMD)  /= 0) then
                call MUSG_PEST_ExternalCodeExecute(FnumTG)

            else if(index(MUSG_CMD, MUSG_PEST_RTWellOptimization_CMD)  /= 0) then
                call MUSG_PEST_RTWellOptimization(FnumTG)

 
            else if(index(MUSG_CMD, MUSG_End_CMD)  /= 0) then
                exit

            else
                call ErrMsg('MUSG?:'//MUSG_CMD)
            end if
        end do

        10 continue

    end subroutine ProcessModflowUSG

    subroutine MUSG_ProcessFiles(FnumTG, musg_l)
        implicit none
        
        integer :: FnumTG
        type (MUSG_Project) musg_l
        
        !integer :: Initial_nKeyWord
        

        ! read prefix for project
        read(FnumTG,'(a)') musg_l.Prefix
		call lcase(musg_l.Prefix)
        call Msg('Modflow project prefix: '//musg_l.Prefix)

        ! Process GSF file (should be part of NAM file processing, not separate
        ! Same for cln.gsf and swf.gsf later
        musg_l.FNameGSF=musg_l.Prefix(:len_trim(musg_l.Prefix))//'.gsf'
        inquire(file=musg_l.FNameGSF,exist=FileExists)
        if(.not. FileExists) then
            call Msg('No grid specification file: '//musg_l.FNameGSF)
        else
            call Msg('Modflow GSF file: '//musg_l.FNameGSF)
	        call getunit(musg_l.iGSF)
            open(musg_l.iGSF,file=musg_l.FNameGSF,status='unknown',form='formatted')
        
            call MUSG_ReadGSF(musg_l)
        endif

        musg_l.FNameCLNGSF=musg_l.Prefix(:len_trim(musg_l.Prefix))//'.cln.gsf'
        inquire(file=musg_l.FNameCLNGSF,exist=FileExists)
        if(.not. FileExists) then
            call Msg('No grid specification file: '//musg_l.FNameCLNGSF)
        else
            call Msg('Modflow CLNGSF file: '//musg_l.FNameCLNGSF)
	        call getunit(musg_l.iCLNGSF)
            open(musg_l.iCLNGSF,file=musg_l.FNameCLNGSF,status='unknown',form='formatted')
        
            call MUSG_ReadCLNGSF(musg_l)
        endif

        !musg_l.FNameSWFGSF=musg_l.Prefix(:len_trim(musg_l.Prefix))//'.swf.gsf'
        !inquire(file=musg_l.FNameSWFGSF,exist=FileExists)
        !if(.not. FileExists) then
        !    call Msg('No grid specification file: '//musg_l.FNameSWFGSF)
        !else
        !    call Msg('Modflow SWFGSF file: '//musg_l.FNameSWFGSF)
	       ! call getunit(musg_l.iSWFGSF)
        !    open(musg_l.iSWFGSF,file=musg_l.FNameSWFGSF,status='unknown',form='formatted')
        !
        !    call MUSG_ReadSWFGSF(musg_l)
        !endif
        !
        
        ! Scan file
        musg_l.FNameLEX=musg_l.MUTPrefix(:len_trim(musg_l.MUTPrefix))//'.scan'
        open(musg_l.iLEX,file=musg_l.FNameLEX,status='unknown',form='formatted')
        write(musg_l.iLEX,'(a)') 'Scan file from project '//musg_l.prefix(:len_trim(musg_l.prefix))
        musg_l.nKeyWord=0
        allocate(musg_l.KeyWord(musg_l.nDim))
        musg_l.KeyWord(:)='UNDEFINED'


        ! Process NAM file
        musg_l.FNameNAM=musg_l.Prefix(:len_trim(musg_l.Prefix))//'.nam'
        inquire(file=musg_l.FNameNAM,exist=FileExists)
        if(.not. FileExists) then
            call ErrMsg('No file found: '//musg_l.FNameNAM)
        endif
        call Msg('Modflow nam file: '//musg_l.FNameNAM)
	    call getunit(musg_l.iNAM)
        open(musg_l.iNAM,file=musg_l.FNameNAM,status='unknown',form='formatted')
        
        write(musg_l.iLEX,'(a)') 'Scanning: '//musg_l.FNameNAM(:len_trim(musg_l.FNameNAM))//'-----------------------------------------------------'
        call MUSG_ScanFileToLexicon(musg_l.iNAM,musg_l)

        
        ! read initial comment lines beginning with #
        do 
            read(musg_l.iNAM,'(a)',iostat=status) line
            if(status /= 0) exit
            
            if(line(1:1).eq.'#') then
                call Msg(line(:len_trim(line)))
                cycle
            endif
            
            call lcase(line)
        
            if(index(line,'list') .gt. 0) then
                call openMUSGFile('LIST',line,musg_l.Prefix,musg_l.iLIST,musg_l.FNameLIST)
                
                call MUSG_WriteVolumeBudgetToTecplot(musg_l)
                call MUSG_CreateStepPeriodTimeFile(musg_l)

            else if(index(line,'disu') .gt. 0) then
                call openMUSGFile('DISU',line,musg_l.Prefix,musg_l.iDISU,musg_l.FNameDISU)
                
                write(musg_l.iLEX,'(a)')'Scanning: '//musg_l.FNameDISU(:len_trim(musg_l.FNameDISU))//'-----------------------------------------------------'
                call MUSG_ScanFileToLexicon(musg_l.iDISU,musg_l)
                
                !call MUSG_ReadDISU(musg_l)
                
                
            else if(index(line,'bas6') .gt. 0) then
                call openMUSGFile('BAS6',line,musg_l.Prefix,musg_l.iBAS6,musg_l.FNameBAS6)

                write(musg_l.iLEX,'(a)') 'Scanning: '//musg_l.FNameBAS6(:len_trim(musg_l.FNameBAS6))//'-----------------------------------------------------'
                call MUSG_ScanFileToLexicon(musg_l.iBAS6,musg_l)
                call MUSG_SetOptionsFromBAS6(musg_l)

            else if(index(line,'sms') .gt. 0) then
                call openMUSGFile('SMS',line,musg_l.Prefix,musg_l.iSMS,musg_l.FNameSMS)

                write(musg_l.iLEX,'(a)') 'Scanning: '//musg_l.FNameSMS(:len_trim(musg_l.FNameSMS))//'-----------------------------------------------------'
                call MUSG_ScanFileToLexicon(musg_l.iSMS,musg_l)

            else if(index(line,'oc') .gt. 0) then
                call openMUSGFile('OC',line,musg_l.Prefix,musg_l.iOC,musg_l.FNameOC)

                write(musg_l.iLEX,'(a)') 'Scanning: '//musg_l.FNameOC(:len_trim(musg_l.FNameOC))//'-----------------------------------------------------'
                call MUSG_ScanFileToLexicon(musg_l.iOC,musg_l)
                call MUSG_GetOutputTimesFromOC(musg_l)

            else if(index(line,'rch') .gt. 0) then
                call openMUSGFile('RCH',line,musg_l.Prefix,musg_l.iRCH,musg_l.FNameRCH)

                write(musg_l.iLEX,'(a)') 'Scanning: '//musg_l.FNameRCH(:len_trim(musg_l.FNameRCH))//'-----------------------------------------------------'
                call MUSG_ScanFileToLexicon(musg_l.iRCH,musg_l)

            else if(index(line,'riv') .gt. 0) then
                call openMUSGFile('RIV',line,musg_l.Prefix,musg_l.iRIV,musg_l.FNameRIV)

                write(musg_l.iLEX,'(a)') 'Scanning: '//musg_l.FNameRIV(:len_trim(musg_l.FNameRIV))//'-----------------------------------------------------'
                call MUSG_ScanFileToLexicon(musg_l.iRIV,musg_l)

            else if(index(line,'wel') .gt. 0) then
                call openMUSGFile('WEL',line,musg_l.Prefix,musg_l.iWEL,musg_l.FNameWEL)

                write(musg_l.iLEX,'(a)') 'Scanning: '//musg_l.FNameWEL(:len_trim(musg_l.FNameWEL))//'-----------------------------------------------------'
                call MUSG_ScanFileToLexicon(musg_l.iWEL,musg_l)
                
            else if(index(line,'chd') .gt. 0) then
                call openMUSGFile('CHD',line,musg_l.Prefix,musg_l.iCHD,musg_l.FNameCHD)

                write(musg_l.iLEX,'(a)') 'Scanning: '//musg_l.FNameCHD(:len_trim(musg_l.FNameCHD))//'-----------------------------------------------------'
                call MUSG_ScanFileToLexicon(musg_l.iCHD,musg_l)
                
            else if(index(line,'evt') .gt. 0) then
                call openMUSGFile('EVT',line,musg_l.Prefix,musg_l.iEVT,musg_l.FNameEVT)

                write(musg_l.iLEX,'(a)') 'Scanning: '//musg_l.FNameEVT(:len_trim(musg_l.FNameEVT))//'-----------------------------------------------------'
                call MUSG_ScanFileToLexicon(musg_l.iEVT,musg_l)


            else if(index(line,'drn') .gt. 0) then
                call openMUSGFile('DRN',line,musg_l.Prefix,musg_l.iDRN,musg_l.FNameDRN)

                write(musg_l.iLEX,'(a)') 'Scanning: '//musg_l.FNameDRN(:len_trim(musg_l.FNameDRN))//'-----------------------------------------------------'
                call MUSG_ScanFileToLexicon(musg_l.iDRN,musg_l)
 
            else if(index(line,'lpf') .gt. 0) then
                call openMUSGFile('LPF',line,musg_l.Prefix,musg_l.iLPF,musg_l.FNameLPF)

                write(musg_l.iLEX,'(a)') 'Scanning: '//musg_l.FNameLPF(:len_trim(musg_l.FNameLPF))//'-----------------------------------------------------'
                call MUSG_ScanFileToLexicon(musg_l.iLPF,musg_l)
                
                

            else if(index(line,'data(binary)') .gt. 0) then
                if(index(line,'.hds') .gt. 0) then
                    call openBinaryMUSGFile('HDS',line,musg_l.Prefix,musg_l.iHDS,musg_l.FNameHDS)
                    call MUSG_ReadBinaryHeadFile(musg_l)
                
                else if(index(line,'.ddn') .gt. 0) then
                    call openBinaryMUSGFile('DDN',line,musg_l.Prefix,musg_l.iDDN,musg_l.FNameDDN)
                    call MUSG_ReadBinaryDrawdownFile(musg_l)
                    call MUSG_GWF_ToTecplot(musg_l)
                    !call MUSG_CLN_ToTecplot(musg_l)

                else if(index(line,'.cbb') .gt. 0) then
                    call openBinaryMUSGFile('CBB',line,musg_l.Prefix,musg_l.iCBB,musg_l.FNameCBB)
                    open(musg_l.iCBB,file=musg_l.FNameCBB,status='old',form='binary',action='read')
                    call MUSG_ReadBinaryCbbFile(musg_l)
                    call MUSG_CBBToTecplot(musg_l)
                
                else if(index(line,'.cbcln') .gt. 0) then
                    call openBinaryMUSGFile('CBCCLN',line,musg_l.Prefix,musg_l.iCBCCLN,musg_l.FNameCBCCLN)
                    open(musg_l.iCBCCLN,file=musg_l.FNameCBCCLN,status='old',form='binary',action='read')
                    !call MUSG_ReadBinaryCBCCLNFile(musg_l)
                    !call MUSG_CBCCLNToTecplot(musg_l)
                endif

            else if(index(line,'cln') .gt. 0) then
                call openMUSGFile('CLN',line,musg_l.Prefix,musg_l.iCLN,musg_l.FNameCLN)

                write(musg_l.iLEX,'(a)') 'Scanning: '//musg_l.FNameCLN(:len_trim(musg_l.FNameCLN))//'-----------------------------------------------------'
                call MUSG_ScanFileToLexicon(musg_l.iCLN,musg_l)
                
            else if(index(line,'gnc') .gt. 0 .or. index(line,'gnc') .gt. 0) then
                call openMUSGFile('GNC',line,musg_l.Prefix,musg_l.iGNC,musg_l.FNameGNC)

                write(musg_l.iLEX,'(a)') 'Scanning: '//musg_l.FNameGNC(:len_trim(musg_l.FNameGNC))//'-----------------------------------------------------'
                call MUSG_ScanFileToLexicon(musg_l.iGNC,musg_l)
            
            else
                call Msg(' ')
                call Msg('---------------Not currently post-processing: '//line(:len_trim(line)))

            end if
        
        end do

        !open(musg_l.iLEX,file=musg_l.FNameLEX,status='unknown',form='formatted')
        !write(musg_l.iLEX,'(a)') 'Scan file from project '//musg_l.prefix(:len_trim(musg_l.prefix))
        write(musg_l.iLEX,'(a,i8,a)') 'Found ',musg_l.nKeyWord,' keywords'
        !do i=1,musg_l.nKeyWord
        !    write(musg_l.iLEX,'(a)',iostat=status) musg_l.KeyWord(i)
        !end do
        close(musg_l.iLEX)
        
   end subroutine MUSG_ProcessFiles
    

   
    subroutine openMUSGFile(FileType,line,prefix,iUnit,FName)
        implicit none
        
        character(*) :: FileType
        character(*) :: line
        character(*) :: prefix
        integer :: iUnit
        character(*) :: FName
        
        call Msg(' ')
        call Msg('---------------'//FileType(:len_trim(FileType))//' file postprocessing ')

        l1=index(line,Prefix(:len_trim(Prefix)))-1

        ! check for path string before prefix
        if(line(l1:l1) .eq. '/' .or. line(l1:l1) .eq. '\') then
            l1=l1-1
            do
                if(line(l1:l1) .eq. BLANK) exit
                l1=l1-1
            enddo   
        endif    
        FName=line(l1:)
        inquire(file=FName,exist=FileExists)
        if(.not. FileExists) then
            call ErrMsg('No file found: '//FName)
        endif
        call Msg('Modflow '//FileType(:len_trim(FileType))//' file: '//FName)
	    call getunit(iUnit)
        open(iUnit,file=FName,status='unknown',form='formatted')  
        
    end subroutine openMUSGFile
    
    subroutine openBinaryMUSGFile(FileType,line,prefix,iUnit,FName)
        implicit none
        
        character(*) :: FileType
        character(*) :: line
        character(*) :: prefix
        integer :: iUnit
        character(*) :: FName
        
        call Msg(' ')
        call Msg('---------------'//FileType(:len_trim(FileType))//' file postprocessing ')

        l1=index(line,Prefix(:len_trim(Prefix)))-1

        ! check for path string before prefix
        if(line(l1:l1) .eq. '/' .or. line(l1:l1) .eq. '\') then
            l1=l1-1
            do
                if(line(l1:l1) .eq. BLANK .or. line(l1:l1) .eq. TAB ) exit
                !write(*,*) ichar(line(l1:l1)), line(l1:l1)
                l1=l1-1
            enddo   
        endif    
        FName=line(l1+1:)
        inquire(file=FName,exist=FileExists)
        if(.not. FileExists) then
            call ErrMsg('No file found: '//FName)
        endif
        call Msg('Modflow '//FileType(:len_trim(FileType))//' file: '//FName)
	    call getunit(iUnit)
        open(iUnit,file=FName,status='old',form='binary',action='read')  
        
    end subroutine openBinaryMUSGFile

    
    subroutine MUSG_GetOutputTimesFromOC(musg_l)
        implicit none

        type (MUSG_Project) musg_l
        
        integer :: Fnum
        integer :: FNumStepPeriodTime
        character(MAXSTRING) :: FNameStepPeriodTime   
        integer :: ntime
        integer :: iTStep
        integer :: iPeriod
        integer :: thisStep
        integer :: thisPeriod
        real(dr) :: TotalTime
     
        character(MAXSTRING) :: line

     
        FNum=musg_l.iOC

        do 
            read(FNum,'(a)',iostat=status) line
            call lcase(line)
            if(status /= 0) exit
            
            if(line(1:1).eq.'#') then
                call Msg(line(:len_trim(line)))
                cycle
            endif

            if(index(line,'atsa nptimes').ne. 0)  then  ! read output time array from adaptive timestepping information
                call Msg('Use output time array from adaptive timestepping input')
                l1=index(line,'atsa nptimes')+12
                line=line(l1:)
            
                read(line,'(i5)') musg_l.ntime
                allocate(musg_l.totim(musg_l.ntime))

                read(FNum,*) (musg_l.totim(i),i=1,musg_l.ntime)
                write(TMPStr,'(i5)') musg_l.ntime 
                call Msg('Number of output times:'//TMPStr)
                do i=1,musg_l.ntime 
                    write(TMPStr,'(i5,f20.5)') i, musg_l.totim(i)
                    call Msg(TMPStr)
                end do
                return
                
            endif
        end do

        call Msg('No adaptive timestepping input')

        ! Not using ATSA so check for PERIOD STEP data 
        rewind(FNum)
        do 
            read(FNum,'(a)',iostat=status) line
            call lcase(line)
            if(status /= 0) exit
            
            if(line(1:1).eq.'#') then
                call Msg(line(:len_trim(line)))
                cycle
            endif
            
            if(index(line,'period').ne. 0)  then ! create output time array from KSTP data
                FNameStepPeriodTime=musg_l.MUTPrefix(:len_trim(musg_l.MUTPrefix))//'.StepPeriodTime'
                call OpenAscii(FNumStepPeriodTime,FNameStepPeriodTime)
                call Msg('Create output time array from PERIOD STEP data')
                call Msg('using output time data from file: '//trim(FNameStepPeriodTime))
                backspace(FNum)
                musg_l.ntime=0
                count: do
                    read(FNum,'(a)',iostat=status) line
                    call lcase(line)
                    if(status /= 0) exit
                    
                    if(index(line,'period').ne. 0)  musg_l.ntime=musg_l.ntime+1
                    
                end do count

                write(TMPStr,'(i5)') musg_l.ntime 
                call Msg('Number of output times:'//TMPStr)


                
                rewind(FNum)

                
                allocate(musg_l.totim(musg_l.ntime))
                ntime=0
                assign: do 
                    read(FNum,'(a)',iostat=status) line
                    call lcase(line)
                    if(status /= 0) exit assign
                    
                    if(index(line,'period').ne. 0)  then
                        line=line(index(line,'period')+7:)
                        read(line,*) thisPeriod
                        line=line(index(line,'step')+5:)
                        read(line,*) thisStep
                        
                        StepPeriodTime: do
                            read(FNumStepPeriodTime,*,iostat=status) iTStep, iPeriod, TotalTime
                            if(status /= 0) exit StepPeriodTime
                            
                            !if (iPeriod .eq. 1000) then
                            !    continue
                            !endif
                            
                            if(iTStep .eq. thisStep .and. iPeriod .eq.  thisPeriod) then
                                ntime=ntime+1
                                musg_l.totim(ntime)=TotalTime
                                write(TMPStr,'(i5,f20.5)') ntime, musg_l.totim(ntime)
                                call Msg(TMPStr)
                                cycle assign
                            endif
                            cycle 
                        end do StepPeriodTime
                        
                    endif
                end do assign
            endif
        end do

    
        if(.not. allocated(musg_l.totim)) then
            call ErrMsg('Could not find output time info in OC file')
        endif
            
            
    end subroutine MUSG_GetOutputTimesFromOC
        
    subroutine MUSG_ReadBinaryHeadFile(musg_l)
    ! borrowed from J. Doherty.
        implicit none
        
        integer :: FNum

        integer :: kstp,kper
        real     :: pertim
        real     :: totim
        integer :: ntime
        character*16   :: text
        integer  :: ilay        
        real     :: rtemp  
        integer  :: nstrt,nend,nstrt_kp,nument
        integer :: inode, ii

        
        type (MUSG_Project) musg_l

        !if(.not. musg_l.gwf.have_mesh) call ErrMsg('Must read mesh from GSF file first')
        
        allocate(musg_l.head(musg_l.gwf.nCell,musg_l.ntime))

        ! temporary fill of cln head array
        if(musg_l.cln.have_mesh) then
            call Msg('*** Temporarily set cln head array = 9.999')
            allocate(musg_l.clnhead(musg_l.cln.nCell,musg_l.ntime))
            musg_l.clnhead(:,:)=9.999
        end if

	    if(status /= 0) then
		    call ErrMsg('FILE ERROR: '//fname)
		    stop
        endif

        ntime=1
        FNum=musg_l.iHDS
        do
          read(FNum,err=9300,end=1000) kstp,kper,pertim,totim,text,nstrt,nend,ilay
          write(*,*) kstp,kper,pertim,totim,text,nstrt,nend,ilay
          
          if((nstrt.le.0).or.(nend.le.0).or.(ilay.le.0)) go to 9300
          if(index(text,'cln').ne.0)then
            nstrt_kp=nstrt
            nstrt=nend
            nend=nstrt_kp
            nument=nend-nstrt+1
            if(nument.le.0)then
              call msg('nument.le.0')
              continue
            end if
            read(FNum,err=9400,end=9400) (rtemp,ii=1,nument)
          else
              read(FNum,end=9400) (musg_l.head(inode,ntime),inode=nstrt,nend)
              !write(*,*) totim,nstrt,musg_l.head(nstrt,ntime)
          end if
          if(ilay==musg_l.gwf.nLay)then
              ntime=ntime+1 
          endif

        end do
9300    continue
9400    continue
1000    continue
    end subroutine MUSG_ReadBinaryHeadFile
    
    subroutine MUSG_ReadBinaryDrawdownFile(musg_l)
    ! borrowed from J. Doherty.
        implicit none

        integer :: Fnum

        integer  :: kstp,kper
        real     :: pertim
        real     :: totim
        integer :: ntime
        character*16   :: text
        integer  :: ilay        
        real     :: rtemp  
        integer  :: nstrt,nend,nstrt_kp,nument
        integer :: inode, ii

        
        type (MUSG_Project) musg_l

        allocate(musg_l.sat(musg_l.gwf.nCell,musg_l.ntime))
        
        ! temporary fill of cln sat array
        if(musg_l.cln.have_mesh) then
            call Msg('*** Temporarily set cln sat array = .999')
            allocate(musg_l.clnsat(musg_l.cln.nCell,musg_l.ntime))
            musg_l.clnsat(:,:)=0.999
        end if

        ntime=1
        FNum=musg_l.iDDN
        do
          read(FNum,err=9300,end=1000) kstp,kper,pertim,totim,text,nstrt,nend,ilay
          if((nstrt.le.0).or.(nend.le.0).or.(ilay.le.0)) go to 9300
          if(index(text,'cln').ne.0)then
            nstrt_kp=nstrt
            nstrt=nend
            nend=nstrt_kp
            nument=nend-nstrt+1
            if(nument.le.0)then
              call msg('nument.le.0')
              continue
            end if
            read(FNum,err=9400,end=9400) (rtemp,ii=1,nument)
          else
              read(FNum,err=9400,end=9400) (musg_l.sat(inode,ntime),inode=nstrt,nend)
              !write(*,*) totim,nstrt,musg_l.sat(nstrt,musg_l.ntime)
          end if
          if(ilay==musg_l.gwf.nLay)then
              ntime=ntime+1 
          endif

        end do
9300    continue
9400    continue
1000    continue
    end subroutine MUSG_ReadBinaryDrawdownFile

    
    subroutine MUSG_ReadBinaryCbbFile(musg_l)
    ! borrowed from J. Doherty.
        implicit none

        integer :: Fnum

        integer :: kstp,kper,NVAL,idum,ICODE
        character*16   :: text
        
        
        integer :: KSTP_last
        !real :: rmin
        !real :: rmax
        integer :: ntime

        
        type (MUSG_Project) musg_l

        ntime=0
        KSTP_last=0
        FNum=musg_l.iCBB
        do
            read(FNum,err=9300,end=1000) KSTP,KPER,TEXT,NVAL,idum,ICODE

            if(KSTP .ne. KSTP_last) then
                KSTP_last=KSTP
                ntime=ntime+1 
            endif

              
            if((NVAL.le.0)) go to 9300
            if(ICODE .gt. 0)then
                if(index(TEXT,'FLOW JA FACE') .ne. 0) then
                    if(ntime .eq.1) allocate(musg_l.cbb_ja(NVAL,musg_l.ntime))
                    read(FNum,err=9400,end=9400) (musg_l.cbb_ja(I,ntime),I=1,NVAL)
                    !rmax=-1e20
                    !rmin=1e20
                    !do i=1,nval
                    !    rmax=max(rmax,musg_l.cbb_ja(I,ntime))
                    !    rmin=min(rmin,musg_l.cbb_ja(I,ntime))
                    !enddo
                    !write(*,*) text
                    !write(*,*) nval
                    !write(*,*) rmin
                    !write(*,*) rmax
                else 
                    if(ntime .eq.1) THEN
                        if(index(text,'STORAGE') .ne.0) then
	                        allocate(musg_l.Cbb_STORAGE(NVAL,musg_l.ntime))
                        else if(index(text,'CONSTANT HEAD') .ne.0) then
	                        allocate(musg_l.Cbb_CONSTANT_HEAD(NVAL,musg_l.ntime))
                        else if(index(text,'RECHARGE') .ne.0) then
	                        allocate(musg_l.Cbb_RECHARGE(NVAL,musg_l.ntime))
                        else if(index(text,'DRAINS') .ne.0) then
	                        allocate(musg_l.Cbb_DRAINS(NVAL,musg_l.ntime))
                        end if
                    endif  

                    if(index(text,'STORAGE') .ne.0) then
                        read(FNum,err=9400,end=9400) (musg_l.Cbb_STORAGE(I,ntime),I=1,NVAL)
                        !rmax=-1e20
                        !rmin=1e20
                        !do i=1,nval
                        !    rmax=max(rmax,musg_l.Cbb_STORAGE(I,ntime))
                        !    rmin=min(rmin,musg_l.Cbb_STORAGE(I,ntime))
                        !enddo
                        !write(*,*) text
                        !write(*,*) nval
                        !write(*,*) rmin
                        !write(*,*) rmax
                    else if(index(text,'CONSTANT HEAD') .ne.0) then
                        read(FNum,err=9400,end=9400) (musg_l.Cbb_CONSTANT_HEAD(I,ntime),I=1,NVAL)
                        !rmax=-1e20
                        !rmin=1e20
                        !do i=1,nval
                        !    rmax=max(rmax,musg_l.Cbb_CONSTANT_HEAD(I,ntime))
                        !    rmin=min(rmin,musg_l.Cbb_CONSTANT_HEAD(I,ntime))
                        !enddo
                        !write(*,*) text
                        !write(*,*) nval
                        !write(*,*) rmin
                        !write(*,*) rmax
                    else if(index(text,'RECHARGE') .ne.0) then
                        read(FNum,err=9400,end=9400) (musg_l.Cbb_RECHARGE(I,ntime),I=1,NVAL)
                        !rmax=-1e20
                        !rmin=1e20
                        !do i=1,nval
                        !    rmax=max(rmax,musg_l.Cbb_RECHARGE(I,ntime))
                        !    rmin=min(rmin,musg_l.Cbb_RECHARGE(I,ntime))
                        !enddo
                        !write(*,*) text
                        !write(*,*) nval
                        !write(*,*) rmin
                        !write(*,*) rmax
                    else if(index(text,'DRAINS') .ne.0) then
                        read(FNum,err=9400,end=9400) (musg_l.Cbb_DRAINS(I,ntime),I=1,NVAL)
                        !rmax=-1e20
                        !rmin=1e20
                        !do i=1,nval
                        !    rmax=max(rmax,musg_l.Cbb_DRAINS(I,ntime))
                        !    rmin=min(rmin,musg_l.Cbb_DRAINS(I,ntime))
                        !enddo
                        !write(*,*) text
                        !write(*,*) nval
                        !write(*,*) rmin
                        !write(*,*) rmax
                    end if
                  
                  
                endif
            else if(ICODE .eq. -1) then
                      call Msg( 'The budget data is written in the compact budget style.')
                      call Msg( 'Not supported yet.')
                      stop
            end if



        end do
9300    continue
9400    continue
1000    continue
 
    end subroutine MUSG_ReadBinaryCbbFile

   
    subroutine MUSG_SetOptionsFromBAS6(musg_l)
        implicit none

        type (MUSG_Project) musg_l
        
        integer :: Fnum
        
     
        character(4000) :: line

     
        FNum=musg_l.iBAS6

        do 
            read(FNum,'(a)',iostat=status) line
            call lcase(line)
            if(status /= 0) return
            
            if(line(1:1).eq.'#') then
                call Msg(line(:len_trim(line)))
                cycle
            endif
            
            if(index(line,'unstructured').ne. 0)  musg_l.UNSTRUCTURED=.true.
            if(musg_l.UNSTRUCTURED) then
                call Msg('Using UNSTRUCTURED grid')
            else
                call Msg('Using STRUCTURED grid')
            endif

            if(index(line,'free').ne. 0)  musg_l.FREE=.true.  
             if(musg_l.FREE) then
                call Msg('Using FREE file format')
            else
                call Msg('Using FIXED file format')
            endif
           
            
            if(index(line,'richards').ne. 0)  musg_l.RICHARDS=.true.
             if(musg_l.RICHARDS) then
                call Msg('Applying RICHARDS equation for variably-saturated flow')
            else
                call Msg('Applying CONFINED/UNCONFINED approach')
            endif
            
            exit
        end do

            
    end subroutine MUSG_SetOptionsFromBAS6
        
    subroutine MUSG_WriteVolumeBudgetToTecplot(musg_l)
        implicit none

        type (MUSG_Project) musg_l
        
        integer :: i

        integer :: Fnum
        integer :: FnumTecplot
        character(MAXLBL) :: FNameTecplot
        
        
        character(20) :: Varname(100)
        real(dr) :: VarNumRate(100)
        Real(dr) :: VarNumCumulative(100)
        logical :: DoVars
        integer :: bline
        logical :: InSection
        real(dr) :: TotalTime
        real(dr) :: dum1, dum2, dum3, dum4
        
        character(4000) :: var_line
        character(4000) :: output_line
        character(4000) :: line

        

        FNameTecplot=musg_l.MUTPrefix(:len_trim(musg_l.MUTPrefix))//'.'//musg_l.Prefix(:len_trim(musg_l.Prefix))//'.VolumeBudget.tecplot.dat'
        call OpenAscii(FNumTecplot,FNameTecplot)
        call Msg( 'To File: '//trim(FNameTecplot))

        write(FNumTecplot,*) 'Title = "Modflow Volume Budget"'

        DoVars=.true.
        
        FNum=musg_l.iLIST

        do 
            read(FNum,'(a)',iostat=status) line
            if(status /= 0) return
            
            if(index(line,'MODEL TIME UNIT IS').gt.0) then
                l1=index(line,'MODEL TIME UNIT IS')
                musg_l.Tunits=line(l1+19:)
                var_line='VARIABLES = "TOTAL TIME'//'('//trim(adjustl(musg_l.Tunits))//')",'

                continue
                
            else if(index(line,'MODEL LENGTH UNIT IS').gt.0) then
                l1=index(line,'MODEL LENGTH UNIT IS')
                musg_l.Lunits=line(l1+21:)
                

                continue
            else if(index(line,'VOLUMETRIC BUDGET FOR ENTIRE MODEL AT END OF TIME STEP').gt.0) then 
                bline=0  
                InSection=.true.
                
                do  ! find start of budget data                 
                    read(FNum,'(a)',iostat=status) line
                    if(status /= 0) return
                
                    if(index(line,'           ---                                      ---').gt.0) exit
                end do
                
                do  ! read IN section
                    read(FNum,'(a)',iostat=status) line
                    if(status /= 0) return
                    
                    if(index(line,'OUT:') > 0) exit
                                            
                    if(index(line,'=') .gt. 0) then   ! read this line of budget data
                        bline=bline+1
                        if(DoVars) then
                            l1=index(line,'=')
                            VarName(Bline)='IN_'//trim(adjustl(line(:l1-2)))
                            l1=len_trim(var_line)
                            var_line=var_line(:l1)//'"'//trim(VarName(Bline))//'",'

                        endif
                                   
                        l1=index(line,'=')
                        read(line(l1+1:),*) VarNumCumulative(Bline)
                                    
                        line=line(l1+1:)
                        l1=index(line,'=')
                        read(line(l1+1:),*) VarNumRate(Bline)
                           
                                  
                    endif

                end do

                do  ! read OUT section
                    read(FNum,'(a)',iostat=status) line
                    if(status /= 0) return
                    
                    if(index(line,'TOTAL OUT =') > 0) then
                        backspace(FNum)
                        exit
                    endif
                                            
                    if(index(line,'=') .gt. 0) then   ! read this line of budget data
                        bline=bline+1
                        if(DoVars) then
                            l1=index(line,'=')
                            VarName(Bline)='OUT_'//trim(adjustl(line(:l1-2)))
                            l1=len_trim(var_line)
                            var_line=var_line(:l1)//'"'//trim(VarName(Bline))//'",'

                        endif
                                   
                        l1=index(line,'=')
                        read(line(l1+1:),*) VarNumCumulative(Bline)
                                    
                        line=line(l1+1:)
                        l1=index(line,'=')
                        read(line(l1+1:),*) VarNumRate(Bline)
                           
                                  
                    endif

                end do
                
                do  ! read to the end of the budget data
                    read(FNum,'(a)',iostat=status) line
                    if(status /= 0) return
                    
                    if(index(line,'TOTAL TIME').gt.0) then
                        l1=index(line,'TOTAL TIME')
                        if(index(musg_l.Tunits,'SECONDS').gt.0) then
                            read(line(l1+10:),*) TotalTime
                        elseif(index(musg_l.Tunits,'MINUTES').gt.0) then
                            read(line(l1+10:),*) dum1, TotalTime
                        elseif(index(musg_l.Tunits,'HOURS').gt.0) then
                            read(line(l1+10:),*) dum1, dum2, TotalTime
                        elseif(index(musg_l.Tunits,'DAYS').gt.0) then
                            read(line(l1+10:),*) dum1, dum2, dum3, TotalTime
                        elseif(index(musg_l.Tunits,'YEARS').gt.0) then
                            read(line(l1+10:),*) dum1, dum2, dum3, dum4, TotalTime
                        endif
                        exit
                        
                    else if(index(line,'=') .gt. 0) then   ! read this line of budget data
                        bline=bline+1
                        if(DoVars) then
                            l1=index(line,'=')
                            VarName(Bline)=trim(adjustl(line(:l1-2)))
                            l1=len_trim(var_line)
                            var_line=var_line(:l1)//'"'//trim(VarName(Bline))//'",'

                        endif
                                   
                        l1=index(line,'=')
                        read(line(l1+1:),*) VarNumCumulative(Bline)
                                    
                        line=line(l1+1:)
                        l1=index(line,'=')
                        read(line(l1+1:),*) VarNumRate(Bline)
                           
                    
                     endif
                end do
             
                if(DoVars) then
                    l1=len_trim(var_line)
                    write(FNumTecplot,'(a)') var_line(:l1-1)
                    write(output_line,'(a)')  'zone t="'//trim(musg_l.FNameLIST)//'"'
 
                    TMPStr=', AUXDATA TimeUnits = "'//musg_l.Tunits(:len_trim(musg_l.Tunits))//'"'
                    l1=len_trim(output_line)+1
                    write(output_line(l1:),'(a)')	TMPStr                 

                    TMPStr=', AUXDATA LengthUnits = "'//musg_l.Lunits(:len_trim(musg_l.Lunits))//'"'
                    l1=len_trim(output_line)+1
                    write(output_line(l1:),'(a)')	TMPStr                 
                    
                    write(FNumTecplot,'(a)') output_line 

                    
                    DoVars=.false.
                endif
                
                write(output_line,'(f20.4)') TotalTime
                do i=1,bline
                    l1=len_trim(output_line)+1
                    write(output_line(l1:),'(f20.4)')	VarNumRate(i)
                end do

                write(FNumTecplot,'(a)') output_line

            endif
        end do 
    end subroutine MUSG_WriteVolumeBudgetToTecplot
    
    subroutine MUSG_CreateStepPeriodTimeFile(musg_l)
        implicit none

        type (MUSG_Project) musg_l
        
        integer :: Fnum
        integer :: FNumStepPeriodTime
        character(MAXLBL) :: FNameStepPeriodTime
        
        
        integer :: iTStep
        integer :: iPeriod
        real(dr) :: TotalTime
        real(dr) :: dum1, dum2, dum3, dum4
        
        character(4000) :: line

        FNum=musg_l.iLIST
        rewind(FNum)

        FNameStepPeriodTime=musg_l.MUTPrefix(:len_trim(musg_l.MUTPrefix))//'.StepPeriodTime'
        call OpenAscii(FNumStepPeriodTime,FNameStepPeriodTime)
        call Msg( 'Time step, stress period, time to file: '//trim(FNameStepPeriodTime))

        do 
            read(FNum,'(a)',iostat=status) line
            if(status /= 0) return
            
            if(index(line,'TIME SUMMARY AT END OF TIME STEP').gt.0) then
                line=line(index(line,'STEP')+5:)
                read(line,*) iTStep
                line=line(index(line,'PERIOD')+7:)
                read(line,*) iPeriod
                
                loop: do
                    read(FNum,'(a)',iostat=status) line
                    if(status /= 0) return
                    if(index(line,'TOTAL TIME').gt.0) then
                        l1=index(line,'TOTAL TIME')
                        if(index(musg_l.Tunits,'SECONDS').gt.0) then
                            read(line(l1+10:),*) TotalTime
                        elseif(index(musg_l.Tunits,'MINUTES').gt.0) then
                            read(line(l1+10:),*) dum1, TotalTime
                        elseif(index(musg_l.Tunits,'HOURS').gt.0) then
                            read(line(l1+10:),*) dum1, dum2, TotalTime
                        elseif(index(musg_l.Tunits,'DAYS').gt.0) then
                            read(line(l1+10:),*) dum1, dum2, dum3, TotalTime
                        elseif(index(musg_l.Tunits,'YEARS').gt.0) then
                            read(line(l1+10:),*) dum1, dum2, dum3, dum4, TotalTime
                        endif
                        exit loop
                    endif
                end do loop
                
                write(FNumStepPeriodTime,*) iTStep, iPeriod, TotalTime

            end if
                

        end do
                
    end subroutine MUSG_CreateStepPeriodTimeFile
    
    subroutine MUSG_GWF_ToTecplot(musg_l)
        implicit none
        type (MUSG_Project) musg_l

        integer :: Fnum
        character(MAXLBL) :: FName
        integer :: i, j

        if(.not. musg_l.gwf.have_mesh) then
		    call ErrMsg('ERROR: no mesh information')
		    stop
        endif
        
        ! tecplot output file
        FName=musg_l.MUTPrefix(:len_trim(musg_l.MUTPrefix))//'.'//musg_l.Prefix(:len_trim(musg_l.Prefix))//'.HDS_DDN.tecplot.dat'
        call OpenAscii(FNum,FName)
        call Msg( 'To File: '//trim(FName))

        write(FNum,*) 'Title = "Modflow HDS and DDN (Saturation) file Outputs "'

        write(FNum,'(a)') 'variables="X","Y","Z","Layer","Hydraulic Head","Saturation"'
        
        write(FNum,'(a,f20.4,a,i8,a,i8,a)')'ZONE t="GWF" SOLUTIONTIME=',musg_l.totim(1),',N=',musg_l.gwf.nvertex,', E=',musg_l.gwf.nCell,', datapacking=block, &
            zonetype=febrick, VARLOCATION=([4,5,6]=CELLCENTERED)'

        write(FNum,'(a)') '# x'
        write(FNum,'(5e20.12)') (musg_l.gwf.x(i),i=1,musg_l.gwf.nvertex)
        write(FNum,'(a)') '# y'
        write(FNum,'(5e20.12)') (musg_l.gwf.y(i),i=1,musg_l.gwf.nvertex)
        write(FNum,'(a)') '# z'
        write(FNum,'(5e20.12)') (musg_l.gwf.z(i),i=1,musg_l.gwf.nvertex)
        write(FNum,'(a)') '# layer'
        write(FNum,'(5i8)') (musg_l.gwf.lay(i),i=1,musg_l.gwf.nCell)
        write(FNum,'(a)') '# head'
        write(FNum,'(5e20.12)') (musg_l.head(i,1),i=1,musg_l.gwf.nCell)
        write(FNum,'(a)') '# saturation'
        write(FNum,'(5e20.12)') (musg_l.sat(i,1),i=1,musg_l.gwf.nCell)
        
        do i=1,musg_l.gwf.nCell
            write(FNum,'(8i8)') (musg_l.gwf.ivertex(j,i),j=1,musg_l.gwf.m)
        end do
       
        
        do j=2,musg_l.ntime
            write(FNum,'(a,f20.4,a,i8,a,i8,a)')'ZONE t="GWF" SOLUTIONTIME=',musg_l.totim(j),',N=',musg_l.gwf.nvertex,', E=',musg_l.gwf.nCell,', datapacking=block, &
            zonetype=febrick, VARLOCATION=([4,5,6]=CELLCENTERED), VARSHARELIST=([1,2,3,4,]), CONNECTIVITYSHAREZONE=1 '
            write(FNum,'(a)') '# head'
            write(FNum,'(5e20.12)') (musg_l.head(i,j),i=1,musg_l.gwf.nCell)
            write(FNum,'(a)') '# sat'
            write(FNum,'(5e20.12)') (musg_l.sat(i,j),i=1,musg_l.gwf.nCell)
        enddo
        
        call FreeUnit(FNum)

    end subroutine MUSG_GWF_ToTecplot

    subroutine MUSG_CLN_ToTecplot(musg_l)
        implicit none
        type (MUSG_Project) musg_l

        integer :: Fnum
        character(MAXLBL) :: FName
        integer :: i, j

        if(.not. musg_l.cln.have_mesh) then
		    call ErrMsg('ERROR: no mesh information')
		    stop
        endif
        
        ! tecplot output file
        FName=musg_l.MUTPrefix(:len_trim(musg_l.MUTPrefix))//'.'//musg_l.Prefix(:len_trim(musg_l.Prefix))//'.cln.HDS_DDN.tecplot.dat'
        call OpenAscii(FNum,FName)
        call Msg( 'To File: '//trim(FName))

        write(FNum,*) 'Title = "'//musg_l.Prefix(:len_trim(musg_l.Prefix))//': CLN Heads and drawdown (saturation)"'

        write(FNum,'(a)') 'variables="X","Y","Z","Layer","Hydraulic Head","Saturation"'
        
        write(FNum,'(a,f20.4,a,i8,a,i8,a)')'ZONE t="CLN" SOLUTIONTIME=',musg_l.totim(1),',N=',musg_l.cln.nvertex,', E=',musg_l.cln.nCell,', datapacking=block, &
            zonetype=febrick, VARLOCATION=([4,5,6]=CELLCENTERED)'

        write(FNum,'(a)') '# x'
        write(FNum,'(5e20.12)') (musg_l.cln.x(i),i=1,musg_l.cln.nvertex)
        write(FNum,'(a)') '# y'
        write(FNum,'(5e20.12)') (musg_l.cln.y(i),i=1,musg_l.cln.nvertex)
        write(FNum,'(a)') '# z'
        write(FNum,'(5e20.12)') (musg_l.cln.z(i),i=1,musg_l.cln.nvertex)
        write(FNum,'(a)') '# layer'
        write(FNum,'(5i8)') (musg_l.cln.lay(i),i=1,musg_l.cln.nCell)
        write(FNum,'(a)') '# head'
        write(FNum,'(5e20.12)') (musg_l.clnhead(i,1),i=1,musg_l.cln.nCell)
        write(FNum,'(a)') '# saturation'
        write(FNum,'(5e20.12)') (musg_l.clnsat(i,1),i=1,musg_l.cln.nCell)
        
        do i=1,musg_l.cln.nCell
            write(FNum,'(8i8)') (musg_l.cln.ivertex(j,i),j=1,musg_l.cln.m)
        end do
       
        
        do j=2,musg_l.ntime
            write(FNum,'(a,f20.4,a,i8,a,i8,a)')'ZONE t="CLN" SOLUTIONTIME=',musg_l.totim(j),',N=',musg_l.cln.nvertex,', E=',musg_l.cln.nCell,', datapacking=block, &
            zonetype=febrick, VARLOCATION=([4,5,6]=CELLCENTERED), VARSHARELIST=([1,2,3,4,]), CONNECTIVITYSHAREZONE=1 '
            write(FNum,'(a)') '# head'
            write(FNum,'(5e20.12)') (musg_l.head(i,j),i=1,musg_l.cln.nCell)
            write(FNum,'(a)') '# sat'
            write(FNum,'(5e20.12)') (musg_l.sat(i,j),i=1,musg_l.cln.nCell)
        enddo
        
        call FreeUnit(FNum)

    end subroutine MUSG_CLN_ToTecplot
    
    subroutine MUSG_CBBToTecplot(musg_l)
        implicit none
        type (MUSG_Project) musg_l

        integer :: Fnum
        character(MAXLBL) :: FName

        integer :: i, j, nvar

        if(.not. musg_l.gwf.have_mesh) then
		    call ErrMsg('ERROR: no mesh information')
		    stop
        endif
        
        ! tecplot output file
        FName=musg_l.MUTPrefix(:len_trim(musg_l.MUTPrefix))//'.'//musg_l.Prefix(:len_trim(musg_l.Prefix))//'.CBB.tecplot.dat'
        call OpenAscii(FNum,FName)
        call Msg( 'To File: '//trim(FName))

        write(FNum,*) 'Title = "Modflow CBB file Outputs "'

        VarSTR='variables="X","Y","Z","Layer",'
        nVar=4
        if(allocated(musg_l.Cbb_STORAGE)) then
            VarSTR=VarSTR(:len_trim(VarSTR))//'"Storage",'
            nVar=nVar+1
        endif
        if(allocated(musg_l.Cbb_CONSTANT_HEAD)) then
            VarSTR=VarSTR(:len_trim(VarSTR))//'"Constant head",'
            nVar=nVar+1
        endif
        if(allocated(musg_l.Cbb_DRAINS)) then
            VarSTR=VarSTR(:len_trim(VarSTR))//'"Drains",'
            nVar=nVar+1
        endif
        if(allocated(musg_l.Cbb_RECHARGE)) then
            VarSTR=VarSTR(:len_trim(VarSTR))//'"Recharge",'
            nVar=nVar+1
        endif
        
        write(FNum,'(a)') VarSTR(:len_trim(VarSTR))
            
        write(ZoneSTR,'(a,f20.4,a,i8,a,i8,a)')'ZONE t="CBB" SOLUTIONTIME=',musg_l.totim(1),',N=',musg_l.gwf.nvertex,', E=',musg_l.gwf.nCell,', datapacking=block, &
            zonetype=febrick'
        
        CellCenteredSTR=', VARLOCATION=([4'
        if(nVar.ge.5) then
            do j=5,nVar
                write(str1,'(i1)') j
                CellCenteredSTR=CellCenteredSTR(:len_trim(CellCenteredSTR))//','//str1
            end do
        endif
        CellCenteredSTR=CellCenteredSTR(:len_trim(CellCenteredSTR))//']=CELLCENTERED)'

        write(FNum,'(a)') ZoneSTR(:len_trim(ZoneSTR))//CellCenteredSTR(:len_trim(CellCenteredSTR))

        write(FNum,'(a)') '# x'
        write(FNum,'(5e20.12)') (musg_l.gwf.x(i),i=1,musg_l.gwf.nvertex)
        write(FNum,'(a)') '# y'
        write(FNum,'(5e20.12)') (musg_l.gwf.y(i),i=1,musg_l.gwf.nvertex)
        write(FNum,'(a)') '# z'
        write(FNum,'(5e20.12)') (musg_l.gwf.z(i),i=1,musg_l.gwf.nvertex)
        write(FNum,'(a)') '# layer'
        write(FNum,'(5i8)') (musg_l.gwf.lay(i),i=1,musg_l.gwf.nCell)
        if(allocated(musg_l.Cbb_STORAGE)) then
            write(FNum,'(a)') '# storage'
            write(FNum,'(5e20.12)') (musg_l.Cbb_STORAGE(i,1),i=1,musg_l.gwf.nCell)
        endif
        if(allocated(musg_l.Cbb_CONSTANT_HEAD)) then
            write(FNum,'(a)') '# constant head'
            write(FNum,'(5e20.12)') (musg_l.Cbb_CONSTANT_HEAD(i,1),i=1,musg_l.gwf.nCell)
        endif        
        if(allocated(musg_l.Cbb_DRAINS)) then
            write(FNum,'(a)') '# drains'
            write(FNum,'(5e20.12)') (musg_l.Cbb_DRAINS(i,1),i=1,musg_l.gwf.nCell)
        endif        
        if(allocated(musg_l.Cbb_RECHARGE)) then
            write(FNum,'(a)') '# recharge'
            write(FNum,'(5e20.12)') (musg_l.Cbb_RECHARGE(i,1),i=1,musg_l.gwf.nCell)
        endif        
        
        do i=1,musg_l.gwf.nCell
            write(FNum,'(8i8)') (musg_l.gwf.ivertex(j,i),j=1,musg_l.gwf.m)
        end do
       
        do j=2,musg_l.ntime
            write(FNum,'(a,f20.4,a,i8,a,i8,a)')'ZONE t="CBB" SOLUTIONTIME=',musg_l.totim(j),',N=',musg_l.gwf.nvertex,', E=',musg_l.gwf.nCell,', datapacking=block, &
            zonetype=febrick,'//CellCenteredSTR(:len_trim(CellCenteredSTR))//', VARSHARELIST=([1,2,3,4,]), CONNECTIVITYSHAREZONE=1 '
            if(allocated(musg_l.Cbb_STORAGE)) then
                write(FNum,'(a)') '# storage'
                write(FNum,'(5e20.12)') (musg_l.Cbb_STORAGE(i,j),i=1,musg_l.gwf.nCell)
            endif
            if(allocated(musg_l.Cbb_CONSTANT_HEAD)) then
                write(FNum,'(a)') '# constant head'
                write(FNum,'(5e20.12)') (musg_l.Cbb_CONSTANT_HEAD(i,j),i=1,musg_l.gwf.nCell)
            endif        
            if(allocated(musg_l.Cbb_DRAINS)) then
                write(FNum,'(a)') '# drains'
                write(FNum,'(5e20.12)') (musg_l.Cbb_DRAINS(i,j),i=1,musg_l.gwf.nCell)
            endif        
            if(allocated(musg_l.Cbb_RECHARGE)) then
                write(FNum,'(a)') '# recharge'
                write(FNum,'(5e20.12)') (musg_l.Cbb_RECHARGE(i,j),i=1,musg_l.gwf.nCell)
            endif        
        enddo
        
        call FreeUnit(FNum)

    end subroutine MUSG_CBBToTecplot

   
   

    subroutine MUSG_ReadGSF(musg_l)
        implicit none

        type (MUSG_Project) musg_l

        integer :: i, j
        
        integer :: i1, i2
        
        itmp=musg_l.iGSF
        
        ! read initial comment lines beginning with #
        do 
            read(itmp,'(a)') line
            if(line(1:1).eq.'#') then
                write(*,'(a)') line
                cycle
            endif
            backspace(itmp)
            exit
        end do

        read(itmp,*) musg_l.gwf.meshtype
        read(itmp,*) musg_l.gwf.nCell, musg_l.gwf.nLay, musg_l.gwf.iz, musg_l.gwf.ic
        read(itmp,*) musg_l.gwf.nvertex
        allocate(musg_l.gwf.x(musg_l.gwf.nvertex),musg_l.gwf.y(musg_l.gwf.nvertex),musg_l.gwf.z(musg_l.gwf.nvertex), stat=ialloc)
        call AllocChk(ialloc,'gwf xyzvertex arrays')
        musg_l.gwf.x = 0 ! automatic initialization
        musg_l.gwf.y = 0 ! automatic initialization
        musg_l.gwf.z = 0 ! automatic initialization
        
        read(itmp,*) (musg_l.gwf.x(i),musg_l.gwf.y(i),musg_l.gwf.z(i),i=1,musg_l.gwf.nvertex)

        musg_l.gwf.m=8  ! hardwired for octree meshes 
        allocate(musg_l.gwf.ivertex(musg_l.gwf.m,musg_l.gwf.nCell),stat=ialloc)
        allocate(musg_l.gwf.xCell(musg_l.gwf.nCell),musg_l.gwf.yCell(musg_l.gwf.nCell),musg_l.gwf.zCell(musg_l.gwf.nCell),musg_l.gwf.lay(musg_l.gwf.nCell),stat=ialloc)
        call AllocChk(ialloc,'gwf ivertex, xyzcell arrays')
        musg_l.gwf.ivertex = 0 ! automatic initialization
        do i=1,musg_l.gwf.nCell
        read(itmp,*) i1,musg_l.gwf.xCell(i),musg_l.gwf.yCell(i),musg_l.gwf.zCell(i),musg_l.gwf.lay(i),i2,(musg_l.gwf.ivertex(j,i),j=1,musg_l.gwf.m)
        end do
	    call freeunit(itmp)
        
        musg_l.gwf.have_mesh=.true.

	    return
    end subroutine MUSG_ReadGSF
    
    subroutine MUSG_ReadCLNGSF(musg_l)
        implicit none

        type (MUSG_Project) musg_l

        integer :: i, j
        
        integer :: i1, i2
        
        itmp=musg_l.iCLNGSF
        
        ! read initial comment lines beginning with #
        do 
            read(itmp,'(a)') line
            if(line(1:1).eq.'#') then
                write(*,'(a)') line
                cycle
            endif
            backspace(itmp)
            exit
        end do

        read(itmp,*) musg_l.cln.meshtype
        read(itmp,*) musg_l.cln.nCell, musg_l.cln.nLay, musg_l.cln.iz, musg_l.cln.ic
        read(itmp,*) musg_l.cln.nvertex
        allocate(musg_l.cln.x(musg_l.cln.nvertex),musg_l.cln.y(musg_l.cln.nvertex),musg_l.cln.z(musg_l.cln.nvertex), stat=ialloc)
        call AllocChk(ialloc,'cln xyzvertex arrays')
        musg_l.cln.x = 0 ! automatic initialization
        musg_l.cln.y = 0 ! automatic initialization
        musg_l.cln.z = 0 ! automatic initialization
        
        read(itmp,*) (musg_l.cln.x(i),musg_l.cln.y(i),musg_l.cln.z(i),i=1,musg_l.cln.nvertex)

        musg_l.cln.m=2  ! correct for cln segments ?
        allocate(musg_l.cln.ivertex(musg_l.cln.m,musg_l.cln.nCell),stat=ialloc)
        allocate(musg_l.cln.xCell(musg_l.cln.nCell),musg_l.cln.yCell(musg_l.cln.nCell),musg_l.cln.zCell(musg_l.cln.nCell),musg_l.cln.lay(musg_l.cln.nCell),stat=ialloc)
        call AllocChk(ialloc,'cln ivertex, xyzcell arrays')
        musg_l.cln.ivertex = 0 ! automatic initialization
        do i=1,musg_l.cln.nCell
        read(itmp,*) i1,musg_l.cln.xCell(i),musg_l.cln.yCell(i),musg_l.cln.zCell(i),musg_l.cln.lay(i),i2,(musg_l.cln.ivertex(j,i),j=1,musg_l.cln.m)
        end do
	    call freeunit(itmp)
        
        musg_l.cln.have_mesh=.true.

	    return
    end subroutine MUSG_ReadCLNGSF
   ! subroutine MUSG_ReadCLN(musg_l)
   !     implicit none
   !
   !     type (MUSG_Project) musg_l
   !
   !     integer :: i, j
   !     
   !     integer :: i1, i2, iDum
   !     
   !     itmp=musg_l.iCLN
   !     
   !     ! read initial comment lines beginning with #
   !     do 
   !         read(itmp,'(a)') line
   !         if(line(1:1).eq.'#') then
   !             write(*,'(a)') line
   !             cycle
   !         endif
   !         backspace(itmp)
   !         exit
   !     end do
   !
   !     read(itmp,*) iDum,musg_l.gwf.NCLN
   !     read(itmp,'(a)') line
   !     read(itmp,*) iDum,musg_l.gwf.NCLN
   !     
   !
	  !  return
   !end subroutine MUSG_ReadCLN

   
    subroutine MUSG_ScanFileToLexicon(FNum,musg_l)
        implicit none

        type (MUSG_Project) musg_l
        
        integer :: Fnum
     
        character(MAXSTRING) :: line
        character(MAXSTRING) :: PossibleKey
        
        !integer :: ISTART, ISTOP, ICOL
        !integer :: N
        !real :: R
        !integer IOUT
        
        !return
    
        do 
            read(FNum,'(a)',iostat=status) line
            call lcase(line)
            if(status /= 0) exit
            
            if(line(1:1).eq.'#') then
                call Msg(line(:len_trim(line)))
                cycle
            endif
            
            do i=1,len_trim(line)
                !write(*,*) i,ichar(line(i:i))
                if(ichar(line(i:i)) .ge. 42 .and. ichar(line(i:i)) .le. 57 .or. ichar(line(i:i)) .eq. 32) cycle
                if(line(i:i) .eq. 'e' .or.  &
                    line(i:i) .eq. 'd' .or.   &
                    line(i:i) .eq. 'g') then
                    if(line(i+1:i+1) .eq. '+' .or. line(i+1:i+1) .eq. '-') cycle
                endif
                if(i.eq.1) then
                    PossibleKey=line(i:)
                else
                    PossibleKey=line(i-1:)
                endif
                call AddToLexicon(PossibleKey, musg_l)
                exit
            end do
        end do
        
        rewind(FNum)    
            
    end subroutine MUSG_ScanFileToLexicon

    subroutine AddToLexicon(PKey, musg_l)
        implicit none
        
        
        type (MUSG_Project) musg_l

        character(*) :: PKey
        
        !if(musg_l.nKeyWord .gt. 0) then
        !    if(musg_l.Keyword(musg_l.nKeyWord) .eq. PKey) then
        !        !write(musg_l.iLEX,'(a)',iostat=status) 'Repeat: '// PKey(:len_trim(PKey))
        !        return
        !    end if
        !end if
        
        musg_l.nKeyWord=musg_l.nKeyWord+1
        if(musg_l.nKeyWord>musg_l.nDim) call GrowKeywordArray(musg_l,musg_l.nDim)
        musg_l.Keyword(musg_l.nKeyWord)=PKey
        write(musg_l.iLEX,'(a)',iostat=status) musg_l.Keyword(musg_l.nKeyWord)

        
        
    end subroutine AddToLexicon
    
     subroutine GrowKeyWordArray(musg_l,ndim) !--- during run if necessary 
        type (MUSG_Project) musg_l
	    real, parameter :: nf_mult=2
	    integer :: ndim_new
	    integer :: ndim,i
	    character(MAXSTRING), allocatable :: KeyWord_tmp(:) 

	    ndim_new=nint(ndim*nf_mult)
        write(*,*) 'ndim_new ', ndim_new

	    allocate(Keyword_tmp(ndim_new), stat=ialloc)
	    call AllocChk(ialloc,'allocate Keyword_tmp arrays')
	    Keyword_tmp(:)=char(0)

	    ! copy current data
	    do i=1,ndim
		    Keyword_tmp(i)	=	musg_l.Keyword(i)
	    end do

	    ! destroy arrays
	    deallocate(musg_l.Keyword)
	    ! reallocate
	    allocate(musg_l.Keyword(ndim_new), stat=ialloc)
	    call AllocChk(ialloc,'reallocate musg_l.Keyword arrays')
	    musg_l.Keyword(:)=char(0)

	    ! copy current data
	    do i=1,ndim
		    musg_l.Keyword(i)	=	Keyword_tmp(i)	
	    end do

	    ndim=ndim_new
	    
	    deallocate(Keyword_tmp)

    end subroutine GrowKeyWordArray
   
    
! -------------------------------------------------------------------------------------------   
   subroutine MUSG_ReadAsciiHeadFile(FnumTG,musg_l)
        implicit none

        integer :: j
        integer :: FnumTG
        integer :: Fnum
        character(MAXLBL) :: FName
        character(MAXSTRING) :: line
        integer :: i1

        type (MUSG_Project) musg_l

        read(FnumTG,'(a)') FName
        call OpenAscii(FNum,FName)
        call Msg( 'Head file: '//FName)

        allocate(musg_l.head(musg_l.gwf.nCell,1))

	    if(status /= 0) then
		    call ErrMsg('FILE ERROR: '//fname)
		    stop
        endif
        read(itmp,*) line
	    read(itmp,*) (i1, musg_l.head(j,1),j=1,musg_l.gwf.nCell)
	    call freeunit(FNum)

        continue

    end subroutine MUSG_ReadAsciiHeadFile
    

    
    
    subroutine MUSG_ReadAsciiKxFile(FnumTG,musg_l)
        implicit none

        integer :: j,m
        integer :: FnumTG
        integer :: Fnum
        character(MAXLBL) :: FName
        
        real(dr) :: top
        real(dr) :: bot


        type (MUSG_Project) musg_l

        read(FnumTG,'(a)') FName
        call OpenAscii(FNum,FName)
        call Msg( 'Kx file: '//FName)

        allocate(musg_l.Kx(musg_l.gwf.nCell))
        allocate(musg_l.Thick(musg_l.gwf.nCell))
        allocate(musg_l.T(musg_l.gwf.nCell))

	    if(status /= 0) then
		    call ErrMsg('FILE ERROR: '//fname)
		    stop
        endif
	    read(itmp,*) (musg_l.Kx(j),j=1,musg_l.gwf.nCell)
	    call freeunit(FNum)
        
        do j=1,musg_l.gwf.nCell
            top=0.0
            bot=0.0
            do m=1,4
                top=top+musg_l.gwf.z(musg_l.gwf.ivertex(m,j))/4.0d0
            end do
            do m=5,8
                bot=bot+musg_l.gwf.z(musg_l.gwf.ivertex(m,j))/4.0d0
            end do
            
            if(j==3657) then
                continue
            end if
            
            if (abs(musg_l.head(j,1)-999.0d0) < 1e-5 ) then                           ! inactive
                musg_l.Thick(j)=0.0
                continue
            else if (musg_l.head(j,1) < bot) then                           ! dry
                musg_l.Thick(j)=0.0
                continue
            else if(musg_l.head(j,1) < top .and. musg_l.head(j,1) > bot) then    ! partially saturated
                musg_l.Thick(j)=musg_l.head(j,1)-bot
                continue
            else
                musg_l.Thick(j)=top-bot                              ! saturated
                continue
            endif
            
            musg_l.T(j)=musg_l.Thick(j)*musg_l.Kx(j)
        
        end do
            

        continue

    end subroutine MUSG_ReadAsciiKxFile
   subroutine MUSG_ReadAsciiSsFile(FnumTG,musg_l)
        implicit none

        integer :: j
        integer :: FnumTG
        integer :: Fnum
        character(MAXLBL) :: FName

        type (MUSG_Project) musg_l

        read(FnumTG,'(a)') FName
        call OpenAscii(FNum,FName)
        call Msg( 'Ss file: '//FName)

        allocate(musg_l.Ss(musg_l.gwf.nCell))

	    if(status /= 0) then
		    call ErrMsg('FILE ERROR: '//fname)
		    stop
        endif
	    read(itmp,*) (musg_l.ss(j),j=1,musg_l.gwf.nCell)
	    call freeunit(FNum)

        continue

    end subroutine MUSG_ReadAsciiSsFile
   subroutine MUSG_ReadAsciiSyFile(FnumTG,musg_l)
        implicit none

        integer :: j
        integer :: FnumTG
        integer :: Fnum
        character(MAXLBL) :: FName

        type (MUSG_Project) musg_l

        read(FnumTG,'(a)') FName
        call OpenAscii(FNum,FName)
        call Msg( 'Sy file: '//FName)

        allocate(musg_l.Sy(musg_l.gwf.nCell))

	    if(status /= 0) then
		    call ErrMsg('FILE ERROR: '//fname)
		    stop
        endif
	    read(itmp,*) (musg_l.Sy(j),j=1,musg_l.gwf.nCell)
	    call freeunit(FNum)

        continue

    end subroutine MUSG_ReadAsciiSyFile
   subroutine MUSG_ReadAsciiVanisFile(FnumTG,musg_l)
        implicit none

        integer :: j
        integer :: FnumTG
        integer :: Fnum
        character(MAXLBL) :: FName

        type (MUSG_Project) musg_l

        read(FnumTG,'(a)') FName
        call OpenAscii(FNum,FName)
        call Msg( 'Vanis file: '//FName)

        allocate(musg_l.Vanis(musg_l.gwf.nCell))

	    if(status /= 0) then
		    call ErrMsg('FILE ERROR: '//fname)
		    stop
        endif
	    read(itmp,*) (musg_l.Vanis(j),j=1,musg_l.gwf.nCell)
	    call freeunit(FNum)

        continue

    end subroutine MUSG_ReadAsciiVanisFile
   subroutine MUSG_ReadRiverFlowsAsciiFile(FnumTG,musg_l)
        implicit none

        integer :: i
        integer :: FnumTG
        integer :: Fnum
        character(MAXLBL) :: FName
        character(MAXSTRING) :: Line
        real(dr) :: r1

        type (MUSG_Project) musg_l

        read(FnumTG,'(a)') FName
        call OpenAscii(FNum,FName)
        call Msg( 'River Flows file: '//FName)

        read(FNum,'(a)') line  ! header
        do 
            read(FNum,*,iostat=status) r1
	        if(status /= 0) exit

            musg_l.nlines=musg_l.nlines+1
        end do
        musg_l.nlines=musg_l.nlines-1
        allocate(musg_l.StressPeriod(musg_l.nlines), & 
                    & musg_l.RiverCell(musg_l.nlines), &
                    & musg_l.RiverFlow(musg_l.nlines), &
                    & musg_l.RiverHead(musg_l.nlines), &
                    & musg_l.RiverElev(musg_l.nlines), &
                    & musg_l.RiverCond(musg_l.nlines), &
                    & musg_l.RiverConc(musg_l.nlines))
        
        rewind(FNum)
        read(FNum,'(a)') line  ! header
        read(FNum,*) (musg_l.StressPeriod(i), r1, &
                        & musg_l.RiverCell(i), &
                        & musg_l.RiverFlow(i), &
                        & musg_l.RiverHead(i), &
                        & musg_l.RiverElev(i), &
                        & musg_l.RiverCond(i), &
                        & musg_l.RiverConc(i), &
        i=1,musg_l.nlines)

        
        continue

    end subroutine MUSG_ReadRiverFlowsAsciiFile
   subroutine MUSG_RiverFlowsToTecplot(FnumTG,musg_l) !--- Dump river flow data by cell to tecplot .dat file
        implicit none 

        integer :: FnumTG
        integer :: Fnum
        character(MAXLBL) :: FName

        type (MUSG_Project) musg_l

        integer :: LastStressPeriod
        character(MAXSTRING) :: VarBuffer
        character(MAXSTRING) :: OutputBuffer
        integer :: i, j
        integer :: iFile
        integer :: NCellFluxvsT, iCell
    
        read(FnumTG,'(a)') FName
        l1=index(FName,'.dat')
        if(l1==0) then
            FNAME=trim(FName)//'.tecplot.dat'
        endif
        call OpenAscii(FNum,FName)
        call Msg( 'To File: '//trim(FName))
    
        VarBuffer='variables="X","Y","Z","Flux(ft3/d)","Head(ft)","Conductance(ft/d)","Elevation(ft)","Concentration(ug/L)","Elapsed Time (days)","Cell"'
        write(FNum,'(a)') trim(VarBuffer)
    
        LastStressPeriod=0
        OutputBuffer=''
        ifile=0
        do i=1,musg_l.nlines
            if(musg_l.StressPeriod(i) /= LastStressPeriod) then
                write(FNum,'(a,i5,a,f20.4)') 'zone t= "Stress Period ',musg_l.StressPeriod(i),'", SOLUTIONTIME = ',musg_l.totim(musg_l.StressPeriod(i))
                LastStressPeriod=musg_l.StressPeriod(i)
            endif
            write(FNum,'(9f20.4,i8)') musg_l.gwf.xCell(musg_l.RiverCell(i)),musg_l.gwf.yCell(musg_l.RiverCell(i)),musg_l.gwf.zCell(musg_l.RiverCell(i)), &
                                    & musg_l.RiverFlow(i), &
                                    & musg_l.RiverHead(i), &
                                    & musg_l.RiverElev(i), &
                                    & musg_l.RiverCond(i), &
                                    & musg_l.RiverConc(i), &
                                    & musg_l.totim(musg_l.StressPeriod(i)), &
                                    & musg_l.RiverCell(i)
        end do
    
        call FreeUnit(FNum)
        
        read(FnumTG,*,iostat=status,end=10) NCellFluxvsT
	    if(status == 0 .and. NCellFluxvsT>0 ) then
            do i=1,NCellFluxvsT
                read(FnumTG,*) iCell
                TmpSTR=FileNumberString(icell)
                FName='cell_'//trim(TmpSTR)//'.dat'
                call OpenAscii(FNum,FName)
                call Msg( 'Cell flux to File: '//trim(FName))
            
                write(FNum,'(a)') 'variables="Time(days)","Flux(ft3/d)"'
                write(FNum,'(a,i8,a)') 'zone t= "Cell ',iCell,'"'
            
                do j=1,musg_l.nlines
                    if(musg_l.RiverCell(j)==iCell) then
                        write(FNum,'(9f20.4,i8)') musg_l.totim(musg_l.StressPeriod(j)), musg_l.RiverFlow(j)
                    endif
                end do
                call FreeUnit(FNum)
                
            end do
        endif
            
            
10      continue        
    
    end subroutine MUSG_RiverFlowsToTecplot
   subroutine MUSG_ReadHeadCalibrationAsciiFile(FnumTG,musg_l)
        implicit none

        integer :: i
        integer :: FnumTG
        integer :: Fnum
        character(MAXLBL) :: FName
        character(MAXSTRING) :: Line
        real(dr) :: r1

        type (MUSG_Project) musg_l

        read(FnumTG,'(a)') FName
        call OpenAscii(FNum,FName)
        call Msg( 'River Flows file: '//FName)

        read(FNum,'(a)') line  ! header
        do 
            read(FNum,*,iostat=status) r1
	        if(status /= 0) exit

            musg_l.nlinesHead=musg_l.nlinesHead+1
        end do
 
        allocate(     musg_l.StressPeriodHead(musg_l.nlinesHead), & 
                    & musg_l.WellNameHead(musg_l.nlinesHead), &
                    & musg_l.Xhead(musg_l.nlinesHead), &
                    & musg_l.YHead(musg_l.nlinesHead), &
                    & musg_l.ZminHead(musg_l.nlinesHead), &
                    & musg_l.ZmaxHead(musg_l.nlinesHead), &
                    & musg_l.Observed_ft(musg_l.nlinesHead), &
                    & musg_l.Simulated_ft(musg_l.nlinesHead))
        rewind(FNum)
        read(FNum,'(a)') line  ! header
        read(FNum,*) (   musg_l.StressPeriodHead(i), & 
                       & musg_l.WellNameHead(i), &
                       & musg_l.Xhead(i), &
                       & musg_l.YHead(i), &
                       & musg_l.ZminHead(i), &
                       & musg_l.ZmaxHead(i), &
                       & musg_l.Observed_ft(i), &
                       & musg_l.Simulated_ft(i), &
        & i=1,musg_l.nlinesHead)

        
        continue

    end subroutine MUSG_ReadHeadCalibrationAsciiFile
   subroutine MUSG_HeadCalibrationToTecplot(FnumTG,musg_l) !--- Dump Head Calibration data to tecplot .dat file
        implicit none 

        integer :: FnumTG
        integer :: Fnum
        character(MAXLBL) :: FName

        type (MUSG_Project) musg_l

        character(MAXSTRING) :: VarBuffer
        character(MAXSTRING) :: OutputBuffer
        integer :: i
        integer :: iFile
        
        real(dr) :: zmidpoint
        real(dr) :: obs
        real(dr) :: sim
        integer :: navg
        
        real(dr) :: TZeroDateNum
    
        read(FnumTG,'(a)') FName
        l1=index(FName,'.dat')
        if(l1==0) then
            FNAME=trim(FName)//'.tecplot.dat'
        endif
        call OpenAscii(FNum,FName)
        call Msg( 'To File: '//trim(FName))
        VarBuffer='variables="X","Y","Z","Observed(ft)","Simulated(ft)","Residual(ft)","Stress Period","DateNum"'
        write(FNum,'(a)') trim(VarBuffer)
        
        read(FnumTG,*) TZeroDateNum

         
        OutputBuffer=''
        ifile=0

        ! initialize with first stress period and first well data (i.e. first record)
        write(FNum,'(a,i5,a,f20.4)') 'zone t= "Stress Period ',musg_l.StressPeriodHead(1),'", SOLUTIONTIME = ',musg_l.totim(musg_l.StressPeriodHead(1))
        navg=1
        obs=musg_l.Observed_ft(1)
        sim=musg_l.Simulated_ft(1)
        do i=2,musg_l.nlinesHead
            if(musg_l.StressPeriodHead(i) == 208) then
                continue
            endif
            if(musg_l.StressPeriodHead(i) /= musg_l.StressPeriodHead(i-1)) then  ! end of stress period and well data 
                ! Calculate average and write last reading
                zmidpoint=(musg_l.ZminHead(i-1)+musg_l.ZmaxHead(i-1))/2.0
                write(FNum,'(6f20.4,i8,F20.4)') musg_l.xhead(i-1),musg_l.yhead(i-1),zmidpoint, &
                    & obs/navg, &
                    & sim/navg, &
                    & (sim-obs)/navg, &
                    & musg_l.StressPeriodHead(i-1), &
                    & musg_l.totim(musg_l.StressPeriodHead(i-1))+TZeroDateNum
!                write(FNum,'(a)')  '% '//musg_l.WellNameHead(i-1)


                write(FNum,'(a,i5,a,f20.4)') 'zone t= "Stress Period ',musg_l.StressPeriodHead(i),'", SOLUTIONTIME = ',musg_l.totim(musg_l.StressPeriodHead(i))
                navg=1
                obs=musg_l.Observed_ft(i)
                sim=musg_l.Simulated_ft(i)
            else  ! stress period reading i
                if(musg_l.WellNameHead(i) == musg_l.WellNameHead(i-1)) then ! same well as last reading, update avg
                    navg=navg+1
                    obs=obs+musg_l.Observed_ft(i)
                    sim=sim+musg_l.Simulated_ft(i)
                else  ! different well than last reading
                    ! Calculate average and write last reading
                    zmidpoint=(musg_l.ZminHead(i-1)+musg_l.ZmaxHead(i-1))/2.0
                    write(FNum,'(6f20.4,i8,F20.4)') musg_l.xhead(i-1),musg_l.yhead(i-1),zmidpoint, &
                           & obs/navg, &
                           & sim/navg, &
                           & (sim-obs)/navg , &
                           & musg_l.StressPeriodHead(i-1), &
                           & musg_l.totim(musg_l.StressPeriodHead(i-1))+TZeroDateNum
!                    write(FNum,'(a)')  '% '//musg_l.WellNameHead(i-1)
                    ! initialize with this reading
                    navg=1
                    obs=musg_l.Observed_ft(i)
                    sim=musg_l.Simulated_ft(i)

                 endif
            
            endif 
        end do
        ! Calculate average and write last reading
        zmidpoint=(musg_l.ZminHead(musg_l.nlinesHead)+musg_l.ZmaxHead(musg_l.nlinesHead))/2.0
        write(FNum,'(6f20.4,i8,F20.4)') musg_l.xhead(musg_l.nlinesHead),musg_l.yhead(musg_l.nlinesHead),zmidpoint, &
            & obs/navg, &
            & sim/navg, &
            & (sim-obs)/navg, &
            & musg_l.StressPeriodHead(i-1), &
            & musg_l.totim(musg_l.StressPeriodHead(i-1))+TZeroDateNum
!        write(FNum,'(a)')  '% '//musg_l.WellNameHead(musg_l.nlinesHead)

    
        call FreeUnit(FNum)
    
    end subroutine MUSG_HeadCalibrationToTecplot
    subroutine MUSG_ReadWellConstructionCSVFile(FnumTG,musg_l)
        implicit none

        integer :: i
        integer :: FnumTG
        integer :: Fnum
        character(MAXLBL) :: FName
        character(MAXSTRING) :: line
        type (MUSG_Project) musg_l

        read(FnumTG,'(a)') FName
        call OpenAscii(FNum,FName)
        call Msg( 'Well construction details file: '//FName)
        
        ! Count well data
        musg_l.nWellConst=0
        read(FNum,*) line ! throw away header
        do 
            read(FNum,'(a)',iostat=status) line 
            if(status/=0) then
                exit
            end if
            
            musg_l.nWellConst=musg_l.nWellConst+1
        end do

        allocate(musg_l.NameWellConst(musg_l.nWellConst), &
	        & musg_l.XWellConst(musg_l.nWellConst), &
	        & musg_l.YWellConst(musg_l.nWellConst), &
	        & musg_l.BotElevWellConst(musg_l.nWellConst), &
	        & musg_l.TopElevWellConst(musg_l.nWellConst), &
	        & musg_l.CasingRadiusWellConst(musg_l.nWellConst), &
	        & musg_l.TonWellConst(musg_l.nWellConst), &
	        & musg_l.ToffWellConst(musg_l.nWellConst))
        
        rewind(FNum)

        read(FNum,*) line
        do i=1,musg_l.nWellConst
	        read(FNum,*) musg_l.NameWellConst(i), &
	            & musg_l.XWellConst(i), &
	            & musg_l.YWellConst(i), &
	            & musg_l.BotElevWellConst(i), &
	            & musg_l.TopElevWellConst(i), &
	            & musg_l.CasingRadiusWellConst(i), &
	            & musg_l.TonWellConst(i), &
	            & musg_l.ToffWellConst(i)
        end do
        
        continue
        
	    call freeunit(FNum)

        continue

   end subroutine MUSG_ReadWellConstructionCSVFile
    subroutine MUSG_Read_EIWellCSVFile(FnumTG,musg_l)
        implicit none

        integer :: i, i1, i2
        integer :: FnumTG
        integer :: Fnum
        character(MAXLBL) :: FName
        character(MAXSTRING) :: line
        type (MUSG_Project) musg_l

        read(FnumTG,'(a)') FName
        call OpenAscii(FNum,FName)
        call Msg( 'EI well construction details file: '//FName)
        
        ! Count well data
        musg_l.n_EIWell=0
        read(FNum,*) line ! throw away header
        do 
            read(FNum,'(a)',iostat=status) line 
            if(status/=0) then
                exit
            end if
            
            musg_l.n_EIWell=musg_l.n_EIWell+1
        end do

        allocate(musg_l.Name_EIWell(musg_l.n_EIWell), &
	        & musg_l.X_EIWell(musg_l.n_EIWell), &
	        & musg_l.Y_EIWell(musg_l.n_EIWell), &
	        & musg_l.TopElev_EIWell(musg_l.n_EIWell), &
	        & musg_l.ScreenALength_EIWell(musg_l.n_EIWell), &
	        & musg_l.ScreenBOffset_EIWell(musg_l.n_EIWell), &
	        & musg_l.ScreenBLength_EIWell(musg_l.n_EIWell))
        
        rewind(FNum)

        read(FNum,'(a)') line
        do i=1,musg_l.n_EIWell
	        read(FNum,*) musg_l.Name_EIWell(i), &
	            & musg_l.X_EIWell(i), &
	            & musg_l.Y_EIWell(i), &
	            & musg_l.TopElev_EIWell(i), &
	            & musg_l.ScreenALength_EIWell(i), &
	            & musg_l.ScreenBOffset_EIWell(i), &
	            & musg_l.ScreenBLength_EIWell(i)
            i1=index(musg_l.Name_EIWell(i),'IW')
            i2=index(musg_l.Name_EIWell(i),'EW')
            if(i1==0 .and. i2==0) then
                call ErrMsg('Extraction/Injection well name "'//trim(musg_l.Name_EIWell(i))//'" must contain the string "IW" (injection well) or "EW" (extraction well)') 
            endif
        end do
        
        continue
        
	    call freeunit(FNum)

        continue

   end subroutine MUSG_Read_EIWellCSVFile


   subroutine MUSG_RiverConductanceUpdate(FnumTG) !--- Dump Head Calibration data to tecplot .dat file
        implicit none 

        integer :: FnumTG
        integer :: Fnum
        character(MAXLBL) :: FName
        integer :: FnumOut
        character(MAXLBL) :: FNameOut


        integer :: i
        
        integer :: nRiv       
        real(dr) :: length_ft(1000)
        integer :: node(1000)
        integer :: zonenum(1000)
        character*80 :: line
        real(dr) :: Cond_d_Len(1000)
        
        real(dr) :: a2, a4, a5
        integer :: i1, i2
        real(dr) :: oldCond, newCond
        
    
        ! file 1
        read(FnumTG,'(a)') FName  
        call OpenAscii(FNum,FName)
        call Msg( 'File 1: '//trim(FName))
        
        i=1
        read(FNum,'(a)') line
        do 
           read(FNum,*,iostat=status) node(i),Length_ft(i),ZoneNum(i)
           if(status /= 0) exit
            
           i=i+1
        end do
        nRiv=i-1 
        call FreeUnit(FNum)


        ! file 2
        read(FnumTG,'(a)') FName  
        call OpenAscii(FNum,FName)
        call Msg( 'File 2: '//trim(FName))
        
        read(FNum,'(a)') line
        do 
           read(FNum,*,iostat=status)  i, Cond_d_Len(i)
           if(status /= 0) exit
            
        end do

        ! file 3
        read(FnumTG,'(a)') FName  
        call OpenAscii(FNum,FName)
        call Msg( 'File 3: '//trim(FName))
        
        ! file Out
        read(FnumTG,'(a)') FNameOut  
        call OpenAscii(FNumOut,FNameOut)
        call Msg( 'File Out: '//trim(FNameOut))
        
        
        read(FNum,'(a)') line
        write(FNumOut,'(a)') line
        read(FNum,'(a)') line
        write(FNumOut,'(a)') line
        do 
            read(FNum,'(a)',iostat=status) line
            if(status /= 0) exit
           
            write(FNumOut,'(a)') line
            do i=1,nRiv
                read(FNum,*) i1, a2, oldCond, a4, a5, i2
                newCond=Length_ft(i)*Cond_d_Len(i)
                if(abs(newcond-oldcond) > 1e-3) then
                    continue
                endif
                write(FNumOut,'(i8,1x,f12.6,1x,1pe15.6,1x,f12.6,1x,1pe15.6,i5)') i1, a2, newCond, a4, a5, i2
            end do
        end do
        call FreeUnit(FNum)
        call FreeUnit(FNumOut)
        continue
    
    end subroutine MUSG_RiverConductanceUpdate
    subroutine MUSG_PEST_WellRatePenalties(FnumTG) !--- Overwrite rates in Modflow-USG well file
        implicit none 
        
        integer :: i, j

        integer :: FnumTG
        integer :: FnumPenalty
        character(MAXLBL) :: FNamePenalty
        integer :: nRange
        real(dr), allocatable :: MinPenalty(:)
        real(dr), allocatable :: MaxPenalty(:)
        real(dr), allocatable :: MinRange(:)
        real(dr), allocatable :: MaxRange(:)
        real(dr) :: InjRatePercentTargetLowerMin
        real(dr) :: InjRatePercentTargetLowerMax
        real(dr) :: InjratePercentPenaltyLowerMin
        real(dr) :: InjratePercentPenaltyLowerMax
        real(dr) :: InjRatePercentTargetHigher
        real(dr) :: InjratePercentPenaltyHigher
        real(dr) :: PESTStressPeriodInjWellFlips
        real(dr) :: PESTStressPeriodExtWellFlips


        real(dr) :: MinExtRate
        real(dr) :: MaxInjRateGPM
        
        integer :: FnumExtWell
        character(MAXLBL) :: FNameExtWell
        integer :: nExtWell
        character(20), allocatable :: ExtWellName(:)
        real(dr), allocatable :: ExtWellRate(:,:)
        
        integer :: FnumInjWell
        character(MAXLBL) :: FNameInjWell
        integer :: nInjWell
        character(20), allocatable :: InjWellName(:)
        real(dr), allocatable :: InjWellFraction(:,:)
        
        integer :: nStressPeriods
        
        ! CLN file
        integer :: FnumCLN
        character(MAXLBL) :: FNameCLN
        integer :: nCLN
        integer :: iCLNFirstEWIW
        integer, allocatable :: CLNType(:)   ! 0 - conventional well, 1 - extaction (EW) well, 2 - injection (IW) well
        integer, parameter :: EW_upper=11
        integer, parameter :: EW_lower=12
        integer, parameter :: IW_upper=21
        integer, parameter :: IW_lower=22
        
        ! Calculations
        real(dr) :: sumExtWellRates
        real(dr) :: sumInjWellFractions
        real(dr), allocatable :: InjWellRate(:,:)
        
        ! Modflow-USG well file
        integer :: FnumMUSGWell
        character(MAXLBL) :: FNameMUSGWell
        integer :: FnumMUSGWellOut
        character(MAXLBL) :: FNameMUSGWellOut
        integer :: iStressPeriod
        character(MAXLBL) :: line
        character(20) :: ThisWellName
        logical :: WellFound
        integer :: cln
        real(dr) :: rate
        
        ! Modflow-USG transient ibound file
        integer :: FnumTIB
        character(MAXLBL) :: FNameTIB
        integer :: FnumTIBOut
        character(MAXLBL) :: FNameTIBOut
        integer :: nCLNTurnOff
        integer :: nCLNTurnOn
        integer :: nCells  
        integer :: iDum2
        integer :: nEWIWTurnOff
        integer :: nEWIWTurnOn
        integer :: nlist
        integer :: CLNArray(MAXCLN)  
        
         
        
        ! Penalties input file
        read(FnumTG,'(a)') FNamePenalty  
        call OpenAscii(FNumPenalty,FNamePenalty)
        call Msg( 'Penalties file: '//trim(FNamePenalty))
    
        
        read(FNumPenalty,*) MinExtRate
        write(TMPStr,'(4f15.3)') MinExtRate
        call Msg( 'Minimum extraction rate: '//trim(TMPStr))
        read(FNumPenalty,*) nRange
        allocate(MinRange(nRange), MaxRange(nRange), MinPenalty(nRange), MaxPenalty(nRange))
        do i=1,nRange
            read(FNumPenalty,*) MinRange(i), MaxRange(i), MinPenalty(i), MaxPenalty(i)
            write(TMPStr,'(4f15.3)') MinRange(i), MaxRange(i), MinPenalty(i), MaxPenalty(i)
            call Msg( 'Rate1 Rate2 Penalty1 Penalty2: '//trim(TMPStr))
            if(i>1) then
                if(Minrange(i)/=MaxRange(i-1)) then
                    write(ErrStr,'(a,2f10.2)') 'Range 1: ', MinRange(i-1), MaxRange(i-1)
                    call Msg(ErrStr)
                    write(ErrStr,'(a,2f10.2)') 'Range 2: ', MinRange(i), MaxRange(i)
                    call Msg(ErrStr)
                    call ErrMsg('Min range 2 not equal to max range 1')
                end if
                if(MinPenalty(i)/=MaxPenalty(i-1)) then
                    write(ErrStr,'(a,2f10.2)') 'Penalty 1: ', MinPenalty(i-1), MaxPenalty(i-1)
                    call Msg(ErrStr)
                    write(ErrStr,'(a,2f10.2)') 'Penalty 2: ', MinPenalty(i), MaxPenalty(i)
                    call Msg(ErrStr)
                    call ErrMsg('Min penalty 2 not equal to max penalty 1')
                end if
            end if
        end do

        read(FNumPenalty,*) MaxInjRateGPM
        write(TMPStr,*) MaxInjRateGPM
        call Msg( 'Maximum injection rate: '//trim(TMPStr))

        read(FNumPenalty,*) InjRatePercentTargetLowerMin,InjRatePercentTargetLowerMax, InjratePercentPenaltyLowerMin, InjratePercentPenaltyLowerMax
        write(TMPStr,'(4f15.3)') InjRatePercentTargetLowerMin,InjRatePercentTargetLowerMax, InjratePercentPenaltyLowerMin, InjratePercentPenaltyLowerMax
        call Msg( 'Injection Rate%1 Rate%2 Penalty1 Penalty2: '//trim(TMPStr))

        read(FNumPenalty,*) InjRatePercentTargetHigher, InjratePercentPenaltyHigher
        write(TMPStr,'(4f15.3)') InjRatePercentTargetHigher, InjratePercentPenaltyHigher
        call Msg( 'InjRatePercentTargetHigher InjratePercentPenaltyHigher: '//trim(TMPStr))

        read(FNumPenalty,*) PESTStressPeriodInjWellFlips
        write(TMPStr,*) nint(PESTStressPeriodInjWellFlips)
        call Msg( 'Injection (IW) Wells flip over at stress period: '//trim(TMPStr))
       
        read(FNumPenalty,*) PESTStressPeriodExtWellFlips
        write(TMPStr,*) nint(PESTStressPeriodExtWellFlips)
        call Msg( 'Extraction (EW) Wells flip over at stress period: '//trim(TMPStr))
       
        
        
        ! Extraction well file
        read(FNumPenalty,'(a)') FNameExtWell
        call OpenAscii(FNumExtWell,FNameExtWell)
        call Msg( 'Extraction well file: '//trim(FNameExtWell))

        read(FNumExtWell,*) nExtWell
        read(FNumExtWell,*) nStressPeriods
        allocate(ExtWellName(nExtWell),ExtWellRate(nExtWell,nStressPeriods))
        do i=1,nExtWell
            read(FNumExtWell,*) ExtWellName(i)
            do j=1,nStressPeriods
                read(FNumExtWell,*) ExtWellRate(i,j)
            end do
        end do

        call FreeUnit(FNumExtWell)
        
        ! Injection well file
        read(FNumPenalty,'(a)') FNameInjWell
        call OpenAscii(FNumInjWell,FNameInjWell)
        call Msg( 'Injection well file: '//trim(FNameInjWell))

        read(FNumInjWell,*) nInjWell
        read(FNumExtWell,*) nStressPeriods
        allocate(InjWellName(nInjWell),InjWellFraction(nInjWell,nStressPeriods),InjWellRate(nInjWell,nStressPeriods))

        do i=1,nInjWell
            read(FNumInjWell,*) InjWellName(i)
            do j=1,nStressPeriods
                read(FNumExtWell,*)InjWellFraction(i,j)
            end do
        end do
        call FreeUnit(FNumInjWell)

        call FreeUnit(FNumPenalty)
        
        ! CLN information file
        read(FNumTG,'(a)') FNameCLN
        call OpenAscii(FNumCLN,FNameCLN)
        call Msg( 'CLN file: '//trim(FNameCLN))

        read(FNumCLN,*) nCLN
        
        allocate(CLNType(nCLN))
        CLNType(:)=0  ! by default a conventional well
        
        read(FNumCLN,'(a)') line   ! throw away line 2
        ! determine CLN mumber of first extraction/injection well and flag CLNType
        iCLNFirstEWIW=nCLN+1
        do i=1,nCLN  
            read(FNumCLN,'(a)') line  
            l1=index(line,'Well = ')
            if(l1>0) then
                read(line(l1+7:),'(a)') ThisWellName
                if(ThisWellName(1:2)=="EW") then 
                    iCLNFirstEWIW=min(i,iCLNFirstEWIW)
                    if(index(ThisWellName,'_A')>0) CLNType(i)=EW_upper
                    if(index(ThisWellName,'_B')>0) CLNType(i)=EW_lower
                else if(ThisWellName(1:2)=="IW") then
                    iCLNFirstEWIW=min(i,iCLNFirstEWIW)
                    if(index(ThisWellName,'_A')>0) CLNType(i)=IW_upper
                    if(index(ThisWellName,'_B')>0) CLNType(i)=IW_lower
                endif
            endif
        end do
        
        continue
        
        do j=1,nStressPeriods 
            ! Sum extraction well rates
            sumExtWellRates=0.0d0
            do i=1,nExtWell
                sumExtWellRates=sumExtWellrates+abs(ExtWellRate(i,j))
            end do
    
            ! Normalize injection well fractions
            sumInjWellFractions=0.0d0
            do i=1,nInjWell
                sumInjWellFractions=sumInjWellFractions+InjWellFraction(i,j)
            end do
            do i=1,nInjWell
                InjWellFraction(i,j)=InjWellFraction(i,j)*1.0d0/sumInjWellFractions
            end do
            
            ! check, new fractions should sum to 1
            sumInjWellFractions=0.0d0
            do i=1,nInjWell
                sumInjWellFractions=sumInjWellFractions+InjWellFraction(i,j)
            end do
            
            write(TMPStr,'(g15.3)') sumInjWellFractions
            call Msg('Check sum of normalized injection well fractions is 1: '//TMPStr)
        
            ! Calculate injection well rates
            do i=1,nInjWell
                InjWellRate(i,j)=InjWellFraction(i,j)*sumExtWellrates
                if(InjWellRate(i,j)>MaxInjRateGPM) InjWellRate(i,j)=MaxInjRateGPM
            end do
        
        end do
       

        call FreeUnit(FNumPenalty)

        ! Modflow-USG well file input file
        read(FnumTG,'(a)') FNameMUSGWell  
        call OpenAscii(FnumMUSGWell,FNameMUSGWell)
        call Msg( 'Modflow-USG well input file: '//trim(FNameMUSGWell))
        
        ! Open new well output file
        FNameMUSGWellOut='out_'//FNameMUSGWell  
        call OpenAscii(FnumMUSGWellOut,FNameMUSGWellOut)
        call Msg( 'Modflow-USG well output file: '//trim(FNameMUSGWellOut))

        read(FNumMUSGWell,'(a)') line
        write(FNumMUSGWellout,'(a)') line
        
        iStressPeriod=0
        read_well_file: do 
            read(FNumMUSGWell,'(a)',iostat=status) line
            if(status /= 0) then  ! end of file
                exit read_well_file
            endif
            
            l1=index(line,'stress period')
            if(l1 > 0) then ! new stress period
                iStressPeriod=iStressPeriod+1
                ! write stress period header line
                write(FNumMUSGWellout,'(a)') line
            else  ! this is a well line
                ! Get well name
                l1=index(line,'well = ')
                if(l1>0) then
                    read(line(l1+7:),'(a)') ThisWellName
                    WellFound=.false.
                    
                    ! check if is an extraction well
                    do i=1,nExtWell
                        if(index(ExtWellName(i),ThisWellName) > 0) then ! found an extraction well
                            read(line,'(i11,g15.7,a80)') Cln, rate, line
                            if(iStressPeriod >= nint(PESTStressPeriodExtWellFlips)) then
                                cln=cln+1
                                line=trim(line)//'_B'    ! append suffix for lower screens
                            endif
                            write(FNumMUSGWellout,'(i11,g15.7,a80)') Cln, ExtWellRate(i,iStressPeriod)*192.5d0, line
                            WellFound=.true.
                            exit 
                        endif
                    end do
                    
                    ! check if is an injection well
                    if(.not. WellFound) then
                        do i=1,nInjWell
                            if(index(InjWellName(i),ThisWellName) > 0) then ! found an injection well
                                read(line,'(i11,g15.7,a80)') Cln, rate, line
                                if(iStressPeriod >= nint(PESTStressPeriodInjWellFlips)) then
                                    cln=cln+1
                                    line=trim(line)//'_B'    ! append suffix for lower screens
                                endif
                                write(FNumMUSGWellout,'(i11,g15.7,a80)') Cln, InjWellRate(i,iStressPeriod)*192.5d0, line
                                WellFound=.true.
                                exit 
                            endif
                        end do
                    endif

                    ! normal well
                    if(.not. WellFound) then
                        write(FNumMUSGWellout,'(a)') line
                    endif
                end if 
            endif
        end do read_well_file  
        
        ! Modflow-USG transient ibound input file
        read(FnumTG,'(a)') FNameTIB  
        call OpenAscii(FnumTIB,FNameTIB)
        call Msg( 'Modflow-USG transient ibound input file: '//trim(FNameTIB))
        
        read(FnumTG,*) nCells
        
        
        ! Open new transient ibound output file
        FNameTIBOut='out_'//FNameTIB 
        call OpenAscii(FnumTIBOut,FNameTIBOut)
        call Msg( 'Modflow-USG transient ibound output file: '//trim(FNameTIBOut))

        read_tib_file: do 
            read(FnumTIB,'(a)',iostat=status) line
            if(status /= 0) then  ! end of file
                exit read_tib_file
            endif
            
            
            l1=index(line,'stress period ')
            if(l1 > 0) then ! new stress period
                
                l1=l1+14  ! position at end of string 'stress period '
                
                TMPStr=line(31:)

                ! Extract stress period from line
                l2=l1+index(line(l1:),':')-2
                read(line(l1:l2),*) iStressPeriod
                
                read(line,*) nCLNTurnOff, nCLNTurnOn, idum2
                
                if(iStressPeriod==1) then ! Always turn off lower screens of new CLN's unless PESTStressPeriodInjWellFlips=1 and/or PESTStressPeriodExtWellFlips=1
               
                    ! determine how many more CLN's are going to be turned off 
                    nEWIWTurnoff=nCLNTurnOff
                    do i=iCLNFirstEWIW,ncln
                        if(PESTStressPeriodInjWellFlips>1 .and. CLNType(i)==IW_lower) nEWIWTurnoff=nEWIWTurnoff+1
                        if(PESTStressPeriodExtWellFlips>1 .and. CLNType(i)==EW_lower) nEWIWTurnoff=nEWIWTurnoff+1
                    end do
                    
                    write(FNumTIBOut,'(3i10,a)')  nEWIWTurnoff, nCLNTurnOn, idum2, trim(TMPStr)
                    
                    if(nCLNTurnOff>0) then  ! there were convetional well to be turned off
                        read(FnumTIB,'(a)') line
                        write(FNumTIBOut,'(a)') line ! always write next line as is  
                        ! read and write the existing data
                        read(FnumTIB,'(10i10)') (CLNArray(i),i=1,nCLNTurnOff)
                        write(FnumTIBout,'(10i10)') (CLNArray(i),i=1,nCLNTurnOff)
                    else if(nEWIWTurnoff>nCLNTurnOff) then   ! some or all IW/EW screens are turned off
                        write(FNumTIBOut,'(a)') 'INTERNAL  1  (FREE)  -1  IB0 array'
                        
                        ! make a list of CLN numbers to be turned off
                        nList=0
                        CLNArray(:)=0
                        do i=iCLNFirstEWIW,ncln
                            if(PESTStressPeriodInjWellFlips>1 .and. CLNType(i)==IW_lower) then
                                nList=nList+1
                                CLNArray(nlist)=i+ncells                          
                            else if (PESTStressPeriodExtWellFlips>1 .and. CLNType(i)==EW_lower) then
                                nList=nList+1
                                CLNArray(nlist)=i+ncells                              
                            endif
                        end do
                        write(FnumTIBout,'(10i10)') (CLNArray(i),i=1,nlist)
                    end if
                   
                    if(nCLNTurnOn>0) then  
                        do i=1,nCLNTurnOn  ! read and write the next nCLNTurnOn lines
                            read(FnumTIB,'(a)') line
                            write(FNumTIBOut,'(a)') line ! always write next line as is  
                        end do
                    endif

                else if(iStressPeriod == nint(PESTStressPeriodInjWellFlips) .and. &   ! turn on new cln lower screens for all injection (IW) wells
                        iStressPeriod == nint(PESTStressPeriodExtWellFlips)) then     ! turn on new cln lower screens for all extraction (EW) wells
                    ! determine how many new CLN's are going to be turned on 
                    
                    nEWIWTurnOn=nCLNTurnOn
                    do i=iCLNFirstEWIW,ncln
                        if(CLNType(i)==IW_lower) nEWIWTurnOn=nEWIWTurnOn+1
                        if(CLNType(i)==EW_lower) nEWIWTurnOn=nEWIWTurnOn+1
                    end do
                   
                    write(FNumTIBOut,'(3i10,a)')  nCLNTurnOff, nEWIWTurnOn, idum2, trim(TMPStr)
                    
                    if(nCLNTurnOff>0) then  
                        ! read and write the existing data
                        read(FnumTIB,'(10i10)') (CLNArray(i),i=1,nCLNTurnOff)
                        write(FnumTIBout,'(10i10)') (CLNArray(i),i=1,nCLNTurnOff)
                    end if

                    if(nCLNTurnOn>0) then  
                        do i=1,nCLNTurnOn  ! read and write the next nCLNTurnOn lines
                            read(FnumTIB,'(a)') line
                            write(FNumTIBOut,'(a)') line ! always write next line as is  
                        end do
                    endif

                    ! make a list of CLN numbers to be turned on
                    nList=0
                    CLNArray(:)=0
                    do i=iCLNFirstEWIW,ncln
                        if(CLNType(i)==IW_lower) then
                            nList=nList+1
                            CLNArray(nlist)=i+ncells                          
                        else if(CLNType(i)==EW_lower) then
                            nList=nList+1
                            CLNArray(nlist)=i+ncells                          
                        endif
                    end do
                    write(FnumTIBout,'(i10,a)') (CLNArray(i),'            AVHEAD',i=1,nlist)
                    
               else if(iStressPeriod == nint(PESTStressPeriodInjWellFlips)) then ! turn on new cln lower screens for injection (IW) wells
                    ! determine how many new CLN's are going to be turned on 
                    
                    nEWIWTurnOn=nCLNTurnOn
                    do i=iCLNFirstEWIW,ncln
                        if(CLNType(i)==IW_lower) nEWIWTurnOn=nEWIWTurnOn+1
                    end do
                   
                    write(FNumTIBOut,'(3i10,a)')  nCLNTurnOff, nEWIWTurnOn, idum2, trim(TMPStr)
                    
                    if(nCLNTurnOff>0) then  
                        ! read and write the existing data
                        read(FnumTIB,'(10i10)') (CLNArray(i),i=1,nCLNTurnOff)
                        write(FnumTIBout,'(10i10)') (CLNArray(i),i=1,nCLNTurnOff)
                    end if

                    if(nCLNTurnOn>0) then  
                        do i=1,nCLNTurnOn  ! read and write the next nCLNTurnOn lines
                            read(FnumTIB,'(a)') line
                            write(FNumTIBOut,'(a)') line ! always write next line as is  
                        end do
                    endif

                    ! make a list of CLN numbers to be turned on
                    nList=0
                    CLNArray(:)=0
                    do i=iCLNFirstEWIW,ncln
                        if(CLNType(i)==IW_lower) then
                            nList=nList+1
                            CLNArray(nlist)=i+ncells                          
                        endif
                    end do
                    write(FnumTIBout,'(i10,a)') (CLNArray(i),'            AVHEAD',i=1,nlist)
                    
                else if(iStressPeriod == nint(PESTStressPeriodExtWellFlips)) then ! turn on new cln lower screens for extraction (EW) wells
                    ! determine how many new CLN's are going to be turned on 
                    nEWIWTurnOn=nCLNTurnOn
                    do i=iCLNFirstEWIW,ncln
                        if(CLNType(i)==EW_lower) nEWIWTurnOn=nEWIWTurnOn+1
                    end do
                   
                    write(FNumTIBOut,'(3i10,a)')  nCLNTurnOff, nEWIWTurnOn, idum2, trim(TMPStr)
                    
                    if(nCLNTurnOff>0) then  
                        ! read and write the existing data
                        read(FnumTIB,'(10i10)') (CLNArray(i),i=1,nCLNTurnOff)
                        write(FnumTIBout,'(10i10)') (CLNArray(i),i=1,nCLNTurnOff)
                    end if

                    if(nCLNTurnOn>0) then  
                        do i=1,nCLNTurnOn  ! read and write the next nCLNTurnOn lines
                            read(FnumTIB,'(a)') line
                            write(FNumTIBOut,'(a)') line ! always write next line as is  
                        end do
                    endif

                    ! make a list of CLN numbers to be turned on
                    nList=0
                    CLNArray(:)=0
                    do i=iCLNFirstEWIW,ncln
                        if(CLNType(i)==EW_lower) then
                            nList=nList+1
                            CLNArray(nlist)=i+ncells                          
                        endif
                    end do
                    write(FnumTIBout,'(i10,a)') (CLNArray(i),'            AVHEAD',i=1,nlist)
                    

                else    
                    write(FNumTIBOut,'(3i10,a)')  nCLNTurnOff, nCLNTurnOn, idum2, trim(TMPStr)
                    if(nCLNTurnOff>0) then  
                        read(FnumTIB,'(a)') line
                        write(FNumTIBOut,'(a)') line ! always write next line as is  
                        ! read and write the existing data
                        read(FnumTIB,'(10i10)') (CLNArray(i),i=1,nCLNTurnOff)
                        write(FnumTIBout,'(10i10)') (CLNArray(i),i=1,nCLNTurnOff)
                    end if
                    
                    if(nCLNTurnOn>0) then  
                        do i=1,nCLNTurnOn  ! read and write the next iDum2 lines
                            read(FnumTIB,'(a)') line
                            write(FNumTIBOut,'(a)') line ! always write next line as is  
                        end do
                    endif
                    if(iDum2>0) then  
                        continue
                    endif
                endif
                
               
            end if
            
            
        end do read_tib_file

        continue

        
    end subroutine MUSG_PEST_WellRatePenalties
    


    subroutine MUSG_PEST_UpdateWellRatePenalties(FnumTG) !--- Update rates in Modflow-USG well file
        implicit none 
        
        integer :: i, j

        integer :: FnumTG
        integer :: FnumPenalty
        character(MAXLBL) :: FNamePenalty
        integer :: nRange
        real(dr), allocatable :: MinPenalty(:)
        real(dr), allocatable :: MaxPenalty(:)
        real(dr), allocatable :: MinRange(:)
        real(dr), allocatable :: MaxRange(:)
        real(dr) :: InjRatePercentTargetLowerMin
        real(dr) :: InjRatePercentTargetLowerMax
        real(dr) :: InjratePercentPenaltyLowerMin
        real(dr) :: InjratePercentPenaltyLowerMax
        real(dr) :: InjRatePercentTargetHigher
        real(dr) :: InjratePercentPenaltyHigher
        real(dr) :: PESTStressPeriodInjWellFlips
        real(dr) :: PESTStressPeriodExtWellFlips


        real(dr) :: MinExtRate
        real(dr) :: MaxInjRateGPM
        
       
        ! CLN file
        integer :: FnumCLN
        character(MAXLBL) :: FNameCLN
        integer :: nCLN

        integer :: FNumWellCompOut
        character(MAXLBL) :: FNameWellCompOut

        ! Calculations
        real(dr) :: sumExtWellRates
        real(dr) :: ExtWellRatePenalty
        real(dr) :: sumInjWellRates
        real(dr) :: InjWellRatePenalty
        
        ! Modflow-USG well file
        integer :: iStressPeriod
        character(MAXLBL) :: line
        character(20), allocatable :: ThisWellName(:)
        
        
        real(dr) :: req_rate
        real(dr) :: act_rate
        integer :: icln
        
        real(dr) :: RateRatio
        
        
        ! CLN information file
        read(FNumTG,'(a)') FNameCLN
        call OpenAscii(FNumCLN,FNameCLN)
        call Msg( 'CLN file: '//trim(FNameCLN))

        read(FNumCLN,*) nCLN
        allocate(ThisWellName(nCLN))
        read(FNumCLN,'(a)') line   ! throw away line 2
        ! store well names
        do i=1,nCLN  
            read(FNumCLN,'(a)') line  
            l1=index(line,'Well = ')
            if(l1>0) then
                read(line(l1+7:),'(a)') ThisWellName(i)
            endif
        end do
        
        ! Penalties input file
        read(FnumTG,'(a)') FNamePenalty  
        call OpenAscii(FNumPenalty,FNamePenalty)
        call Msg( 'Penalties file: '//trim(FNamePenalty))
    
        
        read(FNumPenalty,*) MinExtRate
        write(TMPStr,'(4f15.3)') MinExtRate
        call Msg( 'Minimum extraction rate: '//trim(TMPStr))
        read(FNumPenalty,*) nRange
        allocate(MinRange(nRange), MaxRange(nRange), MinPenalty(nRange), MaxPenalty(nRange))
        do i=1,nRange
            read(FNumPenalty,*) MinRange(i), MaxRange(i), MinPenalty(i), MaxPenalty(i)
            write(TMPStr,'(4f15.3)') MinRange(i), MaxRange(i), MinPenalty(i), MaxPenalty(i)
            call Msg( 'Rate1 Rate2 Penalty1 Penalty2: '//trim(TMPStr))
            if(i>1) then
                if(Minrange(i)/=MaxRange(i-1)) then
                    write(ErrStr,'(a,2f10.2)') 'Range 1: ', MinRange(i-1), MaxRange(i-1)
                    call Msg(ErrStr)
                    write(ErrStr,'(a,2f10.2)') 'Range 2: ', MinRange(i), MaxRange(i)
                    call Msg(ErrStr)
                    call ErrMsg('Min range 2 not equal to max range 1')
                end if
                if(MinPenalty(i)/=MaxPenalty(i-1)) then
                    write(ErrStr,'(a,2f10.2)') 'Penalty 1: ', MinPenalty(i-1), MaxPenalty(i-1)
                    call Msg(ErrStr)
                    write(ErrStr,'(a,2f10.2)') 'Penalty 2: ', MinPenalty(i), MaxPenalty(i)
                    call Msg(ErrStr)
                    call ErrMsg('Min penalty 2 not equal to max penalty 1')
                end if
            end if
        end do

        read(FNumPenalty,*) MaxInjRateGPM
        write(TMPStr,*) MaxInjRateGPM
        call Msg( 'Maximum injection rate: '//trim(TMPStr))

        read(FNumPenalty,*) InjRatePercentTargetLowerMin,InjRatePercentTargetLowerMax, InjratePercentPenaltyLowerMin, InjratePercentPenaltyLowerMax
        write(TMPStr,'(4f15.3)') InjRatePercentTargetLowerMin,InjRatePercentTargetLowerMax, InjratePercentPenaltyLowerMin, InjratePercentPenaltyLowerMax
        call Msg( 'Injection Rate%1 Rate%2 Penalty1 Penalty2: '//trim(TMPStr))

        read(FNumPenalty,*) InjRatePercentTargetHigher, InjratePercentPenaltyHigher
        write(TMPStr,'(4f15.3)') InjRatePercentTargetHigher, InjratePercentPenaltyHigher
        call Msg( 'InjRatePercentTargetHigher InjratePercentPenaltyHigher: '//trim(TMPStr))

        read(FNumPenalty,*) PESTStressPeriodInjWellFlips
        write(TMPStr,*) nint(PESTStressPeriodInjWellFlips)
        call Msg( 'Injection (IW) Wells flip over at stress period: '//trim(TMPStr))
       
        read(FNumPenalty,*) PESTStressPeriodExtWellFlips
        write(TMPStr,*) nint(PESTStressPeriodExtWellFlips)
        call Msg( 'Extraction (EW) Wells flip over at stress period: '//trim(TMPStr))
       
        
        ! Wellcomp output  file
        read(FnumTG,'(a)') FNameWellCompOut
        call OpenAscii(FNumWellCompOut,FNameWellCompOut)
        call Msg( 'WellComp output file: '//trim(FNameWellCompOut))

        ! Penalties output file
        FNamePenalty='PenaltiesBySP.txt'
        call OpenAscii(FNumPenalty,FNamePenalty)
        call Msg( 'Penalties by stress period output file: '//trim(FNamePenalty))
        write(FNumPenalty,'(a)') '      SP#          ExtRate             ExtPenalty            InjRate           InjPenalty'

        read_wellcomp_file: do 
            read(FNumWellCompOut,'(a)',iostat=status) line
            if(status /= 0) then  ! end of file
                exit read_wellcomp_file
            endif
            
            l1=index(line,'Stress period = ')
            if(l1 > 0) then ! new stress period
                read(line(l1+16:),*) iStressPeriod
                sumExtWellRates=0.0d0
                sumInjWellRates=0.0d0
                stress_period: do 
                    read(FNumWellCompOut,'(a)') line  ! blank line
                    l1=index(line,'Node         Connection')
                    if(l1 > 0) then ! Start reading cln infoes
                        cln_lines: do 
                            read(FNumWellCompOut,'(a)') line  
                            l1=index(line,'CLN')
                            if(l1 > 0) then ! read and process cln info 
                                read(line,*) icln
                                read(line(l1+4:),*) req_rate,act_rate
                                if(index(ThisWellName(icln),'EW') > 0) then  ! extraction well 
                                    sumExtWellRates=sumExtWellrates+abs(act_rate)/192.5d0
                                else if(index(ThisWellName(icln),'IW') > 0) then ! injection well
                                    sumInjWellRates=sumInjWellRates+abs(act_rate)/192.5d0
                                end if
                            else
                                do j=1,nrange
                                    if(sumExtWellrates>= MinRange(j) .and. sumExtWellRates<= MaxRange(j)) then ! linear interpolation for penalty
                                        ExtWellRatePenalty= MinPenalty(j)+(sumExtWellRates-MinRange(j))/(MaxRange(j)-MinRange(j))*(MaxPenalty(j)-MinPenalty(j))
                                    endif
                                end do
                                
                                InjWellRatePenalty=0.0d0
                                if(sumExtWellRates == 0.0d0) then
                                    RateRatio=1.0d0
                                else
                                    RateRatio=sumInjWellRates/sumExtWellRates

                                    if(RateRatio < InjRatePercentTargetLowerMax) then
                                        InjWellRatePenalty=InjRatePercentPenaltyLowerMin+(InjRatePercentTargetLowerMax-RateRatio)/(InjRatePercentTargetLowerMax-InjRatePercentTargetLowerMin) &
                                &        * (InjRatePercentPenaltyLowerMax-InjRatePercentPenaltyLowerMin)     
                                    else if(RateRatio > InjRatePercentTargetHigher) then
                                        InjWellRatePenalty=InjRatePercentPenaltyHigher
                                    endif
                               
                                endif
       
                                write(FNumPenalty,'(i8,4f20.5)') IstressPeriod, sumExtWellRates, ExtWellRatePenalty, RateRatio, InjWellRatePenalty
                              
                                exit stress_period
                            endif
                        end do cln_lines
                    end if
                end do stress_period
            end if
            
        end do read_wellcomp_file
        
    end subroutine MUSG_PEST_UpdateWellRatePenalties

    subroutine MUSG_PEST_FlowSourceCapture(FnumTG) !--- Average flows for 
        USE IFPORT 
        implicit none 
        
        integer :: i, j

        LOGICAL(4) result 

        integer :: FnumTG
        integer :: FnumCSV
        character(MAXLBL) :: FNameCSV
        character(MAXLBL) :: FlowSourceDir
        character(MAXLBL) :: CMD_line
        character(MAXLBL) :: line
        integer :: nCSVFiles
        integer :: FnumCSVtmp
        integer :: nCells
                
        real(dr), allocatable :: Flowthrough(:,:)
        integer :: iDum
        
        integer :: iCSV
        integer :: iGroup
        integer :: FnumCellList
        character(MAXLBL), allocatable :: FNameCellList(:)
        real(dr), allocatable :: FS_Average(:,:)
        integer :: jCell
        integer :: nCellListFiles
        integer, allocatable :: nCountGroup(:)
        real(dr), allocatable :: StressPeriod(:)
        integer, allocatable :: indx_col(:)
 
 
        integer :: FnumGroupAverage
        character(MAXLBL) :: FNameGroupAverage

        read(FNumTG,'(a)') FlowSourceDir
        
        ! All of the .csv file in the directory FlowSourceDir are flowsource output files
        CMD_line='dir /b '//trim(FlowSourceDir)//'\*.csv > csvfiles'
        result = SYSTEMQQ(CMD_line)
       
        ! count the number of csv files
        FNameCSV='csvfiles'
        call OpenAscii(FnumCSV,FNameCSV)
        call Msg('CSV filenames file: '//trim(FNameCSV))
        nCSVFiles=0
        do 
            read(FnumCSV,'(a)',iostat=status) line
            if(status /= 0) then
                exit
            endif
            
            nCSVFiles=nCSVFiles+1
        end do
        
        ! count the number of cells in the last CSV file
        call OpenAscii(FnumCSVTmp,trim(FlowSourceDir)//'\'//trim(line))
        nCells=0
        do 
            read(FnumCSVTmp,'(a)',iostat=status) line
            if(status /= 0) then
                exit
            endif
            
            nCells=nCells+1
            
        end do
        
        nCells=nCells-1
        
        call FreeUnit(FnumCSVTmp)
        
        ! dimension flowsource arrays
        allocate(Flowthrough(nCSVFiles,nCells),StressPeriod(nCSVFiles),indx_col(nCSVFiles))
        
        rewind(FNumCSV)
        do i=1,nCSVFiles
            read(FnumCSV,'(a)',iostat=status) line
            
            ! Extract stress period from line
            l1=index(line,'_Time-')+6
            l2=l1+index(line(l1:),'_')-2
            read(line(l1:l2),*) StressPeriod(i)
            
            call OpenAscii(FnumCSVTmp,trim(FlowSourceDir)//'\'//trim(line))
            call Msg('CSV file: '//trim(FlowSourceDir)//'\'//trim(line))
            read(FnumCSVTmp,'(a)') line   ! throw away header
            do j=1,nCells
                read(FnumCSVTmp,*) iDum,Flowthrough(i,j)
            end do
        end do
        
        ! Process cell list files
        read(FNumTG,*) nCellListFiles
        allocate(nCountGroup(nCellListFiles),FS_average(nCSVFiles,nCellListFiles),FNameCellList(nCellListFiles))
        nCountGroup(:)=0
        FS_average(:,:)=0.0d0
        do iGroup=1,nCellListFiles
            read(FNumTG,'(a)') FNameCellList(iGroup)
            call OpenAscii(FnumCellList,FNameCellList(iGroup))
            call Msg('Cell list file: '//trim(FNameCellList(iGroup)))
            do 
                read(FnumCellList,*,iostat=status) jCell   
                if(status/=0) then
                    exit
                endif
                
                nCountGroup(iGroup)=nCountGroup(iGroup)+1
                
                do iCSV=1,nCSVFiles
                    FS_average(iCSV,iGroup)=FS_average(iCSV,iGroup)+Flowthrough(iCSV,jCell)
                end do
            
            end do
            
        end do
        
        ! Calculate average for each group and csv file
        do iGroup=1,nCellListFiles
            do iCSV=1,nCSVFiles
                FS_average(iCSV,iGroup)=FS_average(iCSV,iGroup)/nCountGroup(iGroup)
            end do
        end do
        
        ! Sort by StressPeriod. Order written to indx_col.
        call indexx3(nCSVFiles,StressPeriod,indx_col) 

        
        
        continue
       
        ! Write one output file per group
        do iGroup=1,nCellListFiles
            FNameGroupAverage='avg_'//trim(FNameCellList(iGroup))//'.csv'
            call OpenAscii(FNumGroupAverage,FNameGroupAverage)
            call Msg('Group FS average csv output file: '//trim(FNameGroupAverage))
            write(FNumGroupAverage,'(a)') "Stress Period, Average Capture"
            do iCSV=1,nCSVFiles
               write(FNumGroupAverage,'(f10.0,a,f12.5)') StressPeriod(indx_col(iCSV)),',',FS_average(indx_col(iCSV),iGroup)*100.0d0
            end do
            call FreeUnit(FNumGroupAverage)
        end do
            
       
      
    end subroutine MUSG_PEST_FlowSourceCapture

    subroutine MUSG_PEST_ExternalCodeExecute(FnumTG) !--- Execute an external program as defined in the .tg file. 
        USE IFPORT 
        implicit none 

        integer :: FnumTG
        LOGICAL(4) result 
        character(MAXLBL) :: CMD_line

        read(FNumTG,'(a)') CMD_line
        result = SYSTEMQQ(trim(CMD_line))

    end subroutine MUSG_PEST_ExternalCodeExecute
    
    !subroutine MUSG_PEST_CLNFileCalculations(FnumTG, musg_l) !--- Given an EI pumping well, calculate the CLN file entries
    !    USE IFPORT 
    !    implicit none 
    !
    !    type (MUSG_Project) musg_l
    !
    !
    !    integer :: i,j, k
    !
    !    !LOGICAL(4) result 
    !    !
    !    integer :: FnumTG
    !    
    !    !integer :: FnumCSV
    !    !character(MAXLBL) :: FNameCSV
    !    
    !    character(30) :: WellID
    !    real(dr) :: CellHeight
    !    integer :: iCellCurr
    !    real(dr) :: CurrTopElev
    !    
    !    read(FnumTG,'(a)') WellID
    !    do i=1, musg_l.nWellConst
    !        if(index(musg_l.NameWellConst(i),trim(WellID)) > 0) then
    !            
    !            do j=1,musg_l.gwf.nCell/musg_l.gwf.nLay  ! loop over the cells in layer 1
    !                if(musg_l.XWellConst(i) >= musg_l.gwf.X(Musg_l.gwf.ivertex(1,j)) .and. musg_l.XWellConst(i) <= musg_l.gwf.X(Musg_l.gwf.ivertex(4,j))) then
    !                
    !                    if(musg_l.YWellConst(i) >= musg_l.gwf.Y(Musg_l.gwf.ivertex(1,j)) .and. musg_l.YWellConst(i) <= musg_l.gwf.Y(Musg_l.gwf.ivertex(2,j))) then
    !                        iCellCurr=j
    !                        write(*,'(a,i8,3f15.3)') ' Cell x y z',iCellCurr, musg_l.gwf.xCell(iCellCurr), musg_l.gwf.yCell(iCellCurr), musg_l.gwf.zCell(iCellCurr)
    !                        write(*,*) ' k, vertex(k), Xvertex(k), Yvertex(k), Zvertex(k)'
    !                        do k=1,musg_l.gwf.m  
    !                            write(*,'(i2,i8,3f15.3)') k, Musg_l.gwf.ivertex(k,iCellCurr),musg_l.gwf.X(Musg_l.gwf.ivertex(k,iCellCurr)),musg_l.gwf.Y(Musg_l.gwf.ivertex(k,iCellCurr)),musg_l.gwf.Z(Musg_l.gwf.ivertex(k,iCellCurr))
    !                        end do
    !                        pause
    !                        CurrTopElev=musg_l.TopElevWellConst(i)
    !                        LayerLoop: do k=1,musg_l.gwf.nLay 
    !                            if(CurrTopElev > musg_l.gwf.Z(Musg_l.gwf.ivertex(8,iCellCurr)))  then  ! if current screen top > current cell bottom
    !                                if(musg_l.BotElevWellConst(i) > musg_l.gwf.Z(Musg_l.gwf.ivertex(8,iCellCurr))) then ! if current screen bot > current cell bottom
    !                                    CellHeight=CurrTopElev-musg_l.BotElevWellConst(i)
    !                                    write(*,*) iCellCurr, CellHeight
    !                                    exit LayerLoop
    !                                else
    !                                    CellHeight=CurrTopElev-musg_l.gwf.Z(Musg_l.gwf.ivertex(8,iCellCurr))
    !                                    write(*,*) iCellCurr, CellHeight
    !                                    CurrTopElev=musg_l.gwf.Z(Musg_l.gwf.ivertex(8,iCellCurr))
    !                                    iCellCurr=iCellCurr+musg_l.gwf.nCell/musg_l.gwf.nLay
    !                                endif
    !                            else
    !                                iCellCurr=iCellCurr+musg_l.gwf.nCell/musg_l.gwf.nLay
    !                                
    !                            end if
    !                        end do LayerLoop
    !                    end if
    !                end if 
    !            end do
    !            
    !           continue
    !        end if
    !        
    !    end do
    !    
    !end subroutine MUSG_PEST_CLNFileCalculations
    
    subroutine MUSG_PEST_EIWellCLNFileUpdate(FnumTG, musg_l) !--- Given new EI pumping wells, add screens as new CLN file entries
        USE IFPORT 
        implicit none 

        type (MUSG_Project) musg_l


        integer :: i,j, k

        integer :: FnumTG
        
        integer :: FnumCLN
        character(MAXLBL) :: FNameCLN
        integer :: FnumCLNout
        character(MAXLBL) :: FNameCLNout

        integer :: FNumFSCtl
        character(MAXLBL) :: FnameFSCtl
        integer :: FNumFSCtlOut
        character(MAXLBL) :: FnameFSCtlOut

        integer :: FNumWel
        character(MAXLBL) :: FnameWel
        integer :: FNumWelOut
        character(MAXLBL) :: FnameWelOut
        
        integer :: iCellCurr
        real(dr) :: CurrTopElev
        real(dr) :: CurrBotElev
        real(dr) :: CurrScreenLength
        logical :: ScreenFound
        integer :: nEIScreens
        real(dr) :: MeshBottom
        
        
        integer, allocatable :: IFNO(:)
        integer, allocatable :: IFTYP(:) 
        integer, allocatable :: IFDIR(:) 
        real(dr), allocatable :: FLENG(:) 
        real(dr), allocatable :: FELEV(:) 
        real(dr), allocatable :: FANGLE(:) 
        integer, allocatable :: IFLIN(:)
        integer, allocatable :: ICCWADI(:)
        
        integer, allocatable :: nCellList(:)
	    character(31), allocatable :: NameEIScreen(:)
        integer, allocatable :: CellNumber(:,:)
        real(dr), allocatable :: CellScreenLength(:,:)
        
        real(dr), allocatable :: StartingHeads(:)
        
        integer :: i1, i2, i3, i4, i5
        integer :: ICLNOrig, ICLNNew, NCONDUITYP
        integer :: ICellListOrig, ICellListNew
        integer :: nSum
        character(MAXSTRING) :: line
        
        logical :: WellFound

        integer :: IWellOrig, IWellNew

        
        allocate(IFNO(2*musg_l.n_EIWell), &
        &   IFTYP(2*musg_l.n_EIWell), &
        &   IFDIR(2*musg_l.n_EIWell), &
        &   FLENG(2*musg_l.n_EIWell), &
        &   FELEV(2*musg_l.n_EIWell), &
        &   FANGLE(2*musg_l.n_EIWell), &
        &   IFLIN(2*musg_l.n_EIWell), &
        &   ICCWADI(2*musg_l.n_EIWell))
        
        IFNO(:) = 0
        IFTYP(:) = 1
        IFDIR(:) = 0
        FLENG(:) = 0.0d0
        FELEV(:) = 0.0d0
        FANGLE(:) = 0.0d0
        IFLIN(:) = 1
        ICCWADI(:) = 0
        
        allocate(nCellList(2*musg_l.n_EIWell), &
        &   NameEIScreen(2*musg_l.n_EIWell), & 
        &   CellNumber(2*musg_l.n_EIWell,100),& 
        &   CellScreenLength(2*musg_l.n_EIWell,100))
        
        nEIScreens=0
        
        !-----------------------------------------------------------------------------------
        ! Find where screens fit in 3D Modflow domain
        WellSearch:do i=1, musg_l.n_EIWell
            call Msg('------------------------------------------------------------------------------')
            call Msg('EI Well: '//trim(musg_l.Name_EIWell(i)))
            write(TmpSTR,'(a, 2f15.3)') 'X Y: ',musg_l.X_EIWell(i), musg_l.Y_EIWell(i)
            call Msg(TmpSTR)
            CurrBotElev=musg_l.TopElev_EIWell(i)- musg_l.ScreenALength_EIWell(i)
            call Msg('       Elevation       Length      Comment')
            write(TmpSTR,'(f15.3,a)') musg_l.TopElev_EIWell(i), '           -        Screen A top elevation '
            call Msg(TmpSTR)
            write(TmpSTR,'(2f15.3,5x,a)') CurrBotElev, musg_l.ScreenALength_EIWell(i), 'Screen A bottom elevation, screen A length'
            call Msg(TmpSTR)
            CurrBotElev=CurrBotElev- musg_l.ScreenBOffset_EIWell(i)
            write(TmpSTR,'(2f15.3,5x,a)') CurrBotElev, musg_l.ScreenBOffset_EIWell(i), 'Screen B top elevation, screen B offset'
            call Msg(TmpSTR)
            CurrBotElev=CurrBotElev- musg_l.ScreenBLength_EIWell(i)
            write(TmpSTR,'(2f15.3,5x,a)') CurrBotElev, musg_l.ScreenBLength_EIWell(i), 'Screen B bottom elevation, screen B length'
            call Msg(TmpSTR)
            
            WellFound=.false.
                 
            do j=1,musg_l.gwf.nCell/musg_l.gwf.nLay  ! loop over the cells in layer 1
                if(musg_l.X_EIWell(i) >= musg_l.gwf.X(Musg_l.gwf.ivertex(1,j)) .and. musg_l.X_EIWell(i) <= musg_l.gwf.X(Musg_l.gwf.ivertex(4,j))) then
                    if(musg_l.Y_EIWell(i) >= musg_l.gwf.Y(Musg_l.gwf.ivertex(1,j)) .and. musg_l.Y_EIWell(i) <= musg_l.gwf.Y(Musg_l.gwf.ivertex(2,j))) then
                        iCellCurr=j
                        WellFound=.true.

                        write(TmpSTR,'(a,i8,a,2f15.3)') 'Found in cell ', iCellCurr,' at cell centroid X Y ', musg_l.gwf.xCell(iCellCurr), musg_l.gwf.yCell(iCellCurr)
                        call Msg(TmpSTR)

                        write(TmpSTR,'(a,2f15.3)') 'X range ', musg_l.gwf.X(Musg_l.gwf.ivertex(1,j)), musg_l.gwf.X(Musg_l.gwf.ivertex(4,j))
                        call Msg(TmpSTR)
                        write(TmpSTR,'(a,2f15.3)') 'Y range ', musg_l.gwf.Y(Musg_l.gwf.ivertex(1,j)), musg_l.gwf.Y(Musg_l.gwf.ivertex(2,j))
                        call Msg(TmpSTR)

                        
                        
                        call Msg(' Layer  Cell     Vertex         Z      Height')
                        write(TmpSTR,'(i5,i8,i8,f15.3)') 0, iCellCurr, 4,musg_l.gwf.Z(Musg_l.gwf.ivertex(4,iCellCurr))
                        call Msg(TmpSTR)
                        CurrTopElev=musg_l.gwf.Z(Musg_l.gwf.ivertex(4,iCellCurr))
                        do k=1,musg_l.gwf.nLay                             
                            write(TmpSTR,'(i5,i8,i8,5f15.3)') k, iCellCurr,8, musg_l.gwf.Z(Musg_l.gwf.ivertex(8,iCellCurr)),CurrTopElev-musg_l.gwf.Z(Musg_l.gwf.ivertex(8,iCellCurr))
                            call Msg(TmpSTR)
                            if(k==musg_l.gwf.nLay) then
                                MeshBottom= musg_l.gwf.Z(Musg_l.gwf.ivertex(8,iCellCurr))
                            endif
                            CurrTopElev=musg_l.gwf.Z(Musg_l.gwf.ivertex(8,iCellCurr))
                            iCellCurr=iCellCurr+musg_l.gwf.nCell/musg_l.gwf.nLay
                        end do
                        
                        iCellCurr=j

                        
                        ! Top screen A
                        CurrTopElev=musg_l.TopElev_EIWell(i)
                        CurrBotElev=CurrTopElev-musg_l.ScreenALength_EIWell(i)
                        CurrScreenLength=0.0d0
                        ScreenFound=.false.
                        LayerLoop1: do k=1,musg_l.gwf.nLay 
                            if(CurrTopElev > musg_l.gwf.Z(Musg_l.gwf.ivertex(8,iCellCurr)))  then  ! if current screen top > current cell bottom
                                if(.not. ScreenFound) then
                                    nEIScreens=nEIScreens+1
                                    ScreenFound=.true.
                                    call Msg(' +Scr A    Cell      ScreenLength')
                                    nCellList(nEIScreens)=0
                                    NameEIScreen(nEIScreens)=trim(musg_l.Name_EIWell(i))//'_A'
                                end if
                                if(CurrBotElev > musg_l.gwf.Z(Musg_l.gwf.ivertex(8,iCellCurr))) then ! if current screen bot > current cell bottom
                                    nCellList(nEIScreens)=nCellList(nEIScreens)+1  
                                    CellNumber(nEIScreens,nCellList(nEIScreens))= iCellCurr
                                    CellScreenLength(nEIScreens,nCellList(nEIScreens)) = CurrTopElev-CurrBotElev
                                    write(TmpSTR,'(i5,i8,f15.3)') nEIScreens,CellNumber(nEIScreens,nCellList(nEIScreens)), CellScreenLength(nEIScreens,nCellList(nEIScreens))
                                    call Msg(TmpSTR)
                                    CurrScreenLength=CurrScreenLength+CellScreenLength(nEIScreens,nCellList(nEIScreens))
                                    exit LayerLoop1
                                else

                                    nCellList(nEIScreens)=nCellList(nEIScreens)+1  
                                    CellNumber(nEIScreens,nCellList(nEIScreens))= iCellCurr
                                    CellScreenLength(nEIScreens,nCellList(nEIScreens)) = CurrTopElev-musg_l.gwf.Z(Musg_l.gwf.ivertex(8,iCellCurr))
                                    write(TmpSTR,'(i5,i8,f15.3)') nEIScreens,CellNumber(nEIScreens,nCellList(nEIScreens)), CellScreenLength(nEIScreens,nCellList(nEIScreens))
                                    call Msg(TmpSTR)
                                    CurrScreenLength=CurrScreenLength+CellScreenLength(nEIScreens,nCellList(nEIScreens))
                                    
                                    CurrTopElev=musg_l.gwf.Z(Musg_l.gwf.ivertex(8,iCellCurr))
                                    iCellCurr=iCellCurr+musg_l.gwf.nCell/musg_l.gwf.nLay
                                endif
                            else
                                iCellCurr=iCellCurr+musg_l.gwf.nCell/musg_l.gwf.nLay
                                if(iCellCurr > musg_l.gwf.nCell .and. ScreenFound) then
                                    nEIScreens=nEIScreens+1
                                endif
                                    
                            end if
                        end do LayerLoop1
                        
                        if(ScreenFound) then
                            FLENG(nEIScreens) = CurrScreenLength
                            FELEV(nEIScreens) = CurrBotElev
                            
                            If(FELEV(nEIScreens)< MeshBottom) then
                                write(TmpSTR,'(a,f15.3,a,f15.3)') 'Bottom of screen FELEV ',FELEV(nEIScreens),' below mesh bottom ',MeshBottom
                                call ErrMsg(TmpSTR)
                            endif
                            
                            call Msg('         FLENG         FELEV')
                            write(TmpSTR,'(2f15.3)') FLENG(nEIScreens),FELEV(nEIScreens)
                            call Msg(TmpSTR)
                        end if
                        
                        
                        ! Screen B
                        CurrTopElev=CurrBotElev-musg_l.ScreenBOffset_EIWell(i)
                        CurrBotElev=CurrTopElev-musg_l.ScreenBLength_EIWell(i)
                        CurrScreenLength=0.0d0
                        ScreenFound=.false.
                        iCellCurr=j
                        LayerLoop2: do k=1,musg_l.gwf.nLay 
                            if(CurrTopElev > musg_l.gwf.Z(Musg_l.gwf.ivertex(8,iCellCurr)))  then  ! if current screen top > current cell bottom
                                if(.not. ScreenFound) then
                                    nEIScreens=nEIScreens+1
                                    ScreenFound=.true.
                                    call Msg(' +Scr B    Cell      ScreenLength')
                                    nCellList(nEIScreens)=0
                                    NameEIScreen(nEIScreens)=trim(musg_l.Name_EIWell(i))//'_B'
                                endif
                                if(CurrBotElev > musg_l.gwf.Z(Musg_l.gwf.ivertex(8,iCellCurr))) then ! if current screen bot > current cell bottom

                                    nCellList(nEIScreens)=nCellList(nEIScreens)+1  
                                    CellNumber(nEIScreens,nCellList(nEIScreens))= iCellCurr
                                    CellScreenLength(nEIScreens,nCellList(nEIScreens)) = CurrTopElev-CurrBotElev
                                    write(TmpSTR,'(i5,i8,f15.3)') nEIScreens,CellNumber(nEIScreens,nCellList(nEIScreens)), CellScreenLength(nEIScreens,nCellList(nEIScreens))
                                    call Msg(TmpSTR)
                                    CurrScreenLength=CurrScreenLength+CellScreenLength(nEIScreens,nCellList(nEIScreens))
                                    exit LayerLoop2
                                else

                                    nCellList(nEIScreens)=nCellList(nEIScreens)+1  
                                    CellNumber(nEIScreens,nCellList(nEIScreens))= iCellCurr
                                    CellScreenLength(nEIScreens,nCellList(nEIScreens)) = CurrTopElev-musg_l.gwf.Z(Musg_l.gwf.ivertex(8,iCellCurr))
                                    write(TmpSTR,'(i5,i8,f15.3)') nEIScreens,CellNumber(nEIScreens,nCellList(nEIScreens)), CellScreenLength(nEIScreens,nCellList(nEIScreens))
                                    call Msg(TmpSTR)
                                    CurrScreenLength=CurrScreenLength+CellScreenLength(nEIScreens,nCellList(nEIScreens))
                                    CurrTopElev=musg_l.gwf.Z(Musg_l.gwf.ivertex(8,iCellCurr))
                                    iCellCurr=iCellCurr+musg_l.gwf.nCell/musg_l.gwf.nLay
                                endif
                            else
                                iCellCurr=iCellCurr+musg_l.gwf.nCell/musg_l.gwf.nLay
                                if(iCellCurr > musg_l.gwf.nCell .and. ScreenFound) then
                                    nEIScreens=nEIScreens+1
                                endif
                                    
                            end if
                        end do LayerLoop2

                         if(ScreenFound) then
                            FLENG(nEIScreens) = CurrScreenLength
                            FELEV(nEIScreens) = CurrBotElev
 
                            If(FELEV(nEIScreens)< MeshBottom) then
                                write(TmpSTR,'(a,f15.3,a,f15.3)') 'Bottom of screen FELEV ',FELEV(nEIScreens),' below mesh bottom ',MeshBottom
                                call ErrMsg(TmpSTR)
                            endif
                            
                            call Msg('         FLENG         FELEV')
                            write(TmpSTR,'(2f15.3)') FLENG(nEIScreens),FELEV(nEIScreens)
                            call Msg(TmpSTR)
                        end if

                    end if
                end if 
                if(WellFound) cycle WellSearch
            end do
        end do WellSearch
        
        !-----------------------------------------------------------------------------------
        ! Insert wells in Modflow-USG CLN input file
        read(FnumTG,'(a)') FnameCLN  
        call OpenAscii(FnumCLN,FnameCLN)
        call Msg( 'Modflow-USG CLN input file: '//trim(FNameCLN))
        
        ! Open new CLN well output file
        FNameCLNOut='out_'//FNameCLN  
        call OpenAscii(FnumCLNOut,FNameCLNOut)
        call Msg( 'Modflow-USG CLN output file: '//trim(FNameCLNOut))
        
        ! line 1 of CLN file 
        read(FnumCLN,*) iCLNOrig,i1,i2,i3,i4,i5,iCellListOrig,NCONDUITYP
        iCLNNew=iCLNOrig+nEIScreens
        
        allocate(StartingHeads(iCLNNew))
        
        nSum=0
        do i=1,nEIScreens
            nSum=nSum+nCellList(i)
        end do
        iCellListNew=iCellListOrig+nSum
        write(FnumCLNOut,'(8i8)') iCLNNew,i1,i2,i3,i4,i5,iCellListNew,NCONDUITYP
        
        read(FnumCLN,'(a)') line
        write(FnumCLNOut,'(a)') line
        
        do i=1,iCLNOrig
            read(FnumCLN,'(a)') line
            i2=index(line,'Well =')+6
            if(index(line(i2:),'IW')>0) then
                call ErrMsg('Original CLN well name '//trim(line(i2:))//' must not contain string "IW"') 
            endif 
            if(index(line(i2:),'EW')>0) then
                call ErrMsg('Original CLN well name '//trim(line(i2:))//' must not contain string "EW"') 
            endif 

            write(FnumCLNOut,'(a)') line
        end do
        
        i1=index(line,'              IFNO')
        i2=index(line,'Well =')+6
        
        do i=1,nEIScreens
            write(FnumCLNOut,'(i8,i3,i3,f11.4,f15.4,f15.6,i10,i4,a,a)') iCLNOrig+i, IFTYP(i), IFDIR(i), FLENG(i), FELEV(i), FANGLE(i), IFLIN(i), ICCWADI(i), line(i1:i2), trim(NameEIScreen(i))
        end do

        do i=1,iCellListOrig
            read(FnumCLN,'(a)') line
            write(FnumCLNOut,'(a)') line
        end do

        i1=index(line,'              IFNOD')
        i2=index(line,'Well =')+6

        do i=1,nEIScreens
            do j=1,nCellList(i)
                !write(FnumCLNOut,*) iCLNOrig+i, CellNumber(i,j), 0, 0.0, CellScreenLength(i,j), 1.0, 0, line(i1:i2), trim(NameEIScreen(i))
                write(FnumCLNOut,'(i8,i8,i3,f11.6,g19.7,f11.6,i9,a,a)') iCLNOrig+i, CellNumber(i,j), 0, 0.0, CellScreenLength(i,j), 1.0, 0, line(i1:i2), trim(NameEIScreen(i))
            end do
        end do

        do i=1,NCONDUITYP   ! always NCONDUITYP lines.  Never less than 1.
            read(FnumCLN,'(a)') line
            write(FnumCLNOut,'(a)') line
        end do
        
        read(FnumCLN,'(a)') line  ! always an IBOUND line
        write(FnumCLNOut,'(a)') line
        

        ! starting heads  iCLNNew=iCLNOrig+nEIScreens
        read(FnumCLN,'(a)') line  ! always a starting heads header line
        write(FnumCLNOut,'(a)') line
        read(FnumCLN,*) (StartingHeads(i),i=1,iCLNOrig)
        do i=iCLNOrig+1,iCLNNew
            StartingHeads(i)=StartingHeads(iCLNOrig)
        end do     
        write(FnumCLNOut,*) (StartingHeads(i),i=1,iCLNNew)

        ! in transport case may be more data so read/write to end of file
        do 
            read(FnumCLN,'(a)',iostat=status) line
            if(status/=0) then
                exit
            endif
            write(FnumCLNOut,'(a)') line
        end do
        
        
        !-----------------------------------------------------------------------------------
        ! Read well file and insert placeholders for EW/IW wells

        ! Modflow Well file
        read(FnumTG,'(a)') FnameWel
        call OpenAscii(FNumWel,FnameWel)
        call Msg( 'Modflow well file: '//trim(FnameWel))
        
        ! Open new FlowSource control output file
        FnameWelOut='place_'//FnameWel  
        call OpenAscii(FnumWelOut,FnameWelOut)
        call Msg( 'Modflow well output file: '//trim(FnameWelOut))

        ! Always keep first line
        read(FNumWel,'(a)') line
        write(FNumWelout,'(a)') line
        
        do 
            read(FNumWel,'(a)',iostat=status) line
            if(status/=0) exit

            read(line,*) i1,i2,iWellOrig
            iWellNew=iWellOrig+musg_l.n_EIWell

            write(FNumWelout,'(3i10,a)') i1,i2,iWellNew,trim(line(31:))
            do i=1,IwellOrig
                read(FNumWel,'(a)') line
                write(FNumWelout,'(a)') line
            end do
            i1=index(line,'well =')+6
            do i=1,musg_l.n_EIWell
                write(FNumWelout,'(i11,g14.7,a,a)') iCLNOrig+i*2-1,0.0d0,line(26:i1),trim(musg_l.Name_EIWell(i))
            end do     
               
            
        end do
        continue
      
        
        !-----------------------------------------------------------------------------------
        ! Flowsource control file
        read(FnumTG,'(a)') FnameFSCtl
        call OpenAscii(FNumFSCtl,FnameFSCtl)
        call Msg( 'FlowSource control file: '//trim(FnameFSCtl))
        
        ! Open new FlowSource control output file
        FnameFSCtlOut='out_'//FnameFSCtl  
        call OpenAscii(FnumFSCtlOut,FnameFSCtlOut)
        call Msg( 'FlowSource control output file: '//trim(FnameFSCtlOut))
        
        do 
            read(FNumFSCtl,'(a)') line
            write(FNumFSCtlout,'(a)') line
            if(index(line,'# note that if forward tracking mode is selected') > 0) then  ! insert cell lists
                do i=1,nEIScreens,2
                    if(index(NameEIScreen(i),'EW')>0) then
                        write(FNumFSCtlout,'(a)') ' '
                        write(FNumFSCtlout,'(a)') '# '//trim(NameEIScreen(i))//', '//trim(NameEIScreen(i+1))
                        do j=1,nCellList(i)
                            write(FNumFSCtlout,'(a,i8)') 'FlowDestination = ',CellNumber(i,j)
                        end do
                        do j=1,nCellList(i+1)
                            if(j==1) then
                                if(CellNumber(i+1,j) /= CellNumber(i,nCellList(i)) ) write(FNumFSCtlout,'(a,i8)') 'FlowDestination = ',CellNumber(i+1,j)
                            else
                                write(FNumFSCtlout,'(a,i8)') 'FlowDestination = ',CellNumber(i+1,j)
                            endif    
                        end do
                    end if
                end do
                exit
            end if
        end do

        end_ctl: do 
            read(FNumFSCtl,'(a)') line
            if(index(line,'#--------------------------') > 0) then  ! add end of ctl file
                write(FNumFSCtlout,'(a)') ' '
                write(FNumFSCtlout,'(a)') line
               
                do 
                    read(FNumFSCtl,'(a)',iostat=status) line
                    if(status /= 0) then
                        exit end_ctl
                    endif 
                    
                    write(FNumFSCtlout,'(a)') line
                   
                end do
            end if
        end do end_ctl
        
        
    end subroutine MUSG_PEST_EIWellCLNFileUpdate
    subroutine old_MUSG_PEST_EIWellCLNFileUpdate(FnumTG, musg_l) !--- Given new EI pumping wells, add screens as new CLN file entries
        USE IFPORT 
        implicit none 

        type (MUSG_Project) musg_l


        integer :: i,j, k

        integer :: FnumTG
        
        integer :: FnumCLN
        character(MAXLBL) :: FNameCLN
        
        integer :: FnumCLNout
        character(MAXLBL) :: FNameCLNout

        integer :: FNumFSCtl
        character(MAXLBL) :: FnameFSCtl
        
        integer :: FNumFSCtlOut
        character(MAXLBL) :: FnameFSCtlOut
        
        integer :: iCellCurr
        real(dr) :: CurrTopElev
        real(dr) :: CurrBotElev
        real(dr) :: CurrScreenLength
        logical :: ScreenFound
        integer :: nEIScreens
        real(dr) :: MeshBottom
        
        
        integer, allocatable :: IFNO(:)
        integer, allocatable :: IFTYP(:) 
        integer, allocatable :: IFDIR(:) 
        real(dr), allocatable :: FLENG(:) 
        real(dr), allocatable :: FELEV(:) 
        real(dr), allocatable :: FANGLE(:) 
        integer, allocatable :: IFLIN(:)
        integer, allocatable :: ICCWADI(:)
        
        integer, allocatable :: nCellList(:)
	    character(31), allocatable :: NameEIScreen(:)
        integer, allocatable :: CellNumber(:,:)
        real(dr), allocatable :: CellScreenLength(:,:)
        
        integer :: i1, i2, i3, i4, i5
        integer :: ICLNOrig, ICLNNew, NCONDUITYP
        integer :: ICellListOrig, ICellListNew
        integer :: nSum
        character(MAXSTRING) :: line
        
        logical :: WellFound

        
        allocate(IFNO(2*musg_l.n_EIWell), &
        &   IFTYP(2*musg_l.n_EIWell), &
        &   IFDIR(2*musg_l.n_EIWell), &
        &   FLENG(2*musg_l.n_EIWell), &
        &   FELEV(2*musg_l.n_EIWell), &
        &   FANGLE(2*musg_l.n_EIWell), &
        &   IFLIN(2*musg_l.n_EIWell), &
        &   ICCWADI(2*musg_l.n_EIWell))
        
        IFNO(:) = 0
        IFTYP(:) = 1
        IFDIR(:) = 0
        FLENG(:) = 0.0d0
        FELEV(:) = 0.0d0
        FANGLE(:) = 0.0d0
        IFLIN(:) = 1
        ICCWADI(:) = 0
        
        allocate(nCellList(2*musg_l.n_EIWell), &
        &   NameEIScreen(2*musg_l.n_EIWell), & 
        &   CellNumber(2*musg_l.n_EIWell,100),& 
        &   CellScreenLength(2*musg_l.n_EIWell,100))
        
        nEIScreens=0
        
        WellSearch:do i=1, musg_l.n_EIWell
            call Msg('------------------------------------------------------------------------------')
            call Msg('EI Well: '//trim(musg_l.Name_EIWell(i)))
            write(TmpSTR,'(a, 2f15.3)') 'X Y: ',musg_l.X_EIWell(i), musg_l.Y_EIWell(i)
            call Msg(TmpSTR)
            CurrBotElev=musg_l.TopElev_EIWell(i)- musg_l.ScreenALength_EIWell(i)
            call Msg('       Elevation       Length      Comment')
            write(TmpSTR,'(f15.3,a)') musg_l.TopElev_EIWell(i), '           -        Screen A top elevation '
            call Msg(TmpSTR)
            write(TmpSTR,'(2f15.3,5x,a)') CurrBotElev, musg_l.ScreenALength_EIWell(i), 'Screen A bottom elevation, screen A length'
            call Msg(TmpSTR)
            CurrBotElev=CurrBotElev- musg_l.ScreenBOffset_EIWell(i)
            write(TmpSTR,'(2f15.3,5x,a)') CurrBotElev, musg_l.ScreenBOffset_EIWell(i), 'Screen B top elevation, screen B offset'
            call Msg(TmpSTR)
            CurrBotElev=CurrBotElev- musg_l.ScreenBLength_EIWell(i)
            write(TmpSTR,'(2f15.3,5x,a)') CurrBotElev, musg_l.ScreenBLength_EIWell(i), 'Screen B bottom elevation, screen B length'
            call Msg(TmpSTR)
            
            WellFound=.false.
                 
            do j=1,musg_l.gwf.nCell/musg_l.gwf.nLay  ! loop over the cells in layer 1
                if(musg_l.X_EIWell(i) >= musg_l.gwf.X(Musg_l.gwf.ivertex(1,j)) .and. musg_l.X_EIWell(i) <= musg_l.gwf.X(Musg_l.gwf.ivertex(4,j))) then
                    if(musg_l.Y_EIWell(i) >= musg_l.gwf.Y(Musg_l.gwf.ivertex(1,j)) .and. musg_l.Y_EIWell(i) <= musg_l.gwf.Y(Musg_l.gwf.ivertex(2,j))) then
                        iCellCurr=j
                        WellFound=.true.

                        write(TmpSTR,'(a,i8,a,2f15.3)') 'Found in cell ', iCellCurr,' at cell centroid X Y ', musg_l.gwf.xCell(iCellCurr), musg_l.gwf.yCell(iCellCurr)
                        call Msg(TmpSTR)

                        write(TmpSTR,'(a,2f15.3)') 'X range ', musg_l.gwf.X(Musg_l.gwf.ivertex(1,j)), musg_l.gwf.X(Musg_l.gwf.ivertex(4,j))
                        call Msg(TmpSTR)
                        write(TmpSTR,'(a,2f15.3)') 'Y range ', musg_l.gwf.Y(Musg_l.gwf.ivertex(1,j)), musg_l.gwf.Y(Musg_l.gwf.ivertex(2,j))
                        call Msg(TmpSTR)

                        
                        
                        call Msg(' Layer  Cell     Vertex         Z      Height')
                        write(TmpSTR,'(i5,i8,i8,f15.3)') 0, iCellCurr, 4,musg_l.gwf.Z(Musg_l.gwf.ivertex(4,iCellCurr))
                        call Msg(TmpSTR)
                        CurrTopElev=musg_l.gwf.Z(Musg_l.gwf.ivertex(4,iCellCurr))
                        do k=1,musg_l.gwf.nLay                             
                            write(TmpSTR,'(i5,i8,i8,5f15.3)') k, iCellCurr,8, musg_l.gwf.Z(Musg_l.gwf.ivertex(8,iCellCurr)),CurrTopElev-musg_l.gwf.Z(Musg_l.gwf.ivertex(8,iCellCurr))
                            call Msg(TmpSTR)
                            if(k==musg_l.gwf.nLay) then
                                MeshBottom= musg_l.gwf.Z(Musg_l.gwf.ivertex(8,iCellCurr))
                            endif
                            CurrTopElev=musg_l.gwf.Z(Musg_l.gwf.ivertex(8,iCellCurr))
                            iCellCurr=iCellCurr+musg_l.gwf.nCell/musg_l.gwf.nLay
                        end do
                        
                        iCellCurr=j

                        
                        ! Top screen A
                        CurrTopElev=musg_l.TopElev_EIWell(i)
                        CurrBotElev=CurrTopElev-musg_l.ScreenALength_EIWell(i)
                        CurrScreenLength=0.0d0
                        ScreenFound=.false.
                        LayerLoop1: do k=1,musg_l.gwf.nLay 
                            if(CurrTopElev > musg_l.gwf.Z(Musg_l.gwf.ivertex(8,iCellCurr)))  then  ! if current screen top > current cell bottom
                                if(.not. ScreenFound) then
                                    nEIScreens=nEIScreens+1
                                    ScreenFound=.true.
                                    call Msg(' +Scr A    Cell      ScreenLength')
                                    nCellList(nEIScreens)=0
                                    NameEIScreen(nEIScreens)=trim(musg_l.Name_EIWell(i))//'_A'
                                end if
                                if(CurrBotElev > musg_l.gwf.Z(Musg_l.gwf.ivertex(8,iCellCurr))) then ! if current screen bot > current cell bottom
                                    nCellList(nEIScreens)=nCellList(nEIScreens)+1  
                                    CellNumber(nEIScreens,nCellList(nEIScreens))= iCellCurr
                                    CellScreenLength(nEIScreens,nCellList(nEIScreens)) = CurrTopElev-CurrBotElev
                                    write(TmpSTR,'(i5,i8,f15.3)') nEIScreens,CellNumber(nEIScreens,nCellList(nEIScreens)), CellScreenLength(nEIScreens,nCellList(nEIScreens))
                                    call Msg(TmpSTR)
                                    CurrScreenLength=CurrScreenLength+CellScreenLength(nEIScreens,nCellList(nEIScreens))
                                    exit LayerLoop1
                                else

                                    nCellList(nEIScreens)=nCellList(nEIScreens)+1  
                                    CellNumber(nEIScreens,nCellList(nEIScreens))= iCellCurr
                                    CellScreenLength(nEIScreens,nCellList(nEIScreens)) = CurrTopElev-musg_l.gwf.Z(Musg_l.gwf.ivertex(8,iCellCurr))
                                    write(TmpSTR,'(i5,i8,f15.3)') nEIScreens,CellNumber(nEIScreens,nCellList(nEIScreens)), CellScreenLength(nEIScreens,nCellList(nEIScreens))
                                    call Msg(TmpSTR)
                                    CurrScreenLength=CurrScreenLength+CellScreenLength(nEIScreens,nCellList(nEIScreens))
                                    
                                    CurrTopElev=musg_l.gwf.Z(Musg_l.gwf.ivertex(8,iCellCurr))
                                    iCellCurr=iCellCurr+musg_l.gwf.nCell/musg_l.gwf.nLay
                                endif
                            else
                                iCellCurr=iCellCurr+musg_l.gwf.nCell/musg_l.gwf.nLay
                                if(iCellCurr > musg_l.gwf.nCell .and. ScreenFound) then
                                    nEIScreens=nEIScreens+1
                                endif
                                    
                            end if
                        end do LayerLoop1
                        
                        if(ScreenFound) then
                            FLENG(nEIScreens) = CurrScreenLength
                            FELEV(nEIScreens) = CurrBotElev
                            
                            If(FELEV(nEIScreens)< MeshBottom) then
                                write(TmpSTR,'(a,f15.3,a,f15.3)') 'Bottom of screen FELEV ',FELEV(nEIScreens),' below mesh bottom ',MeshBottom
                                call ErrMsg(TmpSTR)
                            endif
                            
                            call Msg('         FLENG         FELEV')
                            write(TmpSTR,'(2f15.3)') FLENG(nEIScreens),FELEV(nEIScreens)
                            call Msg(TmpSTR)
                        end if
                        
                        
                        ! Screen B
                        CurrTopElev=CurrBotElev-musg_l.ScreenBOffset_EIWell(i)
                        CurrBotElev=CurrTopElev-musg_l.ScreenBLength_EIWell(i)
                        CurrScreenLength=0.0d0
                        ScreenFound=.false.
                        iCellCurr=j
                        LayerLoop2: do k=1,musg_l.gwf.nLay 
                            if(CurrTopElev > musg_l.gwf.Z(Musg_l.gwf.ivertex(8,iCellCurr)))  then  ! if current screen top > current cell bottom
                                if(.not. ScreenFound) then
                                    nEIScreens=nEIScreens+1
                                    ScreenFound=.true.
                                    call Msg(' +Scr B    Cell      ScreenLength')
                                    nCellList(nEIScreens)=0
                                    NameEIScreen(nEIScreens)=trim(musg_l.Name_EIWell(i))//'_B'
                                endif
                                if(CurrBotElev > musg_l.gwf.Z(Musg_l.gwf.ivertex(8,iCellCurr))) then ! if current screen bot > current cell bottom

                                    nCellList(nEIScreens)=nCellList(nEIScreens)+1  
                                    CellNumber(nEIScreens,nCellList(nEIScreens))= iCellCurr
                                    CellScreenLength(nEIScreens,nCellList(nEIScreens)) = CurrTopElev-CurrBotElev
                                    write(TmpSTR,'(i5,i8,f15.3)') nEIScreens,CellNumber(nEIScreens,nCellList(nEIScreens)), CellScreenLength(nEIScreens,nCellList(nEIScreens))
                                    call Msg(TmpSTR)
                                    CurrScreenLength=CurrScreenLength+CellScreenLength(nEIScreens,nCellList(nEIScreens))
                                    exit LayerLoop2
                                else

                                    nCellList(nEIScreens)=nCellList(nEIScreens)+1  
                                    CellNumber(nEIScreens,nCellList(nEIScreens))= iCellCurr
                                    CellScreenLength(nEIScreens,nCellList(nEIScreens)) = CurrTopElev-musg_l.gwf.Z(Musg_l.gwf.ivertex(8,iCellCurr))
                                    write(TmpSTR,'(i5,i8,f15.3)') nEIScreens,CellNumber(nEIScreens,nCellList(nEIScreens)), CellScreenLength(nEIScreens,nCellList(nEIScreens))
                                    call Msg(TmpSTR)
                                    CurrScreenLength=CurrScreenLength+CellScreenLength(nEIScreens,nCellList(nEIScreens))
                                    CurrTopElev=musg_l.gwf.Z(Musg_l.gwf.ivertex(8,iCellCurr))
                                    iCellCurr=iCellCurr+musg_l.gwf.nCell/musg_l.gwf.nLay
                                endif
                            else
                                iCellCurr=iCellCurr+musg_l.gwf.nCell/musg_l.gwf.nLay
                                if(iCellCurr > musg_l.gwf.nCell .and. ScreenFound) then
                                    nEIScreens=nEIScreens+1
                                endif
                                    
                            end if
                        end do LayerLoop2

                         if(ScreenFound) then
                            FLENG(nEIScreens) = CurrScreenLength
                            FELEV(nEIScreens) = CurrBotElev
 
                            If(FELEV(nEIScreens)< MeshBottom) then
                                write(TmpSTR,'(a,f15.3,a,f15.3)') 'Bottom of screen FELEV ',FELEV(nEIScreens),' below mesh bottom ',MeshBottom
                                call ErrMsg(TmpSTR)
                            endif
                            
                            call Msg('         FLENG         FELEV')
                            write(TmpSTR,'(2f15.3)') FLENG(nEIScreens),FELEV(nEIScreens)
                            call Msg(TmpSTR)
                        end if

                    end if
                end if 
                if(WellFound) cycle WellSearch
            end do
        end do WellSearch
        
         ! Modflow-USG CLN input file
        read(FnumTG,'(a)') FnameCLN  
        call OpenAscii(FnumCLN,FnameCLN)
        call Msg( 'Modflow-USG CLN input file: '//trim(FNameCLN))
        
        ! Open new CLN well output file
        FNameCLNOut='out_'//FNameCLN  
        call OpenAscii(FnumCLNOut,FNameCLNOut)
        call Msg( 'Modflow-USG CLN output file: '//trim(FNameCLNOut))
        
        ! line 1 of CLN file 
        read(FnumCLN,*) iCLNOrig,i1,i2,i3,i4,i5,iCellListOrig,NCONDUITYP
        iCLNNew=iCLNOrig+nEIScreens
        nSum=0
        do i=1,nEIScreens
            nSum=nSum+nCellList(i)
        end do
        iCellListNew=iCellListOrig+nSum
        write(FnumCLNOut,'(8i8)') iCLNNew,i1,i2,i3,i4,i5,iCellListNew,NCONDUITYP
        
        read(FnumCLN,'(a)') line
        write(FnumCLNOut,'(a)') line
        
        do i=1,iCLNOrig
            read(FnumCLN,'(a)') line
            i2=index(line,'Well =')+6
            if(index(line(i2:),'IW')>0) then
                call ErrMsg('Original CLN well name '//trim(line(i2:))//' must not contain string "IW"') 
            endif 
            if(index(line(i2:),'EW')>0) then
                call ErrMsg('Original CLN well name '//trim(line(i2:))//' must not contain string "EW"') 
            endif 

            write(FnumCLNOut,'(a)') line
        end do
        
        i1=index(line,'              IFNO')
        i2=index(line,'Well =')+6
        
        do i=1,nEIScreens
            write(FnumCLNOut,'(i8,i3,i3,f11.4,f15.4,f15.6,i10,i4,a,a)') iCLNOrig+i, IFTYP(i), IFDIR(i), FLENG(i), FELEV(i), FANGLE(i), IFLIN(i), ICCWADI(i), line(i1:i2), trim(NameEIScreen(i))
        end do

        do i=1,iCellListOrig
            read(FnumCLN,'(a)') line
            write(FnumCLNOut,'(a)') line
        end do

        i1=index(line,'              IFNOD')
        i2=index(line,'Well =')+6

        do i=1,nEIScreens
            do j=1,nCellList(i)
                !write(FnumCLNOut,*) iCLNOrig+i, CellNumber(i,j), 0, 0.0, CellScreenLength(i,j), 1.0, 0, line(i1:i2), trim(NameEIScreen(i))
                write(FnumCLNOut,'(i8,i8,i3,f11.6,g19.7,f11.6,i9,a,a)') iCLNOrig+i, CellNumber(i,j), 0, 0.0, CellScreenLength(i,j), 1.0, 0, line(i1:i2), trim(NameEIScreen(i))
            end do
        end do

        do i=1,iCLNOrig+NCONDUITYP+2   ! NCONDUITYP+2 lines then all starting heads
            read(FnumCLN,'(a)') line
            write(FnumCLNOut,'(a)') line
        end do

        do i=1,nEIScreens  ! repeat last starting head for each new screen
            write(FnumCLNOut,'(a)') line
        end do
        
        ! in transport case may be more data so read/write to end of file
        do 
            read(FnumCLN,'(a)',iostat=status) line
            if(status/=0) then
                exit
            endif
            write(FnumCLNOut,'(a)') line
        end do
        
         ! Flowsource control file
        read(FnumTG,'(a)') FnameFSCtl
        call OpenAscii(FNumFSCtl,FnameFSCtl)
        call Msg( 'FlowSource control file: '//trim(FnameFSCtl))
        
        ! Open new FlowSource control output file
        FnameFSCtlOut='out_'//FnameFSCtl  
        call OpenAscii(FnumFSCtlOut,FnameFSCtlOut)
        call Msg( 'FlowSource control output file: '//trim(FnameFSCtlOut))
        
        do 
            read(FNumFSCtl,'(a)') line
            write(FNumFSCtlout,'(a)') line
            if(index(line,'# note that if forward tracking mode is selected') > 0) then  ! insert cell lists
                do i=1,nEIScreens,2
                    if(index(NameEIScreen(i),'EW')>0) then
                        write(FNumFSCtlout,'(a)') ' '
                        write(FNumFSCtlout,'(a)') '# '//trim(NameEIScreen(i))//', '//trim(NameEIScreen(i+1))
                        do j=1,nCellList(i)
                            write(FNumFSCtlout,'(a,i8)') 'FlowDestination = ',CellNumber(i,j)
                        end do
                        do j=1,nCellList(i+1)
                            if(j==1) then
                                if(CellNumber(i+1,j) /= CellNumber(i,nCellList(i)) ) write(FNumFSCtlout,'(a,i8)') 'FlowDestination = ',CellNumber(i+1,j)
                            else
                                write(FNumFSCtlout,'(a,i8)') 'FlowDestination = ',CellNumber(i+1,j)
                            endif    
                        end do
                    end if
                end do
                exit
            end if
        end do

        end_ctl: do 
            read(FNumFSCtl,'(a)') line
            if(index(line,'#--------------------------') > 0) then  ! add end of ctl file
                write(FNumFSCtlout,'(a)') ' '
                write(FNumFSCtlout,'(a)') line
               
                do 
                    read(FNumFSCtl,'(a)',iostat=status) line
                    if(status /= 0) then
                        exit end_ctl
                    endif 
                    
                    write(FNumFSCtlout,'(a)') line
                   
                end do
            end if
        end do end_ctl
        
    end subroutine old_MUSG_PEST_EIWellCLNFileUpdate

    subroutine MUSG_PEST_CountParticlesToWells(FnumTG) !--- Given modpath output file and cln file info, count particles to each well and bin to arrival time.
        USE IFPORT 
        implicit none 

        integer :: i, j, k, l

        integer :: FnumTG
        
        integer :: FnumParticlesToWells
        character(MAXLBL) :: FNameParticlesToWells

        integer :: FnumCLN
        character(MAXLBL) :: FNameCLN
        
        integer :: FnumEndpoint7
        character(MAXLBL) :: FNameEndpoint7
        
        integer :: FnumEndpoint7_out
        character(MAXLBL) :: FNameEndpoint7_out
        
        character(20), allocatable :: cln_WellName(:)
        integer, allocatable :: cln_nCells(:)
        integer, allocatable :: cln_CellList(:,:) ! up to 100 cells per cln
        
        integer, allocatable :: cln_nParticles(:,:)  ! number of particles reporting to this CLN(well),Bin(time)
        

        character(MAXLBL) :: line
        integer :: nCln
        integer :: nTotCellList
        integer :: ifnod
        integer :: igwnod
        integer :: i1, i2, i3, i4, i5, i6
        real(dr) :: f1, f3, f4, f5, f6, f7, f8
        
        
        integer :: TrackingDirection
        integer :: TotalParticleCount
        integer :: iExit
        integer :: ICellStart
        integer :: ICellEnd
        real(dr) :: FinalTime
        
        integer :: nBins
        real(dr), allocatable :: BinStart(:)
        real(dr), allocatable :: BinEnd(:)
        

       ! Paticles to wells information file
        read(FNumTG,'(a)') FNameParticlesToWells
        call OpenAscii(FnumParticlesToWells,FNameParticlesToWells)
        call Msg( 'ParticlesToWells file: '//trim(FNameParticlesToWells))
        
        
        read(FnumParticlesToWells,'(a)') FNameCLN
        call OpenAscii(FnumCLN,FNameCLN)
        call Msg( 'CLN file: '//trim(FNameCLN))
        

        read(FNumCLN,*) nCLN, i1, i2, i3, i4, i5, nTotCellList
        allocate(cln_WellName(nCLN), cln_nCells(nCLN), cln_CellList(NCln,100))
        cln_nCells(:)=0
        cln_CellList(:,:)=0
        
        
        read(FNumCLN,'(a)') line   ! throw away line 2
        ! read cln well names
        do i=1,nCLN  
            read(FNumCLN,'(a)') line 
            l1=index(line,'Well = ')
            if(l1>0) then
                read(line(l1+7:),'(a)') cln_WellName(i)
            endif
        end do
        
        do i=1,nTotCellList
            read(FNumCLN,*) ifnod, igwnod
            cln_nCells(ifnod)=cln_nCells(ifnod)+1
            cln_CellList(ifnod,cln_nCells(ifnod))=igwnod
        end do
        
        call FreeUnit(FNumCLN)
        
        read(FnumParticlesToWells,*) nBins
        write(TMPStr,'(i8)') nBins
        call Msg('Number of bins: '//TMPStr)
        if(nBins>30) call ErrMsg('Current number of bins must be less than 30: '//TMPStr)
        
        
        allocate(BinStart(nBins), BinEnd(nBins))
        do i=1,nBins
            read(FnumParticlesToWells,*) BinStart(i),BinEnd(i)
            write(TMPStr,'(4g15.7)') BinStart(i),BinEnd(i)
            call Msg('Bin start, Bin end time: '//TMPStr)

            if(BinStart(i)>=BinEnd(i)) then
                write(TMPStr,'(4f15.3)') BinStart(i),BinEnd(i)
                call ErrMsg('Bin start time >= Bin end time: '//TMPStr)
            endif

            if(i>1) then
                do j=1,i-1
                    if(BinStart(i) < BinEnd(j)) then
                        write(TMPStr,'(4f15.3)') BinStart(i), BinEnd(j)
                        call ErrMsg('Bin start time < Last bin end time: '//TMPStr)
                    endif
                end do
            end if
        end do

        allocate(cln_nParticles(nCLN,nBins))
        cln_nParticles(:,:)=0

        do 
            ! CLN file
            read(FnumParticlesToWells,'(a)',iostat=status) FNameEndpoint7
            if(status /= 0) then
                exit
            endif
            
            call OpenAscii(FnumEndpoint7,FNameEndpoint7)
            call Msg( 'Endpoint7 file: '//trim(FNameEndpoint7))
            
            read(FnumEndpoint7,'(a)') line   ! check first line
            if(index(line,'MODPATH_ENDPOINT_FILE') == 0) then
                call Msg( 'Not a valid Modpath7 endpoint file')
                stop
            endif

            read(FnumEndpoint7,*) TrackingDirection, TotalParticleCount
            read(FnumEndpoint7,'(a)') line   
            read(FnumEndpoint7,'(a)') line   
            read(FnumEndpoint7,'(a)') line   
            read(FnumEndpoint7,'(a)') line   
            Particle: do i=1, TotalParticleCount
                read(FnumEndpoint7,*) i1, i2, i3, iExit, f1, FinalTime, iCellStart, i4, f3, f4, f5, f6, f7, f8, i5, i6, iCellEnd
                do k=1,nCLN
                    do j=1,cln_nCells(k)
                        if(iCellEnd==cln_CellList(k,j) .and. iExit==5) then
                            do l=1,nBins
                                if(FinalTime >= BinStart(l) .and. FinalTime < BinEnd(l)) then  
                                    cln_nParticles(k,l)=cln_nParticles(k,l)+1
                                    cycle Particle
                                endif
                            end do
                            write(TMPStr,'(4f15.3)') FinalTime
                            call ErrMsg('FinalTime fall outside of bin range: '//TMPStr)
                        endif
                    end do
                end do
            end do Particle
            
            ! Write particle count file
            FNameEndpoint7_out=trim(FNameEndpoint7)//'_PToWell.csv'
            call OpenAscii(FnumEndpoint7_out,FNameEndpoint7_out)
            call Msg( 'Endpoint7 particle count file: '//trim(FNameEndpoint7_out))
            write(FnumEndpoint7_out,'(a,30(i8,a))') 'WellName,#ParticlesReportingToBin',(i,',',i=1,nBins)
            do i=1,nCLN
                write(FnumEndpoint7_out,'(a20,a,30(i8,a))') cln_WellName(i), ',',(cln_nParticles(i,j),',',j=1,nBins)
            end do

            call FreeUnit(FnumEndpoint7)
            call FreeUnit(FnumEndpoint7_out)
             
        end do
        
    end subroutine MUSG_PEST_CountParticlesToWells

    subroutine M2005_PEST_CountParticlesToWells(FnumTG) !--- Given modpath5 output file and ??? file info, count particles to each well and bin to arrival time.
        USE IFPORT 
        implicit none 

        integer :: i, j, k

        integer :: FnumTG
        
        integer :: FnumParticlesToWells
        character(MAXLBL) :: FNameParticlesToWells

        integer :: FnumMNW
        character(MAXLBL) :: FNameMNW
        
        integer :: FNumEndpoint5
        character(MAXLBL) :: FNameEndpoint5
        
        integer :: FNumEndpoint5_out
        character(MAXLBL) :: FNameEndpoint5_out
        
        integer :: nLines
        
        character(20), allocatable :: WellName(:)
        integer, allocatable :: nIJKs(:)
        integer, allocatable :: IJKList(:,:,:) ! up to 100 IJKs per mnw
        integer :: iRow, jCol, kLyr
        
        integer, allocatable :: nParticles(:)  ! number of particles reporting to this MNW(well),Bin(time)
        

        character(MAXLBL) :: line
        integer :: nWells
        
        integer :: i1
        
       ! Paticles to wells information file
        read(FNumTG,'(a)') FNameParticlesToWells
        call OpenAscii(FnumParticlesToWells,FNameParticlesToWells)
        call Msg( 'ParticlesToWells file: '//trim(FNameParticlesToWells))
        
        read(FnumParticlesToWells,'(a)') FNameMNW
        call OpenAscii(FnumMNW,FNameMNW)
        call Msg( 'MNW file: '//trim(FNameMNW))
        
        read(FNumMNW,'(a)') line   ! throw away line 1
        read(FNumMNW,'(a)') line   ! throw away line 2
        read(FNumMNW,*) nLines
        ! count wells
        nWells=0
        do i=1,nLines
            read(FNumMNW,'(a)') line
            if(index(line,'SITE:') > 1) nWells=nWells+1
        end do

        allocate(WellName(nWells), nIJKs(nWells), IJKList(nWells,3,100))
        nIJKs(:)=0
        IJKList(:,:,:)=0

        continue
        
        rewind(FNumMNW) 
        read(FNumMNW,'(a)') line   ! throw away line 1
        read(FNumMNW,'(a)') line   ! throw away line 2
        read(FNumMNW,'(a)') line   ! throw away line 2
        ! read well name and ijk's
        nWells=0
        do i=1,nLines
            read(FNumMNW,'(a)') line
            if(index(line,'SITE:') > 1) then
                nWells=nWells+1
                nIJKs(nWells)=nIJKs(nWells)+1
                read(line,*) kLyr, iRow, jCol
                IJKList(nWells,1,nIJKs(nWells))=iRow
                IJKList(nWells,2,nIJKs(nWells))=jCol
                IJKList(nWells,3,nIJKs(nWells))=kLyr
                l1=index(line,'SITE:')
                WellName(nWells)=line(l1+5:)
            else
                nIJKs(nWells)=nIJKs(nWells)+1
                read(line,*) kLyr, iRow, jCol
                IJKList(nWells,1,nIJKs(nWells))=iRow
                IJKList(nWells,2,nIJKs(nWells))=jCol
                IJKList(nWells,3,nIJKs(nWells))=kLyr
            end if
        end do
        call FreeUnit(FNumMNW)

        allocate(nParticles(nWells))

        do 
            ! Particles file
            read(FnumParticlesToWells,'(a)',iostat=status) FNameEndpoint5
            if(status /= 0) then
                exit
            endif
            
            call OpenAscii(FNumEndpoint5,FNameEndpoint5)
            call Msg( 'Endpoint5 file: '//trim(FNameEndpoint5))
            
            read(FNumEndpoint5,'(a)') line   ! check first line
            if(index(line,'@ [ MODPATH 5.0') == 0) then
                call Msg( 'Not a Modpath 5 endpoint file')
                stop
            endif
            
            nParticles(:)=0
            Particle: do 
                read(FNumEndpoint5,*,iostat=status) i1, jCol, iRow, kLyr
                if(status/=0) exit Particle
                
                do k=1,nWells
                    do j=1,nIJKs(k)
                        if(IJKList(k,1,j)==iRow .and. IJKList(k,2,j)==jCol .and. IJKList(k,3,j)==kLyr) then
                            nParticles(k)=nParticles(k)+1
                            cycle Particle
                        endif
                    end do
                end do
            end do Particle
            
            ! Write particle count file
            FNameEndpoint5_out=trim(FNameEndpoint5)//'_PToWell.csv'
            call OpenAscii(FNumEndpoint5_out,FNameEndpoint5_out)
            call Msg( 'Endpoint5 particle count file: '//trim(FNameEndpoint5_out))
            write(FNumEndpoint5_out,'(a,30(i8,a))') 'WellName,#ParticlesReporting'
            do i=1,nWells
                write(FNumEndpoint5_out,'(a20,a,30(i8,a))') WellName(i), ',',nParticles(i)
            end do

            call FreeUnit(FNumEndpoint5)
            call FreeUnit(FNumEndpoint5_out)
             
        end do
        
    end subroutine M2005_PEST_CountParticlesToWells

    subroutine MUSG_PEST_RTWellOptimization(FnumTG) !--- Given RT pumping well info, update well .WEL file
        !USE IFPORT 
        implicit none 

        !type (MUSG_Project) musg_l


        integer :: i, j

        integer :: FnumTG
        
        integer :: FnumRTRates
        character(MAXLBL) :: FnameRTRates
        
        integer :: FnumRTOnOff
        character(MAXLBL) :: FNameRTOnOff
        integer :: FnumRTOnOffNints
        character(MAXLBL) :: FNameRTOnOffNints

        integer :: FnumRTRateOut
        character(MAXLBL) :: FnameRTRateOut

        integer :: FNumWel
        character(MAXLBL) :: FnameWel
        integer :: FNumWelOut
        character(MAXLBL) :: FnameWelOut
        
     !   integer, allocatable :: nCellList(:)
	    !character(31), allocatable :: NameEIScreen(:)
     !   integer, allocatable :: CellNumber(:,:)
     !   real(dr), allocatable :: CellScreenLength(:,:)
     !   
        
        integer :: i1, i2
        character(MAXSTRING) :: line

        character(31) :: WellName(MAXCLN)
        integer :: RTWell_CLN_Number
        real(dr) :: HighRate_ft3_day(MAXCLN)
        real(dr) :: LowRate_ft3_day(MAXCLN)
        
        integer :: nRTOnOff
        integer, allocatable :: cln_number_On_Off(:)
        integer, allocatable :: SP_num_On_Off(:)
        real(dr), allocatable :: On_off(:)
        real(dr),allocatable :: AppliedRate_ft3_day(:)
        real(dr) :: State
        integer, allocatable :: indx_col(:)
        
        integer :: iStressperiod
        integer :: nStressperiods
        integer :: IWellOrig
        integer :: IWellNew(MAXSTRESS)
        integer :: idum1, idum2
        real(dr) :: RTRateTotal(MAXSTRESS)
        
        integer :: iOnOff
        integer :: iCLN
        
        character(MAXSTRING) :: CMDString

        
        integer :: FnumLADWP
        character(MAXLBL) :: FnameLADWP
        real(dr) :: LADWPRateTotal(MAXSTRESS)
        real(dr) :: LADWPRatio
        real(dr) :: LADWPPenalty

        
        integer :: FnumPenalty
        character(MAXLBL) :: FNamePenalty
        integer :: nRange
        real(dr), allocatable :: MinPenalty(:)
        real(dr), allocatable :: MaxPenalty(:)
        real(dr), allocatable :: MinRange(:)
        real(dr), allocatable :: MaxRange(:)

        !-----------------------------------------------------------------------------------
        ! RT wells rate file
        read(FnumTG,'(a)') FnameRTRates  
        call OpenAscii(FnumRTRates,FnameRTRates)
        call Msg( 'RT well rate input file: '//trim(FnameRTRates))
        
        ! Read header
        read(FnumRTRates,'(a)') line
        do 
            read(FnumRTRates,'(a)',iostat=status) line
            if(status/=0) exit
            
            l1=index(line,',')
            read(line(:l1-1),'(a)') TMPStr
            
            line=line(l1+1:)
            l1=index(line,',')
            read(line(:l1),*) RTWell_CLN_Number
            
            WellName(RTWell_CLN_Number)=TMPStr
            
            line=line(l1+1:)
            read(line,*) HighRate_ft3_day(RTWell_CLN_Number)

            l1=index(line,',')
            line=line(l1+1:)
            read(line,*) LowRate_ft3_day(RTWell_CLN_Number)

        end do

        continue

        !-----------------------------------------------------------------------------------
        ! RT wells on/off file
        read(FnumTG,'(a)') FNameRTOnOff  
        call OpenAscii(FNumRTOnOff,FNameRTOnOff)
        call Msg( 'RT well on/off input file: '//trim(FNameRTOnOff))
        
        FNameRTOnOffNints='AppliedRates.txt'
        call OpenAscii(FNumRTOnOffNints,FNameRTOnOffNints)
        call Msg( 'RT well on/off input file: '//trim(FNameRTOnOffNints))

        ! Read header
        read(FNumRTOnOff,'(a)') line
        write(FNumRTOnOffNints,'(a)') '   cln,Stress period,    On_off, State,  Applied rate (ft3/day)'

        nRTOnOff=0
        do 
            read(FNumRTOnOff,'(a)',iostat=status) line
            if(status/=0) exit
            
            nRTOnOff=nRTOnOff+1
            
        end do
        
        allocate(cln_number_On_Off(nRTOnOff), SP_num_On_Off(nRTOnOff), On_off(nRTOnOff), indx_col(nRTOnOff), AppliedRate_ft3_day(nRTOnOff))

        rewind(FNumRTOnOff)
        ! Read header
        read(FNumRTOnOff,'(a)') line
        do i=1, nRTOnOff
            read(FNumRTOnOff,*) cln_number_On_Off(i), SP_num_On_Off(i), On_off(i)
            
            ! Compute applied rate from inputs
            if(LowRate_ft3_day(cln_number_On_Off(i))==-9999.) then  ! on/off state from nint
                if(nint(On_off(i))==0) then 
                    AppliedRate_ft3_day(i)=0.0d0
                    State=0.0
                else if(nint(On_off(i))==1) then 
                    AppliedRate_ft3_day(i)=HighRate_ft3_day(cln_number_On_Off(i))
                    State=1.0
                else  
                    call ErrMsg('Nint not 0 or 1')
                endif
            else if(LowRate_ft3_day(cln_number_On_Off(i))/=-9999.) then  ! off/low/high state from fraction
                if(On_off(i) >= 0.0 .and. On_off(i) < 0.333) then
                    AppliedRate_ft3_day(i)=0.0d0
                    State=0.0
                else if(On_off(i) >= 0.333 .and. On_off(i) < 0.666) then    
                    AppliedRate_ft3_day(i)=LowRate_ft3_day(cln_number_On_Off(i))
                    State=0.5
                else if(On_off(i) >= 0.666 .and. On_off(i) <= 1.0) then 
                    AppliedRate_ft3_day(i)=HighRate_ft3_day(cln_number_On_Off(i))
                    State=1.0
                else
                    call ErrMsg('Nint not in range between 0.0 and 1.0')

                endif
            else
                call ErrMsg('Bad low rate')
            endif
            write(FNumRTOnOffNints,'(i6,i6,f20.5,f5.1,f20.5)') cln_number_On_Off(i), SP_num_On_Off(i), On_off(i),State,AppliedRate_ft3_day(i)
        end do
     
        
        !-----------------------------------------------------------------------------------
        ! Pass1: Read/write well file and update RT well rate, inserting well if not present
        !    - count new total wells for stress period
        
        ! Modflow Well .wel file
        read(FnumTG,'(a)') FnameWel
        call OpenAscii(FNumWel,FnameWel)
        call Msg( 'Modflow well file: '//trim(FnameWel))
        
        ! Open new well .wel output file
        FnameWelOut='pass1.tmp'
        call OpenAscii(FnumWelOut,FnameWelOut)
        call Msg( 'Modflow well output TEMP file: '//trim(FnameWelOut))

        read(FNumWel,'(a)') line
        write(FnumWelOut,'(a)') trim(line)  ! save first line

        iOnOff=1
        RTRateTotal(:)= 0.0
        nStressPeriods=0
        read_wel_file: do 
            read(FNumWel,'(a)',iostat=status) line
            if(status /= 0) then  ! end of file
                exit read_wel_file
            endif
            
            l1=index(line,'stress period ')
            if(l1 > 0) then ! new stress period
                write(FnumWelOut,'(a)') trim(line)  ! save stress period header
                
                ! Extract stress period from line
                l1=l1+14  ! position at end of string 'stress period '
                TMPStr=line(31:)
                l2=l1+index(line(l1:),':')-2
                read(line(l1:l2),*) iStressPeriod
                
                nStressPeriods=max(nStressPeriods,iStressPeriod)

                
                read(line,*) idum1, idum2, iWellOrig
                iWellNew(iStressPeriod)=iWellOrig
                do 
                    read(FNumWel,'(a)',iostat=status) line
                    if(status/=0) exit read_wel_file
                    l1=index(line,'stress period ')
                    if(l1 > 0) then ! next stress period
                        backspace(FNumWel)
                        cycle read_wel_file
                    endif

                    
                    read(line,*) iCLN
                    
                    if( iOnOff<=nRTOnOff) then
                        if(SP_num_On_Off(iOnOff)==iStressPeriod) then
                            if(iCLN<cln_number_On_Off(iOnOff)) then ! NON RT CLN
                                write(FnumWelOut,'(a)') trim(line)  
                            else if(iCLN==cln_number_On_Off(iOnOff)) then  ! RT CLN already in list 
                                !write(FnumWelOut,'(a,i5,f20.1,5x,a)') 'overwrite ', iCLN,PlaceholderRate_ft3_day(cln_number_On_Off(iOnOff)),trim(WellName(cln_number_On_Off(iOnOff)))
                                i1=27
                                i2=index(line,'well =')+6
                                write(FnumWelOut,'(i11,g15.7,a)') cln_number_On_Off(iOnOff), AppliedRate_ft3_day(iOnOff), line(i1:i2)//trim(WellName(cln_number_On_Off(iOnOff)))
                                RTRateTotal(iStressPeriod)= RTRateTotal(iStressPeriod)+AppliedRate_ft3_day(iOnOff)
                                iOnOff=iOnOff+1
                            else if(iCLN>cln_number_On_Off(iOnOff)) then  ! RT CLN not in list
                                !write(FnumWelOut,'(a,i5,f20.1,5x,a)') 'add ', iCLN,PlaceholderRate_ft3_day(cln_number_On_Off(iOnOff)),WellName(cln_number_On_Off(iOnOff))
                                i1=27
                                i2=index(line,'well =')+6
                                write(FnumWelOut,'(i11,g15.7,a)') cln_number_On_Off(iOnOff), AppliedRate_ft3_day(iOnOff), line(i1:i2)//trim(WellName(cln_number_On_Off(iOnOff)))
                                RTRateTotal(iStressPeriod)= RTRateTotal(iStressPeriod)+AppliedRate_ft3_day(iOnOff)
                                iOnOff=iOnOff+1
                                iWellNew(iStressPeriod)=iWellNew(iStressPeriod)+1
                                backspace(FnumWel)
                                !write(FnumWelOut,'(a,i5)') 'inc nwells ', iWellNew(iStressPeriod)
                            endif
                            !iOnOff=min(iOnOff,nRTOnOff)
                        else
                            write(FnumWelOut,'(a)') trim(line)  
                        end if
                    else
                        write(FnumWelOut,'(a)') trim(line)  
                    end if
                    
                        
                end do
                continue
                
            endif
        end do read_wel_file
        
        call freeunit(FNumWel)
        call freeunit(FNumWelOut)
        
        ! Pass2: update number of wells per stress period
        call OpenAscii(FNumWel,'pass1.tmp')
        
        ! Open new well .wel output file
        FnameWelOut='rt_'//FnameWel  
        call OpenAscii(FnumWelOut,FnameWelOut)
        call Msg( 'Modflow well output file: '//trim(FnameWelOut))
        
        do 
            read(FnumWel,'(a)',iostat=status) line
            if(status /= 0) then  ! end of file
                exit 
            endif
       
            l1=index(line,'stress period ')
            if(l1 > 0) then ! new stress period, adjust number of wells
                
                ! Extract stress period from line
                l1=l1+14  ! position at end of string 'stress period '
                TMPStr=line(31:)
                l2=l1+index(line(l1:),':')-2
                read(line(l1:l2),*) iStressPeriod

                read(line,*) idum1, idum2, iWellOrig
                write(line(:30),'(3i10)') idum1, idum2,iWellNew(iStressPeriod)
                
            endif
           
            write(FnumWelOut,'(a)') trim(line) 

        end do
        
        call freeunit(FNumWel)
        call freeunit(FNumWelOut)
        CMDString='del pass1.tmp'
        i=system(CMDString)
        
        
        ! LADPW rates input file
        read(FnumTG,'(a)') FnameLADWP  
        call OpenAscii(FnumLADWP,FnameLADWP)
        call Msg( 'LADPW rate input file: '//trim(FnameLADWP))
        
        read(FnumLADWP,'(a)') line
        do i=1,nStressPeriods
            read(FnumLADWP,*) j, LADWPRateTotal(i)
        end do
            
        ! Penalties input file
        read(FnumTG,'(a)') FNamePenalty  
        call OpenAscii(FNumPenalty,FNamePenalty)
        call Msg( 'LADPW rate penalties file: '//trim(FNamePenalty))
        
        read(FNumPenalty,*) nRange
        allocate(MinRange(nRange), MaxRange(nRange), MinPenalty(nRange), MaxPenalty(nRange))
        do i=1,nRange
            read(FNumPenalty,*) MinRange(i), MaxRange(i), MinPenalty(i), MaxPenalty(i)
            write(TMPStr,'(4f20.5)') MinRange(i), MaxRange(i), MinPenalty(i), MaxPenalty(i)
            call Msg( 'Rate1 Rate2 Penalty1 Penalty2: '//trim(TMPStr))
            if(i>1) then
                if(Minrange(i)/=MaxRange(i-1)) then
                    write(ErrStr,'(a,2f20.5)') 'Range 1: ', MinRange(i-1), MaxRange(i-1)
                    call Msg(ErrStr)
                    write(ErrStr,'(a,2f20.5)') 'Range 2: ', MinRange(i), MaxRange(i)
                    call Msg(ErrStr)
                    call ErrMsg('Min range 2 not equal to max range 1')
                end if
                if(MinPenalty(i)/=MaxPenalty(i-1)) then
                    write(ErrStr,'(a,2f20.5)') 'Penalty 1: ', MinPenalty(i-1), MaxPenalty(i-1)
                    call Msg(ErrStr)
                    write(ErrStr,'(a,2f20.5)') 'Penalty 2: ', MinPenalty(i), MaxPenalty(i)
                    call Msg(ErrStr)
                    call ErrMsg('Min penalty 2 not equal to max penalty 1')
                end if
            end if
        end do

        
        ! Open RT well rate output file
        FnameRTRateOut='rt_rates_per_SP.lst'  
        call OpenAscii(FnumRTRateOut,FnameRTRateOut)
        call Msg( 'RT well rate output file: '//trim(FnameRTRateOut))
        write(FnumRTRateOut,'(a)') 'Stress period,RT Well Rate Total, LADPW Design Rate,       RT:LADWP Ratio,     PEST Penalty'
        do i=1,nStressPeriods
            LADWPRatio=RTRateTotal(i)/LADWPRateTotal(i)
            do j=1,nrange
                if(LADWPRatio>= MinRange(j) .and. LADWPRatio<= MaxRange(j)) then ! linear interpolation for penalty
                    LADWPPenalty= MinPenalty(j)+(LADWPRatio-MinRange(j))/(MaxRange(j)-MinRange(j))*(MaxPenalty(j)-MinPenalty(j))
                endif
            end do
            write(FnumRTRateOut,'(i10,4f20.5)') i,RTRateTotal(i),LADWPRateTotal(i),LADWPRatio,LADWPPenalty
        end do
       
        continue


    end subroutine MUSG_PEST_RTWellOptimization
    


    end module MUSG

