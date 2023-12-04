module MUSG !
    use GeneralRoutines
    !use ProcessCSV
    use fem
    use tecplot
    use global
    use CLN1MODULE
    use SWF1MODULE
    implicit none
    
    ! Pre/Post-processor for Modflow project files Fall 2023
    ! Added instructions 
    character(60) :: MUSG_ProcessFiles_CMD='process modflow files'

    ! legacy instructions for Jeff Randall
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

    ! legacy Modflow 2005 and Modpath 5 version for Nicole (requested by Argha Namhata, Perth)   
    character(60) :: M2005_PEST_CountParticlesToWells_CMD='count particles to wells modflow2005 modpath5'

    character(60) :: MUSG_End_CMD=	'end'
    character(256) :: MUSG_CMD
    
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
        
        real, allocatable :: Head(:,:)
	    real, allocatable :: Sat(:,:)
        
        ! GWF.cbb components
        real, allocatable :: Cbb_STORAGE(:,:)
	    real, allocatable :: Cbb_CONSTANT_HEAD(:,:)
	    real, allocatable :: Cbb_RECHARGE(:,:)
	    real, allocatable :: Cbb_DRAINS(:,:)
	    real, allocatable :: Cbb_SWF(:,:)
	    real, allocatable :: Cbb_ja(:,:)
	    real, allocatable :: FLX_BAL_ERR(:,:)
        
        real, allocatable :: laycbd(:)
        real, allocatable :: bot(:)
        real, allocatable :: top(:)

        
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
        
        ! CLN_GSF file required for grid dimensions but not listed in NAM file
        character(128) :: FNameCLN_GSF
        integer :: iCLN_GSF
        
        ! SWF_GSF file required for grid dimensions but not listed in NAM file
        character(128) :: FNameSWF_GSF
        integer :: iSWF_GSF
        
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
        ! BAS6 options 
        logical :: xsection=.false.
        logical :: chtoch=.false.
        logical :: free=.false.
        logical :: printtime=.false.
        logical :: unstructured=.false.
        logical :: printfv=.false.
        logical :: converge=.false.
        logical :: richards=.false.
        logical :: dpin=.false.
        logical :: dpout=.false.
        logical :: dpio=.false.
        logical :: ihm=.false.
        logical :: syall=.false.
        integer :: ixsec = 0    
        integer :: ichflg = 0   
        integer :: ifrefm = 0   
        integer :: iprtim = 0   
        integer :: iunstr = 0   
        integer :: iprconn = 0  
        integer :: ifrcnvg = 0 
        integer :: iunsat=1
        integer :: idpin = 0    
        integer :: idpout = 0   
        integer :: ihmsim = 0   
        integer :: iuihm = 0    
        integer :: isyall = 0   

        ! SMS file
        character(128) :: FNameSMS
        integer :: iSMS

        ! OC file
        character(128) :: FNameOC
        integer :: iOC
        integer :: ntime = 0
        real, allocatable :: timot(:)
        
        ! LPF file
        character(128) :: FNameLPF
        integer :: iLPF

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
 
        ! CLN file
        character(128) :: FNameCLN
        integer :: iCLN

        ! SWF file
        character(128) :: FNameSWF
        integer :: iSWF

        ! GNC file
        character(128) :: FNameGNC
        integer :: iGNC

        ! LAK file
        character(128) :: FNameLAK
        integer :: iLAK=0


        
        ! DATA(BINARY) files
        ! HDS file
        character(128) :: FNameHDS
        integer :: iHDS
        character(128) :: FNameHDS_CLN
        integer :: iHDS_CLN
        character(128) :: FNameHDS_SWF
        integer :: iHDS_SWF
        
        ! DDN file
        character(128) :: FNameDDN
        integer :: iDDN
        character(128) :: FNameDDN_CLN
        integer :: iDDN_CLN
        character(128) :: FNameDDN_SWF
        integer :: iDDN_SWF
        
        ! CBB file
        character(128) :: FNameCBB
        integer :: iCBB
        character(128) :: FNameCBB_CLN
        integer :: iCBB_CLN
        character(128) :: FNameCBB_SWF
        integer :: iCBB_SWF

        ! CBCCLN file
        character(128) :: FNameCBCCLN
        integer :: iCBCCLN
  
        ! Scan file
        integer :: nDim=10000
        integer :: nKeyWord
        character(MAXSTRING), ALLOCATABLE :: KeyWord(:) ! read buffer for location data
        character(128) :: FNameSCAN
        integer :: iSCAN
        
        ! Modflow file extensions MUT currently does not recognize or process
        integer ::iBCF6
        integer ::iEVS 
        integer ::iGHB 
        integer ::iRTS 
        integer ::iTIB 
        integer ::iDPF 
        integer ::iPCB 
        integer ::iBCT 
        integer ::iFHB 
        integer ::iRES 
        integer ::iSTR 
        integer ::iIBS 
        integer ::iHFB6
        integer ::iDIS 
        integer ::iPVAL
        integer ::iSGB 
        integer ::iHOB 
        integer ::iDPT 
        integer ::iZONE
        integer ::iMULT
        integer ::iDROB
        integer ::iRVOB
        integer ::iGBOB
        integer ::iDDF 
        integer ::iCHOB
        integer ::iETS 
        integer ::iDRT 
        integer ::iQRT 
        integer ::iGMG 
        integer ::ihyd 
        integer ::iSFR 
        integer ::iMDT 
        integer ::iGAGE
        integer ::iLVDA
        integer ::iSYF 
        integer ::ilmt6
        integer ::iMNW1
        integer ::iKDEP
        integer ::iSUB 
        integer ::iUZF 
        integer ::igwm 
        integer ::iSWT 
        integer ::iPATH
        integer ::iPTH 
        integer ::iTVM 


        !character(128) :: Name
        !integer :: LengthName
        !logical :: Exists=.false.
	    !integer :: Unit

        !logical :: blockel
        !
        !
        !logical,allocatable :: nchosen(:)
        !
        !integer :: iz
        !integer :: ic
        !
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
        
      !! URWORD        
      !integer :: linlen
      !integer :: ncode, icol, iout, in
      !integer :: istart
      !real :: r
      !integer :: istop,n

        

    end type MUSG_Project

    ! other local variables
    integer, Parameter :: MAXCLN=10000  ! assuming never more than 10000 CLN's
    integer, Parameter :: MAXSTRESS=10000  ! assuming never more than 10000 Stress Periods

    
    contains

   subroutine ProcessModflowUSG(FnumTG, Modflow,prefix) !--- Process MUSG instructions for this data structure  Modflow
        implicit none

        integer :: FnumTG
        character(*) :: prefix
        type (MUSG_Project) Modflow
        Modflow.MUTPrefix=prefix

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
                call MUSG_ProcessFiles(FnumTG, Modflow)
            !

            
            !else if(index(MUSG_CMD, MUSG_ReadAsciiHeadFile_CMD)  /= 0) then
            !    call MUSG_ReadAsciiHeadFile(FnumTG, Modflow)
            !
            !
            !else if(index(MUSG_CMD, MUSG_ReadAsciiKxFile_CMD)  /= 0) then
            !    call MUSG_ReadAsciiKxFile(FnumTG, Modflow)
            !
            !else if(index(MUSG_CMD, MUSG_ReadAsciiSsFile_CMD)  /= 0) then
            !    call MUSG_ReadAsciiSsFile(FnumTG, Modflow)
            !
            !else if(index(MUSG_CMD, MUSG_ReadAsciiSyFile_CMD)  /= 0) then
            !    call MUSG_ReadAsciiSyFile(FnumTG, Modflow)
            !
            !else if(index(MUSG_CMD, MUSG_ReadAsciiVanisFile_CMD)  /= 0) then
            !    call MUSG_ReadAsciiVanisFile(FnumTG, Modflow)
            !
            !
            !else if(index(MUSG_CMD, MUSG_ReadRiverFlowsAsciiFile_CMD)  /= 0) then
            !    call MUSG_ReadRiverFlowsAsciiFile(FnumTG, Modflow)
            !
            !else if(index(MUSG_CMD, MUSG_RiverFlowsToTecplot_CMD)  /= 0) then
            !    call MUSG_RiverFlowsToTecplot(FnumTG, Modflow)
            !
            !else if(index(MUSG_CMD, MUSG_ReadHeadCalibrationAsciiFile_CMD)  /= 0) then
            !    call MUSG_ReadHeadCalibrationAsciiFile(FnumTG, Modflow)
            !
            !else if(index(MUSG_CMD, MUSG_HeadCalibrationToTecplot_CMD)  /= 0) then
            !    call MUSG_HeadCalibrationToTecplot(FnumTG, Modflow)
            !
            !else if(index(MUSG_CMD, MUSG_RiverConductanceUpdate_CMD)  /= 0) then
            !    call MUSG_RiverConductanceUpdate(FnumTG)
            !
            !else if(index(MUSG_CMD, MUSG_PEST_WellRatePenalties_CMD)  /= 0) then
            !    call MUSG_PEST_WellRatePenalties(FnumTG)
            !
            !else if(index(MUSG_CMD, MUSG_PEST_UpdateWellRatePenalties_CMD)  /= 0) then
            !    call MUSG_PEST_UpdateWellRatePenalties(FnumTG)
            !
            !else if(index(MUSG_CMD, MUSG_PEST_FlowSourceCapture_CMD)  /= 0) then
            !    call MUSG_PEST_FlowSourceCapture(FnumTG)

            !else if(index(MUSG_CMD, MUSG_PEST_CLNFileCalculations_CMD)  /= 0) then
            !    call MUSG_PEST_CLNFileCalculations(FnumTG, Modflow)

            !else if(index(MUSG_CMD, MUSG_PEST_EIWellCLNFileUpdate_CMD)  /= 0) then
            !    call MUSG_PEST_EIWellCLNFileUpdate(FnumTG, Modflow)
            !
            !else if(index(MUSG_CMD, MUSG_PEST_CountParticlesToWells_CMD)  /= 0) then
            !    call MUSG_PEST_CountParticlesToWells(FnumTG)
            !
            !else if(index(MUSG_CMD, M2005_PEST_CountParticlesToWells_CMD)  /= 0) then
            !    call M2005_PEST_CountParticlesToWells(FnumTG)
            !
            !    
            !else if(index(MUSG_CMD, MUSG_ReadWellConstructionCSVFile_CMD)  /= 0) then
            !    call MUSG_ReadWellConstructionCSVFile(FnumTG, Modflow)
            !
            !else if(index(MUSG_CMD, MUSG_Read_EIWellCSVFile_CMD)  /= 0) then
            !    call MUSG_Read_EIWellCSVFile(FnumTG, Modflow)
            !
            !else if(index(MUSG_CMD, MUSG_PEST_ExternalCodeExecute_CMD)  /= 0) then
            !    call MUSG_PEST_ExternalCodeExecute(FnumTG)
            !
            !else if(index(MUSG_CMD, MUSG_PEST_RTWellOptimization_CMD)  /= 0) then
            !    call MUSG_PEST_RTWellOptimization(FnumTG)

 
            else if(index(MUSG_CMD, MUSG_End_CMD)  /= 0) then
                exit

            else
                call ErrMsg('MUSG?:'//MUSG_CMD)
            end if
        end do

        10 continue

    end subroutine ProcessModflowUSG

    subroutine MUSG_ProcessFiles(FnumTG, Modflow)
        implicit none
        !-------ASSIGN VERSION NUMBER AND DATE
        CHARACTER*40 VERSION
        CHARACTER*14 MFVNAM
        PARAMETER (VERSION='USG-TRANSPORT VERSION 2.02.1')
        PARAMETER (MFVNAM='USG-TRANSPORT ') !USG = Un-Structured Grids
        
        integer :: FnumTG
        type (MUSG_Project) Modflow
       
        integer :: inunit
        CHARACTER*4 CUNIT(NIUNIT)
        DATA CUNIT/'BCF6', 'WEL ', 'DRN ', 'RIV ', 'EVT ', 'EVS ', 'GHB ',&  !  7  et time series is now EVS as ETS is for segmented ET&
                'RCH ', 'RTS ', 'TIB ', 'DPF ', 'OC  ', 'SMS ', 'PCB ',&  ! 14
                'BCT ', 'FHB ', 'RES ', 'STR ', 'IBS ', 'CHD ', 'HFB6',&  ! 21
                'LAK ', 'LPF ', 'DIS ', 'DISU', 'PVAL', 'SGB ', 'HOB ',&  ! 28
                'CLN ', 'DPT ', 'ZONE', 'MULT', 'DROB', 'RVOB', 'GBOB',&  ! 35
                'GNC ', 'DDF ', 'CHOB', 'ETS ', 'DRT ', 'QRT ', 'GMG ',&  ! 42
                'hyd ', 'SFR ', 'MDT ', 'GAGE', 'LVDA', 'SYF ', 'lmt6',&  ! 49
                'MNW1', '    ', '    ', 'KDEP', 'SUB ', 'UZF ', 'gwm ',&  ! 56
                'SWT ', 'PATH', 'PTH ', '    ', '    ', '    ', '    ',&  ! 63
                'TVM ', 'SWF ', 35*'    '/

        integer :: maxunit, nc 

        INCLUDE 'openspec.inc'

        
        ! read prefix for project
        read(FnumTG,'(a)') Modflow.Prefix
		call lcase(Modflow.Prefix)
        call Msg('Modflow project prefix: '//Modflow.Prefix)

        
        ! Scan file
        Modflow.FNameSCAN=Modflow.MUTPrefix(:len_trim(Modflow.MUTPrefix))//'o.scan'
        open(Modflow.iSCAN,file=Modflow.FNameSCAN,status='unknown',form='formatted')
        write(Modflow.iSCAN,'(a)') 'Scan file from project '//Modflow.prefix(:len_trim(Modflow.prefix))
        Modflow.nKeyWord=0
        allocate(Modflow.KeyWord(Modflow.nDim))
        Modflow.KeyWord(:)='UNDEFINED'


        ! Process NAM file
        Modflow.FNameNAM=Modflow.Prefix(:len_trim(Modflow.Prefix))//'.nam'
        call openMUSGFile('NAM',' '//Modflow.FNameNAM,Modflow.Prefix,Modflow.iNAM,Modflow.FNameNAM)
        INUNIT = 99
        MAXUNIT= INUNIT
        !
        !4------OPEN NAME FILE.
        OPEN (UNIT=INUNIT,FILE=Modflow.FNameNAM,STATUS='OLD',ACTION=ACTION(1))
        NC=INDEX(Modflow.FNameNAM,' ')
        WRITE(*,490)' Using NAME file: ',Modflow.FNameNAM(1:NC)
490     FORMAT(A,A)
        
        ALLOCATE(IUNIT(NIUNIT))

        call Msg(' ')
        call Msg('-------Open and scan files listed in NAM file:')
        !
        !C2------Open all files in name file.
        CALL SGWF2BAS8OPEN(INUNIT,IOUT,IUNIT,CUNIT,NIUNIT,&
            VERSION,INBAS,MAXUNIT,modflow)
        
        !do i=1,niunit
        !    write(iout,*) i, iunit(i),cunit(i)
        !end do
        !
        
        ! Unit numbering starts at BCF6=7 so add 6 to iunut index
        Modflow.iBAS6 =inbas       
        Modflow.iBCF6 =iunit(1)       
        Modflow.iWEL  =iunit(2)       
        Modflow.iDRN  =iunit(3)       
        Modflow.iRIV  =iunit(4)       
        Modflow.iEVT  =iunit(5)       
        Modflow.iEVS  =iunit(6)       
        Modflow.iGHB  =iunit(7)       
        Modflow.iRCH  =iunit(8)       
        Modflow.iRTS  =iunit(9)       
        Modflow.iTIB =iunit(10)       
        Modflow.iDPF =iunit(11)       
        Modflow.iOC  =iunit(12)       
        Modflow.iSMS =iunit(13)       
        Modflow.iPCB =iunit(14)       
        Modflow.iBCT =iunit(15)       
        Modflow.iFHB =iunit(16)       
        Modflow.iRES =iunit(17)       
        Modflow.iSTR =iunit(18)       
        Modflow.iIBS =iunit(19)       
        Modflow.iCHD =iunit(20)       
        Modflow.iHFB6=iunit(21)       
        Modflow.iLAK =iunit(22)       
        Modflow.iLPF =iunit(23)       
        Modflow.iDIS =iunit(24)       
        Modflow.iDISU=iunit(25)       
        Modflow.iPVAL=iunit(26)       
        Modflow.iSGB =iunit(27)       
        Modflow.iHOB =iunit(28)       
        Modflow.iCLN =iunit(29)       
        Modflow.iDPT =iunit(30)       
        Modflow.iZONE=iunit(31)       
        Modflow.iMULT=iunit(32)       
        Modflow.iDROB=iunit(33)       
        Modflow.iRVOB=iunit(34)       
        Modflow.iGBOB=iunit(35)       
        Modflow.iGNC =iunit(36)       
        Modflow.iDDF =iunit(37)       
        Modflow.iCHOB=iunit(38)       
        Modflow.iETS =iunit(39)       
        Modflow.iDRT =iunit(40)       
        Modflow.iQRT =iunit(41)       
        Modflow.iGMG =iunit(42)       
        Modflow.ihyd =iunit(43)       
        Modflow.iSFR =iunit(44)       
        Modflow.iMDT =iunit(45)       
        Modflow.iGAGE=iunit(46)       
        Modflow.iLVDA=iunit(47)       
        Modflow.iSYF =iunit(48)       
        Modflow.ilmt6=iunit(49)       
        Modflow.iMNW1=iunit(50)       
        Modflow.iKDEP=iunit(53)       
        Modflow.iSUB =iunit(54)       
        Modflow.iUZF =iunit(55)       
        Modflow.igwm =iunit(56)       
        Modflow.iSWT =iunit(57)       
        Modflow.iPATH=iunit(58)       
        Modflow.iPTH =iunit(59)       
        Modflow.iTVM =iunit(64)  
        Modflow.iSWF =iunit(65)       
        
        ! First read all GSF (grid specification) files for GWF domain, then CLN and SWF domains if present
        call Msg(' ')
        call Msg('-------Read all GSF (grid specification) files:')
        Modflow.FNameGSF=Modflow.Prefix(:len_trim(Modflow.Prefix))//'.gwf.gsf'
        inquire(file=Modflow.FNameGSF,exist=FileExists)
        if(.not. FileExists) then
            call Msg('No grid specification file: '//Modflow.FNameGSF)
        else
            call Msg('Modflow GWF GSF file: '//Modflow.FNameGSF)
	        call getunit(Modflow.iGSF)
            open(Modflow.iGSF,file=Modflow.FNameGSF,status='unknown',form='formatted')
        
            call MUSG_Read_GWF_GSF(Modflow)
        endif

        if(Modflow.iCLN /= 0) THEN
            Modflow.FNameCLN_GSF=Modflow.Prefix(:len_trim(Modflow.Prefix))//'.cln.gsf'
            inquire(file=Modflow.FNameCLN_GSF,exist=FileExists)
            if(.not. FileExists) then
                call Msg('No grid specification file: '//Modflow.FNameCLN_GSF)
            else
                call Msg('Modflow CLN GSF file: '//Modflow.FNameCLN_GSF)
	            call getunit(Modflow.iCLN_GSF)
                open(Modflow.iCLN_GSF,file=Modflow.FNameCLN_GSF,status='unknown',form='formatted')
        
                call MUSG_Read_CLN_GSF(Modflow)
            endif
        end if

        if(Modflow.iSWF /= 0) THEN
            Modflow.FNameSWF_GSF=Modflow.Prefix(:len_trim(Modflow.Prefix))//'.swf.gsf'
            inquire(file=Modflow.FNameSWF_GSF,exist=FileExists)
            if(.not. FileExists) then
                call Msg('No grid specification file: '//Modflow.FNameSWF_GSF)
            else
                call Msg('Modflow SWF GSF file: '//Modflow.FNameSWF_GSF)
	            call getunit(Modflow.iSWF_GSF)
                open(Modflow.iSWF_GSF,file=Modflow.FNameSWF_GSF,status='unknown',form='formatted')
        
                call MUSG_Read_SWF_GSF(Modflow)
            endif
        end if

        ! Read data in Modflow-USG order

        call Msg(' ')
        call Msg('-------Read options from BAS6:')
        call MUSG_ReadBAS6_Options(Modflow) ! based on subroutine SGWF2BAS8OPEN

        call Msg(' ')
        call Msg('-------Read first part of DISU:')
        call MUSG_ReadDISU_pt1(Modflow)  ! based on subroutine SDIS2GLO8AR
        NEQS = NODES

        IF(Modflow.iCLN/=0) THEN
            call Msg(' ')
            call Msg('-------Read data from CLN pt1:')
            call MUSG_ReadCLN(Modflow)  ! based on subroutine SDIS2CLN1AR
            NEQS = NEQS + NCLNNDS

            call MUSG_ReadCLN_pt2(Modflow)  ! based on subroutine SDIS2CLN1AR
        endif
        
        IF(Modflow.iSWF/=0) THEN
            call Msg(' ')
            call Msg('-------Read data from SWF pt1:')
            call MUSG_ReadSWF(Modflow)  ! based on subroutine SDIS2SWF1AR
            NEQS = NEQS + NSWFNDS

            ! Young-jin handles this in SDIS2SWF1AR above so I think not required
            !call MUSG_ReadSWF_pt2(Modflow)  ! based on subroutine SDIS2CLN1AR
        endif

        !crm not reading ghost node stuff yet
        !C---------------------------------------------------------------------
        !C3-----READ GNC PACKAGE INPUT  (CONNECTIVITIES AND FRACTIONS)
        !C---------------------------------------------------------------------

        
        !C5------ALLOCATE SPACE FOR PARAMETERS AND FLAGS.
        ALLOCATE(IA(NEQS+1))
        ALLOCATE (IBOUND(NEQS+1))
        ALLOCATE(AREA(NEQS))
        IA = 0


        call Msg(' ')
        call Msg('-------Read second part DISU:')
        WRITE(FNumEco,11) Modflow.iDISu
        11 FORMAT(1X,/1X,'DIS -- UNSTRUCTURED GRID DISCRETIZATION PACKAGE,',&
            ' VERSION 1 : 5/17/2010 - INPUT READ FROM UNIT ',I4)
        if(Modflow.unstructured) then

            !C     *****************************************************************
            !C     READ AND SET NODLAY ARRAY, AND READ GEOMETRIC PARAMETERS AND
            !C     MATRIX CONNECTIVITY FOR UNSTRUCTURED GRID
            !C     *****************************************************************
            call MUSG_ReadDISU_pt2(Modflow)  ! based on subroutine SGWF2DIS8UR
            
            ! Hardwired to read CLn and FAHL arrays for now 
            call MUSG_ReadDISU_pt3(Modflow)  

            !ENDIF
        else
            ! call MUSG_ReadDISU_StucturedGridData(Modflow)
        endif
        
        !!C--------------------------------------------------------------------------
        !!C7H------PREPARE IDXGLO ARRAY FOR CLN/SWF DOMAIN
        !IF(Modflow.iCLN/=0)THEN
        !    !CALL FILLIDXGLO_CLN
        !ENDIF
        !IF(Modflow.iSWF/=0) THEN
        !    CALL FILLIDXGLO_SWF
        !ENDIF


        call Msg(' ')
        call Msg('-------Read Stress Period Data from DISU:')
        call MUSG_ReadDISU_StressPeriodData(Modflow)   
        
        
        !C7-----Allocate space for remaining global arrays.
        ALLOCATE (HNEW(NEQS))
        !ALLOCATE (HOLD(NEQS))
        !ALLOCATE (IFMBC)
        !IFMBC = 0
        !ALLOCATE (FMBE(NEQS))
        !ALLOCATE (Sn(NEQS),So(NEQS))
        !Sn = 1.0
        !So = 1.0
        !ALLOCATE (RHS(NEQS))
        !ALLOCATE (BUFF(NEQS))
        ALLOCATE (STRT(NEQS))
        !DDREF=>STRT
        !ALLOCATE (LAYHDT(NLAY))
        !ALLOCATE (LAYHDS(NLAY))
        !WRITE(IOUT,'(//)')

        !C------------------------------------------------------------------------
        !C10------Read rest of groundwater BAS Package file (IBOUND and initial heads)
        call Msg(' ')
        call Msg('-------Read IBOUND and initial heads from BAS6:')
        IF(IUNSTR.EQ.0)THEN
        !C10A-------FOR STRUCTURED GRIDS
            !CALL SGWF2BAS8SR
        ELSE
        !C10B-------FOR UNSTRUCTURED GRIDS
            CALL MUSG_ReadBAS6_IBOUND_IHEADS(Modflow)  ! based on subroutine SGWF2BAS8UR
        ENDIF

        
        !C
        !C-----------------------------------------------------------------------
        !C11-----SET UP OUTPUT CONTROL.
        call Msg(' ')
        call Msg('-------Read data from OC:')
        CALL MUSG_ReadOC(Modflow) ! based on subroutine SGWF2BAS7I  
        
        IF(Modflow.iLPF/=0) THEN
            !C
            !C-----------------------------------------------------------------------
            !C11-----Read LPF Package file 
            call Msg(' ')
            call Msg('-------Read data from LPF:')
            CALL MUSG_ReadLPF(Modflow) ! based on subroutine SGWF2BAS7I  
        end if
        
        IF(Modflow.iCLN/=0) THEN
            !C------------------------------------------------------------------------
            !C------Read rest of CLN Package file (IBOUND and initial heads)
            call Msg(' ')
            call Msg('-------Read IBOUND and initial heads from CLN:')
            CALL MUSG_ReadCLN_IBOUND_IHEADS(Modflow)  ! based on subroutine CLN2BAS1AR
        end if
        
        IF(Modflow.iSWF/=0) THEN
            !C------------------------------------------------------------------------
            !C------Read rest of SWF Package file (IBOUND and initial heads)
            call Msg(' ')
            call Msg('-------Read IBOUND and initial heads from SWF:')
            CALL MUSG_ReadSWF_IBOUND_IHEADS(Modflow)  ! based on subroutine SWF2BAS1AR
        end if
        
        IF(Modflow.iWEL/=0) THEN
            !C------------------------------------------------------------------------
            !C------Read WEL Package file
            call Msg(' ')
            call Msg('-------Read data from WEL:')
            CALL MUSG_ReadWEL(Modflow)  ! based on subroutine GWF2WEL7U1AR
        end if
        
        IF(Modflow.iCHD/=0) THEN
            !C------------------------------------------------------------------------
            !C------Read CHD Package file
            call Msg(' ')
            call Msg('-------Read data from CHD:')
            CALL MUSG_ReadCHD(Modflow)  ! based on subroutine GWF2CHD7U1AR
        end if

        IF(Modflow.iRCH/=0) THEN
            !C------------------------------------------------------------------------
            !C------Read RCH Package file
            call Msg(' ')
            call Msg('-------Read data from RCH:')
            CALL MUSG_ReadRCH(Modflow)  ! based on subroutine GWF2RCH8U1AR
            call MUSG_ReadRCH_StressPeriods(Modflow) ! based on subroutine GWF2RCH8U1RP
        end if

        call MUSG_WriteVolumeBudgetToTecplot(Modflow)
        
        call MUSG_CreateStepPeriodTimeFile(Modflow)

        
        call MUSG_ReadBinary_GWF_HDS_File(Modflow)
        call MUSG_ReadBinary_GWF_DDN_File(Modflow)
        !call MUSG_ReadBinary_GWF_CBB_File(Modflow)
        if(Modflow.gwf.have_mesh) then
		    call Msg('Generating mesh-based Tecplot output files for GWF:')
            call MUSG_GWF_HDS_DDN_ToTecplot(Modflow)
            call MUSG_GWF_IBOUND_ToTecplot(Modflow)
            call MUSG_GWF_CBB_ToTecplot(Modflow)
        else
		   call Msg('Generating cell-based Tecplot output files for GWF:')
           call MUSG_GWF_IBOUNDv2_ToTecplot(Modflow)
        endif
        
        IF(Modflow.iCLN/=0) THEN
            call MUSG_ReadBinary_CLN_HDS_File(Modflow)
            call MUSG_ReadBinary_CLN_DDN_File(Modflow)
            !call MUSG_ReadBinary_CLN_CBB_File(Modflow)
            if(Modflow.cln.have_mesh) then
    		    call Msg('Generating mesh-based Tecplot output files for CLN:')
                call MUSG_CLN_HDS_DDN_ToTecplot(Modflow)
                !call MUSG_CLN_CBB_ToTecplot(Modflow)
            else
		       call Msg('No cell-based Tecplot output files for CLN:')
               !call MUSG_CLN_IBOUNDv2_ToTecplot(Modflow)
            endif
                    
        end if
        
        IF(Modflow.iSWF/=0) THEN
            call MUSG_ReadBinary_SWF_HDS_File(Modflow)
            call MUSG_ReadBinary_SWF_DDN_File(Modflow)
            !call MUSG_ReadBinary_SWF_CBB_File(Modflow)
            if(Modflow.swf.have_mesh) then
    		    call Msg('Generating mesh-based Tecplot output files for SWF:')
                call MUSG_SWF_HDS_DDN_ToTecplot(Modflow)
                !call MUSG_SWF_CBB_ToTecplot(Modflow)
            else
		       call Msg('No cell-based Tecplot output files for SWF:')
               !call MUSG_CLN_IBOUNDv2_ToTecplot(Modflow)
            endif
                    
        end if


        !open(Modflow.iSCAN,file=Modflow.FNameSCAN,status='unknown',form='formatted')
        !write(Modflow.iSCAN,'(a)') 'Scan file from project '//Modflow.prefix(:len_trim(Modflow.prefix))
        write(Modflow.iSCAN,'(a,i8,a)') 'Found ',Modflow.nKeyWord,' keywords'
        !do i=1,Modflow.nKeyWord
        !    write(Modflow.iSCAN,'(a)',iostat=status) Modflow.KeyWord(i)
        !end do
        close(Modflow.iSCAN)
        
    end subroutine MUSG_ProcessFiles
    
    SUBROUTINE SGWF2BAS8OPEN(INUNIT,IOUT,IUNIT,CUNIT,&
                   NIUNIT,VERSION,INBAS,MAXUNIT, modflow)
!     ******************************************************************
!     OPEN FILES.
!     ******************************************************************
!
!        SPECIFICATIONS:
!     ------------------------------------------------------------------
      USE NAMEFILEMODULE
      USE GWFBASMODULE, ONLY:IFLUSHS,CFLUSH
      INCLUDE 'openspec.inc'
      integer :: IUNIT(NIUNIT)
      CHARACTER*4 CUNIT(NIUNIT)
      CHARACTER*7 FILSTAT
      CHARACTER*20 FILACT, FMTARG, ACCARG
      CHARACTER*(*) VERSION
      CHARACTER*40 SPACES
      CHARACTER*300 LINE, FNAME
      CHARACTER*20 FILTYP
      LOGICAL LOP
      INTEGER, DIMENSION(99) ::TMPFLUSHS !kkz -tmp for list of binary output unit numbers to flush
      
      integer :: iout, inunit, maxunit, inbas, i, lenver, indent, niunit
      integer :: lloc, ityp1, ityp2
      real :: r
      integer :: n, istop, iu, istart, inam2, inam1, iflen, iflush, iopt2, iopt1, ii
      
      type (MUSG_Project) Modflow

      
!     ---------------------------------------------------------------
!
!1------INITIALIZE CONSTANTS.
      INBAS=0
      NFILE=0
      IOUT=0
      DO 5 I=1,NIUNIT
      IUNIT(I)=0
5     CONTINUE
      SPACES=' '
      LENVER=LEN_TRIM(VERSION)
      INDENT=40-(LENVER+8)/2
      !ALLOCATE(CFLUSH) !kkz - allocate and initialize count of binary files to flush; move to alloc and dealloc if keeping
      CFLUSH=1         !kkz - initialize at 1 to account for mandatory output listing file
!
!2------READ A LINE; IGNORE BLANK LINES AND PRINT COMMENT LINES.
10    READ(INUNIT,'(A)',END=1000) LINE
      IF(LINE.EQ.' ') GO TO 10
      IF(LINE(1:1).EQ.'#') THEN
        IF(NFILE.NE.0 .AND. IOUT.NE.0) WRITE(IOUT,'(A)') LINE
        GO TO 10
      END IF
!
!3------DECODE THE FILE TYPE, UNIT NUMBER, AND NAME.
      LLOC=1
      CALL URWORD(LINE,LLOC,ITYP1,ITYP2,1,N,R,IOUT,INUNIT)
      FILTYP=LINE(ITYP1:ITYP2)
      CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,IU,R,IOUT,INUNIT)
      CALL URWORD(LINE,LLOC,INAM1,INAM2,0,N,R,IOUT,INUNIT)
      IFLEN=INAM2-INAM1+1
      FNAME(1:IFLEN)=LINE(INAM1:INAM2)
      INQUIRE(UNIT=IU,OPENED=LOP)
      IF(LOP) THEN
         IF(IOUT.EQ.0) THEN
            WRITE(*,11) FNAME(1:IFLEN),IU
   11       FORMAT(1X,/1X,'CANNOT OPEN ',A,' ON UNIT',I4,&
                   ' BECAUSE UNIT IS ALREADY BEING USED')
         ELSE
            WRITE(IOUT,11) FNAME(1:IFLEN),IU
         END IF
         CALL USTOP(' ')
      END IF
!
!4------KEEP TRACK OF LARGEST UNIT NUMBER
      IF (IU.GT.MAXUNIT) MAXUNIT = IU
!
!5------SET DEFAULT FILE ATTRIBUTES.
      FMTARG='FORMATTED'
      ACCARG='SEQUENTIAL'
      FILSTAT='UNKNOWN'
      FILACT=' '
!4A-IHM--ALLOW FOR UNFORMATTED PACKAGES WITH A NEGATIVE UNIT NUMBER
      IF(IU.LT.0)THEN
        FMTARG = 'BINARY'
        IU = ABS(IU)
      ENDIF
!
!6------SPECIAL CHECK FOR 1ST FILE.
      IF(NFILE.EQ.0) THEN
        IF(FILTYP.EQ.'LIST') THEN
          !IOUT=IU
          Modflow.iLIST=IU
          Modflow.FNameLIST=FNAME(1:IFLEN)
          OPEN(UNIT=IU,FILE=FNAME(1:IFLEN),STATUS='OLD',&
               FORM='FORMATTED',ACCESS='SEQUENTIAL',SHARE = 'DENYNONE'&
               , BUFFERED='NO')
!          WRITE(IOUT,60) MFVNAM,SPACES(1:INDENT),VERSION(1:LENVER)
!60        FORMAT(34X,'USG-TRANSPORT ',A,/,&
!                  6X,' FURTHER  DEVELOPMENTS BASED ON MODFLOW-USG',/,&
!                  A,'VERSION ',A,/)
!          WRITE(IOUT,78) FNAME(1:IFLEN),IOUT
!78        FORMAT(1X,'LIST FILE: ',A,/25X,'UNIT ',I4)
          TMPFLUSHS(CFLUSH)=IU   !kkz - store unit number of the listing file to flush in the tmp array
        ELSE
          WRITE(*,*)&
            ' FIRST ENTRY IN NAME FILE MUST BE "LIST".'
          CALL USTOP(' ')
        END IF
!7  Get next file name
        NFILE=1
        GO TO 10
      END IF
!
!8------CHECK FOR "BAS" FILE TYPE.
      IF(FILTYP.EQ.'BAS6') THEN
         INBAS=IU
         FILSTAT='OLD    '
         FILACT=ACTION(1)
!
!9------CHECK FOR "UNFORMATTED" FILE TYPE.
      ELSE IF(FILTYP.EQ.'DATA(BINARY)' .OR.&
             FILTYP.EQ.'DATAGLO(BINARY)') THEN
         FMTARG=FORM
         ACCARG=ACCESS
         
!
!kkz check for both UNFORMATTED as well as BINARY per JCH
!9------CHECK FOR "UNFORMATTED" FILE TYPE.
      ELSE IF(FILTYP.EQ.'DATA(UNFORMATTED)' .OR.&
             FILTYP.EQ.'DATAGLO(UNFORMATTED)') THEN
         FMTARG=FORM
         ACCARG=ACCESS
!
!10-----CHECK FOR "FORMATTED" FILE TYPE.
      ELSE IF(LINE(ITYP1:ITYP2).EQ.'DATA' .OR.&
             LINE(ITYP1:ITYP2).EQ.'DATAGLO') THEN
         FMTARG='FORMATTED'
         ACCARG='SEQUENTIAL'
!
!11-----CHECK FOR MAJOR OPTIONS.
      ELSE
        DO 20 I=1,NIUNIT
           IF(LINE(ITYP1:ITYP2).EQ.CUNIT(I)) THEN
              IUNIT(I)=IU
              FILSTAT='OLD    '
              FILACT=ACTION(1)
! --------------IHM - SYF FILES ARE FOR IHM
              IF(CUNIT(I) .EQ. 'SYF') THEN
                FILSTAT = 'UNKNOWN'
                FILACT= ACTION (1)
                SYIU = IU
                SYFNAME = FNAME
                SYIFLEN = IFLEN
                ACCARG='SEQUENTIAL'
              ENDIF
! ---------IHM ----------------------------------
              GO TO 30
           END IF
20      CONTINUE
        WRITE(IOUT,21) LINE(ITYP1:ITYP2)
21      FORMAT(1X,'ILLEGAL FILE TYPE IN NAME FILE: ',A)
        CALL USTOP(' ')
30      CONTINUE
        END IF
!
!12-----FOR DATA FILES, CHECK FOR "REPLACE" OR "OLD" OPTION
      IARCVs(NFILE) = 1
      IFLUSH = 0
101   CALL URWORD(LINE,LLOC,IOPT1,IOPT2,1,N,R,IOUT,INUNIT)
      IF (FILSTAT.EQ.'UNKNOWN') THEN
        IF (LINE(IOPT1:IOPT2).EQ.'REPLACE' .OR.&
           LINE(IOPT1:IOPT2).EQ.'OLD')&
           FILSTAT = LINE(IOPT1:IOPT2)
      ENDIF
!12A -----IHM check for archive option for IHM
        IF(LINE(IOPT1:IOPT2).EQ.'FLUSH') THEN
          IFLUSH = 1
          WRITE(IOUT,70) FNAME(1:IFLEN)
   70     FORMAT(1X,'FLUSH OPTION IS USED FOR THIS FILE',A80)
        ENDIF
!12A -----IHM check for archive option for IHM
        IF(LINE(IOPT1:IOPT2).EQ.'NO-ARCHIVE') THEN
          IARCVs(nfile) = 0
          WRITE(IOUT,71) FNAME(1:IFLEN)
   71     FORMAT(1X,'NO-ARCHIVE OPTION IN IHM IS USED:',&
          ' STRESS PERIOD INFORMATION WILL BE REPLACED FOR THIS FILE.',&
           A20)
        ENDIF
! ----IHM ------------------------------------------------
      IF(LLOC.LT.300) GO TO 101
201   CONTINUE
!12A----Open file as read-only when 'OLD' is present to allow parallel
!12A----model runs to read data from file simultaneously.
      IF (FILACT.EQ.' ') THEN
        IF (FILSTAT.EQ.'OLD') THEN
          FILACT=ACTION(1)
        ELSE
          FILACT=ACTION(2)
        ENDIF
      ENDIF
!
!13-----WRITE THE FILE NAME AND OPEN IT.
      WRITE(IOUT,50) FNAME(1:IFLEN),&
          LINE(ITYP1:ITYP2),IU,FILSTAT,FMTARG,ACCARG
50    FORMAT(1X,/1X,'OPENING ',A,/&
       1X,'FILE TYPE:',A,'   UNIT ',I4,3X,'STATUS:',A,/&
       1X,'FORMAT:',A,3X,'ACCESS:',A)
!kkz - ALWAYS BUFFERING. NO IF STATEMENT OR OPTION
      OPEN(UNIT=IU,FILE=FNAME(1:IFLEN),FORM=FMTARG, SHARE = 'DENYNONE',&  !allows sharing of files for parallel PEST runs&
           ACCESS=ACCARG,STATUS=FILSTAT,ACTION=FILACT,ERR=2000)
!      OPEN(UNIT=IU,FILE=FNAME(1:IFLEN),FORM=FMTARG, SHARE = 'DENYNONE',  !allows sharing of files for parallel PEST runs&
!    1      ACCESS=ACCARG,STATUS=FILSTAT,ACTION=FILACT,BUFFERED='YES',&
!    2      ERR=2000)
      IF(IFLUSH.NE.0) THEN   !kkz - if not STATUS=OLD, then assume an output file to flush at the end of each timestep
        CFLUSH = CFLUSH + 1    !kkz - increment counter for files to be flushed
        TMPFLUSHS(CFLUSH)=IU   !kkz - store unit number of an output file to flush in the tmp array
      ENDIF
      
      !rgm workaround to assign binary output file unit numbers and names to Modflow data structure
      IF(FILTYP.EQ.'DATA(BINARY)') THEN
         call lcase(FNAME(1:IFLEN))
         if(index(FNAME(1:IFLEN),'cln.hds') /= 0) then
             modflow.iHDS_CLN=IU
             modflow.FNameHDS_CLN=FNAME(1:IFLEN)
         else if(index(FNAME(1:IFLEN),'swf.hds') /= 0) then
             modflow.iHDS_SWF=IU
             modflow.FNameHDS_SWF=FNAME(1:IFLEN)
         else if(index(FNAME(1:IFLEN),'gwf.hds') /= 0) then
             modflow.iHDS=IU
             modflow.FNameHDS=FNAME(1:IFLEN)
         else if(index(FNAME(1:IFLEN),'cln.ddn') /= 0) then
             modflow.iDDN_CLN=IU
             modflow.FNameDDN_CLN=FNAME(1:IFLEN)
         else if(index(FNAME(1:IFLEN),'swf.ddn') /= 0) then
             modflow.iDDN_SWF=IU
             modflow.FNameHDS_SWF=FNAME(1:IFLEN)
         else if(index(FNAME(1:IFLEN),'gwf.ddn') /= 0) then
             modflow.iDDN=IU
             modflow.FNameDDN=FNAME(1:IFLEN)
         else if(index(FNAME(1:IFLEN),'cln.cbb') /= 0) then
             modflow.iCBB_CLN=IU
             modflow.FNameCBB_CLN=FNAME(1:IFLEN)
         else if(index(FNAME(1:IFLEN),'swf.cbb') /= 0) then
             modflow.iCBB_SWF=IU
             modflow.FNameCBB_SWF=FNAME(1:IFLEN)
         else if(index(FNAME(1:IFLEN),'gwf.cbb') /= 0) then
             modflow.iCBB=IU
             modflow.FNameCBB=FNAME(1:IFLEN)
         endif
      endif
     
      
      
!
!11a-------IHM save all file information for each unit that was opened.
      ii = nfile
      ius(ii) = iu
      fnames(ii) = fname(1:iflen)
      iflens(ii) = iflen
      filstats(ii) = filstat
      filacts(ii) = filact
      fmtargs(ii) = fmtarg
      accargs(ii) = accarg
      if(ius(ii).eq.iunit(12)) iarcvs(ii) = 1  ! archive OC file
      nfiles = nfile
! IHM--------------------------------------------------
      NFILE=NFILE+1
      GO TO 10
!
!14-----END OF NAME FILE.  RETURN PROVIDED THAT LISTING FILE AND BAS
!14-----FILES HAVE BEEN OPENED.
1000  IF(NFILE.EQ.0) THEN
         WRITE(*,*) ' NAME FILE IS EMPTY.'
         CALL USTOP(' ')
      ELSE IF(INBAS.EQ.0) THEN
         WRITE(IOUT,*) ' BAS PACKAGE FILE HAS NOT BEEN OPENED.'
         CALL USTOP(' ')
      END IF
      CLOSE (UNIT=INUNIT)
!
      !kkz - allocate and fill IFLUSHS from tmp array
      ALLOCATE(IFLUSHS(CFLUSH))
      DO 1550 I=1,CFLUSH
          IFLUSHS(I) = TMPFLUSHS(I)
1550  CONTINUE
!
      RETURN
!
!15-----ERROR OPENING FILE.
 2000 CONTINUE
      WRITE(*,2010)FNAME(1:IFLEN),IU,FILSTAT,FMTARG,ACCARG,FILACT
      WRITE(IOUT,2010)FNAME(1:IFLEN),IU,FILSTAT,FMTARG,ACCARG,FILACT
 2010 FORMAT(/,1X,'*** ERROR OPENING FILE "',A,'" ON UNIT ',I5,/,&
     7X,'SPECIFIED FILE STATUS: ',A,/&
     7X,'SPECIFIED FILE FORMAT: ',A,/&
     7X,'SPECIFIED FILE ACCESS: ',A,/&
     7X,'SPECIFIED FILE ACTION: ',A,/&
     2X,'-- STOP EXECUTION (SGWF2BAS7OPEN)')
      CALL USTOP(' ')
!
    END SUBROUTINE SGWF2BAS8OPEN

   
    subroutine openMUSGFile(FileType,line,prefix,iUnit,FName)
        implicit none
        
        character(*) :: FileType
        character(*) :: line
        character(*) :: prefix
        integer :: iUnit
        character(*) :: FName
        
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
	    call getunit(iUnit)
        open(iUnit,file=FName,status='unknown',form='formatted')  
        call Msg('Opened ascii '//FileType(:len_trim(FileType))//' file: '//FName(:len_trim(FName)))
        write(TmpSTR,'(i5)') iUnit
        call Msg('Reading from unit: '//TmpSTR(:len_trim(TmpSTR)))
        
    end subroutine openMUSGFile
    
    subroutine openBinaryMUSGFile(FileType,line,prefix,iUnit,FName)
        implicit none
        
        character(*) :: FileType
        character(*) :: line
        character(*) :: prefix
        integer :: iUnit
        character(*) :: FName
        
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
        call Msg('Opened binary '//FileType(:len_trim(FileType))//' file: '//FName)
	    call getunit(iUnit)
        open(iUnit,file=FName,status='old',form='binary',action='read')  
        write(TmpSTR,'(i5)') iUnit
        call Msg('Reading from unit: '//TmpSTR(:len_trim(TmpSTR)))
        
    end subroutine openBinaryMUSGFile

    
    subroutine MUSG_ReadOC(Modflow)
        !SUBROUTINE SGWF2BAS7I(NLAY,INOC,IOUT,IFREFM,NIUNIT,ITRUNIT,ICUNIT)
        !     ******************************************************************
        !     SET UP OUTPUT CONTROL.
        !     ******************************************************************
        !
        !        SPECIFICATIONS:
        !     ------------------------------------------------------------------
        USE GLOBAL, ONLY: NSTP,ISSFLG,NPER,INCLN,IDPF,IUNIT,&
        NEQS
        USE GWFBASMODULE, ONLY: IHEDFM,IDDNFM,IHEDUN,IDDNUN,IPEROC,ITSOC,&
        CHEDFM,CDDNFM,IBDOPT,LBHDSV,LBDDSV,IBOUUN,LBBOSV,CBOUFM,&
        IAUXSV,IOFLG,VBVL,VBNM,ISPCFM,ISPCUN,CSPCFM,IATS,NPTIMES,&
        NPSTPS,DELTAT,TMINAT,TMAXAT,TADJAT,TCUTAT,&
        IDDREF,IDDREFNEW,IUDFAST,IUDFASTC,&
        IFAST,ISPFAST,ITSFAST,IUGFAST,IUCFAST,IFASTH,&
        IFASTC,ISPFASTC,ITSFASTC,IUGFASTC,IUCFASTC,IUMFASTC,&
        IUGBOOT,IUCBOOT,IUDBOOT,IBOOT,&
        BOOTSCALE,BOOTSLOPE,IBOOTSCALE,HREADBOOT
        USE CLN1MODULE, ONLY: ICLNHD, ICLNDD, ICLNIB,ICLNCN
        implicit none
    
        type (MUSG_Project) Modflow
        
        CHARACTER*400 LINE
        integer :: inoc, lloc, in, istart, k
        real :: r
        integer :: istop, n, itrunit, icunit
        
        itrunit=0   !rgm for now assume no bct (block-centred transport) file opened
        icunit=Modflow.iCLN
        
        !     ------------------------------------------------------------------
        !
        !1-----ALLOCATE SPACE FOR IOFLG, VBVL, AND VBNM ARRAYS.
        ALLOCATE (IOFLG(NLAY,7))
        ALLOCATE (VBVL(4,NIUNIT))
        ALLOCATE (VBNM(NIUNIT))
        IDDREF=0
        IDDREFNEW=0
        !
        !1A------ASSIGN DEFAULT VALUES.
        CHEDFM=' '
        CDDNFM=' '
        CSPCFM=' '
        CBOUFM='(20I4)'
        IHEDFM=0
        IDDNFM=0
        ISPCFM=0
        IHEDUN=0
        IDDNUN=0
        ISPCUN=0
        IBOUUN=0
        IBDOPT=1
        LBHDSV=0
        LBDDSV=0
        LBBOSV=0
        IAUXSV=0
        !
        !2------TEST OUTPUT CONTROL INPUT UNIT TO SEE IF OUTPUT CONTROL IS
        !2------ACTIVE.
        IF( Modflow.iOC/=0) THEN
            INOC=Modflow.iOC
        else
            INOC=0
        endif
        IF(INOC.LE.0) THEN
            !
            !2A-----OUTPUT CONTROL IS INACTIVE. PRINT A MESSAGE LISTING DEFAULTS.
            WRITE(IOUT, 41)
            41    FORMAT(1X,/1X,'DEFAULT OUTPUT CONTROL',/1X,&
            'THE FOLLOWING OUTPUT COMES AT THE END OF EACH STRESS PERIOD:')
            WRITE(IOUT, 42)
            42    FORMAT(1X,'TOTAL VOLUMETRIC BUDGET')
            WRITE(IOUT, 43)
            43    FORMAT(1X,10X,'HEAD')
            !
            !2B-----SET DEFAULT FLAGS IN IOFLG SO THAT HEAD IS PRINTED FOR
            !2B-----EVERY LAYER.
            DO 80 K=1,NLAY
                IOFLG(K,1)=1
                IOFLG(K,2)=0
                IOFLG(K,3)=0
                IOFLG(K,4)=0
                IOFLG(K,5)=0
                IOFLG(K,6)=0
                IOFLG(K,7)=0
            80    CONTINUE
            GO TO 1000
        END IF
        !
        !3------OUTPUT CONTROL IS ACTIVE.  READ FIRST RECORD AND DECODE FIRST
        !3------WORD.  MUST USE URWORD IN CASE FIRST WORD IS ALPHABETIC.
        CALL URDCOM(INOC,IOUT,LINE)
        !--------------------------------------------------------------------------------
        !3A------CHECK FOR OPTIONS
        !ALLOCATE(IATS,NPTIMES,NPSTPS,IBUDFLAT,ICBCFLAT,IHDDFLAT,ISPCFLAT)
        !ALLOCATE(DELTAT,TMINAT,TMAXAT,TADJAT,TCUTAT)
        !ALLOCATE(IFAST,ISPFAST,ITSFAST,IUGFAST,IUCFAST,IFASTH)
        !ALLOCATE(IFASTC,ISPFASTC,ITSFASTC,IUGFASTC,IUCFASTC,IUMFASTC,&
        !IUDFAST,IUDFASTC)
        !ALLOCATE (IUGBOOT,IUCBOOT,IUDBOOT,IBOOT,IBOOTSCALE)
        !ALLOCATE (DTBOOTSCALE)
        IATS=0
        NPTIMES=0
        NPSTPS=0
        !-----initialize booting option flags
        IUGBOOT = 0
        IUCBOOT = 0
        IUDBOOT = 0
        IBOOT = 0
        IBOOTSCALE = 0
        !-----initialize fastforward option flags
        IFAST=0
        IFASTH=0
        IFASTC=0
        ISPFAST=0
        ITSFAST=0
        IUGFAST=0
        IUCFAST=0
        IUDFAST = 0
        !
        ISPFASTC=0
        ITSFASTC=0
        IUGFASTC=0
        IUCFASTC=0
        IUDFASTC = 0
        IUMFASTC = 0
        
        LLOC = 1
        10 CALL URWORD(LINE,LLOC,ISTART,ISTOP,1,N,R,IOUT,IN)
        IF(LINE(ISTART:ISTOP).EQ.'ATS'.OR.&
            LINE(ISTART:ISTOP).EQ.'ATSA') THEN
            !3B------READ KEYWORD OPTION ATS FOR ADAPTIVE TIME STEPPING.
            IATS = 1
        ELSEIF(LINE(ISTART:ISTOP).EQ.'NPTIMES') THEN
            !3C------IS KEWORD OPTION FOR NUMBER OF PRINT TIMES
            CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,NPTIMES,R,IOUT,INOC)
        ELSEIF(LINE(ISTART:ISTOP).EQ.'NPSTPS') THEN
            !3C------IS KEWORD OPTION FOR NUMBER OF PRINT TIMES
            CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,NPSTPS,R,IOUT,INOC)
            WRITE(IOUT,14) NPSTPS
            14      FORMAT(/1X,'OUTPUT PROVIDED EVERY', I8,' TIME STEPS (NPSTPS)')
        ELSEIF(LINE(ISTART:ISTOP).EQ.'FASTFORWARD') THEN
            IFAST = 1
            IFASTH = 1
            !3C------IS KEWORD OPTION FOR FASTFORWARD FROM SEPARATE FILE
            CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,ISPFAST,R,IOUT,INOC)
            CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,ITSFAST,R,IOUT,INOC)
            CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,IUGFAST,R,IOUT,INOC)
            WRITE(IOUT,15) ISPFAST,ITSFAST,IUGFAST
            15      FORMAT(/1X,'FASTFORWARDING TO STRESS PERIOD,',I10,&
            ' AND TIME-STEP',I10&
            /10X,'READING FASTFORWARD GWF HEADS FROM UNIT',I5)
            IF(INCLN.GT.0)THEN
                CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,IUCFAST,R,IOUT,INOC)
                WRITE(IOUT,16) IUCFAST
                16        FORMAT(10X,'READING FASTFORWARD CLN HEADS FROM UNIT',I5)
            ENDIF
            IF(IDPF.GT.0)THEN
                CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,IUDFAST,R,IOUT,INOC)
                WRITE(IOUT,17) IUDFAST
                17        FORMAT(10X,'READING FASTFORWARD DDF HEADS FROM UNIT',I5)
            ENDIF
        ELSEIF(LINE(ISTART:ISTOP).EQ.'FASTFORWARDC') THEN
            IFAST = 1
            IFASTC = 1
            !3C------IS KEWORD OPTION FOR FASTFORWARD OF CONC FROM SEPARATE FILE
            CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,ISPFASTC,R,IOUT,INOC)
            CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,ITSFASTC,R,IOUT,INOC)
            CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,IUGFASTC,R,IOUT,INOC)
            WRITE(IOUT,25) ISPFASTC,ITSFASTC,IUGFASTC
            25      FORMAT(/1X,'FASTFORWARDING TO STRESS PERIOD,',I10,&
            ' AND TIME-STEP',I10&
            /10X,'READING FASTFORWARD GWF CONC FROM UNIT',I5)
            IF(INCLN.GT.0)THEN
                CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,IUCFASTC,R,IOUT,INOC)
                WRITE(IOUT,26) IUCFASTC
                26        FORMAT(10X,'READING FASTFORWARD CLN CONC FROM UNIT',I5)
            ENDIF
            IF(IUNIT(30).GT.0)THEN
                CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,IUDFASTC,R,IOUT,INOC)
                WRITE(IOUT,27) IUDFASTC
                27        FORMAT(10X,'READING FASTFORWARD DDT CONCS FROM UNIT',I5)
            ENDIF
            !        IF(ImdT.GT.0)THEN
            !          CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,IUMFASTC,R,IOUT,INOC)
            !          WRITE(IOUT,28) IUMFASTC
            !28        FORMAT(10X,'READING FASTFORWARD MATRIX CONCS FROM UNIT',I5)
            !        ENDIF
        ELSEIF(LINE(ISTART:ISTOP).EQ.'BOOTSTRAPPING') THEN
            !3C------IS KEWORD OPTION FOR BOOTSTRAPPING THE NEW ESTIMATE FOR HEAD AT FIRST ITERATION FROM SEPARATE FILE
            CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,IUGBOOT,R,IOUT,INOC)
            WRITE(IOUT,29) IUGBOOT
            29      FORMAT(/1X,'BOOTSTRAPPING IS DONE FOR TRANSIENT SIMULATION,',/&
            10X,'READING BOOTSTRAP GWF HEADS FROM UNIT',I5)
            IF(INCLN.GT.0)THEN
                CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,IUCBOOT,R,IOUT,INOC)
                WRITE(IOUT,30) IUCBOOT
                30        FORMAT(10X,'READING BOOTSTRAP CLN HEADS FROM UNIT',I5)
            ENDIF
            IF(IUNIT(30).GT.0)THEN
                CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,IUDBOOT,R,IOUT,INOC)
                WRITE(IOUT,31) IUDBOOT
                31        FORMAT(10X,'READING BOOTSTRAP DDF HEADS FROM UNIT',I5)
            ENDIF
        END IF
        !
        IF(LLOC.LT.200) GO TO 10
        !
        !-------ALLOCATE SPACE FOR BOOTSTRAPING ARRAYS
        IF(IUGBOOT.NE.0)THEN
            ALLOCATE (BOOTSCALE(NEQS),BOOTSLOPE(NEQS),HREADBOOT(NEQS))
            BOOTSCALE = 1.0
        ENDIF
        !-------SET DEFAULTS FOR ADAPTIVE TIME STEPPING
        IF(NPSTPS.GT.0.OR.NPTIMES.GT.0) IATS=1
        IF(NPSTPS.EQ.0) NPSTPS=100000000
        IF(IATS.EQ.1)THEN
            WRITE(IOUT,13)
            13   FORMAT(/1X,'ADAPTIVE TIME STEPPING PERFORMED. ATS',&
            ' VARIABLES WILL BE READ.'/1X,61('-'))
            DO K=1,NPER
                IF(ISSFLG(K).EQ.1.AND.ITRUNIT.EQ.0) THEN
                    NSTP(K) = 1
                ELSE
                    NSTP(K) = 1000000
                ENDIF
            ENDDO
            DELTAT = 1.0
            TMINAT = 1.0E-10
            TMAXAT = 1.0E10
            TADJAT = 2.0
            TCUTAT = 5.0
        ENDIF
        !------------------------------------------------------------------------------
        LLOC=1
        CALL URWORD(LINE,LLOC,ISTART,ISTOP,1,N,R,IOUT,INOC)
        !
        !4------TEST FOR NUMERIC OUTPUT CONTROL.  FIRST WORD WILL NOT BE
        !4------"PERIOD", "HEAD", "DRAWDOWN", OR "COMPACT" OR CONC OR CONCENTRATION.
        IF(LINE(ISTART:ISTOP).NE.'PERIOD' .AND. LINE(ISTART:ISTOP).NE.&
        'HEAD' .AND. LINE(ISTART:ISTOP).NE.'DRAWDOWN' .AND.&
        LINE(ISTART:ISTOP).NE.'COMPACT' .AND.&
        LINE(ISTART:ISTOP).NE.'IBOUND'.AND.&
        LINE(ISTART:ISTOP).NE.'CONC'.AND.&
        LINE(ISTART:ISTOP).NE.'CONCENTRATION'.AND.&
        LINE(ISTART:ISTOP).NE.'FASTFORWARD'.AND.&
        LINE(ISTART:ISTOP).NE.'FASTFORWARDC'.AND.&
        LINE(ISTART:ISTOP).NE.'BOOTSTRAPPING'.AND.&
        LINE(ISTART:ISTOP).NE.'ATSA') THEN
            !4A-----NUMERIC OUTPUT CONTROL.  DECODE THE INITIAL RECORD ACCORDINGLY.
            WRITE(IOUT,102)
            102    FORMAT(1X,/1X,'OUTPUT CONTROL IS SPECIFIED EVERY TIME STEP')
            IF(ITRUNIT.EQ.0)THEN
                IF(IFREFM.EQ.0) THEN
                    READ(LINE,'(4I10)') IHEDFM,IDDNFM,IHEDUN,IDDNUN
                ELSE
                    LLOC=1
                    CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,IHEDFM,R,IOUT,INOC)
                    CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,IDDNFM,R,IOUT,INOC)
                    CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,IHEDUN,R,IOUT,INOC)
                    CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,IDDNUN,R,IOUT,INOC)
                END IF
                WRITE(IOUT,103) IHEDFM,IDDNFM
                103     FORMAT(1X,'HEAD PRINT FORMAT CODE IS',I4,&
                '    DRAWDOWN PRINT FORMAT CODE IS',I4)
                WRITE(IOUT,104) IHEDUN,IDDNUN
                104      FORMAT(1X,'HEADS WILL BE SAVED ON UNIT ',I4,&
                '    DRAWDOWNS WILL BE SAVED ON UNIT ',I4)
            ELSE
                IF(IFREFM.EQ.0) THEN
                    READ(LINE,'(6I10)')IHEDFM,IDDNFM,IHEDUN,IDDNUN,ISPCFM,ISPCUN
                ELSE
                    LLOC=1
                    CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,IHEDFM,R,IOUT,INOC)
                    CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,IDDNFM,R,IOUT,INOC)
                    CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,IHEDUN,R,IOUT,INOC)
                    CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,IDDNUN,R,IOUT,INOC)
                    CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,ISPCFM,R,IOUT,INOC)
                    CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,ISPCUN,R,IOUT,INOC)
                END IF
                WRITE(IOUT,113) IHEDFM,IDDNFM,ISPCFM
                113     FORMAT(1X,'HEAD PRINT FORMAT CODE IS',I4,&
                '    DRAWDOWN PRINT FORMAT CODE IS',I4,&
                '        CONC PRINT FORMAT CODE IS',I4)
                WRITE(IOUT,114) IHEDUN,IDDNUN,ISPCUN
                114      FORMAT(1X,'HEADS WILL BE SAVED ON UNIT ',I4,&
                '    DRAWDOWNS WILL BE SAVED ON UNIT ',I4,&
                '         CONC WILL BE SAVED ON UNIT ',I4)
            ENDIF
            IPEROC=-1
            ITSOC=-1
            !4B---READ OUTPUT TIME SERIES VECTOR
            IF(NPTIMES.GT.0) CALL PTIMES1RP(INOC,IOUT,modflow)
        ELSE
            !----------------------------------------------------------------------------------------
            !4C---FOR ALPHABETIC OC, READ OUTPUT TIME SERIES VECTOR FIRST
            IF(NPTIMES.GT.0) CALL PTIMES1RP(INOC,IOUT,modflow)
            !4D-----ALPHABETIC OUTPUT CONTROL.  CALL MODULE TO READ INITIAL RECORDS.
            CALL SGWF2BAS7J(INOC,IOUT,LINE,LLOC,ISTART,ISTOP)
        ENDIF
        IF(ICUNIT.GT.0) THEN
            IF(ICLNHD.LT.0) ICLNHD = IHEDUN
            IF(ICLNDD.LT.0) ICLNDD = IDDNUN
            IF(ICLNIB.LT.0) ICLNIB = IBOUUN
            IF(ICLNCN.LT.0) ICLNCN = ISPCUN
        ENDIF
        !
        !5------RETURN.
        1000 RETURN
    END subroutine MUSG_ReadOC
    
    SUBROUTINE SGWF2BAS7J(INOC,IOUT,LINE,LLOC,ISTART,ISTOP)
        !     ******************************************************************
        !     READ INITIAL ALPHABETIC OUTPUT CONTROL RECORDS.
        !     ******************************************************************
        !
        !        SPECIFICATIONS:
        !     ------------------------------------------------------------------
        USE GLOBAL, ONLY: ITRNSP
        USE GWFBASMODULE, ONLY: IHEDFM,IDDNFM,IHEDUN,IDDNUN,IPEROC,ITSOC,&
                    CHEDFM,CDDNFM,IBDOPT,LBHDSV,LBDDSV,IATS,IDDREFNEW,&
                    IBOUUN,LBBOSV,CBOUFM,IAUXSV,ISPCFM,ISPCUN,CSPCFM
        !
        CHARACTER*400 LINE
        
        integer :: inoc, iout, lloc, istart, istop
        real :: r
        integer :: n
        !     ------------------------------------------------------------------
        !
        !1------ALPHABETIC OUTPUT CONTROL.  WRITE MESSAGE AND SET INITIAL VALUES
        !1------FOR IPEROC AND ITSOC.
        WRITE(IOUT,91)
        91 FORMAT(1X,/1X,'OUTPUT CONTROL IS SPECIFIED ONLY AT TIME STEPS',&
        ' FOR WHICH OUTPUT IS DESIRED')
        IPEROC=9999
        ITSOC=9999
        !
        !2------LOOK FOR ALPHABETIC WORDS:
        
        !2A-----LOOK FOR "PERIOD", WHICH INDICATES THE END OF INITIAL OUTPUT
        !2A-----CONTROL DATA.  IF FOUND, DECODE THE PERIOD NUMBER AND TIME
        !2A-----STEP NUMBER FOR LATER USE.
    100 IF(LINE(ISTART:ISTOP).EQ.'PERIOD') THEN
            CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,IPEROC,R,IOUT,INOC)
            CALL URWORD(LINE,LLOC,ISTART,ISTOP,1,N,R,IOUT,INOC)
            !2Ai-------SKIP TIME STEP IF ADAPTIVE TIME STEPPING
            IF(IATS.NE.0) GO TO 20
            IF(LINE(ISTART:ISTOP).NE.'STEP') GO TO 2000
            CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,ITSOC,R,IOUT,INOC)
            CALL URWORD(LINE,LLOC,ISTART,ISTOP,1,N,R,IOUT,INOC)
            20       CONTINUE
            IF(ITRNSP.EQ.0)THEN
                WRITE(IOUT,101) IHEDFM,IDDNFM
                101      FORMAT(1X,'HEAD PRINT FORMAT CODE IS',I4,&
                    '    DRAWDOWN PRINT FORMAT CODE IS',I4)
                WRITE(IOUT,102) IHEDUN,IDDNUN
                102      FORMAT(1X,'HEADS WILL BE SAVED ON UNIT ',I4,&
                    '    DRAWDOWNS WILL BE SAVED ON UNIT ',I4)
            ELSE
                WRITE(IOUT,113) IHEDFM,IDDNFM,ISPCFM
                113      FORMAT(1X,'HEAD PRINT FORMAT CODE IS',I4,&
                    '    DRAWDOWN PRINT FORMAT CODE IS',I4,&
                    '        CONC PRINT FORMAT CODE IS',I4)
                WRITE(IOUT,122) IHEDUN,IDDNUN,ISPCUN
                122      FORMAT(1X,'HEADS WILL BE SAVED ON UNIT ',I4,&
                    '    DRAWDOWNS WILL BE SAVED ON UNIT ',I4,&
                    '        CONCS WILL BE SAVED ON UNIT ',I4)
            ENDIF
            !2Aii------READ DDREFERENCE FLAG
            IF(LINE(ISTART:ISTOP).EQ.'DDREFERENCE') THEN
                IDDREFNEW=1
            ELSE
                IDDREFNEW=0
            END IF
            !
            GO TO 1000
            
        !
        !2B-----LOOK FOR "HEAD PRINT ..." AND "HEAD SAVE ...".  IF
        !2B-----FOUND, SET APPROPRIATE FLAGS.
        ELSE IF(LINE(ISTART:ISTOP).EQ.'HEAD') THEN
            CALL URWORD(LINE,LLOC,ISTART,ISTOP,1,N,R,IOUT,INOC)
            IF(LINE(ISTART:ISTOP).EQ.'PRINT') THEN
                CALL URWORD(LINE,LLOC,ISTART,ISTOP,1,N,R,IOUT,INOC)
                IF(LINE(ISTART:ISTOP).NE.'FORMAT') GO TO 2000
                CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,IHEDFM,R,IOUT,INOC)
            ELSE IF(LINE(ISTART:ISTOP).EQ.'SAVE') THEN
                CALL URWORD(LINE,LLOC,ISTART,ISTOP,1,N,R,IOUT,INOC)
                IF(LINE(ISTART:ISTOP).EQ.'UNIT') THEN
                    CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,IHEDUN,R,IOUT,&
                        INOC)
                ELSE IF(LINE(ISTART:ISTOP).EQ.'FORMAT') THEN
                    CALL URWORD(LINE,LLOC,ISTART,ISTOP,0,N,R,IOUT,INOC)
                    CHEDFM=LINE(ISTART:ISTOP)
                    WRITE(IOUT,103) CHEDFM
                103          FORMAT(1X,'HEADS WILL BE SAVED WITH FORMAT: ',A)
                    CALL URWORD(LINE,LLOC,ISTART,ISTOP,1,N,R,IOUT,INOC)
                    IF(LINE(ISTART:ISTOP).EQ.'LABEL') THEN
                        LBHDSV=1
                        WRITE(IOUT,104)
                104     FORMAT(1X,'SAVED HEADS WILL BE LABELED')
                    END IF
                ELSE
                    GO TO 2000
                END IF
            ELSE
                GO TO 2000
            END IF
            
        !
        !2C-----LOOK FOR "DRAWDOWN PRINT ..." AND "DRAWDOWN SAVE ...".
        !2C-----IF FOUND, SET APPROPRIATE FLAGS
        ELSE IF(LINE(ISTART:ISTOP).EQ.'DRAWDOWN') THEN
            CALL URWORD(LINE,LLOC,ISTART,ISTOP,1,N,R,IOUT,INOC)
            IF(LINE(ISTART:ISTOP).EQ.'PRINT') THEN
                CALL URWORD(LINE,LLOC,ISTART,ISTOP,1,N,R,IOUT,INOC)
                IF(LINE(ISTART:ISTOP).NE.'FORMAT') GO TO 2000
                CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,IDDNFM,R,IOUT,INOC)
            ELSE IF(LINE(ISTART:ISTOP).EQ.'SAVE') THEN
                CALL URWORD(LINE,LLOC,ISTART,ISTOP,1,N,R,IOUT,INOC)
                IF(LINE(ISTART:ISTOP).EQ.'UNIT') THEN
                    CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,IDDNUN,R,IOUT,&
                            INOC)
                ELSE IF(LINE(ISTART:ISTOP).EQ.'FORMAT') THEN
                    CALL URWORD(LINE,LLOC,ISTART,ISTOP,0,N,R,IOUT,INOC)
                    CDDNFM=LINE(ISTART:ISTOP)
                    WRITE(IOUT,105) CDDNFM
                105          FORMAT(1X,'DRAWDOWN WILL BE SAVED WITH FORMAT: ',A)
                    CALL URWORD(LINE,LLOC,ISTART,ISTOP,1,N,R,IOUT,INOC)
                    IF(LINE(ISTART:ISTOP).EQ.'LABEL') THEN
                        LBDDSV=1
                        WRITE(IOUT,106)
                106             FORMAT(1X,'SAVED DRAWDOWN WILL BE LABELED')
                    END IF
                ELSE
                    GO TO 2000
                END IF
            ELSE
                GO TO 2000
            END IF
            
        !
        !2B-----LOOK FOR "CONC PRINT ..." AND "CONC SAVE ...".  IF
        !2B-----FOUND, SET APPROPRIATE FLAGS.
        ELSE IF(LINE(ISTART:ISTOP).EQ.'CONC'.OR.LINE(ISTART:ISTOP)&
            .EQ.'CONCENTRATION') THEN
            CALL URWORD(LINE,LLOC,ISTART,ISTOP,1,N,R,IOUT,INOC)
            IF(LINE(ISTART:ISTOP).EQ.'PRINT') THEN
                CALL URWORD(LINE,LLOC,ISTART,ISTOP,1,N,R,IOUT,INOC)
                IF(LINE(ISTART:ISTOP).NE.'FORMAT') GO TO 2000
                CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,ISPCFM,R,IOUT,INOC)
            ELSE IF(LINE(ISTART:ISTOP).EQ.'SAVE') THEN
                CALL URWORD(LINE,LLOC,ISTART,ISTOP,1,N,R,IOUT,INOC)
                IF(LINE(ISTART:ISTOP).EQ.'UNIT') THEN
                    CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,ISPCUN,R,IOUT,&
                        INOC)
                ELSE IF(LINE(ISTART:ISTOP).EQ.'FORMAT') THEN
                    CALL URWORD(LINE,LLOC,ISTART,ISTOP,0,N,R,IOUT,INOC)
                    CSPCFM=LINE(ISTART:ISTOP)
                    WRITE(IOUT,115) CSPCFM
                115          FORMAT(1X,'CONCS WILL BE SAVED WITH FORMAT: ',A)
                    CALL URWORD(LINE,LLOC,ISTART,ISTOP,1,N,R,IOUT,INOC)
                    IF(LINE(ISTART:ISTOP).EQ.'LABEL') THEN
                        LBHDSV=1
                        WRITE(IOUT,116)
                116     FORMAT(1X,'SAVED CONCS WILL BE LABELED')
                    END IF
                ELSE
                        GO TO 2000
                END IF
            ELSE
                GO TO 2000
            END IF
        !
        !2D-----LOOK FOR "COMPACT BUDGET FILES" -- "COMPACT" IS SUFFICIENT.
        !2D-----IF FOUND, SET APPROPRIATE FLAG.
        ELSE IF(LINE(ISTART:ISTOP).EQ.'COMPACT') THEN
            CALL URWORD(LINE,LLOC,ISTART,ISTOP,1,N,R,IOUT,INOC)
            IF(LINE(ISTART:ISTOP).EQ.'BUDGET') THEN
                IBDOPT=2
                WRITE(IOUT,107)
                107       FORMAT(1X,&
                'COMPACT CELL-BY-CELL BUDGET FILES WILL BE WRITTEN')
                CALL URWORD(LINE,LLOC,ISTART,ISTOP,1,N,R,IOUT,INOC)
                IF(LINE(ISTART:ISTOP).EQ.'AUXILIARY' .OR.&
                    LINE(ISTART:ISTOP).EQ.'AUX') THEN
                    IAUXSV=1
                    WRITE(IOUT,108)
                108 FORMAT(1X,&
                    'AUXILIARY DATA WILL BE SAVED IN CELL-BY-CELL BUDGET FILES')
                END IF
            ELSE
                GO TO 2000
            END IF
 
        !
        !2E-----LOOK FOR  "IBOUND SAVE ...".  IF FOUND, SET APPROPRIATE FLAGS.
        ELSE IF(LINE(ISTART:ISTOP).EQ.'IBOUND') THEN
            CALL URWORD(LINE,LLOC,ISTART,ISTOP,1,N,R,IOUT,INOC)
            IF(LINE(ISTART:ISTOP).EQ.'SAVE') THEN
                CALL URWORD(LINE,LLOC,ISTART,ISTOP,1,N,R,IOUT,INOC)
                IF(LINE(ISTART:ISTOP).EQ.'UNIT') THEN
                    CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,IBOUUN,R,IOUT,&
                        INOC)
                    WRITE(IOUT,111) IBOUUN
                111          FORMAT(1X,'IBOUND WILL BE SAVED ON UNIT ',I4)
                ELSE IF(LINE(ISTART:ISTOP).EQ.'FORMAT') THEN
                    CALL URWORD(LINE,LLOC,ISTART,ISTOP,0,N,R,IOUT,INOC)
                    CBOUFM=LINE(ISTART:ISTOP)
                    WRITE(IOUT,112) CBOUFM
                112          FORMAT(1X,'IBOUND WILL BE SAVED WITH FORMAT: ',A)
                    CALL URWORD(LINE,LLOC,ISTART,ISTOP,1,N,R,IOUT,INOC)
                    IF(LINE(ISTART:ISTOP).EQ.'LABEL') THEN
                        LBBOSV=1
                        WRITE(IOUT,109)
                109             FORMAT(1X,'SAVED IBOUND WILL BE LABELED')
                    END IF
                ELSE
                    GO TO 2000
                END IF
            ELSE
                GO TO 2000
            END IF
        
        !2F-------FOR ADAPTIVE TIME STEPPING WITH ALPHABTIC INPUT READ NEXT RECORD
        ELSE IF(LINE(ISTART:ISTOP).EQ.'ATSA') THEN
            GO TO 110
        
        ELSE IF(LINE(ISTART:ISTOP).EQ.'FASTFORWARD') THEN
            GO TO 110
        
        ELSE IF(LINE(ISTART:ISTOP).EQ.'FASTFORWARDC') THEN
            GO TO 110
        !
        !2F-----ERROR IF UNRECOGNIZED WORD.
        ELSE
            GO TO 2000
        END IF
        !
        !3------FINISHED READING A RECORD.  READ NEXT RECORD, IGNORING BLANK
        !3------LINES.  GO BACK AND DECODE IT.
        110 READ(INOC,'(A)',END=1000) LINE
        IF(LINE.EQ.' ') GO TO 110
        LLOC=1
        CALL URWORD(LINE,LLOC,ISTART,ISTOP,1,N,R,IOUT,INOC)
        GO TO 100
        !
        !4------RETURN.
        1000 RETURN
        !
        !5------ERROR DECODING INPUT DATA.
        2000 WRITE(IOUT,2001) LINE
        2001 FORMAT(1X,/1X,'ERROR READING OUTPUT CONTROL INPUT DATA:'/1X,A80)
        CALL USTOP(' ')
    END SUBROUTINE SGWF2BAS7J
    
    SUBROUTINE PTIMES1RP(INOC,IOUT,modflow)
        !       ******************************************************************
        !        READ PRINT TIME ARRAY.
        !       ******************************************************************
        !
        !       SPECIFICATIONS:
        !       ------------------------------------------------------------------
                USE GWFBASMODULE, ONLY: TIMOT,ITIMOT,TIMOTC,ITIMOTC,&
                    TMINAT,NPTIMES
        type (MUSG_Project) Modflow
                
        !
        
                integer :: inoc, iout, it, i
        !     ------------------------------------------------------------------
        !
        !1------READ PRINT TIME ARRAY
              ALLOCATE(TIMOT(NPTIMES+1),TIMOTC(NPTIMES+1))
              ALLOCATE(modflow.TIMOT(NPTIMES+1))
              
              !ALLOCATE(ITIMOT,ITIMOTC)
              ITIMOT=1
              ITIMOTC = 1   ! USE SEPARATE INDEX AND ARRAY FOR CONCENTRATIONS
        !
              WRITE(IOUT,11)NPTIMES
11            FORMAT(/10X,  'OUTPUT WILL BE PRINTED AT FOLLOWING',I8,' TIMES'/&
                     10X,  50('-'))
              
              modflow.ntime=nptimes
        !
              READ(INOC,*) (TIMOT(IT),IT=1,NPTIMES)
              WRITE(IOUT,'(10(1PG15.7))') (TIMOT(IT),IT=1,NPTIMES)
              TIMOT(NPTIMES+1) = 1.0E20
        !2------PERFORM CONSISTENCY CHECKS
              DO IT=1,NPTIMES-1
                IF(TIMOT(IT+1)-TIMOT(IT).LE.TMINAT) THEN
                  WRITE(IOUT,12)
        12        FORMAT( 'PRINT TIMES ARE NOT SEQUENTIALLY INCREASING',&
                 ' OR ARE LESS THAN TMINAT APART')
                  STOP
                ENDIF
              ENDDO
        !3 ---FILL TIMOTC WITH TIMOT FOR TRANSPORT
              DO I = 1,NPTIMES+1
                TIMOTC(I) = TIMOT(I)
                modflow.TIMOT(I) = TIMOT(I)
              ENDDO
        !
        !4------RETURN.
         1000 RETURN
    END SUBROUTINE PTIMES1RP
    
    SUBROUTINE MUSG_ReadLPF(Modflow)
        !     ******************************************************************
        !     ALLOCATE AND READ DATA FOR LAYER PROPERTY FLOW PACKAGE
        !     ******************************************************************
        !
        !        SPECIFICATIONS:
        !     ------------------------------------------------------------------
              USE GLOBAL, ONLY:NLAY,ITRSS,LAYHDT,LAYHDS,&
                         NCNFBD,IOUT,NOVFC,itrnsp,&
                         NODES,IUNSTR,ICONCV,NOCVCO,NJAS,IWADI,&
                         IDEALLOC_LPF,ARAD,iunsat,&
                         IOUTNORMAL
              USE GWFBCFMODULE,ONLY:IBCFCB,IWDFLG,IWETIT,IHDWET,WETFCT,HDRY,CV,&
                              LAYCON,LAYAVG,SC1,SC2,WETDRY,CHANI,IHANISO,&
                              IKCFLAG,LAYWET,ISFAC,ITHFLG,LAYAVGV,&
                              LAYTYP,LAYVKA,LAYSTRT,alpha,beta,sr,brook,&
                              LAYFLG,VKA,VKCB,HANI,HK,IBPN,BP,IDRY,IALTSTO,&
                ITABRICH,INTRICH,IUZONTAB,RETCRVS,NUTABROWS,NUZONES
              
              implicit none
              type (MUSG_Project) Modflow

        !
              CHARACTER*14 LAYPRN(5),AVGNAM(5),TYPNAM(3),VKANAM(2),WETNAM(2),&
                         HANNAM
              DATA AVGNAM/'      HARMONIC','   LOGARITHMIC','   LOG+ARITHM ',&
                '   ARITHMETIC ','FINITE ELEMENT'/
              DATA TYPNAM/'     CONFINED ','  CONVERTIBLE ','     UPSTREAM '/
              DATA VKANAM/'    VERTICAL K','    ANISOTROPY'/
              DATA WETNAM/'  NON-WETTABLE','      WETTABLE'/
              DATA HANNAM/'      VARIABLE'/
              CHARACTER*400 LINE
              CHARACTER*24 ANAME(10),STOTXT
              CHARACTER*4 PTYP
        !
              DATA ANAME(1) /'   HYD. COND. ALONG ROWS'/
              DATA ANAME(2) /'  HORIZ. ANI. (COL./ROW)'/
              DATA ANAME(3) /'     VERTICAL HYD. COND.'/
              DATA ANAME(4) /' HORIZ. TO VERTICAL ANI.'/
              DATA ANAME(5) /'QUASI3D VERT. HYD. COND.'/
              DATA ANAME(6) /'        SPECIFIC STORAGE'/
              DATA ANAME(7) /'          SPECIFIC YIELD'/
              DATA ANAME(8) /'        WETDRY PARAMETER'/
              DATA ANAME(9) /'     STORAGE COEFFICIENT'/
              DATA ANAME(10) /'UNSAT PARAMETER ZONE MAP'/
              
              integer :: in, lloc, istop, istart, i, k
              real :: r
              integer :: nplpf, nopchk, n
              integer :: izon, itrows,inlak
              integer :: NCNVRT
                integer :: NHANI
                integer :: NWETD
                integer :: ILAYUNSAT
                integer :: NPHK
                integer :: NPVKCB
                integer :: NPVK
                integer :: NPVANI
                integer :: NPSS
                integer :: NPSY
                integer :: NPHANI

              
              in=modflow.iLPF
              rewind(modflow.iLPF)
              iout=FNumEco
              inlak=modflow.iLAK
        !
        !     ------------------------------------------------------------------
        !1------Allocate scalar data.
              !ALLOCATE(ISFAC,ITHFLG)
              ZERO=0.
        !
        !2------IDENTIFY PACKAGE
              WRITE(IOUT,1) IN
            1 FORMAT(1X,/1X,'LPF -- LAYER-PROPERTY FLOW PACKAGE, VERSION 7',&
             ', 5/2/2005',/,9X,'INPUT READ FROM UNIT ',I4)
        !
        !3------READ COMMENTS AND ITEM 1.
        !csp commented out to be backward compatible with LPF package of MF2K5.
        !csp      IF(IFREFM.EQ.0) THEN
        !csp        IF(IUNSTR.NE.0)THEN
        !csp          READ(IN,2)IBCFCB,HDRY,NPLPF,IKCFLAG
        !csp        ELSE
        !csp          READ(IN,2)IBCFCB,HDRY,NPLPF
        !csp        ENDIF
        !csp      ELSE

                CALL URDCOM(IN,IOUT,LINE)
                LLOC=1
                CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,IBCFCB,R,IOUT,IN)
                CALL URWORD(LINE,LLOC,ISTART,ISTOP,3,I,HDRY,IOUT,IN)
                CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,NPLPF,R,IOUT,IN)
                IF(IUNSTR.NE.0)&
                 CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,IKCFLAG,R,IOUT,IN)
        !csp      ENDIF
        2     FORMAT(I10,F10.3,2I10)
        !
        !3A-----WRITE ITEM 1
              IF(IBCFCB.LT.0) WRITE(IOUT,8)
            8 FORMAT(1X,'CONSTANT-HEAD CELL-BY-CELL FLOWS WILL BE PRINTED',&
               ' WHEN ICBCFL IS NOT 0')
              IF(IBCFCB.GT.0) WRITE(IOUT,9) IBCFCB
            9 FORMAT(1X,'CELL-BY-CELL FLOWS WILL BE SAVED ON UNIT ',I4)
              WRITE(IOUT,11) HDRY
           11 FORMAT(1X,'HEAD AT CELLS THAT CONVERT TO DRY=',1PG13.5)
              IF(NPLPF.GT.0) THEN
                 WRITE(IOUT,15) NPLPF
           15    FORMAT(1X,I5,' Named Parameters     ')
              ELSE
                 NPLPF=0
                 WRITE(IOUT,'(A)') ' No named parameters'
              END IF
        !
              IF(IUNSTR.NE.0)THEN
                IF(IKCFLAG.EQ.0)WRITE(IOUT,39)
           39   FORMAT(1X,'IKCFLAG=0, NODAL INPUT OF HY AND CV')
                IF(IKCFLAG.EQ.1)WRITE(IOUT,41)
           41   FORMAT(1X,'IKCFLAG=1, CONNECTIVITY INPUT OF HY',1X,&
                     '(OR TRAN FOR CONFINED) AND CV')
                IF(IKCFLAG.EQ.-1)WRITE(IOUT,44)
           44   FORMAT(1X,'IKCFLAG=-1, CONNECTIVITY INPUT OF CONDUCTANCE')
              ENDIF
        !
        !3B-----GET OPTIONS.
              ISFAC=0
              ICONCV=0
              ITHFLG=0
              NOCVCO=0
              NOPCHK=0
              STOTXT=ANAME(6)
           20 CALL URWORD(LINE,LLOC,ISTART,ISTOP,1,I,R,IOUT,IN)
              IF(LINE(ISTART:ISTOP).EQ.'STORAGECOEFFICIENT') THEN
                 ISFAC=1
                 STOTXT=ANAME(9)
                 WRITE(IOUT,21)
           21    FORMAT(1X,'STORAGECOEFFICIENT OPTION:',/,&
                  1X,'Read storage coefficient rather than specific storage')
              ELSE IF(LINE(ISTART:ISTOP).EQ.'CONSTANTCV') THEN
                 ICONCV=1
                 WRITE(IOUT,23)
           23    FORMAT(1X,'CONSTANTCV OPTION:',/,1X,'Constant vertical',&
                      ' conductance for convertible layers')
              ELSE IF(LINE(ISTART:ISTOP).EQ.'THICKSTRT') THEN
                 ITHFLG=1
                 WRITE(IOUT,25)
           25    FORMAT(1X,'THICKSTRT OPTION:',/,1X,'Negative LAYTYP indicates',&
              ' confined layer with thickness computed from STRT-BOT')
              ELSE IF(LINE(ISTART:ISTOP).EQ.'NOCVCORRECTION') THEN
                 NOCVCO=1
                 WRITE(IOUT,27)
           27    FORMAT(1X,'NOCVCORRECTION OPTION:',/,1X,&
                 'Do not adjust vertical conductance when applying',&
                           ' the vertical flow correction')
              ELSE IF(LINE(ISTART:ISTOP).EQ.'NOVFC') THEN
                 NOVFC=1
                 IWADI = 0
                 NOCVCO=1
                 WRITE(IOUT,29)
           29    FORMAT(1X,'NOVFC OPTION:',/,1X,&
                 'Do not apply the vertical flow correction')
              ELSE IF(LINE(ISTART:ISTOP).EQ.'NOPARCHECK') THEN
                 NOPCHK=1
                 WRITE(IOUT,30)
           30    FORMAT(1X,'NOPARCHECK  OPTION:',/,1X,&
                 'For data defined by parameters, do not check to see if ',&
                     'parameters define data at all cells')
              ELSE IF(LINE(ISTART:ISTOP).EQ.'BUBBLEPT') THEN
                 IBPN=1
                 WRITE(IOUT,31)
           31    FORMAT(1X,'BUBBLEPT  OPTION:',/,1X,&
                 'For Richards equation the bubble point head is also ',&
                     'input at all cells')
                 allocate(bp(nodes))
                 DO N=1,NODES
                   BP(NODES) = 0.0
                 ENDDO
              ELSE IF(LINE(ISTART:ISTOP).EQ.'FULLYDRY') THEN
                 IDRY=1
                 WRITE(IOUT,32)
           32    FORMAT(1X,'FULLYDRY  OPTION:',/,1X,&
                 'For Richards equation, residual saturation is only applied ',&
                 'to relative permeability and soil saturation can vary from ',&
                 '0 to 1')
               ELSE IF(LINE(ISTART:ISTOP).EQ.'TABRICH') THEN
                 ITABRICH=1
                 !ALLOCATE(NUZONES,NUTABROWS)
                 WRITE(IOUT,33)
           33    FORMAT(1X,'TABULAR INPUT OPTION:',/,1X,&
                 'For Richards equation the retention ',&
                 'and relative permeability curves are provided ',&
                 'as tabular input for different soil types')
                 CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,NUZONES,R,IOUT,IN)
                 WRITE(IOUT,36) NUZONES
        36       FORMAT(1X,'NUMBER OF SOIL TYPE ZONES = ', I10)
                 CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,NUTABROWS,R,IOUT,IN)
                 WRITE(IOUT,37) NUTABROWS
        37       FORMAT(1X,'NUMBER OF ROWS OF TABULAR INPUT = ', I10)
              ELSE IF(LINE(ISTART:ISTOP).EQ.'INTRICH') THEN
                 INTRICH=1
                 WRITE(IOUT,34)
           34    FORMAT(1X,'INTEGRATED CURVE OPTION:',/,1X,&
                 'For Richards equation with tabular input, the retention ',&
                 'and relative permeability curves are integrated over the ',&
                 'grid block thickness')
              ELSE IF(LINE(ISTART:ISTOP).EQ.'ALTSTO') THEN
                 IALTSTO=1
                 WRITE(IOUT,35)
           35    FORMAT(1X,'ALTERNATIVE STORAGE FORMULATION (IALTSTO = 1)')
              END IF
        !
              IF(LLOC.LT.200) GO TO 20
        !
        !-------READ STUFF TABULAR INPUT OF RICHARDS EQUATION
              IF(ITABRICH.NE.0) THEN
        !-------DIMENSION AND READ ZONE MAP
                ALLOCATE(IUZONTAB(NODES))
                CALL U1DINT(IUZONTAB,ANAME(10),NODES,0,IN,IOUT)
        !-------DIMENSION AND READ TABLES
                ALLOCATE(RETCRVS(3,NUTABROWS,NUZONES))
        !
                DO IZON = 1,NUZONES
                  WRITE(IOUT,65) IZON
        65        FORMAT(/10X,'RETENTION TABLE NUMBER',I5/10X,27('-')/&
               10X,'CAPILLARY HEAD',7X,'SATURATION',3X,'RELATIVE PERMEABILITY')
                  DO ITROWS = 1,NUTABROWS
                    READ (IN,*) (RETCRVS(I,ITROWS,IZON),I=1,3)
                    WRITE(IOUT,66)(RETCRVS(I,ITROWS,IZON),I=1,3)
        66          FORMAT(10X,E14.6,3X,E14.6,7X,E14.6)
                  ENDDO
                ENDDO
              ENDIF
        !
        !4------ALLOCATE AND READ LAYTYP, LAYAVG, CHANI, LAYVKA, LAYWET, LAYSTRT.
              ALLOCATE(LAYTYP(NLAY))
              ALLOCATE(LAYAVG(NLAY))
              ALLOCATE(CHANI(NLAY))
              ALLOCATE(LAYVKA(NLAY))
              ALLOCATE(LAYSTRT(NLAY))
              ALLOCATE(LAYWET(NLAY))
              ALLOCATE(LAYCON(NLAY))
              ALLOCATE(LAYHDT(NLAY))
              ALLOCATE(LAYHDS(NLAY))
              ALLOCATE(LAYAVGV(NLAY))
              READ(IN,*) (LAYTYP(K),K=1,NLAY)
              READ(IN,*) (LAYAVG(K),K=1,NLAY)
              READ(IN,*) (CHANI(K),K=1,NLAY)
              READ(IN,*) (LAYVKA(K),K=1,NLAY)
              READ(IN,*) (LAYWET(K),K=1,NLAY)
              DO K=1,NLAY
                  LAYAVGV(K) = 0  ! FINITE DIFFERENCE HARMONIC AVERAGING IN VERTICAL DIRECTION DEFAULT
              ENDDO

        !
        !4A-----PRINT A TABLE OF VALUES FOR LAYTYP, LAYAVG, CHANI, LAYVKA, LAYWET.
              WRITE(IOUT,47)
           47 FORMAT(1X,/3X,'LAYER FLAGS:',/1X,&
              'LAYER       LAYTYP        LAYAVG         CHANI ',&
              '       LAYVKA        LAYWET',/1X,75('-'))
              DO 50 K=1,NLAY
              WRITE(IOUT,48) K,LAYTYP(K),LAYAVG(K),CHANI(K),LAYVKA(K),LAYWET(K)
           48 FORMAT(1X,I4,2I14,1PE14.3,2I14)
        !-----------------------------------------------------------------------------------
        !4A2 ---ALSO CONVERT LAVAVGV TO 1 (WEIGHTED ARITH AVERAGING FOR LPF)
        !4A2----WHEN LAYAVG IS 4 FOR FINITE ELEMENT AVERAGING
              IF(LAYAVG(K).EQ.4)THEN
                LAYAVGV(K) = 1
              ENDIF
        !
        !4B-----SET OPTIONS FOR BCF
              IF(LAYTYP(K).EQ.0)THEN
                LAYCON(K) = 0
              ELSEIF(LAYTYP(K).GT.0.AND.LAYTYP(K).LT.4)THEN
                LAYCON(K) = 3
              ELSEIF(LAYTYP(K).EQ.4)THEN
                LAYCON(K) = 4
              ELSEIF(LAYTYP(K).EQ.5)THEN
                LAYCON(K) = 5
              ELSEIF(LAYTYP(K).LT.0)THEN
                IF(ITHFLG.EQ.1)THEN
                  LAYCON(K) = 0
                ELSE
                  LAYCON(K) = 3
                ENDIF
              ENDIF
              IDEALLOC_LPF = 1
              IF(INLAK.GT.0) IDEALLOC_LPF = 2  !DEALLOCATE ONLY AFTER LAK7U1RP IS DONE
              IF(ICONCV.EQ.0) IDEALLOC_LPF = 0 ! NEED LPF ARRAYS FOR VARIABLE CV OPTION
              IF(CHANI(1).LE.0) IDEALLOC_LPF = 0 ! NEED LPF ARRAYS FOR VARIABLE ANISOTROPY
        !
        !4C-----SET GLOBAL HEAD-DEPENDENT TRANSMISSIVITY AND STORAGE FLAGS.
              IF (LAYTYP(K).NE.0) THEN
                LAYHDT(K)=1
                LAYHDS(K)=1
              ELSE
                LAYHDT(K)=0
                LAYHDS(K)=0
              ENDIF
           50 CONTINUE

        !
        !4D-----SET LAYSTRT AND RESET LAYTYP IF THICKSTRT OPTION IS ACTIVE.
              DO 60 K=1,NLAY
              LAYSTRT(K)=0
              IF(LAYTYP(K).LT.0 .AND. ITHFLG.NE.0) THEN
                 LAYSTRT(K)=1
                 LAYTYP(K)=0
                 LAYHDT(K)=0
                 LAYHDS(K)=0
                 WRITE(IOUT,57) K
           57    FORMAT(1X,'Layer',I5,&
               ' is confined because LAYTYP<0 and THICKSTRT option is active')
              END IF
           60 CONTINUE
        !
        !4E-----SET HORIZONTAL ANISOTROPY FLAG
              IHANISO = 0
              DO K=1,NLAY
                IF(ABS(CHANI(K) - 1.0).GT.1.0E-6) IHANISO = 1
              ENDDO
              IF(IHANISO.EQ.1) THEN
                !ALLOCATE(IOUTNORMAL)
                IOUTNORMAL = 0
                ALLOCATE(ARAD(NJAS))
              ENDIF
        !
        !4F-----BASED ON LAYTYP, LAYAVG, CHANI, LAYWET, COUNT THE NUMBER OF EACH
        !4F-----TYPE OF 2-D ARRAY; CHECK VALUES FOR CONSISTENCY; AND SETUP
        !4F-----POINTERS IN LAYTYP, CHANI, AND LAYWET FOR CONVENIENT ACCESS
        !4F-----TO SC2, HANI, and WETDRY.  PRINT INTERPRETED VALUES OF FLAGS.
              NCNVRT=0
              NHANI=0
              NWETD=0
              WRITE(IOUT,67)
           67 FORMAT(1X,/3X,'INTERPRETATION OF LAYER FLAGS:',/1X,&
               '                       INTERBLOCK     HORIZONTAL',&
               '    DATA IN',/1X,&
               '        LAYER TYPE   TRANSMISSIVITY   ANISOTROPY',&
               '   ARRAY VKA   WETTABILITY',/1X,&
               'LAYER      (LAYTYP)      (LAYAVG)       (CHANI)',&
               '      (LAYVKA)      (LAYWET)',/1X,75('-'))
              ILAYUNSAT = 0
              DO 100 K=1,NLAY
        !
              IF(LAYTYP(K).EQ.5) ILAYUNSAT = 1 ! AT LEAST 1 LAYER USES RICHARDS EQUATION
        !
              IF(LAYTYP(K).NE.0) THEN
                 NCNVRT=NCNVRT+1
                 LAYTYP(K)=NCNVRT
              END IF
              IF(CHANI(K).LE.ZERO) THEN
                 NHANI=NHANI+1
                 CHANI(K)=-NHANI
              END IF
              IF(LAYWET(K).NE.0) THEN
                 IF(LAYTYP(K).EQ.0) THEN
                    WRITE(IOUT,*)&
                       ' LAYWET is not 0 and LAYTYP is 0 for layer:',K
                    WRITE(IOUT,*) ' LAYWET must be 0 if LAYTYP is 0'
                    CALL USTOP(' ')
                 ELSE
                    NWETD=NWETD+1
                    LAYWET(K)=NWETD
                 END IF
              END IF
              IF(LAYAVG(K).LT.0 .OR. LAYAVG(K).GT.4) THEN
                 WRITE(IOUT,74) LAYAVG(K)
           74    FORMAT(1X,I8,&
                 ' IS AN INVALID LAYAVG VALUE -- MUST BE 0, 1, 2, 3 or 4')
                 CALL USTOP(' ')
              END IF
              LAYPRN(1)=TYPNAM(1)
              IF(LAYTYP(K).NE.0) LAYPRN(1)=TYPNAM(2)
              IF(LAYCON(K).EQ.4.OR.LAYCON(K).EQ.5) LAYPRN(1)=TYPNAM(3)
              LAYPRN(2)=AVGNAM(LAYAVG(K)+1)
              IF(LAYAVGV(K).NE.0) LAYPRN(2) = AVGNAM(5)
              IF(CHANI(K).LE.0) THEN
                 LAYPRN(3)=HANNAM
              ELSE
                 WRITE(LAYPRN(3),'(1PE14.3)') CHANI(K)
              END IF
              LAYPRN(4)=VKANAM(1)
              IF(LAYVKA(K).NE.0) LAYPRN(4)=VKANAM(2)
              LAYPRN(5)=WETNAM(1)
              IF(LAYWET(K).NE.0) LAYPRN(5)=WETNAM(2)
              WRITE(IOUT,78) K,(LAYPRN(I),I=1,5)
           78 FORMAT(1X,I4,5A)
          100 CONTINUE
        !
        !4G-----PRINT WETTING INFORMATION.
              IF(NWETD.EQ.0) THEN
                 WRITE(IOUT,13)
           13    FORMAT(1X,/,1X,'WETTING CAPABILITY IS NOT ACTIVE IN ANY LAYER')
                 IWDFLG=0
              ELSE
                 WRITE(IOUT,12) NWETD
           12    FORMAT(1X,/,1X,'WETTING CAPABILITY IS ACTIVE IN',I4,' LAYERS')
                 IWDFLG=1
                 READ(IN,*) WETFCT,IWETIT,IHDWET
                 IF(IWETIT.LE.0) IWETIT=1
                 WRITE(IOUT,*) ' WETTING FACTOR=',WETFCT
                 WRITE(IOUT,*) ' WETTING ITERATION INTERVAL=',IWETIT
                 WRITE(IOUT,*) ' IHDWET=',IHDWET
              END IF
        !
        !5------ALLOCATE MEMORY FOR ARRAYS.
              ALLOCATE(CV(NODES))
        !
              ALLOCATE(LAYFLG(6,NLAY))
              ALLOCATE(HK(NODES))
              ALLOCATE(VKA(NODES))
              IF(NCNFBD.GT.0) THEN
                 ALLOCATE(VKCB(NODES))
              ELSE
                 ALLOCATE(VKCB(1))
              END IF
              IF(ITRSS.NE.0) THEN
                 ALLOCATE(SC1(NODES))
              ELSE
                 ALLOCATE(SC1(1))
              END IF
              IF(ITRSS.NE.0 .AND. NCNVRT.GT.0 .or. itrnsp.ne.0) THEN
                 ALLOCATE(SC2(NODES))
                 SC2 = 0.0
              ELSE
                 ALLOCATE(SC2(1))
              END IF
              IF(NHANI.GT.0) THEN
                 ALLOCATE(HANI(NODES))
              ELSE
                 ALLOCATE(HANI(1))
              END IF
              IF(NWETD.GT.0) THEN
                 ALLOCATE(WETDRY(NODES))
              ELSE
                 ALLOCATE(WETDRY(1))
              END IF
        !
              if(iunsat.eq.1.OR.ILAYUNSAT.EQ.1)then
                if(itabrich.eq.0)then
                  ALLOCATE(alpha(NODES),beta(nodes),sr(nodes),brook(nodes))
                  DO N=1,NODES
                    ALPHA(N) =0.0
                    BETA(N) = 1.0
                    SR(N) = 0.999
                    BROOK(N) = 1.0
                  ENDDO
                endif
                IF(IUNSAT.EQ.1)THEN
                  DO K=1,NLAY
                    IF(LAYCON(K).NE.0) LAYCON(K)=5
                  ENDDO
                ENDIF
              ENDIF
        !
        !6------READ PARAMETER DEFINITIONS
              NPHK=0
              NPVKCB=0
              NPVK=0
              NPVANI=0
              NPSS=0
              NPSY=0
              NPHANI=0
              IF(NPLPF.GT.0) THEN
                 WRITE(IOUT,115)
          115    FORMAT(/,' PARAMETERS DEFINED IN THE LPF PACKAGE')
                 DO 120 K=1,NPLPF
                 CALL UPARARRRP(IN,IOUT,N,1,PTYP,1,0,-1)
        !   Note that NPHK and the other NP variables in
        !   this group are used only as flags, not counts
                 IF(PTYP.EQ.'HK') THEN
                    NPHK=1
                 ELSE IF(PTYP.EQ.'HANI') THEN
        !6A-----WHEN A HANI PARAMETER IS USED, THEN ALL HORIZONTAL ANISOTROPY
        !6A-----MUST BE DEFINED USING PARAMETERS.  ENSURE THAT ALL CHANI <= 0
                    DO 118 I = 1, NLAY
                      IF (CHANI(I).GT.0.0) THEN
                        WRITE(IOUT,117)
          117           FORMAT(/,&
             ' ERROR: WHEN A HANI PARAMETER IS USED, CHANI FOR ALL LAYERS',/,&
             ' MUST BE LESS THAN OR EQUAL TO 0.0 -- STOP EXECUTION',&
             ' (GWF2LPFU1AR)')
                        CALL USTOP(' ')
                      ENDIF
          118       CONTINUE
                    NPHANI=1
                 ELSE IF(PTYP.EQ.'VKCB') THEN
                    NPVKCB=1
                 ELSE IF(PTYP.EQ.'VK') THEN
                    NPVK=1
                    CALL SGWF2LPFU1CK(IOUT,N,'VK  ')
                 ELSE IF(PTYP.EQ.'VANI') THEN
                    NPVANI=1
                    CALL SGWF2LPFU1CK(IOUT,N,'VANI')
                 ELSE IF(PTYP.EQ.'SS') THEN
                    NPSS=1
                 ELSE IF(PTYP.EQ.'SY') THEN
                    NPSY=1
                 ELSE
                    WRITE(IOUT,*) ' Invalid parameter type for LPF Package'
                    CALL USTOP(' ')
                 END IF
          120    CONTINUE
              END IF
        !
        !7------READ PARAMETERS AND CONVERT FOR UNSTRUCTURED AND STRUCTURED GRIDS
              IF(IUNSTR.EQ.0) THEN
                CALL SGWF2LPFU1S(IN,NPHK,NPHANI,NPVK,NPVANI,NPSS,NPSY,NPVKCB,&
                 STOTXT,NOPCHK)
              ELSE
                CALL SGWF2LPFU1G(IN,NPHK,NPHANI,NPVK,NPVANI,NPSS,NPSY,NPVKCB,&
                 STOTXT,NOPCHK)
              ENDIF
        
!        !--------------------------------------------------------------------------------
!        !8------SET INITIAL  GRID-BLOCK SATURATED THICKNESS FRACTIONS AND TRANSMISSIVITY WHEN NEEDED
!              DO K=1,NLAY
!                IF(LAYCON(K).EQ.4.OR.LAYCON(K).EQ.5) THEN
!        !8A-------SET INITIAL SATURATED GRID-BLOCK FRACTIONS FOR LAYCON=4
!                  NNDLAY = NODLAY(K)
!                  NSTRT = NODLAY(K-1)+1
!                  DO N=NSTRT,NNDLAY
!                    IF(IBOUND(N).NE.0) THEN
!        !-------------CALCULATE SATURATED THICKNESS/TOTAL THICKNESS.
!                      HD=HNEW(N)
!                      BBOT=BOT(N)
!                      TTOP=TOP(N)
!                      TOTTHICK = TTOP - BBOT
!                      CALL SAT_THIK(N,HD,TOTTHICK,BBOT,THCK,K,TTOP)
!                      Sn(N)=THCK
!                      So(N) = Sn(N)
!                    ENDIF
!                  ENDDO
!                ENDIF
!              ENDDO
!        !--------------------------------------------------------------------
!        !9------SET CONSTANT TERMS IN PGF ARRAY IF IT IS NOT READ DIRECTLY
!              IF(IKCFLAG.EQ.0)THEN
!        !9A--------CHECK CV CONSISTENCY
!                CALL SGWF2LPFU1N
!        !
!        !10--------FILL PGF ARRAY
!        !
!        !10A--------FILL VERTICAL TERMS INTO PGF
!                IF(NLAY.GT.1) CALL SGWF2LPFU1VCOND
!        !
!        !10B------FILL HORIZONTAL TERMS INTO PGF - HY FOR LAYCON 4 AND T FOR LAYCONS 0 OR 2
!                CALL FILLPGFH
!              ENDIF
!        !
!        !-----------------------------------------------------------------------------------
!        !11------SET UP STORAGE CAPACITIES FROM COEFFICIENTS
!              IF(ITRSS.NE.0)THEN
!                IF(ISFAC.EQ.0) THEN
!                  CALL SGWF2LPFU1SC(SC1(1),1)
!                ELSE
!                  CALL SGWF2LPFU1SC(SC1(1),0)
!                END IF
!                IF(NCNVRT.GT.0) THEN
!                  CALL SGWF2LPFU1SC(SC2(1),0)
!                ENDIF
!              END IF
!        !
!        !--------------------------------------------------------------------------------
!        !12-----DEALLOCATE UNWANTED ARRAYS
!        !sp need cv for merging Kv with boundary leakance for RIV      DEALLOCATE(CV)
!        !------NEED HK FOR CONDUIT CELLS SO KEEP
!        !      ILAYCON13=0
!        !      DO I=1,NLAY
!        !        IF(LAYCON(I).EQ.1.OR.LAYCON(I).EQ.3)ILAYCON13=1
!        !      ENDDO
!        !      IF(ILAYCON13.EQ.0)THEN
!        !        DEALLOCATE(HK)
!        !      ENDIF
        !13-----RETURN
              RETURN
    END SUBROUTINE MUSG_ReadLPF
 
    SUBROUTINE MUSG_ReadCLN_IBOUND_IHEADS(Modflow)
!     ******************************************************************
!     READ IBOUND AND STARTING HEADS AND PREPARE KADI, Sn AND PGF ARRAYS FOR CLN
!     ******************************************************************
!
!        SPECIFICATIONS:
!     ------------------------------------------------------------------
      USE CLN1MODULE, ONLY: NCLNNDS
      USE GLOBAL, ONLY: IOUT,IBOUND,NODES,STRT,HNEW,INCLN,&
      IDPIN
      USE GWFBASMODULE, ONLY: HNOFLO
      
      implicit none

      type (MUSG_Project) Modflow

      REAL, DIMENSION(:),ALLOCATABLE  ::HTMP1
      REAL*8, DIMENSION(:),ALLOCATABLE  ::HTMP18
      CHARACTER*24 ANAME(2)
      DATA ANAME(1) /' CONDUIT BOUNDARY ARRAY'/
      DATA ANAME(2) /'   CONDUIT INITIAL HEAD'/
      
      integer :: n
      
      iout=FNumEco
      INCLN=Modflow.iCLN
!     ------------------------------------------------------------------
!
!1------IDENTIFY PACKAGE.
      WRITE(IOUT,1)INCLN
    1 FORMAT(1X,/1X,'CLN -- CONDUIT DOMAIN FLOW PACKAGE, VERSION 1,',&
       ' 5/17/2010 INPUT READ FROM UNIT ',I4)
!
!2-------READ IBOUND FOR CLN NODES
      CALL U1DINT(IBOUND(NODES+1),ANAME(1),NCLNNDS,0,INCLN,IOUT)
!3-------READ INITIAL HEADS FOR CLN NODES
      IF(IDPIN.EQ.0) THEN  !----------------------------------SINGLE PRECISION READ
      ALLOCATE(HTMP1(NCLNNDS))
      CALL U1DREL(HTMP1,ANAME(2),NCLNNDS,0,INCLN,IOUT)
      DO N=1,NCLNNDS
        HNEW(NODES+N) = HTMP1(N)
        STRT(NODES+N) = HTMP1(N)
        IF(IBOUND(NODES+N).EQ.0) HNEW(NODES+N)=HNOFLO
      ENDDO
      DEALLOCATE(HTMP1)
      ELSE    !----------------------------------DOUBLE PRECISION READ
      ALLOCATE(HTMP18(NCLNNDS))
      CALL U1DREL8(HTMP18,ANAME(2),NCLNNDS,0,INCLN,IOUT)
      DO N=1,NCLNNDS
        HNEW(NODES+N) = HTMP18(N)
        STRT(NODES+N) = HTMP18(N)
        IF(IBOUND(NODES+N).EQ.0) HNEW(NODES+N)=HNOFLO
      ENDDO
      DEALLOCATE(HTMP18)
      ENDIF
!!
!!4-----SET VOLUMETRIC FRACTIONS FOR CLN-NODES IN SATURATION ARRAY
!      DO  IFN=1,NCLNNDS
!        N = ACLNNDS(IFN,1)
!        IFLIN = IFLINCLN(IFN)
!        IF(IBOUND(N).NE.0.AND.IFLIN.LE.0) THEN
!!---------CALCULATE INITIAL SATURATED THICKNESS FOR UNCONFINED CASES.
!          HD=HNEW(N)
!          BBOT = ACLNNDS(IFN,5)
!          CALL CLN_THIK(IFN,HD,BBOT,THCK)
!          Sn(N)=THCK
!          So(N) = Sn(N)
!        ENDIF
!      ENDDO
!!--------------------------------------------------------------------------------
!!5-------FILL PGF ARRAY FOR CLN FLOW AND ITS CONNECTION WITH POROUS MATRIX
!      CALL SFILLPGF_CLN
!!----------------------------------------------------------------------------------------
!!12A------ESTABLISH WADI CONDITION FOR CLN
!        IWADICLN = 0
!        DO I = 1,NCLNNDS
!           IF(ICCWADICLN(I).NE.0) IWADICLN = 1
!        ENDDO
!        DO I = 1,NCLNGWC
!          IF(ICGWADICLN(I).NE.0) IWADICLN = 1
!        ENDDO
!        IF(IWADICLN.EQ.1) IWADI = 1
!
!6------RETURN
      RETURN
    END SUBROUTINE MUSG_ReadCLN_IBOUND_IHEADS

    SUBROUTINE MUSG_ReadSWF_IBOUND_IHEADS(Modflow)
!     ******************************************************************
!     READ IBOUND AND STARTING HEADS AND PREPARE KADI, Sn AND PGF ARRAYS FOR SWF
!     ******************************************************************
!
!        SPECIFICATIONS:
!     ------------------------------------------------------------------
!      USE CLN1MODULE, ONLY: NCLNNDS,NCLNGWC,ACLNNDS,IFLINCLN,
!     1    ICCWADICLN,ICGWADICLN
      USE SWF1MODULE
      USE CLN1MODULE, ONLY:NCLNNDS
      USE GLOBAL, ONLY: IOUT,IBOUND,NODES,STRT,HNEW,INSWF,&
         IDPIN,IUNSTR
      USE GWFBASMODULE, ONLY: HNOFLO

      implicit none

      type (MUSG_Project) Modflow

      REAL, DIMENSION(:),ALLOCATABLE  ::HTMP1
      REAL*8, DIMENSION(:),ALLOCATABLE  ::HTMP18
      CHARACTER*24 ANAME(2)
      DATA ANAME(1) /'     SWF BOUNDARY ARRAY'/
      DATA ANAME(2) /'       SWF INITIAL HEAD'/
      
      integer :: k, n
      
      iout=FNumEco
      INSWF=Modflow.iSWF
!     ------------------------------------------------------------------
!
!1------IDENTIFY PACKAGE.
      WRITE(IOUT,1) INSWF
    1 FORMAT(1X,/1X,'SWF -- SURFACE DOMAIN FLOW PACKAGE, VERSION 1,',&
       ' 5/17/2010 INPUT READ FROM UNIT ',I4)
!===
!-------FILL PGF ARRAY FOR CLN FLOW AND ITS CONNECTION WITH POROUS MATRIX
      ALLOCATE(CL12_SWF(NJA_SWF),FAHL_SWF(NJA_SWF))
      IF(INSWF.NE.0) THEN
        IF(IUNSTR.EQ.0) THEN
!          CALL FILLGFS_SWF(IOUT)
        ELSE
!          CALL FILLGFU_SWF(INSWF,IOUT)
          CALL U1DREL(CL12_SWF,ANAME(1),NJA_SWF,K,INSWF,IOUT)
        ENDIF
      ENDIF
      CALL U1DREL(FAHL_SWF,ANAME(1),NJA_SWF,K,INSWF,IOUT)
      !CALL SFILLPGF_SWF
!
!2-------READ IBOUND FOR CLN NODES
      CALL U1DINT(IBOUND(NODES+NCLNNDS+1),ANAME(1),NSWFNDS,0,INSWF,IOUT)
!3-------READ INITIAL HEADS FOR CLN NODES
      IF(IDPIN.EQ.0) THEN  !----------------------------------SINGLE PRECISION READ
          ALLOCATE(HTMP1(NSWFNDS))
          CALL U1DREL(HTMP1,ANAME(2),NSWFNDS,0,INSWF,IOUT)
          DO N=1,NSWFNDS
            HNEW(NODES+NCLNNDS+N) = HTMP1(N)
            STRT(NODES+NCLNNDS+N) = HTMP1(N)
            IF(IBOUND(NODES+NCLNNDS+N).EQ.0) HNEW(NODES+NCLNNDS+N)=HNOFLO
          ENDDO
          DEALLOCATE(HTMP1)
      ELSE    !----------------------------------DOUBLE PRECISION READ
          ALLOCATE(HTMP18(NSWFNDS))
          CALL U1DREL8(HTMP18,ANAME(2),NSWFNDS,0,INSWF,IOUT)
          DO N=1,NSWFNDS
            HNEW(NODES+NCLNNDS+N) = HTMP18(N)
            STRT(NODES+NCLNNDS+N) = HTMP18(N)
            IF(IBOUND(NODES+NCLNNDS+N).EQ.0) HNEW(NODES+NCLNNDS+N)=HNOFLO
          ENDDO
          DEALLOCATE(HTMP18)
      ENDIF
!!
!!4-----SET VOLUMETRIC FRACTIONS FOR CLN-NODES IN SATURATION ARRAY
!      DO  IFN=1,NSWFNDS
!        N = ASWFNDS(IFN,1)
!!        IFLIN = IFLINCLN(IFN)
!        IF(IBOUND(N).NE.0) THEN
!!---------CALCULATE INITIAL SATURATED THICKNESS FOR UNCONFINED CASES.
!          HD=HNEW(N)
!          BBOT = ASWFNDS(IFN,4)
!          CALL SWF_THIK(HD,BBOT,THCK)
!          Sn(N) = THCK
!          So(N) = Sn(N)
!        ENDIF
!      ENDDO
!!--------------------------------------------------------------------------------
!!12A------ESTABLISH WADI CONDITION FOR CLN
!        IWADISWF = 0
!        DO I = 1,NSWFNDS
!           IF(ISSWADISWF(I).NE.0) IWADISWF = 1
!        ENDDO
!        DO I = 1,NSWFGWC
!          IF(ISGWADISWF(I).NE.0) IWADISWF = 1
!        ENDDO
!        IF(IWADISWF.EQ.1) IWADI = 1
!!
!!6------RETURN
      RETURN
      END SUBROUTINE MUSG_ReadSWF_IBOUND_IHEADS 

    
      SUBROUTINE MUSG_ReadWEL(Modflow)
!     ******************************************************************
!     ALLOCATE ARRAY STORAGE FOR WELL PACKAGE
!     ******************************************************************
!
!        SPECIFICATIONS:
!     ------------------------------------------------------------------
      USE GLOBAL,      ONLY:IOUT,NCOL,NROW,NLAY,IFREFM,IUNSTR,NEQS
      USE GWFWELMODULE, ONLY:NWELLS,MXWELL,NWELVL,IWELCB,IPRWEL,NPWEL,&
                            IWELPB,NNPWEL,WELAUX,WELL,IWELQV,NNPWCLN,&
                            IAFR,IWELLBOT,WELLBOT,NAUXWEL
      implicit none
      
      type (MUSG_Project) Modflow
!
      CHARACTER*400 LINE
      
      integer :: in, mxpw, mxactw, lloc
      real :: r
      integer :: istart, istop, n, lstsum, k, lstbeg, ip, numinst, nlst
      integer :: ninlst, i
      
        in=modflow.iWEL
        iout=FNumEco

!     ------------------------------------------------------------------
      ALLOCATE(NWELLS,MXWELL,NWELVL,IWELCB,IPRWEL,IAFR,NAUXWEL)
      ALLOCATE(NPWEL,IWELPB,NNPWEL,IWELQV,NNPWCLN,IWELLBOT)
!
!1------IDENTIFY PACKAGE AND INITIALIZE NWELLS.
      WRITE(IOUT,1)IN
    1 FORMAT(1X,/1X,'WEL -- WELL PACKAGE, VERSION 7, 5/2/2005',&
     ' INPUT READ FROM UNIT ',I4)
      NWELLS=0
      NNPWEL=0
      NNPWCLN=0
      IWELQV=0
      IAFR=0
      IWELLBOT = 0
!
!2------READ MAXIMUM NUMBER OF WELLS AND UNIT OR FLAG FOR
!2------CELL-BY-CELL FLOW TERMS.
      CALL URDCOM(IN,IOUT,LINE)
      CALL UPARLSTAL(IN,IOUT,LINE,NPWEL,MXPW)
      IF(IFREFM.EQ.0) THEN
         READ(LINE,'(3I10)') MXACTW,IWELCB
         LLOC=21
      ELSE
         LLOC=1
         CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,MXACTW,R,IOUT,IN)
         CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,IWELCB,R,IOUT,IN)
      END IF
      WRITE(IOUT,3) MXACTW
    3 FORMAT(1X,'MAXIMUM OF ',I6,' ACTIVE WELLS AT ONE TIME')
      IF(IWELCB.LT.0) WRITE(IOUT,7)
    7 FORMAT(1X,'CELL-BY-CELL FLOWS WILL BE PRINTED WHEN ICBCFL NOT 0')
      IF(IWELCB.GT.0) WRITE(IOUT,8) IWELCB
    8 FORMAT(1X,'CELL-BY-CELL FLOWS WILL BE SAVED ON UNIT ',I4)
      WRITE(IOUT,9) MXACTW,IWELCB
    9 FORMAT(1X,'MAXIMUM NUMBER OF ACTIVE WELLS (MXACTW) =',I7&
       /1X,'C-B-C FLUX FLAG OR UNIT NUMBER (IWELCB) =',I4)
!
!3------READ AUXILIARY VARIABLES AND PRINT FLAG.
      ALLOCATE(WELAUX(20))
      NAUXWEL=0
      IPRWEL=1
   10 CALL URWORD(LINE,LLOC,ISTART,ISTOP,1,N,R,IOUT,IN)
      IF(LINE(ISTART:ISTOP).EQ.'AUXILIARY' .OR.&
             LINE(ISTART:ISTOP).EQ.'AUX') THEN
         CALL URWORD(LINE,LLOC,ISTART,ISTOP,1,N,R,IOUT,IN)
         IF(NAUXWEL.LT.20) THEN
            NAUXWEL=NAUXWEL+1
            WELAUX(NAUXWEL)=LINE(ISTART:ISTOP)
            WRITE(IOUT,12) WELAUX(NAUXWEL)
   12       FORMAT(1X,'AUXILIARY WELL VARIABLE: ',A)
         END IF
         GO TO 10
      ELSE IF(LINE(ISTART:ISTOP).EQ.'AUTOFLOWREDUCE') THEN
         WRITE(IOUT,16)
   16    FORMAT(1X,'WELL FLUX WILL BE REDUCED WHEN SATURATED ',&
            'THICKNESS IS LESS THAN 1 PERCENT OF CELL THICKNESS')
         IWELQV = 1
         GO TO 10
      ELSE IF(LINE(ISTART:ISTOP).EQ.'IUNITAFR') THEN
         CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,IAFR,R,IOUT,IN)
         WRITE(IOUT,25) IAFR
   25    FORMAT(1X,'WELL REDUCTION INFO WILL BE WRITTEN TO UNIT: ',&
            I5)
         GO TO 10
      ELSE IF(LINE(ISTART:ISTOP).EQ.'NOPRINT') THEN
         WRITE(IOUT,13)
   13    FORMAT(1X,'LISTS OF WELL CELLS WILL NOT BE PRINTED')
         IPRWEL = 0
         GO TO 10
      ELSE IF(LINE(ISTART:ISTOP).EQ.'WELLBOT') THEN
         IWELLBOT = 1
         WRITE(IOUT,27)
   27    FORMAT(1X,'BOTTOM ELEVATIONS OF WELLS ARE READ',&
            1X,'(WELLBOT OPTION)')
         GO TO 10
      END IF
!3A-----THERE ARE FIVE INPUT VALUES PLUS ONE LOCATION FOR
!3A-----CELL-BY-CELL FLOW BESIDES AUX VARIABLES.
      IF(IWELLBOT.EQ.0) THEN
        NWELVL=5+NAUXWEL
      ELSE
        NWELVL=6+NAUXWEL
      ENDIF
!
!4------ALLOCATE SPACE FOR THE WELL DATA.
      IWELPB=MXACTW+1
      MXWELL=MXACTW+MXPW
      ALLOCATE (WELL(NWELVL,MXWELL))
      ALLOCATE (WELLBOT(2,MXWELL))  ! 1 IS FOR WELL BOTTOM; 2 IS FOR 0.01 * THICKNESS FROM THIS BOTTOM
!
!5------READ NAMED PARAMETERS.
      WRITE(IOUT,18) NPWEL
   18 FORMAT(1X,//1X,I5,' Well parameters')
      IF(NPWEL.GT.0) THEN
        LSTSUM=IWELPB
        DO 120 K=1,NPWEL
          LSTBEG=LSTSUM
          CALL UPARLSTRP(LSTSUM,MXWELL,IN,IOUT,IP,'WEL','Q',1,&
                        NUMINST)
          NLST=LSTSUM-LSTBEG
          IF(NUMINST.EQ.0) THEN
!5A-----READ PARAMETER WITHOUT INSTANCES.
            IF(IUNSTR.EQ.0)THEN
              IF(IWELLBOT.EQ.0)THEN
                CALL ULSTRD(NLST,WELL,LSTBEG,NWELVL,MXWELL,1,IN,&
               IOUT,'WELL NO.  LAYER   ROW   COL   STRESS FACTOR',&
               WELAUX,20,NAUXWEL,IFREFM,NCOL,NROW,NLAY,4,4,IPRWEL)
              ELSE
                CALL ULSTRD(NLST,WELL,LSTBEG,NWELVL,MXWELL,1,IN,&
               IOUT,'WELL NO. LAYER  ROW   COL  STRESS FACTOR  WELBOT',&
               WELAUX,20,NAUXWEL,IFREFM,NCOL,NROW,NLAY,5,5,IPRWEL)
              ENDIF
            ELSE
              IF(IWELLBOT.EQ.0)THEN
               CALL ULSTRDU(NLST,WELL,LSTBEG,NWELVL,MXWELL,1,IN,&
              IOUT,'WELL NO.      NODE       STRESS FACTOR',&
              WELAUX,20,NAUXWEL,IFREFM,NEQS,4,4,IPRWEL)
              ELSE
               CALL ULSTRDU(NLST,WELL,LSTBEG,NWELVL,MXWELL,1,IN,&
              IOUT,'WELL NO. LAYER  ROW   COL  STRESS FACTOR  WELBOT',&
              WELAUX,20,NAUXWEL,IFREFM,NEQS,5,5,IPRWEL)
              ENDIF
            ENDIF
          ELSE
!5B-----READ INSTANCES.
            NINLST=NLST/NUMINST
            DO 110 I=1,NUMINST
            CALL UINSRP(I,IN,IOUT,IP,IPRWEL)
            IF(IUNSTR.EQ.0)THEN
              IF(IWELLBOT.EQ.0)THEN
                CALL ULSTRD(NINLST,WELL,LSTBEG,NWELVL,MXWELL,1,IN,&
               IOUT,'WELL NO.  LAYER   ROW   COL   STRESS FACTOR',&
               WELAUX,20,NAUXWEL,IFREFM,NCOL,NROW,NLAY,4,4,IPRWEL)
              ELSE
                CALL ULSTRD(NINLST,WELL,LSTBEG,NWELVL,MXWELL,1,IN,&
               IOUT,'WELL NO. LAYER  ROW   COL  STRESS FACTOR  WELBOT',&
               WELAUX,20,NAUXWEL,IFREFM,NCOL,NROW,NLAY,5,5,IPRWEL)
              ENDIF
            ELSE
              IF(IWELLBOT.EQ.0)THEN
                CALL ULSTRDU(NINLST,WELL,LSTBEG,NWELVL,MXWELL,1,IN,&
               IOUT,'WELL NO.      NODE       STRESS FACTOR',&
               WELAUX,20,NAUXWEL,IFREFM,NEQS,4,4,IPRWEL)
              ELSE
                CALL ULSTRDU(NINLST,WELL,LSTBEG,NWELVL,MXWELL,1,IN,&
               IOUT,'WELL NO. LAYER  ROW   COL  STRESS FACTOR  WELBOT',&
               WELAUX,20,NAUXWEL,IFREFM,NEQS,5,5,IPRWEL)
              ENDIF
            ENDIF
            LSTBEG=LSTBEG+NINLST
  110       CONTINUE
          END IF
  120   CONTINUE
      END IF
!
!6------RETURN
      RETURN
      END SUBROUTINE MUSG_ReadWEL

      SUBROUTINE SWF_THIK(HD,BBOT,THCK)
!     ******************************************************************
!     COMPUTE FRACTION OF TOTAL VOLUME THAT IS SATURATED
!     FOR CONDUIT NODE AND STORE IN THCK -
!     FRACTION SATURATED DEPENDS ON CONDUIT ORIENTATION
!     ******************************************************************
!
!      SPECIFICATIONS:
!     ------------------------------------------------------------------
      USE SWF1MODULE
      DOUBLE PRECISION THCK,HD,BBOT,&
       DEPTH,SATEPS,D0S0
      
      integer :: ioption
      real :: epsilon
!     ------------------------------------------------------------------
      EPSILON = 0.1D0
      SATEPS = 0.9D0*EPSILON
      D0S0=EPSILON/SATEPS
!
      DEPTH = HD-BBOT
      DEPTH = MAX(MIN_DEPTH,DEPTH)
      ioption=1
      if(ioption==1) then
!    1: Polynomial option
      IF(DEPTH>=EPSILON) THEN
          THCK = SATEPS+(DEPTH-EPSILON)
      ELSEIF(DEPTH<=MIN_DEPTH) THEN
          THCK = 0.0D0
      ELSE
          THCK = (DEPTH/EPSILON)**2.0D0**((3.0D0*SATEPS-EPSILON) &
               +(EPSILON-2.0D0*SATEPS)*DEPTH/EPSILON)
      ENDIF
      elseif(ioption==2) then
!    2: Simple option       (S0 is fixed to be d0/2)
      IF(DEPTH>=EPSILON) THEN
          THCK = (DEPTH-0.5d0*EPSILON)
      ELSEIF(DEPTH<=MIN_DEPTH) THEN
          THCK = 0.0D0
      ELSE
          THCK = 1.0d0/2.0d0/EPSILON*DEPTH**2.0D0
      ENDIF
      elseif(ioption==3) then
!    3: Power option
      IF(DEPTH>=EPSILON) THEN
          THCK = SATEPS+(DEPTH-EPSILON)
      ELSEIF(DEPTH<=MIN_DEPTH) THEN
          THCK = 0.0D0
      ELSE
          THCK = SATEPS/(EPSILON**D0S0)*(DEPTH**D0S0)
      ENDIF
      else
      continue
      endif

      RETURN
!      IF(DEPTH>=EPSILON) THEN
!        THCK = 1.0D0*DEPTH
!      ELSEIF(DEPTH<=0.0D0) THEN
!        THCK = 0.0D0*DEPTH
!      ELSE
!       THCK = (-2.0D0*(DEPTH-1.0D-6)**3.0D0 +
!     1         3.0D0*(DEPTH-1.0D-6)**2.0D0)*DEPTH
!      ENDIF
!      THCK=1.0D0
!      KK = 0
!C1------GET DIRECTION OF LINE SEGMENT
!      IFDIR = ACLNNDS(ICLN,3)
!      I = ACLNNDS(ICLN,1)
!C--------------------------------------------------------
!      IF(IFDIR.EQ.0)THEN
!C2-------VERTICAL LINE SEGMENT
!        TOTTHICK = ACLNNDS(ICLN,4)
!        TTOP = BBOT + TOTTHICK
!        CALL SAT_THIK(I,HD,TOTTHICK,BBOT,THCK,KK,TTOP)
!      ELSEIF(IFDIR.EQ.1)THEN
!C3-------HORIZONTAL LINE SEGMENT CONDUIT
!        IC = ACLNNDS(ICLN,2)
!        CALL CLNAGET(IC,AREAF)
!        CALL CLNAW(ICLN,HD,AREAW)
!        THCK = AREAW / AREAF
!      ELSEIF(IFDIR.EQ.2)THEN
!C4-------ANGLED CONDUIT
!        FANGLE = ACLNNDS(ICLN,6)
!        TOTTHICK = ACLNNDS(ICLN,4) * SIN(FANGLE)
!        TTOP = BBOT + TOTTHICK
!        BBOT = ACLNNDS(ICLN,5)
!        I = ACLNNDS(ICLN,1)
!        CALL SAT_THIK(I,HD,TOTTHICK,BBOT,THCK,KK,TTOP)
!      ENDIF
!csp      IF(THCK.LT.1.0E-7) THCK = 1.0E-7
!C
!C5------RETURN.
      RETURN
      END SUBROUTINE SWF_THIK
 
!      SUBROUTINE SFILLPGF_SWF
!!     ******************************************************************
!!     COMPUTE AND FILL CONSTANT TERMS INTO PGF ARRAY FOR CONDUIT DOMAIN FLOW
!!     ALSO FILL AREA IN FAHL, FILL CL1, AND CL2 FOR CLN NODES
!!     AND SET IVC = 5 OR 6 FOR SWF-CONDUIT OR SWF-MATRIX CONNECTIONS
!!     ******************************************************************
!      USE GLOBAL, ONLY:IA,PGF,FAHL,CL1,CL2,&
!                 JA,JAS,IVC,AREA
!      USE GWFBCFMODULE,ONLY:VKA
!      USE SWF1MODULE
!      
!      integer :: nc1, ii_swf, nc2, kk_swf, iis_swf, is1, is2, ii, iis
!      real :: rou1, rou2, roughness
!      integer :: ifn, ih, nh, nl, jj, ifnc, n
!!
!!--------------------------------------------------------------------------------------
!!1-----CONNECT CLN NODES TO EACH OTHER
!!--------------------------------------------------------------------------------------
!!1A---------loop over all SWF nodes
!      DO NC1 = 1,NSWFNDS
!!2----------loop over all connections of node NC1
!        DO II_SWF = IA_SWF(NC1)+1,IA_SWF(NC1+1)-1
!          NC2 = JA_SWF(II_SWF)
!          IF(NC2.GT.NC1) CYCLE
!! IIS_SWF
!          DO KK_SWF=IA_SWF(NC2),IA_SWF(NC2+1)-1
!              IF(JA_SWF(KK_SWF).EQ.NC1) THEN
!                  IIS_SWF = KK_SWF
!              ENDIF
!          ENDDO
!          GEOM = FAHL_SWF(II_SWF)/(CL12_SWF(II_SWF)+CL12_SWF(IIS_SWF))
!          IS1 = ASWFNDS(NC1,2)     !SWF TYPE FOR NODE 1
!          IS2 = ASWFNDS(NC2,2)     !SWF TYPE FOR NODE 2
!          II = IDXGLO_SWF(II_SWF)
!          IIS = JAS(II)
!!
!          CL1(IIS)=CL12_SWF(II_SWF)
!          CL2(IIS)=CL12_SWF(IIS_SWF)
!          FAHL(IIS)=FAHL_SWF(IIS)
!!
!          ROU1=ASWFCOND(IS1,2)
!          ROU2=ASWFCOND(IS2,2)
!          ROUGHNESS=0.5D0*(ROU1+ROU2)
!          PGF(IIS)=GEOM/ROUGHNESS
!          IVC(IIS)=5
!        ENDDO
!      ENDDO
!!
!!--------------------------------------------------------------------------------------
!!4-----CONNECT SWF NODES WITH POROUS MATRIX
!!-------------------------------------------------------------------------------------
!!4A-----loop over all SWF node to GW connections
!      DO IFN = 1,NSWFGWC
!        IH = ASWFGWC(IFN,1)
!        NH = ASWFNDS(IH,1)
!        NL = ASWFGWC(IFN,2)
!        DO II = IA(NL)+1,IA(NL+1)-1
!          JJ = JA(II)
!          IF(JJ.NE.NH) CYCLE
!          GEOM = ASWFGWC(IFN,4)/ASWFGWC(IFN,3)
!          IIS = JAS(II)
!          IVC(IIS)=6
!!--------------------------------------------------------------------------------------
!          PGF(IIS) = GEOM*VKA(NL)
!!--------------------------------------------------------------------------------------
!        ENDDO
!      ENDDO
!!
!      DO IFNC=1,NSWFNDS
!        N = ASWFNDS(IFNC,1)
!        AREA(N) = ASWFNDS(IFNC,3)
!      ENDDO
!!7------RETURN
!      RETURN
!      END SUBROUTINE SFILLPGF_SWF
      
!      SUBROUTINE FILLIDXGLO_SWF
!!     ******************************************************************
!!      FILL POINTER ARRAY FOR SWF DOMAIN TO GLOBAL MATRIX IDXGLO_SWF
!!     ******************************************************************
!!
!!        SPECIFICATIONS:
!!     ------------------------------------------------------------------
!      USE SWF1MODULE, ONLY: IDXGLO_SWF,NJA_SWF,NSWFNDS,ASWFNDS
!      USE CLN1MODULE, ONLY: NCLNNDS
!      USE GLOBAL, ONLY: IA,JA,NODES
!      
!      integer :: ipos, nc1, nd1, ii, nd2
!!     ------------------------------------------------------------------
!      ALLOCATE(IDXGLO_SWF(NJA_SWF))
!!1--------LOOP OVER ALL CLN NODES
!      IPOS = 1
!      DO NC1 = 1,NSWFNDS
!        ND1 = ASWFNDS(NC1,1)
!!2-------LOOP OVER ALL CONNECTIONS OF NODE NC1 IN GLOBAL ARRAY
!        DO II = IA(ND1),IA(ND1+1)-1
!          ND2 = JA(II)
!          IF(ND2.GT.(NODES+NCLNNDS).AND.&
!            ND2.LE.(NODES+NCLNNDS+NSWFNDS))THEN
!            IDXGLO_SWF(IPOS) = II
!            IPOS = IPOS + 1
!          ENDIF
!        ENDDO
!      ENDDO
!!----------------------------------------------------------------------------------------
!!3------RETURN
!      RETURN
!      END SUBROUTINE FILLIDXGLO_SWF
      
      SUBROUTINE UPARLSTRP(LSTSUM,MXLST,IN,IOUT,NP,PACK,PTYPX,ITERP,&
                          NUMINST)
!     ******************************************************************
!     Read and store list parameter definition information for one
!     parameter.
!     ******************************************************************
!
!        SPECIFICATIONS:
!     ------------------------------------------------------------------
      USE PARAMMODULE
      CHARACTER*(*) PACK,PTYPX
      CHARACTER*4 PTYP
      CHARACTER*10 PN,CTMP1,CTMP2
      CHARACTER*400 LINE
      
      integer :: in, np, iout,lstsum,iterp, mxlst, numinst, lloc, istart, istop,n
      real :: r, pv
      integer :: nlst, ni
!     ------------------------------------------------------------------
!
!1------Read the parameter name and definition.
      READ(IN,'(A)') LINE
      LLOC=1
      CALL URWORD(LINE,LLOC,ISTART,ISTOP,0,N,R,IOUT,IN)
      PN=LINE(ISTART:ISTOP)
      CTMP1=PN
      CALL UPCASE(CTMP1)
      CALL URWORD(LINE,LLOC,ISTART,ISTOP,1,N,R,IOUT,IN)
      PTYP=LINE(ISTART:ISTOP)
      CALL URWORD(LINE,LLOC,ISTART,ISTOP,3,N,PV,IOUT,IN)
      CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,NLST,R,IOUT,IN)
!
!2------Check for multiple instances.
      CALL URWORD(LINE,LLOC,ISTART,ISTOP,1,N,R,IOUT,IN)
      IF (LINE(ISTART:ISTOP).EQ.'INSTANCES') THEN
        CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,NUMINST,R,IOUT,IN)
        IF (NUMINST.LT.1) THEN
          WRITE(IOUT,12) PN,PTYP
   12     FORMAT(/,1X,'*** ERROR: NUMINST SPECIFIED LESS THAN 1',&
             ' FOR PARAMETER "',A,'"',/,12X,'OF TYPE "',A,&
             '" -- STOP EXECUTION (UPARLSTRP)')
          CALL USTOP(' ')
        ENDIF
      ELSE
        NUMINST = 0
      ENDIF
!
!3------Look for parameter in list of parameters.
      DO 10 NP=1,MXPAR
        CTMP2=PARNAM(NP)
        CALL UPCASE(CTMP2)
        IF(CTMP1.EQ.CTMP2) THEN
!
!3A-----If found, determine if it is an illegal duplicate.
          IF(PARTYP(NP).NE.' ' .AND. ITERP.EQ.1) THEN
!
!3B-----Illegal duplicate.
            WRITE(IOUT,110) CTMP1
  110       FORMAT (' Duplicate parameter name: ',A)
            CALL USTOP(' ')
          END IF
!
!3C-----Parameter was predefined in SEN file (PARTYP blank) or in
!3C-----a prior simulation (ITERP not 1) -- leave its value alone.
!3C-----(i.e. ignore PV).
          GO TO 100
        ELSE IF(PARNAM(NP).EQ.' ') THEN
!
!4------Parameter was not found in the list, so it is a new definition.
          PARNAM(NP)=PN
          B(NP)=PV
          IPSUM=IPSUM+1
          GO TO 100
        ENDIF
10    CONTINUE
!
!5------Too many parameters.
      WRITE(IOUT,99)
  99  FORMAT(' Number of parameters exceeds MXPAR -- STOP EXECUTION')
      CALL USTOP(' ')
!
!6------Parameter is a new parameter, or it was predefined in the
!6------Parameter Value file or defined in a previous simulation.  Continue
!6------processing.
 100  CONTINUE
      IF(ITERP.EQ.1) THEN
!
!7------Parameter is new or was predefined in the Parameter Value file.
!7------Process the remaining parameter information.
        PARTYP(NP)=PTYP
        IPLOC(1,NP)=LSTSUM
        NI=MAX(1,NUMINST)
        LSTSUM=LSTSUM+(NLST*NI)
        IPLOC(2,NP)=LSTSUM-1
        IPLOC(3,NP)=NUMINST
        IPLOC(4,NP)=INAMLOC
        INAMLOC=INAMLOC+NUMINST
!
!8------WRITE PARAMETER INFORMATION
        WRITE(IOUT,121) PARNAM(NP),PARTYP(NP)
  121   FORMAT(1X/,1X,'PARAMETER NAME:',A,'   TYPE:',A)
        WRITE(IOUT,122) PV
  122   FORMAT(1X,'Parameter value from package file is: ',1PG13.5)
        IF(B(NP).NE.PV) THEN
          WRITE(IOUT,123) B(NP)
  123     FORMAT(1X,'This value has been changed to:',7X,1PG13.5,&
             ', as read from',/,' the Parameter Value file')
        END IF
        WRITE(IOUT,130) NLST
  130   FORMAT(  '   NUMBER OF ENTRIES: ',I6)
        IF(NUMINST.GT.0) THEN
          WRITE(IOUT,131)NUMINST
  131     FORMAT('   NUMBER OF INSTANCES: ',I4)
        ENDIF
!
!9------Check if the parameter list will fit in the package list array.
        IF((LSTSUM-1) .GT. MXLST) THEN
          WRITE(IOUT,134) LSTSUM-1,MXLST
  134     FORMAT(1X,'EXCEEDED THE MAXIMUM NUMBER OF LIST ENTRIES:'/&
               1X,I5,' list entries have been specified'/&
               1X,'The maximum number of list entries is',I5)
          CALL USTOP(' ')
        END IF
!
!10-----Check if number of instances exceeds the maximum allowed.
        IF((INAMLOC-1).GT.MXINST) THEN
          WRITE(IOUT,135)INAMLOC-1,MXINST
  135     FORMAT(1X,'EXCEEDED THE MAXIMUM NUMBER OF INSTANCES:'/&
               1X,I5,' instances have been specified'/&
               1X,'The maximum number of instances is',I5)
          CALL USTOP(' ')
        END IF
!
!11-----Check for correct parameter type.
        IF(PARTYP(NP).NE.PTYPX) THEN
          WRITE(IOUT,137) PTYPX,PACK
  137     FORMAT(1X,'Parameter type must be:',A,' in the ',A,' Package')
          CALL USTOP(' ')
        END IF
!
!12-----Parameter definition must include at least one cell.
        IF (NLST.LE.0) THEN
          WRITE(IOUT,140) PN
          CALL USTOP(' ')
        ENDIF
  140   FORMAT(' ERROR:  DEFINITION FOR PARAMETER "',A,'"',&
           ' INCLUDES NO CELLS',/,'   -- STOP EXECUTION (UPARLSTRP)')
      ELSE
!
!13-----This is not the first time the simulation was run, so the parameter
!13-----was already defined.  Set values of arguments to be returned.
        LSTSUM=LSTSUM+IPLOC(2,NP)-IPLOC(1,NP)+1
        NUMINST=IPLOC(3,NP)
      ENDIF
!
!14-----Set the parameter to be inactive.
      IACTIVE(NP)=0
!
!15-----Return.
      RETURN
    END SUBROUTINE UPARLSTRP

    SUBROUTINE ULSTRD(NLIST,RLIST,LSTBEG,LDIM,MXLIST,IAL,INPACK,IOUT,&
          LABEL,CAUX,NCAUX,NAUX,IFREFM,NCOL,NROW,NLAY,ISCLOC1,ISCLOC2,&
          IPRFLG)
!     ******************************************************************
!     Read and print a list.  NAUX of the values in the list are
!     optional -- auxiliary data.
!     ******************************************************************
      CHARACTER*(*) LABEL
      CHARACTER*16 CAUX(NCAUX)
      real :: RLIST(LDIM,MXLIST)
      CHARACTER*400 LINE,FNAME
      integer :: nunopn
      DATA NUNOPN/99/
      INCLUDE 'openspec.inc'
      
      integer :: nlist, lstbeg, iout, inpack, ial, iscloc1, naux
      integer :: nrow, iscloc2, ifrefm, ncol, nlay, iprflg, in, iclose
      real :: sfac
      integer :: lloc, i
      real :: r
      integer :: istop, istart, n, nread2, nread1, ii, jj, k, j
      integer :: mxlist, ldim, idum, iloc, ncaux
      
!     ------------------------------------------------------------------
!
!1------If the list is empty, return.
      IF (NLIST.EQ.0) RETURN
!
!2------Check for and decode EXTERNAL and OPEN/CLOSE records.
      IN=INPACK
      ICLOSE=0
      READ(IN,'(A)') LINE
      SFAC=1.
      LLOC=1
      CALL URWORD(LINE,LLOC,ISTART,ISTOP,1,I,R,IOUT,IN)
      IF(LINE(ISTART:ISTOP).EQ.'EXTERNAL') THEN
         CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,I,R,IOUT,IN)
         IN=I
         IF(IPRFLG.EQ.1)WRITE(IOUT,111) IN
  111    FORMAT(1X,'Reading list on unit ',I4)
         READ(IN,'(A)') LINE
      ELSE IF(LINE(ISTART:ISTOP).EQ.'OPEN/CLOSE') THEN
         CALL URWORD(LINE,LLOC,ISTART,ISTOP,0,N,R,IOUT,IN)
         FNAME=LINE(ISTART:ISTOP)
         IN=NUNOPN
         IF(IPRFLG.EQ.1)WRITE(IOUT,115) IN,FNAME
  115    FORMAT(1X,/1X,'OPENING FILE ON UNIT ',I4,':',/1X,A)
         OPEN(UNIT=IN,FILE=FNAME,ACTION=ACTION(1))
         ICLOSE=1
         READ(IN,'(A)') LINE
      END IF
!
!3------Check for SFAC record.
      LLOC=1
      CALL URWORD(LINE,LLOC,ISTART,ISTOP,1,I,R,IOUT,IN)
      IF(LINE(ISTART:ISTOP).EQ.'SFAC') THEN
         CALL URWORD(LINE,LLOC,ISTART,ISTOP,3,I,SFAC,IOUT,IN)
         IF(IPRFLG.EQ.1) THEN
           WRITE(IOUT,116) SFAC
  116      FORMAT(1X,'LIST SCALING FACTOR=',1PG12.5)
           IF(ISCLOC1.EQ.ISCLOC2) THEN
              WRITE(IOUT,113) ISCLOC1
  113         FORMAT(1X,'(THE SCALE FACTOR WAS APPLIED TO FIELD',I2,')')
           ELSE
              WRITE(IOUT,114) ISCLOC1,ISCLOC2
  114         FORMAT(1X,'(THE SCALE FACTOR WAS APPLIED TO FIELDS',&
                I2,'-',I2,')')
           END IF
         ENDIF
         READ(IN,'(A)') LINE
      END IF
!
!3------Write a label for the list if the list will be printed.
      IF(IPRFLG.EQ.1) THEN
         WRITE(IOUT,'(1X)')
         CALL ULSTLB(IOUT,LABEL,CAUX,NCAUX,NAUX)
      END IF
!
!4------Setup indices for reading the list
      NREAD2=LDIM-IAL
      NREAD1=NREAD2-NAUX
      N=NLIST+LSTBEG-1
!
!5------Read the list.
      DO 250 II=LSTBEG,N
!
!5A-----Read a line into the buffer.  (The first line has already been
!5A-----read to scan for EXTERNAL and SFAC records.)
      IF(II.NE.LSTBEG) READ(IN,'(A)') LINE
!
!5B-----Get the non-optional values from the line.
      IF(IFREFM.EQ.0) THEN
         READ(LINE,'(3I10,9F10.0)') K,I,J,(RLIST(JJ,II),JJ=4,NREAD1)
         LLOC=10*NREAD1+1
      ELSE
         LLOC=1
         CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,K,R,IOUT,IN)
         CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,I,R,IOUT,IN)
         CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,J,R,IOUT,IN)
         DO 200 JJ=4,NREAD1
         CALL URWORD(LINE,LLOC,ISTART,ISTOP,3,IDUM,RLIST(JJ,II),IOUT,IN)
200      CONTINUE
      END IF
      RLIST(1,II)=K
      RLIST(2,II)=I
      RLIST(3,II)=J
!
!5C------Scale fields ISCLOC1-ISCLOC2 by SFAC
      DO 204 ILOC=ISCLOC1,ISCLOC2
        RLIST(ILOC,II)=RLIST(ILOC,II)*SFAC
204   CONTINUE
!
!5D-----Get the optional values from the line
      IF(NAUX.GT.0) THEN
         DO 210 JJ=NREAD1+1,NREAD2
         CALL URWORD(LINE,LLOC,ISTART,ISTOP,3,IDUM,RLIST(JJ,II),IOUT,IN)
210      CONTINUE
      END IF
!
!5E-----Write the values that were read if IPRFLG is 1.
      NN=II-LSTBEG+1
      IF(IPRFLG.EQ.1)&
         WRITE(IOUT,205) NN,K,I,J,(RLIST(JJ,II),JJ=4,NREAD2)
205   FORMAT(1X,I6,I7,I7,I7,26G16.4)
!
!5F-----Check for illegal grid location
      IF(K.LT.1 .OR. K.GT.NLAY) THEN
         WRITE(IOUT,*) ' Layer number in list is outside of the grid'
         CALL USTOP(' ')
      END IF
      IF(I.LT.1 .OR. I.GT.NROW) THEN
         WRITE(IOUT,*) ' Row number in list is outside of the grid'
         CALL USTOP(' ')
      END IF
      IF(J.LT.1 .OR. J.GT.NCOL) THEN
         WRITE(IOUT,*) ' Column number in list is outside of the grid'
         CALL USTOP(' ')
      END IF
  250 CONTINUE
!
!6------Done reading the list.  If file is open/close, close it.
      IF(ICLOSE.NE.0) CLOSE(UNIT=IN)
!
      RETURN
    END SUBROUTINE ULSTRD
          
    SUBROUTINE ULSTRDU(NLIST,RLIST,LSTBEG,LDIM,MXLIST,IAL,INPACK,IOUT,&
          LABEL,CAUX,NCAUX,NAUX,IFREFM,NODES,ISCLOC1,ISCLOC2,&
          IPRFLG)
!     ******************************************************************
!     Read and print a list for unstructured grid variables.
!      NAUX of the values in the list are Optional -- auxiliary data.
!     ******************************************************************
      CHARACTER*(*) LABEL
      CHARACTER*16 CAUX(NCAUX)
      real :: RLIST(LDIM,MXLIST)
      CHARACTER*400 LINE,FNAME
      integer :: nunopn
      DATA NUNOPN/99/
      INCLUDE 'openspec.inc'
      
      integer :: inpack, iout, ial, lstbeg, nlist, iscloc1, nodes
      integer :: naux, ifrefm, iscloc2, iprflg, in, iclose
      real :: sfac
      integer :: lloc, istop
      real :: r
      integer :: i, istart, n, nread2, nread1, ii,k,jj, idum, iloc
      integer :: mxlist, ldim, ncaux
      
!     ------------------------------------------------------------------
!
!1------If the list is empty, return.
      IF (NLIST.EQ.0) RETURN
!
!2------Check for and decode EXTERNAL and OPEN/CLOSE records.
      IN=INPACK
      ICLOSE=0
      READ(IN,'(A)') LINE
      SFAC=1.
      LLOC=1
      CALL URWORD(LINE,LLOC,ISTART,ISTOP,1,I,R,IOUT,IN)
      IF(LINE(ISTART:ISTOP).EQ.'EXTERNAL') THEN
         CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,I,R,IOUT,IN)
         IN=I
         IF(IPRFLG.EQ.1)WRITE(IOUT,111) IN
  111    FORMAT(1X,'Reading list on unit ',I4)
         READ(IN,'(A)') LINE
      ELSE IF(LINE(ISTART:ISTOP).EQ.'OPEN/CLOSE') THEN
         CALL URWORD(LINE,LLOC,ISTART,ISTOP,0,N,R,IOUT,IN)
         FNAME=LINE(ISTART:ISTOP)
         IN=NUNOPN
         IF(IPRFLG.EQ.1)WRITE(IOUT,115) IN,FNAME
  115    FORMAT(1X,/1X,'OPENING FILE ON UNIT ',I4,':',/1X,A)
         OPEN(UNIT=IN,FILE=FNAME,ACTION=ACTION(1))
         ICLOSE=1
         READ(IN,'(A)') LINE
      END IF
!
!3------Check for SFAC record.
      LLOC=1
      CALL URWORD(LINE,LLOC,ISTART,ISTOP,1,I,R,IOUT,IN)
      IF(LINE(ISTART:ISTOP).EQ.'SFAC') THEN
         CALL URWORD(LINE,LLOC,ISTART,ISTOP,3,I,SFAC,IOUT,IN)
         IF(IPRFLG.EQ.1) THEN
           WRITE(IOUT,116) SFAC
  116      FORMAT(1X,'LIST SCALING FACTOR=',1PG12.5)
           IF(ISCLOC1.EQ.ISCLOC2) THEN
              WRITE(IOUT,113) ISCLOC1
  113         FORMAT(1X,'(THE SCALE FACTOR WAS APPLIED TO FIELD',I2,')')
           ELSE
              WRITE(IOUT,114) ISCLOC1,ISCLOC2
  114         FORMAT(1X,'(THE SCALE FACTOR WAS APPLIED TO FIELDS',&
                I2,'-',I2,')')
           END IF
         ENDIF
         READ(IN,'(A)') LINE
      END IF
!
!3------Write a label for the list if the list will be printed.
      IF(IPRFLG.EQ.1) THEN
         WRITE(IOUT,'(1X)')
         CALL ULSTLB(IOUT,LABEL,CAUX,NCAUX,NAUX)
      END IF
!
!4------Setup indices for reading the list
      NREAD2=LDIM-IAL
      NREAD1=NREAD2-NAUX
      N=NLIST+LSTBEG-1
!
!5------Read the list.
      DO 250 II=LSTBEG,N
!
!5A-----Read a line into the buffer.  (The first line has already been
!5A-----read to scan for EXTERNAL and SFAC records.)
      IF(II.NE.LSTBEG) READ(IN,'(A)') LINE
!
!5B-----Get the non-optional values from the line.
      IF(IFREFM.EQ.0) THEN
         READ(LINE,'(I10,9F10.0)') K,(RLIST(JJ,II),JJ=4,NREAD1)
         LLOC=10*NREAD1+1
      ELSE
         LLOC=1
         CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,K,R,IOUT,IN)
         DO 200 JJ=4,NREAD1
         CALL URWORD(LINE,LLOC,ISTART,ISTOP,3,IDUM,RLIST(JJ,II),IOUT,IN)
200      CONTINUE
      END IF
      RLIST(1,II)=K
      RLIST(2,II)=1
      RLIST(3,II)=1
!
!5C------Scale fields ISCLOC1-ISCLOC2 by SFAC
      DO 204 ILOC=ISCLOC1,ISCLOC2
        RLIST(ILOC,II)=RLIST(ILOC,II)*SFAC
204   CONTINUE
!
!5D-----Get the optional values from the line
      IF(NAUX.GT.0) THEN
         DO 210 JJ=NREAD1+1,NREAD2
         CALL URWORD(LINE,LLOC,ISTART,ISTOP,3,IDUM,RLIST(JJ,II),IOUT,IN)
210      CONTINUE
      END IF
!
!5E-----Write the values that were read if IPRFLG is 1.
      NN=II-LSTBEG+1
      IF(IPRFLG.EQ.1) &
          WRITE(IOUT,205) NN,K,(RLIST(JJ,II),JJ=4,NREAD2) 
205  FORMAT(1X,I8,5X,I8,26G16.4)
!
!5F-----Check for illegal grid location
      IF(K.LT.1 .OR. K.GT.NODES) THEN
         WRITE(IOUT,*) ' Node number in list is outside of the grid'
         CALL USTOP(' ')
      END IF
  250 CONTINUE
!
!6------Done reading the list.  If file is open/close, close it.
      IF(ICLOSE.NE.0) CLOSE(UNIT=IN)
!
      RETURN
    END SUBROUTINE ULSTRDU

    SUBROUTINE ULSTLB(IOUT,LABEL,CAUX,NCAUX,NAUX)
!     ******************************************************************
!     PRINT A LABEL FOR A LIST
!     ******************************************************************
!
!        SPECIFICATIONS:
!     ------------------------------------------------------------------
      CHARACTER*(*) LABEL
      CHARACTER*16 CAUX(NCAUX)
      CHARACTER*400 BUF
      CHARACTER*1 DASH(400)
      DATA DASH/400*'-'/
      
      integer :: ncaux, naux, iout, len, nbuf, i, n1, j
!     ------------------------------------------------------------------
!
!1------Construct the complete label in BUF.  Start with BUF=LABEL.
      BUF=LABEL
!
!2------Add auxiliary data names if there are any.
      NBUF=LEN(LABEL)+9
      IF(NAUX.GT.0) THEN
         DO 10 I=1,NAUX
         N1=NBUF+1
         NBUF=NBUF+16
         BUF(N1:NBUF)=CAUX(I)
10       CONTINUE
      END IF
!
!3------Write the label.
      WRITE(IOUT,103) BUF(1:NBUF)
  103 FORMAT(1X,A)
!
!4------Add a line of dashes.
      WRITE(IOUT,104) (DASH(J),J=1,NBUF)
  104 FORMAT(1X,400A)
!
!5------Return.
      RETURN
    END SUBROUTINE ULSTLB
    
    SUBROUTINE MUSG_ReadCHD(Modflow)
!     ******************************************************************
!     ALLOCATE ARRAY STORAGE FOR TIME-VARIANT SPECIFIED-HEAD CELLS AND
!     READ NAMED PARAMETER DEFINITIONS
!     ******************************************************************
!
!     SPECIFICATIONS:
!     ------------------------------------------------------------------
      USE GLOBAL,      ONLY:IOUT,NCOL,NROW,NLAY,IFREFM,IUNSTR,NEQS
      USE GWFCHDMODULE,ONLY:NCHDS,MXCHD,NCHDVL,IPRCHD,NPCHD,ICHDPB,&
                           NNPCHD,CHDAUX,CHDS
      implicit none
      
      type (MUSG_Project) Modflow

      CHARACTER*400 LINE
      
      integer :: in, mxpc, mxactc, lloc
      real :: r
      integer :: istart, istop, naux, n, lstsum, k, lstbeg, ip, numinst, nlst, ib, lb, i

      in=modflow.iCHD 
      iout=FNumEco
!     ------------------------------------------------------------------
      ALLOCATE(NCHDS,MXCHD,NCHDVL,IPRCHD)
      ALLOCATE(NPCHD,ICHDPB,NNPCHD)
!
!1------IDENTIFY OPTION AND INITIALIZE # OF SPECIFIED-HEAD CELLS
      WRITE(IOUT,1)IN
    1 FORMAT(1X,/1X,'CHD -- TIME-VARIANT SPECIFIED-HEAD OPTION,',&
       ' VERSION 7, 5/2/2005',/1X,'INPUT READ FROM UNIT ',I4)
      NCHDS=0
      NNPCHD=0
!
!2------READ AND PRINT MXCHD (MAXIMUM NUMBER OF SPECIFIED-HEAD
!2------CELLS TO BE SPECIFIED EACH STRESS PERIOD)
      CALL URDCOM(IN,IOUT,LINE)
      CALL UPARLSTAL(IN,IOUT,LINE,NPCHD,MXPC)
      IF(IFREFM.EQ.0) THEN
         READ(LINE,'(I10)') MXACTC
         LLOC=11
      ELSE
         LLOC=1
         CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,MXACTC,R,IOUT,IN)
      END IF
      WRITE(IOUT,3) MXACTC
    3 FORMAT(1X,'MAXIMUM OF ',I6,&
       ' TIME-VARIANT SPECIFIED-HEAD CELLS AT ONE TIME')
!
!3------READ AUXILIARY VARIABLES AND PRINT OPTION
      ALLOCATE (CHDAUX(100))
      NAUX=0
      IPRCHD=1
   10 CALL URWORD(LINE,LLOC,ISTART,ISTOP,1,N,R,IOUT,IN)
      IF(LINE(ISTART:ISTOP).EQ.'AUXILIARY' .OR.&
             LINE(ISTART:ISTOP).EQ.'AUX') THEN
         CALL URWORD(LINE,LLOC,ISTART,ISTOP,1,N,R,IOUT,IN)
         IF(NAUX.LT.100) THEN
            NAUX=NAUX+1
            CHDAUX(NAUX)=LINE(ISTART:ISTOP)
            WRITE(IOUT,12) CHDAUX(NAUX)
   12       FORMAT(1X,'AUXILIARY CHD VARIABLE: ',A)
         END IF
         GO TO 10
      ELSE IF(LINE(ISTART:ISTOP).EQ.'NOPRINT') THEN
         WRITE(IOUT,13)
   13    FORMAT(1X,&
     'LISTS OF TIME-VARIANT SPECIFIED-HEAD CELLS WILL NOT BE PRINTED')
         IPRCHD = 0
         GO TO 10
      END IF
      NCHDVL=5+NAUX
!
!4------ALLOCATE SPACE FOR TIME-VARIANT SPECIFIED-HEAD LIST.
      ICHDPB=MXACTC+1
      MXCHD=MXACTC+MXPC
      ALLOCATE (CHDS(NCHDVL,MXCHD))
!
!1------READ NAMED PARAMETERS.
      WRITE(IOUT,1000) NPCHD
 1000 FORMAT(1X,//1X,I5,' TIME-VARIANT SPECIFIED-HEAD PARAMETERS')
      IF(NPCHD.GT.0) THEN
        NAUX=NCHDVL-5
        LSTSUM=ICHDPB
        DO 120 K=1,NPCHD
          LSTBEG=LSTSUM
          CALL UPARLSTRP(LSTSUM,MXCHD,IN,IOUT,IP,'CHD','CHD',1,NUMINST)
          NLST=LSTSUM-LSTBEG
          IF (NUMINST.GT.1) NLST = NLST/NUMINST
!         ASSIGN STARTING INDEX FOR READING INSTANCES
          IF (NUMINST.EQ.0) THEN
            IB=0
          ELSE
            IB=1
          ENDIF
!         READ LIST(S) OF CELLS, PRECEDED BY INSTANCE NAME IF NUMINST>0
          LB=LSTBEG
          DO 110 I=IB,NUMINST
            IF (I.GT.0) THEN
              CALL UINSRP(I,IN,IOUT,IP,IPRCHD)
            ENDIF
            IF(IUNSTR.EQ.0)THEN
              CALL ULSTRD(NLST,CHDS,LB,NCHDVL,MXCHD,0,IN,IOUT,&
          'CHD NO.   LAYER   ROW   COL   START FACTOR      END FACTOR',&
             CHDAUX,100,NAUX,IFREFM,NCOL,NROW,NLAY,4,5,IPRCHD)
            ELSE
              CALL ULSTRDU(NLST,CHDS,LB,NCHDVL,MXCHD,0,IN,IOUT,&
          'CHD NO.        NODE           START FACTOR      END FACTOR',&
             CHDAUX,100,NAUX,IFREFM,NEQS,4,5,IPRCHD)
            ENDIF
            LB=LB+NLST
  110     CONTINUE
  120   CONTINUE
      END IF
!
!3------RETURN.
      RETURN
    END SUBROUTINE MUSG_ReadCHD

      SUBROUTINE MUSG_ReadRCH(modflow)
!     ******************************************************************
!     ALLOCATE ARRAY STORAGE FOR RECHARGE
!     ******************************************************************
!
!        SPECIFICATIONS:
!     ------------------------------------------------------------------
      USE GLOBAL,      ONLY:IOUT,IFREFM,NODLAY,IUNSTR
      USE GWFBASMODULE, ONLY: IATS
      USE GWFRCHMODULE,ONLY:NRCHOP,IRCHCB,NPRCH,IRCHPF,RECH,IRCH,&
       MXNDRCH,INIRCH,NIRCH,SELEV,iznrch,mxznrch,ISELEV,IPONDOPT,&
       IRTSOPT,ICONCRCHOPT,INRTS,TIMRCH,IRTSRD,RECHSV,RCHF,&
       RCHCONC,IRCHCONC,ICONCRCHOPT
      USE GWTBCTMODULE, ONLY: MCOMPT

      implicit none
      
      type (MUSG_Project) Modflow

!
      CHARACTER*400 LINE
      CHARACTER*4 PTYP
      
      integer :: in, lloc, istart, istop
      real :: r
      integer :: n, inoc, i, iconcrch, ii, inbct, inselev, k
      
      in=modflow.iRCH 
      iout=FNumEco
      
      INBCT=0   !rgm for now assume no bct (block-centred transport) 


!     ------------------------------------------------------------------
!
!1-------ALLOCATE SCALAR VARIABLES.
      ALLOCATE(NRCHOP,IRCHCB,MXNDRCH)
      ALLOCATE(NPRCH,IRCHPF,INIRCH,NIRCH)
      ALLOCATE(INRTS,TIMRCH,IRTSRD)
      IRTSRD=0
!
!2------IDENTIFY PACKAGE.
      IRCHPF=0
      WRITE(IOUT,1)IN
    1 FORMAT(1X,/1X,'RCH -- RECHARGE PACKAGE, VERSION 7, 5/2/2005',&
     ' INPUT READ FROM UNIT ',I4)
!
!3------READ NRCHOP AND IRCHCB.
      CALL URDCOM(IN,IOUT,LINE)
      CALL UPARARRAL(IN,IOUT,LINE,NPRCH)
      IF(IFREFM.EQ.0) THEN
         READ(LINE,'(2I10)') NRCHOP,IRCHCB
      ELSE
         LLOC=1
         CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,NRCHOP,R,IOUT,IN)
         CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,IRCHCB,R,IOUT,IN)
      END IF
!
!3B------READ KEYWORD OPTIONS SEEPELEV, RTS AND RECHARGE CONC.
      ALLOCATE(IPONDOPT,IRTSOPT,ICONCRCHOPT)
      IPONDOPT=0
      IRTSOPT=0
      ICONCRCHOPT=0
      LLOC=1
   10 CALL URWORD(LINE,LLOC,ISTART,ISTOP,1,N,R,IOUT,IN)
!3B1------FOR SEEPAGE-FACE ELEVATION
      IF(LINE(ISTART:ISTOP).EQ.'SEEPELEV') THEN
        WRITE(IOUT,13)
   13   FORMAT(1X,'SEEPAGE-FACE ELEVATIONS WILL BE READ.',&
           '  VARIABLE INSELEV REQUIRED IN RECORD 5.')
        IPONDOPT = 1
      END IF
!3B2------FOR RTS
      IF(LINE(ISTART:ISTOP).EQ.'RTS') THEN
!3B2A-----CHECK TO SEE IF ATS IS ON. OR ELSE WRITE WARNING AND STOP
        IF(IATS.EQ.0)THEN
          WRITE(IOUT,15)
          STOP
        ENDIF
15      FORMAT(1X,'TRANSIENT RECHARGE NEEDS ADAPTIVE TIME-STEPPING.',&
          'STOPPING')
!3B2B------SET OPTION, AND READ MAXIMUM NUMBER OF ZONES OF TRANSIENT RCH.
        ALLOCATE(MXZNRCH)
        CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,MXZNRCH,R,IOUT,INOC)
        WRITE(IOUT,14)MXZNRCH
   14   FORMAT(1X,'TRANSIENT RECHARGE FILE WITH',I8,' ZONES WILL BE',&
           ' READ. RECHARGE ZONE INDICES WILL BE READ FROM RCH FILE.'&
              /1X,107('-'))
        IRTSOPT = 1
      END IF
!3BC------FOR CONCENTRATION OF RECHARGE
      IF(LINE(ISTART:ISTOP).EQ.'CONCENTRATION' .OR.&
        LINE(ISTART:ISTOP).EQ.'CONC' ) THEN
        WRITE(IOUT,16)
   16   FORMAT(1X,'SPECIES CONCENTRATION WILL BE READ.',&
           '  ARRAY IRCHCONC REQUIRED TO INDICATE SPECIES')
        ICONCRCHOPT = 1
      END IF
      IF(LLOC.LT.200) GO TO 10
!3C------READ NUMBER OF RECHARGE NODES IF UNSTRUCTURED AND NRCHOP=2
      IF(IUNSTR.EQ.1.AND.NRCHOP.EQ.2)THEN
        READ(IN,*) MXNDRCH
      ELSE
        MXNDRCH = NODLAY(1)
      ENDIF
!3D-----ALLOCATE ZONAL ARRAY
        IF(IRTSOPT.EQ.1)THEN
          ALLOCATE(iznrch(mxndrch))
        ELSE
          ALLOCATE(iznrch(1))
        ENDIF
!3D-----ALLOCATE CONCENTRATION ARRAY
        IF(ICONCRCHOPT.GT.0)THEN
          ALLOCATE(IRCHCONC(MCOMPT))
!3D1------READ INDEX ARRAY FOR COMPONENTS WHOSE CONC IS READ
          READ(IN,*)(IRCHCONC(I),I=1,MCOMPT)
          ICONCRCH = 0
          DO II=1,MCOMPT
            ICONCRCH = ICONCRCH + IRCHCONC(II)
          ENDDO
          ALLOCATE(RCHCONC(mxndrch,ICONCRCH))
!3D1------READ ARRAY OF COMPONENT NUMBERS
        ELSE
          ALLOCATE(IRCHCONC(1))
          ALLOCATE(RCHCONC(1,1))
        ENDIF
!
!4------CHECK TO SEE THAT OPTION IS LEGAL.
      IF(NRCHOP.LT.1.OR.NRCHOP.GT.3) THEN
        WRITE(IOUT,8) NRCHOP
    8   FORMAT(1X,'ILLEGAL RECHARGE OPTION CODE (NRCHOP = ',I5,&
            ') -- SIMULATION ABORTING')
        CALL USTOP(' ')
      END IF
!
!5------OPTION IS LEGAL -- PRINT OPTION CODE.
      IF(NRCHOP.EQ.1) WRITE(IOUT,201)
  201 FORMAT(1X,'OPTION 1 -- RECHARGE TO TOP LAYER')
      IF(NRCHOP.EQ.2) WRITE(IOUT,202)
  202 FORMAT(1X,'OPTION 2 -- RECHARGE TO ONE SPECIFIED NODE IN EACH',&
          ' VERTICAL COLUMN')
      IF(NRCHOP.EQ.3) WRITE(IOUT,203)
  203 FORMAT(1X,'OPTION 3 -- RECHARGE TO HIGHEST ACTIVE NODE IN',&
          ' EACH VERTICAL COLUMN')
!
!6------IF CELL-BY-CELL FLOWS ARE TO BE SAVED, THEN PRINT UNIT NUMBER.
      IF(IRCHCB.GT.0) WRITE(IOUT,204) IRCHCB
  204 FORMAT(1X,'CELL-BY-CELL FLOWS WILL BE SAVED ON UNIT ',I4)
!
!7------ALLOCATE SPACE FOR THE RECHARGE (RECH) AND INDICATOR (IRCH)
!7------ARRAYS.
      ALLOCATE (RECH(MXNDRCH))
      ALLOCATE (IRCH(MXNDRCH))
!8------IF TRANSPORT IS ACTIVE THEN ALLOCATE ARRAY TO STORE FLUXES
      IF(INBCT.GT.0)THEN
        ALLOCATE (RCHF(MXNDRCH))
      ENDIF
!--------ALLOCATE SPACE TO SAVE SEEPAGE-FACE INFORMATION
      ALLOCATE (ISELEV)
      ISELEV = 0
      INSELEV = 0
      IF (IPONDOPT.GT.0) THEN
        ALLOCATE(SELEV(mxndrch))
        DO I=1,MXNDRCH
          SELEV(I) = 1.0E20
        ENDDO
      ELSE
          ALLOCATE(SELEV(1))
      ENDIF
!---------ALLOCATE SPACE TO SAVE ORIGINAL RECH ARRAY FROM STRESS PERIODS
      IF(IRTSOPT.GT.0)THEN
        ALLOCATE (RECHSV(MXNDRCH))
      ELSE
        ALLOCATE (RECHSV(1))
      ENDIF
!
!8------READ NAMED PARAMETERS
      WRITE(IOUT,5) NPRCH
    5 FORMAT(1X,//1X,I5,' Recharge parameters')
      IF(NPRCH.GT.0) THEN
         DO 20 K=1,NPRCH
         CALL UPARARRRP(IN,IOUT,N,0,PTYP,1,1,0)
         IF(PTYP.NE.'RCH') THEN
            WRITE(IOUT,7)
    7       FORMAT(1X,'Parameter type must be RCH')
            CALL USTOP(' ')
         END IF
   20    CONTINUE
      END IF
!
!9------RETURN
      RETURN
      END SUBROUTINE MUSG_ReadRCH
      
      SUBROUTINE MUSG_ReadRCH_StressPeriods(Modflow)
!     ******************************************************************
!     READ RECHARGE DATA FOR STRESS PERIOD
!     ******************************************************************
!
!        SPECIFICATIONS:
!     ------------------------------------------------------------------
      USE GLOBAL,      ONLY:IOUT,NCOL,NROW,NLAY,IFREFM,&
       NODLAY,AREA,IUNSTR,NODES
      USE GWFRCHMODULE,ONLY:NRCHOP,NPRCH,IRCHPF,RECH,IRCH,INIRCH,NIRCH,&
       SELEV,iznrch,mxznrch,ISELEV,IPONDOPT,IRTSOPT,RECHSV,ICONCRCHOPT,&
       tstartrch,tendrch,factrrch,RTSRCH,INRTS,IRTSRD,TIMRCH,&
       RCHCONC,IRCHCONC
      USE GWTBCTMODULE, ONLY: MCOMPT
      
      implicit none
      
      type (MUSG_Project) Modflow

      REAL, DIMENSION(:,:),ALLOCATABLE  ::TEMP
      INTEGER, DIMENSION(:,:),ALLOCATABLE  ::ITEMP
!
      CHARACTER*24 ANAME(5)
      CHARACTER(LEN=200) line
!
      DATA ANAME(1) /'    RECHARGE LAYER INDEX'/
      DATA ANAME(2) /'                RECHARGE'/
      DATA ANAME(3) /'                   SELEV'/
      DATA ANAME(4) /'                  iznrch'/
      DATA ANAME(5) /'                    CONC'/
      
      integer :: in, lloc, iniznrch, inselev, inconc
      real :: r
      integer :: istop, n, istart, inoc, inrech, i, j, ir, ic
      integer :: iflag, kper, iurts, izr, iconcrch, ii
      
      in=modflow.iRCH
      iout=FNumEco
!     ------------------------------------------------------------------
      ALLOCATE (TEMP(NCOL,NROW))
      ALLOCATE (ITEMP(NCOL,NROW))
!2------IDENTIFY PACKAGE.
      WRITE(IOUT,1)IN
    1 FORMAT(1X,/1X,'RCH -- RECHARGE PACKAGE, VERSION 7, 5/2/2005',&
     ' INPUT READ FROM UNIT ',I4)
!
!2------READ FLAGS SHOWING WHETHER DATA IS TO BE REUSED.
      lloc = 1
      iniznrch=0
      INSELEV=0
      INCONC=0
      CALL URDCOM(In, Iout, line)
!3------GET OPTIONS FIRST
   10 CALL URWORD(LINE,LLOC,ISTART,ISTOP,1,N,R,IOUT,IN)
      IF(LINE(ISTART:ISTOP).EQ.'INRCHZONES') THEN
!3B------READ KEYWORD OPTION FOR RTS ZONES TO BE READ.
        CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,INIZNRCH,R,IOUT,INOC)
        WRITE(IOUT,14) INIZNRCH
14      FORMAT(/1X,'FLAG FOR INPUT OF RTS ZONES (INIZNRCH) = ',&
             I8)
      ELSEIF(LINE(ISTART:ISTOP).EQ.'INSELEV') THEN
!3C------IS KEWORD OPTION FOR SEEPAGE ELEVATION TO BE READ
        CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,INSELEV,R,IOUT,INOC)
        WRITE(IOUT,15) INSELEV
15      FORMAT(/1X,'FLAG FOR INPUT OF SEEPAGE ELEVATIONS (INSELEV) = ',&
             I8)
       ELSEIF(LINE(ISTART:ISTOP).EQ.'INCONC') THEN
!3C------IS KEWORD OPTION FOR CONCENTRATION TO BE READ
        INCONC = 1
        WRITE(IOUT,16) INCONC
16      FORMAT(/1X,'FLAG FOR INPUT OF CONCENTRATIONS (INCONC) = ',&
             I8)
      END IF
      IF(LLOC.LT.200) GO TO 10
      LLOC = 1
!3D------READ FLAGS
      IF(IFREFM.EQ.0)THEN
        IF(NRCHOP.EQ.2) THEN
          READ(LINE,'(2I10)') INRECH,INIRCH
        ELSE
          READ(LINE,'(I10)') INRECH
          INIRCH = NODLAY(1)
        ENDIF
      ELSE
        IF(NRCHOP.EQ.2) THEN
          CALL URWORD(line, lloc, istart, istop, 2, inrech, r, Iout, In)
          CALL URWORD(line, lloc, istart, istop, 2, inirch, r, Iout, In)
        ELSE
          CALL URWORD(line, lloc, istart, istop, 2, inrech, r, Iout, In)
          INIRCH = NODLAY(1)
        ENDIF
      END IF
      IF(INIRCH.GE.0) NIRCH = INIRCH
      IF(INSELEV.GE.0) ISELEV = INSELEV
!
!3------TEST INRECH TO SEE HOW TO DEFINE RECH.
      IF(INRECH.LT.0) THEN
!
!3A-----INRECH<0, SO REUSE RECHARGE ARRAY FROM LAST STRESS PERIOD.
        WRITE(IOUT,3)
    3   FORMAT(1X,/1X,'REUSING RECH FROM LAST STRESS PERIOD')
      ELSE
        IF(IUNSTR.EQ.0)THEN
!
!3B-----INRECH=>0, SO READ RECHARGE RATE.
          IF(NPRCH.EQ.0) THEN
!
!3B1--------THERE ARE NO PARAMETERS, SO READ RECH USING U2DREL.
            CALL U2DREL(TEMP,ANAME(2),NROW,NCOL,0,IN,IOUT)
          ELSE
!3B2--------DEFINE RECH USING PARAMETERS.  INRECH IS THE NUMBER OF
!3B2--------PARAMETERS TO USE THIS STRESS PERIOD.
            CALL PRESET('RCH')
            WRITE(IOUT,33)
   33       FORMAT(1X,///1X,&
           'RECH array defined by the following parameters:')
            IF(INRECH.EQ.0) THEN
              WRITE(IOUT,34)
   34         FORMAT(' ERROR: When parameters are defined for the RCH',&
           ' Package, at least one parameter',/,' must be specified',&
           ' each stress period -- STOP EXECUTION (GWF2RCH8U1RPLL)')
              CALL USTOP(' ')
            END IF
            CALL UPARARRSUB2(TEMP,NCOL,NROW,0,INRECH,IN,IOUT,'RCH',&
                 ANAME(2),'RCH',IRCHPF)
          END IF
          N=0
          DO I=1,NROW
          DO J=1,NCOL
            N=N+1
            RECH(N)=TEMP(J,I)
          ENDDO
          ENDDO
        ELSE ! READ RECH FOR UNSTRUCTURED GRID
!3B-------INRECH=>0, SO READ RECHARGE RATE.
          IF(NPRCH.EQ.0) THEN
!
!3B1--------THERE ARE NO PARAMETERS, SO READ RECH USING U2DREL.
            CALL U2DREL(RECH,ANAME(2),1,NIRCH,0,IN,IOUT)
          ELSE
!
!3B2--------DEFINE RECH USING PARAMETERS.  INRECH IS THE NUMBER OF
!3B2--------PARAMETERS TO USE THIS STRESS PERIOD.
            CALL PRESET('RCH')
            WRITE(IOUT,33)
            IF(INRECH.EQ.0) THEN
              WRITE(IOUT,34)
              CALL USTOP(' ')
            END IF
            CALL UPARARRSUB2(RECH,NIRCH,1,0,INRECH,IN,IOUT,'RCH',&
                 ANAME(2),'RCH',IRCHPF)
          END IF
        ENDIF
      ENDIF
!
!5------IF NRCHOP=2 THEN A LAYER INDICATOR ARRAY IS NEEDED.  TEST INIRCH
!5------TO SEE HOW TO DEFINE IRCH.
        IF(NRCHOP.EQ.2) THEN
          IF(INIRCH.LT.0) THEN
!
!5A---------INIRCH<0, SO REUSE LAYER INDICATOR ARRAY FROM LAST STRESS PERIOD.
            WRITE(IOUT,2)
    2       FORMAT(1X,/1X,'REUSING IRCH FROM LAST STRESS PERIOD')
          ELSE
!
!5B---------INIRCH=>0, SO CALL U2DINT TO READ LAYER INDICATOR ARRAY(IRCH)
            IF(IUNSTR.EQ.0)THEN
              CALL U2DINT(ITEMP,ANAME(1),NROW,NCOL,0,IN,IOUT)
              N=0
              DO 57 IR=1,NROW
              DO 57 IC=1,NCOL
                N=N+1
                IF(ITEMP(IC,IR).LT.1 .OR. ITEMP(IC,IR).GT.NLAY) THEN
                  WRITE(IOUT,56) IC,IR,ITEMP(IC,IR)
   56             FORMAT(/1X,'INVALID LAYER NUMBER IN IRCH FOR COLUMN',&
                 I4,'  ROW',I4,'  :',I4)
                 CALL USTOP(' ')
                END IF
                IRCH(N) = (ITEMP(IC,IR)-1)*NROW*NCOL + (IR-1)*NCOL + IC
   57         CONTINUE
              NIRCH = NROW*NCOL
            ELSE
              CALL U2DINT(IRCH,ANAME(1),1,NIRCH,0,IN,IOUT)
!----------------------------------------------------
! ------------CHECK FOR IRCH BEING LARGER THAN NODES
              IFLAG = 0
              DO I=1,NIRCH
                IF(IRCH(I).GT.NODES)THEN
                  IFLAG = IRCH(I)
                  GO TO 112
                ENDIF
              ENDDO
112           CONTINUE
! ------------WRITE MESSAGE AND STOP IF IEVT IS LARGER THAN NODES
              IF(IFLAG.GT.0)THEN
                WRITE(IOUT,75)IFLAG,NODES
75              FORMAT('INDEX NODE NO.',I10,&
               ', LARGER THAN TOTAL GWF NODES (',I10,'), STOPPING')
                STOP
              ENDIF
!----------------------------------------------------
            END IF
          END IF
        ELSE ! NRCHOP IS NOT 2 SO SET TOP LAYER OF NODES IN IRCH
          DO I=1,NIRCH
            IRCH(I) = I
          ENDDO
        END IF
!
!-------IF RECHARGE IS READ THEN MULTIPLY BY AREA TO GIVE FLUX
        IF(INRECH.GE.0) THEN
!
!4--------MULTIPLY RECHARGE RATE BY CELL AREA TO GET VOLUMETRIC RATE.
          DO 50 NN=1,NIRCH
            N = IRCH(NN)
            RECH(NN)=RECH(NN)*AREA(N)
   50     CONTINUE
        END IF
!----------------------------------------------------------------
!----------RECHARGE ZONES
      IF(IRTSOPT.EQ.0) GO TO 101
      IF(INiznrch.LE.0) THEN
!
!3A-----INiznrch=<0, SO REUSE iznrch ARRAY FROM LAST STRESS PERIOD.
        WRITE(IOUT,5)
    5   FORMAT(1X,/1X,'REUSING iznrch FROM LAST STRESS PERIOD')
      ELSEif(INiznrch.gt.0)then
!3B-----READ IZNRCH ARRAY AND FIRST TIME OF RTS FILE AT KPER=1
        mxznrch = iniznrch
        IF(IUNSTR.EQ.0)THEN
          CALL U2DINT(iznrch,ANAME(4),NROW,NCOL,0,IN,IOUT)
        ELSE
          CALL U2DINT(iznrch,ANAME(4),1,NIRCH,0,IN,IOUT)
        ENDIF
!3C-------READ FIRST LINE OF RTS FILE AT KPER=1
        IF(KPER.EQ.1)THEN
          inrts = IURTS
          allocate(tstartrch,tendrch,factrrch,rtsrch(mxznrch))
         read(inrts,*)tstartrch,tendrch,factrrch,(rtsrch(i),i=1,mxznrch)
         write(iout,7)tstartrch,tendrch,factrrch,(rtsrch(i),i=1,mxznrch)
7        format(2x,'*** RTS read - Tstart, Tend, Factor, Rts(mxznrch)'/&
          5x,200g15.7)
!3D-------SET FLAGS FOR RTS AND ATS
          TIMRCH = TENDRCH
          IRTSRD = 0
        ENDIF
      ENDIF
!-----------------------------------------------------------------
!4--------APPLY RTS TO RECHARGE ARRAY IF RECHARGE OR ZONES CHANGE
        IF(INRECH.GE.0.OR.INIZNRCH.GT.0)THEN
!---------save original stress-period RECH in RECHSV array for later use
          IF(INRECH.GE.0)THEN
            DO NN=1,NIRCH
              RECHSV(NN) = RECH(NN)
            ENDDO
          ENDIF
!---------Add RTS recharge to RECH already on nodes
          DO 52 NN=1,NIRCH
          N = IRCH(NN)
          izr = iznrch(n)
          if(izr.ge.1.and.izr.le.mxznrch)&
         RECH(NN)=RECHSV(NN) + rtsrch(izr)*AREA(N)*factrrch
   52     CONTINUE
          WRITE(IOUT,6)
6         FORMAT(2X,'*** RECH ARRAY UPDATED FROM RTS FILE ***')
        ENDIF
101   CONTINUE
!----------------------------------------------------------------
!----------UNCONFINED RECHARGE WITHOUT PONDING
      IF(IPONDOPT.EQ.0) GO TO 102
      IF(INSELEV.LE.0) THEN
!
!3A-----INSELEV<0, SO REUSE SELEV ARRAY FROM LAST STRESS PERIOD.
        WRITE(IOUT,4)
    4   FORMAT(1X,/1X,'REUSING SELEV FROM LAST STRESS PERIOD')
      ELSEif(INSELEV.gt.0)then
        IF(IUNSTR.EQ.0)THEN
          CALL U2DREL(SELEV,ANAME(3),NROW,NCOL,0,IN,IOUT)
        ELSE
          CALL U2DREL(SELEV,ANAME(3),1,NIRCH,0,IN,IOUT)
        ENDIF
      ENDIF
102   CONTINUE
!----------------------------------------------------------------
!----------CONCENTRATION OF RECHARGE FOR TRANSPORT
      IF(ICONCRCHOPT.EQ.0) GO TO 103
      IF(INCONC.LE.0) THEN
!
!3A-----INCONC<0, SO REUSE CONCENTRATION ARRAY FROM LAST STRESS PERIOD.
        WRITE(IOUT,8)
    8   FORMAT(1X,/1X,'REUSING CONCENTRATION FROM LAST STRESS PERIOD')
      ELSEif(INCONC.gt.0)then
        ICONCRCH = 0
        DO II=1,MCOMPT
!          WRITE(IOUT,*)(' READING FOR COMPONENT NUMBER',MCOMPT)
          WRITE(IOUT,*) ' READING FOR COMPONENT NUMBER',MCOMPT  !kkz - remove parentheses per JCH (alternative is to use a FORMAT)
          IF(IRCHCONC(II).NE.0)THEN
            ICONCRCH = ICONCRCH + 1
            IF(IUNSTR.EQ.0)THEN
             CALL U2DREL(RCHCONC(1,ICONCRCH),ANAME(5),NROW,NCOL,&
                  0,IN,IOUT)
            ELSE
             CALL U2DREL(RCHCONC(1,ICONCRCH),ANAME(5),1,NIRCH,0,IN,IOUT)
            ENDIF
          ENDIF
        ENDDO
      ENDIF
103   CONTINUE
!---------------------------------------------------------------
      DEALLOCATE(TEMP)
      DEALLOCATE(ITEMP)
!6------RETURN
      RETURN
    END SUBROUTINE MUSG_ReadRCH_StressPeriods


    SUBROUTINE UPARARRAL(IN,IOUT,LINE,NP)
!     ******************************************************************
!     Setup array parameter definition for a package.
!     ******************************************************************
!
!        SPECIFICATIONS:
!     ------------------------------------------------------------------
      USE PARAMMODULE
      CHARACTER*(*) LINE
      
      integer :: in, np, iout, lloc, istop
      real :: r
      integer :: n, istart
!     ------------------------------------------------------------------
!
!  If NP has not already been defined, decode PARAMETER definitions if
!  they exist
      IF(IN.GT.0) THEN
         NP=0
         LLOC=1
         CALL URWORD(LINE,LLOC,ISTART,ISTOP,1,N,R,IOUT,IN)
         IF(LINE(ISTART:ISTOP).EQ.'PARAMETER') THEN
            CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,NP,R,IOUT,IN)
            READ(IN,'(A)') LINE
         END IF
      END IF
!
!  Process the parameter information
      IF(NP.GT.0) THEN
         WRITE(IOUT,31) NP
   31    FORMAT(1X,I5,' Named Parameters     ')
      ELSE
         NP=0
         WRITE(IOUT,'(A)') ' No named parameters'
      END IF
!
      RETURN
    END SUBROUTINE UPARARRAL
    
   SUBROUTINE PRESET(PTYP)
!     ******************************************************************
!     Clear active flag for all parameters of a specified type
!     ******************************************************************
!
!        SPECIFICATIONS:
!     ------------------------------------------------------------------
      USE PARAMMODULE
      CHARACTER*(*) PTYP
      
      integer :: i
!     ------------------------------------------------------------------
!
!1------Loop through all parameters.  Set IACTIVE to 0 when the
!1------parameter type matches.
      DO 10 I=1,IPSUM
      IF(PARTYP(I).EQ.PTYP) IACTIVE(I)=0
   10 CONTINUE
!
!2------Return.
      RETURN
    END SUBROUTINE PRESET

    SUBROUTINE UPARARRSUB2(ZZ,NCOL,NROW,ILAY,NP,IN,IOUT,PTYP,ANAME,&
           PACK,IPF)
!     ******************************************************************
!     Read a series of parameter names and substitute their values into
!     a 2-D array.
!     ******************************************************************
!
!        SPECIFICATIONS:
!     ------------------------------------------------------------------
      USE PARAMMODULE
      DIMENSION ZZ(NCOL,NROW)
      CHARACTER*(*) PTYP,PACK
      CHARACTER*24 ANAME
      CHARACTER*400 LINE
      CHARACTER*10 CTMP1,CTMP2,CTMP3,CTMP4
      
      integer :: in, ncol, nrow, ilay, iout, np, ipf, init, n
      real :: rdum, zz
      integer :: lloc, idum, istart, istop, ip, numinst, iloc,ni, ki, ii, nsub, i
!     ------------------------------------------------------------------
!
!1------Set initialization flag to cause USUB2D to initialze ZZ to 0.
      INIT=1
!
!2------Read each parameter name.
      DO 100 N=1,NP
        READ(IN,'(A)') LINE
        LLOC=1
        CALL URWORD(LINE,LLOC,ISTART,ISTOP,0,IDUM,RDUM,IOUT,IN)
        WRITE(IOUT,5) LINE(ISTART:ISTOP)
    5   FORMAT(' Parameter:  ',A)
        IF(LINE(ISTART:ISTOP).EQ.' ') THEN
          WRITE(IOUT,*) ' Blank parameter name in the ',PACK,' file.'
          CALL USTOP(' ')
        END IF
!
!3------Loop through each parameter looking for the specified name.
        CTMP1=LINE(ISTART:ISTOP)
        CALL UPCASE(CTMP1)
        DO 10 IP=1,IPSUM
          CTMP2=PARNAM(IP)
          CALL UPCASE(CTMP2)
          IF(CTMP1.EQ.CTMP2) GO TO 20
!
!3A-----Stop looping if the end of the parameter list is found.
          IF(PARNAM(IP).EQ.' ') GO TO 15
   10   CONTINUE
   15   WRITE(IOUT,16) PACK
   16   FORMAT(1X,'Error in ',A,' file:',/&
           1X,'The above parameter must be defined prior to its use')
        CALL USTOP(' ')
!
!4------Found parameter.
   20   CONTINUE
        IF(PARTYP(IP).NE.PTYP) THEN
!5------Print an error message if the parameter type does not match.
          WRITE(IOUT,83) PARNAM(IP),PARTYP(IP),PACK,PTYP
   83     FORMAT(1X,'Parameter type conflict:',/&
                1X,'Named parameter:',A,' was defined as type:',A,/&
                1X,'However, this parameter is used in the ',A,&
                  ' file, so it should be type:',A)
          CALL USTOP(' ')
        ENDIF
!
!6------Check to see if this parameter is time varying (has instances).
        NUMINST=IPLOC(3,IP)
        ILOC=IPLOC(4,IP)
        NI=1
!
!6A-----If parameter is time-varying, read instance name.
        IF(NUMINST.GT.0) THEN
          CALL URWORD(LINE,LLOC,ISTART,ISTOP,0,IDUM,RDUM,IOUT,IN)
          CTMP3=LINE(ISTART:ISTOP)
          IF(CTMP3.EQ.' ') THEN
            WRITE(IOUT,1000) PACK,PARNAM(IP)
 1000       FORMAT(/,1X,'Blank instance name in the ',A,&
                  ' file for parameter ',A)
            CALL USTOP(' ')
          ENDIF
          WRITE(IOUT,1010) CTMP3
 1010     FORMAT(3X,'Instance:  ',A)
          CALL UPCASE(CTMP3)
!
!6B------Look for instance name
          DO 50 KI=1,NUMINST
            CTMP4=INAME(ILOC+KI-1)
            CALL UPCASE(CTMP4)
            IF(CTMP3.EQ.CTMP4) THEN
              NI=KI
              GOTO 55
            ENDIF
   50     CONTINUE
          WRITE(IOUT,1020) PACK,CTMP3,PARNAM(IP)
 1020     FORMAT(/,1X,'The ',A,' file specifies undefined instance "',&
                A,'" for parameter ',A)
          CALL USTOP(' ')
   55     CONTINUE
        ENDIF
!
!7------Check to see if this parameter is already active.
        IF (IACTIVE(IP).GT.0) THEN
          WRITE(IOUT,1030) PARNAM(IP)
 1030     FORMAT(/,1X,'*** ERROR: PARAMETER "',A,&
             '" HAS ALREADY BEEN ACTIVATED THIS STRESS PERIOD',/,&
             ' -- STOP EXECUTION (UPARARRSUB2)')
          CALL USTOP(' ')
        ENDIF
!
!8------Activate the parameter and substitute.  Reset INIT so that
!8------any further calls to USUB2D will not reinitilize ZZ.
        IACTIVE(IP)=NI
        II=IP
        CALL USUB2D(ZZ,NCOL,NROW,II,ILAY,INIT,NSUB)
        INIT=0
!
!9------Get new value of print flag if it is there.
        CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,I,RDUM,-1,IN)
        IF(LINE(ISTART:ISTOP) .NE.'E' .AND.&
          LINE(ISTART:ISTOP) .NE.' ') IPF=I
!
  100 CONTINUE
!
!10-----PRINT THE ARRAY.
  200 CALL ULAPRWC(ZZ,NCOL,NROW,ILAY,IOUT,IPF,ANAME)
!
!11-----Return.
      RETURN
      END SUBROUTINE UPARARRSUB2

    SUBROUTINE UPARARRRP(IN,IOUT,NP,ILFLG,PTYP,ITERP,ITVP,IACT)
        !     ******************************************************************
        !     Read and store array parameter definition information for one
        !     parameter.
        !     ******************************************************************
        !
        !        SPECIFICATIONS:
        !     ------------------------------------------------------------------
              USE PARAMMODULE
              CHARACTER*(*) PTYP
              CHARACTER*400 LINE
              CHARACTER*10 PN,CTMP1,CTMP2
              
              integer :: in, iact, np, itvp, iterp, ilflg, iout, lloc, n, i, j
              real :: r, pv
              integer :: istop, ni, istart, nclu, numinst, ib, inst, kk
              integer :: im1, im2, iz1, iz2
        !     ------------------------------------------------------------------
        !
        !1------Read a parameter definition line and decode the parameter name,
        !1------type, and value
              READ(IN,'(A)') LINE
              LLOC=1
              CALL URWORD(LINE,LLOC,ISTART,ISTOP,0,N,R,IOUT,IN)
              PN=LINE(ISTART:ISTOP)
              CTMP1=PN
              CALL UPCASE(CTMP1)
              CALL URWORD(LINE,LLOC,ISTART,ISTOP,1,N,R,IOUT,IN)
              PTYP=LINE(ISTART:ISTOP)
              CALL URWORD(LINE,LLOC,ISTART,ISTOP,3,N,PV,IOUT,IN)
        !
        !2------Look for the parameter name in the parameter list
              DO 10 NP=1,MXPAR
                CTMP2=PARNAM(NP)
                CALL UPCASE(CTMP2)
                IF(CTMP1.EQ.CTMP2) THEN
        !
        !2A-----If found, determine if it is an illegal duplicate or if it was
        !         predefined.
                  IF(PARTYP(NP).NE.' ' .AND. ITERP.EQ.1) THEN
        !           Illegal duplicate
                    WRITE(IOUT,110) CTMP1
          110       FORMAT(' Duplicate parameter name: ',A)
                    CALL USTOP(' ')
                  END IF
        !         Parameter was predefined -- leave its value alone
        !         (i.e. ignore PV).
                  GO TO 100
                ELSE IF(PARNAM(NP).EQ.' ') THEN
        !         Parameter was not found in the list, so it is a new
        !         definition. Put values in the list.
                  PARNAM(NP)=PN
                  B(NP)=PV
                  IPSUM=IPSUM+1
                  GO TO 100
                END IF
        10    CONTINUE
        !
        !2B-----Entire parameter list has been searched without finding
        !2B-----a blank entry for the new parameter.  Too many parameters
              WRITE(IOUT,11)
           11 FORMAT(1X,'The number of parameters has exceeded the maximum')
              CALL USTOP(' ')
        !
        !3------Parameter is a new parameter or it was prefined in the
        !3------Parameter Value file.  Get the number of clusters.
          100 PARTYP(NP)=PTYP
              CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,NCLU,R,IOUT,IN)
              IF(NCLU.LE.0) THEN
                WRITE(IOUT,104) PN
          104   FORMAT(' ERROR:  DEFINITION FOR PARAMETER "',A,'"',&
                ' INCLUDES NO CLUSTERS',/,'   -- STOP EXECUTION (UPARARRRP)')
                CALL USTOP(' ')
              ENDIF
              IF(ITERP.EQ.1) THEN
                NUMINST=0
                IF (ITVP.GT.0) THEN
        !
        !4------CHECK FOR MULTIPLE INSTANCES.
                  CALL URWORD(LINE,LLOC,ISTART,ISTOP,1,N,R,IOUT,IN)
                  IF (LINE(ISTART:ISTOP).EQ.'INSTANCES') THEN
                    CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,NUMINST,R,IOUT,IN)
                    IF (NUMINST.LT.1) THEN
                      WRITE(IOUT,12) PARNAM(NP),PTYP
           12         FORMAT(/,1X,'*** ERROR: NUMINST SPECIFIED LESS THAN 1',&
                         ' FOR PARAMETER "',A,'"',/,12X,'OF TYPE "',A,&
                         '" -- STOP EXECUTION (UPARARRRP)')
                      CALL USTOP(' ')
                    ENDIF
                  ENDIF
                ENDIF
        !
        !5------SET IPLOC VALUES.
                IPLOC(1,NP)=ICLSUM+1
                NI=MAX(1,NUMINST)
                ICLSUM=ICLSUM+NCLU*NI
                IPLOC(2,NP)=ICLSUM
                IPLOC(3,NP)=NUMINST
                IPLOC(4,NP)=INAMLOC
                INAMLOC=INAMLOC+NUMINST
        !
        !6------MAKE SURE THAT THE MAXIMUM NUMBER OF CLUSTERS IN IPCLST IS
        !6------NOT EXCEEDED.
                IF(IPLOC(2,NP).GT.MXCLST) THEN
                  WRITE(IOUT,117) IPLOC(2,NP),MXCLST
          117     FORMAT(1X,I5,&
                ' CLUSTERS WERE SPECIFIED, BUT THERE IS SPACE FOR ONLY',I5)
                  WRITE(IOUT,*) NP,NCLU
                  WRITE(IOUT,'(A)') PARNAM(NP)
                  WRITE(IOUT,'(4I10)') IPLOC
                  CALL USTOP(' ')
                END IF
                WRITE(IOUT,121) PARNAM(NP),PARTYP(NP),NCLU
          121   FORMAT(1X/,1X,'PARAMETER NAME:',A,'   TYPE:',A,'   CLUSTERS:',&
                      I4)
                WRITE(IOUT,122) PV
          122   FORMAT(1X,'Parameter value from package file is: ',1PG13.5)
                IF(B(NP).NE.PV) THEN
                  WRITE(IOUT,123) B(NP)
          123     FORMAT(1X,'This value has been changed to:',7X,1PG13.5,&
                     ', as read from',/,' the Parameter Value file')
                END IF
        !
        !7------MAKE SURE THE MAXIMUM NUMBER OF INSTANCES IS NOT EXCEEDED.
                IF(NUMINST.GT.0) THEN
                  WRITE(IOUT,124)NUMINST
          124      FORMAT(3X,'NUMBER OF INSTANCES: ',I4)
                  IF((INAMLOC-1).GT.MXINST) THEN
                    WRITE(IOUT,125)INAMLOC-1,MXINST
          125       FORMAT(1X,'EXCEEDED THE MAXIMUM NUMBER OF INSTANCES:'/&
                    1X,I5,' instances have been specified'/&
                    1X,'The maximum number of instances is',I5)
                    CALL USTOP(' ')
                  ENDIF
                ENDIF
              ELSE
                NUMINST=IPLOC(3,NP)
              ENDIF
              IACTIVE(NP)=IACT
        !
        !8------Process clusters for each instance.
              IF(NUMINST.EQ.0) THEN
                IB=0
              ELSE
                IB=1
              ENDIF
              I=IPLOC(1,NP)-1
              DO 210 INST=IB,NUMINST
                IF(NUMINST.GT.0) CALL UINSRP(INST,IN,IOUT,NP,ITERP)
        !
        !9------Read and process clusters.
                DO 200 KK=1,NCLU
                  I=I+1
                  READ(IN,'(A)') LINE
                  IF(ITERP.EQ.1) THEN
                    LLOC=1
                    IF(ILFLG.NE.0) THEN
        !
        !9A-----Get layer number for cluster
                      CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,IPCLST(1,I),R,IOUT,&
                                 IN)
                    ELSE
                      IPCLST(1,I)=0
                    END IF
        !
        !9B-----Get multiplier and zone array names.
                    CALL URWORD(LINE,LLOC,IM1,IM2,0,N,R,IOUT,IN)
                    CALL URWORD(LINE,LLOC,IZ1,IZ2,0,N,R,IOUT,IN)
        !
        !9C-----Get zone numbers.
                    DO 30 J=5,14
                      CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,IPCLST(J,I),R,-1,IN)
                      IF(IPCLST(J,I).EQ.0) THEN
                        IPCLST(4,I)=J-1
                        GO TO 32
                      END IF
           30       CONTINUE
                    IPCLST(4,I)=14
           32       CONTINUE
                    IF(ILFLG.NE.0) THEN
                      WRITE(IOUT,36) IPCLST(1,I),LINE(IM1:IM2),LINE(IZ1:IZ2)
           36         FORMAT(16X,'LAYER: ',I3,'    MULTIPLIER ARRAY: ',A,&
                     '    ZONE ARRAY: ',A)
                    ELSE
                      WRITE(IOUT,37) LINE(IM1:IM2),LINE(IZ1:IZ2)
           37         FORMAT(16X,'MULTIPLIER ARRAY: ',A,'    ZONE ARRAY: ',A)
                    END IF
        !
        !9D-----Find the multiplier array number.
                    CTMP1=LINE(IM1:IM2)
                    CALL UPCASE(CTMP1)
                    IF(CTMP1.EQ.'NONE') THEN
                      IPCLST(2,I)=0
                    ELSE
                      IF(NMLTAR.GT.0) THEN
                      DO 40 J=1,NMLTAR
                        CTMP2=MLTNAM(J)
                        CALL UPCASE(CTMP2)
                        IF(CTMP1.EQ.CTMP2) GO TO 45
           40           CONTINUE
                      END IF
                      WRITE(IOUT,'(A)') ' Multiplier array has not been defined'
                      CALL USTOP(' ')
           45         IPCLST(2,I)=J
                    END IF
        !
        !9E-----Find the zone array number.
                    CTMP1=LINE(IZ1:IZ2)
                    CALL UPCASE(CTMP1)
                    IF(CTMP1.EQ.'ALL') THEN
                      IPCLST(3,I)=0
                    ELSE
                      IF(IPCLST(4,I).EQ.4) THEN
                        WRITE(IOUT,47)
           47           FORMAT(1X,&
                       'There were no zone values specified in the cluster',/&
                       1X,'At least one zone must be specified')
                        CALL USTOP(' ')
                      END IF
                      WRITE(IOUT,48) (IPCLST(J,I),J=5,IPCLST(4,I))
           48         FORMAT(1X,'               ZONE VALUES:',10I5)
                      IF(NZONAR.GT.0) THEN
                        DO 50 J=1,NZONAR
                          CTMP2=ZONNAM(J)
                          CALL UPCASE(CTMP2)
                          IF(CTMP1.EQ.CTMP2) GO TO 55
           50           CONTINUE
                      END IF
                      WRITE(IOUT,'(A)') ' Zone array has not been defined'
                      CALL USTOP(' ')
           55         IPCLST(3,I)=J
                    END IF
                  ENDIF
        !
          200   CONTINUE
          210 CONTINUE
        !
        !10-----RETURN.
              RETURN
    END SUBROUTINE UPARARRRP
    
    SUBROUTINE SGWF2LPFU1CK(IOUT,NP,PTYP)
        !     ******************************************************************
        !     CHECK THAT JUST-DEFINED PARAMETER OF TYPE 'VK' OR 'VANI' IS USED
        !     CONSISTENTLY WITH LAYVKA ENTRIES FOR LAYERS LISTED IN CLUSTERS FOR
        !     THE PARAMETER
        !     ******************************************************************
        !
        !      SPECIFICATIONS:
        !     ------------------------------------------------------------------
              USE GWFBCFMODULE,  ONLY:LAYVKA
              USE PARAMMODULE
        !
              CHARACTER*4 PTYP
              
              integer :: np, iout, icl, lay, lv
        !     ------------------------------------------------------------------
        !
        !1------LOOP THROUGH THE CLUSTERS FOR THIS PARAMETER.
              DO 10 ICL = IPLOC(1,NP),IPLOC(2,NP)
                LAY = IPCLST(1,ICL)
                LV = LAYVKA(LAY)
                IF (PTYP.EQ.'VK  ' .AND. LV.NE.0) THEN
                  WRITE (IOUT,590) LAY,LV,LAY,PARNAM(NP),'VK'
          590     FORMAT(/,&
             1X,'LAYVKA entered for layer ',i3,' is: ',i3,'; however,',&
             ' layer ',i3,' is',/,' listed in a cluster for parameter "',a,&
             '" of type ',a,' and')
                  WRITE (IOUT,600)
          600     FORMAT(&
             1X,'parameters of type VK can apply only to layers for which',&
             /,' LAYVKA is specified as zero -- STOP EXECUTION (SGWF2LPFU1CK)')
                  CALL USTOP(' ')
                ELSEIF (PTYP.EQ.'VANI' .AND. LV.EQ.0) THEN
                  WRITE (IOUT,590) LAY,LV,LAY,PARNAM(NP),'VANI'
                  WRITE (IOUT,610)
          610     FORMAT(&
             1X,'parameters of type VANI can apply only to layers for which',/,&
             ' LAYVKA is not specified as zero -- STOP EXECUTION',&
             ' (SGWF2LPFU1CK)')
                  CALL USTOP(' ')
                ENDIF
           10 CONTINUE
        !
        !2------Return.
              RETURN
    END SUBROUTINE SGWF2LPFU1CK
   
!-------------------------------------------------------------------------
    SUBROUTINE SGWF2LPFU1S(IN,NPHK,NPHANI,NPVK,NPVANI,NPSS,NPSY,&
               NPVKCB,STOTXT,NOPCHK)
        !     ******************************************************************
        !     ALLOCATE AND READ DATA FOR LAYER PROPERTY FLOW PACKAGE FOR STRUCTURED GRID
        !     ******************************************************************
        !
        !        SPECIFICATIONS:
        !     ------------------------------------------------------------------
              USE GLOBAL, ONLY:NCOL,NROW,NLAY,ITRSS,LAYCBD,ISYM,&
                         IBOUND,BUFF,IOUT,NODES,IVC,&
                         IUNSTR,IA,JA,JAS,NJA,ARAD,IPRCONN
              USE GWFBCFMODULE,ONLY:LAYCON,HK,SC1,SC2,WETDRY,&
                                   laywet,&
                                   LAYTYP,CHANI,LAYVKA,&
                                   LAYFLG,VKA,VKCB,HANI,IHANISO,&
                                   alpha,beta,sr,brook,BP,IBPN,itabrich
        !
              REAL, DIMENSION(:,:),ALLOCATABLE  ::TEMP
              REAL, DIMENSION (:), ALLOCATABLE :: TEMPPL
        !
              CHARACTER*24 ANAME(15),STOTXT
              CHARACTER*4 PTYP
        !
              DATA ANAME(1) /'   HYD. COND. ALONG ROWS'/
              DATA ANAME(2) /'  HORIZ. ANI. (COL./ROW)'/
              DATA ANAME(3) /'     VERTICAL HYD. COND.'/
              DATA ANAME(4) /' HORIZ. TO VERTICAL ANI.'/
              DATA ANAME(5) /'QUASI3D VERT. HYD. COND.'/
              DATA ANAME(6) /'        SPECIFIC STORAGE'/
              DATA ANAME(7) /'          SPECIFIC YIELD'/
              DATA ANAME(8) /'        WETDRY PARAMETER'/
              DATA ANAME(9) /'     STORAGE COEFFICIENT'/
              DATA ANAME(10) /'        WETDRY PARAMETER'/
              DATA ANAME(11) /'                   alpha'/
              DATA ANAME(12) /'                    beta'/
              DATA ANAME(13) /'                      sr'/
              DATA ANAME(14) /'                   brook'/
              DATA ANAME(15) /'     BUBBLING POINT HEAD'/
              REAL PI
              
              integer :: npsy, in, npvani, npss, nphk, nphani, npvk, npvkcb, nopchk, n, i, k, j
              integer :: ii, jj, iis, kk, khani, ianame
        !     ------------------------------------------------------------------
        !1-------ALLOCATE TEMP ARRAY FOR STORING 3-D INFORMATION
              ALLOCATE(TEMP(NCOL,NROW))
              ZERO = 0.0
        !2------SET ANGLE INTO ARAD WHEN THERE IS HORIZONTAL ANISOTROPY
              IF(IHANISO.EQ.1)THEN
                PI = 3.1415926536
        !2A----SET FACE ANGLES IN ARAD
                DO N=1,NODES
                  DO II = IA(N)+1,IA(N+1)-1
                    JJ = JA(II)
                    IF(JJ.GE.N) CYCLE
                    IIS = JAS(II)
                    IF(IVC(IIS).EQ.1) CYCLE
                    IF((N-JJ).EQ.1) THEN
                      ARAD(IIS) = pi
                    ELSEIF((JJ-N).EQ.1) THEN
                      ARAD = 0
                    ELSEIF(JJ .LT. N) THEN
                      ARAD(IIS) =  pi/2.
                    ELSE
                      ARAD(IIS) = -pi/2.
                    ENDIF
                  ENDDO
                ENDDO
        !
        !2B-------WRITE FACE ANGLES ARRAY
                IF(IPRCONN.NE.0)THEN
                  WRITE(IOUT,*)'FACE ANGLE IS BELOW, 22G15.6, UNSYMMETRIC'
                  ALLOCATE(TEMPPL(NJA))
                  DO N=1,NODES
                  DO II = IA(N)+1,IA(N+1)-1
                    JJ = JA(II)
                    IF(JJ.GE.N)THEN
                      IIS = JAS(II)
                      TEMPPL(II) = ARAD(IIS)
                      TEMPPL(ISYM(II)) = ARAD(IIS)
                    ENDIF
                  ENDDO
                  ENDDO
                  WRITE(IOUT,55)(TEMPPL(J),J=1,NJA)
        55      FORMAT(1P,22G15.6)
        !SP          WRITE(IOUT,55)(ARAD(J),J=1,NJAS) !COMMENTED OUT SYMMETRIC WRITE
                  DEALLOCATE (TEMPPL)
                ENDIF
              ENDIF
        !
        !3------DEFINE DATA FOR EACH LAYER -- VIA READING OR NAMED PARAMETERS.
              DO 200 K=1,NLAY
              KK=K
        !
        !3A-----DEFINE HORIZONTAL HYDRAULIC CONDUCTIVITY (HK)
              IF(NPHK.EQ.0) THEN
                 CALL U2DREL(TEMP(1,1),ANAME(1),NROW,NCOL,KK,IN,IOUT)
              ELSE
                 READ(IN,*) LAYFLG(1,K)
                 WRITE(IOUT,121) ANAME(1),K,LAYFLG(1,K)
          121    FORMAT(1X,/1X,A,' FOR LAYER',I4,&
                ' will BE DEFINED BY PARAMETERS',/1X,'(PRINT FLAG=',I4,')')
                 CALL UPARARRSUB1(TEMP(1,1),NCOL,NROW,KK,'HK',&
                   IOUT,ANAME(1),LAYFLG(1,KK))
                 IF(NOPCHK.EQ.0) CALL UPARARRCK(BUFF(1),IBOUND,IOUT,K,NCOL,&
                 NLAY,NROW,IUNSTR,'HK  ')
              END IF
              DO I=1,NROW
              DO J=1,NCOL
                N=J+(I-1)*NCOL+(K-1)*NROW*NCOL
                HK(N) = TEMP(J,I)
              ENDDO
              ENDDO
        !
        !3B-----READ HORIZONTAL ANISOTROPY IF CHANI IS NON-ZERO
              IF(CHANI(K).LE.ZERO) THEN
                KHANI=-CHANI(K)
                IF(NPHANI.EQ.0) THEN
                   CALL U2DREL(TEMP(1,1),ANAME(2),NROW,NCOL,KK,IN,IOUT)
                ELSE
                   READ(IN,*) LAYFLG(6,K)
                   WRITE(IOUT,121) ANAME(2),K,LAYFLG(6,K)
                   CALL UPARARRSUB1(TEMP(1,1),NCOL,NROW,KK,'HANI',&
                   IOUT,ANAME(2),LAYFLG(6,KK))
                   IF(NOPCHK.EQ.0)CALL UPARARRCK(BUFF(1),IBOUND,IOUT,K,NCOL,&
                   NLAY,NROW,IUNSTR,'HANI')
                END IF
                DO I=1,NROW
                DO J=1,NCOL
                  N=J+(I-1)*NCOL+(K-1)*NROW*NCOL
                  HANI(N) = TEMP(J,I)
                ENDDO
                ENDDO
              END IF
        !
        !3C-----DEFINE VERTICAL HYDRAULIC CONDUCTIVITY OR HORIZONTAL TO VERTICAL
        !3C-----ANISOTROPY (VKA).
              IANAME=3
              PTYP='VK'
              IF(LAYVKA(K).NE.0) THEN
                 IANAME=4
                 PTYP='VANI'
              END IF
              IF(NPVK.EQ.0 .AND. NPVANI.EQ.0) THEN
                 CALL U2DREL(TEMP(1,1),ANAME(IANAME),NROW,NCOL,KK,IN,IOUT)
              ELSE
                 READ(IN,*) LAYFLG(2,K)
                 WRITE(IOUT,121) ANAME(IANAME),K,LAYFLG(2,K)
                 CALL UPARARRSUB1(TEMP(1,1),NCOL,NROW,KK,PTYP,IOUT,&
                                    ANAME(IANAME),LAYFLG(2,KK))
                 IF(NOPCHK.EQ.0)CALL UPARARRCK(BUFF(1),IBOUND,IOUT,K,NCOL,&
                  NLAY,NROW,IUNSTR,PTYP)
              END IF
              DO I=1,NROW
              DO J=1,NCOL
                N=J+(I-1)*NCOL+(K-1)*NROW*NCOL
                VKA(N) = TEMP(J,I)
              ENDDO
              ENDDO
        !
        !3D-----DEFINE SPECIFIC STORAGE OR STORAGE COEFFICIENT IN ARRAY SC1 IF TRANSIENT.
              IF(ITRSS.NE.0) THEN
                 IF(NPSS.EQ.0) THEN
                    CALL U2DREL(TEMP(1,1),STOTXT,NROW,NCOL,KK,IN,IOUT)
                 ELSE
                    READ(IN,*) LAYFLG(3,K)
                    WRITE(IOUT,121) STOTXT,K,LAYFLG(3,K)
                    CALL UPARARRSUB1(TEMP(1,1),NCOL,NROW,KK,'SS',&
                        IOUT,STOTXT,LAYFLG(3,KK))
                    IF(NOPCHK.EQ.0) CALL UPARARRCK(BUFF(1),IBOUND(1),IOUT,K,&
                    NCOL,NLAY,NROW,IUNSTR,'SS  ')
                 END IF
                 DO I=1,NROW
                 DO J=1,NCOL
                  N=J+(I-1)*NCOL+(K-1)*NROW*NCOL
                  SC1(N) = TEMP(J,I)
                 ENDDO
                 ENDDO
              END IF
        !
        !3E-----DEFINE SPECIFIC YIELD IN ARRAY SC2 IF TRANSIENT AND LAYER IS
        !3E-----IS CONVERTIBLE.
              IF(LAYTYP(K).NE.0) THEN
                 IF(ITRSS.NE.0) THEN
                    IF(NPSY.EQ.0) THEN
                       CALL U2DREL(TEMP(1,1),ANAME(7),NROW,NCOL,KK,IN,&
                              IOUT)
                    ELSE
                       READ(IN,*) LAYFLG(4,K)
                       WRITE(IOUT,121) ANAME(7),K,LAYFLG(4,K)
                       CALL UPARARRSUB1(TEMP(1,1),NCOL,&
                      NROW,KK,'SY',IOUT,ANAME(7),LAYFLG(4,KK))
                       IF(NOPCHK.EQ.0) CALL UPARARRCK(BUFF(1),IBOUND(1),IOUT,K,&
                       NCOL,NLAY,NROW,IUNSTR,'SY  ')
                    END IF
                    DO I=1,NROW
                    DO J=1,NCOL
                      N=J+(I-1)*NCOL+(K-1)*NROW*NCOL
                      SC2(N) = TEMP(J,I)
                    ENDDO
                    ENDDO
                 END IF
              END IF
        !
        !3F-----READ CONFINING BED VERTICAL HYDRAULIC CONDUCTIVITY (VKCB)
              IF(LAYCBD(K).NE.0) THEN
                 IF(NPVKCB.EQ.0) THEN
                    CALL U2DREL(TEMP(1,1),ANAME(5),NROW,NCOL,KK,IN,&
                          IOUT)
                 ELSE
                    READ(IN,*) LAYFLG(5,K)
                    WRITE(IOUT,121) ANAME(5),K,LAYFLG(5,K)
                    CALL UPARARRSUB1(TEMP(1,1),NCOL,NROW,KK,&
                      'VKCB',IOUT,ANAME(5),LAYFLG(5,KK))
                    IF(NOPCHK.EQ.0) CALL UPARARRCK(BUFF(1),IBOUND(1),IOUT,K,&
                    NCOL,NLAY,NROW,IUNSTR,'VKCB')
                 END IF
                 DO I=1,NROW
                 DO J=1,NCOL
                   N=J+(I-1)*NCOL+(K-1)*NROW*NCOL
                   VKCB(N) = TEMP(J,I)
                 ENDDO
                 ENDDO
              END IF
        !
        !3G-----READ WETDRY CODES IF WETTING CAPABILITY HAS BEEN INVOKED
        !3G-----(LAYWET NOT 0).
              IF(LAYWET(K).NE.0) THEN
                 CALL U2DREL(TEMP(1,1),ANAME(8),NROW,NCOL,KK,IN,IOUT)
                 DO I=1,NROW
                 DO J=1,NCOL
                   N=J+(I-1)*NCOL+(K-1)*NROW*NCOL
                   WETDRY(N) = TEMP(J,I)
                 ENDDO
                 ENDDO
              END IF
        !4----read parameters for Richards Equation if solving unsaturated zone flow
              IF(LAYCON(K).NE.5)GOTO 300
              IF(ITABRICH.EQ.0) THEN
        !3H-----READ alpha, beta, sr, brook
              CALL U2DREL(TEMP(1,1),ANAME(11),NROW,NCOL,KK,IN,IOUT)
              DO I=1,NROW
              DO J=1,NCOL
                N=J+(I-1)*NCOL+(K-1)*NROW*NCOL
                alpha(N) = TEMP(J,I)
              ENDDO
              ENDDO
              CALL U2DREL(TEMP(1,1),ANAME(12),NROW,NCOL,KK,IN,IOUT)
              DO I=1,NROW
              DO J=1,NCOL
                N=J+(I-1)*NCOL+(K-1)*NROW*NCOL
                beta(N) = TEMP(J,I)
              ENDDO
              ENDDO
              CALL U2DREL(TEMP(1,1),ANAME(13),NROW,NCOL,KK,IN,IOUT)
              DO I=1,NROW
              DO J=1,NCOL
                N=J+(I-1)*NCOL+(K-1)*NROW*NCOL
                sr(N) = TEMP(J,I)
              ENDDO
              ENDDO
              CALL U2DREL(TEMP(1,1),ANAME(14),NROW,NCOL,KK,IN,IOUT)
              DO I=1,NROW
              DO J=1,NCOL
                N=J+(I-1)*NCOL+(K-1)*NROW*NCOL
                brook(N) = TEMP(J,I)
              ENDDO
              ENDDO
              IF(IBPN.GT.0)THEN
                CALL U2DREL(TEMP(1,1),ANAME(15),NROW,NCOL,KK,IN,IOUT)
                DO I=1,NROW
                DO J=1,NCOL
                  N=J+(I-1)*NCOL+(K-1)*NROW*NCOL
                  bP(N) = TEMP(J,I)
                ENDDO
                ENDDO
              ENDIF
        !6------read tabular input for retention and relative permeability curves
              ELSE


              ENDIF
          300 CONTINUE
        !-------------------------------------------------------------------------
          200 CONTINUE
        !-------------------------------------------------------------------------
              DEALLOCATE(TEMP)
        !
        !4------RETURN
              RETURN
      END SUBROUTINE SGWF2LPFU1S
      
      SUBROUTINE SGWF2LPFU1G(IN,NPHK,NPHANI,NPVK,NPVANI,NPSS,NPSY,&
               NPVKCB,STOTXT,NOPCHK)
        !     ******************************************************************
        !     ALLOCATE AND READ DATA FOR LAYER PROPERTY FLOW PACKAGE FOR UNSTRUCTURED (GENERAL) GRID
        !     ******************************************************************
        !
        !        SPECIFICATIONS:
        !     ------------------------------------------------------------------
              USE GLOBAL,      ONLY:NLAY,ITRSS,LAYCBD,&
                              IBOUND,BUFF,IOUT,ARAD,JAS,&
                              NODES,IUNSTR,PGF,NJAS,TOP,BOT,&
                              NODLAY,IA,JA,IDSYMRD,IATMP,NJATMP,IVC,cl1
              USE GWFBCFMODULE,ONLY:SC1,SC2,WETDRY,&
                                   IKCFLAG,laywet,&
                                   LAYTYP,CHANI,LAYVKA,&
                                   LAYFLG,VKA,VKCB,HANI,HK,IHANISO,&
                                   alpha,beta,sr,brook,BP,IBPN,ITABRICH, LAYCON
        !
              REAL, DIMENSION(:),ALLOCATABLE  ::TEMP
        !
              CHARACTER*24 ANAME(16),STOTXT
              CHARACTER*4 PTYP
        !
              DATA ANAME(1) /'   HYD. COND. ALONG ROWS'/
              DATA ANAME(2) /'  HORIZ. ANI. (COL./ROW)'/
              DATA ANAME(3) /'     VERTICAL HYD. COND.'/
              DATA ANAME(4) /' HORIZ. TO VERTICAL ANI.'/
              DATA ANAME(5) /'QUASI3D VERT. HYD. COND.'/
              DATA ANAME(6) /'        SPECIFIC STORAGE'/
              DATA ANAME(7) /'          SPECIFIC YIELD'/
              DATA ANAME(8) /'        WETDRY PARAMETER'/
              DATA ANAME(9) /'     STORAGE COEFFICIENT'/
              DATA ANAME(10) /' CONNECTION CONDUCTIVITY'/
              DATA ANAME(11) /'              FACE ANGLE'/
              DATA ANAME(12) /'                   alpha'/
              DATA ANAME(13) /'                    beta'/
              DATA ANAME(14) /'                      sr'/
              DATA ANAME(15) /'                   brook'/
              DATA ANAME(16) /'     BUBBLING POINT HEAD'/
              
            integer :: npss, nphk, in, nphani, npsy, npvk, npvani, nopchk, npvkcb
            integer :: kk, nndlay, nstrt, khani, ianame, iis, n
            real :: thick1, thick2, thick, akn
            integer :: ndslay, ii, jj, ikn, k

        !     ------------------------------------------------------------------
        !
                ZERO=0.
        !1------READ FACE ANGLES IF ANISOTROPIC
              IF(IHANISO.EQ.1)THEN
                CALL U1DRELNJA(ARAD(1),IATMP,ANAME(11),NJATMP,IN,IOUT,IDSYMRD)
        !---------
        !8A.......CHANGE OUTWARD NORMAL FOR COMPATIBIITY WITH OLDER CROSS-DISPERSION FORMULATION
        ! --------OLDER FORMULATION DID DIFFERENTLY AND Y WAS + UPWARD IN MANY APPLICATIONS
        !        IF(IOUTNORMAL. EQ.0) THEN
        !          DO N=1,NODES
        !            IF(IBOUND(N).EQ.0) CYCLE
        !C3-------GO OVER UPPER CONNECTIONS OF NODE N AND FILL Vx, Vy, Vz IN BOTH
        !            DO II = IA(N)+1,IA(N+1)-1
        !              JJ = JA(II)
        !              IF(JJ.GT.N.AND.JJ.LE.NODES)THEN
        !                IIS = JAS(II)
        !                IF(IBOUND(JJ).NE.0)THEN
        !                   ANGLE = ARAD(IIS)
        !                   IF(ANGLE .GT. 1.57079. AND. ANGLE. LT. 1.57080)THEN
        !                     ARAD(IIS) = -1.570796 ! CONVERT + 90 DEGREES TO - 90 DEGREES
        !                   ELSEIF(ANGLE .GT. 4.712. AND. ANGLE. LT. 4.713)THEN
        !                     ARAD(IIS) = 1.570796  ! CONVERT - 90 DEGREES TO + 90 DEGREES
        !                   ENDIF
        !                ENDIF
        !              ENDIF
        !            ENDDO
        !          ENDDO
        !        ENDIF
              ENDIF
        !2------LOOP OVER ALL LAYERS TO DEFINE ARRAYS
              DO 200 K = 1,NLAY
              KK = K
              NNDLAY = NODLAY(K)
              NSTRT = NODLAY(K-1)+1
              NDSLAY = NNDLAY - NODLAY(K-1)
        !
        !2A-------Perform checks for unstructured grid formulations
              IF(IKCFLAG.NE.0)THEN
                IF(LAYCON(K).EQ.1.OR.LAYCON(K).EQ.3)WRITE(IOUT,10)K
              ENDIF
        10    FORMAT(5X,'**LAYTYP=1 IS NOT ALLOWED WITH IKCFLAG = 1 OR -1,',&
               1X,'SINCE K OF CONNECTIVITY IS READ. CHECK LAYER',I8)
        !-------------------------------------------------
              IF(IKCFLAG.NE.0)GO TO 120
        !-------------------------------------------------
        !3------DEFINE ARRAYS FOR EACH LAYER
        !3A-----DEFINE HORIZONTAL HYDRAULIC CONDUCTIVITY (HK)
              IF(NPHK.EQ.0) THEN
                 CALL U1DREL(HK(NSTRT),ANAME(1),NDSLAY,K,IN,IOUT)
              ELSE
                 READ(IN,*) LAYFLG(1,K)
                 WRITE(IOUT,121) ANAME(1),K,LAYFLG(1,K)
          121    FORMAT(1X,/1X,A,' FOR LAYER',I4,&
                ' WILL BE DEFINED BY PARAMETERS',/1X,'(PRINT FLAG=',I4,')')
                 CALL UPARARRSUB1(HK(NSTRT),NDSLAY,1,KK,'HK',&
                   IOUT,ANAME(1),LAYFLG(1,KK))
                 IF(NOPCHK.EQ.0) CALL UPARARRCK(BUFF,IBOUND(NSTRT),IOUT,K,&
                 NDSLAY,1,1,IUNSTR,'HK  ')
              END IF
        !
        !3B-----READ HORIZONTAL ANISOTROPY IF CHANI IS NON-ZERO
              IF(CHANI(K).LE.ZERO) THEN
                KHANI=-CHANI(K)
                IF(NPHANI.EQ.0) THEN
                   CALL U1DREL(HANI(NSTRT),ANAME(2),NDSLAY,K,IN,IOUT)
                ELSE
                   READ(IN,*) LAYFLG(6,K)
                   WRITE(IOUT,121) ANAME(2),K,LAYFLG(6,K)
                   CALL UPARARRSUB1(HANI(NSTRT),NDSLAY,1,KK,'HANI',&
                   IOUT,ANAME(2),LAYFLG(6,KK))
                   IF(NOPCHK.EQ.0) CALL UPARARRCK(BUFF,IBOUND(NSTRT),IOUT,K,&
                   NDSLAY,1,1,IUNSTR,'HANI')
                END IF
              END IF
        !
        !3C-----DEFINE VERTICAL HYDRAULIC CONDUCTIVITY OR HORIZONTAL TO VERTICAL
        !3C-----ANISOTROPY (VKA).
              IANAME=3
              PTYP='VK'
              IF(LAYVKA(K).NE.0) THEN
                 IANAME=4
                 PTYP='VANI'
              END IF
              IF(NPVK.EQ.0 .AND. NPVANI.EQ.0) THEN
                 CALL U1DREL(VKA(NSTRT),ANAME(IANAME),NDSLAY,K,IN,IOUT)
              ELSE
                 READ(IN,*) LAYFLG(2,K)
                 WRITE(IOUT,121) ANAME(IANAME),K,LAYFLG(2,K)
                 CALL UPARARRSUB1(VKA(NSTRT),NDSLAY,1,KK,PTYP,IOUT,&
                                    ANAME(IANAME),LAYFLG(2,KK))
                 IF(NOPCHK.EQ.0) CALL UPARARRCK(BUFF,IBOUND(NSTRT),IOUT,K,&
                 NDSLAY,1,1,IUNSTR,PTYP)
              END IF
        !-------------------------------------------------
        120   CONTINUE
        !-------------------------------------------------
        !
        !3D-----DEFINE SPECIFIC STORAGE OR STORAGE COEFFICIENT IN ARRAY SC1 IF TRANSIENT.
              IF(ITRSS.NE.0) THEN
                 IF(NPSS.EQ.0) THEN
                    CALL U1DREL(SC1(NSTRT),STOTXT,NDSLAY,K,IN,IOUT)
                 ELSE
                    READ(IN,*) LAYFLG(3,K)
                    WRITE(IOUT,121) STOTXT,K,LAYFLG(3,K)
                    CALL UPARARRSUB1(SC1(NSTRT),NDSLAY,1,KK,'SS',&
                        IOUT,STOTXT,LAYFLG(3,KK))
                    IF(NOPCHK.EQ.0)CALL UPARARRCK(BUFF,IBOUND(NSTRT),IOUT,K,&
                    NDSLAY,1,1,IUNSTR,'SS  ')
                 END IF
              END IF
        !
        !3E-----DEFINE SPECIFIC YIELD IN ARRAY SC2 IF TRANSIENT AND LAYER IS
        !3E-----IS CONVERTIBLE.
              IF(LAYTYP(K).NE.0) THEN
                 IF(ITRSS.NE.0) THEN
                    IF(NPSY.EQ.0) THEN
                       CALL U1DREL(SC2(NSTRT),ANAME(7),NDSLAY,K,IN,IOUT)
                    ELSE
                       READ(IN,*) LAYFLG(4,K)
                       WRITE(IOUT,121) ANAME(7),K,LAYFLG(4,K)
                       CALL UPARARRSUB1(SC2(NSTRT),NDSLAY,&
                      1,KK,'SY',IOUT,ANAME(7),LAYFLG(4,KK))
                     IF(NOPCHK.EQ.0)CALL UPARARRCK(BUFF,IBOUND(NSTRT),IOUT,K,&
                     NDSLAY,1,1,IUNSTR,'SY  ')
                    END IF
                 END IF
              END IF
        !
        !3F-----READ CONFINING BED VERTICAL HYDRAULIC CONDUCTIVITY (VKCB) IF NODAL INPUT
              IF(IKCFLAG.EQ.0.AND.LAYCBD(K).NE.0) THEN
                 IF(NPVKCB.EQ.0) THEN
                    CALL U1DREL(VKCB(NSTRT),ANAME(5),NDSLAY,K,IN,&
                          IOUT)
                 ELSE
                    READ(IN,*) LAYFLG(5,K)
                    WRITE(IOUT,121) ANAME(5),K,LAYFLG(5,K)
                    CALL UPARARRSUB1(VKCB(NSTRT),NDSLAY,1,KK,&
                      'VKCB',IOUT,ANAME(5),LAYFLG(5,KK))
                    IF(NOPCHK.EQ.0) CALL UPARARRCK(BUFF,IBOUND(NSTRT),IOUT,K,&
                     NDSLAY,1,1,IUNSTR,'VKCB')
                 END IF
              END IF
        !
        !3G-----READ WETDRY CODES IF WETTING CAPABILITY HAS BEEN INVOKED
        !3G-----(LAYWET NOT 0).
              IF(LAYWET(K).NE.0) THEN
                 CALL U1DREL(WETDRY(NSTRT),ANAME(8),NDSLAY,K,IN,IOUT)
              END IF
        !
        !---------------------------------------------------------
              if(LAYCON(k).NE.5) go to 300
              IF(ITABRICH.EQ.0) THEN
        !3H-----READ alpha, beta, brook
              CALL U1DREL(alpha(NSTRT),ANAME(12),NDSLAY,K,IN,IOUT)
              CALL U1DREL(beta(NSTRT),ANAME(13),NDSLAY,K,IN,IOUT)
              CALL U1DREL(sr(NSTRT),ANAME(14),NDSLAY,K,IN,IOUT)
              CALL U1DREL(brook(NSTRT),ANAME(15),NDSLAY,K,IN,IOUT)
              IF(IBPN.GT.0)THEN
                CALL U1DREL(bP(NSTRT),ANAME(15),NDSLAY,K,IN,IOUT)
              ENDIF
        !6------read tabular input for retention and relative permeability curves
              ELSE


              ENDIF
          300 CONTINUE
        !---------------------------------------------------------------
          200 CONTINUE
        !---------------------------------------------------------------
              IF(IKCFLAG.NE.0)THEN
        !4--------READ EFFECTIVE SATURATED K OF CONNECTION
                ALLOCATE(TEMP(NJAS))
                CALL U1DRELNJA(TEMP(1),IATMP,ANAME(10),NJATMP,IN,IOUT,IDSYMRD)
                IF(IKCFLAG.EQ.1)THEN
                  DO IIS=1,NJAS
                    PGF(IIS) = PGF(IIS) * TEMP(IIS)
                  ENDDO
        !-----------INCLUDE THICKNESS TERM
                  DO N=1,NODES
                    THICK1 = TOP(N) - BOT(N)
        !-----------GO OVER CONNECTIONS OF NODE N AND FILL FOR UPPER SYMMETRIC PART
                    DO II = IA(N)+1,IA(N+1)-1
                      JJ = JA(II)
                      IF(JJ.GE.N.AND.JJ.LE.NODES)THEN
                        IIS = JAS(II)
        !                IF(IVC(IIS).NE.0) CYCLE ! DO ONLY FOR HORIZONTAL CONNECTION
                        THICK2 = TOP(JJ) - BOT(JJ)
                        THICK = 0.5 * (THICK1 + THICK2)
                        PGF(IIS) = PGF(IIS) * THICK
                      ENDIF
                    ENDDO
                  ENDDO
                ELSE
                  DO IIS=1,NJAS
                    PGF(IIS) = TEMP(IIS)
                  ENDDO
                ENDIF
        !-------SET HK FOR THEIM SOLUTION CONNECTION
                  DO N=1,NODES
                    THICK = TOP(N) - BOT(N)
                    AKN = 0.0
                    IKN = 0
        !-----------GO OVER CONNECTIONS OF NODE N AND FILL FOR UPPER SYMMETRIC PART
                    DO II = IA(N)+1,IA(N+1)-1
                      JJ = JA(II)
                      IF(JJ.LE.NODES)THEN
                        IIS = JAS(II)
                        IF(IVC(IIS).EQ.0)THEN
                          IF(IKCFLAG.EQ.1) THEN
                           AKN = AKN + PGF(IIS) / THICK
                          ELSE
                            AKN = AKN + PGF(IIS) / THICK * CL1(IIS)
                          ENDIF
                        ENDIF
                        IKN = IKN + 1
                      ENDIF
                    ENDDO
                    IF(IKN.GT.0) THEN
                      HK(N) = AKN / IKN
                    ENDIF
                  ENDDO
                DEALLOCATE(TEMP)
              ENDIF
        !
        !5------RETURN
              RETURN
    END SUBROUTINE SGWF2LPFU1G

    SUBROUTINE UPARARRSUB1(ZZ,NCOL,NROW,ILAY,PTYP,IOUT,ANAME,IPF)
        !     ******************************************************************
        !     Substitute parameter-based values into a 2-D array based on a
        !     parameter type.
        !     ******************************************************************
        !
        !        SPECIFICATIONS:
        !     ------------------------------------------------------------------
              USE PARAMMODULE
              DIMENSION ZZ(NCOL,NROW)
              CHARACTER*(*) PTYP
              CHARACTER*24 ANAME
              
              integer :: ncol, ilay, nrow, iout, ipf, init, ip, ii, nsub
              real :: zz
        !     ------------------------------------------------------------------
        !
        !1------Set initialization flag to cause USUB2D to initialze ZZ to 0.
        !1------Write a header above the list of parameters that define ZZ.
              INIT=1
              WRITE(IOUT,11) ANAME
           11 FORMAT(1X,/,1X,A,' is defined by the following parameters:')
        !
        !2------Loop through each parameter looking for the specified file type.
              DO 100 IP=1,IPSUM
        !
        !2A-----Stop looping if the end of the parameter list is found.
              IF(PARNAM(IP).EQ.' ') GO TO 200
        !
        !2B-----Check for the specified parameter type.
              IF(PARTYP(IP).EQ.PTYP) THEN
        !
        !2C-----Loop through each cluster definition for layers that match the
        !2C-----specified layer.
                 II=IP
                 CALL USUB2D(ZZ,NCOL,NROW,II,ILAY,INIT,NSUB)
                 INIT=0
                 IF(NSUB.GT.0) WRITE(IOUT,47) PARNAM(IP)
           47    FORMAT(1X,A)
              END IF
          100 CONTINUE
        !
        !3------PRINT THE ARRAY.
          200 CALL ULAPRWC(ZZ,NCOL,NROW,ILAY,IOUT,IPF,ANAME)
        !
        !4------Return.
              RETURN
    END SUBROUTINE UPARARRSUB1

    SUBROUTINE UPARARRCK(BUFF,IBOUND,IOUT,LAY,NCOL,NLAY,NROW,&
       IUNSTR,PTYP)
!     ******************************************************************
!     CHECK FOR COMPLETE DEFINITION OF ONE LAYER OF CELLS BY ARRAY
!     PARAMETERS OF A GIVEN TYPE.
!     ******************************************************************
!        SPECIFICATIONS:
!     ------------------------------------------------------------------
      USE PARAMMODULE
      INTEGER IBOUND(NCOL,NROW,NLAY)
      REAL BUFF(NCOL,NROW)
      CHARACTER*4 PTYP
      
      integer :: lay, iout, iunstr, i, j, ip, ic, iza, ii
      integer :: nlay, ncol, nrow, izi, iz, ilay, ierr, kk, ij, jj
!     ------------------------------------------------------------------
!
!1------Make sure that the parameter type is non-blank.
      IF (PTYP.EQ.' ') THEN
        WRITE (IOUT,500)
  500   FORMAT(1X,'ERROR: BLANK PARAMETER TYPE -- STOP EXECUTION',&
            ' (UPARARRCK)')
        CALL USTOP(' ')
      ENDIF
!
!2------Initialize BUFF to 0.
      DO 20 I = 1, NROW
        DO 10 J = 1, NCOL
          BUFF(J,I) = 0.0
   10   CONTINUE
   20 CONTINUE
!
!3------Loop through parameters to find matching parameter type.
!3------Increment BUFF for each cell where a parameter of the specified
!3------type applies.
      DO 100 IP = 1, IPSUM
        IF (PARTYP(IP).EQ.PTYP) THEN
!
!3A-----Loop through clusters associated with this parameter.
          DO 80 IC = IPLOC(1,IP), IPLOC(2,IP)
            IF (IPCLST(1,IC).EQ.LAY) THEN
              IZA = IPCLST(3,IC)
              DO 60 I = 1, NROW
                DO 50 J = 1,NCOL
                  IF (IZA.GT.0) THEN
!
!3B-----Loop through zones listed for this cluster.
                    DO 40 IZI = 5, IPCLST(4,IC)
                      IZ = IPCLST(IZI,IC)
                      IF (IZ.EQ.IZON(J,I,IZA)) THEN
                        BUFF(J,I) = BUFF(J,I) + 1.0
                      ENDIF
   40               CONTINUE
                  ELSE
!
!3C-----Zones do not apply to this cluster -- apply to all cells.
                    BUFF(J,I) = BUFF(J,I) + 1.0
                  ENDIF
   50           CONTINUE
   60         CONTINUE
            ENDIF
   80     CONTINUE
        ENDIF
  100 CONTINUE
!
!4------Identify any active cells where BUFF is equal to zero, which
!4------indicates cells that are not defined by any parameter of the
!4------specified type applies.
      ILAY = LAY
      IF(IUNSTR.EQ.1) ILAY = 1
      IERR = 0
      DO 140 I = 1, NROW
        DO 120 J = 1, NCOL
          IF (IBOUND(J,I,ILAY).NE.0) THEN
            IF (BUFF(J,I).EQ.0.0)THEN
              IF(IUNSTR.NE.0)THEN
                KK = LAY
                IJ = J - (KK-1)*NCOL*NROW
                II = (IJ-1)/NCOL + 1
                JJ = IJ - (II-1)*NCOL
                WRITE (IOUT,510) II,JJ,KK,PTYP
  510           FORMAT(1X,'ROW: ',I5,', COLUMN: ',I5,' IN LAYER ',I3,&
             ' NOT DEFINED FOR PARAMETER TYPE ',A)
              ELSE
                WRITE (IOUT,511) J,LAY,PTYP
  511           FORMAT(1X,'NODE: ',I9,', IN LAYER: ',I6,&
             ' NOT DEFINED FOR PARAMETER TYPE ',A)
              ENDIF
              IERR = IERR + 1
            ENDIF
          ENDIF
  120   CONTINUE
  140 CONTINUE
!
!5------IF any active cells were found with undefined values, write an
!5------error message and stop.
      IF (IERR.GT.0) THEN
        WRITE (IOUT,520)
  520   FORMAT(/,1X,'PARAMETER DEFINITIONS INCOMPLETE -- STOP',&
            ' EXECUTION (UPARARRCK)')
        CALL USTOP(' ')
      ENDIF
!
!6------Return.
      RETURN
      END SUBROUTINE UPARARRCK
      
      SUBROUTINE U1DRELNJA(ARRAY,IAG,ANAME,NJAG,IN,IOUT,IDSYMRD)
!     ******************************************************************
!     READ SYMMETRIC SUBSURFACE PROPERTY ARRAY FOR UNSTRUCTURED GRIDS
!     ******************************************************************
!
!        SPECIFICATIONS:
!     ------------------------------------------------------------------
      USE GLOBAL,      ONLY:IA,JA,JAS,NJAS,NODES,NJA
      CHARACTER*24 ANAME
      REAL, DIMENSION(:),ALLOCATABLE  ::TEMP,TEMPU
      DIMENSION ARRAY(NJAS)
      INTEGER IAG(NODES+1)
      
      integer :: iout, idsymrd, njag
      real :: array
      integer :: in, k, njags, n, iic, ii, jj, iis, iagnum, iig
      
!     ------------------------------------------------------------------
      K = 0
      IF(IDSYMRD.EQ.1)THEN
!1------READ SYMMETRIC DATA
        IF(NJA.EQ.NJAG)THEN  !
!1A-------NO CLN OR GNC NODES SO FILL ARRAY DIRECTLY AND RETURN
          CALL U1DREL(ARRAY,ANAME,NJAS,K,IN,IOUT)
          RETURN
        ENDIF
!
        NJAGS = (NJAG - NODES) / 2
        ALLOCATE(TEMP(NJAGS))
!1-------READ SYMMETRIC DATA IN TEMP LOCATION FOR SUBSURFACE NODES
        CALL U1DREL(TEMP,ANAME,NJAGS,K,IN,IOUT)
!1A------FILL INTO UNSYMMETRIC TEMPU LOCATION
        DO N=1,NODES
          IIC = 0  ! ITERATION COUNTER OF GROUNDWATER CONNECTIONS
          DO II = IA(N)+1,IA(N+1)-1
            JJ = JA(II)
            IF(JJ.GT.N .AND. JJ.LE. NODES)THEN
              IIC = IIC + 1
              IIS = JAS(II)
              ARRAY(IIS) = TEMP(IIC)
            ENDIF
          ENDDO
        ENDDO
        DEALLOCATE(TEMP)
        RETURN
      ELSE
!2------READ UNSYMMETRIC DATA IN TEMPU LOCATION AND TRANSFER
        ALLOCATE(TEMPU(NJAG))
        CALL U1DREL(TEMPU(1),ANAME,NJAG,K,IN,IOUT)
      ENDIF
!3------COPY ONLY UPPER TRIANGLE OF TEMPU INTO SYMMETRIC ARRAY FOR SUBSURFACE NODES
      DO N=1,NODES
        IAGNUM = IAG(N+1)-IAG(N)
        IIC = 0 ! ITERATION COUNTER OF II LOOP
!
         DO II = IA(N), IA(N)+IAGNUM-1
          IIC = IIC + 1
          IIG = IAG(N)+IIC-1
          JJ = JA(II)
          IF(JJ.LE.N) CYCLE
          IIS = JAS(II)
          ARRAY(IIS) = TEMPU(IIG)
        ENDDO
      ENDDO
      DEALLOCATE(TEMPU)
!
!4------RETURN
      RETURN
      END SUBROUTINE U1DRELNJA
      
      SUBROUTINE USUB2D(ZZ,NCOL,NROW,IP,ILAY,INIT,NSUB)
!     ******************************************************************
!     Substitute values for a single parameter into a 2-D array.
!     ******************************************************************
!
!        SPECIFICATIONS:
!     ------------------------------------------------------------------
      USE PARAMMODULE
      real :: ZZ(NCOL,NROW)
      
      integer :: init, nsub, ncol, ilay, ip, nrow, i,j, icstart, icstop
      real :: aa
      integer :: numinst, nclu, ni, ic, mlt, iz, jj
      
!     ------------------------------------------------------------------
!
!1------Define constants.
      ZERO=0.0
!
!2------Initialize the array if INIT is not 0.
      IF(INIT.NE.0) THEN
        DO 10 I=1,NROW
          DO 5 J=1,NCOL
            ZZ(J,I)=ZERO
    5     CONTINUE
   10   CONTINUE
      END IF
!
!3------Identify clusters, which depends on the instance if the
!3------parameter is a time varying parameter.
      ICSTART=IPLOC(1,IP)
      ICSTOP=IPLOC(2,IP)
      NUMINST=IPLOC(3,IP)
      IF(NUMINST.GT.1) THEN
!       Select correct instance
        NCLU=(ICSTOP-ICSTART+1)/NUMINST
        NI=IACTIVE(IP)
        ICSTART=ICSTART+(NI-1)*NCLU
        ICSTOP=ICSTART+NCLU-1
      ENDIF
!
!4------Loop through each cluster definition for layers that match the
!4------specified layer.
      NSUB=0
      DO 80 IC=ICSTART,ICSTOP
!
!4A-----Check if the cluster layer matches the specified layer
        IF(IPCLST(1,IC).EQ.ILAY) THEN
!
!4B-----The parameter layer matches the specified layer.  Look at zone
!4B-----value to determine which cells to substitute. Also identify the
!4B-----multiplier array.
          MLT=IPCLST(2,IC)
          AA=1.
          IZ=IPCLST(3,IC)
          IF(IZ.GT.0) THEN
!
!4C-----IZ>0. Loop through all cells.  If the value in the zone array
!4C-----is equal to one of the cluster zone values, add the parameter
!4C-----value into the array.
            DO 50 I=1,NROW
              DO 40 J=1,NCOL
                DO 30 JJ=5,IPCLST(4,IC)
                  IF(IZON(J,I,IZ).EQ.IPCLST(JJ,IC)) THEN
                    IF(MLT.GT.0) AA=RMLT(J,I,MLT)
                    ZZ(J,I)=ZZ(J,I)+AA*B(IP)
                    NSUB=NSUB+1
                  END IF
   30           CONTINUE
   40         CONTINUE
   50       CONTINUE
          ELSE
!
!4D-----IZ is 0.  Loop through all cells adding the parameter value into
!4D-----the array.
            DO 70 I=1,NROW
              DO 60 J=1,NCOL
                IF(MLT.GT.0) AA=RMLT(J,I,MLT)
                ZZ(J,I)=ZZ(J,I)+AA*B(IP)
   60         CONTINUE
   70       CONTINUE
            NSUB=NSUB+NCOL*NROW
          END IF
        END IF
   80 CONTINUE
!
!5------Return.
      RETURN
      END SUBROUTINE USUB2D
      
      SUBROUTINE ULAPRWC(A,NCOL,NROW,ILAY,IOUT,IPRN,ANAME)
!     ******************************************************************
!     WRITE A TWO-DIMENSIONAL REAL ARRAY.  IF THE ARRAY IS CONSTANT,
!     PRINT JUST THE CONSTANT VALUE.  IF THE ARRAY IS NOT CONSTANT, CALL
!     ULAPRW TO PRINT IT.
!     ******************************************************************
!
!        SPECIFICATIONS:
!     ------------------------------------------------------------------
      real :: A(NCOL,NROW)
      CHARACTER*(*) ANAME
      
      integer :: nrow, iout, iprn, ilay, ncol, i,j
      real :: tmp
      
!     ------------------------------------------------------------------
!
!  Check to see if entire array is a constant.
      TMP=A(1,1)
      DO 300 I=1,NROW
      DO 300 J=1,NCOL
      IF(A(J,I).NE.TMP) GO TO 400
  300 CONTINUE
      IF(ILAY.GT.0) THEN
         WRITE(IOUT,302) ANAME,TMP,ILAY
  302    FORMAT(1X,/1X,A,' =',1P,G14.6,' FOR LAYER',I4)
      ELSE IF(ILAY.EQ.0) THEN
         WRITE(IOUT,303) ANAME,TMP
  303    FORMAT(1X,/1X,A,' =',1P,G14.6)
      ELSE
         WRITE(IOUT,304) ANAME,TMP
  304    FORMAT(1X,/1X,A,' =',1P,G14.6,' FOR CROSS SECTION')
      END IF
      RETURN
!
!  Print the array.
  400 IF(ILAY.GT.0) THEN
         WRITE(IOUT,494) ANAME,ILAY
  494    FORMAT(1X,//11X,A,' FOR LAYER',I4)
      ELSE IF(ILAY.EQ.0) THEN
         WRITE(IOUT,495) ANAME
  495    FORMAT(1X,//11X,A)
      ELSE
         WRITE(IOUT,496) ANAME
  496    FORMAT(1X,//11X,A,' FOR CROSS SECTION')
      END IF
      IF(IPRN.GE.0) CALL ULAPRW(A,ANAME,0,0,NCOL,NROW,0,IPRN,IOUT)
!
      RETURN
      END SUBROUTINE ULAPRWC
      
      SUBROUTINE ULAPRW(BUF,TEXT,KSTP,KPER,NCOL,NROW,ILAY,IPRN,IOUT)
!     ******************************************************************
!     PRINT 1 LAYER ARRAY
!     ******************************************************************
!
!        SPECIFICATIONS:
!     ------------------------------------------------------------------
      CHARACTER*16 TEXT
      real :: BUF(NCOL,NROW)
      
      integer :: kper, ilay, iout, iprn, kstp, ip, i, j, ncol, nrow
!     ------------------------------------------------------------------
!
!1------PRINT A HEADER DEPENDING ON ILAY
      IF(ILAY.GT.0) THEN
         WRITE(IOUT,1) TEXT,ILAY,KSTP,KPER
    1    FORMAT('1',/2X,A,' IN LAYER ',I3,' AT END OF TIME STEP ',I4,&
          ' IN STRESS PERIOD ',I4/2X,75('-'))
      ELSE IF(ILAY.LT.0) THEN
         WRITE(IOUT,2) TEXT,KSTP,KPER
    2    FORMAT('1',/1X,A,' FOR CROSS SECTION AT END OF TIME STEP',I4,&
          ' IN STRESS PERIOD ',I4/1X,79('-'))
      END IF
!
!2------MAKE SURE THE FORMAT CODE (IP OR IPRN) IS
!2------BETWEEN 1 AND 21.
    5 IP=IPRN
      IF(IP.LT.1 .OR. IP.GT.21) IP=12
!
!3------CALL THE UTILITY MODULE UCOLNO TO PRINT COLUMN NUMBERS.
      IF(IP.EQ.1) CALL UCOLNO(1,NCOL,0,11,11,IOUT)
      IF(IP.EQ.2) CALL UCOLNO(1,NCOL,0,9,14,IOUT)
      IF(IP.GE.3 .AND. IP.LE.6) CALL UCOLNO(1,NCOL,3,15,8,IOUT)
      IF(IP.GE.7 .AND. IP.LE.11) CALL UCOLNO(1,NCOL,3,20,6,IOUT)
      IF(IP.EQ.12) CALL UCOLNO(1,NCOL,0,10,12,IOUT)
      IF(IP.GE.13 .AND. IP.LE.18) CALL UCOLNO(1,NCOL,3,10,7,IOUT)
      IF(IP.EQ.19) CALL UCOLNO(1,NCOL,0,5,13,IOUT)
      IF(IP.EQ.20) CALL UCOLNO(1,NCOL,0,6,12,IOUT)
      IF(IP.EQ.21) CALL UCOLNO(1,NCOL,0,7,10,IOUT)
!
!4------LOOP THROUGH THE ROWS PRINTING EACH ONE IN ITS ENTIRETY.
      DO 1000 I=1,NROW
      GO TO(10,20,30,40,50,60,70,80,90,100,110,120,130,140,150,160,170,&
           180,190,200,210), IP
!
!------------ FORMAT 11G10.3
   10 WRITE(IOUT,11) I,(BUF(J,I),J=1,NCOL)
   11 FORMAT(1X,I3,2X,1PG10.3,10(1X,G10.3):/(5X,11(1X,G10.3)))
      GO TO 1000
!
!------------ FORMAT 9G13.6
   20 WRITE(IOUT,21) I,(BUF(J,I),J=1,NCOL)
   21 FORMAT(1X,I3,2X,1PG13.6,8(1X,G13.6):/(5X,9(1X,G13.6)))
      GO TO 1000
!
!------------ FORMAT 15F7.1
   30 WRITE(IOUT,31) I,(BUF(J,I),J=1,NCOL)
   31 FORMAT(1X,I3,1X,15(1X,F7.1):/(5X,15(1X,F7.1)))
      GO TO 1000
!
!------------ FORMAT 15F7.2
   40 WRITE(IOUT,41) I,(BUF(J,I),J=1,NCOL)
   41 FORMAT(1X,I3,1X,15(1X,F7.2):/(5X,15(1X,F7.2)))
      GO TO 1000
!
!------------ FORMAT 15F7.3
   50 WRITE(IOUT,51) I,(BUF(J,I),J=1,NCOL)
   51 FORMAT(1X,I3,1X,15(1X,F7.3):/(5X,15(1X,F7.3)))
      GO TO 1000
!
!------------ FORMAT 15F7.4
   60 WRITE(IOUT,61) I,(BUF(J,I),J=1,NCOL)
   61 FORMAT(1X,I3,1X,15(1X,F7.4):/(5X,15(1X,F7.4)))
      GO TO 1000
!
!------------ FORMAT 20F5.0
   70 WRITE(IOUT,71) I,(BUF(J,I),J=1,NCOL)
   71 FORMAT(1X,I3,1X,20(1X,F5.0):/(5X,20(1X,F5.0)))
      GO TO 1000
!
!------------ FORMAT 20F5.1
   80 WRITE(IOUT,81) I,(BUF(J,I),J=1,NCOL)
   81 FORMAT(1X,I3,1X,20(1X,F5.1):/(5X,20(1X,F5.1)))
      GO TO 1000
!
!------------ FORMAT 20F5.2
   90 WRITE(IOUT,91) I,(BUF(J,I),J=1,NCOL)
   91 FORMAT(1X,I3,1X,20(1X,F5.2):/(5X,20(1X,F5.2)))
      GO TO 1000
!
!------------ FORMAT 20F5.3
  100 WRITE(IOUT,101) I,(BUF(J,I),J=1,NCOL)
  101 FORMAT(1X,I3,1X,20(1X,F5.3):/(5X,20(1X,F5.3)))
      GO TO 1000
!
!------------ FORMAT 20F5.4
  110 WRITE(IOUT,111) I,(BUF(J,I),J=1,NCOL)
  111 FORMAT(1X,I3,1X,20(1X,F5.4):/(5X,20(1X,F5.4)))
      GO TO 1000
!
!------------ FORMAT 10G11.4
  120 WRITE(IOUT,121) I,(BUF(J,I),J=1,NCOL)
  121 FORMAT(1X,I3,2X,1PG11.4,9(1X,G11.4):/(5X,10(1X,G11.4)))
      GO TO 1000
!
!------------ FORMAT 10F6.0
  130 WRITE(IOUT,131) I,(BUF(J,I),J=1,NCOL)
  131 FORMAT(1X,I3,1X,10(1X,F6.0):/(5X,10(1X,F6.0)))
      GO TO 1000
!
!------------ FORMAT 10F6.1
  140 WRITE(IOUT,141) I,(BUF(J,I),J=1,NCOL)
  141 FORMAT(1X,I3,1X,10(1X,F6.1):/(5X,10(1X,F6.1)))
      GO TO 1000
!
!------------ FORMAT 10F6.2
  150 WRITE(IOUT,151) I,(BUF(J,I),J=1,NCOL)
  151 FORMAT(1X,I3,1X,10(1X,F6.2):/(5X,10(1X,F6.2)))
      GO TO 1000
!
!------------ FORMAT 10F6.3
  160 WRITE(IOUT,161) I,(BUF(J,I),J=1,NCOL)
  161 FORMAT(1X,I3,1X,10(1X,F6.3):/(5X,10(1X,F6.3)))
      GO TO 1000
!
!------------ FORMAT 10F6.4
  170 WRITE(IOUT,171) I,(BUF(J,I),J=1,NCOL)
  171 FORMAT(1X,I3,1X,10(1X,F6.4):/(5X,10(1X,F6.4)))
      GO TO 1000
!
!------------ FORMAT 10F6.5
  180 WRITE(IOUT,181) I,(BUF(J,I),J=1,NCOL)
  181 FORMAT(1X,I3,1X,10(1X,F6.5):/(5X,10(1X,F6.5)))
      GO TO 1000
!
!------------FORMAT 5G12.5
  190 WRITE(IOUT,191) I,(BUF(J,I),J=1,NCOL)
  191 FORMAT(1X,I3,2X,1PG12.5,4(1X,G12.5):/(5X,5(1X,G12.5)))
      GO TO 1000
!
!------------FORMAT 6G11.4
  200 WRITE(IOUT,201) I,(BUF(J,I),J=1,NCOL)
  201 FORMAT(1X,I3,2X,1PG11.4,5(1X,G11.4):/(5X,6(1X,G11.4)))
      GO TO 1000
!
!------------FORMAT 7G9.2
  210 WRITE(IOUT,211) I,(BUF(J,I),J=1,NCOL)
  211 FORMAT(1X,I3,2X,1PG9.2,6(1X,G9.2):/(5X,7(1X,G9.2)))
!
 1000 CONTINUE
!
!5------RETURN
      RETURN
      END SUBROUTINE ULAPRW

      SUBROUTINE UCOLNO(NLBL1,NLBL2,NSPACE,NCPL,NDIG,IOUT)
!     ******************************************************************
!     OUTPUT COLUMN NUMBERS ABOVE A MATRIX PRINTOUT
!        NLBL1 IS THE START COLUMN LABEL (NUMBER)
!        NLBL2 IS THE STOP COLUMN LABEL (NUMBER)
!        NSPACE IS NUMBER OF BLANK SPACES TO LEAVE AT START OF LINE
!        NCPL IS NUMBER OF COLUMN NUMBERS PER LINE
!        NDIG IS NUMBER OF CHARACTERS IN EACH COLUMN FIELD
!        IOUT IS OUTPUT CHANNEL
!     ******************************************************************
!
!        SPECIFICATIONS:
!     ------------------------------------------------------------------
      CHARACTER*1 DOT,SPACE,DG,BF
      DIMENSION BF(130),DG(10)
!
      DATA DG(1),DG(2),DG(3),DG(4),DG(5),DG(6),DG(7),DG(8),DG(9),DG(10)/&
              '0','1','2','3','4','5','6','7','8','9'/
      DATA DOT,SPACE/'.',' '/
      
      integer :: ndig, iout, nspace, ncpl, nlbl1, nlbl, n, ntot, j1, j2
      integer nlbl2, nwrap, i, nbf, j, i1, i2, i3, i4 
!     ------------------------------------------------------------------
!
!1------CALCULATE # OF COLUMNS TO BE PRINTED (NLBL), WIDTH
!1------OF A LINE (NTOT), NUMBER OF LINES (NWRAP).
      WRITE(IOUT,1)
    1 FORMAT(1X)
      NLBL=NLBL2-NLBL1+1
      N=NLBL
      IF(NLBL.GT.NCPL) N=NCPL
      NTOT=NSPACE+N*NDIG
      IF(NTOT.GT.130) GO TO 50
      NWRAP=(NLBL-1)/NCPL + 1
      J1=NLBL1-NCPL
      J2=NLBL1-1
!
!2------BUILD AND PRINT EACH LINE
      DO 40 N=1,NWRAP
!
!3------CLEAR THE BUFFER (BF).
      DO 20 I=1,130
      BF(I)=SPACE
   20 CONTINUE
      NBF=NSPACE
!
!4------DETERMINE FIRST (J1) AND LAST (J2) COLUMN # FOR THIS LINE.
      J1=J1+NCPL
      J2=J2+NCPL
      IF(J2.GT.NLBL2) J2=NLBL2
!
!5------LOAD THE COLUMN #'S INTO THE BUFFER.
      DO 30 J=J1,J2
      NBF=NBF+NDIG
      I2=J/10
      I1=J-I2*10+1
      BF(NBF)=DG(I1)
      IF(I2.EQ.0) GO TO 30
      I3=I2/10
      I2=I2-I3*10+1
      BF(NBF-1)=DG(I2)
      IF(I3.EQ.0) GO TO 30
      I4=I3/10
      I3=I3-I4*10+1
      BF(NBF-2)=DG(I3)
      IF(I4.EQ.0) GO TO 30
      IF(I4.GT.9) THEN
!5A-----If more than 4 digits, use "X" for 4th digit.
         BF(NBF-3)='X'
      ELSE
         BF(NBF-3)=DG(I4+1)
      END IF
   30 CONTINUE
!
!6------PRINT THE CONTENTS OF THE BUFFER (I.E. PRINT THE LINE).
      WRITE(IOUT,31) (BF(I),I=1,NBF)
   31 FORMAT(1X,130A1)
!
   40 CONTINUE
!
!7------PRINT A LINE OF DOTS (FOR ESTHETIC PURPOSES ONLY).
   50 NTOT=NTOT
      IF(NTOT.GT.130) NTOT=130
      WRITE(IOUT,51) (DOT,I=1,NTOT)
   51 FORMAT(1X,130A1)
!
!8------RETURN
      RETURN
      END SUBROUTINE UCOLNO
      
      SUBROUTINE UPARLSTAL(IN,IOUT,LINE,NP,MXL)
!     ******************************************************************
!     Setup list parameter definition for a package
!     ******************************************************************
!
!        SPECIFICATIONS:
!     ------------------------------------------------------------------
      USE PARAMMODULE
      CHARACTER*(*) LINE
      
      integer ::  mxl, in, np, iout, lloc, istart
      real :: r
      integer :: n, istop
!     ------------------------------------------------------------------
!
!1------Decode PARAMETER definitions if they exist
      NP=0
      MXL=0
      LLOC=1
      CALL URWORD(LINE,LLOC,ISTART,ISTOP,1,N,R,IOUT,IN)
      IF(LINE(ISTART:ISTOP).EQ.'PARAMETER') THEN
         CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,NP,R,IOUT,IN)
         IF(NP.LT.0) NP=0
         CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,MXL,R,IOUT,IN)
         IF(MXL.LT.0) MXL=0
         WRITE(IOUT,31) NP,MXL
   31    FORMAT(1X,I10,' Named Parameters     ',I10,' List entries')
         READ(IN,'(A)') LINE
      ELSE
         WRITE(IOUT,'(A)') ' No named parameters'
      END IF
!
!2------Return.
      RETURN
      END SUBROUTINE UPARLSTAL

               !
!    SUBROUTINE SAT_THIK(N,HD,TOTTHICK,BBOT,THCK,K,TTOP)
!        !     ******************************************************************
!        !     COMPUTE Kr FOR LAYCON OF 4 AND 5
!        !     ******************************************************************
!        !
!        !      SPECIFICATIONS:
!        !     ------------------------------------------------------------------
!                USE GLOBAL,     ONLY:ISSFLG,NODES,iunsat,HNEW
!        !s      USE DDFMODULE, ONLY: ZETASWI,ISHARP
!                USE GWFBCFMODULE,ONLY:alpha,beta,sr,BP,IBPN,LAYCON,IDRY,&
!                retcrvs,nutabrows,nuzones,iuzontab,ITABRICH,INTRICH
!                DOUBLE PRECISION THCK,HD,BBOT,TTOP,X,S,V,COF1,COF2,FACTOR1,FACTOR2&
!                ,eps,acof,y,TOTTHICK,seff,gamma, tabS,pc, topS, botS,cellS,bott,&
!                mul,div
!                
!                integer :: n,k,method,izon, iv
!                real :: thickfact, bpn
!                
!        !     ------------------------------------------------------------------
!        !
!        ! ----DEPENDING ON THE METHOD, FIND THE SATURATED THICKNESS
!        !
!        !--------------------------------------------------------
!                ZERO=0.
!                METHOD = 3  !  METHOD 3 IS THE UNCONFINED SATURATED THICKNESS CURVE OF NWT
!        !1-------USE METHODS 4, 5 AND 6 FOR RICHARDS EQUATION
!                IF(N.LE.NODES)THEN
!                IF(LAYCON(K).EQ.5) THEN ! METHOD 4 USES VAN GENUCHTEN AND BROOKS-COREY FUNCTIONS
!                    METHOD = 4
!                    IF(ITABRICH.NE.0)THEN !METHOD 5 USES TABULAR INPUT
!                    METHOD = 5
!                    IF(INTRICH.NE.0)THEN  !METHOD 6 USES INTEGRATED FUNCTIONS (DISCARD THIS)
!                        METHOD = 6
!                    ENDIF
!                    ENDIF
!                ENDIF
!                ENDIF
!        !-------------------------------------------------------------------------
!        !2--------ADJUST DIMENSIONS FOR FRESHWATER AND SALTWATER EQUATIONS OF SWI
!        !s      IF(INDDF. NE. 0) THEN
!        !2A-----RECOMPUTE ZETA
!        !s        IF(ISHARP.NE.0) CALL ZETANODE (N, ZETA,HD)
!        !2B--------SELECT EQUATION
!        !s        IEQSWI = 1 ! RIGHT NOW HARDWIRE FOR ONLY FRESHWATER ****************
!        !s        IF(ISHARP.NE.0 .AND. IEQSWI .EQ. 1) THEN
!        !s          THICKKP = TOTTHICK
!        !2C--------- EQUATION IS FRESHWATER EQUATION
!        !s          IF(ZETA. GT. BBOT) THEN
!        !s           BBOT = ZETA
!        !s           TOTTHICK = TTOP - BBOT
!        !s           IF(TOTTHICK. LE. 0.0) THEN
!        !s              THCK = 0.0
!        !s              TOTTHICK = THICKKP
!        !s              RETURN
!        !s            ENDIF
!        !s          ENDIF
!        !s        ELSEIF(ISHARP. NE.0. AND. IEQSWI. EQ. 2) THEN
!        !2D--------- EQUATION IS SALTWATER EQUATION
!        !s          IF(HNEW(N). LT. TTOP) THEN         ! THIS SHOULD BE HNEW OF EQUATION 1 FRESHWATER *********
!        !s            TTOP = HNEW(N)                   ! THIS SHOULD BE HNEW OF EQUATION 1 FRESHWATER *********
!        !s            TOTTHICK = TTOP - BBOT
!        !s            HD = ZETA
!        !s            IF(TOTTHICK. LE. 0.0) THEN
!        !s              THCK = 0.0
!        !s              TOTTHICK = THICKKP
!        !s              RETURN
!        !s            ENDIF
!        !s          ENDIF
!        !s        ENDIF
!        !s      ENDIF
!        !      if(IUNSAT.EQ.1.AND.N.LE.NODES) method = 4
!        !------------------------------------------------------------------------------
!                IF(METHOD.EQ.1)THEN
!        !3-------STRAIGHT LINE, NO SMOOTHING
!                TTOP = BBOT + TOTTHICK
!                IF(HD.GT.TTOP) HD=TTOP
!                THCK = (HD - BBOT) / TOTTHICK
!                IF(THCK.LT.ZERO) THCK=0.0
!                ELSEIF(METHOD.EQ.2)THEN
!        !4-------STRAIGHT LINE WITH CUBIC SMOOTHING
!                Thickfact = 0.01
!                x = (HD-bbot)
!                s = Thickfact*TOTTHICK
!                v = TOTTHICK
!                cof1 = (1.0D0/s**2.0D0)
!                cof2 = (2.0D0/s)
!                factor1 = -cof1*x**3.0D0+cof2*x**2.0D0
!                factor2 = 1.0D0 + cof1*(v-x)**3.0D0-&
!                    cof2*(v-x)**2.0D0
!                THCK = 0.0D0
!                IF ( x.LT.0.0D0 ) THEN
!                    THCK = 0.0D0
!                ELSEIF ( x.LT.s ) THEN
!                    THCK = factor1
!                ELSEIF ( x.LT.v-s ) THEN
!                    THCK = x/v
!                ELSEIF ( x.LT.v ) THEN
!                    THCK = factor2
!                ELSEIF ( x.GE.v ) THEN
!                    THCK = 1.0D0
!                END IF
!                ELSEIF(METHOD.EQ.3)THEN
!        !5-------STRAIGHT LINE WITH PARABOLIC SMOOTHING
!                EPS = 0.01
!                eps = 1.0e-6
!                ACOF = 1.0 / (1.0 - EPS)
!                x = (HD-bbot)/TOTTHICK
!                IF(X.LT.0)THEN
!                    Y = 0.0
!                ELSEIF(X.LT.EPS)THEN
!                    Y = ACOF *0.5/EPS * X**2
!                ELSEIF(X.LT.1.0-EPS)THEN
!                    Y = ACOF * X + (1.0-ACOF)*0.5
!                ELSEIF(X.LT.1.0)THEN
!                    X = 1.0 - X
!                    Y = ACOF *0.5/EPS * X**2
!                    Y = 1.0-Y
!                ELSE
!                    Y = 1.0
!                ENDIF
!                THCK = Y
!                ELSEIF(METHOD.EQ.4)THEN
!        !6---------vanG FUNCTION WITH MODIFICATIONS FOR BUBBLEPT AND FULLYDRY
!                bpn = 0.0
!                if(ibpn.eq.1)then
!                    bpn = bp(N)
!                endif
!                TTOP = BBOT + TOTTHICK
!                pc = 0.5*(ttop+bbot) - (hd-bpn)
!                if(pc.le.0)then
!                    thck = 1.0
!                else
!                    gamma = 1.-1./beta(n)
!                    Seff = (1. + (alpha(n)*pc)**beta(n))**gamma
!                    Seff = 1.0 / Seff
!                    if(idry.eq.0) then
!                    thck = seff * (1-sr(n)) + sr(n)
!                    else
!                    thck = seff
!                    endif
!                endif
!        !------------------------------------
!                ELSEIF(METHOD.EQ.5) THEN
!        !7-------FOR TABULAR INPUT FIND SATURATION FROM TABLE
!                TTOP = BBOT + TOTTHICK
!                pc = 0.5*(ttop+bbot) - hd
!                if(pc.le.0)then
!                    thck = 1.0
!                else
!                    izon = iuzontab(n)
!                    iv = 2  !variable is saturation
!                    thck = tabS(pc,retcrvs,izon,iv,nutabrows,nuzones,ttop)
!                endif
!                ELSEIF(METHOD.EQ.6) THEN
!        !8---------FOR USE WITH TABULAR INPUT OF INTERPOLATED TABLE
!                iv = 2  !variable is saturation
!                izon = iuzontab(n)
!                TTOP = BBOT + TOTTHICK
!                bott = bbot   ! bott changes to water table if below water table in tabS
!                topS = tabS((ttop-hd),retcrvs,izon,iv,nutabrows,nuzones,ttop)
!                botS = tabS((bbot-hd),retcrvs,izon,iv,nutabrows,nuzones,bott)
!                if(hd.lt. bbot) then
!        !9----------water level is below bottom so integrate curve from top to bottom
!                    if((ttop-bott).gt.1.0e-5)then
!                    cellS=-(topS - botS)/(ttop - bott)
!                    if(cellS.lt.1.0e-20) cellS = 1.0e-20
!                    else ! past bottom of table so use last two entries to compute value
!                    mul = retcrvs(iv,nutabrows,izon)-retcrvs(iv,nutabrows-1,izon)
!                    div = retcrvs(1,nutabrows,izon)-retcrvs(1,nutabrows-1,izon)
!                    cellS = mul/div
!                    endif
!                elseif(hd.gt. ttop) then
!        !10----------water level is above top so integrated saturation of cell is 1
!                    cellS = 1.0
!                else
!        !11----------water level is within cell so integrate
!                    cellS = (botS - topS + 1.0 * (hd -bbot))/(ttop - bbot)
!                endif
!                thck = cellS
!        !-------------------------------------------------------------------
!                ENDIF
!        !-------------------------------------------------------------------------
!        !2--------GET BACK TOTTHICK AND MAKE THCK FRACTION OF THAT
!        !s      IF(INDDF. NE. 0) THEN
!        !s        IF(ISHARP.NE.0 ) THEN
!        !2A--------- EQUATION IS FRESHWATER OR SALTWATER
!        !s          THCK = THCK * TOTTHICK/THICKKP
!        !s          TOTTHICK = THICKKP
!        !s        ENDIF
!        !s      ENDIF
!        !      if(IUNSAT.EQ.1.AND.N.LE.NODES) method = 4
!        !------------------------------------------------------------------------------
!                RETURN
!    END SUBROUTINE SAT_THIK
!
!    SUBROUTINE SGWF2LPFU1N
!!     ******************************************************************
!        !     INITIALIZE AND CHECK LPF DATA
!        !     ******************************************************************
!        !
!        !        SPECIFICATIONS:
!        !     ------------------------------------------------------------------
!              USE GLOBAL,      ONLY:NCOL,NROW,NLAY,IBOUND,HNEW,LAYCBD,IUNSTR,&
!                                   IOUT,NODLAY,IA,JA,JAS,IVC
!              USE GWFBCFMODULE,ONLY:WETDRY,laywet,HK,VKCB,VKA
!              
!              real :: hcnv
!              integer :: nndlay, nstrt, n, jprev, jnext, ii, jj, iis, kk, ij, i, j, k
!        !     ------------------------------------------------------------------
!        !
!        !1------DEFINE CONSTANTS.
!              ZERO=0.
!              HCNV=888.88
!        !
!        !2-------INSURE THAT EACH ACTIVE CELL HAS AT LEAST ONE NON-ZERO
!        !2-------TRANSMISSIVE PARAMETER.
!              DO 60 K=1,NLAY
!                NNDLAY = NODLAY(K)
!                NSTRT = NODLAY(K-1)+1
!              IF(LAYWET(K).NE.0) THEN
!        !
!        !3------WETTING IS ACTIVE.
!                DO 40 N=NSTRT,NNDLAY
!                 IF(IBOUND(N).EQ.0 .AND. WETDRY(N).EQ.ZERO)&
!                             GO TO 40
!        !
!        !3A-----CHECK HORIZONTAL HYDRAULIC CONDUCTIVITY (HK).
!                 IF(HK(N).NE.ZERO) GO TO 40
!        !
!                 JPREV=0
!                 JNEXT=0
!                 DO II=IA(N)+1,IA(N+1)-1
!                  JJ=JA(II)
!                  IIS = JAS(II)
!                  IF(JJ.LT.N.AND.IVC(IIS).EQ.1)THEN
!                    JPREV=JJ
!                  ELSEIF(IVC(IIS).EQ.1)THEN
!                    JNEXT=JJ
!                  ENDIF
!                ENDDO
!        !
!        !3B-----CHECK VERTICAL HYDRAULIC CONDUCTIVITY AND CONFINING BED
!        !3B-----VERTICAL HYDRAULIC CONDUCTIVITY.
!                 IF(NLAY.GT.1) THEN
!                    IF(VKA(N).NE.ZERO) THEN
!                       IF(K.NE.NLAY) THEN
!                          IF (VKA(JNEXT).NE.ZERO) THEN
!                             IF(LAYCBD(K).NE.0) THEN
!                                IF(VKCB(N).NE.ZERO) GO TO 40
!                             ELSE
!                                GO TO 40
!                             END IF
!                          END IF
!                       END IF
!                       IF(K.NE.1) THEN
!                          IF (VKA(JPREV).NE.ZERO) THEN
!                             IF (LAYCBD(K-1).NE.0) THEN
!                                IF(VKCB(JPREV).NE.ZERO) GO TO 40
!                             ELSE
!                                GO TO 40
!                             END IF
!                          ENDIF
!                       END IF
!                    END IF
!                 END IF
!        !
!        !3C-----ALL TRANSMISSIVE TERMS ARE ALL 0, SO CONVERT CELL TO NO FLOW.
!                 IBOUND(N)=0
!                 HNEW(N)=HCNV
!                 WETDRY(N)=ZERO
!                 IF(IUNSTR.EQ.0)THEN
!                   KK = (N-1) / (NCOL*NROW) + 1
!                   IJ = N - (KK-1)*NCOL*NROW
!                   I = (IJ-1)/NCOL + 1
!                   J = IJ - (I-1)*NCOL
!                   WRITE(IOUT,43) KK,I,J
!                 ELSE
!                   WRITE(IOUT,43) N
!                 ENDIF
!           40    CONTINUE
!        !
!              ELSE
!        !
!        !4------WETTING IS INACTIVE
!                 DO 50 N=NSTRT,NNDLAY
!                 IF(IBOUND(N).EQ.0) GO TO 50
!        !
!        !4A-----CHECK HORIZONTAL HYDRAULIC CONDUCTIVITY (HK).
!                 IF(HK(N).NE.ZERO) GO TO 50
!        !
!                 JPREV=0
!                 JNEXT=0
!                 DO II=IA(N)+1,IA(N+1)-1
!                  JJ=JA(II)
!                  IIS = JAS(II)
!                  IF(JJ.LT.N.AND.IVC(IIS).EQ.1)THEN
!                    JPREV=JJ
!                  ELSEIF(IVC(IIS).EQ.1)THEN
!                    JNEXT=JJ
!                  ENDIF
!                ENDDO
!        !
!        !4B-----CHECK VERTICAL HYDRAULIC CONDUCTIVITY AND CONFINING BED
!        !4B-----VERTICAL HYDRAULIC CONDUCTIVITY.
!                 IF(NLAY.GT.1) THEN
!                    IF(VKA(N).NE.ZERO) THEN
!                       IF(K.NE.NLAY) THEN
!                          IF (VKA(JNEXT).NE.ZERO) THEN
!                             IF(LAYCBD(K).NE.0) THEN
!                                IF(VKCB(N).NE.ZERO) GO TO 50
!                             ELSE
!                                GO TO 50
!                             END IF
!                          END IF
!                       END IF
!                       IF(K.NE.1) THEN
!                          IF (VKA(JPREV).NE.ZERO) THEN
!                             IF (LAYCBD(K-1).NE.0) THEN
!                                IF(VKCB(JPREV).NE.ZERO) GO TO 50
!                             ELSE
!                                GO TO 50
!                             END IF
!                          ENDIF
!                       END IF
!                    END IF
!                 END IF
!        !
!        !4C-----ALL TRANSMISSIVE TERMS ARE 0, SO CONVERT CELL TO NO FLOW.
!                 IBOUND(N)=0
!                 HNEW(N)=HCNV
!                 WRITE(IOUT,43) N
!           43    FORMAT(1X,'NODE (LAYER,ROW,COL) ',I8,&
!              ' ELIMINATED BECAUSE ALL HYDRAULIC',/,&
!              ' CONDUCTIVITIES TO NODE ARE 0')
!           50    CONTINUE
!              END IF
!           60 CONTINUE
!        !
!        !5------RETURN.
!              RETURN
!    END SUBROUTINE SGWF2LPFU1N

        
   subroutine MUSG_ReadBinary_GWF_HDS_File(Modflow)
    ! borrowed from J. Doherty.
        implicit none
        
        integer :: FNum

        integer :: kstp,kper
        real     :: pertim
        real   :: locTIMOT(100)
        integer :: i
        character*16   :: text
        integer  :: ilay        
        real     :: rtemp  
        integer  :: nstrt,nend,nstrt_kp,nument
        integer :: inode, ii

        
        type (MUSG_Project) Modflow

        !if(.not. Modflow.gwf.have_mesh) call ErrMsg('Must read mesh from GSF file first')
        
        allocate(Modflow.gwf.head(Modflow.gwf.nCell,Modflow.ntime))

        FNum=Modflow.iHDS
        do i=1,Modflow.ntime
          read(FNum,err=9300,end=1000) kstp,kper,pertim,locTIMOT(i),text,nstrt,nend,ilay
          write(*,*) kstp,kper,pertim,locTIMOT(i),text,nstrt,nend,ilay
          
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
              read(FNum,end=9400) (Modflow.gwf.head(inode,i),inode=nstrt,nend)
              !write(*,*) totim,nstrt,Modflow.gwf.head(nstrt,i)
          end if
          !if(ilay==Modflow.gwf.nLay)then
          !    ntime=ntime+1 
          !endif

        end do

9300    continue
9400    continue
1000    continue
        
            continue        
        
    end subroutine MUSG_ReadBinary_GWF_HDS_File
    
    subroutine MUSG_ReadBinary_GWF_DDN_File(Modflow)
    ! borrowed from J. Doherty.
        implicit none

        integer :: Fnum

        integer  :: kstp,kper
        real     :: pertim
        real     :: totim
        integer :: i
        character*16   :: text
        integer  :: ilay        
        real     :: rtemp  
        integer  :: nstrt,nend,nstrt_kp,nument
        integer :: inode, ii

        
        type (MUSG_Project) Modflow

        allocate(Modflow.gwf.sat(Modflow.gwf.nCell,Modflow.ntime))
        
        FNum=Modflow.iDDN
        do i=1,Modflow.ntime
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
              read(FNum,err=9400,end=9400) (Modflow.gwf.sat(inode,i),inode=nstrt,nend)
              !write(*,*) totim,nstrt,Modflow.gwf.sat(nstrt,Modflow.i)
          end if

        end do
9300    continue
9400    continue
1000    continue
    end subroutine MUSG_ReadBinary_GWF_DDN_File

    
    subroutine MUSG_ReadBinary_GWF_CBB_File(Modflow)
    ! borrowed from J. Doherty.
        implicit none

        integer :: Fnum

        integer :: kstp,kper,NVAL,idum,ICODE
        character*16   :: text
        
        
        integer :: KSTP_last
        !real :: rmin
        !real :: rmax
        integer :: ntime, i

        
        type (MUSG_Project) Modflow

        ntime=0
        KSTP_last=0
        FNum=Modflow.iCBB
        do
            read(FNum,err=9300,end=1000) KSTP,KPER,TEXT,NVAL,idum,ICODE
            write(*,*) 'cbb ',KSTP,KPER,TEXT,NVAL,idum,ICODE

            if(KSTP .ne. KSTP_last) then
                KSTP_last=KSTP
                ntime=ntime+1 
            endif

              
            if((NVAL.le.0)) go to 9300
            if(ICODE .gt. 0)then
                if(index(TEXT,'FLOW JA FACE') .ne. 0) then
                    if(ntime .eq.1) allocate(Modflow.gwf.cbb_ja(NVAL,Modflow.ntime))
                    read(FNum,err=9400,end=9400) (Modflow.gwf.cbb_ja(I,ntime),I=1,NVAL)
                    !rmax=-1e20
                    !rmin=1e20
                    !do i=1,nval
                    !    rmax=max(rmax,Modflow.gwf.cbb_ja(I,ntime))
                    !    rmin=min(rmin,Modflow.gwf.cbb_ja(I,ntime))
                    !enddo
                    !write(*,*) text
                    !write(*,*) nval
                    !write(*,*) rmin
                    !write(*,*) rmax
                else 
                    if(ntime .eq.1) THEN
                        if(index(text,'STORAGE') .ne.0) then
	                        allocate(Modflow.gwf.cbb_STORAGE(NVAL,Modflow.ntime))
                        else if(index(text,'CONSTANT HEAD') .ne.0) then
	                        allocate(Modflow.gwf.cbb_CONSTANT_HEAD(NVAL,Modflow.ntime))
                        else if(index(text,'RECHARGE') .ne.0) then
	                        allocate(Modflow.gwf.cbb_RECHARGE(NVAL,Modflow.ntime))
                        else if(index(text,'DRAINS') .ne.0) then
	                        allocate(Modflow.gwf.cbb_DRAINS(NVAL,Modflow.ntime))
                        else if(index(text,'SWF') .ne.0) then
	                        allocate(Modflow.gwf.cbb_SWF(NVAL,Modflow.ntime))
                        else 
                            call ErrMsg(Modflow.FNameCBB(:len_trim(Modflow.FNameCBB))//': TEXT variable '//text//' not currently recognized ')
                            
                        end if
                    endif  

                    if(index(text,'STORAGE') .ne.0) then
                        read(FNum,err=9400,end=9400) (Modflow.gwf.cbb_STORAGE(I,ntime),I=1,NVAL)
                        !rmax=-1e20
                        !rmin=1e20
                        !do i=1,nval
                        !    rmax=max(rmax,Modflow.gwf.cbb_STORAGE(I,ntime))
                        !    rmin=min(rmin,Modflow.gwf.cbb_STORAGE(I,ntime))
                        !enddo
                        !write(*,*) text
                        !write(*,*) nval
                        !write(*,*) rmin
                        !write(*,*) rmax
                    else if(index(text,'CONSTANT HEAD') .ne.0) then
                        read(FNum,err=9400,end=9400) (Modflow.gwf.cbb_CONSTANT_HEAD(I,ntime),I=1,NVAL)

                    else if(index(text,'RECHARGE') .ne.0) then
                        read(FNum,err=9400,end=9400) (Modflow.gwf.cbb_RECHARGE(I,ntime),I=1,NVAL)
                        
                    else if(index(text,'DRAINS') .ne.0) then
                        read(FNum,err=9400,end=9400) (Modflow.gwf.cbb_DRAINS(I,ntime),I=1,NVAL)
                        
                    else if(index(text,'SWF') .ne.0) then
                        read(FNum,err=9400,end=9400) (Modflow.gwf.cbb_SWF(I,ntime),I=1,NVAL)
                        
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
 
    end subroutine MUSG_ReadBinary_GWF_CBB_File

    subroutine MUSG_ReadBinary_CLN_HDS_File(Modflow)
        implicit none
        
        integer :: i
        character*16   :: text
        
        integer :: idum, KSTPREAD,KPERREAD
        DOUBLE PRECISION :: TOTIMREAD,PERTIMREAD
        
        type (MUSG_Project) Modflow

        allocate(Modflow.cln.head(Modflow.cln.nCell,Modflow.ntime))
        
        call msg(' ')
        write(TmpSTR,'(a,i5,a)')'Reading CLN head data from unit ',Modflow.iHDS_CLN,' file '//Modflow.FNameHDS_CLN(:len_trim(Modflow.FNameHDS_CLN))
        call msg(TmpSTR)
        call msg('    Period      Step      Time Cell1Head            Name')

        do i=1,Modflow.ntime
            IDUM = 1
            CALL ULASAVRD(Modflow.cln.head(:,i),TEXT,KSTPREAD,KPERREAD,PERTIMREAD,&
                TOTIMREAD,NCLNNDS,IDUM,IDUM,Modflow.iHDS_CLN)
            WRITE(TmpSTR,'(2(i10), 2(f10.3), a)') KPERREAD,KSTPREAD,TOTIMREAD,Modflow.cln.head(1,i),TEXT
            call msg(TmpSTR)
        end do

    end subroutine MUSG_ReadBinary_CLN_HDS_File
    
    SUBROUTINE ULASAVRD(BUF,TEXT,KSTP,KPER,PERTIM,TOTIM,NCOL,&
                        NROW,ILAY,ICHN)
!     ******************************************************************
!     SAVE 1 LAYER ARRAY ON DISK
!     ******************************************************************
!
!        SPECIFICATIONS:
!     ------------------------------------------------------------------
      CHARACTER*4 TEXT(4)
      DOUBLE PRECISION PERTIM,TOTIM
      real :: BUF(NCOL,NROW)
      REAL PERTIMS,TOTIMS
      
      integer :: ncol, kper, kstp, nrow, ilay, ichn, ncols, nrows, ic, ir
!     ------------------------------------------------------------------
!1------WRITE AN UNFORMATTED RECORD CONTAINING IDENTIFYING
!1------INFORMATION.
      READ(ICHN) KSTP,KPER,PERTIMS,TOTIMS,TEXT,NCOLs,NROWs,ILAY
!
!2------WRITE AN UNFORMATTED RECORD CONTAINING ARRAY VALUES
!2------THE ARRAY IS DIMENSIONED (NCOL,NROW)
      READ(ICHN) ((BUF(IC,IR),IC=1,NCOL),IR=1,NROW)
!
      PERTIM = PERTIMS
      TOTIM = TOTIMS
!
!3------RETURN
      RETURN
    END SUBROUTINE ULASAVRD

    
    subroutine MUSG_ReadBinary_CLN_DDN_File(Modflow)
        implicit none
        
        integer :: i
        character*16   :: text
        
        integer :: idum, KSTPREAD,KPERREAD
        DOUBLE PRECISION :: TOTIMREAD,PERTIMREAD
        
        type (MUSG_Project) Modflow

        allocate(Modflow.cln.sat(Modflow.cln.nCell,Modflow.ntime))

        call msg(' ')
        write(TmpSTR,'(a,i5,a)')'Reading CLN saturation(DDN) data from unit ',Modflow.iDDN_CLN,' file '//Modflow.FNameDDN_CLN(:len_trim(Modflow.FNameDDN_CLN))
        call msg(TmpSTR)
        call msg('    Period      Step      Time Cell1Head            Name')
 
        IOUT=FNumEco
        do i=1,Modflow.ntime
            IDUM = 1
            CALL ULASAVRD(Modflow.cln.sat(:,i),TEXT,KSTPREAD,KPERREAD,PERTIMREAD,&
                TOTIMREAD,NCLNNDS,IDUM,IDUM,Modflow.iDDN_CLN)
            WRITE(TmpSTR,'(2(i10), 2(f10.3), a)') KPERREAD,KSTPREAD,TOTIMREAD,Modflow.cln.sat(1,i),TEXT
            call msg(TmpSTR)
        end do

    end subroutine MUSG_ReadBinary_CLN_DDN_File

    subroutine MUSG_ReadBinary_CLN_CBB_File(Modflow)
        implicit none
        
        integer :: i
        character*16   :: text
        
        integer :: inu, idum, KSTPREAD,KPERREAD
        DOUBLE PRECISION :: TOTIMREAD,PERTIMREAD
        
        type (MUSG_Project) Modflow

        allocate(Modflow.cln.FLX_BAL_ERR(Modflow.cln.nCell,Modflow.ntime))

        write(TmpSTR,'(a,i5,a)')'Reading CLN cell by cell flow data from unit ',Modflow.iCBB_CLN,' file '//Modflow.FNameCBB_CLN(:len_trim(Modflow.FNameCBB_CLN))
        call msg(TmpSTR)
        call msg('Period Step Time Text')

        INU=Modflow.iCBB_CLN
        IOUT=FNumEco
        do i=1,Modflow.ntime
            !INU = ICLNCB
            IDUM = 1
            CALL ULASAVRD(Modflow.cln.FLX_BAL_ERR(:,i),TEXT,KSTPREAD,KPERREAD,PERTIMREAD,&
                TOTIMREAD,NCLNNDS,IDUM,IDUM,INU)
            WRITE(TmpSTR,'(i10, i10, F10.3, a)')KPERREAD,KSTPREAD,TOTIMREAD,TEXT
            call msg(TmpSTR)

        end do

    end subroutine MUSG_ReadBinary_CLN_CBB_File

    subroutine MUSG_ReadBinary_SWF_HDS_File(Modflow)
        implicit none
        
        integer :: i
        character*16   :: text
        
        integer :: idum, KSTPREAD,KPERREAD
        DOUBLE PRECISION :: TOTIMREAD,PERTIMREAD
        
        type (MUSG_Project) Modflow

        allocate(Modflow.swf.head(Modflow.swf.nCell,Modflow.ntime))
        
        call msg(' ')
        write(TmpSTR,'(a,i5,a)')'Reading SWF head data from unit ',Modflow.iHDS_SWF,' file '//Modflow.FNameHDS_SWF(:len_trim(Modflow.FNameHDS_SWF))
        call msg(TmpSTR)
        call msg('    Period      Step      Time Cell1Head            Name')

        do i=1,Modflow.ntime
            IDUM = 1
            CALL ULASAVRD(Modflow.swf.head(:,i),TEXT,KSTPREAD,KPERREAD,PERTIMREAD,&
                TOTIMREAD,NSWFNDS,IDUM,IDUM,Modflow.iHDS_SWF)
            WRITE(TmpSTR,'(2(i10), 2(f10.3), a)') KPERREAD,KSTPREAD,TOTIMREAD,Modflow.swf.head(1,i),TEXT
            call msg(TmpSTR)
        end do

    end subroutine MUSG_ReadBinary_SWF_HDS_File

    subroutine MUSG_ReadBinary_SWF_DDN_File(Modflow)
        implicit none
        
        integer :: i
        character*16   :: text
        
        integer :: idum, KSTPREAD,KPERREAD
        DOUBLE PRECISION :: TOTIMREAD,PERTIMREAD
        
        type (MUSG_Project) Modflow

        allocate(Modflow.swf.sat(Modflow.swf.nCell,Modflow.ntime))

        call msg(' ')
        write(TmpSTR,'(a,i5,a)')'Reading SWF saturation(DDN) data from unit ',Modflow.iDDN_SWF,' file '//Modflow.FNameDDN_SWF(:len_trim(Modflow.FNameDDN_SWF))
        call msg(TmpSTR)
        call msg('    Period      Step      Time Cell1Head            Name')
 
        IOUT=FNumEco
        do i=1,Modflow.ntime
            IDUM = 1
            CALL ULASAVRD(Modflow.swf.sat(:,i),TEXT,KSTPREAD,KPERREAD,PERTIMREAD,&
                TOTIMREAD,NSWFNDS,IDUM,IDUM,Modflow.iDDN_SWF)
            WRITE(TmpSTR,'(2(i10), 2(f10.3), a)') KPERREAD,KSTPREAD,TOTIMREAD,Modflow.swf.sat(1,i),TEXT
            call msg(TmpSTR)
        end do

    end subroutine MUSG_ReadBinary_SWF_DDN_File
    
    
    !    subroutine MUSG_ReadBinary_SWF_HDS_File(Modflow)
!    ! borrowed from J. Doherty.
!        implicit none
!        
!        integer :: FNum
!
!        integer :: kstp,kper
!        real     :: pertim
!        real   :: totim
!        integer :: i
!        character*16   :: text
!        integer  :: ilay        
!        real     :: rtemp  
!        integer  :: nstrt,nend,nstrt_kp,nument
!        integer :: inode, ii
!
!        
!        type (MUSG_Project) Modflow
!
!        !if(.not. Modflow.gwf.have_mesh) call ErrMsg('Must read mesh from GSF file first')
!        
!        allocate(Modflow.swf.head(Modflow.swf.nCell,Modflow.ntime))
!
!        FNum=Modflow.iHDS_SWF
!        do i=1,Modflow.ntime
!          read(FNum,err=9300,end=1000) kstp,kper,pertim,totim,text,nstrt,nend,ilay
!          
!          if((nstrt.le.0).or.(nend.le.0).or.(ilay.le.0)) go to 9300
!          if(index(text,'swf').ne.0)then
!            nstrt_kp=nstrt
!            nstrt=nend
!            nend=nstrt_kp
!            nument=nend-nstrt+1
!            if(nument.le.0)then
!              call msg('nument.le.0')
!              continue
!            end if
!            read(FNum,err=9400,end=9400) (rtemp,ii=1,nument)
!          else
!              read(FNum,end=9400) (Modflow.swf.head(inode,i),inode=nstrt,nend)
!              !write(*,*) totim,nstrt,Modflow.gwf.head(nstrt,i)
!          end if
!
!        end do
!
!9300    continue
!9400    continue
!1000    continue
!        
!        
!        
!    end subroutine MUSG_ReadBinary_SWF_HDS_File
    
!    subroutine MUSG_ReadBinary_SWF_DDN_File(Modflow)
!    ! borrowed from J. Doherty.
!        implicit none
!
!        integer :: Fnum
!
!        integer  :: kstp,kper
!        real     :: pertim
!        real     :: totim
!        integer :: i
!        character*16   :: text
!        integer  :: ilay        
!        real     :: rtemp  
!        integer  :: nstrt,nend,nstrt_kp,nument
!        integer :: inode, ii
!
!        
!        type (MUSG_Project) Modflow
!
!        allocate(Modflow.swf.sat(Modflow.swf.nCell,Modflow.ntime))
!        
!        FNum=Modflow.iDDN_SWF
!        do i=1,Modflow.ntime
!          read(FNum,err=9300,end=1000) kstp,kper,pertim,totim,text,nstrt,nend,ilay
!          if((nstrt.le.0).or.(nend.le.0).or.(ilay.le.0)) go to 9300
!          if(index(text,'swf').ne.0)then
!            nstrt_kp=nstrt
!            nstrt=nend
!            nend=nstrt_kp
!            nument=nend-nstrt+1
!            if(nument.le.0)then
!              call msg('nument.le.0')
!              continue
!            end if
!            read(FNum,err=9400,end=9400) (rtemp,ii=1,nument)
!          else
!              read(FNum,err=9400,end=9400) (Modflow.swf.sat(inode,i),inode=nstrt,nend)
!              !write(*,*) totim,nstrt,Modflow.gwf.sat(nstrt,Modflow.ntime)
!          end if
!
!        end do
!9300    continue
!9400    continue
!1000    continue
!    end subroutine MUSG_ReadBinary_SWF_DDN_File

    subroutine MUSG_ReadBinary_SWF_CBB_File(Modflow)
    ! borrowed from J. Doherty.
        implicit none

        integer :: Fnum

        integer :: kstp,kper,NVAL,idum,ICODE, i
        character*16   :: text
        
        
        integer :: KSTP_last
        !real :: rmin
        !real :: rmax
        integer :: ntime

        
        type (MUSG_Project) Modflow

        ntime=0
        KSTP_last=0
        FNum=Modflow.iCBB_SWF
        do
            read(FNum,err=9300,end=1000) KSTP,KPER,TEXT,NVAL,idum,ICODE
            write(*,*) 'swf cbb ',KSTP,KPER,TEXT,NVAL,idum,ICODE

            if(KSTP .ne. KSTP_last) then
                KSTP_last=KSTP
                ntime=ntime+1 
            endif

              
            if((NVAL.le.0)) go to 9300
            if(ICODE .gt. 0)then
                if(index(TEXT,'FLOW JA FACE') .ne. 0) then
                    if(ntime .eq.1) allocate(Modflow.swf.cbb_ja(NVAL,Modflow.ntime))
                    read(FNum,err=9400,end=9400) (Modflow.swf.cbb_ja(I,ntime),I=1,NVAL)
                    !rmax=-1e20
                    !rmin=1e20
                    !do i=1,nval
                    !    rmax=max(rmax,Modflow.swf.cbb_ja(I,ntime))
                    !    rmin=min(rmin,Modflow.swf.cbb_ja(I,ntime))
                    !enddo
                    !write(*,*) text
                    !write(*,*) nval
                    !write(*,*) rmin
                    !write(*,*) rmax
                else 
                    if(ntime .eq.1) THEN
                        if(index(text,'SWF STORAGE') .ne.0) then
	                        allocate(Modflow.swf.cbb_STORAGE(NVAL,Modflow.ntime))
                        else if(index(text,'SWF CONST HEAD') .ne.0) then
	                        allocate(Modflow.swf.cbb_CONSTANT_HEAD(NVAL,Modflow.ntime))
                        else if(index(text,'FLOW SWF FACE') .ne.0) then
	                        allocate(Modflow.swf.cbb_CONSTANT_HEAD(NVAL,Modflow.ntime))
                            
                            
                        else if(index(text,'RECHARGE') .ne.0) then
	                        allocate(Modflow.swf.cbb_RECHARGE(NVAL,Modflow.ntime))
                        else if(index(text,'DRAINS') .ne.0) then
	                        allocate(Modflow.swf.cbb_DRAINS(NVAL,Modflow.ntime))
                        end if
                    endif  

                    if(index(text,'SWF STORAGE') .ne.0) then
                        read(FNum,err=9400,end=9400) (Modflow.swf.cbb_STORAGE(I,ntime),I=1,NVAL)
                        !rmax=-1e20
                        !rmin=1e20
                        !do i=1,nval
                        !    rmax=max(rmax,Modflow.swf.cbb_STORAGE(I,ntime))
                        !    rmin=min(rmin,Modflow.swf.cbb_STORAGE(I,ntime))
                        !enddo
                        !write(*,*) text
                        !write(*,*) nval
                        !write(*,*) rmin
                        !write(*,*) rmax
                    else if(index(text,'SWF CONST HEAD') .ne.0) then
                        read(FNum,err=9400,end=9400) (Modflow.swf.cbb_CONSTANT_HEAD(I,ntime),I=1,NVAL)
                        !rmax=-1e20
                        !rmin=1e20
                        !do i=1,nval
                        !    rmax=max(rmax,Modflow.swf.cbb_CONSTANT_HEAD(I,ntime))
                        !    rmin=min(rmin,Modflow.swf.cbb_CONSTANT_HEAD(I,ntime))
                        !enddo
                        !write(*,*) text
                        !write(*,*) nval
                        !write(*,*) rmin
                        !write(*,*) rmax
                    else if(index(text,'RECHARGE') .ne.0) then
                        read(FNum,err=9400,end=9400) (Modflow.swf.cbb_RECHARGE(I,ntime),I=1,NVAL)
                        !rmax=-1e20
                        !rmin=1e20
                        !do i=1,nval
                        !    rmax=max(rmax,Modflow.swf.cbb_RECHARGE(I,ntime))
                        !    rmin=min(rmin,Modflow.swf.cbb_RECHARGE(I,ntime))
                        !enddo
                        !write(*,*) text
                        !write(*,*) nval
                        !write(*,*) rmin
                        !write(*,*) rmax
                    else if(index(text,'DRAINS') .ne.0) then
                        read(FNum,err=9400,end=9400) (Modflow.swf.cbb_DRAINS(I,ntime),I=1,NVAL)
                        !rmax=-1e20
                        !rmin=1e20
                        !do i=1,nval
                        !    rmax=max(rmax,Modflow.swf.cbb_DRAINS(I,ntime))
                        !    rmin=min(rmin,Modflow.swf.cbb_DRAINS(I,ntime))
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
 
    end subroutine MUSG_ReadBinary_SWF_CBB_File
    
   
    subroutine MUSG_ReadBAS6_Options(Modflow)
        implicit none

        type (MUSG_Project) Modflow
        
        CHARACTER*80 HEADNG(2)
        integer :: ICHFLG 
        integer :: IPRTIM 
        integer :: IFRCNVG
        integer :: LLOC   
        integer :: ISTART 
        integer :: ISTOP  
        integer :: N      
        real :: R         
        integer :: INOC   
       
        character(4000) :: line
    
        !INBAS=Modflow.iBAS6
        IOUT=FNumEco
        
        !5------Read first lines of BAS Package file and identify grid type and options.
        !5A-----READ AND PRINT COMMENTS.  SAVE THE FIRST TWO COMMENTS IN HEADNG.
        HEADNG(1)=' '
        HEADNG(2)=' '
        WRITE(IOUT,*)
        READ(INBAS,'(A)') LINE
        IF(LINE(1:1).NE.'#') GO TO 20
        HEADNG(1)=LINE(1:80)
        WRITE(IOUT,'(1X,A)') HEADNG(1)
        READ(INBAS,'(A)') LINE
        IF(LINE(1:1).NE.'#') GO TO 20
        HEADNG(2)=LINE(1:80)
        WRITE(IOUT,'(1X,A)') HEADNG(2)
        CALL URDCOM(INBAS,IOUT,LINE)
        !
        !5B-----LOOK FOR OPTIONS IN THE FIRST ITEM AFTER THE HEADING.
        20 IXSEC=0
        ICHFLG=0
        IFREFM=0
        IPRTIM=0
        IUNSTR=0
        IFRCNVG=0
        IDPIN = 0
        IDPOUT = 0
        IHMSIM = 0
        IUIHM = 0
        ISYALL = 0
        LLOC=1
        IPRCONN=0
        25 CALL URWORD(LINE,LLOC,ISTART,ISTOP,1,N,R,IOUT,INBAS)
        IF(LINE(ISTART:ISTOP).EQ.'XSECTION') THEN
            IXSEC=1
            Modflow.xsection=.true.

        ELSE IF(LINE(ISTART:ISTOP).EQ.'CHTOCH') THEN
            ICHFLG=1
            Modflow.chtoch=.true.

        ELSE IF(LINE(ISTART:ISTOP).EQ.'FREE') THEN
            IFREFM=1
            Modflow.free=.true.
            WRITE(IOUT,26)
            26    FORMAT (1X,'THE FREE FORMAT OPTION HAS BEEN SELECTED')
        ELSEIF(LINE(ISTART:ISTOP).EQ.'PRINTTIME') THEN
            IPRTIM=1
            Modflow.printtime=.true.
            WRITE(IOUT,7)
            7    FORMAT(1X,'THE PRINTTIME OPTION HAS BEEN SELECTED')
        ELSEIF(LINE(ISTART:ISTOP).EQ.'UNSTRUCTURED') THEN
            IUNSTR=1
            Modflow.unstructured=.true.
        ELSEIF(LINE(ISTART:ISTOP).EQ.'PRINTFV') THEN
            IPRCONN=1
            Modflow.printfv=.true.
        ELSEIF(LINE(ISTART:ISTOP).EQ.'CONVERGE') THEN
            IFRCNVG=1
            Modflow.converge=.true.
        ELSEIF(LINE(ISTART:ISTOP).EQ.'RICHARDS') THEN
            IUNSat=1
            Modflow.richards=.true.
            WRITE(IOUT,8)
            8    FORMAT(1X,'RICHARDS EQUATION SOLUTION')
        ELSEIF(LINE(ISTART:ISTOP).EQ.'DPIN') THEN
            !----READ OPTIONS FOR SINGLE OR DOUBLE PRECISION READ / WRITE
            WRITE(IOUT,115)
            115       FORMAT(1X,'INPUT BINARY FILES FOR PRIMARY VARIABLES ',&
            'WILL BE IN DOUBLE PRECISION')
            IDPIN = 1
            Modflow.dpin=.true.
        ELSE IF(LINE(ISTART:ISTOP).EQ.'DPOUT') THEN
            WRITE(IOUT,116)
            116      FORMAT(1X,'OUTPUT BINARY FILES FOR PRIMARY VARIABLES ',&
            'WILL BE IN DOUBLE PRECISION')
            IDPOUT = 1
            Modflow.dpout=.true.
        ELSE IF(LINE(ISTART:ISTOP).EQ.'DPIO') THEN
            WRITE(IOUT,115)
            WRITE(IOUT,116)
            IDPIN = 1
            IDPOUT = 1
            Modflow.dpio=.true.
            !-----READ OPTIONS FOR IHM SIMULATION
        ELSE IF(LINE(ISTART:ISTOP).EQ.'IHM') THEN
            IHMSIM = 1
            Modflow.ihm=.true.
            WRITE(IOUT,117)
            117      FORMAT(1X,'MODFLOW-USG IS PART OF AN INTEGRATED HYDROLOGIC',1X,&
            'MODEL (IHM) SIMULATION')
            CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,IUIHM,R,IOUT,INOC)
            IF(IUIHM.GT.0) THEN
                WRITE (IOUT,118) IUIHM
                118        FORMAT(1X,'IHM SIMULATION DEBUGGING INFORMATION IS WRITTEN'&
                1X,'TO FILE ON FORTRAN UNIT' I5)
            ELSE
                WRITE (IOUT,119)
                119        FORMAT(1X,'IHM SIMULATION DEBUGGING INFORMATION IS NOT'&
                1X,'WRITTEN')
            ENDIF
        ELSE IF(LINE(ISTART:ISTOP).EQ.'SY-ALL') THEN
            ISYALL = 1
            Modflow.syall=.true.
            WRITE(IOUT,120)
            120      FORMAT(1X,'IHM SIMULATION REPLACES SY FOR ALL LAYERS')
        END IF
        IF(LLOC.LT.200) GO TO 25
        !!5C-------SET UNSTRUCTURED FLAG IF DISU IS USED
        !INDIS=IUDIS
        !IF(IUNIT(IUDIS+1).GT.0) THEN
        !    IUNSTR=1
        !    INDIS=IUDIS+1
        !ENDIF
        !
        !5D-----PRINT A MESSAGE SHOWING OPTIONS.
        IF(IXSEC.NE.0) WRITE(IOUT,61)
        61 FORMAT(1X,'CROSS SECTION OPTION IS SPECIFIED')
        IF(ICHFLG.NE.0) WRITE(IOUT,62)
        62 FORMAT(1X,'CALCULATE FLOW BETWEEN ADJACENT CONSTANT-HEAD CELLS')
        IF(IUNSTR.NE.0) WRITE(IOUT,63)
        63 FORMAT(1X,'THE UNSTRUCTURED GRID OPTION HAS BEEN SELECTED')
    end subroutine MUSG_ReadBAS6_Options
    
    SUBROUTINE MUSG_ReadBAS6_IBOUND_IHEADS(Modflow)
        !     ******************************************************************
        !     Read IBOUND, HNOFLO and initial heads for unstructured grid input
        !     ******************************************************************
        !
        !        SPECIFICATIONS:
        !     ------------------------------------------------------------------
        USE GLOBAL, ONLY:NLAY,&
                        INBAS,IFREFM,NODES,IOUT,&
                        NIUNIT,HNEW,NODLAY,&
                        IBOUND,STRT,&
                        IDPIN
        USE GWFBASMODULE,ONLY: HNOFLO
                    
        implicit none

        type (MUSG_Project) Modflow
        
        integer :: Kloc, KK, nndlay, nstrt,ndslay,n
       
        REAL, DIMENSION(:),ALLOCATABLE  ::HTMP1
        REAL*8, DIMENSION(:),ALLOCATABLE  ::HTMP18
        CHARACTER*24 ANAME(2)
        DATA ANAME(1) /'          BOUNDARY ARRAY'/
        DATA ANAME(2) /'            INITIAL HEAD'/

        !INBAS=Modflow.iBAS6
        IOUT=FNumEco

        !     ------------------------------------------------------------------
        !
        !1------READ BOUNDARY ARRAY(IBOUND).
        DO Kloc = 1,NLAY
            KK = Kloc
            NNDLAY = NODLAY(Kloc)
            NSTRT = NODLAY(Kloc-1)+1
            NDSLAY = NNDLAY - NODLAY(Kloc-1)
            CALL U1DINT(IBOUND(NSTRT),ANAME(1),NDSLAY,Kloc,INBAS,IOUT)
        ENDDO
        !
        !----------------------------------------------------------------------
        !2------READ AND PRINT HEAD VALUE TO BE PRINTED FOR NO-FLOW CELLS.
        IF(IFREFM.EQ.0) THEN
            READ(INBAS,'(F10.0)') HNOFLO
        ELSE
            READ(INBAS,*) HNOFLO
        END IF
        WRITE(IOUT,3) HNOFLO
        3 FORMAT(1X,/1X,'AQUIFER HEAD WILL BE SET TO ',G12.5,&
        ' AT ALL NO-FLOW NODES (IBOUND=0).')
        !
        !-----------------------------------------------------------------------
        !3------READ INITIAL HEADS.
        IF(IDPIN.EQ.0) THEN !----------------------------------SINGLE PRECISION READ
            ALLOCATE(HTMP1(Nodes))
            DO Kloc=1,NLAY
                NNDLAY = NODLAY(Kloc)
                NSTRT = NODLAY(Kloc-1)+1
                CALL U1DREL(Htmp1(NSTRT),ANAME(2),NNDLAY-NSTRT+1,Kloc,INBAS,IOUT)
            ENDDO
            DO N=1,NODES
                HNEW(N) = HTMP1(N)
                STRT(N) = HNEW(N)
                IF(IBOUND(N).EQ.0) HNEW(N)=HNOFLO
            ENDDO
            DEALLOCATE(HTMP1)
        ELSE       !----------------------------------DOUBLE PRECISION READ
            ALLOCATE(HTMP18(Nodes))
            DO Kloc=1,NLAY
                NNDLAY = NODLAY(Kloc)
                NSTRT = NODLAY(Kloc-1)+1
                CALL U1DREL8(Htmp18(NSTRT),ANAME(2),NNDLAY-NSTRT+1,Kloc,INBAS,IOUT)
            ENDDO
            DO N=1,NODES
                HNEW(N) = HTMP18(N)
                STRT(N) = HNEW(N)
                IF(IBOUND(N).EQ.0) HNEW(N)=HNOFLO
            ENDDO
            DEALLOCATE(HTMP18)
        ENDIF
        !
        !----------------------------------------------------------------------
        !4------RETURN.
        RETURN
    END SUBROUTINE MUSG_ReadBAS6_IBOUND_IHEADS

    subroutine MUSG_ReadDISU_pt1(Modflow)
       
        !      SUBROUTINE SDIS2GLO8AR (IUDIS,IOUT)
        !     *****************************************************************
        !     READ GLOBAL DATA ALLOCATE SPACE FOR 3-D DOMAIN PARAMETERS,
        !     AND READ CONFINING BED INFORMATION ARRAY, LAYCBD
        !     *****************************************************************
        !
        !        SPECIFICATIONS:
        !     ------------------------------------------------------------------
        USE GLOBAL, ONLY:NCOL,NROW,NLAY,NPER,NBOTM,NCNFBD,ITMUNI,&
        LENUNI,NODES,LAYCBD,&
        PERLEN,NSTP,TSMULT,ISSFLG,BOT,TOP,IUNSTR,&
        NJA,NJAG,IVSD,&
        ICONCV,NOCVCO,IDSYMRD,&
        NOVFC

        implicit none

        CHARACTER*400 LINE

        type (MUSG_Project) Modflow
        
        integer :: indis,lloc,istop,istart, k
        real :: r
        
        IOUT=FNumEco

        INDIS=Modflow.iDISU
        IF(INDIS.LE.0) THEN
            WRITE(IOUT,*) ' DIS file must be specified for MODFLOW to run'
            CALL USTOP(' ')
        END IF
        !2-------IDENTIFY PACKAGE
        WRITE(IOUT,11) INDIS
        11 FORMAT(1X,/1X,'DIS -- UNSTRUCTURED GRID DISCRETIZATION PACKAGE,',&
        ' VERSION 1 : 5/17/2010 - INPUT READ FROM UNIT ',I4)
        !
        !
        !3------Read comments and the first line following the comments.
        CALL URDCOM(INDIS,IOUT,LINE)
        !
        !4------Get the grid size, stress periods, and options like
        !4------ITMUNI, and LENUNI from first line.
        LLOC=1
        IVSD=0
        IF(IUNSTR.EQ.0)IVSD = -1
        IF(IUNSTR.EQ.0)THEN
            !4A-----FOR STRUCTURED GRID READ NLAY, NROW AND NCOL
            CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,NLAY,R,IOUT,INDIS)
            CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,NROW,R,IOUT,INDIS)
            CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,NCOL,R,IOUT,INDIS)
            CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,NPER,R,IOUT,INDIS)
            CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,ITMUNI,R,IOUT,INDIS)
            CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,LENUNI,R,IOUT,INDIS)
            NODES = NCOL*NROW*NLAY
            !
            WRITE(IOUT,15) NLAY,NROW,NCOL
            15   FORMAT(1X,I4,' LAYERS',I10,' ROWS',I10,' COLUMNS')
        ELSE
            !4B------FOR UNSTRUCTURED GRID READ NUMBER OF NODES, LAYERS AND CONNECTIVITY SIZES
            CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,NODES,R,IOUT,INDIS)
            CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,NLAY,R,IOUT,INDIS)
            CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,NJAG,R,IOUT,INDIS)
            CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,IVSD,R,IOUT,INDIS)
            CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,NPER,R,IOUT,INDIS)
            CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,ITMUNI,R,IOUT,INDIS)
            CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,LENUNI,R,IOUT,INDIS)
            CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,IDSYMRD,R,IOUT,INDIS)
            !
            NJA = NJAG
            WRITE(IOUT,16) NODES,NLAY,NJAG,IVSD
            16   FORMAT(1X,I10,' NODES',I10,' NLAY',I10,' NJAG',&
            2X,'VERT. SUBDISCRETIZATION INDEX, IVSD = ',I2)
            WRITE(IOUT,17)IDSYMRD
            17      FORMAT(1X,'INDEX FOR INPUT OF UNSTRUCTURED, FINITE-VOLUME',1X,&
            'CONNECTIVITY INFORMATION, IDSYMRD = ',I3)
        ENDIF
        !
        WRITE(IOUT,20) NPER
        20 FORMAT(1X,I4,' STRESS PERIOD(S) IN SIMULATION')
        !
        !5------SELECT AND PRINT A MESSAGE SHOWING TIME UNIT.
        IF(ITMUNI.LT.0 .OR. ITMUNI.GT.5) ITMUNI=0
        IF(ITMUNI.EQ.0) THEN
            WRITE(IOUT,30)
            30    FORMAT(1X,'MODEL TIME UNIT IS UNDEFINED')
        ELSE IF(ITMUNI.EQ.1) THEN
            WRITE(IOUT,40)
            40    FORMAT(1X,'MODEL TIME UNIT IS SECONDS')
        ELSE IF(ITMUNI.EQ.2) THEN
            WRITE(IOUT,50)
            50    FORMAT(1X,'MODEL TIME UNIT IS MINUTES')
        ELSE IF(ITMUNI.EQ.3) THEN
            WRITE(IOUT,60)
            60    FORMAT(1X,'MODEL TIME UNIT IS HOURS')
        ELSE IF(ITMUNI.EQ.4) THEN
            WRITE(IOUT,70)
            70    FORMAT(1X,'MODEL TIME UNIT IS DAYS')
        ELSE
            WRITE(IOUT,80)
            80    FORMAT(1X,'MODEL TIME UNIT IS YEARS')
        END IF
        !
        !6------SELECT AND PRINT A MESSAGE SHOWING LENGTH UNIT.
        IF(LENUNI.LT.0 .OR. LENUNI.GT.3) LENUNI=0
        IF(LENUNI.EQ.0) THEN
            WRITE(IOUT,90)
            90    FORMAT(1X,'MODEL LENGTH UNIT IS UNDEFINED')
        ELSE IF(LENUNI.EQ.1) THEN
            WRITE(IOUT,91)
            91    FORMAT(1X,'MODEL LENGTH UNIT IS FEET')
        ELSE IF(LENUNI.EQ.2) THEN
            WRITE(IOUT,93)
            93    FORMAT(1X,'MODEL LENGTH UNIT IS METERS')
        ELSE IF(LENUNI.EQ.3) THEN
            WRITE(IOUT,95)
            95    FORMAT(1X,'MODEL LENGTH UNIT IS CENTIMETERS')
        END IF
        !7----ALLOCATE SPACE FOR TEMPORAL INFORMATION AND CONFINING LAYERS
        ALLOCATE(LAYCBD(NLAY))
        ALLOCATE(BOT(NODES))
        ALLOCATE(TOP(NODES))
        ALLOCATE (PERLEN(NPER),NSTP(NPER),TSMULT(NPER),ISSFLG(NPER))
        !ALLOCATE (ICONCV,NOCVCO,NOVFC)
        !
        !8----SET FLAGS AND CONFINING INFORMATION
        ICONCV=1
        NOCVCO=1
        NOVFC=0
        !
        !9-------Read confining bed information
        READ(INDIS,*) (LAYCBD(K),K=1,NLAY)
        LAYCBD(NLAY)=0
        WRITE(IOUT,*) ' Confining bed flag for each layer:'
        WRITE(IOUT,'(20I4)') (LAYCBD(K),K=1,NLAY)
        !
        !10------Count confining beds and setup LAYCBD to be the confining
        !10------bed number for each layer.
        NCNFBD=0
        DO 100 K=1,NLAY
            IF(LAYCBD(K).NE.0) THEN
                NCNFBD=NCNFBD+1
                LAYCBD(K)=NCNFBD
            END IF
        100 CONTINUE
        NBOTM=NLAY+NCNFBD
        
        RETURN

    end subroutine MUSG_ReadDISU_pt1

    subroutine MUSG_ReadDISU_pt2(Modflow)
        implicit none

        type (MUSG_Project) Modflow
        
 
        REAL, DIMENSION(:),    ALLOCATABLE  ::TEMP
        CHARACTER*24 ANAME(6)
        DATA ANAME(1) /'  NO. OF NODES PER LAYER'/
        DATA ANAME(2) /'                     TOP'/
        DATA ANAME(3) /'                     BOT'/
        DATA ANAME(4) /'                    AREA'/
        DATA ANAME(5) /'                      IA'/
        DATA ANAME(6) /'                      JA'/
        
        integer :: indis, kk, nndlay,nstrt,ndslay
        integer :: ij, ija, ii, k
        
        indis=Modflow.iDISU
        !
        !------------------------------------------------------------------
        !1-------FILL NODLAY ARRAY WITH LAST NODE NUMBER FOR EACH LAYER AND SET MXNODLAY
        ALLOCATE(NODLAY(0:NLAY))
        !1A----READ NUMBER OF NODES FOR EACH LAYER
        K = 0
        CALL U1DINT(NODLAY(1),ANAME(1),NLAY,K,INDIS,IOUT)
        !1B-----FIND MXNODLAY
        MXNODLAY = 0
        DO K=1,NLAY
            IF(NODLAY(K).GT.MXNODLAY) MXNODLAY = NODLAY(K)
        ENDDO
        !1C------COMPUTE CUMULATIVE TO GIVE NODE NUMBER OF LAST NODE OF A LAYER
        NODLAY(0) = 0
        DO K=2,NLAY
            NODLAY(K) = NODLAY(K-1) + NODLAY(K)
        ENDDO
        !---------------------------------------------------------------------------
        !
        !2------READ TOP ARRAY
        DO K = 1,NLAY
            KK = K
            NNDLAY = NODLAY(K)
            NSTRT = NODLAY(K-1)+1
            NDSLAY = NNDLAY - NODLAY(K-1)
            CALL U1DREL8(TOP(NSTRT),ANAME(2),NDSLAY,K,INDIS,IOUT)
        ENDDO
        !
        !3------READ BOT ARRAY
        DO K = 1,NLAY
            KK = K
            NNDLAY = NODLAY(K)
            NSTRT = NODLAY(K-1)+1
            NDSLAY = NNDLAY - NODLAY(K-1)
            CALL U1DREL8(BOT(NSTRT),ANAME(3),NDSLAY,K,INDIS,IOUT)
        ENDDO
        !
        !4-----READ HORIZONTAL AREA
        IF(IVSD.EQ.-1)THEN
        !4A-------READ AREA ONLY FOR ONE LAYER IF IVSD = -1
            ALLOCATE (TEMP(NODLAY(1)))
            CALL U1DREL(TEMP,ANAME(4),NODLAY(1),1,INDIS,IOUT)
            DO IJ=1,NODLAY(1)
                AREA(IJ) = TEMP(IJ)
            ENDDO
            DEALLOCATE(TEMP)
            DO K=2,NLAY
                DO IJ=1,NODLAY(1)
                    NN = (K-1)*NODLAY(1) + IJ
                    AREA(NN) = AREA(IJ)
                ENDDO
            ENDDO
        ELSE
        !4B------IF IVSD IS NOT -1, READ AREA FOR EACH LAYER
            ALLOCATE (TEMP(NODES))
            DO K = 1,NLAY
                KK = K
                NNDLAY = NODLAY(K)
                NSTRT = NODLAY(K-1)+1
                NDSLAY = NNDLAY - NODLAY(K-1)
                CALL U1DREL(TEMP(NSTRT),ANAME(4),NDSLAY,K,INDIS,IOUT)
                DO IJ = NSTRT,NNDLAY
                    AREA(IJ) = TEMP(IJ)
                ENDDO
            ENDDO
            DEALLOCATE(TEMP)
        ENDIF
        !
        !5------READ CONNECTIONS PER NODE AND CONNECTIVITY AND FILL IA AND JA ARRAYS FOR GWF DOMAIN
        K = 0
        CALL U1DINT(IA,ANAME(5),NODES,K,INDIS,IOUT)
        ALLOCATE(JA(NJA))
        CALL U1DINT(JA,ANAME(6),NJA,K,INDIS,IOUT)
        !5A------ENSURE POSITIVE TERM FOR DIAGONAL OF JA
        DO IJA = 1,NJA
            IF(JA(IJA).LT.0) JA(IJA) = -JA(IJA)
        ENDDO
        !5B------MAKE IA CUMULATIVE FROM CONNECTION-PER-NODE
        DO II=2,NODES+1
            IA(II) = IA(II) + IA(II-1)
        ENDDO
        !---------IA(N+1) IS CUMULATIVE_IA(N) + 1
        DO II=NODES+1,2,-1
            IA(II) = IA(II-1) + 1
        ENDDO
        IA(1) = 1
        
        !----------------------------------------------------------------------
        !15------RETURN.
        RETURN
    end subroutine MUSG_ReadDISU_pt2
    
    
    SUBROUTINE MUSG_ReadDISU_pt3(Modflow)
!        !     ******************************************************************
!        !     READ CLn as CL1, AND FAHL ARRAYS FOR UNSTRUCTURED GRID INPUT.
!        !     ******************************************************************
        USE GLOBAL,   ONLY:FAHL,CL1,NJAG
!
        implicit none
!
        type (MUSG_Project) Modflow
        

        
        integer :: indis, k
        !
        CHARACTER*24 :: ANAME(4)
        DATA ANAME(1) /'     CONNECTION LENGTH 1'/
        DATA ANAME(2) /'     CONNECTION LENGTH 2'/
        DATA ANAME(3) /'    CONNECTION LENGTH 12'/
        DATA ANAME(4) /'      PERPENDICULAR AREA'/

        indis=Modflow.iDISU
        
        
        !12-------READ CONNECTION LENGTHS (DENOM TERM)
        ALLOCATE(CL1(NJAG))
        CL1 = 0.0
        K = 0
        CALL U1DREL(CL1,ANAME(3),NJAG,K,INDIS,IOUT)
        
        ALLOCATE(FAHL(NJAG))
        FAHL = 0.0
        CALL U1DREL(FAHL,ANAME(4),NJAG,K,INDIS,IOUT)
       
        RETURN
    END SUBROUTINE MUSG_ReadDISU_pt3

    subroutine MUSG_ReadDISU_StressPeriodData(Modflow)
    
        implicit none

        type (MUSG_Project) Modflow
        
        integer :: indis,iss,itr,n,lloc, i
        real*8 :: pp
        integer :: istop, istart
        real :: r
        integer :: istartkp, istopkp
        real :: stofrac
        
        
        IOUT=FNumEco
        indis=Modflow.iDISU
        
        !12-----READ AND WRITE LENGTH OF STRESS PERIOD, NUMBER OF TIME STEPS,
        !12-----TIME STEP MULTIPLIER, AND STEADY-STATE FLAG..
        WRITE(IOUT,161)
        161 FORMAT(1X,//1X,'STRESS PERIOD     LENGTH       TIME STEPS',&
                '     multiplier FOR DELT    SS FLAG',/1X,76('-'))
        ISS=0
        ITR=0
        ALLOCATE(STORFRAC(NPER))
        DO 200 N=1,NPER
            READ(INDIS,'(A)') LINE
            LLOC=1
            CALL URWORD8(LINE,LLOC,ISTART,ISTOP,3,I,PP,IOUT,INDIS)
            PERLEN(N) = PP
            CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,NSTP(N),R,IOUT,INDIS)
            CALL URWORD(LINE,LLOC,ISTART,ISTOP,3,I,TSMULT(N),IOUT,INDIS)
            CALL URWORD(LINE,LLOC,ISTART,ISTOP,1,I,R,IOUT,INDIS)
            STORFRAC(N) = -1.0
            IF (LINE(ISTART:ISTOP).EQ.'TR') THEN
                ISSFLG(N)=0
                ITR=1
                WRITE(IOUT,163) N,PERLEN(N),NSTP(N),TSMULT(N),LINE(ISTART:ISTOP)
            ELSE IF (LINE(ISTART:ISTOP).EQ.'SS') THEN
                ISSFLG(N)=1
                ISS=1
                WRITE(IOUT,163) N,PERLEN(N),NSTP(N),TSMULT(N),LINE(ISTART:ISTOP)
            ELSE IF (LINE(ISTART:ISTOP).EQ.'TRTOSS') THEN
                ISSFLG(N)=0
                ISTARTKP = ISTART
                ISTOPKP = ISTOP
                CALL URWORD(LINE,LLOC,ISTART,ISTOP,3,I,STOFRAC,IOUT,INDIS)
                STORFRAC(N) = STOFRAC
                WRITE(IOUT,263) N,PERLEN(N),NSTP(N),TSMULT(N),&
                LINE(ISTARTKP:ISTOPKP), STOFRAC
            ELSE
                WRITE(IOUT,162)
                162    FORMAT(' SSFLAG MUST BE EITHER "SS", "TR", OR',&
                ' TRTOSS -- STOP EXECUTION (SGWF2BAS7U1ARDIS)')
                CALL USTOP(' ')
            END IF
            163 FORMAT(1X,I8,1PG21.7,I7,0PF25.3,A11)
            263 FORMAT(1X,I8,1PG21.7,I7,0PF25.3,A11,' STORAGE FRACTION =', E12.4)
            !
            !13-----STOP IF NSTP LE 0, PERLEN EQ 0 FOR TRANSIENT STRESS PERIODS,
            !13-----TSMULT LE 0, OR PERLEN LT 0..
            IF(NSTP(N).LE.0) THEN
                WRITE(IOUT,164)
                164    FORMAT(1X,/1X,&
                'THERE MUST BE AT LEAST ONE TIME STEP IN EVERY STRESS PERIOD')
                CALL USTOP(' ')
            END IF
            ZERO=0.
            IF(PERLEN(N).EQ.ZERO .AND. ISSFLG(N).EQ.0) THEN
                WRITE(IOUT,165)
                165    FORMAT(1X,/1X,&
                'PERLEN MUST NOT BE 0.0 FOR TRANSIENT STRESS PERIODS')
                CALL USTOP(' ')
            END IF
            IF(TSMULT(N).LE.ZERO) THEN
                WRITE(IOUT,170)
                170    FORMAT(1X,/1X,'TSMULT MUST BE GREATER THAN 0.0')
                CALL USTOP(' ')
            END IF
            IF(PERLEN(N).LT.ZERO) THEN
                WRITE(IOUT,175)
                175    FORMAT(1X,/1X,&
                'PERLEN CANNOT BE LESS THAN 0.0 FOR ANY STRESS PERIOD')
                CALL USTOP(' ')
            END IF
        200 CONTINUE
        !
        !14-----Assign ITRSS.
        IF(ISS.EQ.0 .AND. ITR.NE.0) THEN
            ITRSS=1
            WRITE(IOUT,270)
            270    FORMAT(/,1X,'TRANSIENT SIMULATION')
        ELSE IF(ISS.NE.0 .AND. ITR.EQ.0) THEN
            ITRSS=0
            WRITE(IOUT,275)
            275    FORMAT(/,1X,'STEADY-STATE SIMULATION')
        ELSE
            ITRSS=-1
            WRITE(IOUT,280)
            280    FORMAT(/,1X,'COMBINED STEADY-STATE AND TRANSIENT SIMULATION')
        END IF
        !
        ALLOCATE(IA2(NEQS+1))
        ALLOCATE(IA1IA2(NEQS))
        ALLOCATE(JA2(NJA))
        ALLOCATE(JA1JA2(NJA))
        !
        !15-----RETURN.
        RETURN
    end subroutine MUSG_ReadDISU_StressPeriodData


    subroutine MUSG_ReadCLN(Modflow)
        implicit none

        type (MUSG_Project) Modflow
        character(400) :: line
        
        integer :: ICLNNDS
        CHARACTER*24 ANAME(3)
        DATA ANAME(1) /'   NODES PER CLN SEGMENT'/
        DATA ANAME(2) /'                      IA'/
        DATA ANAME(3) /'                      JA'/

        integer :: i1
        integer :: IJA
        integer :: II
        real :: FLENG
        integer :: IFTYP
        integer :: ICCWADI
        real :: FELEV
        integer :: IFDIR
        integer :: IFNO
        real :: FANGLE
        integer :: IFLIN
        integer :: LLOC
        integer :: ISTART
        integer :: ISTOP
        real :: R
        real :: FSKIN        
        real :: FANISO        
        integer :: IFCON
        integer :: IFNOD
        integer :: ICGWADI
        integer :: IFROW
        integer :: IFLAY
        integer :: IFCOL, i, j, k
     
        
        
        IOUT=FNumEco
        INCLN=Modflow.iCLN
        
        WRITE(IOUT,1)
1       FORMAT(1X,/1X,'CLN -- CONNECTED LINE NETWORK DISCRETIZATION PROCESS, VERSION 1, 3/3/2012 ')
        
        do 
            read(Modflow.iCLN,'(a)',iostat=status) line
            call lcase(line)
            if(status /= 0) return
            
            IF(index(line,'options') .ne. 0) THEN
                IF(index(line,'transient') .ne. 0) THEN
                    ICLNTIB=1
                    WRITE(IOUT,71)
71                  FORMAT(1X,'TRANSIENT IBOUND OPTION: READ TRANSIENT IBOUND RECORDS FOR EACH STRESS PERIOD.')
                    
                end if    
            
                IF(index(line,'printiaja') .ne. 0) THEN
                    IPRCONN=1
                    WRITE(IOUT,72)
72                  FORMAT(1X,'PRINT CLN IA AND JA OPTION: THE CLN IA AND JA ARRAYS WILL BE PRINTED TO LIST FILE.')
                    
                end if
            
                IF(index(line,'processccf') .ne. 0) THEN
                    read(Modflow.iCLN,*) ICLNGWCB

                    ICLNPCB=1                                                     !aq CLN CCF
                    WRITE(IOUT,73)                                                !aq CLN CCF
73                  FORMAT(1X,'PROCESS CELL-TO-CELL FLOW BUDGET OPTION: FLOW BUDGET WILL USE A SEPARATE FILE FOR CLN-GWF FLOW.')     !aq CLN CCF
                    
                    IF(ICLNGWCB.LT.0) WRITE(IOUT,18)                              !aq CLN CCF
18                  FORMAT(1X,'CELL-BY-CELL GWP FLOWS WILL BE PRINTED WHEN ICBCFL IS NOT 0 (FLAG ICLNGWCB IS LESS THAN ZERO)')                  !aq CLN CCF
                    
                    IF(ICLNGWCB.GT.0) WRITE(IOUT,19)                      !aq CLN CCF
19                  FORMAT(1X,'CELL-BY-CELL GWP FLOWS WILL BE SAVED(FLAG ICLNGWCB IS GREATER THAN ZERO)')                         !aq CLN CCF
                    
                end if
                
            else 
                read(line,*) NCLN,ICLNNDS,ICLNCB,ICLNHD,ICLNDD,ICLNIB,NCLNGWC,NCONDUITYP
                WRITE(IOUT,3) NCLN,ICLNNDS,NCLNGWC                                             
3               FORMAT(1X,'FLAG (0) OR MAXIMUM NUMBER OF LINEAR NODES (NCLN) =',I7&     
                    /1X,'FLAG (-VE) OR NUMBER OF LINEAR NODES (+VE)',&     
                    1X,'(ICLNNDS) =',I7&     
                    /1X,'NUMBER OF LINEAR NODE TO MATRIX GRID CONNECTIONS',&     
                    ' (NCLNGWC) =',I7/) 
                
                IF(ICLNCB.LT.0) WRITE(IOUT,7)
7               FORMAT(1X,'CELL-BY-CELL FLOWS WILL BE PRINTED WHEN ICBCFL',       &
                    ' IS NOT 0 (FLAG ICLNCB IS LESS THAN ZERO)')

                IF(ICLNCB.GT.0) WRITE(IOUT,8) ICLNCB
8               FORMAT(1X,'CELL-BY-CELL FLOWS WILL BE SAVED ON UNIT ',I5,         &
                    '(FLAG ICLNCB IS GREATER THAN ZERO)')

                IF(ICLNCB.EQ.0) WRITE(IOUT,6)
6               FORMAT(1X,'CELL-BY-CELL FLOWS WILL NOT BE SAVED OR PRINTED',       &
                    1X,'(FLAG ICLNCB IS EQUAL TO ZERO)')

                IF(ICLNHD.LT.0) WRITE(IOUT,9)
9               FORMAT(1X,'CLN HEAD OUTPUT WILL BE SAVED TO THE SAME UNIT',1X,      &
                    'NUMBER (IHEDUN) AS USED FOR HEAD OUTPUT FOR POROUS MATRIX',   &!kkz - added trailing comma per JCH
                    1X,'(FLAG ICLNHD IS LESS THAN ZERO)')

                IF(ICLNHD.GT.0) WRITE(IOUT,10) ICLNHD
10              FORMAT(1X,'CLN HEAD OUTPUT WILL BE SAVED ON UNIT ',I4,             &
                    '(FLAG ICLNHD IS GREATER THAN ZERO)')

                IF(ICLNHD.EQ.0) WRITE(IOUT,31)
31              FORMAT(1X,'CLN HEAD OUTPUT WILL NOT BE SAVED OR PRINTED',           &
                    1X,'(FLAG ICLNHD IS EQUAL TO ZERO)')

                IF(ICLNDD.LT.0) WRITE(IOUT,12)
12              FORMAT(1X,'CLN DDN OUTPUT WILL BE SAVED TO THE SAME UNIT',1X,      &
                    'NUMBER (IDDNUN) AS USED FOR DDN OUTPUT FOR POROUS MATRIX',   & !kkz - added trailing comma per JCH
                    1X,'(FLAG ICLNDD IS LESS THAN ZERO)')

                IF(ICLNDD.GT.0) WRITE(IOUT,13) ICLNDD
                13  FORMAT(1X,'CLN DDN OUTPUT WILL BE SAVED ON UNIT ',I4,              &
                    '(FLAG ICLNDD IS GREATER THAN ZERO)')

                IF(ICLNDD.EQ.0) WRITE(IOUT,14)
14              FORMAT(1X,'CLN DDN OUTPUT WILL NOT BE SAVED OR PRINTED',            &
                    1X,'(FLAG ICLNDD IS EQUAL TO ZERO)')

                IF(ICLNIB.LT.0) WRITE(IOUT,32)
32              FORMAT(1X,'CLN IBOUND OUTPUT WILL BE SAVED TO THE SAME UNIT',1X,    &
                    'NUMBER (IBOUUN) AS USED FOR DDN OUTPUT FOR POROUS MATRIX',   & !kkz - added trailing comma per JCH
                    1X,'(FLAG ICLNIB IS LESS THAN ZERO)')
                

                IF(ICLNIB.GT.0) WRITE(IOUT,33) ICLNIB
33              FORMAT(1X,'CLN IBOUND OUTPUT WILL BE SAVED ON UNIT ',I4,           &
                    '(FLAG ICLNIB IS GREATER THAN ZERO)')
                

                IF(ICLNIB.EQ.0) WRITE(IOUT,17)
17              FORMAT(1X,'CLN IBOUND OUTPUT WILL NOT BE SAVED OR PRINTED',         &
                    1X,'(FLAG ICLNIB IS EQUAL TO ZERO)')
                
                !C--------------------------------------------------------------------------------
                !C3B----READ GRAVITY AND KINEMATIC VISCOSITY IN CASE IT IS REQUIRED FOR TURBULENT FLOW
                !ALLOCATE(GRAV,VISK)
                !ALLOCATE(IBHETYP)
                GRAV = 0.0
                VISK = 0.0
                IBHETYP = 0
                
                IF(index(line,'gravity') .ne. 0) THEN
                    i1=index(line,'gravity')+7
                    line=line(i1:)
                    read(line,*) GRAV
                    WRITE(IOUT,34) GRAV
34                  FORMAT(1X,'GRAVITATIONAL ACCELERATION [L/T^2] = ', G15.6)
                endif 
                
                IF(index(line,'viscosity') .ne. 0) THEN
                    i1=index(line,'viscosity')+9
                    line=line(i1:)
                    read(line,*) VISK
                    WRITE(IOUT,35) VISK
35                  FORMAT(1X,'KINEMATIC VISCOSITY [L^2/T] = ', G15.6)
                END IF

                !C3B----READ OPTION FOR NON-CIRCULAR CROSS-SECTIONS
                IF(index(line,'rectangular') .ne. 0) THEN
                    i1=index(line,'rectangular')+11
                    line=line(i1:)
                    read(line,*) NRECTYP
                    WRITE(IOUT,36) NRECTYP
36                  FORMAT(1X,'NUMBER OF RECTANGULAR SECTION GEOMETRIES = ', I10)
                END IF

                !C3C----READ OPTION FOR BHE DETAILS
                IF(index(line,'bhedetail') .ne. 0) THEN
                    IBHETYP = 1
                    IF(ITRNSP.EQ.0) IBHETYP = 0 ! NO BHE (OR INPUT) IF TRANSPORT IS NOT RUN
                    WRITE(IOUT,37)
37                  FORMAT(1X,'BHE DETAILS WILL BE INPUT FOR EACH CLN TYPE')
                ENDIF

                !C3D----READ OPTION FOR SAVING CLN OUTPUT AND UNIT NUMBER
                IF(index(line,'saveclncon') .ne. 0) THEN
                    i1=index(line,'saveclncon')+10
                    line=line(i1:)
                    read(line,*) ICLNCN
                    
                    !IF(INBCT.EQ.0) ICLNCN = 0 ! SHUT OFF IF NO TRANSPORT SIMULATION
                    ICLNCN = 0 ! SHUT OFF for now rgm
                    
                    IF(ICLNCN.LT.0) WRITE(IOUT,42)
42                  FORMAT(1X,'CLN CONC OUTPUT WILL BE SAVED TO THE SAME UNIT',1X,    &
                    'NUMBER (ISPCUN) AS USED FOR CONC OUTPUT FOR POROUS MATRIX',     & !kkz - added trailing comma per JCH
                    1X,'(FLAG ICLNCN IS LESS THAN ZERO)')
                    
                    IF(ICLNCN.GT.0) WRITE(IOUT,43) ICLNCN
43                  FORMAT(1X,'CLN CONC OUTPUT WILL BE SAVED ON UNIT ',I4,      &
                   '(FLAG ICLNCN IS GREATER THAN ZERO)')
                    
                    IF(ICLNCN.EQ.0) WRITE(IOUT,44)
44                  FORMAT(1X,'CLN CONC OUTPUT WILL NOT BE SAVED OR PRINTED',       &
                     1X,'(FLAG ICLNCN IS EQUAL TO ZERO)')
                    
                ENDIF
!
                IF(index(line,'saveclnmas') .ne. 0) THEN
                    i1=index(line,'saveclnmas')+10
                    line=line(i1:)
                    read(line,*) ICLNMB
                    !IF(INBCT.EQ.0) ICLNMB = 0 ! SHUT OFF IF NO TRANSPORT SIMULATION
                    ICLNMB = 0 ! SHUT OFF for now rgm
                endif

                IF(ICLNMB.LT.0) WRITE(IOUT,45)
45              FORMAT(1X,'CLN MASS FLUX OUTPUT WILL BE SAVED TO THE SAME',1X, &
                'UNIT NUMBER (IBCTCB) AS USED FOR CONC OUTPUT FOR POROUS',   & !kkz - added trailing comma per JCH
                1X,'MATRIX (FLAG ICLNMB IS LESS THAN ZERO)')
                
                IF(ICLNMB.GT.0) WRITE(IOUT,46) ICLNMB
46              FORMAT(1X,'CLN MASS FLUX OUTPUT WILL BE SAVED ON UNIT ',I4,         &
                '(FLAG ICLNMB IS GREATER THAN ZERO)')
        
                IF(ICLNMB.EQ.0) WRITE(IOUT,47)
47              FORMAT(1X,'CLN MASS FLUX OUTPUT WILL NOT BE SAVED OR PRINTED',          &
                1X,'(FLAG ICLNMB IS EQUAL TO ZERO)')
            END IF

            !C--------------------------------------------------------------------------------
            !C4------FOR INPUT OF MULTI-NODE WELLS OR CLN SEGMENTS
            !C4------DIMENSION AND READ ARRAY THAT CONTAINS NUMBER OF NODES PER CLN SEGMENT
            IF(NCLN.GT.0)THEN
        !        ALLOCATE(NNDCLN(0:NCLN))
        !        K = 0
        !        CALL U1DINT(NNDCLN(1),ANAME(1),NCLN,K,IOUT,IOUT)
        !        NNDCLN(0) = 0
        !C
        !C5--------MAKE NNDCLN ARRAY CUMULATIVE
        !        DO I = 1,NCLN
        !          NNDCLN(I) = NNDCLN(I) + NNDCLN(I-1)
        !        ENDDO
        !        NCLNCONS = NNDCLN(NCLN)
        !C------------------------------------------------------------------------------
        !C6--------FILL CLNCON WITH CONNECTIVITY OF ADJACENT CLN NODES
        !        IF(ICLNNDS.LT.0)THEN
        !C6A---------FILL CLN CONNECTIONS SEQUENTIALLY WITH GLOBAL NODE NUMBERS
        !          NCLNNDS = NNDCLN(NCLN)
        !          ALLOCATE(CLNCON(NCLNNDS))
        !          DO I=1,NCLNNDS
        !            CLNCON(I) = I ! +NODES  ! (KEEP LOCAL NODE NUMBER)
        !          ENDDO
        !        ELSE
        !C6B-------SET NUMBER OF CLN NODES AND READ CONNECTION ARRAY FOR EACH CLN SEGMENT
        !          NCLNNDS = ICLNNDS
        !          ALLOCATE(CLNCON(NCLNCONS))
        !          DO I=1,NCLN
        !            IF(IFREFM.EQ.0) THEN
        !              read(Modflow.iCLN,'(200I10)')
        !     1        (CLNCON(J),J=NNDCLN(I-1)+1,NNDCLN(I))
        !            ELSE
        !              read(Modflow.iCLN,*) (CLNCON(J),J=NNDCLN(I-1)+1,NNDCLN(I))
        !            ENDIF
        !          ENDDO
        !cspC6C---------CONVERT CLN-NODE NUMBER TO GLOBAL NODE NUMBER
        !csp          DO I=1,NCLNCONS
        !csp            CLNCON(I) = NODES + CLNCON(I)
        !csp          ENDDO
        !        ENDIF
        !C6D--------CONVERT TO IA_CLN AND JA_CLN
        !        ALLOCATE(IA_CLN(NCLNNDS+1))
        !        CALL FILLIAJA_CLN
        !C6E---------DEALLOCATE UNWANTED ARRAYS
        !        DEALLOCATE (NNDCLN) ! NNDCLN NEEDED FOR WRITING BUDGET TO ASCII FILE?
        !        DEALLOCATE (CLNCON)
            ELSE
                !C----------------------------------------------------------------------
                !C7------FOR INPUT OF IA AND JAC OF CLN DOMAIN (NCLN = 0), READ DIRECTLY
                NCLNNDS = ICLNNDS
                ALLOCATE(IA_CLN(NCLNNDS+1))
                !C7A-------READ NJA_CLN
                IF(IFREFM.EQ.0) THEN
                  read(Modflow.iCLN,'(I10)') NJA_CLN
                ELSE
                  read(Modflow.iCLN,*) NJA_CLN
                ENDIF
                !C7B-------READ CONNECTIONS PER NODE AND CONNECTIVITY AND FILL IA_CLN AND JA_CLN ARRAYS
                K = 0
                CALL U1DINT(IA_CLN,ANAME(2),NCLNNDS,K,INCLN,IOUT)
                ALLOCATE(JA_CLN(NJA_CLN))
                CALL U1DINT(JA_CLN,ANAME(3),NJA_CLN,K,INCLN,IOUT)
                !C7C--------ENSURE POSITIVE TERM FOR DIAGONAL OF JA_CLN
                DO IJA = 1,NJA_CLN
                  IF(JA_CLN(IJA).LT.0) JA_CLN(IJA) = -JA_CLN(IJA)
                ENDDO
                !C7D--------MAKE IA_CLN CUMULATIVE FROM CONNECTION-PER-NODE
                DO II=2,NCLNNDS+1
                  IA_CLN(II) = IA_CLN(II) + IA_CLN(II-1)
                ENDDO
                !C---------IA_CLN(N+1) IS CUMULATIVE_IA_CLN(N) + 1
                DO II=NCLNNDS+1,2,-1
                  IA_CLN(II) = IA_CLN(II-1) + 1
                ENDDO
                IA_CLN(1) = 1
            ENDIF
            !C----------------------------------------------------------------------
            !C8------ALLOCATE SPACE FOR CLN PROPERTY ARRAYS
            ALLOCATE(ACLNNDS(NCLNNDS,6))
            ALLOCATE(IFLINCLN(NCLNNDS))
            ALLOCATE(ICCWADICLN(NCLNNDS))
            ALLOCATE(ICGWADICLN(NCLNGWC))
            !C
            !C9------PREPARE TO REFLECT INPUT PROPERTIES INTO LISTING FILE
             WRITE(IOUT,21)
21           FORMAT(/20X,' CONNECTED LINE NETWORK INFORMATION'/&
                20X,40('-')/5X,'CLN-NODE NO.',1X,'CLNTYP',1X,'ORIENTATION',2X,& 
               'CLN LENGTH',4X,'BOT ELEVATION',9X,'FANGLE',9X,'IFLIN',11X,&
               'ICCWADI'/5X,11('-'),2X,6('-'),1X,11('-'),1X,11('-'),4X,13('-'),&
                4X,11('-'),8X,6('-'),4X,7('-'))
            !C
            !C10-------READ BASIC PROPERTIES FOR ALL CLN NODES AND FILL ARRAYS
            DO I = 1,NCLNNDS
                CALL URDCOM(INCLN,IOUT,LINE)
                IF(IFREFM.EQ.0) THEN
                    READ(LINE,'(3I10,3F10.3,2I10)') IFNO,IFTYP,IFDIR,FLENG,FELEV,FANGLE,IFLIN,ICCWADI
                    !READ(LINE,*) IFNO,IFTYP,IFDIR,FLENG,FELEV,FANGLE,IFLIN,ICCWADI
                    LLOC=71
                ELSE
                    LLOC=1
                    CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,IFNO,R,IOUT,IOUT)
                    CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,IFTYP,R,IOUT,IOUT)
                    CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,IFDIR,R,IOUT,IOUT)
                    CALL URWORD(LINE,LLOC,ISTART,ISTOP,3,I,FLENG,IOUT,IOUT)
                    CALL URWORD(LINE,LLOC,ISTART,ISTOP,3,I,FELEV,IOUT,IOUT)
                    CALL URWORD(LINE,LLOC,ISTART,ISTOP,3,I,FANGLE,IOUT,IOUT)
                    CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,IFLIN,R,IOUT,IOUT)
                    CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,ICCWADI,R,IOUT,IOUT)
                END IF
                IF(IFLIN.EQ.0) IFLIN = -1
                !C11A-------FOR ANGLED PIPE, IF DEPTH OF FLOW IS LESS THAN DIAMETER MAKE HORIZONTAL
                !c        IF(IFDIR.EQ.2)THEN
                !c          FDPTH = FLENG * SIN(FANGLE)
                !c          IC=IFTYP
                !c          CALL CLNR(IC,FRAD)
                !c          IF(FDPTH.LT.2.0*FRAD) IFDIR = 1
                !c        ENDIF
                WRITE(IOUT,22)IFNO,IFTYP,IFDIR,FLENG,FELEV,FANGLE,IFLIN,ICCWADI
22              FORMAT(5X,I10,1X,I6,1X,I10,3(1X,E15.6),1X,I10,1X,I10)

                !C11B------FILL PROPERTY ARRAYS WITH READ AND PREPARE INFORMATION
                ACLNNDS(I,1) = IFNO + NODES ! GLOBAL NODE NUMBER FOR CLN-CELL
                ACLNNDS(I,2) = IFTYP
                ACLNNDS(I,3) = IFDIR
                ACLNNDS(I,4) = FLENG
                ACLNNDS(I,5) = FELEV
                ACLNNDS(I,6) = FANGLE
                IFLINCLN(I) = IFLIN
                ICCWADICLN(I) = ICCWADI
            END DO
            !----------------------------------------------------------------------------------------
            !12------ALLOCATE SPACE FOR CLN TO GW PROPERTY ARRAYS
            ALLOCATE(ACLNGWC(NCLNGWC,6))
            !----------------------------------------------------------------------------------------
            !13------READ CONNECTING SUBSURFACE NODE AND ASSOCIATED PARAMETERS
            IF(IUNSTR.EQ.0)THEN
                !
                !14A-----FOR STRUCTURED GRID READ SUBSURFACE NODE IN IJK FORMATS
                !14A-----AND OTHER CLN SEGMENT PROPERTY INFORMATION

                !1------PREPARE TO REFLECT INPUT INTO LISTING FILE
               WRITE(IOUT,41)
41             FORMAT(/20X,' CLN TO 3-D GRID CONNECTION INFORMATION'/&
                    20X,40('-')/5X,'F-NODE NO.',6X,'LAYER',8X,'ROW',5X,'COLUMN',&
                    2X,'EQTN. TYPE',5X,'      FSKIN',11X,'FLENG',10X,&
                    'FANISO',3X,'ICGWADI'/5X,10('-'),6X,5('-'),8X,3('-'),5X,&
                    6('-'),2X,11('-'),3X,12('-'),2X,14('-'),4X,12('-'),3X,7('-'))
                
                !2-------READ PROPERTIES AND SUBSURFACE CONNECTION INFORMATION FOR ALL CLN NODES
                DO I = 1,NCLNGWC
                    CALL URDCOM(INCLN,IOUT,LINE)
                    IF(IFREFM.EQ.0) THEN
                        READ(LINE,*) IFNO,IFLAY,IFROW,IFCOL,IFCON,&
                        FSKIN,FLENG,FANISO,ICGWADI
                        LLOC=91
                    ELSE
                        LLOC=1
                        CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,IFNO,R,IOUT,INCLN)
                        CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,IFLAY,R,IOUT,INCLN)
                        CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,IFROW,R,IOUT,INCLN)
                        CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,IFCOL,R,IOUT,INCLN)
                        CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,IFCON,R,IOUT,INCLN)
                        CALL URWORD(LINE,LLOC,ISTART,ISTOP,3,I,FSKIN,IOUT,INCLN)
                        CALL URWORD(LINE,LLOC,ISTART,ISTOP,3,I,FLENG,IOUT,INCLN)
                        CALL URWORD(LINE,LLOC,ISTART,ISTOP,3,I,FANISO,IOUT,INCLN)
                        CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,ICGWADI,R,IOUT,INCLN)
                    END IF
                    !3--------SET SKIN PARAMETER AND REFLECT INPUT IN LST FILE
                    IF(IFCON.EQ.0)FSKIN = 0.0
                    WRITE(IOUT,52)IFNO,IFLAY,IFROW,IFCOL,IFCON,FSKIN,FLENG,FANISO,&
                            ICGWADI
52                          FORMAT(5X,I10,3(1X,I10),2X,I10,3(1X,E15.6),1X,I9)
                            
                        
                    !4--------FILL CLN AND GW NODE NUMBERS AND CONNECTION PROPERTY MATRIX
                    ACLNGWC(I,1) = IFNO
                    IFNOD = (IFLAY-1)*NROW*NCOL + (IFROW-1)*NCOL + IFCOL
                    ACLNGWC(I,2) = IFNOD
                    ACLNGWC(I,3) = IFCON
                    ACLNGWC(I,4) = FSKIN
                    ACLNGWC(I,5) = FANISO
                    ACLNGWC(I,6) = FLENG
                    ICGWADICLN(I) = ICGWADI
                ENDDO
            ELSE
                !
                !14B-----FOR UNSTRUCTURED GRID READ SUBSURFACE NODE NUMBER OF
                !14B-----CONNECTION AND OTHER CLN SEGMENT PROPERTY INFORMATION

                !1------PREPARE TO REFLECT INPUT INTO LISTING FILE
                WRITE(IOUT,23)
23              FORMAT(/20X,' CLN TO 3-D GRID CONNECTION INFORMATION'/&
                    20X,40('-')/5X,'F-NODE NO.',1X,'GW-NODE NO',2X,&
                    'EQTN. TYPE',2X,'      FSKIN',11X,&
                    'FLENG',9X,'FANISO'3X,'ICGWADI'/5X,10('-'),1X,10('-'),&
                    1X,11('-'),5X,11('-'),1X,17('-'),1X,15('-'),3X,10('-'))
                
                !2-------READ PROPERTIES AND SUBSURFACE CONNECTION INFORMATION FOR ALL CLN NODES
                DO I = 1,NCLNGWC
                    CALL URDCOM(INCLN,IOUT,LINE)
                    IF(IFREFM.EQ.0) THEN
                        READ(LINE,'(3I10,3F10.3,I10)') IFNO,IFNOD,IFCON,FSKIN,FLENG,&
                            FANISO,ICGWADI
                        LLOC=71
                    ELSE
                        LLOC=1
                        CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,IFNO,R,IOUT,INCLN)
                        CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,IFNOD,R,IOUT,INCLN)
                        CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,IFCON,R,IOUT,INCLN)
                        CALL URWORD(LINE,LLOC,ISTART,ISTOP,3,I,FSKIN,IOUT,INCLN)
                        CALL URWORD(LINE,LLOC,ISTART,ISTOP,3,I,FLENG,IOUT,INCLN)
                        CALL URWORD(LINE,LLOC,ISTART,ISTOP,3,I,FANISO,IOUT,INCLN)
                        CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,ICGWADI,R,IOUT,INCLN)
                    END IF
                    !3--------SET SKIN PARAMETER AND REFLECT INPUT IN LST FILE
                    IF(IFCON.EQ.0)FSKIN = 0.0
                    WRITE(IOUT,24)IFNO,IFNOD,IFCON,FSKIN,FLENG,FANISO,ICGWADI
24                  FORMAT(5X,I10,1X,I10,2X,I10,3(1X,E15.6),1X,I9)
                    
                    !4--------FILL CLN AND GW NODE NUMBERS AND CONNECTION PROPERTY MATRIX
                    ACLNGWC(I,1) = IFNO
                    ACLNGWC(I,2) = IFNOD
                    ACLNGWC(I,3) = IFCON
                    ACLNGWC(I,4) = FSKIN
                    ACLNGWC(I,5) = FANISO
                    ACLNGWC(I,6) = FLENG
                    ICGWADICLN(I) = ICGWADI
                ENDDO
                
            ENDIF
            !!----------------------------------------------------------------------------------------
            !!15B------ALLOCATE SPACE AND FILL PROPERTIES FOR ALL CONDUIT TYPE CLNs
            !      IF(NRECTYP.GT.0)THEN
            !        CALL SCLN2REC1RP
            !      ENDIF
            !!----------------------------------------------------------------------------------------
            !!16------ALLOCATE SPACE AND FILL PROPERTIES FOR OTHER CLN TYPES HERE
            !!ADD------ADD OTHER CLN TYPE READ AND PREPARE INFORMATION HERE
            !!----------------------------------------------------------------------------------------
               
            WRITE(IOUT,'(/,A)')' IA_CLN IS BELOW, 40I10'
            WRITE(IOUT,55)(IA_CLN(I),I=1,NCLNNDS+1)
            WRITE(IOUT,*)'NJA_CLN = ',NJA_CLN
            WRITE(IOUT,*)'JA_CLN IS BELOW, 40I10'
            WRITE(IOUT,55)(JA_CLN(J),J=1,NJA_CLN)
55          FORMAT(40I10)
            
            exit
            
        end do
        RETURN
    end subroutine MUSG_ReadCLN

          SUBROUTINE MUSG_ReadSWF(Modflow)
!     ******************************************************************
!     ALLOCATE SPACE AND READ NODE AND CONNECTIVITY INFORMATION FOR SWF DOMAIN
!     ******************************************************************
!
!        SPECIFICATIONS:
!     ------------------------------------------------------------------
      USE SWF1MODULE
      USE CLN1MODULE, ONLY: NCLNNDS
      USE GLOBAL, ONLY: IOUT,NODES,IFREFM,IUNSTR,&
                       INCLN

      implicit none

      type (MUSG_Project) Modflow
      
      CHARACTER*24 ANAME(3)
      CHARACTER*400 LINE
      DATA ANAME(1) /'           NODES PER SWF'/
      DATA ANAME(2) /'                      IA'/
      DATA ANAME(3) /'                      JA'/
      
      integer :: inswf, ioptfound, lloc, istart, i, in, istop
      real :: r, farea, felev, sgcl, sgcarea, smann, swfh2, swfh1
      integer :: iswfnds, k, ija, ii, iftyp, ifno, isswadi, ifgwno, ifcon, isgwadi
      
      
      IOUT=FNumEco
      INSWF=Modflow.iSWF

      
      
!      DOUBLE PRECISION FRAD
!     ------------------------------------------------------------------
!
!      IF(.NOT.ALLOCATED(NCLNNDS)) THEN
        !ALLOCATE(NCLNNDS)
        NCLNNDS = 0
!      ENDIF
!1------IDENTIFY PACKAGE.
        !INSWF = IUNIT(IUSWF)
        WRITE(IOUT,1)INSWF
    1   FORMAT(1X,/1X,'SWF -- SURFACE WATER FLOW (SWF) DISCRETIZATION ',&
         'PROCESS, VERSION 1, 10/1/2023 INPUT READ FROM UNIT ',I4)
!
!2------ALLOCATE SCALAR VARIABLES AND INITIALIZE.
      ALLOCATE(ISWFCB,ISWFHD,ISWFDD,ISWFIB,NSWFNDS,NSWFGWC,NJA_SWF,&
       ISWFCN,ISWFMB,NSWFTYP)
      ISWFMB = 0
      ALLOCATE(ISWFPCB)
      ALLOCATE(ISWFGWCB)
      ISWFPCB=0
      ALLOCATE(ISWFTIB) !TRANSIENT IBOUND OPTION OF USGS RELEASE
      ISWFTIB=0
!
!3------SWF DIMENSIONING AND OUTPUT OPTIONS, ETC
      CALL URDCOM(INSWF,IOUT,LINE)
!3A-----CHECK FOR OPTIONS KEYWORD AT TOP OF FILE - CURRENTLY NO OPTIONS
      IPRCONN=0
      IOPTFOUND=0
      LLOC=1
      CALL URWORD(LINE,LLOC,ISTART,ISTOP,1,I,R,IOUT,IN)
! CURRENTLY NO OPTIONS - WILL HAVE AN OPTION TO MAKE TOP LAYER AS SWF DOMAIN
!      IF(LINE(ISTART:ISTOP).EQ.'OPTIONS') THEN
!        IOPTFOUND=1
!   70   CALL URWORD(LINE,LLOC,ISTART,ISTOP,1,I,R,IOUT,IN)
!        IF(LINE(ISTART:ISTOP).EQ.'TRANSIENT') THEN
!          ICLNTIB=1
!          WRITE(IOUT,71)
!   71     FORMAT(1X,'TRANSIENT IBOUND OPTION:',
!     1     ' READ TRANSIENT IBOUND RECORDS FOR EACH STRESS PERIOD.')
!        ELSEIF(LINE(ISTART:ISTOP).EQ.'PRINTIAJA') THEN
!          IPRCONN=1
!          WRITE(IOUT,72)
!   72     FORMAT(1X,'PRINT CLN IA AND JA OPTION:',
!     1     ' THE CLN IA AND JA ARRAYS WILL BE PRINTED TO LIST FILE.')
!        ELSEIF(LINE(ISTART:ISTOP).EQ.'PROCESSCCF') THEN                 !aq CLN CCF
!          CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,ICLNGWCB,R,IOUT,INOC)    !aq CLN CCF
!          ICLNPCB=1                                                     !aq CLN CCF
!          WRITE(IOUT,73)                                                !aq CLN CCF
!   73 FORMAT(1X,'PROCESS CELL-TO-CELL FLOW BUDGET OPTION:',             !aq CLN CCF
!     1   ' FLOW BUDGET WILL USE A SEPARATE FILE FOR CLN-GWF FLOW.')     !aq CLN CCF
!C                                                                       !aq CLN CCF
!          IF(ICLNGWCB.LT.0) WRITE(IOUT,18)                              !aq CLN CCF
!   18 FORMAT(1X,'CELL-BY-CELL GWP FLOWS WILL BE PRINTED WHEN ICBCFL',   !aq CLN CCF
!     1  ' IS NOT 0 (FLAG ICLNGWCB IS LESS THAN ZERO)')                  !aq CLN CCF
!          IF(ICLNGWCB.GT.0) WRITE(IOUT,19) ICLNGWCB                     !aq CLN CCF
!   19     FORMAT(1X,'CELL-BY-CELL GWP FLOWS WILL BE SAVED ON UNIT ',I5,     !aq CLN CCF
!     1  '(FLAG ICLNGWCB IS GREATER THAN ZERO)')                         !aq CLN CCF
!        ELSEIF(LINE(ISTART:ISTOP).EQ.'TOPSWF') THEN
!          ISWFTOP=1
!          WRITE(IOUT,74)
!   74 FORMAT(1X,'SURFACE WATER FLOW DOMAIN SAME AS TOP')
!        ELSEIF(LINE(ISTART:ISTOP).EQ.' ') THEN
!          CONTINUE
!        ELSE
!          WRITE(IOUT,79) LINE(ISTART:ISTOP)
!   79     FORMAT(1X,'UNKNOWN OPTION DETECTED: ',A)
!        ENDIF
!        IF(LLOC.LT.200) GO TO 70
!      END IF
      IF(IOPTFOUND.GT.0) CALL URDCOM(INCLN,IOUT,LINE)  ! NO OPTION NO EFFECT
!
      IF(IFREFM.EQ.0) THEN
        READ(LINE,'(8I10)') ISWFNDS,NJA_SWF,NSWFGWC,NSWFTYP,&
         ISWFCB,ISWFHD,ISWFDD,ISWFIB
          LLOC=81
      ELSE
        LLOC=1
        CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,ISWFNDS,R,IOUT,INCLN)
        CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,NJA_SWF,R,IOUT,INCLN)
        CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,NSWFGWC,R,IOUT,INCLN)
        CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,NSWFTYP,R,IOUT,INCLN)
        CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,ISWFCB,R,IOUT,INCLN)
        CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,ISWFHD,R,IOUT,INCLN)
        CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,ISWFDD,R,IOUT,INCLN)
        CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,ISWFIB,R,IOUT,INCLN)
!sp        CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,NRECTYP,R,IOUT,INCLN)
!SP        IF(INBCT.GT.0) THEN
!SP          CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,ICLNCN,R,IOUT,INCLN)
!SP        ENDIF
!ADD----ADD NUMBER OF OTHER CLN NODE TYPES HERE TO CATALOGUE THEM
      END IF
!---------------------------------------------------------------------------
!3A-----REFLECT FLAGS IN OUTPUT LISTING FILE
    !  WRITE(IOUT,3) ISWFNDS,NSWFGWC
    !3 FORMAT(1X,'FLAG (-VE) OR NUMBER OF SWF NODES (+VE)',
    ! 1  1X,'(ISWFNDS) =',I7
    ! 1  /1X,'NUMBER OF SWF NODE TO MATRIX GRID CONNECTIONS',
    ! 1  ' (NSWFGWC) =',I7/)
!
!      IF(ISWFCB.LT.0) WRITE(IOUT,7)
!    7 FORMAT(1X,'CELL-BY-CELL FLOWS WILL BE PRINTED WHEN ICBCFL',
!     1   ' IS NOT 0 (FLAG ICLNCB IS LESS THAN ZERO)')
!      IF(ICLNCB.GT.0) WRITE(IOUT,8) ICLNCB
!    8 FORMAT(1X,'CELL-BY-CELL FLOWS WILL BE SAVED ON UNIT ',I5,
!     1  '(FLAG ICLNCB IS GREATER THAN ZERO)')
!      IF(ICLNCB.EQ.0) WRITE(IOUT,6)
!    6 FORMAT(1X,'CELL-BY-CELL FLOWS WILL NOT BE SAVED OR PRINTED',
!     1  1X,'(FLAG ICLNCB IS EQUAL TO ZERO)')
!
   !   IF(ICLNHD.LT.0) WRITE(IOUT,9)
   ! 9 FORMAT(1X,'CLN HEAD OUTPUT WILL BE SAVED TO THE SAME UNIT',1X,
   !  1   'NUMBER (IHEDUN) AS USED FOR HEAD OUTPUT FOR POROUS MATRIX',   !kkz - added trailing comma per JCH
   !  2   1X,'(FLAG ICLNHD IS LESS THAN ZERO)')
   !   IF(ICLNHD.GT.0) WRITE(IOUT,10) ICLNHD
   !10 FORMAT(1X,'CLN HEAD OUTPUT WILL BE SAVED ON UNIT ',I4,
   !  1  '(FLAG ICLNHD IS GREATER THAN ZERO)')
   !   IF(ICLNHD.EQ.0) WRITE(IOUT,31)
   !31 FORMAT(1X,'CLN HEAD OUTPUT WILL NOT BE SAVED OR PRINTED',
   !  1  1X,'(FLAG ICLNHD IS EQUAL TO ZERO)')
!
   !   IF(ICLNDD.LT.0) WRITE(IOUT,12)
   !12 FORMAT(1X,'CLN DDN OUTPUT WILL BE SAVED TO THE SAME UNIT',1X,
   !  1   'NUMBER (IDDNUN) AS USED FOR DDN OUTPUT FOR POROUS MATRIX',   !kkz - added trailing comma per JCH
   !  2   1X,'(FLAG ICLNDD IS LESS THAN ZERO)')
   !   IF(ICLNDD.GT.0) WRITE(IOUT,13) ICLNDD
   !13 FORMAT(1X,'CLN DDN OUTPUT WILL BE SAVED ON UNIT ',I4,
   !  1  '(FLAG ICLNDD IS GREATER THAN ZERO)')
   !   IF(ICLNDD.EQ.0) WRITE(IOUT,14)
   !14 FORMAT(1X,'CLN DDN OUTPUT WILL NOT BE SAVED OR PRINTED',
   !  1  1X,'(FLAG ICLNDD IS EQUAL TO ZERO)')
!
   !   IF(ICLNIB.LT.0) WRITE(IOUT,32)
   !32 FORMAT(1X,'CLN IBOUND OUTPUT WILL BE SAVED TO THE SAME UNIT',1X,
   !  1   'NUMBER (IBOUUN) AS USED FOR DDN OUTPUT FOR POROUS MATRIX',   !kkz - added trailing comma per JCH
   !  2   1X,'(FLAG ICLNIB IS LESS THAN ZERO)')
   !   IF(ICLNIB.GT.0) WRITE(IOUT,33) ICLNIB
   !33 FORMAT(1X,'CLN IBOUND OUTPUT WILL BE SAVED ON UNIT ',I4,
   !  1  '(FLAG ICLNIB IS GREATER THAN ZERO)')
   !   IF(ICLNIB.EQ.0) WRITE(IOUT,17)
   !17 FORMAT(1X,'CLN IBOUND OUTPUT WILL NOT BE SAVED OR PRINTED',
   !  1  1X,'(FLAG ICLNIB IS EQUAL TO ZERO)')
!
!--------------------------------------------------------------------------------
!C3B----READ GRAVITY AND KINEMATIC VISCOSITY IN CASE IT IS REQUIRED FOR TURBULENT FLOW
!      ALLOCATE(GRAV,VISK)
!C      ALLOCATE(IBHETYP)
!      GRAV = 0.0
!      VISK = 0.0
!C      IBHETYP = 0
!   25 CALL URWORD(LINE,LLOC,ISTART,ISTOP,1,N,R,IOUT,INCLN)
!      IF(LINE(ISTART:ISTOP).EQ.'GRAVITY') THEN
!         CALL URWORD(LINE,LLOC,ISTART,ISTOP,3,I,GRAV,IOUT,IN)
!         WRITE(IOUT,34) GRAV
!34       FORMAT(1X,'GRAVITATIONAL ACCELERATION [L/T^2] = ', G15.6)
!      ELSE IF(LINE(ISTART:ISTOP).EQ.'VISCOSITY') THEN
!         CALL URWORD(LINE,LLOC,ISTART,ISTOP,3,I,VISK,IOUT,IN)
!         WRITE(IOUT,35) VISK
!35       FORMAT(1X,'KINEMATIC VISCOSITY [L^2/T] = ', G15.6)
!      END IF
!3B----READ OPTION FOR NON-CIRCULAR CROSS-SECTIONS
!      IF(LINE(ISTART:ISTOP).EQ.'RECTANGULAR') THEN
!         CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,NRECTYP,R,IOUT,IN)
!         WRITE(IOUT,36) NRECTYP
!36       FORMAT(1X,'NUMBER OF RECTANGULAR SECTION GEOMETRIES = ', I10)
!      ENDIF
!C3C----READ OPTION FOR BHE DETAILS
!      IF(LINE(ISTART:ISTOP).EQ.'BHEDETAIL') THEN
!         IBHETYP = 1
!         IF(ITRNSP.EQ.0) IBHETYP = 0 ! NO BHE (OR INPUT) IF TRANSPORT IS NOT RUN
!         WRITE(IOUT,37)
!37       FORMAT(1X,'BHE DETAILS WILL BE INPUT FOR EACH CLN TYPE')
!      ENDIF
!3D----READ OPTION FOR SAVING CLN OUTPUT AND UNIT NUMBER
!      IF(LINE(ISTART:ISTOP).EQ.'SAVECLNCON') THEN
!         CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,ICLNCN,R,IOUT,IN)
!         IF(INBCT.EQ.0) ICLNCN = 0 ! SHUT OFF IF NO TRANSPORT SIMULATION
!C
!        IF(ICLNCN.LT.0) WRITE(IOUT,42)
!   42   FORMAT(1X,'CLN CONC OUTPUT WILL BE SAVED TO THE SAME UNIT',1X,
!     1   'NUMBER (ISPCUN) AS USED FOR CONC OUTPUT FOR POROUS MATRIX',   !kkz - added trailing comma per JCH
!     2   1X,'(FLAG ICLNCN IS LESS THAN ZERO)')
!        IF(ICLNCN.GT.0) WRITE(IOUT,43) ICLNCN
!   43   FORMAT(1X,'CLN CONC OUTPUT WILL BE SAVED ON UNIT ',I4,
!     1    '(FLAG ICLNCN IS GREATER THAN ZERO)')
!        IF(ICLNCN.EQ.0) WRITE(IOUT,44)
!   44   FORMAT(1X,'CLN CONC OUTPUT WILL NOT BE SAVED OR PRINTED',
!     1    1X,'(FLAG ICLNCN IS EQUAL TO ZERO)')
!      ENDIF
!      IF(LINE(ISTART:ISTOP).EQ.'SAVECLNMAS') THEN
!         CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,ICLNMB,R,IOUT,IN)
!         IF(INBCT.EQ.0) ICLNMB = 0 ! SHUT OFF IF NO TRANSPORT SIMULATION
!C
!        IF(ICLNMB.LT.0) WRITE(IOUT,45)
!   45   FORMAT(1X,'CLN MASS FLUX OUTPUT WILL BE SAVED TO THE SAME',1X,
!     1   'UNIT NUMBER (IBCTCB) AS USED FOR CONC OUTPUT FOR POROUS',   !kkz - added trailing comma per JCH
!     2   1X,'MATRIX (FLAG ICLNMB IS LESS THAN ZERO)')
!        IF(ICLNMB.GT.0) WRITE(IOUT,46) ICLNMB
!   46   FORMAT(1X,'CLN MASS FLUX OUTPUT WILL BE SAVED ON UNIT ',I4,
!     1    '(FLAG ICLNMB IS GREATER THAN ZERO)')
!        IF(ICLNMB.EQ.0) WRITE(IOUT,47)
!   47   FORMAT(1X,'CLN MASS FLUX OUTPUT WILL NOT BE SAVED OR PRINTED',
!     1    1X,'(FLAG ICLNMB IS EQUAL TO ZERO)')
!      ENDIF
!
!      IF(LLOC.LT.200) GO TO 25
!--------------------------------------------------------------------------------
!4------FOR INPUT OF MULTI-NODE WELLS OR CLN SEGMENTS
!4------DIMENSION AND READ ARRAY THAT CONTAINS NUMBER OF NODES PER CLN SEGMENT
!!      IF(NCLN.GT.0)THEN
!!        ALLOCATE(NNDCLN(0:NCLN))
!!        K = 0
!!        CALL U1DINT(NNDCLN(1),ANAME(1),NCLN,K,INCLN,IOUT)
!!        NNDCLN(0) = 0
!!C
!!C5--------MAKE NNDCLN ARRAY CUMULATIVE
!!        DO I = 1,NCLN
!!          NNDCLN(I) = NNDCLN(I) + NNDCLN(I-1)
!!        ENDDO
!!        NCLNCONS = NNDCLN(NCLN)
!!C------------------------------------------------------------------------------
!!C6--------FILL CLNCON WITH CONNECTIVITY OF ADJACENT CLN NODES
!!        IF(ICLNNDS.LT.0)THEN
!!C6A---------FILL CLN CONNECTIONS SEQUENTIALLY WITH GLOBAL NODE NUMBERS
!!          NCLNNDS = NNDCLN(NCLN)
!!          ALLOCATE(CLNCON(NCLNNDS))
!!          DO I=1,NCLNNDS
!!            CLNCON(I) = I ! +NODES  ! (KEEP LOCAL NODE NUMBER)
!!          ENDDO
!!        ELSE
!!C6B-------SET NUMBER OF CLN NODES AND READ CONNECTION ARRAY FOR EACH CLN SEGMENT
!          NSWFNDS = ISWFNDS
!          ALLOCATE(SWFCON(NSWFCONS))
!          DO I=1,NCLN
!C            IF(IFREFM.EQ.0) THEN
!C              READ(INCLN,'(200I10)')
!C     1        (CLNCON(J),J=NNDCLN(I-1)+1,NNDCLN(I))
!C            ELSE
!              READ(INCLN,*) (CLNCON(J),J=NNDCLN(I-1)+1,NNDCLN(I))
!C            ENDIF
!          ENDDO
!!cspC6C---------CONVERT CLN-NODE NUMBER TO GLOBAL NODE NUMBER
!!csp          DO I=1,NCLNCONS
!!csp            CLNCON(I) = NODES + CLNCON(I)
!!csp          ENDDO
!!        ENDIF
!C6D--------CONVERT TO IA_CLN AND JA_CLN
!        ALLOCATE(IA_CLN(NCLNNDS+1))
!        CALL FILLIAJA_CLN
!C6E---------DEALLOCATE UNWANTED ARRAYS
!        DEALLOCATE (NNDCLN) ! NNDCLN NEEDED FOR WRITING BUDGET TO ASCII FILE?
!        DEALLOCATE (CLNCON)
!      ELSE
!----------------------------------------------------------------------
!7------FOR INPUT OF IA AND JAC OF CLN DOMAIN (NCLN = 0), READ DIRECTLY
      NSWFNDS = ISWFNDS
!      ALLOCATE(BOTSWF(NSWFNNS))
!      ALLOCATE(AREASWF(NSWFNNS))
!     READ SWF BOTTOM ELEVATION/AREA - BOTSWF,AREASWF
!      CALL U1DREL8(BOTSWF(1),ANAME(2),NSWFNNS,0,INSWF,IOUT)
!      CALL U1DREL8(AREASWF(1),ANAME(3),NSWFNNS,0,INSWF,IOUT)
!
      ALLOCATE(IA_SWF(NSWFNDS+1))
!7B-------READ CONNECTIONS PER NODE AND CONNECTIVITY AND FILL IA_SWF AND JA_SWF ARRAYS
        K = 0
        CALL U1DINT(IA_SWF,ANAME(2),NSWFNDS,K,INSWF,IOUT)
        ALLOCATE(JA_SWF(NJA_SWF))
        CALL U1DINT(JA_SWF,ANAME(3),NJA_SWF,K,INSWF,IOUT)
!7C--------ENSURE POSITIVE TERM FOR DIAGONAL OF JA_SWF
        DO IJA = 1,NJA_SWF
          IF(JA_SWF(IJA).LT.0) JA_SWF(IJA) = -JA_SWF(IJA)
        ENDDO
!7D--------MAKE IA_SWF CUMULATIVE FROM CONNECTION-PER-NODE
        DO II=2,NSWFNDS+1
          IA_SWF(II) = IA_SWF(II) + IA_SWF(II-1)
        ENDDO
!---------IA_SWF(N+1) IS CUMULATIVE_IA_SWF(N) + 1
        DO II=NSWFNDS+1,2,-1
          IA_SWF(II) = IA_SWF(II-1) + 1
        ENDDO
        IA_SWF(1) = 1
! SWF-GWF CONNECTION

!      ENDIF
!----------------------------------------------------------------------
!8------ALLOCATE SPACE FOR SWF PROPERTY ARRAYS
      ALLOCATE(ASWFNDS(NSWFNDS,4))
      ALLOCATE(ISSWADISWF(NSWFNDS))
      ALLOCATE(ISGWADISWF(NSWFGWC))
!------- ALLOCATE SPACE FOR SWF PROPERTY ARRAYS
!
!      ALLOCATE(MANNSWF(NSWFNNS))
!      ALLOCATE(MICHSWF(NSWFNNS))
!      ALLOCATE(CLSGSWF(NSWFNNS))
!      ALLOCATE(ISSWADISWF(NSWFNNS))
!      ALLOCATE(ISGWADISWF(NSWFNNS))
!      CALL U1DREL8(MANNSWF(1),ANAME(4),NSWFNNS,0,INSWF,IOUT)
!      CALL U1DREL8(MICHSWF(1),ANAME(5),NSWFNNS,0,INSWF,IOUT)
!      CALL U1DINT(ISSWADISWF(1),ANAME(6),NSWFNNS,0,INSWF,IOUT)
!      CALL U1DINT(ISGWADISWF(1),ANAME(7),NSWFNNS,0,INSWF,IOUT)
!
!      ALLOCATE(CLSGSWF(NSWFNNS))
!      CALL U1DREL8(CLSGSWF(1),ANAME(6),NSWFNNS,0,INSWF,IOUT)
!
!9------PREPARE TO REFLECT INPUT PROPERTIES INTO LISTING FILE
!      WRITE(IOUT,21)
!21    FORMAT(/20X,' CONNECTED LINE NETWORK INFORMATION'/
!     1  20X,40('-')/5X,'CLN-NODE NO.',1X,'CLNTYP',1X,'ORIENTATION',2X,
!     1  'CLN LENGTH',4X,'BOT ELEVATION',9X,'FANGLE',9X,'IFLIN',11X,
!     1  'ICCWADI'/5X,11('-'),2X,6('-'),1X,11('-'),1X,11('-'),4X,13('-'),
!     1   4X,11('-'),8X,6('-'),4X,7('-'))
!
!10-------READ BASIC PROPERTIES FOR ALL SWF NODES AND FILL ARRAYS
      DO I = 1,NSWFNDS
        CALL URDCOM(INSWF,IOUT,LINE)
        IF(IFREFM.EQ.0) THEN
          READ(LINE,'(3I10,3F10.3,2I10)') IFNO,IFTYP,FAREA,FELEV,ISSWADI
          LLOC=51
        ELSE
          LLOC=1
          CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,IFNO,R,IOUT,INSWF)
          CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,IFTYP,R,IOUT,INSWF)
          CALL URWORD(LINE,LLOC,ISTART,ISTOP,3,I,FAREA,IOUT,INSWF)
          CALL URWORD(LINE,LLOC,ISTART,ISTOP,3,I,FELEV,IOUT,INSWF)
          CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,ISSWADI,R,IOUT,INSWF)
        END IF
!        WRITE(IOUT,22)IFNO,IFTYP,IFDIR,FLENG,FELEV,FANGLE,IFLIN,ICCWADI
!22      FORMAT(5X,I10,1X,I6,1X,I10,3(1X,E15.6),1X,I10,1X,I10)
!C11B------FILL PROPERTY ARRAYS WITH READ AND PREPARE INFORMATION
        ASWFNDS(I,1) = IFNO + NODES + NCLNNDS ! GLOBAL NODE NUMBER FOR CLN-CELL
        ASWFNDS(I,2) = IFTYP
        ASWFNDS(I,3) = FAREA
        ASWFNDS(I,4) = FELEV
        ISSWADISWF(I) = ISSWADI
      ENDDO
!----------------------------------------------------------------------------------------
!12------ALLOCATE SPACE FOR CLN TO GW PROPERTY ARRAYS
      ALLOCATE(ASWFGWC(NSWFGWC,5))
!----------------------------------------------------------------------------------------
!13------READ CONNECTING SUBSURFACE NODE AND ASSOCIATED PARAMETERS
      IF(IUNSTR.EQ.0)THEN
!
!14A-----FOR STRUCTURED GRID READ SUBSURFACE NODE IN IJK FORMATS
!14A-----AND OTHER CLN SEGMENT PROPERTY INFORMATION
!        CALL SSWF2DIS1SR
      ELSE
!
!14B-----FOR UNSTRUCTURED GRID READ SUBSURFACE NODE NUMBER OF
!14B-----CONNECTION AND OTHER CLN SEGMENT PROPERTY INFORMATION
!        CALL SCLN2DIS1UR
        DO I=1,NSWFGWC
          CALL URDCOM(INSWF,IOUT,LINE)
          LLOC=1
          CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,IFNO,R,IOUT,INSWF)
          CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,IFGWNO,R,IOUT,INSWF)
          CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,IFCON,R,IOUT,INSWF)
          CALL URWORD(LINE,LLOC,ISTART,ISTOP,3,I,SGCL,IOUT,INSWF)
          CALL URWORD(LINE,LLOC,ISTART,ISTOP,3,I,SGCAREA,IOUT,INSWF)
          CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,ISGWADI,R,IOUT,INSWF)
          ASWFGWC(I,1)=IFNO
          ASWFGWC(I,2)=IFGWNO
          ASWFGWC(I,3)=IFCON
          ASWFGWC(I,4)=SGCL
          ASWFGWC(I,5)=SGCAREA
          ISGWADISWF(I)=ISGWADI
        ENDDO
      ENDIF
!
      ALLOCATE(ASWFCOND(NSWFTYP,4))
      DO I=1,NSWFTYP
        CALL URDCOM(INSWF,IOUT,LINE)
        IF(IFREFM.EQ.0) THEN
          READ(LINE,'(I10,3F10.3)') IFNO,SMANN,SWFH1,SWFH2
          LLOC=41
        ELSE
          LLOC=1
          CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,IFNO,R,IOUT,INSWF)
          CALL URWORD(LINE,LLOC,ISTART,ISTOP,3,I,SMANN,IOUT,INSWF)
          CALL URWORD(LINE,LLOC,ISTART,ISTOP,3,I,SWFH1,IOUT,INSWF)
          CALL URWORD(LINE,LLOC,ISTART,ISTOP,3,I,SWFH2,IOUT,INSWF)
        ENDIF
        ASWFCOND(I,1)=IFNO
        ASWFCOND(I,2)=SMANN
        ASWFCOND(I,3)=SWFH1
        ASWFCOND(I,4)=SWFH2
      ENDDO

!----------------------------------------------------------------------------------------
!C15A------ALLOCATE SPACE AND FILL PROPERTIES FOR ALL CONDUIT TYPE CLNs
!      IF(NCONDUITYP.GT.0)THEN
!        CALL SCLN2COND1RP
!      ENDIF
!C----------------------------------------------------------------------------------------
!C15B------ALLOCATE SPACE AND FILL PROPERTIES FOR ALL CONDUIT TYPE CLNs
!      IF(NRECTYP.GT.0)THEN
!        CALL SCLN2REC1RP
!      ENDIF
!----------------------------------------------------------------------------------------
!16------ALLOCATE SPACE AND FILL PROPERTIES FOR OTHER CLN TYPES HERE
!ADD------ADD OTHER CLN TYPE READ AND PREPARE INFORMATION HERE
!----------------------------------------------------------------------------------------

! ---------------------------------------------------------------------------------------
!C16------IF IPRCONN THEN WRITE CLN CONNECTIVITY TO THE OUTPUT FILE
!      IF(IPRCONN.NE.0)THEN
!        WRITE(IOUT,'(/,A)')' IA_CLN IS BELOW, 40I10'
!        WRITE(IOUT,55)(IA_CLN(I),I=1,NCLNNDS+1)
!        WRITE(IOUT,*)'NJA_CLN = ',NJA_CLN
!        WRITE(IOUT,*)'JA_CLN IS BELOW, 40I10'
!        WRITE(IOUT,55)(JA_CLN(J),J=1,NJA_CLN)
!55      FORMAT(40I10)
!      ENDIF
!----------------------------------------------------------------------------------------
!C18-------FOR ANGLED PIPE, IF DEPTH OF FLOW IS LESS THAN DIAMETER MAKE HORIZONTAL
!      WRITE(IOUT,*)
!      DO I = 1,NCLNNDS
!        IFDIR = ACLNNDS(I,3)
!        IF(IFDIR.EQ.2)THEN
!          IFTYP =  ACLNNDS(I,2)
!          FLENG = ACLNNDS(I,4)
!          FANGLE = ACLNNDS(I,6)
!          FDPTH = FLENG * SIN(FANGLE)
!          IC=IFTYP
!          CALL CLNR(IC,FRAD)
!          IF(FDPTH.LT.2.0*FRAD) THEN
!              IFDIR = 1
!              ACLNNDS(I,3) = IFDIR
!              IFNO = ACLNNDS(I,1) - NODES
!              WRITE(IOUT,222)IFNO
!          ENDIF
!        ENDIF
!222      FORMAT(5X,'ANGLED CLN CELL NO', I7,' MADE HORIZONTAL')
!      ENDDO
!17-----RETURN
      RETURN
      END SUBROUTINE MUSG_ReadSWF

    
    subroutine MUSG_ReadCLN_pt2(Modflow)
        implicit none

        type (MUSG_Project) Modflow
        character(400) :: line
        
        integer :: IFNO, i
        real :: FRAD
        real :: CONDUITK
        integer :: LLOC
        real :: TCOND
        real :: TTHK
        real :: TCOEF
        real :: TCFLUID
        integer :: ISTOP
        real :: R, r1, r2, r3
        integer :: ISTART
        real :: FSRAD
        real*8 :: AREAF
        real*8 :: PERIF
        
        IOUT=FNumEco

        !----------------------------------------------------------------------------------------
        !12------ALLOCATE SPACE FOR CONDUIT TYPE CLNs AND PREPARE TO REFLECT INPUT TO LISTING FILE
        ALLOCATE (ACLNCOND(NCONDUITYP,5))
        ALLOCATE (BHEPROP(NCONDUITYP,4))
        BHEPROP = 0.0
        IF(IBHETYP.EQ.0) THEN
            WRITE(IOUT,62)
62          FORMAT(/20X,' CONDUIT NODE INFORMATION'/&
            20X,40('-')/5X,'CONDUIT NODE',8X,'RADIUS',3X,'CONDUIT SAT K',&
            /5X,12('-'),8X,6('-'),3X,13('-'))
            
        ELSE
            WRITE(IOUT,63)
63          FORMAT(/20X,' CONDUIT NODE INFORMATION'/&
            20X,40('-')/5X,'CONDUIT NODE',8X,'RADIUS',3X,'CONDUIT SAT K',&
            5X,'COND. PIPE', 6X,'THICK PIPE',5X,'COND. FLUID',&
            5X,'CONV. COEFF',&
            /5X,12('-'),4X,10('-'),3X,13('-'))
            
        ENDIF
        !13------READ CONDUIT PROPERTIES FOR EACH CONDUIT TYPE
        DO I=1,NCONDUITYP
            CALL URDCOM(INCLN,IOUT,LINE)
            IF(IFREFM.EQ.0) THEN
                IF(IBHETYP.EQ.0)THEN
                    READ(LINE,'(I10,2F10.3)') IFNO,FRAD,CONDUITK
                    LLOC=71
                ELSE
                    READ(LINE,'(I10,6F10.3)')IFNO,FRAD,CONDUITK,TCOND,TTHK,&
                    TCFLUID,TCOEF
                    LLOC=111
                ENDIF
            ELSE
                LLOC=1
                CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,IFNO,R,IOUT,INCLN)
                CALL URWORD(LINE,LLOC,ISTART,ISTOP,3,I,FSRAD,IOUT,INCLN)
                CALL URWORD(LINE,LLOC,ISTART,ISTOP,3,I,CONDUITK,IOUT,INCLN)
                
                !rgm looks like young-jin added a line of data here
                read(INCLN,'(a)') line
                istart=0
                istop=0
                LLOC=1
                CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,IFNO,R,IOUT,INCLN)
                CALL URWORD(LINE,LLOC,ISTART,ISTOP,3,I,r1,IOUT,INCLN)
                CALL URWORD(LINE,LLOC,ISTART,ISTOP,3,I,r2,IOUT,INCLN)
                CALL URWORD(LINE,LLOC,ISTART,ISTOP,3,I,r3,IOUT,INCLN)
                
                
                
                FRAD = FSRAD
                IF(IBHETYP.EQ.1)THEN
                    CALL URWORD(LINE,LLOC,ISTART,ISTOP,3,I,TCOND,IOUT,INCLN)
                    CALL URWORD(LINE,LLOC,ISTART,ISTOP,3,I,TTHK,IOUT,INCLN)
                    CALL URWORD(LINE,LLOC,ISTART,ISTOP,3,I,TCFLUID,IOUT,INCLN)
                    CALL URWORD(LINE,LLOC,ISTART,ISTOP,3,I,TCOEF,IOUT,INCLN)
                ENDIF
            END IF
            !
            !14--------FILL PROPERTY ARRAYS WITH READ AND PREPARE INFORMATION
            ACLNCOND(I,1) = IFNO
            ACLNCOND(I,2) = FRAD
            ACLNCOND(I,3) = CONDUITK
            CALL CLNA(IFNO,AREAF)
            ACLNCOND(I,4) = AREAF
            CALL CLNP(I,PERIF)
            ACLNCOND(I,5) = PERIF
            IF(IBHETYP.EQ.1)THEN
                BHEPROP(I,1) = TCOND
                BHEPROP(I,2) = TTHK
                BHEPROP(I,3) = TCFLUID
                BHEPROP(I,4) = TCOEF
                WRITE(IOUT,26) IFNO,FRAD,CONDUITK,TCOND,TTHK,TCFLUID,TCOEF
26              FORMAT(5X,I10,6(1X,E15.6))
                
            ELSE
                WRITE(IOUT,25)IFNO,FRAD,CONDUITK
25              FORMAT(5X,I10,2(1X,E15.6))
                
                WRITE(IOUT,67)
67              FORMAT(/20X,' new CONDUIT NODE INFORMATION'/&
                20X,40('-')/5X,'CONDUIT NODE?',8X,'r1',3X,'r2',3X,'r3',&
                /5X,12('-'),8X,6('-'),3X,13('-'),3X,13('-'))

                
            ENDIF
        ENDDO
                
        continue
    end subroutine MUSG_ReadCLN_pt2
    
    SUBROUTINE CLNA(IC,AREAF)
        !--------COMPUTE X-SECTIONAL FLOW AREA FOR NODE
        USE CLN1MODULE, ONLY:  ACLNCOND,NCONDUITYP,ACLNREC,NRECTYP
        DOUBLE PRECISION AREAF,RADFSQ
        integer :: ic
        real :: pi
        integer :: icl
        !--------------------------------------------------------------------------------------
        PI = 3.1415926
        IF(IC.LE.NCONDUITYP)THEN
            !1-------CLN NODE IS A CONDUIT
            RADFSQ = ACLNCOND(IC,2)**2
            AREAF = PI * RADFSQ
        ELSEIF(IC.GT.NCONDUITYP.AND.IC.LE.NCONDUITYP+NRECTYP)THEN
            ICL = IC - NCONDUITYP
            AREAF = ACLNREC(ICL,2) * ACLNREC(ICL,3)
        ELSEIF(IC.GT.NCONDUITYP+NRECTYP)then
            !2------ADD COMPUTATION FOR AREA FOR OTHER CLN TYPES HERE
            !ADD      ADD COMPUTATION FOR AREA FOR OTHER TYPES OF CLNs HERE
        ENDIF
        !7------RETURN
        RETURN
    END SUBROUTINE CLNA
    
    SUBROUTINE CLNP(IC,FPER)
        !--------COMPUTE EFFECTIVE PERIMETER FOR CONNECTION OF CLN SEGMENT TO 3-D GRID
        USE CLN1MODULE, ONLY: ACLNCOND,NCONDUITYP,ACLNREC,NRECTYP
        DOUBLE PRECISION FPER
        integer :: ic
        real :: pi
        integer :: icl
        !--------------------------------------------------------------------------------------
        PI = 3.1415926
        IF(IC.LE.NCONDUITYP)THEN
            !1-------CLN NODE IS A CONDUIT
            FPER = 2 * PI * ACLNCOND(IC,2)
        ELSEIF(IC.GT.NCONDUITYP.AND.IC.LE.NCONDUITYP+NRECTYP)THEN
            !1B---------CLN1 NODE IS A RECTANGULAR SECTION
            ICL = IC - NCONDUITYP
            FPER = 2 * (ACLNREC(ICL,2) + ACLNREC(ICL,3))
        ELSEIF(IC.GT.NCONDUITYP+NRECTYP)THEN
            !2------ADD COMPUTATION FOR PERIMETER FOR OTHER CLN TYPES HERE
            !ADD      ADD COMPUTATION FOR PERIMETER FOR OTHER CLN TYPES HERE
        ENDIF
        !7------RETURN
        RETURN
    END SUBROUTINE CLNP


   
    subroutine MUSG_WriteVolumeBudgetToTecplot(Modflow)
        implicit none

        type (MUSG_Project) Modflow
        
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

        

        FNameTecplot=Modflow.MUTPrefix(:len_trim(Modflow.MUTPrefix))//'o.'//Modflow.Prefix(:len_trim(Modflow.Prefix))//'.VolumeBudget.tecplot.dat'
        call OpenAscii(FNumTecplot,FNameTecplot)
        call Msg( 'To File: '//trim(FNameTecplot))

        write(FNumTecplot,*) 'Title = "Modflow Volume Budget"'

        DoVars=.true.
        
        FNum=Modflow.iLIST
        rewind(FNUM)

        do 
            read(FNum,'(a)',iostat=status) line
            if(status /= 0) return
            
            if(index(line,'MODEL TIME UNIT IS').gt.0) then
                l1=index(line,'MODEL TIME UNIT IS')
                Modflow.Tunits=line(l1+19:)
                var_line='VARIABLES = "TOTAL TIME'//'('//trim(adjustl(Modflow.Tunits))//')",'

                continue
                
            else if(index(line,'MODEL LENGTH UNIT IS').gt.0) then
                l1=index(line,'MODEL LENGTH UNIT IS')
                Modflow.Lunits=line(l1+21:)
                

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
                        if(index(Modflow.Tunits,'SECONDS').gt.0) then
                            read(line(l1+10:),*) TotalTime
                        elseif(index(Modflow.Tunits,'MINUTES').gt.0) then
                            read(line(l1+10:),*) dum1, TotalTime
                        elseif(index(Modflow.Tunits,'HOURS').gt.0) then
                            read(line(l1+10:),*) dum1, dum2, TotalTime
                        elseif(index(Modflow.Tunits,'DAYS').gt.0) then
                            read(line(l1+10:),*) dum1, dum2, dum3, TotalTime
                        elseif(index(Modflow.Tunits,'YEARS').gt.0) then
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
                    write(output_line,'(a)')  'zone t="Project: '//Modflow.Prefix(:len_trim(Modflow.Prefix))//'"'
 
                    TMPStr=', AUXDATA TimeUnits = "'//Modflow.Tunits(:len_trim(Modflow.Tunits))//'"'
                    l1=len_trim(output_line)+1
                    write(output_line(l1:),'(a)')	TMPStr                 

                    TMPStr=', AUXDATA LengthUnits = "'//Modflow.Lunits(:len_trim(Modflow.Lunits))//'"'
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
    
    subroutine MUSG_CreateStepPeriodTimeFile(Modflow)
        implicit none

        type (MUSG_Project) Modflow
        
        integer :: Fnum
        integer :: FNumStepPeriodTime
        character(MAXLBL) :: FNameStepPeriodTime
        
        
        integer :: iTStep
        integer :: iPeriod
        real(dr) :: TotalTime
        real(dr) :: dum1, dum2, dum3, dum4
        
        character(4000) :: line

        FNum=Modflow.iLIST
        rewind(FNum)

        FNameStepPeriodTime=Modflow.MUTPrefix(:len_trim(Modflow.MUTPrefix))//'o.StepPeriodTime'
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
                        if(index(Modflow.Tunits,'SECONDS').gt.0) then
                            read(line(l1+10:),*) TotalTime
                        elseif(index(Modflow.Tunits,'MINUTES').gt.0) then
                            read(line(l1+10:),*) dum1, TotalTime
                        elseif(index(Modflow.Tunits,'HOURS').gt.0) then
                            read(line(l1+10:),*) dum1, dum2, TotalTime
                        elseif(index(Modflow.Tunits,'DAYS').gt.0) then
                            read(line(l1+10:),*) dum1, dum2, dum3, TotalTime
                        elseif(index(Modflow.Tunits,'YEARS').gt.0) then
                            read(line(l1+10:),*) dum1, dum2, dum3, dum4, TotalTime
                        endif
                        exit loop
                    endif
                end do loop
                
                write(FNumStepPeriodTime,*) iTStep, iPeriod, TotalTime

            end if
                

        end do
                
    end subroutine MUSG_CreateStepPeriodTimeFile
    
    subroutine MUSG_GWF_HDS_DDN_ToTecplot(Modflow)
        implicit none
        type (MUSG_Project) Modflow

        integer :: Fnum
        character(MAXLBL) :: FName
        integer :: i, j
        character(4000) :: output_line


      !  if(.not. Modflow.gwf.have_mesh) then
		    !call ErrMsg('ERROR: no mesh information')
		    !stop
      !  endif
        
        ! tecplot output file
        FName=Modflow.MUTPrefix(:len_trim(Modflow.MUTPrefix))//'o.'//Modflow.Prefix(:len_trim(Modflow.Prefix))//'.GWF.HDS_DDN.tecplot.dat'
        call OpenAscii(FNum,FName)
        call Msg( 'To File: '//trim(FName))

        write(FNum,*) 'Title = "Modflow HDS and DDN (Saturation) file Outputs "'

        write(FNum,'(a)') 'variables="X","Y","Z","Layer","Hydraulic Head","Saturation"'
        
        write(output_line,'(a,f20.4,a,i8,a,i8,a)')'ZONE t="GWF" SOLUTIONTIME=',modflow.TIMOT(1),',N=',Modflow.gwf.nvertex,', E=',Modflow.gwf.nCell,', datapacking=block, &
            zonetype=febrick, VARLOCATION=([4,5,6]=CELLCENTERED)'
        
        TMPStr=', AUXDATA TimeUnits = "'//Modflow.Tunits(:len_trim(Modflow.Tunits))//'"'
        l1=len_trim(output_line)+1
        write(output_line(l1:),'(a)')	TMPStr                 

        TMPStr=', AUXDATA LengthUnits = "'//Modflow.Lunits(:len_trim(Modflow.Lunits))//'"'
        l1=len_trim(output_line)+1
        write(output_line(l1:),'(a)')	TMPStr                 

        l1=len_trim(output_line)+1
        write(FNum,'(a)') output_line(:l1)

        write(FNum,'(a)') '# x'
        write(FNum,'(5e20.12)') (Modflow.gwf.x(i),i=1,Modflow.gwf.nvertex)
        write(FNum,'(a)') '# y'
        write(FNum,'(5e20.12)') (Modflow.gwf.y(i),i=1,Modflow.gwf.nvertex)
        write(FNum,'(a)') '# z'
        write(FNum,'(5e20.12)') (Modflow.gwf.z(i),i=1,Modflow.gwf.nvertex)
        write(FNum,'(a)') '# layer'
        write(FNum,'(5i8)') (Modflow.gwf.lay(i),i=1,Modflow.gwf.nCell)
        write(FNum,'(a)') '# head'
        write(FNum,'(5e20.12)') (Modflow.gwf.head(i,1),i=1,Modflow.gwf.nCell)
        write(FNum,'(a)') '# saturation'
        write(FNum,'(5e20.12)') (Modflow.gwf.sat(i,1),i=1,Modflow.gwf.nCell)
        
        do i=1,Modflow.gwf.nCell
            write(FNum,'(8i8)') (Modflow.gwf.ivertex(j,i),j=1,Modflow.gwf.m)
        end do
       
        
        do j=2,Modflow.ntime
            write(FNum,'(a,f20.4,a,i8,a,i8,a)')'ZONE t="GWF" SOLUTIONTIME=',modflow.TIMOT(j),',N=',Modflow.gwf.nvertex,', E=',Modflow.gwf.nCell,', datapacking=block, &
            zonetype=febrick, VARLOCATION=([4,5,6]=CELLCENTERED), VARSHARELIST=([1,2,3,4,]), CONNECTIVITYSHAREZONE=1 '
            write(FNum,'(a)') '# head'
            write(FNum,'(5e20.12)') (Modflow.gwf.head(i,j),i=1,Modflow.gwf.nCell)
            write(FNum,'(a)') '# sat'
            write(FNum,'(5e20.12)') (Modflow.gwf.sat(i,j),i=1,Modflow.gwf.nCell)
        enddo
        
        call FreeUnit(FNum)

    end subroutine MUSG_GWF_HDS_DDN_ToTecplot
    
    subroutine MUSG_GWF_IBOUND_ToTecplot(Modflow)
        implicit none
        type (MUSG_Project) Modflow

        integer :: Fnum
        character(MAXLBL) :: FName
        integer :: i, j

      !  if(.not. Modflow.gwf.have_mesh) then
		    !call ErrMsg('ERROR: no mesh information')
		    !stop
      !  endif
        
        ! tecplot output file
        FName=Modflow.MUTPrefix(:len_trim(Modflow.MUTPrefix))//'o.'//Modflow.Prefix(:len_trim(Modflow.Prefix))//'.GWF.IBOUND.tecplot.dat'
        call OpenAscii(FNum,FName)
        call Msg( 'To File: '//trim(FName))

        write(FNum,*) 'Title = "Modflow IBOUND file Outputs "'

        write(FNum,'(a)') 'variables="X","Y","Z","IBOUND"'
        
        write(FNum,'(a,f20.4,a,i8,a,i8,a)')'ZONE t="GWF" SOLUTIONTIME=',modflow.TIMOT(1),',N=',Modflow.gwf.nvertex,', E=',Modflow.gwf.nCell,', datapacking=block, &
            zonetype=febrick, VARLOCATION=([4]=CELLCENTERED)'

        write(FNum,'(a)') '# x'
        write(FNum,'(5e20.12)') (Modflow.gwf.x(i),i=1,Modflow.gwf.nvertex)
        write(FNum,'(a)') '# y'
        write(FNum,'(5e20.12)') (Modflow.gwf.y(i),i=1,Modflow.gwf.nvertex)
        write(FNum,'(a)') '# z'
        write(FNum,'(5e20.12)') (Modflow.gwf.z(i),i=1,Modflow.gwf.nvertex)
        write(FNum,'(a)') '# IBOUND'
        write(FNum,'(5i5)') (IBOUND(i),i=1,Modflow.gwf.nCell)
        
        do i=1,Modflow.gwf.nCell
            write(FNum,'(8i8)') (Modflow.gwf.ivertex(j,i),j=1,Modflow.gwf.m)
        end do
       
   
        
        call FreeUnit(FNum)

    end subroutine MUSG_GWF_IBOUND_ToTecplot

    subroutine MUSG_GWF_IBOUNDv2_ToTecplot(Modflow)
        implicit none
        type (MUSG_Project) Modflow

        integer :: Fnum
        character(MAXLBL) :: FName
        integer :: i

       
        ! tecplot output file
        FName=Modflow.MUTPrefix(:len_trim(Modflow.MUTPrefix))//'o.'//Modflow.Prefix(:len_trim(Modflow.Prefix))//'.GWF.IBOUNDv2.tecplot.dat'
        call OpenAscii(FNum,FName)
        call Msg( 'To File: '//trim(FName))

        write(FNum,*) 'Title = "Modflow IBOUND file Outputs "'

        write(FNum,'(a)') 'variables="X","Y","Z","IBOUND"'
        
        write(FNum,'(a)')'ZONE t="GWF IBOUND v2" '

        write(FNum,'(a)') '# x, y, z, ibound'
        do i=1, Modflow.gwf.nCell
            write(FNum,'(5e20.12)') Modflow.gwf.xcell(i),Modflow.gwf.ycell(i),Modflow.gwf.zcell(i),IBOUND(i)
        end do
   
        
        call FreeUnit(FNum)

    end subroutine MUSG_GWF_IBOUNDv2_ToTecplot
    
    subroutine MUSG_CLN_HDS_DDN_ToTecplot(Modflow)
        implicit none
        type (MUSG_Project) Modflow

        integer :: Fnum
        character(MAXLBL) :: FName
        integer :: i, j
        character(4000) :: output_line

      !  if(.not. Modflow.cln.have_mesh) then
		    !call ErrMsg('ERROR: no mesh information')
		    !stop
      !  endif
        
        ! tecplot output file
        FName=Modflow.MUTPrefix(:len_trim(Modflow.MUTPrefix))//'o.'//Modflow.Prefix(:len_trim(Modflow.Prefix))//'.CLN.HDS_DDN.tecplot.dat'
        call OpenAscii(FNum,FName)
        call Msg( 'To File: '//trim(FName))

        write(FNum,*) 'Title = "'//Modflow.Prefix(:len_trim(Modflow.Prefix))//': CLN Heads and drawdown (saturation)"'

        write(FNum,'(a)') 'variables="X","Y","Z","Layer","Hydraulic Head","Saturation"'
        
        write(output_line,'(a,f20.4,a,i8,a,i8,a)')'ZONE t="CLN" SOLUTIONTIME=',modflow.TIMOT(1),',N=',Modflow.cln.nvertex,', E=',Modflow.cln.nCell,', datapacking=block, &
            zonetype=felineseg, VARLOCATION=([4,5,6]=CELLCENTERED)'
        
        TMPStr=', AUXDATA TimeUnits = "'//Modflow.Tunits(:len_trim(Modflow.Tunits))//'"'
        l1=len_trim(output_line)+1
        write(output_line(l1:),'(a)')	TMPStr                 

        TMPStr=', AUXDATA LengthUnits = "'//Modflow.Lunits(:len_trim(Modflow.Lunits))//'"'
        l1=len_trim(output_line)+1
        write(output_line(l1:),'(a)')	TMPStr                 

        l1=len_trim(output_line)+1
        write(FNum,'(a)') output_line(:l1)
        
        
        write(FNum,'(a)') '# x'
        write(FNum,'(5e20.12)') (Modflow.cln.x(i),i=1,Modflow.cln.nvertex)
        write(FNum,'(a)') '# y'
        write(FNum,'(5e20.12)') (Modflow.cln.y(i),i=1,Modflow.cln.nvertex)
        write(FNum,'(a)') '# z'
        write(FNum,'(5e20.12)') (Modflow.cln.z(i),i=1,Modflow.cln.nvertex)
        write(FNum,'(a)') '# layer'
        write(FNum,'(5i8)') (Modflow.cln.lay(i),i=1,Modflow.cln.nCell)
        write(FNum,'(a)') '# head'
        write(FNum,'(5e20.12)') (Modflow.cln.head(i,1),i=1,Modflow.cln.nCell)
        write(FNum,'(a)') '# saturation'
        write(FNum,'(5e20.12)') (Modflow.cln.sat(i,1),i=1,Modflow.cln.nCell)
        
        do i=1,Modflow.cln.nCell
            write(FNum,'(8i8)') (Modflow.cln.ivertex(j,i),j=1,Modflow.cln.m)
        end do
       
        
        do j=2,Modflow.ntime
            write(FNum,'(a,f20.4,a,i8,a,i8,a)')'ZONE t="CLN" SOLUTIONTIME=',modflow.TIMOT(j),',N=',Modflow.cln.nvertex,', E=',Modflow.cln.nCell,', datapacking=block, &
            zonetype=felineseg, VARLOCATION=([4,5,6]=CELLCENTERED), VARSHARELIST=([1,2,3,4,]), CONNECTIVITYSHAREZONE=1 '
            write(FNum,'(a)') '# head'
            write(FNum,'(5e20.12)') (Modflow.cln.head(i,j),i=1,Modflow.cln.nCell)
            write(FNum,'(a)') '# sat'
            write(FNum,'(5e20.12)') (Modflow.cln.sat(i,j),i=1,Modflow.cln.nCell)
        enddo
        
        call FreeUnit(FNum)

    end subroutine MUSG_CLN_HDS_DDN_ToTecplot
    
    subroutine MUSG_SWF_HDS_DDN_ToTecplot(Modflow)
        implicit none
        type (MUSG_Project) Modflow

        integer :: Fnum
        character(MAXLBL) :: FName
        integer :: i, j
        character(4000) :: output_line

      !  if(.not. Modflow.swf.have_mesh) then
		    !call ErrMsg('ERROR: no mesh information')
		    !stop
      !  endif
        
        ! tecplot output file
        FName=Modflow.MUTPrefix(:len_trim(Modflow.MUTPrefix))//'o.'//Modflow.Prefix(:len_trim(Modflow.Prefix))//'.SWF.HDS_DDN.tecplot.dat'
        call OpenAscii(FNum,FName)
        call Msg( 'To File: '//trim(FName))

        write(FNum,*) 'Title = "'//Modflow.Prefix(:len_trim(Modflow.Prefix))//': SWF Heads and drawdown (saturation)"'

        write(FNum,'(a)') 'variables="X","Y","Z","Layer","Hydraulic Head","Saturation"'
        
        write(output_line,'(a,f20.4,a,i8,a,i8,a)')'ZONE t="SWF" SOLUTIONTIME=',modflow.TIMOT(1),',N=',Modflow.swf.nvertex,', E=',Modflow.swf.nCell,', datapacking=block, &
            zonetype=fequadrilateral, VARLOCATION=([4,5,6]=CELLCENTERED)'
        
        TMPStr=', AUXDATA TimeUnits = "'//Modflow.Tunits(:len_trim(Modflow.Tunits))//'"'
        l1=len_trim(output_line)+1
        write(output_line(l1:),'(a)')	TMPStr                 

        TMPStr=', AUXDATA LengthUnits = "'//Modflow.Lunits(:len_trim(Modflow.Lunits))//'"'
        l1=len_trim(output_line)+1
        write(output_line(l1:),'(a)')	TMPStr                 

        l1=len_trim(output_line)+1
        write(FNum,'(a)') output_line(:l1)
        
        
        
        write(FNum,'(a)') '# x'
        write(FNum,'(5e20.12)') (Modflow.swf.x(i),i=1,Modflow.swf.nvertex)
        write(FNum,'(a)') '# y'
        write(FNum,'(5e20.12)') (Modflow.swf.y(i),i=1,Modflow.swf.nvertex)
        write(FNum,'(a)') '# z'
        write(FNum,'(5e20.12)') (Modflow.swf.z(i),i=1,Modflow.swf.nvertex)
        write(FNum,'(a)') '# layer'
        write(FNum,'(5i8)') (Modflow.swf.lay(i),i=1,Modflow.swf.nCell)
        write(FNum,'(a)') '# head'
        write(FNum,'(5e20.12)') (Modflow.swf.head(i,1),i=1,Modflow.swf.nCell)
        write(FNum,'(a)') '# saturation'
        write(FNum,'(5e20.12)') (Modflow.swf.sat(i,1),i=1,Modflow.swf.nCell)
        
        do i=1,Modflow.swf.nCell
            write(FNum,'(8i8)') (Modflow.swf.ivertex(j,i),j=1,Modflow.swf.m)
        end do
       
        
        do j=2,Modflow.ntime
            write(FNum,'(a,f20.4,a,i8,a,i8,a)')'ZONE t="SWF" SOLUTIONTIME=',modflow.TIMOT(j),',N=',Modflow.swf.nvertex,', E=',Modflow.swf.nCell,', datapacking=block, &
            zonetype=fequadrilateral, VARLOCATION=([4,5,6]=CELLCENTERED), VARSHARELIST=([1,2,3,4,]), CONNECTIVITYSHAREZONE=1 '
            write(FNum,'(a)') '# head'
            write(FNum,'(5e20.12)') (Modflow.swf.head(i,j),i=1,Modflow.swf.nCell)
            write(FNum,'(a)') '# sat'
            write(FNum,'(5e20.12)') (Modflow.swf.sat(i,j),i=1,Modflow.swf.nCell)
        enddo
        
        call FreeUnit(FNum)

    end subroutine MUSG_SWF_HDS_DDN_ToTecplot

    subroutine MUSG_GWF_CBB_ToTecplot(Modflow)
        implicit none
        type (MUSG_Project) Modflow

        integer :: Fnum
        character(MAXLBL) :: FName

        integer :: i, j, nvar

      !  if(.not. Modflow.gwf.have_mesh) then
		    !call ErrMsg('ERROR: no mesh information')
		    !stop
      !  endif
        
        ! tecplot output file
        FName=Modflow.MUTPrefix(:len_trim(Modflow.MUTPrefix))//'o.'//Modflow.Prefix(:len_trim(Modflow.Prefix))//'.GWF.CBB.tecplot.dat'
        call OpenAscii(FNum,FName)
        call Msg( 'To File: '//trim(FName))

        write(FNum,*) 'Title = "Modflow CBB file Outputs "'

        VarSTR='variables="X","Y","Z","Layer",'
        nVar=4
        if(allocated(Modflow.gwf.cbb_STORAGE)) then
            VarSTR=VarSTR(:len_trim(VarSTR))//'"Storage",'
            nVar=nVar+1
        endif
        if(allocated(Modflow.gwf.cbb_CONSTANT_HEAD)) then
            VarSTR=VarSTR(:len_trim(VarSTR))//'"Constant head",'
            nVar=nVar+1
        endif
        if(allocated(Modflow.gwf.cbb_DRAINS)) then
            VarSTR=VarSTR(:len_trim(VarSTR))//'"Drains",'
            nVar=nVar+1
        endif
        if(allocated(Modflow.gwf.cbb_RECHARGE)) then
            VarSTR=VarSTR(:len_trim(VarSTR))//'"Recharge",'
            nVar=nVar+1
        endif
        
        write(FNum,'(a)') VarSTR(:len_trim(VarSTR))
            
        write(ZoneSTR,'(a,f20.4,a,i8,a,i8,a)')'ZONE t="CBB" SOLUTIONTIME=',modflow.TIMOT(1),',N=',Modflow.gwf.nvertex,', E=',Modflow.gwf.nCell,', datapacking=block, &
            zonetype=febrick'
        
        CellCenteredSTR=', VARLOCATION=([4'
        if(nVar.ge.5) then
            do j=5,nVar
                write(str1,'(i1)') j
                CellCenteredSTR=CellCenteredSTR(:len_trim(CellCenteredSTR))//','//str1
            end do
        endif
        CellCenteredSTR=CellCenteredSTR(:len_trim(CellCenteredSTR))//']=CELLCENTERED)'
        
        write(FNum,'(a)') ZoneSTR(:len_trim(ZoneSTR))//CellCenteredSTR(:len_trim(CellCenteredSTR))//&
            ', AUXDATA TimeUnits = "'//Modflow.Tunits(:len_trim(Modflow.Tunits))//'"'//&
            ', AUXDATA LengthUnits = "'//Modflow.Lunits(:len_trim(Modflow.Lunits))//'"'

        write(FNum,'(a)') '# x'
        write(FNum,'(5e20.12)') (Modflow.gwf.x(i),i=1,Modflow.gwf.nvertex)
        write(FNum,'(a)') '# y'
        write(FNum,'(5e20.12)') (Modflow.gwf.y(i),i=1,Modflow.gwf.nvertex)
        write(FNum,'(a)') '# z'
        write(FNum,'(5e20.12)') (Modflow.gwf.z(i),i=1,Modflow.gwf.nvertex)
        write(FNum,'(a)') '# layer'
        write(FNum,'(5i8)') (Modflow.gwf.lay(i),i=1,Modflow.gwf.nCell)
        if(allocated(Modflow.gwf.cbb_STORAGE)) then
            write(FNum,'(a)') '# storage'
            write(FNum,'(5e20.12)') (Modflow.gwf.cbb_STORAGE(i,1),i=1,Modflow.gwf.nCell)
        endif
        if(allocated(Modflow.gwf.cbb_CONSTANT_HEAD)) then
            write(FNum,'(a)') '# constant head'
            write(FNum,'(5e20.12)') (Modflow.gwf.cbb_CONSTANT_HEAD(i,1),i=1,Modflow.gwf.nCell)
        endif        
        if(allocated(Modflow.gwf.cbb_DRAINS)) then
            write(FNum,'(a)') '# drains'
            write(FNum,'(5e20.12)') (Modflow.gwf.cbb_DRAINS(i,1),i=1,Modflow.gwf.nCell)
        endif        
        if(allocated(Modflow.gwf.cbb_RECHARGE)) then
            write(FNum,'(a)') '# recharge'
            write(FNum,'(5e20.12)') (Modflow.gwf.cbb_RECHARGE(i,1),i=1,Modflow.gwf.nCell)
        endif        
        
        do i=1,Modflow.gwf.nCell
            write(FNum,'(8i8)') (Modflow.gwf.ivertex(j,i),j=1,Modflow.gwf.m)
        end do
       
        do j=2,Modflow.ntime
            write(FNum,'(a,f20.4,a,i8,a,i8,a)')'ZONE t="CBB" SOLUTIONTIME=',modflow.TIMOT(j),',N=',Modflow.gwf.nvertex,', E=',Modflow.gwf.nCell,', datapacking=block, &
            zonetype=febrick,'//CellCenteredSTR(:len_trim(CellCenteredSTR))//', VARSHARELIST=([1,2,3,4,]), CONNECTIVITYSHAREZONE=1 '
            if(allocated(Modflow.gwf.cbb_STORAGE)) then
                write(FNum,'(a)') '# storage'
                write(FNum,'(5e20.12)') (Modflow.gwf.cbb_STORAGE(i,j),i=1,Modflow.gwf.nCell)
            endif
            if(allocated(Modflow.gwf.cbb_CONSTANT_HEAD)) then
                write(FNum,'(a)') '# constant head'
                write(FNum,'(5e20.12)') (Modflow.gwf.cbb_CONSTANT_HEAD(i,j),i=1,Modflow.gwf.nCell)
            endif        
            if(allocated(Modflow.gwf.cbb_DRAINS)) then
                write(FNum,'(a)') '# drains'
                write(FNum,'(5e20.12)') (Modflow.gwf.cbb_DRAINS(i,j),i=1,Modflow.gwf.nCell)
            endif        
            if(allocated(Modflow.gwf.cbb_RECHARGE)) then
                write(FNum,'(a)') '# recharge'
                write(FNum,'(5e20.12)') (Modflow.gwf.cbb_RECHARGE(i,j),i=1,Modflow.gwf.nCell)
            endif        
        enddo
        
        call FreeUnit(FNum)

    end subroutine MUSG_GWF_CBB_ToTecplot
    
    subroutine MUSG_CLN_CBB_ToTecplot(Modflow)
        implicit none
        type (MUSG_Project) Modflow

        integer :: Fnum
        character(MAXLBL) :: FName

        integer :: i, j, nvar

      !  if(.not. Modflow.cln.have_mesh) then
		    !call ErrMsg('ERROR: no mesh information')
		    !stop
      !  endif
        
        ! tecplot output file
        FName=Modflow.MUTPrefix(:len_trim(Modflow.MUTPrefix))//'o.'//Modflow.Prefix(:len_trim(Modflow.Prefix))//'.CLN.CBB.tecplot.dat'
        call OpenAscii(FNum,FName)
        call Msg( 'To File: '//trim(FName))

        write(FNum,*) 'Title = "Modflow CBB file Outputs "'

        VarSTR='variables="X","Y","Z","Layer",'
        nVar=4
        if(allocated(Modflow.cln.cbb_STORAGE)) then
            VarSTR=VarSTR(:len_trim(VarSTR))//'"Storage",'
            nVar=nVar+1
        endif
        if(allocated(Modflow.cln.cbb_CONSTANT_HEAD)) then
            VarSTR=VarSTR(:len_trim(VarSTR))//'"Constant head",'
            nVar=nVar+1
        endif
        if(allocated(Modflow.cln.cbb_DRAINS)) then
            VarSTR=VarSTR(:len_trim(VarSTR))//'"Drains",'
            nVar=nVar+1
        endif
        if(allocated(Modflow.cln.cbb_RECHARGE)) then
            VarSTR=VarSTR(:len_trim(VarSTR))//'"Recharge",'
            nVar=nVar+1
        endif
        
        write(FNum,'(a)') VarSTR(:len_trim(VarSTR))
            
        write(ZoneSTR,'(a,f20.4,a,i8,a,i8,a)')'ZONE t="CBB" SOLUTIONTIME=',modflow.TIMOT(1),',N=',Modflow.cln.nvertex,', E=',Modflow.cln.nCell,', datapacking=block, &
            zonetype=felineseg'
        
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
        write(FNum,'(5e20.12)') (Modflow.cln.x(i),i=1,Modflow.cln.nvertex)
        write(FNum,'(a)') '# y'
        write(FNum,'(5e20.12)') (Modflow.cln.y(i),i=1,Modflow.cln.nvertex)
        write(FNum,'(a)') '# z'
        write(FNum,'(5e20.12)') (Modflow.cln.z(i),i=1,Modflow.cln.nvertex)
        write(FNum,'(a)') '# layer'
        write(FNum,'(5i8)') (Modflow.cln.lay(i),i=1,Modflow.cln.nCell)
        if(allocated(Modflow.cln.cbb_STORAGE)) then
            write(FNum,'(a)') '# storage'
            write(FNum,'(5e20.12)') (Modflow.cln.cbb_STORAGE(i,1),i=1,Modflow.cln.nCell)
        endif
        if(allocated(Modflow.cln.cbb_CONSTANT_HEAD)) then
            write(FNum,'(a)') '# constant head'
            write(FNum,'(5e20.12)') (Modflow.cln.cbb_CONSTANT_HEAD(i,1),i=1,Modflow.cln.nCell)
        endif        
        if(allocated(Modflow.cln.cbb_DRAINS)) then
            write(FNum,'(a)') '# drains'
            write(FNum,'(5e20.12)') (Modflow.cln.cbb_DRAINS(i,1),i=1,Modflow.cln.nCell)
        endif        
        if(allocated(Modflow.cln.cbb_RECHARGE)) then
            write(FNum,'(a)') '# recharge'
            write(FNum,'(5e20.12)') (Modflow.cln.cbb_RECHARGE(i,1),i=1,Modflow.cln.nCell)
        endif        
        
        do i=1,Modflow.cln.nCell
            write(FNum,'(8i8)') (Modflow.cln.ivertex(j,i),j=1,Modflow.cln.m)
        end do
       
        do j=2,Modflow.ntime
            write(FNum,'(a,f20.4,a,i8,a,i8,a)')'ZONE t="CBB" SOLUTIONTIME=',modflow.TIMOT(j),',N=',Modflow.cln.nvertex,', E=',Modflow.cln.nCell,', datapacking=block, &
            zonetype=felineseg,'//CellCenteredSTR(:len_trim(CellCenteredSTR))//', VARSHARELIST=([1,2,3,4,]), CONNECTIVITYSHAREZONE=1 '
            if(allocated(Modflow.cln.cbb_STORAGE)) then
                write(FNum,'(a)') '# storage'
                write(FNum,'(5e20.12)') (Modflow.cln.cbb_STORAGE(i,j),i=1,Modflow.cln.nCell)
            endif
            if(allocated(Modflow.cln.cbb_CONSTANT_HEAD)) then
                write(FNum,'(a)') '# constant head'
                write(FNum,'(5e20.12)') (Modflow.cln.cbb_CONSTANT_HEAD(i,j),i=1,Modflow.cln.nCell)
            endif        
            if(allocated(Modflow.cln.cbb_DRAINS)) then
                write(FNum,'(a)') '# drains'
                write(FNum,'(5e20.12)') (Modflow.cln.cbb_DRAINS(i,j),i=1,Modflow.cln.nCell)
            endif        
            if(allocated(Modflow.cln.cbb_RECHARGE)) then
                write(FNum,'(a)') '# recharge'
                write(FNum,'(5e20.12)') (Modflow.cln.cbb_RECHARGE(i,j),i=1,Modflow.cln.nCell)
            endif        
        enddo
        
        call FreeUnit(FNum)

    end subroutine MUSG_CLN_CBB_ToTecplot
    
    subroutine MUSG_SWF_CBB_ToTecplot(Modflow)
        implicit none
        type (MUSG_Project) Modflow

        integer :: Fnum
        character(MAXLBL) :: FName

        integer :: i, j, nvar

      !  if(.not. Modflow.swf.have_mesh) then
		    !call ErrMsg('ERROR: no mesh information')
		    !stop
      !  endif
        
        ! tecplot output file
        FName=Modflow.MUTPrefix(:len_trim(Modflow.MUTPrefix))//'o.'//Modflow.Prefix(:len_trim(Modflow.Prefix))//'.SWF.CBB.tecplot.dat'
        call OpenAscii(FNum,FName)
        call Msg( 'To File: '//trim(FName))

        write(FNum,*) 'Title = "Modflow CBB file Outputs "'

        VarSTR='variables="X","Y","Z","Layer",'
        nVar=4
        if(allocated(Modflow.swf.cbb_STORAGE)) then
            VarSTR=VarSTR(:len_trim(VarSTR))//'"Storage",'
            nVar=nVar+1
        endif
        if(allocated(Modflow.swf.cbb_CONSTANT_HEAD)) then
            VarSTR=VarSTR(:len_trim(VarSTR))//'"Constant head",'
            nVar=nVar+1
        endif
        if(allocated(Modflow.swf.cbb_DRAINS)) then
            VarSTR=VarSTR(:len_trim(VarSTR))//'"Drains",'
            nVar=nVar+1
        endif
        if(allocated(Modflow.swf.cbb_RECHARGE)) then
            VarSTR=VarSTR(:len_trim(VarSTR))//'"Recharge",'
            nVar=nVar+1
        endif
        
        write(FNum,'(a)') VarSTR(:len_trim(VarSTR))
            
        write(ZoneSTR,'(a,f20.4,a,i8,a,i8,a)')'ZONE t="CBB" SOLUTIONTIME=',modflow.TIMOT(1),',N=',Modflow.swf.nvertex,', E=',Modflow.swf.nCell,', datapacking=block, &
            zonetype=fequadrilateral'
        
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
        write(FNum,'(5e20.12)') (Modflow.swf.x(i),i=1,Modflow.swf.nvertex)
        write(FNum,'(a)') '# y'
        write(FNum,'(5e20.12)') (Modflow.swf.y(i),i=1,Modflow.swf.nvertex)
        write(FNum,'(a)') '# z'
        write(FNum,'(5e20.12)') (Modflow.swf.z(i),i=1,Modflow.swf.nvertex)
        write(FNum,'(a)') '# layer'
        write(FNum,'(5i8)') (Modflow.swf.lay(i),i=1,Modflow.swf.nCell)
        if(allocated(Modflow.swf.cbb_STORAGE)) then
            write(FNum,'(a)') '# storage'
            write(FNum,'(5e20.12)') (Modflow.swf.cbb_STORAGE(i,1),i=1,Modflow.swf.nCell)
        endif
        if(allocated(Modflow.swf.cbb_CONSTANT_HEAD)) then
            write(FNum,'(a)') '# constant head'
            write(FNum,'(5e20.12)') (Modflow.swf.cbb_CONSTANT_HEAD(i,1),i=1,Modflow.swf.nCell)
        endif        
        if(allocated(Modflow.swf.cbb_DRAINS)) then
            write(FNum,'(a)') '# drains'
            write(FNum,'(5e20.12)') (Modflow.swf.cbb_DRAINS(i,1),i=1,Modflow.swf.nCell)
        endif        
        if(allocated(Modflow.swf.cbb_RECHARGE)) then
            write(FNum,'(a)') '# recharge'
            write(FNum,'(5e20.12)') (Modflow.swf.cbb_RECHARGE(i,1),i=1,Modflow.swf.nCell)
        endif        
        
        do i=1,Modflow.swf.nCell
            write(FNum,'(8i8)') (Modflow.swf.ivertex(j,i),j=1,Modflow.swf.m)
        end do
       
        do j=2,Modflow.ntime
            write(FNum,'(a,f20.4,a,i8,a,i8,a)')'ZONE t="CBB" SOLUTIONTIME=',modflow.TIMOT(j),',N=',Modflow.swf.nvertex,', E=',Modflow.swf.nCell,', datapacking=block, &
            zonetype=fequadrilateral,'//CellCenteredSTR(:len_trim(CellCenteredSTR))//', VARSHARELIST=([1,2,3,4,]), CONNECTIVITYSHAREZONE=1 '
            if(allocated(Modflow.swf.cbb_STORAGE)) then
                write(FNum,'(a)') '# storage'
                write(FNum,'(5e20.12)') (Modflow.swf.cbb_STORAGE(i,j),i=1,Modflow.swf.nCell)
            endif
            if(allocated(Modflow.swf.cbb_CONSTANT_HEAD)) then
                write(FNum,'(a)') '# constant head'
                write(FNum,'(5e20.12)') (Modflow.swf.cbb_CONSTANT_HEAD(i,j),i=1,Modflow.swf.nCell)
            endif        
            if(allocated(Modflow.swf.cbb_DRAINS)) then
                write(FNum,'(a)') '# drains'
                write(FNum,'(5e20.12)') (Modflow.swf.cbb_DRAINS(i,j),i=1,Modflow.swf.nCell)
            endif        
            if(allocated(Modflow.swf.cbb_RECHARGE)) then
                write(FNum,'(a)') '# recharge'
                write(FNum,'(5e20.12)') (Modflow.swf.cbb_RECHARGE(i,j),i=1,Modflow.swf.nCell)
            endif        
        enddo
        
        call FreeUnit(FNum)

    end subroutine MUSG_SWF_CBB_ToTecplot
    

   
   

    subroutine MUSG_Read_GWF_GSF(Modflow)
        implicit none

        type (MUSG_Project) Modflow

        integer :: i, j
        
        integer :: i1, i2
        real :: r1, r2, r3
        
        itmp=Modflow.iGSF
        
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

        read(itmp,*) Modflow.gwf.meshtype
        read(itmp,*) Modflow.gwf.nCell, Modflow.gwf.nLay, Modflow.gwf.iz, Modflow.gwf.ic
        read(itmp,*) Modflow.gwf.nvertex
        allocate(Modflow.gwf.x(Modflow.gwf.nvertex),Modflow.gwf.y(Modflow.gwf.nvertex),Modflow.gwf.z(Modflow.gwf.nvertex), stat=ialloc)
        call AllocChk(ialloc,'gwf xyzvertex arrays')
        Modflow.gwf.x = 0 ! automatic initialization
        Modflow.gwf.y = 0 ! automatic initialization
        Modflow.gwf.z = 0 ! automatic initialization
        
        read(itmp,*) (Modflow.gwf.x(i),Modflow.gwf.y(i),Modflow.gwf.z(i),i=1,Modflow.gwf.nvertex)

        ! determine the number of nodes per cell (Modflow.gwf.m)
        read(itmp,*) i1,r1,r2,r3,i2,Modflow.gwf.m
        backspace(itmp)
        allocate(Modflow.gwf.ivertex(Modflow.gwf.m,Modflow.gwf.nCell),stat=ialloc)
        allocate(Modflow.gwf.xCell(Modflow.gwf.nCell),Modflow.gwf.yCell(Modflow.gwf.nCell),Modflow.gwf.zCell(Modflow.gwf.nCell),Modflow.gwf.lay(Modflow.gwf.nCell),stat=ialloc)
        call AllocChk(ialloc,'gwf ivertex, xyzcell arrays')
        Modflow.gwf.ivertex = 0 ! automatic initialization
        do i=1,Modflow.gwf.nCell
        read(itmp,*) i1,Modflow.gwf.xCell(i),Modflow.gwf.yCell(i),Modflow.gwf.zCell(i),Modflow.gwf.lay(i),i2,(Modflow.gwf.ivertex(j,i),j=1,Modflow.gwf.m)
        end do
	    call freeunit(itmp)
        
        Modflow.gwf.have_mesh=.true.
        write(TmpSTR,'(i10)') Modflow.gwf.nCell 
        call Msg('nCell: '//TmpSTR)

	    return
    end subroutine MUSG_Read_GWF_GSF
    
    subroutine MUSG_Read_CLN_GSF(Modflow)
        implicit none

        type (MUSG_Project) Modflow

        integer :: i, j
        
        integer :: i1, i2
        real :: r1, r2, r3
        
        itmp=Modflow.iCLN_GSF
        
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

        read(itmp,*) Modflow.cln.meshtype
        read(itmp,*) Modflow.cln.nCell, Modflow.cln.nLay, Modflow.cln.iz, Modflow.cln.ic
        read(itmp,*) Modflow.cln.nvertex
        allocate(Modflow.cln.x(Modflow.cln.nvertex),Modflow.cln.y(Modflow.cln.nvertex),Modflow.cln.z(Modflow.cln.nvertex), stat=ialloc)
        call AllocChk(ialloc,'cln xyzvertex arrays')
        Modflow.cln.x = 0 ! automatic initialization
        Modflow.cln.y = 0 ! automatic initialization
        Modflow.cln.z = 0 ! automatic initialization
        
        read(itmp,*) (Modflow.cln.x(i),Modflow.cln.y(i),Modflow.cln.z(i),i=1,Modflow.cln.nvertex)

        ! determine the number of nodes per cell (Modflow.cln.m)
        read(itmp,*) i1,r1,r2,r3,i2,Modflow.cln.m
        backspace(itmp)
        allocate(Modflow.cln.ivertex(Modflow.cln.m,Modflow.cln.nCell),stat=ialloc)
        allocate(Modflow.cln.xCell(Modflow.cln.nCell),Modflow.cln.yCell(Modflow.cln.nCell),Modflow.cln.zCell(Modflow.cln.nCell),Modflow.cln.lay(Modflow.cln.nCell),stat=ialloc)
        call AllocChk(ialloc,'cln ivertex, xyzcell arrays')
        Modflow.cln.ivertex = 0 ! automatic initialization
        do i=1,Modflow.cln.nCell
        read(itmp,*) i1,Modflow.cln.xCell(i),Modflow.cln.yCell(i),Modflow.cln.zCell(i),Modflow.cln.lay(i),i2,(Modflow.cln.ivertex(j,i),j=1,Modflow.cln.m)
        end do
	    call freeunit(itmp)
        
        Modflow.cln.have_mesh=.true.

        Modflow.cln.have_mesh=.true.
        write(TmpSTR,'(i10)') Modflow.cln.nCell 
        call Msg('nCell: '//TmpSTR)


	    return
    end subroutine MUSG_Read_CLN_GSF

    subroutine MUSG_Read_SWF_GSF(Modflow)
        implicit none

        type (MUSG_Project) Modflow

        integer :: i, j
        
        integer :: i1, i2
        real :: r1, r2, r3
        
        itmp=Modflow.iSWF_GSF
        
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

        read(itmp,*) Modflow.swf.meshtype
        read(itmp,*) Modflow.swf.nCell, Modflow.swf.nLay, Modflow.swf.iz, Modflow.swf.ic
        read(itmp,*) Modflow.swf.nvertex
        allocate(Modflow.swf.x(Modflow.swf.nvertex),Modflow.swf.y(Modflow.swf.nvertex),Modflow.swf.z(Modflow.swf.nvertex), stat=ialloc)
        call AllocChk(ialloc,'swf xyzvertex arrays')
        Modflow.swf.x = 0 ! automatic initialization
        Modflow.swf.y = 0 ! automatic initialization
        Modflow.swf.z = 0 ! automatic initialization
        
        read(itmp,*) (Modflow.swf.x(i),Modflow.swf.y(i),Modflow.swf.z(i),i=1,Modflow.swf.nvertex)
        
        ! determine the number of nodes per cell (Modflow.swf.m)
        read(itmp,*) i1,r1,r2,r3,i2,Modflow.swf.m
        backspace(itmp)
        allocate(Modflow.swf.ivertex(Modflow.swf.m,Modflow.swf.nCell),stat=ialloc)
        allocate(Modflow.swf.xCell(Modflow.swf.nCell),Modflow.swf.yCell(Modflow.swf.nCell),Modflow.swf.zCell(Modflow.swf.nCell),Modflow.swf.lay(Modflow.swf.nCell),stat=ialloc)
        call AllocChk(ialloc,'swf ivertex, xyzcell arrays')
        
        Modflow.swf.ivertex = 0 ! automatic initialization
        do i=1,Modflow.swf.nCell
            read(itmp,*) i1,Modflow.swf.xCell(i),Modflow.swf.yCell(i),Modflow.swf.zCell(i),Modflow.swf.lay(i),i2,(Modflow.swf.ivertex(j,i),j=1,Modflow.swf.m)
        end do
	    call freeunit(itmp)
        
        Modflow.swf.have_mesh=.true.

        Modflow.swf.have_mesh=.true.
        write(TmpSTR,'(i10)') Modflow.swf.nCell 
        call Msg('nCell: '//TmpSTR)


	    return
    end subroutine MUSG_Read_SWF_GSF
   
    subroutine MUSG_ScanFile(FNum,Modflow)
        implicit none

        type (MUSG_Project) Modflow
        
        integer :: Fnum
        integer :: i
     
        character(MAXSTRING) :: line
        character(MAXSTRING) :: PossibleKey
        
    
        do 
            read(FNum,'(a)',iostat=status) line
            call lcase(line)
            if(status /= 0) exit
            
            if(line(1:1).eq.'#') then
                write(Modflow.iSCAN,'(a)',iostat=status) line(:len_trim(line))
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
                call AddToScan(PossibleKey, Modflow)
                exit
            end do
        end do
        
        rewind(FNum)    
            
    end subroutine MUSG_ScanFile

    subroutine AddToScan(PKey, Modflow)
        implicit none
        
        
        type (MUSG_Project) Modflow

        character(*) :: PKey
        
        !if(Modflow.nKeyWord .gt. 0) then
        !    if(Modflow.Keyword(Modflow.nKeyWord) .eq. PKey) then
        !        !write(Modflow.iSCAN,'(a)',iostat=status) 'Repeat: '// PKey(:len_trim(PKey))
        !        return
        !    end if
        !end if
        
        Modflow.nKeyWord=Modflow.nKeyWord+1
        if(Modflow.nKeyWord>Modflow.nDim) call GrowKeywordArray(Modflow,Modflow.nDim)
        Modflow.Keyword(Modflow.nKeyWord)=PKey
        write(Modflow.iSCAN,'(a)',iostat=status) Modflow.Keyword(Modflow.nKeyWord)

        
        
    end subroutine AddToScan
    
     subroutine GrowKeyWordArray(Modflow,ndim) !--- during run if necessary 
        type (MUSG_Project) Modflow
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
		    Keyword_tmp(i)	=	Modflow.Keyword(i)
	    end do

	    ! destroy arrays
	    deallocate(Modflow.Keyword)
	    ! reallocate
	    allocate(Modflow.Keyword(ndim_new), stat=ialloc)
	    call AllocChk(ialloc,'reallocate Modflow.Keyword arrays')
	    Modflow.Keyword(:)=char(0)

	    ! copy current data
	    do i=1,ndim
		    Modflow.Keyword(i)	=	Keyword_tmp(i)	
	    end do

	    ndim=ndim_new
	    
	    deallocate(Keyword_tmp)

    end subroutine GrowKeyWordArray
    
    
    subroutine URWORD(line,icol,istart,istop,ncode,n,r,iout,in)
        !c     ******************************************************************
        !c     routine to extract a word from a line of text, and optionally
        !c     convert the word to a number.
        !c        istart and istop will be returned with the starting and
        !c          ending character positions of the word.
        !c        the last character in the line is set to blank so that if any
        !c          problems occur with finding a word, istart and istop will
        !c          point to this blank character.  thus, a word will always be
        !c          returned unless there is a numeric conversion error.  be sure
        !c          that the last character in line is not an important character
        !c          because it will always be set to blank.
        !c        a word starts with the first character that is not a space or
        !c          comma, and ends when a subsequent character that is a space
        !c          or comma.  note that these parsing rules do not treat two
        !c          commas separated by one or more spaces as a null word.
        !c        for a word that begins with "'", the word starts with the
        !c          character after the quote and ends with the character
        !c          preceding a subsequent quote.  thus, a quoted word can
        !c          include spaces and commas.  the quoted word cannot contain
        !c          a quote character.
        !c        if ncode is 1, the word is converted to upper case.
        !c        if ncode is 2, the word is converted to an integer.
        !c        if ncode is 3, the word is converted to a real number.
        !c        number conversion error is written to unit iout if iout is
        !c          positive; error is written to default output if iout is 0;
        !c          no error message is written if iout is negative.
        !c     ******************************************************************
        !c

        use ifport
        implicit none

        !        SPECIFICATIONS:
        !     ------------------------------------------------------------------
        CHARACTER*(*) LINE
        CHARACTER*20 STRING
        CHARACTER*30 RW
        CHARACTER*1 TAB

        ! rgm modified ...
        integer :: len
        integer :: linlen
        integer :: in
        integer :: icol
        integer :: iout
        real :: r
        integer :: istart
        integer :: istop
        integer :: n
        integer :: ii, jj, kk
        integer :: ncode
        integer :: l
        integer :: idiff
        

        
        IOUT=FNumEco
        !... to here

        !     ------------------------------------------------------------------
        TAB=CHAR(9)
        !
        !1------Set last char in LINE to blank and set ISTART and ISTOP to point
        !1------to this blank as a default situation when no word is found.  If
        !1------starting location in LINE is out of bounds, do not look for a
        !1------word.
        LINLEN=LEN(LINE)
        LINE(LINLEN:LINLEN)=' '
        ISTART=LINLEN
        ISTOP=LINLEN
        LINLEN=LINLEN-1
        IF(ICOL.LT.1 .OR. ICOL.GT.LINLEN) GO TO 100
        !
        !2------Find start of word, which is indicated by first character that
        !2------is not a blank, a comma, or a tab.
        DO 10 II=ICOL,LINLEN
        IF(LINE(II:II).NE.' ' .AND. LINE(II:II).NE.','&
        .AND. LINE(II:II).NE.TAB) GO TO 20
        10    CONTINUE
        ICOL=LINLEN+1
        GO TO 100
        !
        !3------Found start of word.  Look for end.
        !3A-----When word is quoted, only a quote can terminate it.
        20    IF(LINE(II:II).EQ.'''') THEN
            II=II+1
            IF(II.LE.LINLEN) THEN
                DO 25 JJ=II,LINLEN
                    IF(LINE(JJ:JJ).EQ.'''') GO TO 40
                25          CONTINUE
            END IF
            !
            !3B-----When word is not quoted, space, comma, or tab will terminate.
        ELSE
            DO 30 JJ=II,LINLEN
                IF(LINE(JJ:JJ).EQ.' ' .OR. LINE(JJ:JJ).EQ.','&
                .OR. LINE(JJ:JJ).EQ.TAB) GO TO 40
            30       CONTINUE
        END IF
        !
        !3C-----End of line without finding end of word; set end of word to
        !3C-----end of line.
        JJ=LINLEN+1
        !
        !4------Found end of word; set JJ to point to last character in WORD and
        !-------set ICOL to point to location for scanning for another word.
        40    ICOL=JJ+1
        JJ=JJ-1
        IF(JJ.LT.II) GO TO 100
        ISTART=II
        ISTOP=JJ
        !
        !5------Convert word to upper case and RETURN if NCODE is 1.
        IF(NCODE.EQ.1) THEN
            IDIFF=ICHAR('a')-ICHAR('A')
            DO 50 KK=ISTART,ISTOP
                IF(LINE(KK:KK).GE.'a' .AND. LINE(KK:KK).LE.'z')&
                        LINE(KK:KK)=CHAR(ICHAR(LINE(KK:KK))-IDIFF)
            50       CONTINUE
            !IF (IOUT.GT.0)  write(iout,*) '$$$ urword string: ',&
            !LINE(ISTART:ISTOP)

            RETURN
        END IF
        !
        !6------Convert word to a number if requested.
        100   IF(NCODE.EQ.2 .OR. NCODE.EQ.3) THEN
            RW=' '
            L=30-ISTOP+ISTART
            IF(L.LT.1) GO TO 200
            RW(L:30)=LINE(ISTART:ISTOP)
            IF(NCODE.EQ.2) READ(RW,'(I30)',ERR=200) N
            !IF (IOUT.GT.0 .and. NCODE.EQ.2 )  write(iout,*)&
                !'$$$ urword integer: ',N
            IF(NCODE.EQ.3) READ(RW,'(F30.0)',ERR=200) R
            !IF (IOUT.GT.0 .and. NCODE.EQ.3 )  write(iout,*)&
                !'$$$ urword real: ',R
        END IF
        RETURN
        !
        !7------Number conversion error.
        200   IF(NCODE.EQ.3) THEN
            STRING= 'A REAL NUMBER'
            L=13
        ELSE
            STRING= 'AN INTEGER'
            L=10
        END IF
        !
        !7A-----If output unit is negative, set last character of string to 'E'.
        IF(IOUT.LT.0) THEN
            N=0
            R=0.
            LINE(LINLEN+1:LINLEN+1)='E'
            RETURN
            !
            !7B-----If output unit is positive; write a message to output unit.
        ELSE IF(IOUT.GT.0) THEN
            IF(IN.GT.0) THEN
                WRITE(IOUT,201) IN,LINE(ISTART:ISTOP),STRING(1:L),LINE
            ELSE
                WRITE(IOUT,202) LINE(ISTART:ISTOP),STRING(1:L),LINE
            END IF
            201      FORMAT(1X,/1X,'FILE UNIT ',I4,' : ERROR CONVERTING "',A,&
            '" TO ',A,' IN LINE:',/1X,A)
            202      FORMAT(1X,/1X,'KEYBOARD INPUT : ERROR CONVERTING "',A,&
            '" TO ',A,' IN LINE:',/1X,A)
            !
            !7C-----If output unit is 0; write a message to default output.
        ELSE
            IF(IN.GT.0) THEN
                WRITE(*,201) IN,LINE(ISTART:ISTOP),STRING(1:L),LINE
            ELSE
                WRITE(*,202) LINE(ISTART:ISTOP),STRING(1:L),LINE
            END IF
        END IF
        !
        !7D-----STOP after writing message.
        CALL USTOP(' ')
    end subroutine URWORD
    
    subroutine URWORD8(line,icol,istart,istop,ncode,n,r,iout,in)
        !c     ******************************************************************
        !c     routine to extract a word from a line of text, and optionally
        !c     convert the word to a number.
        !c        istart and istop will be returned with the starting and
        !c          ending character positions of the word.
        !c        the last character in the line is set to blank so that if any
        !c          problems occur with finding a word, istart and istop will
        !c          point to this blank character.  thus, a word will always be
        !c          returned unless there is a numeric conversion error.  be sure
        !c          that the last character in line is not an important character
        !c          because it will always be set to blank.
        !c        a word starts with the first character that is not a space or
        !c          comma, and ends when a subsequent character that is a space
        !c          or comma.  note that these parsing rules do not treat two
        !c          commas separated by one or more spaces as a null word.
        !c        for a word that begins with "'", the word starts with the
        !c          character after the quote and ends with the character
        !c          preceding a subsequent quote.  thus, a quoted word can
        !c          include spaces and commas.  the quoted word cannot contain
        !c          a quote character.
        !c        if ncode is 1, the word is converted to upper case.
        !c        if ncode is 2, the word is converted to an integer.
        !c        if ncode is 3, the word is converted to a real number.
        !c        number conversion error is written to unit iout if iout is
        !c          positive; error is written to default output if iout is 0;
        !c          no error message is written if iout is negative.
        !c     ******************************************************************
        !c
        !c        specifications:
        !c     ------------------------------------------------------------------
        use ifport
        implicit none

    
        character*(*) line
        character*20 string
        character*30 rw
        character*1 tab
        
        ! rgm modified ...
        integer :: len
        integer :: linlen
        integer :: in
        integer :: icol
        integer :: iout
        real*8 :: r
        integer :: istart
        integer :: istop
        integer :: n
        integer :: ii, jj, kk
        integer :: ncode
        integer :: l
        integer :: idiff
        
        !... to here

        
        !c     ------------------------------------------------------------------
        tab=char(9)
        !c
        !c1------set last char in line to blank and set istart and istop to point
        !c1------to this blank as a default situation when no word is found.  if
        !c1------starting location in line is out of bounds, do not look for a
        !c1------word.
        linlen=len(line)
        line(linlen:linlen)=' '
        istart=linlen
        istop=linlen
        linlen=linlen-1
        if(icol.lt.1 .or. icol.gt.linlen) go to 100
        !c
        !c2------find start of word, which is indicated by first character that
        !c2------is not a blank, a comma, or a tab.
        do 10 ii=icol,linlen
            if(line(ii:ii).ne.' ' .and. line(ii:ii).ne.',' .and. line(ii:ii).ne.tab) go to 20
10      continue
        icol=linlen+1
        go to 100
        !c
        !c3------found start of word.  look for end.
        !c3a-----when word is quoted, only a quote can terminate it.
20      if(line(ii:ii).eq.'''') then
            ii=ii+1
            if(ii.le.linlen) then
                do 25 JJ=ii,linlen
                    if(line(JJ:JJ).eq.'''') go to 40
25              continue
            end if
        !c
        !c3b-----when word is not quoted, space, comma, or tab will terminate.
        else
            do 30 JJ=ii,linlen
                if(line(JJ:JJ).eq.' ' .or. line(JJ:JJ).eq.',' .or. line(JJ:JJ).eq.tab) go to 40
30          continue
        end if
        !c
        !c3c-----end of line without finding end of word; set end of word to
        !c3c-----end of line.
        JJ=linlen+1
        !c
        !c4------found end of word; set JJ to point to last character in word and
        !c-------set icol to point to location for scanning for another word.
40      icol=JJ+1
        JJ=JJ-1
        if(JJ.lt.ii) go to 100
        istart=ii
        istop=JJ
        !c
        !c5------convert word to upper case and return if ncode is 1.
         if(ncode.eq.1) then
            idiff=ichar('a')-ichar('a')
            do 50 KK=istart,istop
                if(line(KK:KK).ge.'a' .and. line(KK:KK).le.'z') line(KK:KK)=char(ichar(line(KK:KK))-idiff)
50          continue
            return
         end if
        !c
        !c6------convert word to a number if requested.
100     if(ncode.eq.2 .or. ncode.eq.3) then
            rw=' '
            l=30-istop+istart
            if(l.lt.1) go to 200
            rw(l:30)=line(istart:istop)
            if(ncode.eq.2) read(rw,'(i30)',err=200) n
            IF (IOUT.GT.0 .and. NCODE.EQ.2 )  write(iout,*)&
                '$$$ urword integer: ',N
            if(ncode.eq.3) read(rw,'(f30.0)',err=200) r
            IF (IOUT.GT.0 .and. NCODE.EQ.3 )  write(iout,*)&
                '$$$ urword real: ',R
        end if
        return
        !c
        !c7------number conversion error.
200     if(ncode.eq.3) then
            string= 'a real number'
            l=13
        else
            string= 'an integer'
            l=10
        end if
        !c
        !c7a-----if output unit is negative, set last character of string to 'e'.
        if(iout.lt.0) then
            n=0
            r=0.
            line(linlen+1:linlen+1)='e'
            return
        !c
        !c7b-----if output unit is positive; write a message to output unit.
        else if(iout.gt.0) then
            if(in.gt.0) then
                write(iout,201) in,line(istart:istop),string(1:l),line
            else
                write(iout,202) line(istart:istop),string(1:l),line
            end if
201         format(1x,/1x,'file unit ',i4,' : error converting "',a,'" to ',a,' in line:',/1x,a)
202         format(1x,/1x,'keyboard input : error converting "',a,'" to ',a,' in line:',/1x,a)
        !c
        !c7c-----if output unit is 0; write a message to default output.
        else
            if(in.gt.0) then
                write(*,201) in,line(istart:istop),string(1:l),line
            else
                write(*,202) line(istart:istop),string(1:l),line
            end if
        end if
        !c
        !c7d-----stop after writing message.
        call ustop(' ')
    end subroutine URWORD8

    subroutine ustop(stopmess)
    !c     ******************************************************************
    !c     stop program, with option to print message before stopping
    !c     ******************************************************************
    !c        specifications:
    !c     ------------------------------------------------------------------
        character stopmess*(*)
    !c     ------------------------------------------------------------------
    

    if (stopmess.ne.' ') then
        write(*,10) stopmess
10      format(1x,a)
    endif
    stop
    
    end subroutine ustop 
    
    SUBROUTINE UPCASE(WORD)
        !     ******************************************************************
        !     CONVERT A CHARACTER STRING TO ALL UPPER CASE
        !     ******************************************************************
        !       SPECIFICATIONS:
        !     ------------------------------------------------------------------
              CHARACTER WORD*(*)
        !
              integer :: l, len, idiff, k
        !1------Compute the difference between lowercase and uppercase.
              L = LEN(WORD)
              IDIFF=ICHAR('a')-ICHAR('A')
        !
        !2------Loop through the string and convert any lowercase characters.
              DO 10 K=1,L
              IF(WORD(K:K).GE.'a' .AND. WORD(K:K).LE.'z')&
                WORD(K:K)=CHAR(ICHAR(WORD(K:K))-IDIFF)
        10    CONTINUE
        !
        !3------return.
              RETURN
    END SUBROUTINE UPCASE

    SUBROUTINE UINSRP(I,IN,IOUT,IP,ITERP)
        !     ******************************************************************
        !     Read and store one instance name.
        !     I is the instance number, and IP is the parameter number.
        !     ******************************************************************
        !        SPECIFICATIONS:
        !     ------------------------------------------------------------------
              USE PARAMMODULE
              CHARACTER*10 CTMP1,CTMP2
              CHARACTER*400 LINE
              
              integer :: ip, iout, iterp, i, in, ipl4, iloc, lloc, istart
              real :: r
              integer :: n, istop, j
        !     ------------------------------------------------------------------
        !
        !1------COMPUTE LOCATION OF NAME IN INAME, AND READ LINE CONTAINING
        !1------INSTANCE NAME.
              IPL4 = IPLOC(4,IP)
              ILOC = IPL4+I-1
              READ(IN,1000) LINE
         1000 FORMAT(A)
        !
        !2------GET INSTANCE NAME AND STORE IN INAME.
              LLOC = 1
              CALL URWORD(LINE,LLOC,ISTART,ISTOP,0,N,R,IOUT,IN)
              INAME(ILOC) = LINE(ISTART:ISTOP)
              CTMP1 = LINE(ISTART:ISTOP)
              CALL UPCASE(CTMP1)
        !
        !3------WRITE NAME UNLESS THIS IS NOT THE FIRST TIME THE SIMULATION
        !3------HAS BEEN RUN.
              IF(ITERP.EQ.1) WRITE(IOUT,1010)INAME(ILOC)
         1010 FORMAT(/,3X,'INSTANCE:  ',A)
        !
        !4------CHECK FOR DUPLICATE INSTANCE NAME IF THIS IS NOT THE FIRST
        !4------INSTANCE.
              IF (I.GT.1) THEN
                DO 10 J=IPL4,IPL4+I-2
                  CTMP2 = INAME(J)
                  CALL UPCASE(CTMP2)
                  IF (CTMP1.EQ.CTMP2) THEN
                    WRITE(IOUT,1020)INAME(J)
         1020       FORMAT(/,1X,'*** ERROR: "',A,&
             '" IS A DUPLICATE INSTANCE NAME FOR THIS PARAMETER',/,&
             ' -- STOP EXECUTION (UINSRP)')
                    CALL USTOP(' ')
                  ENDIF
           10   CONTINUE
              ENDIF
        !
        !5------RETURN.
      RETURN
    END SUBROUTINE UINSRP

    SUBROUTINE URDCOM(IN,IOUT,LINE)
    !C     ******************************************************************
    !C     READ COMMENTS FROM A FILE AND PRINT THEM.  RETURN THE FIRST LINE
    !C     THAT IS NOT A COMMENT
    !C     ******************************************************************
    !C
    !C        SPECIFICATIONS:
    !C     ------------------------------------------------------------------
    
    implicit none
    
    CHARACTER*(*) LINE
    
    integer :: IOUT
    integer :: IN
    integer :: LEN
    integer :: L
    integer :: II

    IOUT=FNumEco

    !C     ------------------------------------------------------------------
    !C
    !C1------Read a line
10  READ(IN,'(A)') LINE
    !C
    !C2------If the line does not start with "#", return.
    IF(LINE(1:1).NE.'#') then
        !IF (IOUT.GT.0)  write(iout,*) '$$$ urdcom unit#: ',IN
        if(IN.eq.22) then
            continue
        endif 
        RETURN
    endif
    !C
    !C3------Find the last non-blank character.
    L=LEN(LINE)
    DO 20 II=L,1,-1
        IF(LINE(II:II).NE.' ') GO TO 30
20  CONTINUE
    !C
    !C4------Print the line up to the last non-blank character if IOUT>0.
30  IF (IOUT.GT.0) WRITE(IOUT,'(1X,A)') LINE(1:II)
    GO TO 10
 
    END SUBROUTINE URDCOM

    SUBROUTINE U1DREL(A,ANAME,nJJ,KK,IN,IOUT)
!     ******************************************************************
!     ROUTINE TO INPUT 1-D REAL DATA MATRICES
!       A IS ARRAY TO INPUT
!       ANAME IS 24 CHARACTER DESCRIPTION OF A
!       JJ IS NO. OF ELEMENTS
!       IN IS INPUT UNIT
!       IOUT IS OUTPUT UNIT
!     ******************************************************************
!
!        SPECIFICATIONS:
!     ------------------------------------------------------------------
      CHARACTER*16 TEXT
      CHARACTER*24 ANAME
      real :: A(nJJ)
      real :: PERTIM,TOTIM
      CHARACTER*20 FMTIN
      CHARACTER*400 CNTRL
      CHARACTER*400 FNAME
      INCLUDE 'openspec.inc'
      
      integer :: in, KK, iout, jj
      integer :: nunopn
      DATA NUNOPN/99/
      integer :: iclose, ifree,icol
      real :: R
      integer :: istart, istop,n,locat
      real :: cnstnt
      integer :: iprn, kper, nstrt, ilay, kstp, nndlay, nJJ
      
      IOUT=FNumEco

!     ------------------------------------------------------------------

      !write(iout,*) '$$$ U1DREL aname: ',ANAME(:len_trim(ANAME))
!
!1------READ ARRAY CONTROL RECORD AS CHARACTER DATA.
      READ(IN,'(A)') CNTRL
!
!2------LOOK FOR ALPHABETIC WORD THAT INDICATES THAT THE RECORD IS FREE
!2------FORMAT.  SET A FLAG SPECIFYING IF FREE FORMAT OR FIXED FORMAT.
      ICLOSE=0
      IFREE=1
      ICOL=1
      CALL URWORD(CNTRL,ICOL,ISTART,ISTOP,1,N,R,IOUT,IN)
      IF (CNTRL(ISTART:ISTOP).EQ.'CONSTANT') THEN
         LOCAT=0
      ELSE IF(CNTRL(ISTART:ISTOP).EQ.'INTERNAL') THEN
         LOCAT=IN
      ELSE IF(CNTRL(ISTART:ISTOP).EQ.'EXTERNAL') THEN
         CALL URWORD(CNTRL,ICOL,ISTART,ISTOP,2,LOCAT,R,IOUT,IN)
      ELSE IF(CNTRL(ISTART:ISTOP).EQ.'OPEN/CLOSE') THEN
         CALL URWORD(CNTRL,ICOL,ISTART,ISTOP,0,N,R,IOUT,IN)
         FNAME=CNTRL(ISTART:ISTOP)
         LOCAT=NUNOPN
         WRITE(IOUT,15) LOCAT,FNAME
   15    FORMAT(1X,/1X,'OPENING FILE ON UNIT ',I4,':',/1X,A)
         OPEN(UNIT=LOCAT,FILE=FNAME,ACTION=ACTION(1))
         ICLOSE=1
      ELSE
!
!2A-----DID NOT FIND A RECOGNIZED WORD, SO NOT USING FREE FORMAT.
!2A-----READ THE CONTROL RECORD THE ORIGINAL WAY.
         IFREE=0
         READ(CNTRL,1,ERR=500) LOCAT,CNSTNT,FMTIN,IPRN
    1    FORMAT(I10,F10.0,A20,I10)
      END IF
!
!3------FOR FREE FORMAT CONTROL RECORD, READ REMAINING FIELDS.
      IF(IFREE.NE.0) THEN
         CALL URWORD(CNTRL,ICOL,ISTART,ISTOP,3,N,CNSTNT,IOUT,IN)
         IF(LOCAT.GT.0) THEN
            CALL URWORD(CNTRL,ICOL,ISTART,ISTOP,1,N,R,IOUT,IN)
            FMTIN=CNTRL(ISTART:ISTOP)
            CALL URWORD(CNTRL,ICOL,ISTART,ISTOP,2,IPRN,R,IOUT,IN)
         END IF
      END IF
!
!4------TEST LOCAT TO SEE HOW TO DEFINE ARRAY VALUES.
      IF(LOCAT.GE.0) GO TO 71
!-------LOCAT <0 READ BINARY FILE
!4C-----LOCAT<0; READ UNFORMATTED ARRAY VALUES.
        LOCAT=-LOCAT
        IF(KK.GT.0) THEN
           WRITE(IOUT,201) ANAME,KK,LOCAT
  201      FORMAT(1X,///11X,A,' FOR LAYER',I4,/&
           1x,'READING BINARY ON UNIT ',I4)
        ELSE IF(KK.EQ.0) THEN
           WRITE(IOUT,202) ANAME,LOCAT
  202      FORMAT(1X,///1X,A,/&
           1X,'READING BINARY ON UNIT ',I4)
        ELSE
           WRITE(IOUT,203) ANAME,LOCAT
  203      FORMAT(1X,///1X,A,' FOR CROSS SECTION',/&
           1X,'READING BINARY ON UNIT ',I4)
        END IF
        READ(LOCAT) KSTP,KPER,PERTIM,TOTIM,TEXT,NSTRT,NNDLAY,ILAY
        READ(LOCAT) (A(JJ),JJ=1,nJJ)
        RETURN

71    IF(LOCAT.GT.0) GO TO 90
!
!4A-----LOCAT =0; SET ALL ARRAY VALUES EQUAL TO CNSTNT. RETURN.
      DO 80 JJ=1,nJJ
   80 A(JJ)=CNSTNT
      IF(KK.GT.0.OR.KK.LT.0) WRITE(IOUT,2) ANAME,CNSTNT,KK
    2 FORMAT(1X,/1X,A,' =',1P,G14.6,' FOR LAYER',I4)
      IF(KK.EQ.0) WRITE(IOUT,3) ANAME,CNSTNT
    3 FORMAT(1X,/1X,A,' =',1P,G14.6)
      RETURN
!
!4B-----LOCAT>0; READ FORMATTED RECORDS USING FORMAT FMTIN.
   90 CONTINUE
      IF(KK.GT.0) WRITE(IOUT,4) ANAME,KK,LOCAT,FMTIN
   4  FORMAT(1X,///11X,A,' FOR LAYER',I4,/&
           1X,'READING ON UNIT ',I4,' WITH FORMAT: ',A20)
      IF(KK.EQ.0) WRITE(IOUT,5) ANAME,LOCAT,FMTIN
    5 FORMAT(1X,///11X,A,/&
            1X,'READING ON UNIT ',I4,' WITH FORMAT: ',A20)
      IF(FMTIN.EQ.'(FREE)') THEN
      READ(LOCAT,*) (A(JJ),JJ=1,nJJ)
      ELSE
         READ(LOCAT,FMTIN) (A(JJ),JJ=1,nJJ)
      END IF
      IF(ICLOSE.NE.0) CLOSE(UNIT=LOCAT)
!
!5------IF CNSTNT NOT ZERO THEN MULTIPLY ARRAY VALUES BY CNSTNT.
      ZERO=0.
      IF(CNSTNT.EQ.ZERO) GO TO 120
      DO 100 JJ=1,nJJ
  100 A(JJ)=A(JJ)*CNSTNT
!
!6------IF PRINT CODE (IPRN) =0 OR >0 THEN PRINT ARRAY VALUES.
120   CONTINUE
      IF(IPRN.EQ.0) THEN
         WRITE(IOUT,1001) (A(JJ),JJ=1,nJJ)
1001     FORMAT((1X,1PG12.5,9(1X,G12.5)))
      ELSE IF(IPRN.GT.0) THEN
         WRITE(IOUT,1002) (A(JJ),JJ=1,nJJ)
1002     FORMAT((1X,1PG12.5,4(1X,G12.5)))
      END IF
!
!7------RETURN
      RETURN
!
!8------CONTROL RECORD ERROR.
500   WRITE(IOUT,502) ANAME
502   FORMAT(1X,/1X,'ERROR READING ARRAY CONTROL RECORD FOR ',A,':')
      WRITE(IOUT,'(1X,A)') CNTRL
      CALL USTOP(' ')
    END SUBROUTINE U1DREL
    
    
    SUBROUTINE U1DREL8(A,ANAME,nJJ,KK,IN,IOUT)
!     ******************************************************************
!     ROUTINE TO INPUT 1-D REAL DATA MATRICES
!       A IS ARRAY TO INPUT
!       ANAME IS 24 CHARACTER DESCRIPTION OF A
!       JJ IS NO. OF ELEMENTS
!       IN IS INPUT UNIT
!       IOUT IS OUTPUT UNIT
!     ******************************************************************
!
!        SPECIFICATIONS:
!     ------------------------------------------------------------------
      CHARACTER*16 TEXT
      CHARACTER*24 ANAME
      DOUBLE PRECISION :: A(nJJ)
      DOUBLE PRECISION :: PERTIM,TOTIM
      CHARACTER*20 FMTIN
      CHARACTER*400 CNTRL
      CHARACTER*400 FNAME
      INCLUDE 'openspec.inc'
      
      integer :: in, KK, iout, jj
      integer :: nunopn
      DATA NUNOPN/99/
      integer :: iclose, ifree,icol
      real :: R
      integer :: istart, istop,n,locat
      real :: cnstnt
      integer :: iprn, kper, nstrt, ilay, kstp, nndlay, nJJ
      
      IOUT=FNumEco

!     ------------------------------------------------------------------

      !write(iout,*) '$$$ U1DREL8 aname: ',ANAME(:len_trim(ANAME))
!
!1------READ ARRAY CONTROL RECORD AS CHARACTER DATA.
      READ(IN,'(A)') CNTRL
!
!2------LOOK FOR ALPHABETIC WORD THAT INDICATES THAT THE RECORD IS FREE
!2------FORMAT.  SET A FLAG SPECIFYING IF FREE FORMAT OR FIXED FORMAT.
      ICLOSE=0
      IFREE=1
      ICOL=1
      CALL URWORD(CNTRL,ICOL,ISTART,ISTOP,1,N,R,IOUT,IN)
      IF (CNTRL(ISTART:ISTOP).EQ.'CONSTANT') THEN
         LOCAT=0
      ELSE IF(CNTRL(ISTART:ISTOP).EQ.'INTERNAL') THEN
         LOCAT=IN
      ELSE IF(CNTRL(ISTART:ISTOP).EQ.'EXTERNAL') THEN
         CALL URWORD(CNTRL,ICOL,ISTART,ISTOP,2,LOCAT,R,IOUT,IN)
      ELSE IF(CNTRL(ISTART:ISTOP).EQ.'OPEN/CLOSE') THEN
         CALL URWORD(CNTRL,ICOL,ISTART,ISTOP,0,N,R,IOUT,IN)
         FNAME=CNTRL(ISTART:ISTOP)
         LOCAT=NUNOPN
         WRITE(IOUT,15) LOCAT,FNAME
   15    FORMAT(1X,/1X,'OPENING FILE ON UNIT ',I4,':',/1X,A)
         OPEN(UNIT=LOCAT,FILE=FNAME,ACTION=ACTION(1))
         ICLOSE=1
      ELSE
!
!2A-----DID NOT FIND A RECOGNIZED WORD, SO NOT USING FREE FORMAT.
!2A-----READ THE CONTROL RECORD THE ORIGINAL WAY.
         IFREE=0
         READ(CNTRL,1,ERR=500) LOCAT,CNSTNT,FMTIN,IPRN
    1    FORMAT(I10,F10.0,A20,I10)
      END IF
!
!3------FOR FREE FORMAT CONTROL RECORD, READ REMAINING FIELDS.
      IF(IFREE.NE.0) THEN
         CALL URWORD(CNTRL,ICOL,ISTART,ISTOP,3,N,CNSTNT,IOUT,IN)
         IF(LOCAT.GT.0) THEN
            CALL URWORD(CNTRL,ICOL,ISTART,ISTOP,1,N,R,IOUT,IN)
            FMTIN=CNTRL(ISTART:ISTOP)
            CALL URWORD(CNTRL,ICOL,ISTART,ISTOP,2,IPRN,R,IOUT,IN)
         END IF
      END IF
!
!4------TEST LOCAT TO SEE HOW TO DEFINE ARRAY VALUES.
      IF(LOCAT.GE.0) GO TO 71
!-------LOCAT <0 READ BINARY FILE
!4C-----LOCAT<0; READ UNFORMATTED ARRAY VALUES.
        LOCAT=-LOCAT
        IF(KK.GT.0) THEN
           WRITE(IOUT,201) ANAME,KK,LOCAT
  201      FORMAT(1X,///11X,A,' FOR LAYER',I4,/&
           1x,'READING BINARY ON UNIT ',I4)
        ELSE IF(KK.EQ.0) THEN
           WRITE(IOUT,202) ANAME,LOCAT
  202      FORMAT(1X,///1X,A,/&
           1X,'READING BINARY ON UNIT ',I4)
        ELSE
           WRITE(IOUT,203) ANAME,LOCAT
  203      FORMAT(1X,///1X,A,' FOR CROSS SECTION',/&
           1X,'READING BINARY ON UNIT ',I4)
        END IF
        READ(LOCAT) KSTP,KPER,PERTIM,TOTIM,TEXT,NSTRT,NNDLAY,ILAY
        READ(LOCAT) (A(JJ),JJ=1,nJJ)
        RETURN

71    IF(LOCAT.GT.0) GO TO 90
!
!4A-----LOCAT =0; SET ALL ARRAY VALUES EQUAL TO CNSTNT. RETURN.
      DO 80 JJ=1,nJJ
   80 A(JJ)=CNSTNT
      IF(KK.GT.0.OR.KK.LT.0) WRITE(IOUT,2) ANAME,CNSTNT,KK
    2 FORMAT(1X,/1X,A,' =',1P,G14.6,' FOR LAYER',I4)
      IF(KK.EQ.0) WRITE(IOUT,3) ANAME,CNSTNT
    3 FORMAT(1X,/1X,A,' =',1P,G14.6)
      RETURN
!
!4B-----LOCAT>0; READ FORMATTED RECORDS USING FORMAT FMTIN.
   90 CONTINUE
      IF(KK.GT.0) WRITE(IOUT,4) ANAME,KK,LOCAT,FMTIN
   4  FORMAT(1X,///11X,A,' FOR LAYER',I4,/&
           1X,'READING ON UNIT ',I4,' WITH FORMAT: ',A20)
      IF(KK.EQ.0) WRITE(IOUT,5) ANAME,LOCAT,FMTIN
    5 FORMAT(1X,///11X,A,/&
            1X,'READING ON UNIT ',I4,' WITH FORMAT: ',A20)
      IF(FMTIN.EQ.'(FREE)') THEN
      READ(LOCAT,*) (A(JJ),JJ=1,nJJ)
      ELSE
         READ(LOCAT,FMTIN) (A(JJ),JJ=1,nJJ)
      END IF
      IF(ICLOSE.NE.0) CLOSE(UNIT=LOCAT)
!
!5------IF CNSTNT NOT ZERO THEN MULTIPLY ARRAY VALUES BY CNSTNT.
      ZERO=0.
      IF(CNSTNT.EQ.ZERO) GO TO 120
      DO 100 JJ=1,nJJ
  100 A(JJ)=A(JJ)*CNSTNT
!
!6------IF PRINT CODE (IPRN) =0 OR >0 THEN PRINT ARRAY VALUES.
120   CONTINUE
      IF(IPRN.EQ.0) THEN
         WRITE(IOUT,1001) (A(JJ),JJ=1,nJJ)
1001     FORMAT((1X,1PG12.5,9(1X,G12.5)))
      ELSE IF(IPRN.GT.0) THEN
         WRITE(IOUT,1002) (A(JJ),JJ=1,nJJ)
1002     FORMAT((1X,1PG12.5,4(1X,G12.5)))
      END IF
!
!7------RETURN
      RETURN
!
!8------CONTROL RECORD ERROR.
500   WRITE(IOUT,502) ANAME
502   FORMAT(1X,/1X,'ERROR READING ARRAY CONTROL RECORD FOR ',A,':')
      WRITE(IOUT,'(1X,A)') CNTRL
      CALL USTOP(' ')
    END SUBROUTINE U1DREL8
    
    
    SUBROUTINE U2DREL(A,ANAME,II,JJ,K,IN,IOUT)
        !     ******************************************************************
        !     ROUTINE TO INPUT 2-D REAL DATA MATRICES
        !       A IS ARRAY TO INPUT
        !       ANAME IS 24 CHARACTER DESCRIPTION OF A
        !       II IS NO. OF ROWS
        !       JJ IS NO. OF COLS
        !       K IS LAYER NO. (USED WITH NAME TO TITLE PRINTOUT --)
        !              IF K=0, NO LAYER IS PRINTED
        !              IF K<0, CROSS SECTION IS PRINTED)
        !       IN IS INPUT UNIT
        !       IOUT IS OUTPUT UNIT
        !     ******************************************************************
        !
        !        SPECIFICATIONS:
        !     ------------------------------------------------------------------
              CHARACTER*24 ANAME
              DIMENSION A(JJ,II)
              CHARACTER*20 FMTIN
              CHARACTER*400 CNTRL
              CHARACTER*16 TEXT
              CHARACTER*400 FNAME
              REAL PERTIMRD,TOTIMRD
              
              integer :: NUNOPN
              DATA NUNOPN/99/
              integer :: in, i,j,k, iout, iclose, ifree, icol, n, istop, istart, locat, iprn, jj, ii
              real :: a, r, cnstnt
              integer :: kstp, ilay, kper
              
              INCLUDE 'openspec.inc'
        !     ------------------------------------------------------------------
              !write(iout,*) '$$$ U2DREL aname: ',ANAME(:len_trim(ANAME))
        !
        !1------READ ARRAY CONTROL RECORD AS CHARACTER DATA.
              READ(IN,'(A)') CNTRL
        !
        !2------LOOK FOR ALPHABETIC WORD THAT INDICATES THAT THE RECORD IS FREE
        !2------FORMAT.  SET A FLAG SPECIFYING IF FREE FORMAT OR FIXED FORMAT.
              ICLOSE=0
              IFREE=1
              ICOL=1
              CALL URWORD(CNTRL,ICOL,ISTART,ISTOP,1,N,R,IOUT,IN)
              IF (CNTRL(ISTART:ISTOP).EQ.'CONSTANT') THEN
                 LOCAT=0
              ELSE IF(CNTRL(ISTART:ISTOP).EQ.'INTERNAL') THEN
                 LOCAT=IN
              ELSE IF(CNTRL(ISTART:ISTOP).EQ.'EXTERNAL') THEN
                 CALL URWORD(CNTRL,ICOL,ISTART,ISTOP,2,LOCAT,R,IOUT,IN)
              ELSE IF(CNTRL(ISTART:ISTOP).EQ.'OPEN/CLOSE') THEN
                 CALL URWORD(CNTRL,ICOL,ISTART,ISTOP,0,N,R,IOUT,IN)
                 FNAME=CNTRL(ISTART:ISTOP)
                 LOCAT=NUNOPN
                 WRITE(IOUT,15) LOCAT,FNAME
           15    FORMAT(1X,/1X,'OPENING FILE ON UNIT ',I4,':',/1X,A)
                 ICLOSE=1
              ELSE
        !
        !2A-----DID NOT FIND A RECOGNIZED WORD, SO NOT USING FREE FORMAT.
        !2A-----READ THE CONTROL RECORD THE ORIGINAL WAY.
                 IFREE=0
                 READ(CNTRL,1,ERR=500) LOCAT,CNSTNT,FMTIN,IPRN
            1    FORMAT(I10,F10.0,A20,I10)
              END IF
        !
        !3------FOR FREE FORMAT CONTROL RECORD, READ REMAINING FIELDS.
              IF(IFREE.NE.0) THEN
                 CALL URWORD(CNTRL,ICOL,ISTART,ISTOP,3,N,CNSTNT,IOUT,IN)
                 IF(LOCAT.NE.0) THEN
                    CALL URWORD(CNTRL,ICOL,ISTART,ISTOP,1,N,R,IOUT,IN)
                    FMTIN=CNTRL(ISTART:ISTOP)
                    IF(ICLOSE.NE.0) THEN
                       IF(FMTIN.EQ.'(BINARY)') THEN
                          OPEN(UNIT=LOCAT,FILE=FNAME,FORM=FORM,ACCESS=ACCESS,&
                              ACTION=ACTION(1))
                       ELSE
                          OPEN(UNIT=LOCAT,FILE=FNAME,ACTION=ACTION(1))
                       END IF
                    END IF
                    IF(LOCAT.GT.0 .AND. FMTIN.EQ.'(BINARY)') LOCAT=-LOCAT
                    CALL URWORD(CNTRL,ICOL,ISTART,ISTOP,2,IPRN,R,IOUT,IN)
                 END IF
              END IF
        !
        !4------TEST LOCAT TO SEE HOW TO DEFINE ARRAY VALUES.
              IF(LOCAT.EQ.0) THEN
        !
        !4A-----LOCAT=0; SET ALL ARRAY VALUES EQUAL TO CNSTNT. RETURN.
                DO 80 I=1,II
                DO 80 J=1,JJ
           80   A(J,I)=CNSTNT
                IF(K.GT.0) WRITE(IOUT,2) ANAME,CNSTNT,K
            2   FORMAT(1X,/1X,A,' =',1P,G14.6,' FOR LAYER',I4)
                IF(K.LE.0) WRITE(IOUT,3) ANAME,CNSTNT
            3   FORMAT(1X,/1X,A,' =',1P,G14.6)
                RETURN
              ELSE IF(LOCAT.GT.0) THEN
        !
        !4B-----LOCAT>0; READ FORMATTED RECORDS USING FORMAT FMTIN.
                IF(K.GT.0) THEN
                   WRITE(IOUT,94) ANAME,K,LOCAT,FMTIN
           94      FORMAT(1X,///11X,A,' FOR LAYER',I4,/&
                   1X,'READING ON UNIT ',I4,' WITH FORMAT: ',A)
                ELSE IF(K.EQ.0) THEN
                   WRITE(IOUT,95) ANAME,LOCAT,FMTIN
           95      FORMAT(1X,///11X,A,/&
                   1X,'READING ON UNIT ',I4,' WITH FORMAT: ',A)
                ELSE
                   WRITE(IOUT,96) ANAME,LOCAT,FMTIN
           96      FORMAT(1X,///11X,A,' FOR CROSS SECTION',/&
                   1X,'READING ON UNIT ',I4,' WITH FORMAT: ',A)
                END IF
                DO 100 I=1,II
                IF(FMTIN.EQ.'(FREE)') THEN
                   READ(LOCAT,*) (A(J,I),J=1,JJ)
                ELSE
                   READ(LOCAT,FMTIN) (A(J,I),J=1,JJ)
                END IF
          100   CONTINUE
              ELSE
        !
        !4C-----LOCAT<0; READ UNFORMATTED ARRAY VALUES.
                LOCAT=-LOCAT
                IF(K.GT.0) THEN
                   WRITE(IOUT,201) ANAME,K,LOCAT
          201      FORMAT(1X,///11X,A,' FOR LAYER',I4,/&
                   1X,'READING BINARY ON UNIT ',I4)
                ELSE IF(K.EQ.0) THEN
                   WRITE(IOUT,202) ANAME,LOCAT
          202      FORMAT(1X,///1X,A,/&
                   1X,'READING BINARY ON UNIT ',I4)
                ELSE
                   WRITE(IOUT,203) ANAME,LOCAT
          203      FORMAT(1X,///1X,A,' FOR CROSS SECTION',/&
                   1X,'READING BINARY ON UNIT ',I4)
                END IF
                READ(LOCAT) KSTP,KPER,PERTIMRD,TOTIMRD,TEXT,NCOL,NROW,ILAY
                READ(LOCAT) A
              END IF
        !
        !5------IF CNSTNT NOT ZERO THEN MULTIPLY ARRAY VALUES BY CNSTNT.
              IF(ICLOSE.NE.0) CLOSE(UNIT=LOCAT)
              ZERO=0.
              IF(CNSTNT.EQ.ZERO) GO TO 320
              DO 310 I=1,II
              DO 310 J=1,JJ
              A(J,I)=A(J,I)*CNSTNT
          310 CONTINUE
        !
        !6------IF PRINT CODE (IPRN) >0 OR =0 THEN PRINT ARRAY VALUES.
          320 IF(IPRN.GE.0) CALL ULAPRW(A,ANAME,0,0,JJ,II,0,IPRN,IOUT)
        !
        !7------RETURN
              RETURN
        !
        !8------CONTROL RECORD ERROR.
          500 IF(K.GT.0) THEN
                 WRITE(IOUT,501) ANAME,K
          501    FORMAT(1X,/1X,'ERROR READING ARRAY CONTROL RECORD FOR ',A,&
                  ' FOR LAYER',I4,':')
              ELSE
                 WRITE(IOUT,502) ANAME
          502    FORMAT(1X,/1X,'ERROR READING ARRAY CONTROL RECORD FOR ',A,':')
              END IF
              WRITE(IOUT,'(1X,A)') CNTRL
              CALL USTOP(' ')
      END SUBROUTINE U2DREL


    SUBROUTINE U1DINT(IA,ANAME,nJJ,KK,IN,IOUT)
!     ******************************************************************
!     ROUTINE TO INPUT 1-D REAL DATA MATRICES
!       A IS ARRAY TO INPUT
!       ANAME IS 24 CHARACTER DESCRIPTION OF A
!       JJ IS NO. OF ELEMENTS
!       IN IS INPUT UNIT
!       IOUT IS OUTPUT UNIT
!     ******************************************************************
!
!        SPECIFICATIONS:
!     ------------------------------------------------------------------
      CHARACTER*24 ANAME
      DIMENSION IA(nJJ)
      CHARACTER*20 FMTIN
      CHARACTER*400 CNTRL
      CHARACTER*400 FNAME
      DATA NUNOPN/99/
      INCLUDE 'openspec.inc'
      
      integer :: iout,ia,KK,in
      integer :: NUNOPN
      DATA NUNOPN/99/
      integer :: jj, iclose, ifree, icol, istart
      real :: R
      integer :: n
      integer :: istop, locat, iprn, icnstnt, nJJ
        
      IOUT=FNumEco
      

!     ------------------------------------------------------------------
      !write(iout,*) '$$$ U1DINT aname: ',ANAME(:len_trim(ANAME))
!
!1------READ ARRAY CONTROL RECORD AS CHARACTER DATA.
      READ(IN,'(A)') CNTRL
!
!2------LOOK FOR ALPHABETIC WORD THAT INDICATES THAT THE RECORD IS FREE
!2------FORMAT.  SET A FLAG SPECIFYING IF FREE FORMAT OR FIXED FORMAT.
      ICLOSE=0
      IFREE=1
      ICOL=1
      CALL URWORD(CNTRL,ICOL,ISTART,ISTOP,1,N,R,IOUT,IN)
      IF (CNTRL(ISTART:ISTOP).EQ.'CONSTANT') THEN
         LOCAT=0
      ELSE IF(CNTRL(ISTART:ISTOP).EQ.'INTERNAL') THEN
         LOCAT=IN
      ELSE IF(CNTRL(ISTART:ISTOP).EQ.'EXTERNAL') THEN
         CALL URWORD(CNTRL,ICOL,ISTART,ISTOP,2,LOCAT,R,IOUT,IN)
      ELSE IF(CNTRL(ISTART:ISTOP).EQ.'OPEN/CLOSE') THEN
         CALL URWORD(CNTRL,ICOL,ISTART,ISTOP,0,N,R,IOUT,IN)
         FNAME=CNTRL(ISTART:ISTOP)
         LOCAT=NUNOPN
         WRITE(IOUT,15) LOCAT,FNAME
   15    FORMAT(1X,/1X,'OPENING FILE ON UNIT ',I4,':',/1X,A)
         OPEN(UNIT=LOCAT,FILE=FNAME,ACTION=ACTION(1))
         ICLOSE=1
      ELSE
!
!2A-----DID NOT FIND A RECOGNIZED WORD, SO NOT USING FREE FORMAT.
!2A-----READ THE CONTROL RECORD THE ORIGINAL WAY.
         IFREE=0
         READ(CNTRL,1,ERR=500) LOCAT,ICNSTNT,FMTIN,IPRN
    1    FORMAT(I10,I10.0,A20,I10)
      END IF
!
!3------FOR FREE FORMAT CONTROL RECORD, READ REMAINING FIELDS.
      IF(IFREE.NE.0) THEN
         CALL URWORD(CNTRL,ICOL,ISTART,ISTOP,2,ICNSTNT,R,IOUT,IN)
         IF(LOCAT.GT.0) THEN
            CALL URWORD(CNTRL,ICOL,ISTART,ISTOP,1,N,R,IOUT,IN)
            FMTIN=CNTRL(ISTART:ISTOP)
            CALL URWORD(CNTRL,ICOL,ISTART,ISTOP,2,IPRN,R,IOUT,IN)
         END IF
      END IF
!
!4------TEST LOCAT TO SEE HOW TO DEFINE ARRAY VALUES.
      IF(LOCAT.GT.0) GO TO 90
!
!4A-----LOCAT <0 OR =0; SET ALL ARRAY VALUES EQUAL TO ICNSTNT. RETURN.
      DO 80 JJ=1,nJJ
   80 IA(JJ)=ICNSTNT
      IF(KK.GT.0) THEN
        WRITE(IOUT,2) ANAME,ICNSTNT,KK
    2 FORMAT(1X,/1X,A,' =',1P,I10,' FOR LAYER',I4)
      ELSE
        WRITE(IOUT,3) ANAME,ICNSTNT
    3 FORMAT(1X,/1X,A,' =',1P,I10)
      ENDIF
      RETURN
!
!4B-----LOCAT>0; READ FORMATTED RECORDS USING FORMAT FMTIN.
   90 CONTINUE
      IF(KK.GT.0) WRITE(IOUT,4) ANAME,KK,LOCAT,FMTIN
   4  FORMAT(1X,///11X,A,' FOR LAYER',I4,/&
           1X,'READING ON UNIT ',I4,' WITH FORMAT: ',A20)
      IF(KK.EQ.0) WRITE(IOUT,5) ANAME,LOCAT,FMTIN
    5 FORMAT(1X,///11X,A,/&
            1X,'READING ON UNIT ',I4,' WITH FORMAT: ',A20)
      IF(FMTIN.EQ.'(FREE)') THEN
      READ(LOCAT,*) (IA(JJ),JJ=1,nJJ)
      ELSE
         READ(LOCAT,FMTIN) (IA(JJ),JJ=1,nJJ)
      END IF
      IF(ICLOSE.NE.0) CLOSE(UNIT=LOCAT)
!
!5------IF ICNSTNT NOT ZERO THEN MULTIPLY ARRAY VALUES BY ICNSTNT.
      ZERO=0.
      IF(ICNSTNT.EQ.ZERO) GO TO 120
      DO 100 JJ=1,nJJ
  100 IA(JJ)=IA(JJ)*ICNSTNT
!
!6------IF PRINT CODE (IPRN) =0 OR >0 THEN PRINT ARRAY VALUES.
120   CONTINUE
      IF(IPRN.EQ.0) THEN
         WRITE(IOUT,1001) (IA(JJ),JJ=1,nJJ)
1001     FORMAT(20(1X,I9))
      ELSE IF(IPRN.GT.0) THEN
         WRITE(IOUT,1002) (IA(JJ),JJ=1,nJJ)
1002     FORMAT(8(1X,I9))
      END IF
!
!7------RETURN
      RETURN
!
!8------CONTROL RECORD ERROR.
500   WRITE(IOUT,502) ANAME
502   FORMAT(1X,/1X,'ERROR READING ARRAY CONTROL RECORD FOR ',A,':')
      WRITE(IOUT,'(1X,A)') CNTRL
      CALL USTOP(' ')
      END SUBROUTINE U1DINT

      SUBROUTINE U2DINT(IA,ANAME,II,JJ,K,IN,IOUT)
!     ******************************************************************
!     ROUTINE TO INPUT 2-D INTEGER DATA MATRICES
!       IA IS ARRAY TO INPUT
!       ANAME IS 24 CHARACTER DESCRIPTION OF IA
!       II IS NO. OF ROWS
!       JJ IS NO. OF COLS
!       K IS LAYER NO. (USED WITH NAME TO TITLE PRINTOUT --
!              IF K=0, NO LAYER IS PRINTED
!              IF K<0, CROSS SECTION IS PRINTED)
!       IN IS INPUT UNIT
!       IOUT IS OUTPUT UNIT
!     ******************************************************************
!
!        SPECIFICATIONS:
!     ------------------------------------------------------------------
      CHARACTER*24 ANAME
      DIMENSION IA(JJ,II)
      CHARACTER*20 FMTIN
      CHARACTER*400 CNTRL
      CHARACTER*400 FNAME
      integer :: nunopn
      DATA NUNOPN/99/
      INCLUDE 'openspec.inc'
      
      integer :: iout, k, in, ia, ii, iclose, ifree, icol, istart
      real :: r
      integer :: n, istop, locat, iconst, iprn, i, j, jj
!     ------------------------------------------------------------------
      !write(iout,*) '$$$ U2DINT aname: ',ANAME(:len_trim(ANAME))
!
!1------READ ARRAY CONTROL RECORD AS CHARACTER DATA.
      READ(IN,'(A)') CNTRL
!
!2------LOOK FOR ALPHABETIC WORD THAT INDICATES THAT THE RECORD IS FREE
!2------FORMAT.  SET A FLAG SPECIFYING IF FREE FORMAT OR FIXED FORMAT.
      ICLOSE=0
      IFREE=1
      ICOL=1
      CALL URWORD(CNTRL,ICOL,ISTART,ISTOP,1,N,R,IOUT,IN)
      IF (CNTRL(ISTART:ISTOP).EQ.'CONSTANT') THEN
         LOCAT=0
      ELSE IF(CNTRL(ISTART:ISTOP).EQ.'INTERNAL') THEN
         LOCAT=IN
      ELSE IF(CNTRL(ISTART:ISTOP).EQ.'EXTERNAL') THEN
         CALL URWORD(CNTRL,ICOL,ISTART,ISTOP,2,LOCAT,R,IOUT,IN)
      ELSE IF(CNTRL(ISTART:ISTOP).EQ.'OPEN/CLOSE') THEN
         CALL URWORD(CNTRL,ICOL,ISTART,ISTOP,0,N,R,IOUT,IN)
         FNAME=CNTRL(ISTART:ISTOP)
         LOCAT=NUNOPN
         WRITE(IOUT,15) LOCAT,FNAME
   15    FORMAT(1X,/1X,'OPENING FILE ON UNIT ',I4,':',/1X,A)
         ICLOSE=1
      ELSE
!
!2A-----DID NOT FIND A RECOGNIZED WORD, SO NOT USING FREE FORMAT.
!2A-----READ THE CONTROL RECORD THE ORIGINAL WAY.
         IFREE=0
         READ(CNTRL,1,ERR=600) LOCAT,ICONST,FMTIN,IPRN
    1    FORMAT(I10,I10,A20,I10)
      END IF
!
!3------FOR FREE FORMAT CONTROL RECORD, READ REMAINING FIELDS.
      IF(IFREE.NE.0) THEN
         CALL URWORD(CNTRL,ICOL,ISTART,ISTOP,2,ICONST,R,IOUT,IN)
         IF(LOCAT.NE.0) THEN
            CALL URWORD(CNTRL,ICOL,ISTART,ISTOP,1,N,R,IOUT,IN)
            FMTIN=CNTRL(ISTART:ISTOP)
            IF(ICLOSE.NE.0) THEN
               IF(FMTIN.EQ.'(BINARY)') THEN
                  OPEN(UNIT=LOCAT,FILE=FNAME,FORM=FORM,ACCESS=ACCESS,&
                      ACTION=ACTION(1))
               ELSE
                  OPEN(UNIT=LOCAT,FILE=FNAME,ACTION=ACTION(1))
               END IF
            END IF
            IF(LOCAT.GT.0 .AND. FMTIN.EQ.'(BINARY)') LOCAT=-LOCAT
            CALL URWORD(CNTRL,ICOL,ISTART,ISTOP,2,IPRN,R,IOUT,IN)
         END IF
      END IF
!
!4------TEST LOCAT TO SEE HOW TO DEFINE ARRAY VALUES.
      IF(LOCAT.EQ.0) THEN
!
!4A-----LOCAT=0; SET ALL ARRAY VALUES EQUAL TO ICONST. RETURN.
        DO 80 I=1,II
        DO 80 J=1,JJ
   80   IA(J,I)=ICONST
        IF(K.GT.0) WRITE(IOUT,82) ANAME,ICONST,K
   82   FORMAT(1X,/1X,A,' =',I15,' FOR LAYER',I4)
        IF(K.LE.0) WRITE(IOUT,83) ANAME,ICONST
   83   FORMAT(1X,/1X,A,' =',I15)
        RETURN
      ELSE IF(LOCAT.GT.0) THEN
!
!4B-----LOCAT>0; READ FORMATTED RECORDS USING FORMAT FMTIN.
        IF(K.GT.0) THEN
           WRITE(IOUT,94) ANAME,K,LOCAT,FMTIN
   94      FORMAT(1X,///11X,A,' FOR LAYER',I4,/&
           1X,'READING ON UNIT ',I4,' WITH FORMAT: ',A)
        ELSE IF(K.EQ.0) THEN
           WRITE(IOUT,95) ANAME,LOCAT,FMTIN
   95      FORMAT(1X,///11X,A,/&
           1X,'READING ON UNIT ',I4,' WITH FORMAT: ',A)
        ELSE
           WRITE(IOUT,96) ANAME,LOCAT,FMTIN
   96      FORMAT(1X,///11X,A,' FOR CROSS SECTION',/&
           1X,'READING ON UNIT ',I4,' WITH FORMAT: ',A)
        END IF
        DO 100 I=1,II
        IF(FMTIN.EQ.'(FREE)') THEN
           READ(LOCAT,*) (IA(J,I),J=1,JJ)
        ELSE
           READ(LOCAT,FMTIN) (IA(J,I),J=1,JJ)
        END IF
  100   CONTINUE
      ELSE
!
!4C-----LOCAT<0; READ UNFORMATTED RECORD CONTAINING ARRAY VALUES.
        LOCAT=-LOCAT
        IF(K.GT.0) THEN
           WRITE(IOUT,201) ANAME,K,LOCAT
  201      FORMAT(1X,///11X,A,' FOR LAYER',I4,/&
           1X,'READING BINARY ON UNIT ',I4)
        ELSE IF(K.EQ.0) THEN
           WRITE(IOUT,202) ANAME,LOCAT
  202      FORMAT(1X,///11X,A,/&
           1X,'READING BINARY ON UNIT ',I4)
        ELSE
           WRITE(IOUT,203) ANAME,LOCAT
  203      FORMAT(1X,///11X,A,' FOR CROSS SECTION',/&
           1X,'READING BINARY ON UNIT ',I4)
        END IF
        READ(LOCAT)
        READ(LOCAT) IA
      END IF
!
!5------IF ICONST NOT ZERO THEN MULTIPLY ARRAY VALUES BY ICONST.
      IF(ICLOSE.NE.0) CLOSE(UNIT=LOCAT)
      IF(ICONST.EQ.0) GO TO 320
      DO 310 I=1,II
      DO 310 J=1,JJ
      IA(J,I)=IA(J,I)*ICONST
  310 CONTINUE
!
!6------IF PRINT CODE (IPRN) <0 THEN RETURN.
  320 IF(IPRN.LT.0) RETURN
!
!7------PRINT COLUMN NUMBERS AT TOP OF PAGE.
      IF(IPRN.GT.9 .OR. IPRN.EQ.0) IPRN=6
      GO TO(401,402,403,404,405,406,407,408,409), IPRN
401   CALL UCOLNO(1,JJ,4,60,2,IOUT)
      GO TO 500
402   CALL UCOLNO(1,JJ,4,40,3,IOUT)
      GO TO 500
403   CALL UCOLNO(1,JJ,4,30,4,IOUT)
      GO TO 500
404   CALL UCOLNO(1,JJ,4,25,5,IOUT)
      GO TO 500
405   CALL UCOLNO(1,JJ,4,20,6,IOUT)
      GO TO 500
406   CALL UCOLNO(1,JJ,4,10,12,IOUT)
      GO TO 500
407   CALL UCOLNO(1,JJ,4,25,3,IOUT)
      GO TO 500
408   CALL UCOLNO(1,JJ,4,15,5,IOUT)
      GO TO 500
409   CALL UCOLNO(1,JJ,4,10,7,IOUT)
!
!8------PRINT EACH ROW IN THE ARRAY.
500   DO 510 I=1,II
      GO TO(501,502,503,504,505,506,507,508,509), IPRN
!
!----------------FORMAT 60I1
  501 WRITE(IOUT,551) I,(IA(J,I),J=1,JJ)
  551 FORMAT(1X,I3,1X,60(1X,I2):/(5X,60(1X,I2)))
      GO TO 510
!
!----------------FORMAT 40I2
  502 WRITE(IOUT,552) I,(IA(J,I),J=1,JJ)
  552 FORMAT(1X,I3,1X,40(1X,I2):/(5X,40(1X,I2)))
      GO TO 510
!
!----------------FORMAT 30I3
  503 WRITE(IOUT,553) I,(IA(J,I),J=1,JJ)
  553 FORMAT(1X,I3,1X,30(1X,I3):/(5X,30(1X,I3)))
      GO TO 510
!
!----------------FORMAT 25I4
  504 WRITE(IOUT,554) I,(IA(J,I),J=1,JJ)
  554 FORMAT(1X,I3,1X,25(1X,I4):/(5X,25(1X,I4)))
      GO TO 510
!
!----------------FORMAT 20I5
  505 WRITE(IOUT,555) I,(IA(J,I),J=1,JJ)
  555 FORMAT(1X,I3,1X,20(1X,I5):/(5X,20(1X,I5)))
      GO TO 510
!
!----------------FORMAT 10I11
  506 WRITE(IOUT,556) I,(IA(J,I),J=1,JJ)
  556 FORMAT(1X,I3,1X,10(1X,I11):/(5X,10(1X,I11)))
      GO TO 510
!
!----------------FORMAT 25I2
  507 WRITE(IOUT,557) I,(IA(J,I),J=1,JJ)
  557 FORMAT(1X,I3,1X,25(1X,I2):/(5X,25(1X,I2)))
      GO TO 510
!
!----------------FORMAT 15I4
  508 WRITE(IOUT,558) I,(IA(J,I),J=1,JJ)
  558 FORMAT(1X,I3,1X,15(1X,I4):/(5X,10(1X,I4)))
      GO TO 510
!
!----------------FORMAT 10I6
  509 WRITE(IOUT,559) I,(IA(J,I),J=1,JJ)
  559 FORMAT(1X,I3,1X,10(1X,I6):/(5X,10(1X,I6)))
!
  510 CONTINUE
!
!9------RETURN
      RETURN
!
!10-----CONTROL RECORD ERROR.
  600 IF(K.GT.0) THEN
         WRITE(IOUT,601) ANAME,K
  601    FORMAT(1X,/1X,'ERROR READING ARRAY CONTROL RECORD FOR ',A,&
          ' FOR LAYER',I4,':')
      ELSE
         WRITE(IOUT,602) ANAME
  602    FORMAT(1X,/1X,'ERROR READING ARRAY CONTROL RECORD FOR ',A,':')
      END IF
      WRITE(IOUT,'(1X,A)') CNTRL
      CALL USTOP(' ')
      END SUBROUTINE U2DINT
    
! -------------------------------------------------------------------------------------------   
!   subroutine MUSG_ReadAsciiHeadFile(FnumTG,Modflow)
!        implicit none
!
!        integer :: j
!        integer :: FnumTG
!        integer :: Fnum
!        character(MAXLBL) :: FName
!        character(MAXSTRING) :: line
!        integer :: i1
!
!        type (MUSG_Project) Modflow
!
!        read(FnumTG,'(a)') FName
!        call OpenAscii(FNum,FName)
!        call Msg( 'Head file: '//FName)
!
!        allocate(Modflow.gwf.head(Modflow.gwf.nCell,1))
!
!	    if(status /= 0) then
!		    call ErrMsg('FILE ERROR: '//fname)
!		    stop
!        endif
!        read(itmp,*) line
!	    read(itmp,*) (i1, Modflow.gwf.head(j,1),j=1,Modflow.gwf.nCell)
!	    call freeunit(FNum)
!
!        continue
!
!    end subroutine MUSG_ReadAsciiHeadFile
!    
!
!    
!    
!    subroutine MUSG_ReadAsciiKxFile(FnumTG,Modflow)
!        implicit none
!
!        integer :: j,m
!        integer :: FnumTG
!        integer :: Fnum
!        character(MAXLBL) :: FName
!        
!        real(dr) :: top
!        real(dr) :: bot
!
!
!        type (MUSG_Project) Modflow
!
!        read(FnumTG,'(a)') FName
!        call OpenAscii(FNum,FName)
!        call Msg( 'Kx file: '//FName)
!
!        allocate(Modflow.Kx(Modflow.gwf.nCell))
!        allocate(Modflow.Thick(Modflow.gwf.nCell))
!        allocate(Modflow.T(Modflow.gwf.nCell))
!
!	    if(status /= 0) then
!		    call ErrMsg('FILE ERROR: '//fname)
!		    stop
!        endif
!	    read(itmp,*) (Modflow.Kx(j),j=1,Modflow.gwf.nCell)
!	    call freeunit(FNum)
!        
!        do j=1,Modflow.gwf.nCell
!            top=0.0
!            bot=0.0
!            do m=1,4
!                top=top+Modflow.gwf.z(Modflow.gwf.ivertex(m,j))/4.0d0
!            end do
!            do m=5,8
!                bot=bot+Modflow.gwf.z(Modflow.gwf.ivertex(m,j))/4.0d0
!            end do
!            
!            if(j==3657) then
!                continue
!            end if
!            
!            if (abs(Modflow.gwf.head(j,1)-999.0d0) < 1e-5 ) then                           ! inactive
!                Modflow.Thick(j)=0.0
!                continue
!            else if (Modflow.gwf.head(j,1) < bot) then                           ! dry
!                Modflow.Thick(j)=0.0
!                continue
!            else if(Modflow.gwf.head(j,1) < top .and. Modflow.gwf.head(j,1) > bot) then    ! partially saturated
!                Modflow.Thick(j)=Modflow.gwf.head(j,1)-bot
!                continue
!            else
!                Modflow.Thick(j)=top-bot                              ! saturated
!                continue
!            endif
!            
!            Modflow.T(j)=Modflow.Thick(j)*Modflow.Kx(j)
!        
!        end do
!            
!
!        continue
!
!    end subroutine MUSG_ReadAsciiKxFile
!   subroutine MUSG_ReadAsciiSsFile(FnumTG,Modflow)
!        implicit none
!
!        integer :: j
!        integer :: FnumTG
!        integer :: Fnum
!        character(MAXLBL) :: FName
!
!        type (MUSG_Project) Modflow
!
!        read(FnumTG,'(a)') FName
!        call OpenAscii(FNum,FName)
!        call Msg( 'Ss file: '//FName)
!
!        allocate(Modflow.Ss(Modflow.gwf.nCell))
!
!	    if(status /= 0) then
!		    call ErrMsg('FILE ERROR: '//fname)
!		    stop
!        endif
!	    read(itmp,*) (Modflow.ss(j),j=1,Modflow.gwf.nCell)
!	    call freeunit(FNum)
!
!        continue
!
!    end subroutine MUSG_ReadAsciiSsFile
!   subroutine MUSG_ReadAsciiSyFile(FnumTG,Modflow)
!        implicit none
!
!        integer :: j
!        integer :: FnumTG
!        integer :: Fnum
!        character(MAXLBL) :: FName
!
!        type (MUSG_Project) Modflow
!
!        read(FnumTG,'(a)') FName
!        call OpenAscii(FNum,FName)
!        call Msg( 'Sy file: '//FName)
!
!        allocate(Modflow.Sy(Modflow.gwf.nCell))
!
!	    if(status /= 0) then
!		    call ErrMsg('FILE ERROR: '//fname)
!		    stop
!        endif
!	    read(itmp,*) (Modflow.Sy(j),j=1,Modflow.gwf.nCell)
!	    call freeunit(FNum)
!
!        continue
!
!    end subroutine MUSG_ReadAsciiSyFile
!   subroutine MUSG_ReadAsciiVanisFile(FnumTG,Modflow)
!        implicit none
!
!        integer :: j
!        integer :: FnumTG
!        integer :: Fnum
!        character(MAXLBL) :: FName
!
!        type (MUSG_Project) Modflow
!
!        read(FnumTG,'(a)') FName
!        call OpenAscii(FNum,FName)
!        call Msg( 'Vanis file: '//FName)
!
!        allocate(Modflow.Vanis(Modflow.gwf.nCell))
!
!	    if(status /= 0) then
!		    call ErrMsg('FILE ERROR: '//fname)
!		    stop
!        endif
!	    read(itmp,*) (Modflow.Vanis(j),j=1,Modflow.gwf.nCell)
!	    call freeunit(FNum)
!
!        continue
!
!    end subroutine MUSG_ReadAsciiVanisFile
!   subroutine MUSG_ReadRiverFlowsAsciiFile(FnumTG,Modflow)
!        implicit none
!
!        integer :: i
!        integer :: FnumTG
!        integer :: Fnum
!        character(MAXLBL) :: FName
!        character(MAXSTRING) :: Line
!        real(dr) :: r1
!
!        type (MUSG_Project) Modflow
!
!        read(FnumTG,'(a)') FName
!        call OpenAscii(FNum,FName)
!        call Msg( 'River Flows file: '//FName)
!
!        read(FNum,'(a)') line  ! header
!        do 
!            read(FNum,*,iostat=status) r1
!	        if(status /= 0) exit
!
!            Modflow.nlines=Modflow.nlines+1
!        end do
!        Modflow.nlines=Modflow.nlines-1
!        allocate(Modflow.StressPeriod(Modflow.nlines), & 
!                    & Modflow.RiverCell(Modflow.nlines), &
!                    & Modflow.RiverFlow(Modflow.nlines), &
!                    & Modflow.RiverHead(Modflow.nlines), &
!                    & Modflow.RiverElev(Modflow.nlines), &
!                    & Modflow.RiverCond(Modflow.nlines), &
!                    & Modflow.RiverConc(Modflow.nlines))
!        
!        rewind(FNum)
!        read(FNum,'(a)') line  ! header
!        read(FNum,*) (Modflow.StressPeriod(i), r1, &
!                        & Modflow.RiverCell(i), &
!                        & Modflow.RiverFlow(i), &
!                        & Modflow.RiverHead(i), &
!                        & Modflow.RiverElev(i), &
!                        & Modflow.RiverCond(i), &
!                        & Modflow.RiverConc(i), &
!        i=1,Modflow.nlines)
!
!        
!        continue
!
!    end subroutine MUSG_ReadRiverFlowsAsciiFile
!   subroutine MUSG_RiverFlowsToTecplot(FnumTG,Modflow) !--- Dump river flow data by cell to tecplot .dat file
!        implicit none 
!
!        integer :: FnumTG
!        integer :: Fnum
!        character(MAXLBL) :: FName
!
!        type (MUSG_Project) Modflow
!
!        integer :: LastStressPeriod
!        character(MAXSTRING) :: VarBuffer
!        character(MAXSTRING) :: OutputBuffer
!        integer :: i, j
!        integer :: iFile
!        integer :: NCellFluxvsT, iCell
!    
!        read(FnumTG,'(a)') FName
!        l1=index(FName,'.dat')
!        if(l1==0) then
!            FNAME=trim(FName)//'.tecplot.dat'
!        endif
!        call OpenAscii(FNum,FName)
!        call Msg( 'To File: '//trim(FName))
!    
!        VarBuffer='variables="X","Y","Z","Flux(ft3/d)","Head(ft)","Conductance(ft/d)","Elevation(ft)","Concentration(ug/L)","Elapsed Time (days)","Cell"'
!        write(FNum,'(a)') trim(VarBuffer)
!    
!        LastStressPeriod=0
!        OutputBuffer=''
!        ifile=0
!        do i=1,Modflow.nlines
!            if(Modflow.StressPeriod(i) /= LastStressPeriod) then
!                write(FNum,'(a,i5,a,f20.4)') 'zone t= "Stress Period ',Modflow.StressPeriod(i),'", SOLUTIONTIME = ',modflow.TIMOT(Modflow.StressPeriod(i))
!                LastStressPeriod=Modflow.StressPeriod(i)
!            endif
!            write(FNum,'(9f20.4,i8)') Modflow.gwf.xCell(Modflow.RiverCell(i)),Modflow.gwf.yCell(Modflow.RiverCell(i)),Modflow.gwf.zCell(Modflow.RiverCell(i)), &
!                                    & Modflow.RiverFlow(i), &
!                                    & Modflow.RiverHead(i), &
!                                    & Modflow.RiverElev(i), &
!                                    & Modflow.RiverCond(i), &
!                                    & Modflow.RiverConc(i), &
!                                    & modflow.TIMOT(Modflow.StressPeriod(i)), &
!                                    & Modflow.RiverCell(i)
!        end do
!    
!        call FreeUnit(FNum)
!        
!        read(FnumTG,*,iostat=status,end=10) NCellFluxvsT
!	    if(status == 0 .and. NCellFluxvsT>0 ) then
!            do i=1,NCellFluxvsT
!                read(FnumTG,*) iCell
!                TmpSTR=FileNumberString(icell)
!                FName='cell_'//trim(TmpSTR)//'.dat'
!                call OpenAscii(FNum,FName)
!                call Msg( 'Cell flux to File: '//trim(FName))
!            
!                write(FNum,'(a)') 'variables="Time(days)","Flux(ft3/d)"'
!                write(FNum,'(a,i8,a)') 'zone t= "Cell ',iCell,'"'
!            
!                do j=1,Modflow.nlines
!                    if(Modflow.RiverCell(j)==iCell) then
!                        write(FNum,'(9f20.4,i8)') modflow.TIMOT(Modflow.StressPeriod(j)), Modflow.RiverFlow(j)
!                    endif
!                end do
!                call FreeUnit(FNum)
!                
!            end do
!        endif
!            
!            
!10      continue        
!    
!    end subroutine MUSG_RiverFlowsToTecplot
!   subroutine MUSG_ReadHeadCalibrationAsciiFile(FnumTG,Modflow)
!        implicit none
!
!        integer :: i
!        integer :: FnumTG
!        integer :: Fnum
!        character(MAXLBL) :: FName
!        character(MAXSTRING) :: Line
!        real(dr) :: r1
!
!        type (MUSG_Project) Modflow
!
!        read(FnumTG,'(a)') FName
!        call OpenAscii(FNum,FName)
!        call Msg( 'River Flows file: '//FName)
!
!        read(FNum,'(a)') line  ! header
!        do 
!            read(FNum,*,iostat=status) r1
!	        if(status /= 0) exit
!
!            Modflow.nlinesHead=Modflow.nlinesHead+1
!        end do
! 
!        allocate(     Modflow.StressPeriodHead(Modflow.nlinesHead), & 
!                    & Modflow.WellNameHead(Modflow.nlinesHead), &
!                    & Modflow.Xhead(Modflow.nlinesHead), &
!                    & Modflow.YHead(Modflow.nlinesHead), &
!                    & Modflow.ZminHead(Modflow.nlinesHead), &
!                    & Modflow.ZmaxHead(Modflow.nlinesHead), &
!                    & Modflow.Observed_ft(Modflow.nlinesHead), &
!                    & Modflow.Simulated_ft(Modflow.nlinesHead))
!        rewind(FNum)
!        read(FNum,'(a)') line  ! header
!        read(FNum,*) (   Modflow.StressPeriodHead(i), & 
!                       & Modflow.WellNameHead(i), &
!                       & Modflow.Xhead(i), &
!                       & Modflow.YHead(i), &
!                       & Modflow.ZminHead(i), &
!                       & Modflow.ZmaxHead(i), &
!                       & Modflow.Observed_ft(i), &
!                       & Modflow.Simulated_ft(i), &
!        & i=1,Modflow.nlinesHead)
!
!        
!        continue
!
!    end subroutine MUSG_ReadHeadCalibrationAsciiFile
!   subroutine MUSG_HeadCalibrationToTecplot(FnumTG,Modflow) !--- Dump Head Calibration data to tecplot .dat file
!        implicit none 
!
!        integer :: FnumTG
!        integer :: Fnum
!        character(MAXLBL) :: FName
!
!        type (MUSG_Project) Modflow
!
!        character(MAXSTRING) :: VarBuffer
!        character(MAXSTRING) :: OutputBuffer
!        integer :: i
!        integer :: iFile
!        
!        real(dr) :: zmidpoint
!        real(dr) :: obs
!        real(dr) :: sim
!        integer :: navg
!        
!        real(dr) :: TZeroDateNum
!    
!        read(FnumTG,'(a)') FName
!        l1=index(FName,'.dat')
!        if(l1==0) then
!            FNAME=trim(FName)//'.tecplot.dat'
!        endif
!        call OpenAscii(FNum,FName)
!        call Msg( 'To File: '//trim(FName))
!        VarBuffer='variables="X","Y","Z","Observed(ft)","Simulated(ft)","Residual(ft)","Stress Period","DateNum"'
!        write(FNum,'(a)') trim(VarBuffer)
!        
!        read(FnumTG,*) TZeroDateNum
!
!         
!        OutputBuffer=''
!        ifile=0
!
!         initialize with first stress period and first well data (i.e. first record)
!        write(FNum,'(a,i5,a,f20.4)') 'zone t= "Stress Period ',Modflow.StressPeriodHead(1),'", SOLUTIONTIME = ',modflow.TIMOT(Modflow.StressPeriodHead(1))
!        navg=1
!        obs=Modflow.Observed_ft(1)
!        sim=Modflow.Simulated_ft(1)
!        do i=2,Modflow.nlinesHead
!            if(Modflow.StressPeriodHead(i) == 208) then
!                continue
!            endif
!            if(Modflow.StressPeriodHead(i) /= Modflow.StressPeriodHead(i-1)) then  ! end of stress period and well data 
!                 Calculate average and write last reading
!                zmidpoint=(Modflow.ZminHead(i-1)+Modflow.ZmaxHead(i-1))/2.0
!                write(FNum,'(6f20.4,i8,F20.4)') Modflow.xhead(i-1),Modflow.yhead(i-1),zmidpoint, &
!                    & obs/navg, &
!                    & sim/navg, &
!                    & (sim-obs)/navg, &
!                    & Modflow.StressPeriodHead(i-1), &
!                    & modflow.TIMOT(Modflow.StressPeriodHead(i-1))+TZeroDateNum
!                write(FNum,'(a)')  '% '//Modflow.WellNameHead(i-1)
!
!
!                write(FNum,'(a,i5,a,f20.4)') 'zone t= "Stress Period ',Modflow.StressPeriodHead(i),'", SOLUTIONTIME = ',modflow.TIMOT(Modflow.StressPeriodHead(i))
!                navg=1
!                obs=Modflow.Observed_ft(i)
!                sim=Modflow.Simulated_ft(i)
!            else  ! stress period reading i
!                if(Modflow.WellNameHead(i) == Modflow.WellNameHead(i-1)) then ! same well as last reading, update avg
!                    navg=navg+1
!                    obs=obs+Modflow.Observed_ft(i)
!                    sim=sim+Modflow.Simulated_ft(i)
!                else  ! different well than last reading
!                     Calculate average and write last reading
!                    zmidpoint=(Modflow.ZminHead(i-1)+Modflow.ZmaxHead(i-1))/2.0
!                    write(FNum,'(6f20.4,i8,F20.4)') Modflow.xhead(i-1),Modflow.yhead(i-1),zmidpoint, &
!                           & obs/navg, &
!                           & sim/navg, &
!                           & (sim-obs)/navg , &
!                           & Modflow.StressPeriodHead(i-1), &
!                           & modflow.TIMOT(Modflow.StressPeriodHead(i-1))+TZeroDateNum
!                    write(FNum,'(a)')  '% '//Modflow.WellNameHead(i-1)
!                     initialize with this reading
!                    navg=1
!                    obs=Modflow.Observed_ft(i)
!                    sim=Modflow.Simulated_ft(i)
!
!                 endif
!            
!            endif 
!        end do
!         Calculate average and write last reading
!        zmidpoint=(Modflow.ZminHead(Modflow.nlinesHead)+Modflow.ZmaxHead(Modflow.nlinesHead))/2.0
!        write(FNum,'(6f20.4,i8,F20.4)') Modflow.xhead(Modflow.nlinesHead),Modflow.yhead(Modflow.nlinesHead),zmidpoint, &
!            & obs/navg, &
!            & sim/navg, &
!            & (sim-obs)/navg, &
!            & Modflow.StressPeriodHead(i-1), &
!            & modflow.TIMOT(Modflow.StressPeriodHead(i-1))+TZeroDateNum
!        write(FNum,'(a)')  '% '//Modflow.WellNameHead(Modflow.nlinesHead)
!
!    
!        call FreeUnit(FNum)
!    
!    end subroutine MUSG_HeadCalibrationToTecplot
!    subroutine MUSG_ReadWellConstructionCSVFile(FnumTG,Modflow)
!        implicit none
!
!        integer :: i
!        integer :: FnumTG
!        integer :: Fnum
!        character(MAXLBL) :: FName
!        character(MAXSTRING) :: line
!        type (MUSG_Project) Modflow
!
!        read(FnumTG,'(a)') FName
!        call OpenAscii(FNum,FName)
!        call Msg( 'Well construction details file: '//FName)
!        
!         Count well data
!        Modflow.nWellConst=0
!        read(FNum,*) line ! throw away header
!        do 
!            read(FNum,'(a)',iostat=status) line 
!            if(status/=0) then
!                exit
!            end if
!            
!            Modflow.nWellConst=Modflow.nWellConst+1
!        end do
!
!        allocate(Modflow.NameWellConst(Modflow.nWellConst), &
!	        & Modflow.XWellConst(Modflow.nWellConst), &
!	        & Modflow.YWellConst(Modflow.nWellConst), &
!	        & Modflow.BotElevWellConst(Modflow.nWellConst), &
!	        & Modflow.TopElevWellConst(Modflow.nWellConst), &
!	        & Modflow.CasingRadiusWellConst(Modflow.nWellConst), &
!	        & Modflow.TonWellConst(Modflow.nWellConst), &
!	        & Modflow.ToffWellConst(Modflow.nWellConst))
!        
!        rewind(FNum)
!
!        read(FNum,*) line
!        do i=1,Modflow.nWellConst
!	        read(FNum,*) Modflow.NameWellConst(i), &
!	            & Modflow.XWellConst(i), &
!	            & Modflow.YWellConst(i), &
!	            & Modflow.BotElevWellConst(i), &
!	            & Modflow.TopElevWellConst(i), &
!	            & Modflow.CasingRadiusWellConst(i), &
!	            & Modflow.TonWellConst(i), &
!	            & Modflow.ToffWellConst(i)
!        end do
!        
!        continue
!        
!	    call freeunit(FNum)
!
!        continue
!
!   end subroutine MUSG_ReadWellConstructionCSVFile
!    subroutine MUSG_Read_EIWellCSVFile(FnumTG,Modflow)
!        implicit none
!
!        integer :: i, i1, i2
!        integer :: FnumTG
!        integer :: Fnum
!        character(MAXLBL) :: FName
!        character(MAXSTRING) :: line
!        type (MUSG_Project) Modflow
!
!        read(FnumTG,'(a)') FName
!        call OpenAscii(FNum,FName)
!        call Msg( 'EI well construction details file: '//FName)
!        
!         Count well data
!        Modflow.n_EIWell=0
!        read(FNum,*) line ! throw away header
!        do 
!            read(FNum,'(a)',iostat=status) line 
!            if(status/=0) then
!                exit
!            end if
!            
!            Modflow.n_EIWell=Modflow.n_EIWell+1
!        end do
!
!        allocate(Modflow.Name_EIWell(Modflow.n_EIWell), &
!	        & Modflow.X_EIWell(Modflow.n_EIWell), &
!	        & Modflow.Y_EIWell(Modflow.n_EIWell), &
!	        & Modflow.TopElev_EIWell(Modflow.n_EIWell), &
!	        & Modflow.ScreenALength_EIWell(Modflow.n_EIWell), &
!	        & Modflow.ScreenBOffset_EIWell(Modflow.n_EIWell), &
!	        & Modflow.ScreenBLength_EIWell(Modflow.n_EIWell))
!        
!        rewind(FNum)
!
!        read(FNum,'(a)') line
!        do i=1,Modflow.n_EIWell
!	        read(FNum,*) Modflow.Name_EIWell(i), &
!	            & Modflow.X_EIWell(i), &
!	            & Modflow.Y_EIWell(i), &
!	            & Modflow.TopElev_EIWell(i), &
!	            & Modflow.ScreenALength_EIWell(i), &
!	            & Modflow.ScreenBOffset_EIWell(i), &
!	            & Modflow.ScreenBLength_EIWell(i)
!            i1=index(Modflow.Name_EIWell(i),'IW')
!            i2=index(Modflow.Name_EIWell(i),'EW')
!            if(i1==0 .and. i2==0) then
!                call ErrMsg('Extraction/Injection well name "'//trim(Modflow.Name_EIWell(i))//'" must contain the string "IW" (injection well) or "EW" (extraction well)') 
!            endif
!        end do
!        
!        continue
!        
!	    call freeunit(FNum)
!
!        continue
!
!   end subroutine MUSG_Read_EIWellCSVFile
!
!
!   subroutine MUSG_RiverConductanceUpdate(FnumTG) !--- Dump Head Calibration data to tecplot .dat file
!        implicit none 
!
!        integer :: FnumTG
!        integer :: Fnum
!        character(MAXLBL) :: FName
!        integer :: FnumOut
!        character(MAXLBL) :: FNameOut
!
!
!        integer :: i
!        
!        integer :: nRiv       
!        real(dr) :: length_ft(1000)
!        integer :: node(1000)
!        integer :: zonenum(1000)
!        character*80 :: line
!        real(dr) :: Cond_d_Len(1000)
!        
!        real(dr) :: a2, a4, a5
!        integer :: i1, i2
!        real(dr) :: oldCond, newCond
!        
!    
!         file 1
!        read(FnumTG,'(a)') FName  
!        call OpenAscii(FNum,FName)
!        call Msg( 'File 1: '//trim(FName))
!        
!        i=1
!        read(FNum,'(a)') line
!        do 
!           read(FNum,*,iostat=status) node(i),Length_ft(i),ZoneNum(i)
!           if(status /= 0) exit
!            
!           i=i+1
!        end do
!        nRiv=i-1 
!        call FreeUnit(FNum)
!
!
!         file 2
!        read(FnumTG,'(a)') FName  
!        call OpenAscii(FNum,FName)
!        call Msg( 'File 2: '//trim(FName))
!        
!        read(FNum,'(a)') line
!        do 
!           read(FNum,*,iostat=status)  i, Cond_d_Len(i)
!           if(status /= 0) exit
!            
!        end do
!
!         file 3
!        read(FnumTG,'(a)') FName  
!        call OpenAscii(FNum,FName)
!        call Msg( 'File 3: '//trim(FName))
!        
!         file Out
!        read(FnumTG,'(a)') FNameOut  
!        call OpenAscii(FNumOut,FNameOut)
!        call Msg( 'File Out: '//trim(FNameOut))
!        
!        
!        read(FNum,'(a)') line
!        write(FNumOut,'(a)') line
!        read(FNum,'(a)') line
!        write(FNumOut,'(a)') line
!        do 
!            read(FNum,'(a)',iostat=status) line
!            if(status /= 0) exit
!           
!            write(FNumOut,'(a)') line
!            do i=1,nRiv
!                read(FNum,*) i1, a2, oldCond, a4, a5, i2
!                newCond=Length_ft(i)*Cond_d_Len(i)
!                if(abs(newcond-oldcond) > 1e-3) then
!                    continue
!                endif
!                write(FNumOut,'(i8,1x,f12.6,1x,1pe15.6,1x,f12.6,1x,1pe15.6,i5)') i1, a2, newCond, a4, a5, i2
!            end do
!        end do
!        call FreeUnit(FNum)
!        call FreeUnit(FNumOut)
!        continue
!    
!    end subroutine MUSG_RiverConductanceUpdate
!    subroutine MUSG_PEST_WellRatePenalties(FnumTG) !--- Overwrite rates in Modflow-USG well file
!        implicit none 
!        
!        integer :: i, j
!
!        integer :: FnumTG
!        integer :: FnumPenalty
!        character(MAXLBL) :: FNamePenalty
!        integer :: nRange
!        real(dr), allocatable :: MinPenalty(:)
!        real(dr), allocatable :: MaxPenalty(:)
!        real(dr), allocatable :: MinRange(:)
!        real(dr), allocatable :: MaxRange(:)
!        real(dr) :: InjRatePercentTargetLowerMin
!        real(dr) :: InjRatePercentTargetLowerMax
!        real(dr) :: InjratePercentPenaltyLowerMin
!        real(dr) :: InjratePercentPenaltyLowerMax
!        real(dr) :: InjRatePercentTargetHigher
!        real(dr) :: InjratePercentPenaltyHigher
!        real(dr) :: PESTStressPeriodInjWellFlips
!        real(dr) :: PESTStressPeriodExtWellFlips
!
!
!        real(dr) :: MinExtRate
!        real(dr) :: MaxInjRateGPM
!        
!        integer :: FnumExtWell
!        character(MAXLBL) :: FNameExtWell
!        integer :: nExtWell
!        character(20), allocatable :: ExtWellName(:)
!        real(dr), allocatable :: ExtWellRate(:,:)
!        
!        integer :: FnumInjWell
!        character(MAXLBL) :: FNameInjWell
!        integer :: nInjWell
!        character(20), allocatable :: InjWellName(:)
!        real(dr), allocatable :: InjWellFraction(:,:)
!        
!        integer :: nStressPeriods
!        
!         CLN file
!        integer :: FnumCLN
!        character(MAXLBL) :: FNameCLN
!        integer :: nCLN
!        integer :: iCLNFirstEWIW
!        integer, allocatable :: CLNType(:)   ! 0 - conventional well, 1 - extaction (EW) well, 2 - injection (IW) well
!        integer, parameter :: EW_upper=11
!        integer, parameter :: EW_lower=12
!        integer, parameter :: IW_upper=21
!        integer, parameter :: IW_lower=22
!        
!         Calculations
!        real(dr) :: sumExtWellRates
!        real(dr) :: sumInjWellFractions
!        real(dr), allocatable :: InjWellRate(:,:)
!        
!         Modflow-USG well file
!        integer :: FnumMUSGWell
!        character(MAXLBL) :: FNameMUSGWell
!        integer :: FnumMUSGWellOut
!        character(MAXLBL) :: FNameMUSGWellOut
!        integer :: iStressPeriod
!        character(MAXLBL) :: line
!        character(20) :: ThisWellName
!        logical :: WellFound
!        integer :: cln
!        real(dr) :: rate
!        
!         Modflow-USG transient ibound file
!        integer :: FnumTIB
!        character(MAXLBL) :: FNameTIB
!        integer :: FnumTIBOut
!        character(MAXLBL) :: FNameTIBOut
!        integer :: nCLNTurnOff
!        integer :: nCLNTurnOn
!        integer :: nCells  
!        integer :: iDum2
!        integer :: nEWIWTurnOff
!        integer :: nEWIWTurnOn
!        integer :: nlist
!        integer :: CLNArray(MAXCLN)  
!        
!         
!        
!         Penalties input file
!        read(FnumTG,'(a)') FNamePenalty  
!        call OpenAscii(FNumPenalty,FNamePenalty)
!        call Msg( 'Penalties file: '//trim(FNamePenalty))
!    
!        
!        read(FNumPenalty,*) MinExtRate
!        write(TMPStr,'(4f15.3)') MinExtRate
!        call Msg( 'Minimum extraction rate: '//trim(TMPStr))
!        read(FNumPenalty,*) nRange
!        allocate(MinRange(nRange), MaxRange(nRange), MinPenalty(nRange), MaxPenalty(nRange))
!        do i=1,nRange
!            read(FNumPenalty,*) MinRange(i), MaxRange(i), MinPenalty(i), MaxPenalty(i)
!            write(TMPStr,'(4f15.3)') MinRange(i), MaxRange(i), MinPenalty(i), MaxPenalty(i)
!            call Msg( 'Rate1 Rate2 Penalty1 Penalty2: '//trim(TMPStr))
!            if(i>1) then
!                if(Minrange(i)/=MaxRange(i-1)) then
!                    write(ErrStr,'(a,2f10.2)') 'Range 1: ', MinRange(i-1), MaxRange(i-1)
!                    call Msg(ErrStr)
!                    write(ErrStr,'(a,2f10.2)') 'Range 2: ', MinRange(i), MaxRange(i)
!                    call Msg(ErrStr)
!                    call ErrMsg('Min range 2 not equal to max range 1')
!                end if
!                if(MinPenalty(i)/=MaxPenalty(i-1)) then
!                    write(ErrStr,'(a,2f10.2)') 'Penalty 1: ', MinPenalty(i-1), MaxPenalty(i-1)
!                    call Msg(ErrStr)
!                    write(ErrStr,'(a,2f10.2)') 'Penalty 2: ', MinPenalty(i), MaxPenalty(i)
!                    call Msg(ErrStr)
!                    call ErrMsg('Min penalty 2 not equal to max penalty 1')
!                end if
!            end if
!        end do
!
!        read(FNumPenalty,*) MaxInjRateGPM
!        write(TMPStr,*) MaxInjRateGPM
!        call Msg( 'Maximum injection rate: '//trim(TMPStr))
!
!        read(FNumPenalty,*) InjRatePercentTargetLowerMin,InjRatePercentTargetLowerMax, InjratePercentPenaltyLowerMin, InjratePercentPenaltyLowerMax
!        write(TMPStr,'(4f15.3)') InjRatePercentTargetLowerMin,InjRatePercentTargetLowerMax, InjratePercentPenaltyLowerMin, InjratePercentPenaltyLowerMax
!        call Msg( 'Injection Rate%1 Rate%2 Penalty1 Penalty2: '//trim(TMPStr))
!
!        read(FNumPenalty,*) InjRatePercentTargetHigher, InjratePercentPenaltyHigher
!        write(TMPStr,'(4f15.3)') InjRatePercentTargetHigher, InjratePercentPenaltyHigher
!        call Msg( 'InjRatePercentTargetHigher InjratePercentPenaltyHigher: '//trim(TMPStr))
!
!        read(FNumPenalty,*) PESTStressPeriodInjWellFlips
!        write(TMPStr,*) nint(PESTStressPeriodInjWellFlips)
!        call Msg( 'Injection (IW) Wells flip over at stress period: '//trim(TMPStr))
!       
!        read(FNumPenalty,*) PESTStressPeriodExtWellFlips
!        write(TMPStr,*) nint(PESTStressPeriodExtWellFlips)
!        call Msg( 'Extraction (EW) Wells flip over at stress period: '//trim(TMPStr))
!       
!        
!        
!         Extraction well file
!        read(FNumPenalty,'(a)') FNameExtWell
!        call OpenAscii(FNumExtWell,FNameExtWell)
!        call Msg( 'Extraction well file: '//trim(FNameExtWell))
!
!        read(FNumExtWell,*) nExtWell
!        read(FNumExtWell,*) nStressPeriods
!        allocate(ExtWellName(nExtWell),ExtWellRate(nExtWell,nStressPeriods))
!        do i=1,nExtWell
!            read(FNumExtWell,*) ExtWellName(i)
!            do j=1,nStressPeriods
!                read(FNumExtWell,*) ExtWellRate(i,j)
!            end do
!        end do
!
!        call FreeUnit(FNumExtWell)
!        
!         Injection well file
!        read(FNumPenalty,'(a)') FNameInjWell
!        call OpenAscii(FNumInjWell,FNameInjWell)
!        call Msg( 'Injection well file: '//trim(FNameInjWell))
!
!        read(FNumInjWell,*) nInjWell
!        read(FNumExtWell,*) nStressPeriods
!        allocate(InjWellName(nInjWell),InjWellFraction(nInjWell,nStressPeriods),InjWellRate(nInjWell,nStressPeriods))
!
!        do i=1,nInjWell
!            read(FNumInjWell,*) InjWellName(i)
!            do j=1,nStressPeriods
!                read(FNumExtWell,*)InjWellFraction(i,j)
!            end do
!        end do
!        call FreeUnit(FNumInjWell)
!
!        call FreeUnit(FNumPenalty)
!        
!         CLN information file
!        read(FNumTG,'(a)') FNameCLN
!        call OpenAscii(FNumCLN,FNameCLN)
!        call Msg( 'CLN file: '//trim(FNameCLN))
!
!        read(FNumCLN,*) nCLN
!        
!        allocate(CLNType(nCLN))
!        CLNType(:)=0  ! by default a conventional well
!        
!        read(FNumCLN,'(a)') line   ! throw away line 2
!         determine CLN mumber of first extraction/injection well and flag CLNType
!        iCLNFirstEWIW=nCLN+1
!        do i=1,nCLN  
!            read(FNumCLN,'(a)') line  
!            l1=index(line,'Well = ')
!            if(l1>0) then
!                read(line(l1+7:),'(a)') ThisWellName
!                if(ThisWellName(1:2)=="EW") then 
!                    iCLNFirstEWIW=min(i,iCLNFirstEWIW)
!                    if(index(ThisWellName,'_A')>0) CLNType(i)=EW_upper
!                    if(index(ThisWellName,'_B')>0) CLNType(i)=EW_lower
!                else if(ThisWellName(1:2)=="IW") then
!                    iCLNFirstEWIW=min(i,iCLNFirstEWIW)
!                    if(index(ThisWellName,'_A')>0) CLNType(i)=IW_upper
!                    if(index(ThisWellName,'_B')>0) CLNType(i)=IW_lower
!                endif
!            endif
!        end do
!        
!        continue
!        
!        do j=1,nStressPeriods 
!             Sum extraction well rates
!            sumExtWellRates=0.0d0
!            do i=1,nExtWell
!                sumExtWellRates=sumExtWellrates+abs(ExtWellRate(i,j))
!            end do
!    
!             Normalize injection well fractions
!            sumInjWellFractions=0.0d0
!            do i=1,nInjWell
!                sumInjWellFractions=sumInjWellFractions+InjWellFraction(i,j)
!            end do
!            do i=1,nInjWell
!                InjWellFraction(i,j)=InjWellFraction(i,j)*1.0d0/sumInjWellFractions
!            end do
!            
!             check, new fractions should sum to 1
!            sumInjWellFractions=0.0d0
!            do i=1,nInjWell
!                sumInjWellFractions=sumInjWellFractions+InjWellFraction(i,j)
!            end do
!            
!            write(TMPStr,'(g15.3)') sumInjWellFractions
!            call Msg('Check sum of normalized injection well fractions is 1: '//TMPStr)
!        
!             Calculate injection well rates
!            do i=1,nInjWell
!                InjWellRate(i,j)=InjWellFraction(i,j)*sumExtWellrates
!                if(InjWellRate(i,j)>MaxInjRateGPM) InjWellRate(i,j)=MaxInjRateGPM
!            end do
!        
!        end do
!       
!
!        call FreeUnit(FNumPenalty)
!
!         Modflow-USG well file input file
!        read(FnumTG,'(a)') FNameMUSGWell  
!        call OpenAscii(FnumMUSGWell,FNameMUSGWell)
!        call Msg( 'Modflow-USG well input file: '//trim(FNameMUSGWell))
!        
!         Open new well output file
!        FNameMUSGWellOut='out_'//FNameMUSGWell  
!        call OpenAscii(FnumMUSGWellOut,FNameMUSGWellOut)
!        call Msg( 'Modflow-USG well output file: '//trim(FNameMUSGWellOut))
!
!        read(FNumMUSGWell,'(a)') line
!        write(FNumMUSGWellout,'(a)') line
!        
!        iStressPeriod=0
!        read_well_file: do 
!            read(FNumMUSGWell,'(a)',iostat=status) line
!            if(status /= 0) then  ! end of file
!                exit read_well_file
!            endif
!            
!            l1=index(line,'stress period')
!            if(l1 > 0) then ! new stress period
!                iStressPeriod=iStressPeriod+1
!                 write stress period header line
!                write(FNumMUSGWellout,'(a)') line
!            else  ! this is a well line
!                 Get well name
!                l1=index(line,'well = ')
!                if(l1>0) then
!                    read(line(l1+7:),'(a)') ThisWellName
!                    WellFound=.false.
!                    
!                     check if is an extraction well
!                    do i=1,nExtWell
!                        if(index(ExtWellName(i),ThisWellName) > 0) then ! found an extraction well
!                            read(line,'(i11,g15.7,a80)') Cln, rate, line
!                            if(iStressPeriod >= nint(PESTStressPeriodExtWellFlips)) then
!                                cln=cln+1
!                                line=trim(line)//'_B'    ! append suffix for lower screens
!                            endif
!                            write(FNumMUSGWellout,'(i11,g15.7,a80)') Cln, ExtWellRate(i,iStressPeriod)*192.5d0, line
!                            WellFound=.true.
!                            exit 
!                        endif
!                    end do
!                    
!                     check if is an injection well
!                    if(.not. WellFound) then
!                        do i=1,nInjWell
!                            if(index(InjWellName(i),ThisWellName) > 0) then ! found an injection well
!                                read(line,'(i11,g15.7,a80)') Cln, rate, line
!                                if(iStressPeriod >= nint(PESTStressPeriodInjWellFlips)) then
!                                    cln=cln+1
!                                    line=trim(line)//'_B'    ! append suffix for lower screens
!                                endif
!                                write(FNumMUSGWellout,'(i11,g15.7,a80)') Cln, InjWellRate(i,iStressPeriod)*192.5d0, line
!                                WellFound=.true.
!                                exit 
!                            endif
!                        end do
!                    endif
!
!                     normal well
!                    if(.not. WellFound) then
!                        write(FNumMUSGWellout,'(a)') line
!                    endif
!                end if 
!            endif
!        end do read_well_file  
!        
!         Modflow-USG transient ibound input file
!        read(FnumTG,'(a)') FNameTIB  
!        call OpenAscii(FnumTIB,FNameTIB)
!        call Msg( 'Modflow-USG transient ibound input file: '//trim(FNameTIB))
!        
!        read(FnumTG,*) nCells
!        
!        
!         Open new transient ibound output file
!        FNameTIBOut='out_'//FNameTIB 
!        call OpenAscii(FnumTIBOut,FNameTIBOut)
!        call Msg( 'Modflow-USG transient ibound output file: '//trim(FNameTIBOut))
!
!        read_tib_file: do 
!            read(FnumTIB,'(a)',iostat=status) line
!            if(status /= 0) then  ! end of file
!                exit read_tib_file
!            endif
!            
!            
!            l1=index(line,'stress period ')
!            if(l1 > 0) then ! new stress period
!                
!                l1=l1+14  ! position at end of string 'stress period '
!                
!                TMPStr=line(31:)
!
!                 Extract stress period from line
!                l2=l1+index(line(l1:),':')-2
!                read(line(l1:l2),*) iStressPeriod
!                
!                read(line,*) nCLNTurnOff, nCLNTurnOn, idum2
!                
!                if(iStressPeriod==1) then ! Always turn off lower screens of new CLN's unless PESTStressPeriodInjWellFlips=1 and/or PESTStressPeriodExtWellFlips=1
!               
!                     determine how many more CLN's are going to be turned off 
!                    nEWIWTurnoff=nCLNTurnOff
!                    do i=iCLNFirstEWIW,ncln
!                        if(PESTStressPeriodInjWellFlips>1 .and. CLNType(i)==IW_lower) nEWIWTurnoff=nEWIWTurnoff+1
!                        if(PESTStressPeriodExtWellFlips>1 .and. CLNType(i)==EW_lower) nEWIWTurnoff=nEWIWTurnoff+1
!                    end do
!                    
!                    write(FNumTIBOut,'(3i10,a)')  nEWIWTurnoff, nCLNTurnOn, idum2, trim(TMPStr)
!                    
!                    if(nCLNTurnOff>0) then  ! there were convetional well to be turned off
!                        read(FnumTIB,'(a)') line
!                        write(FNumTIBOut,'(a)') line ! always write next line as is  
!                         read and write the existing data
!                        read(FnumTIB,'(10i10)') (CLNArray(i),i=1,nCLNTurnOff)
!                        write(FnumTIBout,'(10i10)') (CLNArray(i),i=1,nCLNTurnOff)
!                    else if(nEWIWTurnoff>nCLNTurnOff) then   ! some or all IW/EW screens are turned off
!                        write(FNumTIBOut,'(a)') 'INTERNAL  1  (FREE)  -1  IB0 array'
!                        
!                         make a list of CLN numbers to be turned off
!                        nList=0
!                        CLNArray(:)=0
!                        do i=iCLNFirstEWIW,ncln
!                            if(PESTStressPeriodInjWellFlips>1 .and. CLNType(i)==IW_lower) then
!                                nList=nList+1
!                                CLNArray(nlist)=i+ncells                          
!                            else if (PESTStressPeriodExtWellFlips>1 .and. CLNType(i)==EW_lower) then
!                                nList=nList+1
!                                CLNArray(nlist)=i+ncells                              
!                            endif
!                        end do
!                        write(FnumTIBout,'(10i10)') (CLNArray(i),i=1,nlist)
!                    end if
!                   
!                    if(nCLNTurnOn>0) then  
!                        do i=1,nCLNTurnOn  ! read and write the next nCLNTurnOn lines
!                            read(FnumTIB,'(a)') line
!                            write(FNumTIBOut,'(a)') line ! always write next line as is  
!                        end do
!                    endif
!
!                else if(iStressPeriod == nint(PESTStressPeriodInjWellFlips) .and. &   ! turn on new cln lower screens for all injection (IW) wells
!                        iStressPeriod == nint(PESTStressPeriodExtWellFlips)) then     ! turn on new cln lower screens for all extraction (EW) wells
!                     determine how many new CLN's are going to be turned on 
!                    
!                    nEWIWTurnOn=nCLNTurnOn
!                    do i=iCLNFirstEWIW,ncln
!                        if(CLNType(i)==IW_lower) nEWIWTurnOn=nEWIWTurnOn+1
!                        if(CLNType(i)==EW_lower) nEWIWTurnOn=nEWIWTurnOn+1
!                    end do
!                   
!                    write(FNumTIBOut,'(3i10,a)')  nCLNTurnOff, nEWIWTurnOn, idum2, trim(TMPStr)
!                    
!                    if(nCLNTurnOff>0) then  
!                         read and write the existing data
!                        read(FnumTIB,'(10i10)') (CLNArray(i),i=1,nCLNTurnOff)
!                        write(FnumTIBout,'(10i10)') (CLNArray(i),i=1,nCLNTurnOff)
!                    end if
!
!                    if(nCLNTurnOn>0) then  
!                        do i=1,nCLNTurnOn  ! read and write the next nCLNTurnOn lines
!                            read(FnumTIB,'(a)') line
!                            write(FNumTIBOut,'(a)') line ! always write next line as is  
!                        end do
!                    endif
!
!                     make a list of CLN numbers to be turned on
!                    nList=0
!                    CLNArray(:)=0
!                    do i=iCLNFirstEWIW,ncln
!                        if(CLNType(i)==IW_lower) then
!                            nList=nList+1
!                            CLNArray(nlist)=i+ncells                          
!                        else if(CLNType(i)==EW_lower) then
!                            nList=nList+1
!                            CLNArray(nlist)=i+ncells                          
!                        endif
!                    end do
!                    write(FnumTIBout,'(i10,a)') (CLNArray(i),'            AVHEAD',i=1,nlist)
!                    
!               else if(iStressPeriod == nint(PESTStressPeriodInjWellFlips)) then ! turn on new cln lower screens for injection (IW) wells
!                     determine how many new CLN's are going to be turned on 
!                    
!                    nEWIWTurnOn=nCLNTurnOn
!                    do i=iCLNFirstEWIW,ncln
!                        if(CLNType(i)==IW_lower) nEWIWTurnOn=nEWIWTurnOn+1
!                    end do
!                   
!                    write(FNumTIBOut,'(3i10,a)')  nCLNTurnOff, nEWIWTurnOn, idum2, trim(TMPStr)
!                    
!                    if(nCLNTurnOff>0) then  
!                         read and write the existing data
!                        read(FnumTIB,'(10i10)') (CLNArray(i),i=1,nCLNTurnOff)
!                        write(FnumTIBout,'(10i10)') (CLNArray(i),i=1,nCLNTurnOff)
!                    end if
!
!                    if(nCLNTurnOn>0) then  
!                        do i=1,nCLNTurnOn  ! read and write the next nCLNTurnOn lines
!                            read(FnumTIB,'(a)') line
!                            write(FNumTIBOut,'(a)') line ! always write next line as is  
!                        end do
!                    endif
!
!                     make a list of CLN numbers to be turned on
!                    nList=0
!                    CLNArray(:)=0
!                    do i=iCLNFirstEWIW,ncln
!                        if(CLNType(i)==IW_lower) then
!                            nList=nList+1
!                            CLNArray(nlist)=i+ncells                          
!                        endif
!                    end do
!                    write(FnumTIBout,'(i10,a)') (CLNArray(i),'            AVHEAD',i=1,nlist)
!                    
!                else if(iStressPeriod == nint(PESTStressPeriodExtWellFlips)) then ! turn on new cln lower screens for extraction (EW) wells
!                     determine how many new CLN's are going to be turned on 
!                    nEWIWTurnOn=nCLNTurnOn
!                    do i=iCLNFirstEWIW,ncln
!                        if(CLNType(i)==EW_lower) nEWIWTurnOn=nEWIWTurnOn+1
!                    end do
!                   
!                    write(FNumTIBOut,'(3i10,a)')  nCLNTurnOff, nEWIWTurnOn, idum2, trim(TMPStr)
!                    
!                    if(nCLNTurnOff>0) then  
!                         read and write the existing data
!                        read(FnumTIB,'(10i10)') (CLNArray(i),i=1,nCLNTurnOff)
!                        write(FnumTIBout,'(10i10)') (CLNArray(i),i=1,nCLNTurnOff)
!                    end if
!
!                    if(nCLNTurnOn>0) then  
!                        do i=1,nCLNTurnOn  ! read and write the next nCLNTurnOn lines
!                            read(FnumTIB,'(a)') line
!                            write(FNumTIBOut,'(a)') line ! always write next line as is  
!                        end do
!                    endif
!
!                     make a list of CLN numbers to be turned on
!                    nList=0
!                    CLNArray(:)=0
!                    do i=iCLNFirstEWIW,ncln
!                        if(CLNType(i)==EW_lower) then
!                            nList=nList+1
!                            CLNArray(nlist)=i+ncells                          
!                        endif
!                    end do
!                    write(FnumTIBout,'(i10,a)') (CLNArray(i),'            AVHEAD',i=1,nlist)
!                    
!
!                else    
!                    write(FNumTIBOut,'(3i10,a)')  nCLNTurnOff, nCLNTurnOn, idum2, trim(TMPStr)
!                    if(nCLNTurnOff>0) then  
!                        read(FnumTIB,'(a)') line
!                        write(FNumTIBOut,'(a)') line ! always write next line as is  
!                         read and write the existing data
!                        read(FnumTIB,'(10i10)') (CLNArray(i),i=1,nCLNTurnOff)
!                        write(FnumTIBout,'(10i10)') (CLNArray(i),i=1,nCLNTurnOff)
!                    end if
!                    
!                    if(nCLNTurnOn>0) then  
!                        do i=1,nCLNTurnOn  ! read and write the next iDum2 lines
!                            read(FnumTIB,'(a)') line
!                            write(FNumTIBOut,'(a)') line ! always write next line as is  
!                        end do
!                    endif
!                    if(iDum2>0) then  
!                        continue
!                    endif
!                endif
!                
!               
!            end if
!            
!            
!        end do read_tib_file
!
!        continue
!
!        
!    end subroutine MUSG_PEST_WellRatePenalties
!    
!
!
!    subroutine MUSG_PEST_UpdateWellRatePenalties(FnumTG) !--- Update rates in Modflow-USG well file
!        implicit none 
!        
!        integer :: i, j
!
!        integer :: FnumTG
!        integer :: FnumPenalty
!        character(MAXLBL) :: FNamePenalty
!        integer :: nRange
!        real(dr), allocatable :: MinPenalty(:)
!        real(dr), allocatable :: MaxPenalty(:)
!        real(dr), allocatable :: MinRange(:)
!        real(dr), allocatable :: MaxRange(:)
!        real(dr) :: InjRatePercentTargetLowerMin
!        real(dr) :: InjRatePercentTargetLowerMax
!        real(dr) :: InjratePercentPenaltyLowerMin
!        real(dr) :: InjratePercentPenaltyLowerMax
!        real(dr) :: InjRatePercentTargetHigher
!        real(dr) :: InjratePercentPenaltyHigher
!        real(dr) :: PESTStressPeriodInjWellFlips
!        real(dr) :: PESTStressPeriodExtWellFlips
!
!
!        real(dr) :: MinExtRate
!        real(dr) :: MaxInjRateGPM
!        
!       
!         CLN file
!        integer :: FnumCLN
!        character(MAXLBL) :: FNameCLN
!        integer :: nCLN
!
!        integer :: FNumWellCompOut
!        character(MAXLBL) :: FNameWellCompOut
!
!         Calculations
!        real(dr) :: sumExtWellRates
!        real(dr) :: ExtWellRatePenalty
!        real(dr) :: sumInjWellRates
!        real(dr) :: InjWellRatePenalty
!        
!         Modflow-USG well file
!        integer :: iStressPeriod
!        character(MAXLBL) :: line
!        character(20), allocatable :: ThisWellName(:)
!        
!        
!        real(dr) :: req_rate
!        real(dr) :: act_rate
!        integer :: icln
!        
!        real(dr) :: RateRatio
!        
!        
!         CLN information file
!        read(FNumTG,'(a)') FNameCLN
!        call OpenAscii(FNumCLN,FNameCLN)
!        call Msg( 'CLN file: '//trim(FNameCLN))
!
!        read(FNumCLN,*) nCLN
!        allocate(ThisWellName(nCLN))
!        read(FNumCLN,'(a)') line   ! throw away line 2
!         store well names
!        do i=1,nCLN  
!            read(FNumCLN,'(a)') line  
!            l1=index(line,'Well = ')
!            if(l1>0) then
!                read(line(l1+7:),'(a)') ThisWellName(i)
!            endif
!        end do
!        
!         Penalties input file
!        read(FnumTG,'(a)') FNamePenalty  
!        call OpenAscii(FNumPenalty,FNamePenalty)
!        call Msg( 'Penalties file: '//trim(FNamePenalty))
!    
!        
!        read(FNumPenalty,*) MinExtRate
!        write(TMPStr,'(4f15.3)') MinExtRate
!        call Msg( 'Minimum extraction rate: '//trim(TMPStr))
!        read(FNumPenalty,*) nRange
!        allocate(MinRange(nRange), MaxRange(nRange), MinPenalty(nRange), MaxPenalty(nRange))
!        do i=1,nRange
!            read(FNumPenalty,*) MinRange(i), MaxRange(i), MinPenalty(i), MaxPenalty(i)
!            write(TMPStr,'(4f15.3)') MinRange(i), MaxRange(i), MinPenalty(i), MaxPenalty(i)
!            call Msg( 'Rate1 Rate2 Penalty1 Penalty2: '//trim(TMPStr))
!            if(i>1) then
!                if(Minrange(i)/=MaxRange(i-1)) then
!                    write(ErrStr,'(a,2f10.2)') 'Range 1: ', MinRange(i-1), MaxRange(i-1)
!                    call Msg(ErrStr)
!                    write(ErrStr,'(a,2f10.2)') 'Range 2: ', MinRange(i), MaxRange(i)
!                    call Msg(ErrStr)
!                    call ErrMsg('Min range 2 not equal to max range 1')
!                end if
!                if(MinPenalty(i)/=MaxPenalty(i-1)) then
!                    write(ErrStr,'(a,2f10.2)') 'Penalty 1: ', MinPenalty(i-1), MaxPenalty(i-1)
!                    call Msg(ErrStr)
!                    write(ErrStr,'(a,2f10.2)') 'Penalty 2: ', MinPenalty(i), MaxPenalty(i)
!                    call Msg(ErrStr)
!                    call ErrMsg('Min penalty 2 not equal to max penalty 1')
!                end if
!            end if
!        end do
!
!        read(FNumPenalty,*) MaxInjRateGPM
!        write(TMPStr,*) MaxInjRateGPM
!        call Msg( 'Maximum injection rate: '//trim(TMPStr))
!
!        read(FNumPenalty,*) InjRatePercentTargetLowerMin,InjRatePercentTargetLowerMax, InjratePercentPenaltyLowerMin, InjratePercentPenaltyLowerMax
!        write(TMPStr,'(4f15.3)') InjRatePercentTargetLowerMin,InjRatePercentTargetLowerMax, InjratePercentPenaltyLowerMin, InjratePercentPenaltyLowerMax
!        call Msg( 'Injection Rate%1 Rate%2 Penalty1 Penalty2: '//trim(TMPStr))
!
!        read(FNumPenalty,*) InjRatePercentTargetHigher, InjratePercentPenaltyHigher
!        write(TMPStr,'(4f15.3)') InjRatePercentTargetHigher, InjratePercentPenaltyHigher
!        call Msg( 'InjRatePercentTargetHigher InjratePercentPenaltyHigher: '//trim(TMPStr))
!
!        read(FNumPenalty,*) PESTStressPeriodInjWellFlips
!        write(TMPStr,*) nint(PESTStressPeriodInjWellFlips)
!        call Msg( 'Injection (IW) Wells flip over at stress period: '//trim(TMPStr))
!       
!        read(FNumPenalty,*) PESTStressPeriodExtWellFlips
!        write(TMPStr,*) nint(PESTStressPeriodExtWellFlips)
!        call Msg( 'Extraction (EW) Wells flip over at stress period: '//trim(TMPStr))
!       
!        
!         Wellcomp output  file
!        read(FnumTG,'(a)') FNameWellCompOut
!        call OpenAscii(FNumWellCompOut,FNameWellCompOut)
!        call Msg( 'WellComp output file: '//trim(FNameWellCompOut))
!
!         Penalties output file
!        FNamePenalty='PenaltiesBySP.txt'
!        call OpenAscii(FNumPenalty,FNamePenalty)
!        call Msg( 'Penalties by stress period output file: '//trim(FNamePenalty))
!        write(FNumPenalty,'(a)') '      SP#          ExtRate             ExtPenalty            InjRate           InjPenalty'
!
!        read_wellcomp_file: do 
!            read(FNumWellCompOut,'(a)',iostat=status) line
!            if(status /= 0) then  ! end of file
!                exit read_wellcomp_file
!            endif
!            
!            l1=index(line,'Stress period = ')
!            if(l1 > 0) then ! new stress period
!                read(line(l1+16:),*) iStressPeriod
!                sumExtWellRates=0.0d0
!                sumInjWellRates=0.0d0
!                stress_period: do 
!                    read(FNumWellCompOut,'(a)') line  ! blank line
!                    l1=index(line,'Node         Connection')
!                    if(l1 > 0) then ! Start reading cln infoes
!                        cln_lines: do 
!                            read(FNumWellCompOut,'(a)') line  
!                            l1=index(line,'CLN')
!                            if(l1 > 0) then ! read and process cln info 
!                                read(line,*) icln
!                                read(line(l1+4:),*) req_rate,act_rate
!                                if(index(ThisWellName(icln),'EW') > 0) then  ! extraction well 
!                                    sumExtWellRates=sumExtWellrates+abs(act_rate)/192.5d0
!                                else if(index(ThisWellName(icln),'IW') > 0) then ! injection well
!                                    sumInjWellRates=sumInjWellRates+abs(act_rate)/192.5d0
!                                end if
!                            else
!                                do j=1,nrange
!                                    if(sumExtWellrates>= MinRange(j) .and. sumExtWellRates<= MaxRange(j)) then ! linear interpolation for penalty
!                                        ExtWellRatePenalty= MinPenalty(j)+(sumExtWellRates-MinRange(j))/(MaxRange(j)-MinRange(j))*(MaxPenalty(j)-MinPenalty(j))
!                                    endif
!                                end do
!                                
!                                InjWellRatePenalty=0.0d0
!                                if(sumExtWellRates == 0.0d0) then
!                                    RateRatio=1.0d0
!                                else
!                                    RateRatio=sumInjWellRates/sumExtWellRates
!
!                                    if(RateRatio < InjRatePercentTargetLowerMax) then
!                                        InjWellRatePenalty=InjRatePercentPenaltyLowerMin+(InjRatePercentTargetLowerMax-RateRatio)/(InjRatePercentTargetLowerMax-InjRatePercentTargetLowerMin) &
!                                &        * (InjRatePercentPenaltyLowerMax-InjRatePercentPenaltyLowerMin)     
!                                    else if(RateRatio > InjRatePercentTargetHigher) then
!                                        InjWellRatePenalty=InjRatePercentPenaltyHigher
!                                    endif
!                               
!                                endif
!       
!                                write(FNumPenalty,'(i8,4f20.5)') IstressPeriod, sumExtWellRates, ExtWellRatePenalty, RateRatio, InjWellRatePenalty
!                              
!                                exit stress_period
!                            endif
!                        end do cln_lines
!                    end if
!                end do stress_period
!            end if
!            
!        end do read_wellcomp_file
!        
!    end subroutine MUSG_PEST_UpdateWellRatePenalties
!
!    subroutine MUSG_PEST_FlowSourceCapture(FnumTG) !--- Average flows for 
!        USE IFPORT 
!        implicit none 
!        
!        integer :: i, j
!
!        LOGICAL(4) result 
!
!        integer :: FnumTG
!        integer :: FnumCSV
!        character(MAXLBL) :: FNameCSV
!        character(MAXLBL) :: FlowSourceDir
!        character(MAXLBL) :: CMD_line
!        character(MAXLBL) :: line
!        integer :: nCSVFiles
!        integer :: FnumCSVtmp
!        integer :: nCells
!                
!        real(dr), allocatable :: Flowthrough(:,:)
!        integer :: iDum
!        
!        integer :: iCSV
!        integer :: iGroup
!        integer :: FnumCellList
!        character(MAXLBL), allocatable :: FNameCellList(:)
!        real(dr), allocatable :: FS_Average(:,:)
!        integer :: jCell
!        integer :: nCellListFiles
!        integer, allocatable :: nCountGroup(:)
!        real(dr), allocatable :: StressPeriod(:)
!        integer, allocatable :: indx_col(:)
! 
! 
!        integer :: FnumGroupAverage
!        character(MAXLBL) :: FNameGroupAverage
!
!        read(FNumTG,'(a)') FlowSourceDir
!        
!         All of the .csv file in the directory FlowSourceDir are flowsource output files
!        CMD_line='dir /b '//trim(FlowSourceDir)//'\*.csv > csvfiles'
!        result = SYSTEMQQ(CMD_line)
!       
!         count the number of csv files
!        FNameCSV='csvfiles'
!        call OpenAscii(FnumCSV,FNameCSV)
!        call Msg('CSV filenames file: '//trim(FNameCSV))
!        nCSVFiles=0
!        do 
!            read(FnumCSV,'(a)',iostat=status) line
!            if(status /= 0) then
!                exit
!            endif
!            
!            nCSVFiles=nCSVFiles+1
!        end do
!        
!         count the number of cells in the last CSV file
!        call OpenAscii(FnumCSVTmp,trim(FlowSourceDir)//'\'//trim(line))
!        nCells=0
!        do 
!            read(FnumCSVTmp,'(a)',iostat=status) line
!            if(status /= 0) then
!                exit
!            endif
!            
!            nCells=nCells+1
!            
!        end do
!        
!        nCells=nCells-1
!        
!        call FreeUnit(FnumCSVTmp)
!        
!         dimension flowsource arrays
!        allocate(Flowthrough(nCSVFiles,nCells),StressPeriod(nCSVFiles),indx_col(nCSVFiles))
!        
!        rewind(FNumCSV)
!        do i=1,nCSVFiles
!            read(FnumCSV,'(a)',iostat=status) line
!            
!             Extract stress period from line
!            l1=index(line,'_Time-')+6
!            l2=l1+index(line(l1:),'_')-2
!            read(line(l1:l2),*) StressPeriod(i)
!            
!            call OpenAscii(FnumCSVTmp,trim(FlowSourceDir)//'\'//trim(line))
!            call Msg('CSV file: '//trim(FlowSourceDir)//'\'//trim(line))
!            read(FnumCSVTmp,'(a)') line   ! throw away header
!            do j=1,nCells
!                read(FnumCSVTmp,*) iDum,Flowthrough(i,j)
!            end do
!        end do
!        
!         Process cell list files
!        read(FNumTG,*) nCellListFiles
!        allocate(nCountGroup(nCellListFiles),FS_average(nCSVFiles,nCellListFiles),FNameCellList(nCellListFiles))
!        nCountGroup(:)=0
!        FS_average(:,:)=0.0d0
!        do iGroup=1,nCellListFiles
!            read(FNumTG,'(a)') FNameCellList(iGroup)
!            call OpenAscii(FnumCellList,FNameCellList(iGroup))
!            call Msg('Cell list file: '//trim(FNameCellList(iGroup)))
!            do 
!                read(FnumCellList,*,iostat=status) jCell   
!                if(status/=0) then
!                    exit
!                endif
!                
!                nCountGroup(iGroup)=nCountGroup(iGroup)+1
!                
!                do iCSV=1,nCSVFiles
!                    FS_average(iCSV,iGroup)=FS_average(iCSV,iGroup)+Flowthrough(iCSV,jCell)
!                end do
!            
!            end do
!            
!        end do
!        
!         Calculate average for each group and csv file
!        do iGroup=1,nCellListFiles
!            do iCSV=1,nCSVFiles
!                FS_average(iCSV,iGroup)=FS_average(iCSV,iGroup)/nCountGroup(iGroup)
!            end do
!        end do
!        
!         Sort by StressPeriod. Order written to indx_col.
!        call indexx3(nCSVFiles,StressPeriod,indx_col) 
!
!        
!        
!        continue
!       
!         Write one output file per group
!        do iGroup=1,nCellListFiles
!            FNameGroupAverage='avg_'//trim(FNameCellList(iGroup))//'.csv'
!            call OpenAscii(FNumGroupAverage,FNameGroupAverage)
!            call Msg('Group FS average csv output file: '//trim(FNameGroupAverage))
!            write(FNumGroupAverage,'(a)') "Stress Period, Average Capture"
!            do iCSV=1,nCSVFiles
!               write(FNumGroupAverage,'(f10.0,a,f12.5)') StressPeriod(indx_col(iCSV)),',',FS_average(indx_col(iCSV),iGroup)*100.0d0
!            end do
!            call FreeUnit(FNumGroupAverage)
!        end do
!            
!       
!      
!    end subroutine MUSG_PEST_FlowSourceCapture
!
!    subroutine MUSG_PEST_ExternalCodeExecute(FnumTG) !--- Execute an external program as defined in the .tg file. 
!        USE IFPORT 
!        implicit none 
!
!        integer :: FnumTG
!        LOGICAL(4) result 
!        character(MAXLBL) :: CMD_line
!
!        read(FNumTG,'(a)') CMD_line
!        result = SYSTEMQQ(trim(CMD_line))
!
!    end subroutine MUSG_PEST_ExternalCodeExecute
    
    !subroutine MUSG_PEST_CLNFileCalculations(FnumTG, Modflow) !--- Given an EI pumping well, calculate the CLN file entries
    !    USE IFPORT 
    !    implicit none 
    !
    !    type (MUSG_Project) Modflow
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
    !    do i=1, Modflow.nWellConst
    !        if(index(Modflow.NameWellConst(i),trim(WellID)) > 0) then
    !            
    !            do j=1,Modflow.gwf.nCell/Modflow.gwf.nLay  ! loop over the cells in layer 1
    !                if(Modflow.XWellConst(i) >= Modflow.gwf.X(Modflow.gwf.ivertex(1,j)) .and. Modflow.XWellConst(i) <= Modflow.gwf.X(Modflow.gwf.ivertex(4,j))) then
    !                
    !                    if(Modflow.YWellConst(i) >= Modflow.gwf.Y(Modflow.gwf.ivertex(1,j)) .and. Modflow.YWellConst(i) <= Modflow.gwf.Y(Modflow.gwf.ivertex(2,j))) then
    !                        iCellCurr=j
    !                        write(*,'(a,i8,3f15.3)') ' Cell x y z',iCellCurr, Modflow.gwf.xCell(iCellCurr), Modflow.gwf.yCell(iCellCurr), Modflow.gwf.zCell(iCellCurr)
    !                        write(*,*) ' k, vertex(k), Xvertex(k), Yvertex(k), Zvertex(k)'
    !                        do k=1,Modflow.gwf.m  
    !                            write(*,'(i2,i8,3f15.3)') k, Modflow.gwf.ivertex(k,iCellCurr),Modflow.gwf.X(Modflow.gwf.ivertex(k,iCellCurr)),Modflow.gwf.Y(Modflow.gwf.ivertex(k,iCellCurr)),Modflow.gwf.Z(Modflow.gwf.ivertex(k,iCellCurr))
    !                        end do
    !                        pause
    !                        CurrTopElev=Modflow.TopElevWellConst(i)
    !                        LayerLoop: do k=1,Modflow.gwf.nLay 
    !                            if(CurrTopElev > Modflow.gwf.Z(Modflow.gwf.ivertex(8,iCellCurr)))  then  ! if current screen top > current cell bottom
    !                                if(Modflow.BotElevWellConst(i) > Modflow.gwf.Z(Modflow.gwf.ivertex(8,iCellCurr))) then ! if current screen bot > current cell bottom
    !                                    CellHeight=CurrTopElev-Modflow.BotElevWellConst(i)
    !                                    write(*,*) iCellCurr, CellHeight
    !                                    exit LayerLoop
    !                                else
    !                                    CellHeight=CurrTopElev-Modflow.gwf.Z(Modflow.gwf.ivertex(8,iCellCurr))
    !                                    write(*,*) iCellCurr, CellHeight
    !                                    CurrTopElev=Modflow.gwf.Z(Modflow.gwf.ivertex(8,iCellCurr))
    !                                    iCellCurr=iCellCurr+Modflow.gwf.nCell/Modflow.gwf.nLay
    !                                endif
    !                            else
    !                                iCellCurr=iCellCurr+Modflow.gwf.nCell/Modflow.gwf.nLay
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
    !
    !subroutine MUSG_PEST_EIWellCLNFileUpdate(FnumTG, Modflow) !--- Given new EI pumping wells, add screens as new CLN file entries
    !    USE IFPORT 
    !    implicit none 
    !
    !    type (MUSG_Project) Modflow
    !
    !
    !    integer :: i,j, k
    !
    !    integer :: FnumTG
    !    
    !    integer :: FnumCLN
    !    character(MAXLBL) :: FNameCLN
    !    integer :: FnumCLNout
    !    character(MAXLBL) :: FNameCLNout
    !
    !    integer :: FNumFSCtl
    !    character(MAXLBL) :: FnameFSCtl
    !    integer :: FNumFSCtlOut
    !    character(MAXLBL) :: FnameFSCtlOut
    !
    !    integer :: FNumWel
    !    character(MAXLBL) :: FnameWel
    !    integer :: FNumWelOut
    !    character(MAXLBL) :: FnameWelOut
    !    
    !    integer :: iCellCurr
    !    real(dr) :: CurrTopElev
    !    real(dr) :: CurrBotElev
    !    real(dr) :: CurrScreenLength
    !    logical :: ScreenFound
    !    integer :: nEIScreens
    !    real(dr) :: MeshBottom
    !    
    !    
    !    integer, allocatable :: IFNO(:)
    !    integer, allocatable :: IFTYP(:) 
    !    integer, allocatable :: IFDIR(:) 
    !    real(dr), allocatable :: FLENG(:) 
    !    real(dr), allocatable :: FELEV(:) 
    !    real(dr), allocatable :: FANGLE(:) 
    !    integer, allocatable :: IFLIN(:)
    !    integer, allocatable :: ICCWADI(:)
    !    
    !    integer, allocatable :: nCellList(:)
	   ! character(31), allocatable :: NameEIScreen(:)
    !    integer, allocatable :: CellNumber(:,:)
    !    real(dr), allocatable :: CellScreenLength(:,:)
    !    
    !    real(dr), allocatable :: StartingHeads(:)
    !    
    !    integer :: i1, i2, i3, i4, i5
    !    integer :: ICLNOrig, ICLNNew, myNCONDUITYP
    !    integer :: ICellListOrig, ICellListNew
    !    integer :: nSum
    !    character(MAXSTRING) :: line
    !    
    !    logical :: WellFound
    !
    !    integer :: IWellOrig, IWellNew
    !
    !    
    !    allocate(IFNO(2*Modflow.n_EIWell), &
    !    &   IFTYP(2*Modflow.n_EIWell), &
    !    &   IFDIR(2*Modflow.n_EIWell), &
    !    &   FLENG(2*Modflow.n_EIWell), &
    !    &   FELEV(2*Modflow.n_EIWell), &
    !    &   FANGLE(2*Modflow.n_EIWell), &
    !    &   IFLIN(2*Modflow.n_EIWell), &
    !    &   ICCWADI(2*Modflow.n_EIWell))
    !    
    !    IFNO(:) = 0
    !    IFTYP(:) = 1
    !    IFDIR(:) = 0
    !    FLENG(:) = 0.0d0
    !    FELEV(:) = 0.0d0
    !    FANGLE(:) = 0.0d0
    !    IFLIN(:) = 1
    !    ICCWADI(:) = 0
    !    
    !    allocate(nCellList(2*Modflow.n_EIWell), &
    !    &   NameEIScreen(2*Modflow.n_EIWell), & 
    !    &   CellNumber(2*Modflow.n_EIWell,100),& 
    !    &   CellScreenLength(2*Modflow.n_EIWell,100))
    !    
    !    nEIScreens=0
    !    
    !    !-----------------------------------------------------------------------------------
    !    ! Find where screens fit in 3D Modflow domain
    !    WellSearch:do i=1, Modflow.n_EIWell
    !        call Msg('------------------------------------------------------------------------------')
    !        call Msg('EI Well: '//trim(Modflow.Name_EIWell(i)))
    !        write(TmpSTR,'(a, 2f15.3)') 'X Y: ',Modflow.X_EIWell(i), Modflow.Y_EIWell(i)
    !        call Msg(TmpSTR)
    !        CurrBotElev=Modflow.TopElev_EIWell(i)- Modflow.ScreenALength_EIWell(i)
    !        call Msg('       Elevation       Length      Comment')
    !        write(TmpSTR,'(f15.3,a)') Modflow.TopElev_EIWell(i), '           -        Screen A top elevation '
    !        call Msg(TmpSTR)
    !        write(TmpSTR,'(2f15.3,5x,a)') CurrBotElev, Modflow.ScreenALength_EIWell(i), 'Screen A bottom elevation, screen A length'
    !        call Msg(TmpSTR)
    !        CurrBotElev=CurrBotElev- Modflow.ScreenBOffset_EIWell(i)
    !        write(TmpSTR,'(2f15.3,5x,a)') CurrBotElev, Modflow.ScreenBOffset_EIWell(i), 'Screen B top elevation, screen B offset'
    !        call Msg(TmpSTR)
    !        CurrBotElev=CurrBotElev- Modflow.ScreenBLength_EIWell(i)
    !        write(TmpSTR,'(2f15.3,5x,a)') CurrBotElev, Modflow.ScreenBLength_EIWell(i), 'Screen B bottom elevation, screen B length'
    !        call Msg(TmpSTR)
    !        
    !        WellFound=.false.
    !             
    !        do j=1,Modflow.gwf.nCell/Modflow.gwf.nLay  ! loop over the cells in layer 1
    !            if(Modflow.X_EIWell(i) >= Modflow.gwf.X(Modflow.gwf.ivertex(1,j)) .and. Modflow.X_EIWell(i) <= Modflow.gwf.X(Modflow.gwf.ivertex(4,j))) then
    !                if(Modflow.Y_EIWell(i) >= Modflow.gwf.Y(Modflow.gwf.ivertex(1,j)) .and. Modflow.Y_EIWell(i) <= Modflow.gwf.Y(Modflow.gwf.ivertex(2,j))) then
    !                    iCellCurr=j
    !                    WellFound=.true.
    !
    !                    write(TmpSTR,'(a,i8,a,2f15.3)') 'Found in cell ', iCellCurr,' at cell centroid X Y ', Modflow.gwf.xCell(iCellCurr), Modflow.gwf.yCell(iCellCurr)
    !                    call Msg(TmpSTR)
    !
    !                    write(TmpSTR,'(a,2f15.3)') 'X range ', Modflow.gwf.X(Modflow.gwf.ivertex(1,j)), Modflow.gwf.X(Modflow.gwf.ivertex(4,j))
    !                    call Msg(TmpSTR)
    !                    write(TmpSTR,'(a,2f15.3)') 'Y range ', Modflow.gwf.Y(Modflow.gwf.ivertex(1,j)), Modflow.gwf.Y(Modflow.gwf.ivertex(2,j))
    !                    call Msg(TmpSTR)
    !
    !                    
    !                    
    !                    call Msg(' Layer  Cell     Vertex         Z      Height')
    !                    write(TmpSTR,'(i5,i8,i8,f15.3)') 0, iCellCurr, 4,Modflow.gwf.Z(Modflow.gwf.ivertex(4,iCellCurr))
    !                    call Msg(TmpSTR)
    !                    CurrTopElev=Modflow.gwf.Z(Modflow.gwf.ivertex(4,iCellCurr))
    !                    do k=1,Modflow.gwf.nLay                             
    !                        write(TmpSTR,'(i5,i8,i8,5f15.3)') k, iCellCurr,8, Modflow.gwf.Z(Modflow.gwf.ivertex(8,iCellCurr)),CurrTopElev-Modflow.gwf.Z(Modflow.gwf.ivertex(8,iCellCurr))
    !                        call Msg(TmpSTR)
    !                        if(k==Modflow.gwf.nLay) then
    !                            MeshBottom= Modflow.gwf.Z(Modflow.gwf.ivertex(8,iCellCurr))
    !                        endif
    !                        CurrTopElev=Modflow.gwf.Z(Modflow.gwf.ivertex(8,iCellCurr))
    !                        iCellCurr=iCellCurr+Modflow.gwf.nCell/Modflow.gwf.nLay
    !                    end do
    !                    
    !                    iCellCurr=j
    !
    !                    
    !                    ! Top screen A
    !                    CurrTopElev=Modflow.TopElev_EIWell(i)
    !                    CurrBotElev=CurrTopElev-Modflow.ScreenALength_EIWell(i)
    !                    CurrScreenLength=0.0d0
    !                    ScreenFound=.false.
    !                    LayerLoop1: do k=1,Modflow.gwf.nLay 
    !                        if(CurrTopElev > Modflow.gwf.Z(Modflow.gwf.ivertex(8,iCellCurr)))  then  ! if current screen top > current cell bottom
    !                            if(.not. ScreenFound) then
    !                                nEIScreens=nEIScreens+1
    !                                ScreenFound=.true.
    !                                call Msg(' +Scr A    Cell      ScreenLength')
    !                                nCellList(nEIScreens)=0
    !                                NameEIScreen(nEIScreens)=trim(Modflow.Name_EIWell(i))//'_A'
    !                            end if
    !                            if(CurrBotElev > Modflow.gwf.Z(Modflow.gwf.ivertex(8,iCellCurr))) then ! if current screen bot > current cell bottom
    !                                nCellList(nEIScreens)=nCellList(nEIScreens)+1  
    !                                CellNumber(nEIScreens,nCellList(nEIScreens))= iCellCurr
    !                                CellScreenLength(nEIScreens,nCellList(nEIScreens)) = CurrTopElev-CurrBotElev
    !                                write(TmpSTR,'(i5,i8,f15.3)') nEIScreens,CellNumber(nEIScreens,nCellList(nEIScreens)), CellScreenLength(nEIScreens,nCellList(nEIScreens))
    !                                call Msg(TmpSTR)
    !                                CurrScreenLength=CurrScreenLength+CellScreenLength(nEIScreens,nCellList(nEIScreens))
    !                                exit LayerLoop1
    !                            else
    !
    !                                nCellList(nEIScreens)=nCellList(nEIScreens)+1  
    !                                CellNumber(nEIScreens,nCellList(nEIScreens))= iCellCurr
    !                                CellScreenLength(nEIScreens,nCellList(nEIScreens)) = CurrTopElev-Modflow.gwf.Z(Modflow.gwf.ivertex(8,iCellCurr))
    !                                write(TmpSTR,'(i5,i8,f15.3)') nEIScreens,CellNumber(nEIScreens,nCellList(nEIScreens)), CellScreenLength(nEIScreens,nCellList(nEIScreens))
    !                                call Msg(TmpSTR)
    !                                CurrScreenLength=CurrScreenLength+CellScreenLength(nEIScreens,nCellList(nEIScreens))
    !                                
    !                                CurrTopElev=Modflow.gwf.Z(Modflow.gwf.ivertex(8,iCellCurr))
    !                                iCellCurr=iCellCurr+Modflow.gwf.nCell/Modflow.gwf.nLay
    !                            endif
    !                        else
    !                            iCellCurr=iCellCurr+Modflow.gwf.nCell/Modflow.gwf.nLay
    !                            if(iCellCurr > Modflow.gwf.nCell .and. ScreenFound) then
    !                                nEIScreens=nEIScreens+1
    !                            endif
    !                                
    !                        end if
    !                    end do LayerLoop1
    !                    
    !                    if(ScreenFound) then
    !                        FLENG(nEIScreens) = CurrScreenLength
    !                        FELEV(nEIScreens) = CurrBotElev
    !                        
    !                        If(FELEV(nEIScreens)< MeshBottom) then
    !                            write(TmpSTR,'(a,f15.3,a,f15.3)') 'Bottom of screen FELEV ',FELEV(nEIScreens),' below mesh bottom ',MeshBottom
    !                            call ErrMsg(TmpSTR)
    !                        endif
    !                        
    !                        call Msg('         FLENG         FELEV')
    !                        write(TmpSTR,'(2f15.3)') FLENG(nEIScreens),FELEV(nEIScreens)
    !                        call Msg(TmpSTR)
    !                    end if
    !                    
    !                    
    !                    ! Screen B
    !                    CurrTopElev=CurrBotElev-Modflow.ScreenBOffset_EIWell(i)
    !                    CurrBotElev=CurrTopElev-Modflow.ScreenBLength_EIWell(i)
    !                    CurrScreenLength=0.0d0
    !                    ScreenFound=.false.
    !                    iCellCurr=j
    !                    LayerLoop2: do k=1,Modflow.gwf.nLay 
    !                        if(CurrTopElev > Modflow.gwf.Z(Modflow.gwf.ivertex(8,iCellCurr)))  then  ! if current screen top > current cell bottom
    !                            if(.not. ScreenFound) then
    !                                nEIScreens=nEIScreens+1
    !                                ScreenFound=.true.
    !                                call Msg(' +Scr B    Cell      ScreenLength')
    !                                nCellList(nEIScreens)=0
    !                                NameEIScreen(nEIScreens)=trim(Modflow.Name_EIWell(i))//'_B'
    !                            endif
    !                            if(CurrBotElev > Modflow.gwf.Z(Modflow.gwf.ivertex(8,iCellCurr))) then ! if current screen bot > current cell bottom
    !
    !                                nCellList(nEIScreens)=nCellList(nEIScreens)+1  
    !                                CellNumber(nEIScreens,nCellList(nEIScreens))= iCellCurr
    !                                CellScreenLength(nEIScreens,nCellList(nEIScreens)) = CurrTopElev-CurrBotElev
    !                                write(TmpSTR,'(i5,i8,f15.3)') nEIScreens,CellNumber(nEIScreens,nCellList(nEIScreens)), CellScreenLength(nEIScreens,nCellList(nEIScreens))
    !                                call Msg(TmpSTR)
    !                                CurrScreenLength=CurrScreenLength+CellScreenLength(nEIScreens,nCellList(nEIScreens))
    !                                exit LayerLoop2
    !                            else
    !
    !                                nCellList(nEIScreens)=nCellList(nEIScreens)+1  
    !                                CellNumber(nEIScreens,nCellList(nEIScreens))= iCellCurr
    !                                CellScreenLength(nEIScreens,nCellList(nEIScreens)) = CurrTopElev-Modflow.gwf.Z(Modflow.gwf.ivertex(8,iCellCurr))
    !                                write(TmpSTR,'(i5,i8,f15.3)') nEIScreens,CellNumber(nEIScreens,nCellList(nEIScreens)), CellScreenLength(nEIScreens,nCellList(nEIScreens))
    !                                call Msg(TmpSTR)
    !                                CurrScreenLength=CurrScreenLength+CellScreenLength(nEIScreens,nCellList(nEIScreens))
    !                                CurrTopElev=Modflow.gwf.Z(Modflow.gwf.ivertex(8,iCellCurr))
    !                                iCellCurr=iCellCurr+Modflow.gwf.nCell/Modflow.gwf.nLay
    !                            endif
    !                        else
    !                            iCellCurr=iCellCurr+Modflow.gwf.nCell/Modflow.gwf.nLay
    !                            if(iCellCurr > Modflow.gwf.nCell .and. ScreenFound) then
    !                                nEIScreens=nEIScreens+1
    !                            endif
    !                                
    !                        end if
    !                    end do LayerLoop2
    !
    !                     if(ScreenFound) then
    !                        FLENG(nEIScreens) = CurrScreenLength
    !                        FELEV(nEIScreens) = CurrBotElev
    !
    !                        If(FELEV(nEIScreens)< MeshBottom) then
    !                            write(TmpSTR,'(a,f15.3,a,f15.3)') 'Bottom of screen FELEV ',FELEV(nEIScreens),' below mesh bottom ',MeshBottom
    !                            call ErrMsg(TmpSTR)
    !                        endif
    !                        
    !                        call Msg('         FLENG         FELEV')
    !                        write(TmpSTR,'(2f15.3)') FLENG(nEIScreens),FELEV(nEIScreens)
    !                        call Msg(TmpSTR)
    !                    end if
    !
    !                end if
    !            end if 
    !            if(WellFound) cycle WellSearch
    !        end do
    !    end do WellSearch
    !    
    !    !-----------------------------------------------------------------------------------
    !    ! Insert wells in Modflow-USG CLN input file
    !    read(FnumTG,'(a)') FnameCLN  
    !    call OpenAscii(FnumCLN,FnameCLN)
    !    call Msg( 'Modflow-USG CLN input file: '//trim(FNameCLN))
    !    
    !    ! Open new CLN well output file
    !    FNameCLNOut='out_'//FNameCLN  
    !    call OpenAscii(FnumCLNOut,FNameCLNOut)
    !    call Msg( 'Modflow-USG CLN output file: '//trim(FNameCLNOut))
    !    
    !    ! line 1 of CLN file 
    !    read(FnumCLN,*) iCLNOrig,i1,i2,i3,i4,i5,iCellListOrig,NCONDUITYP
    !    iCLNNew=iCLNOrig+nEIScreens
    !    
    !    allocate(StartingHeads(iCLNNew))
    !    
    !    nSum=0
    !    do i=1,nEIScreens
    !        nSum=nSum+nCellList(i)
    !    end do
    !    iCellListNew=iCellListOrig+nSum
    !    write(FnumCLNOut,'(8i8)') iCLNNew,i1,i2,i3,i4,i5,iCellListNew,myNCONDUITYP
    !    
    !    read(FnumCLN,'(a)') line
    !    write(FnumCLNOut,'(a)') line
    !    
    !    do i=1,iCLNOrig
    !        read(FnumCLN,'(a)') line
    !        i2=index(line,'Well =')+6
    !        if(index(line(i2:),'IW')>0) then
    !            call ErrMsg('Original CLN well name '//trim(line(i2:))//' must not contain string "IW"') 
    !        endif 
    !        if(index(line(i2:),'EW')>0) then
    !            call ErrMsg('Original CLN well name '//trim(line(i2:))//' must not contain string "EW"') 
    !        endif 
    !
    !        write(FnumCLNOut,'(a)') line
    !    end do
    !    
    !    i1=index(line,'              IFNO')
    !    i2=index(line,'Well =')+6
    !    
    !    do i=1,nEIScreens
    !        write(FnumCLNOut,'(i8,i3,i3,f11.4,f15.4,f15.6,i10,i4,a,a)') iCLNOrig+i, IFTYP(i), IFDIR(i), FLENG(i), FELEV(i), FANGLE(i), IFLIN(i), ICCWADI(i), line(i1:i2), trim(NameEIScreen(i))
    !    end do
    !
    !    do i=1,iCellListOrig
    !        read(FnumCLN,'(a)') line
    !        write(FnumCLNOut,'(a)') line
    !    end do
    !
    !    i1=index(line,'              IFNOD')
    !    i2=index(line,'Well =')+6
    !
    !    do i=1,nEIScreens
    !        do j=1,nCellList(i)
    !            !write(FnumCLNOut,*) iCLNOrig+i, CellNumber(i,j), 0, 0.0, CellScreenLength(i,j), 1.0, 0, line(i1:i2), trim(NameEIScreen(i))
    !            write(FnumCLNOut,'(i8,i8,i3,f11.6,g19.7,f11.6,i9,a,a)') iCLNOrig+i, CellNumber(i,j), 0, 0.0, CellScreenLength(i,j), 1.0, 0, line(i1:i2), trim(NameEIScreen(i))
    !        end do
    !    end do
    !
    !    do i=1,myNCONDUITYP   ! always myNCONDUITYP lines.  Never less than 1.
    !        read(FnumCLN,'(a)') line
    !        write(FnumCLNOut,'(a)') line
    !    end do
    !    
    !    read(FnumCLN,'(a)') line  ! always an IBOUND line
    !    write(FnumCLNOut,'(a)') line
    !    
    !
    !    ! starting heads  iCLNNew=iCLNOrig+nEIScreens
    !    read(FnumCLN,'(a)') line  ! always a starting heads header line
    !    write(FnumCLNOut,'(a)') line
    !    read(FnumCLN,*) (StartingHeads(i),i=1,iCLNOrig)
    !    do i=iCLNOrig+1,iCLNNew
    !        StartingHeads(i)=StartingHeads(iCLNOrig)
    !    end do     
    !    write(FnumCLNOut,*) (StartingHeads(i),i=1,iCLNNew)
    !
    !    ! in transport case may be more data so read/write to end of file
    !    do 
    !        read(FnumCLN,'(a)',iostat=status) line
    !        if(status/=0) then
    !            exit
    !        endif
    !        write(FnumCLNOut,'(a)') line
    !    end do
    !    
    !    
    !    !-----------------------------------------------------------------------------------
    !    ! Read well file and insert placeholders for EW/IW wells
    !
    !    ! Modflow Well file
    !    read(FnumTG,'(a)') FnameWel
    !    call OpenAscii(FNumWel,FnameWel)
    !    call Msg( 'Modflow well file: '//trim(FnameWel))
    !    
    !    ! Open new FlowSource control output file
    !    FnameWelOut='place_'//FnameWel  
    !    call OpenAscii(FnumWelOut,FnameWelOut)
    !    call Msg( 'Modflow well output file: '//trim(FnameWelOut))
    !
    !    ! Always keep first line
    !    read(FNumWel,'(a)') line
    !    write(FNumWelout,'(a)') line
    !    
    !    do 
    !        read(FNumWel,'(a)',iostat=status) line
    !        if(status/=0) exit
    !
    !        read(line,*) i1,i2,iWellOrig
    !        iWellNew=iWellOrig+Modflow.n_EIWell
    !
    !        write(FNumWelout,'(3i10,a)') i1,i2,iWellNew,trim(line(31:))
    !        do i=1,IwellOrig
    !            read(FNumWel,'(a)') line
    !            write(FNumWelout,'(a)') line
    !        end do
    !        i1=index(line,'well =')+6
    !        do i=1,Modflow.n_EIWell
    !            write(FNumWelout,'(i11,g14.7,a,a)') iCLNOrig+i*2-1,0.0d0,line(26:i1),trim(Modflow.Name_EIWell(i))
    !        end do     
    !           
    !        
    !    end do
    !    continue
    !  
    !    
    !    !-----------------------------------------------------------------------------------
    !    ! Flowsource control file
    !    read(FnumTG,'(a)') FnameFSCtl
    !    call OpenAscii(FNumFSCtl,FnameFSCtl)
    !    call Msg( 'FlowSource control file: '//trim(FnameFSCtl))
    !    
    !    ! Open new FlowSource control output file
    !    FnameFSCtlOut='out_'//FnameFSCtl  
    !    call OpenAscii(FnumFSCtlOut,FnameFSCtlOut)
    !    call Msg( 'FlowSource control output file: '//trim(FnameFSCtlOut))
    !    
    !    do 
    !        read(FNumFSCtl,'(a)') line
    !        write(FNumFSCtlout,'(a)') line
    !        if(index(line,'# note that if forward tracking mode is selected') > 0) then  ! insert cell lists
    !            do i=1,nEIScreens,2
    !                if(index(NameEIScreen(i),'EW')>0) then
    !                    write(FNumFSCtlout,'(a)') ' '
    !                    write(FNumFSCtlout,'(a)') '# '//trim(NameEIScreen(i))//', '//trim(NameEIScreen(i+1))
    !                    do j=1,nCellList(i)
    !                        write(FNumFSCtlout,'(a,i8)') 'FlowDestination = ',CellNumber(i,j)
    !                    end do
    !                    do j=1,nCellList(i+1)
    !                        if(j==1) then
    !                            if(CellNumber(i+1,j) /= CellNumber(i,nCellList(i)) ) write(FNumFSCtlout,'(a,i8)') 'FlowDestination = ',CellNumber(i+1,j)
    !                        else
    !                            write(FNumFSCtlout,'(a,i8)') 'FlowDestination = ',CellNumber(i+1,j)
    !                        endif    
    !                    end do
    !                end if
    !            end do
    !            exit
    !        end if
    !    end do
    !
    !    end_ctl: do 
    !        read(FNumFSCtl,'(a)') line
    !        if(index(line,'#--------------------------') > 0) then  ! add end of ctl file
    !            write(FNumFSCtlout,'(a)') ' '
    !            write(FNumFSCtlout,'(a)') line
    !           
    !            do 
    !                read(FNumFSCtl,'(a)',iostat=status) line
    !                if(status /= 0) then
    !                    exit end_ctl
    !                endif 
    !                
    !                write(FNumFSCtlout,'(a)') line
    !               
    !            end do
    !        end if
    !    end do end_ctl
    !    
    !    
    !end subroutine MUSG_PEST_EIWellCLNFileUpdate
    !subroutine old_MUSG_PEST_EIWellCLNFileUpdate(FnumTG, Modflow) !--- Given new EI pumping wells, add screens as new CLN file entries
    !    USE IFPORT 
    !    implicit none 
    !
    !    type (MUSG_Project) Modflow
    !
    !
    !    integer :: i,j, k
    !
    !    integer :: FnumTG
    !    
    !    integer :: FnumCLN
    !    character(MAXLBL) :: FNameCLN
    !    
    !    integer :: FnumCLNout
    !    character(MAXLBL) :: FNameCLNout
    !
    !    integer :: FNumFSCtl
    !    character(MAXLBL) :: FnameFSCtl
    !    
    !    integer :: FNumFSCtlOut
    !    character(MAXLBL) :: FnameFSCtlOut
    !    
    !    integer :: iCellCurr
    !    real(dr) :: CurrTopElev
    !    real(dr) :: CurrBotElev
    !    real(dr) :: CurrScreenLength
    !    logical :: ScreenFound
    !    integer :: nEIScreens
    !    real(dr) :: MeshBottom
    !    
    !    
    !    integer, allocatable :: IFNO(:)
    !    integer, allocatable :: IFTYP(:) 
    !    integer, allocatable :: IFDIR(:) 
    !    real(dr), allocatable :: FLENG(:) 
    !    real(dr), allocatable :: FELEV(:) 
    !    real(dr), allocatable :: FANGLE(:) 
    !    integer, allocatable :: IFLIN(:)
    !    integer, allocatable :: ICCWADI(:)
    !    
    !    integer, allocatable :: nCellList(:)
	   ! character(31), allocatable :: NameEIScreen(:)
    !    integer, allocatable :: CellNumber(:,:)
    !    real(dr), allocatable :: CellScreenLength(:,:)
    !    
    !    integer :: i1, i2, i3, i4, i5
    !    integer :: ICLNOrig, ICLNNew, myNCONDUITYP
    !    integer :: ICellListOrig, ICellListNew
    !    integer :: nSum
    !    character(MAXSTRING) :: line
    !    
    !    logical :: WellFound
    !
    !    
    !    allocate(IFNO(2*Modflow.n_EIWell), &
    !    &   IFTYP(2*Modflow.n_EIWell), &
    !    &   IFDIR(2*Modflow.n_EIWell), &
    !    &   FLENG(2*Modflow.n_EIWell), &
    !    &   FELEV(2*Modflow.n_EIWell), &
    !    &   FANGLE(2*Modflow.n_EIWell), &
    !    &   IFLIN(2*Modflow.n_EIWell), &
    !    &   ICCWADI(2*Modflow.n_EIWell))
    !    
    !    IFNO(:) = 0
    !    IFTYP(:) = 1
    !    IFDIR(:) = 0
    !    FLENG(:) = 0.0d0
    !    FELEV(:) = 0.0d0
    !    FANGLE(:) = 0.0d0
    !    IFLIN(:) = 1
    !    ICCWADI(:) = 0
    !    
    !    allocate(nCellList(2*Modflow.n_EIWell), &
    !    &   NameEIScreen(2*Modflow.n_EIWell), & 
    !    &   CellNumber(2*Modflow.n_EIWell,100),& 
    !    &   CellScreenLength(2*Modflow.n_EIWell,100))
    !    
    !    nEIScreens=0
    !    
    !    WellSearch:do i=1, Modflow.n_EIWell
    !        call Msg('------------------------------------------------------------------------------')
    !        call Msg('EI Well: '//trim(Modflow.Name_EIWell(i)))
    !        write(TmpSTR,'(a, 2f15.3)') 'X Y: ',Modflow.X_EIWell(i), Modflow.Y_EIWell(i)
    !        call Msg(TmpSTR)
    !        CurrBotElev=Modflow.TopElev_EIWell(i)- Modflow.ScreenALength_EIWell(i)
    !        call Msg('       Elevation       Length      Comment')
    !        write(TmpSTR,'(f15.3,a)') Modflow.TopElev_EIWell(i), '           -        Screen A top elevation '
    !        call Msg(TmpSTR)
    !        write(TmpSTR,'(2f15.3,5x,a)') CurrBotElev, Modflow.ScreenALength_EIWell(i), 'Screen A bottom elevation, screen A length'
    !        call Msg(TmpSTR)
    !        CurrBotElev=CurrBotElev- Modflow.ScreenBOffset_EIWell(i)
    !        write(TmpSTR,'(2f15.3,5x,a)') CurrBotElev, Modflow.ScreenBOffset_EIWell(i), 'Screen B top elevation, screen B offset'
    !        call Msg(TmpSTR)
    !        CurrBotElev=CurrBotElev- Modflow.ScreenBLength_EIWell(i)
    !        write(TmpSTR,'(2f15.3,5x,a)') CurrBotElev, Modflow.ScreenBLength_EIWell(i), 'Screen B bottom elevation, screen B length'
    !        call Msg(TmpSTR)
    !        
    !        WellFound=.false.
    !             
    !        do j=1,Modflow.gwf.nCell/Modflow.gwf.nLay  ! loop over the cells in layer 1
    !            if(Modflow.X_EIWell(i) >= Modflow.gwf.X(Modflow.gwf.ivertex(1,j)) .and. Modflow.X_EIWell(i) <= Modflow.gwf.X(Modflow.gwf.ivertex(4,j))) then
    !                if(Modflow.Y_EIWell(i) >= Modflow.gwf.Y(Modflow.gwf.ivertex(1,j)) .and. Modflow.Y_EIWell(i) <= Modflow.gwf.Y(Modflow.gwf.ivertex(2,j))) then
    !                    iCellCurr=j
    !                    WellFound=.true.
    !
    !                    write(TmpSTR,'(a,i8,a,2f15.3)') 'Found in cell ', iCellCurr,' at cell centroid X Y ', Modflow.gwf.xCell(iCellCurr), Modflow.gwf.yCell(iCellCurr)
    !                    call Msg(TmpSTR)
    !
    !                    write(TmpSTR,'(a,2f15.3)') 'X range ', Modflow.gwf.X(Modflow.gwf.ivertex(1,j)), Modflow.gwf.X(Modflow.gwf.ivertex(4,j))
    !                    call Msg(TmpSTR)
    !                    write(TmpSTR,'(a,2f15.3)') 'Y range ', Modflow.gwf.Y(Modflow.gwf.ivertex(1,j)), Modflow.gwf.Y(Modflow.gwf.ivertex(2,j))
    !                    call Msg(TmpSTR)
    !
    !                    
    !                    
    !                    call Msg(' Layer  Cell     Vertex         Z      Height')
    !                    write(TmpSTR,'(i5,i8,i8,f15.3)') 0, iCellCurr, 4,Modflow.gwf.Z(Modflow.gwf.ivertex(4,iCellCurr))
    !                    call Msg(TmpSTR)
    !                    CurrTopElev=Modflow.gwf.Z(Modflow.gwf.ivertex(4,iCellCurr))
    !                    do k=1,Modflow.gwf.nLay                             
    !                        write(TmpSTR,'(i5,i8,i8,5f15.3)') k, iCellCurr,8, Modflow.gwf.Z(Modflow.gwf.ivertex(8,iCellCurr)),CurrTopElev-Modflow.gwf.Z(Modflow.gwf.ivertex(8,iCellCurr))
    !                        call Msg(TmpSTR)
    !                        if(k==Modflow.gwf.nLay) then
    !                            MeshBottom= Modflow.gwf.Z(Modflow.gwf.ivertex(8,iCellCurr))
    !                        endif
    !                        CurrTopElev=Modflow.gwf.Z(Modflow.gwf.ivertex(8,iCellCurr))
    !                        iCellCurr=iCellCurr+Modflow.gwf.nCell/Modflow.gwf.nLay
    !                    end do
    !                    
    !                    iCellCurr=j
    !
    !                    
    !                    ! Top screen A
    !                    CurrTopElev=Modflow.TopElev_EIWell(i)
    !                    CurrBotElev=CurrTopElev-Modflow.ScreenALength_EIWell(i)
    !                    CurrScreenLength=0.0d0
    !                    ScreenFound=.false.
    !                    LayerLoop1: do k=1,Modflow.gwf.nLay 
    !                        if(CurrTopElev > Modflow.gwf.Z(Modflow.gwf.ivertex(8,iCellCurr)))  then  ! if current screen top > current cell bottom
    !                            if(.not. ScreenFound) then
    !                                nEIScreens=nEIScreens+1
    !                                ScreenFound=.true.
    !                                call Msg(' +Scr A    Cell      ScreenLength')
    !                                nCellList(nEIScreens)=0
    !                                NameEIScreen(nEIScreens)=trim(Modflow.Name_EIWell(i))//'_A'
    !                            end if
    !                            if(CurrBotElev > Modflow.gwf.Z(Modflow.gwf.ivertex(8,iCellCurr))) then ! if current screen bot > current cell bottom
    !                                nCellList(nEIScreens)=nCellList(nEIScreens)+1  
    !                                CellNumber(nEIScreens,nCellList(nEIScreens))= iCellCurr
    !                                CellScreenLength(nEIScreens,nCellList(nEIScreens)) = CurrTopElev-CurrBotElev
    !                                write(TmpSTR,'(i5,i8,f15.3)') nEIScreens,CellNumber(nEIScreens,nCellList(nEIScreens)), CellScreenLength(nEIScreens,nCellList(nEIScreens))
    !                                call Msg(TmpSTR)
    !                                CurrScreenLength=CurrScreenLength+CellScreenLength(nEIScreens,nCellList(nEIScreens))
    !                                exit LayerLoop1
    !                            else
    !
    !                                nCellList(nEIScreens)=nCellList(nEIScreens)+1  
    !                                CellNumber(nEIScreens,nCellList(nEIScreens))= iCellCurr
    !                                CellScreenLength(nEIScreens,nCellList(nEIScreens)) = CurrTopElev-Modflow.gwf.Z(Modflow.gwf.ivertex(8,iCellCurr))
    !                                write(TmpSTR,'(i5,i8,f15.3)') nEIScreens,CellNumber(nEIScreens,nCellList(nEIScreens)), CellScreenLength(nEIScreens,nCellList(nEIScreens))
    !                                call Msg(TmpSTR)
    !                                CurrScreenLength=CurrScreenLength+CellScreenLength(nEIScreens,nCellList(nEIScreens))
    !                                
    !                                CurrTopElev=Modflow.gwf.Z(Modflow.gwf.ivertex(8,iCellCurr))
    !                                iCellCurr=iCellCurr+Modflow.gwf.nCell/Modflow.gwf.nLay
    !                            endif
    !                        else
    !                            iCellCurr=iCellCurr+Modflow.gwf.nCell/Modflow.gwf.nLay
    !                            if(iCellCurr > Modflow.gwf.nCell .and. ScreenFound) then
    !                                nEIScreens=nEIScreens+1
    !                            endif
    !                                
    !                        end if
    !                    end do LayerLoop1
    !                    
    !                    if(ScreenFound) then
    !                        FLENG(nEIScreens) = CurrScreenLength
    !                        FELEV(nEIScreens) = CurrBotElev
    !                        
    !                        If(FELEV(nEIScreens)< MeshBottom) then
    !                            write(TmpSTR,'(a,f15.3,a,f15.3)') 'Bottom of screen FELEV ',FELEV(nEIScreens),' below mesh bottom ',MeshBottom
    !                            call ErrMsg(TmpSTR)
    !                        endif
    !                        
    !                        call Msg('         FLENG         FELEV')
    !                        write(TmpSTR,'(2f15.3)') FLENG(nEIScreens),FELEV(nEIScreens)
    !                        call Msg(TmpSTR)
    !                    end if
    !                    
    !                    
    !                    ! Screen B
    !                    CurrTopElev=CurrBotElev-Modflow.ScreenBOffset_EIWell(i)
    !                    CurrBotElev=CurrTopElev-Modflow.ScreenBLength_EIWell(i)
    !                    CurrScreenLength=0.0d0
    !                    ScreenFound=.false.
    !                    iCellCurr=j
    !                    LayerLoop2: do k=1,Modflow.gwf.nLay 
    !                        if(CurrTopElev > Modflow.gwf.Z(Modflow.gwf.ivertex(8,iCellCurr)))  then  ! if current screen top > current cell bottom
    !                            if(.not. ScreenFound) then
    !                                nEIScreens=nEIScreens+1
    !                                ScreenFound=.true.
    !                                call Msg(' +Scr B    Cell      ScreenLength')
    !                                nCellList(nEIScreens)=0
    !                                NameEIScreen(nEIScreens)=trim(Modflow.Name_EIWell(i))//'_B'
    !                            endif
    !                            if(CurrBotElev > Modflow.gwf.Z(Modflow.gwf.ivertex(8,iCellCurr))) then ! if current screen bot > current cell bottom
    !
    !                                nCellList(nEIScreens)=nCellList(nEIScreens)+1  
    !                                CellNumber(nEIScreens,nCellList(nEIScreens))= iCellCurr
    !                                CellScreenLength(nEIScreens,nCellList(nEIScreens)) = CurrTopElev-CurrBotElev
    !                                write(TmpSTR,'(i5,i8,f15.3)') nEIScreens,CellNumber(nEIScreens,nCellList(nEIScreens)), CellScreenLength(nEIScreens,nCellList(nEIScreens))
    !                                call Msg(TmpSTR)
    !                                CurrScreenLength=CurrScreenLength+CellScreenLength(nEIScreens,nCellList(nEIScreens))
    !                                exit LayerLoop2
    !                            else
    !
    !                                nCellList(nEIScreens)=nCellList(nEIScreens)+1  
    !                                CellNumber(nEIScreens,nCellList(nEIScreens))= iCellCurr
    !                                CellScreenLength(nEIScreens,nCellList(nEIScreens)) = CurrTopElev-Modflow.gwf.Z(Modflow.gwf.ivertex(8,iCellCurr))
    !                                write(TmpSTR,'(i5,i8,f15.3)') nEIScreens,CellNumber(nEIScreens,nCellList(nEIScreens)), CellScreenLength(nEIScreens,nCellList(nEIScreens))
    !                                call Msg(TmpSTR)
    !                                CurrScreenLength=CurrScreenLength+CellScreenLength(nEIScreens,nCellList(nEIScreens))
    !                                CurrTopElev=Modflow.gwf.Z(Modflow.gwf.ivertex(8,iCellCurr))
    !                                iCellCurr=iCellCurr+Modflow.gwf.nCell/Modflow.gwf.nLay
    !                            endif
    !                        else
    !                            iCellCurr=iCellCurr+Modflow.gwf.nCell/Modflow.gwf.nLay
    !                            if(iCellCurr > Modflow.gwf.nCell .and. ScreenFound) then
    !                                nEIScreens=nEIScreens+1
    !                            endif
    !                                
    !                        end if
    !                    end do LayerLoop2
    !
    !                     if(ScreenFound) then
    !                        FLENG(nEIScreens) = CurrScreenLength
    !                        FELEV(nEIScreens) = CurrBotElev
    !
    !                        If(FELEV(nEIScreens)< MeshBottom) then
    !                            write(TmpSTR,'(a,f15.3,a,f15.3)') 'Bottom of screen FELEV ',FELEV(nEIScreens),' below mesh bottom ',MeshBottom
    !                            call ErrMsg(TmpSTR)
    !                        endif
    !                        
    !                        call Msg('         FLENG         FELEV')
    !                        write(TmpSTR,'(2f15.3)') FLENG(nEIScreens),FELEV(nEIScreens)
    !                        call Msg(TmpSTR)
    !                    end if
    !
    !                end if
    !            end if 
    !            if(WellFound) cycle WellSearch
    !        end do
    !    end do WellSearch
    !    
    !     ! Modflow-USG CLN input file
    !    read(FnumTG,'(a)') FnameCLN  
    !    call OpenAscii(FnumCLN,FnameCLN)
    !    call Msg( 'Modflow-USG CLN input file: '//trim(FNameCLN))
    !    
    !    ! Open new CLN well output file
    !    FNameCLNOut='out_'//FNameCLN  
    !    call OpenAscii(FnumCLNOut,FNameCLNOut)
    !    call Msg( 'Modflow-USG CLN output file: '//trim(FNameCLNOut))
    !    
    !    ! line 1 of CLN file 
    !    read(FnumCLN,*) iCLNOrig,i1,i2,i3,i4,i5,iCellListOrig,myNCONDUITYP
    !    iCLNNew=iCLNOrig+nEIScreens
    !    nSum=0
    !    do i=1,nEIScreens
    !        nSum=nSum+nCellList(i)
    !    end do
    !    iCellListNew=iCellListOrig+nSum
    !    write(FnumCLNOut,'(8i8)') iCLNNew,i1,i2,i3,i4,i5,iCellListNew,myNCONDUITYP
    !    
    !    read(FnumCLN,'(a)') line
    !    write(FnumCLNOut,'(a)') line
    !    
    !    do i=1,iCLNOrig
    !        read(FnumCLN,'(a)') line
    !        i2=index(line,'Well =')+6
    !        if(index(line(i2:),'IW')>0) then
    !            call ErrMsg('Original CLN well name '//trim(line(i2:))//' must not contain string "IW"') 
    !        endif 
    !        if(index(line(i2:),'EW')>0) then
    !            call ErrMsg('Original CLN well name '//trim(line(i2:))//' must not contain string "EW"') 
    !        endif 
    !
    !        write(FnumCLNOut,'(a)') line
    !    end do
    !    
    !    i1=index(line,'              IFNO')
    !    i2=index(line,'Well =')+6
    !    
    !    do i=1,nEIScreens
    !        write(FnumCLNOut,'(i8,i3,i3,f11.4,f15.4,f15.6,i10,i4,a,a)') iCLNOrig+i, IFTYP(i), IFDIR(i), FLENG(i), FELEV(i), FANGLE(i), IFLIN(i), ICCWADI(i), line(i1:i2), trim(NameEIScreen(i))
    !    end do
    !
    !    do i=1,iCellListOrig
    !        read(FnumCLN,'(a)') line
    !        write(FnumCLNOut,'(a)') line
    !    end do
    !
    !    i1=index(line,'              IFNOD')
    !    i2=index(line,'Well =')+6
    !
    !    do i=1,nEIScreens
    !        do j=1,nCellList(i)
    !            !write(FnumCLNOut,*) iCLNOrig+i, CellNumber(i,j), 0, 0.0, CellScreenLength(i,j), 1.0, 0, line(i1:i2), trim(NameEIScreen(i))
    !            write(FnumCLNOut,'(i8,i8,i3,f11.6,g19.7,f11.6,i9,a,a)') iCLNOrig+i, CellNumber(i,j), 0, 0.0, CellScreenLength(i,j), 1.0, 0, line(i1:i2), trim(NameEIScreen(i))
    !        end do
    !    end do
    !
    !    do i=1,iCLNOrig+myNCONDUITYP+2   ! myNCONDUITYP+2 lines then all starting heads
    !        read(FnumCLN,'(a)') line
    !        write(FnumCLNOut,'(a)') line
    !    end do
    !
    !    do i=1,nEIScreens  ! repeat last starting head for each new screen
    !        write(FnumCLNOut,'(a)') line
    !    end do
    !    
    !    ! in transport case may be more data so read/write to end of file
    !    do 
    !        read(FnumCLN,'(a)',iostat=status) line
    !        if(status/=0) then
    !            exit
    !        endif
    !        write(FnumCLNOut,'(a)') line
    !    end do
    !    
    !     ! Flowsource control file
    !    read(FnumTG,'(a)') FnameFSCtl
    !    call OpenAscii(FNumFSCtl,FnameFSCtl)
    !    call Msg( 'FlowSource control file: '//trim(FnameFSCtl))
    !    
    !    ! Open new FlowSource control output file
    !    FnameFSCtlOut='out_'//FnameFSCtl  
    !    call OpenAscii(FnumFSCtlOut,FnameFSCtlOut)
    !    call Msg( 'FlowSource control output file: '//trim(FnameFSCtlOut))
    !    
    !    do 
    !        read(FNumFSCtl,'(a)') line
    !        write(FNumFSCtlout,'(a)') line
    !        if(index(line,'# note that if forward tracking mode is selected') > 0) then  ! insert cell lists
    !            do i=1,nEIScreens,2
    !                if(index(NameEIScreen(i),'EW')>0) then
    !                    write(FNumFSCtlout,'(a)') ' '
    !                    write(FNumFSCtlout,'(a)') '# '//trim(NameEIScreen(i))//', '//trim(NameEIScreen(i+1))
    !                    do j=1,nCellList(i)
    !                        write(FNumFSCtlout,'(a,i8)') 'FlowDestination = ',CellNumber(i,j)
    !                    end do
    !                    do j=1,nCellList(i+1)
    !                        if(j==1) then
    !                            if(CellNumber(i+1,j) /= CellNumber(i,nCellList(i)) ) write(FNumFSCtlout,'(a,i8)') 'FlowDestination = ',CellNumber(i+1,j)
    !                        else
    !                            write(FNumFSCtlout,'(a,i8)') 'FlowDestination = ',CellNumber(i+1,j)
    !                        endif    
    !                    end do
    !                end if
    !            end do
    !            exit
    !        end if
    !    end do
    !
    !    end_ctl: do 
    !        read(FNumFSCtl,'(a)') line
    !        if(index(line,'#--------------------------') > 0) then  ! add end of ctl file
    !            write(FNumFSCtlout,'(a)') ' '
    !            write(FNumFSCtlout,'(a)') line
    !           
    !            do 
    !                read(FNumFSCtl,'(a)',iostat=status) line
    !                if(status /= 0) then
    !                    exit end_ctl
    !                endif 
    !                
    !                write(FNumFSCtlout,'(a)') line
    !               
    !            end do
    !        end if
    !    end do end_ctl
    !    
    !end subroutine old_MUSG_PEST_EIWellCLNFileUpdate
    !
    !subroutine MUSG_PEST_CountParticlesToWells(FnumTG) !--- Given modpath output file and cln file info, count particles to each well and bin to arrival time.
    !    USE IFPORT 
    !    implicit none 
    !
    !    integer :: i, j, k, l
    !
    !    integer :: FnumTG
    !    
    !    integer :: FnumParticlesToWells
    !    character(MAXLBL) :: FNameParticlesToWells
    !
    !    integer :: FnumCLN
    !    character(MAXLBL) :: FNameCLN
    !    
    !    integer :: FnumEndpoint7
    !    character(MAXLBL) :: FNameEndpoint7
    !    
    !    integer :: FnumEndpoint7_out
    !    character(MAXLBL) :: FNameEndpoint7_out
    !    
    !    character(20), allocatable :: cln_WellName(:)
    !    integer, allocatable :: cln_nCells(:)
    !    integer, allocatable :: cln_CellList(:,:) ! up to 100 cells per cln
    !    
    !    integer, allocatable :: cln_nParticles(:,:)  ! number of particles reporting to this CLN(well),Bin(time)
    !    
    !
    !    character(MAXLBL) :: line
    !    integer :: nCln
    !    integer :: nTotCellList
    !    integer :: ifnod
    !    integer :: igwnod
    !    integer :: i1, i2, i3, i4, i5, i6
    !    real(dr) :: f1, f3, f4, f5, f6, f7, f8
    !    
    !    
    !    integer :: TrackingDirection
    !    integer :: TotalParticleCount
    !    integer :: iExit
    !    integer :: ICellStart
    !    integer :: ICellEnd
    !    real(dr) :: FinalTime
    !    
    !    integer :: nBins
    !    real(dr), allocatable :: BinStart(:)
    !    real(dr), allocatable :: BinEnd(:)
    !    
    !
    !   ! Paticles to wells information file
    !    read(FNumTG,'(a)') FNameParticlesToWells
    !    call OpenAscii(FnumParticlesToWells,FNameParticlesToWells)
    !    call Msg( 'ParticlesToWells file: '//trim(FNameParticlesToWells))
    !    
    !    
    !    read(FnumParticlesToWells,'(a)') FNameCLN
    !    call OpenAscii(FnumCLN,FNameCLN)
    !    call Msg( 'CLN file: '//trim(FNameCLN))
    !    
    !
    !    read(FNumCLN,*) nCLN, i1, i2, i3, i4, i5, nTotCellList
    !    allocate(cln_WellName(nCLN), cln_nCells(nCLN), cln_CellList(NCln,100))
    !    cln_nCells(:)=0
    !    cln_CellList(:,:)=0
    !    
    !    
    !    read(FNumCLN,'(a)') line   ! throw away line 2
    !    ! read cln well names
    !    do i=1,nCLN  
    !        read(FNumCLN,'(a)') line 
    !        l1=index(line,'Well = ')
    !        if(l1>0) then
    !            read(line(l1+7:),'(a)') cln_WellName(i)
    !        endif
    !    end do
    !    
    !    do i=1,nTotCellList
    !        read(FNumCLN,*) ifnod, igwnod
    !        cln_nCells(ifnod)=cln_nCells(ifnod)+1
    !        cln_CellList(ifnod,cln_nCells(ifnod))=igwnod
    !    end do
    !    
    !    call FreeUnit(FNumCLN)
    !    
    !    read(FnumParticlesToWells,*) nBins
    !    write(TMPStr,'(i8)') nBins
    !    call Msg('Number of bins: '//TMPStr)
    !    if(nBins>30) call ErrMsg('Current number of bins must be less than 30: '//TMPStr)
    !    
    !    
    !    allocate(BinStart(nBins), BinEnd(nBins))
    !    do i=1,nBins
    !        read(FnumParticlesToWells,*) BinStart(i),BinEnd(i)
    !        write(TMPStr,'(4g15.7)') BinStart(i),BinEnd(i)
    !        call Msg('Bin start, Bin end time: '//TMPStr)
    !
    !        if(BinStart(i)>=BinEnd(i)) then
    !            write(TMPStr,'(4f15.3)') BinStart(i),BinEnd(i)
    !            call ErrMsg('Bin start time >= Bin end time: '//TMPStr)
    !        endif
    !
    !        if(i>1) then
    !            do j=1,i-1
    !                if(BinStart(i) < BinEnd(j)) then
    !                    write(TMPStr,'(4f15.3)') BinStart(i), BinEnd(j)
    !                    call ErrMsg('Bin start time < Last bin end time: '//TMPStr)
    !                endif
    !            end do
    !        end if
    !    end do
    !
    !    allocate(cln_nParticles(nCLN,nBins))
    !    cln_nParticles(:,:)=0
    !
    !    do 
    !        ! CLN file
    !        read(FnumParticlesToWells,'(a)',iostat=status) FNameEndpoint7
    !        if(status /= 0) then
    !            exit
    !        endif
    !        
    !        call OpenAscii(FnumEndpoint7,FNameEndpoint7)
    !        call Msg( 'Endpoint7 file: '//trim(FNameEndpoint7))
    !        
    !        read(FnumEndpoint7,'(a)') line   ! check first line
    !        if(index(line,'MODPATH_ENDPOINT_FILE') == 0) then
    !            call Msg( 'Not a valid Modpath7 endpoint file')
    !            stop
    !        endif
    !
    !        read(FnumEndpoint7,*) TrackingDirection, TotalParticleCount
    !        read(FnumEndpoint7,'(a)') line   
    !        read(FnumEndpoint7,'(a)') line   
    !        read(FnumEndpoint7,'(a)') line   
    !        read(FnumEndpoint7,'(a)') line   
    !        Particle: do i=1, TotalParticleCount
    !            read(FnumEndpoint7,*) i1, i2, i3, iExit, f1, FinalTime, iCellStart, i4, f3, f4, f5, f6, f7, f8, i5, i6, iCellEnd
    !            do k=1,nCLN
    !                do j=1,cln_nCells(k)
    !                    if(iCellEnd==cln_CellList(k,j) .and. iExit==5) then
    !                        do l=1,nBins
    !                            if(FinalTime >= BinStart(l) .and. FinalTime < BinEnd(l)) then  
    !                                cln_nParticles(k,l)=cln_nParticles(k,l)+1
    !                                cycle Particle
    !                            endif
    !                        end do
    !                        write(TMPStr,'(4f15.3)') FinalTime
    !                        call ErrMsg('FinalTime fall outside of bin range: '//TMPStr)
    !                    endif
    !                end do
    !            end do
    !        end do Particle
    !        
    !        ! Write particle count file
    !        FNameEndpoint7_out=trim(FNameEndpoint7)//'_PToWell.csv'
    !        call OpenAscii(FnumEndpoint7_out,FNameEndpoint7_out)
    !        call Msg( 'Endpoint7 particle count file: '//trim(FNameEndpoint7_out))
    !        write(FnumEndpoint7_out,'(a,30(i8,a))') 'WellName,#ParticlesReportingToBin',(i,',',i=1,nBins)
    !        do i=1,nCLN
    !            write(FnumEndpoint7_out,'(a20,a,30(i8,a))') cln_WellName(i), ',',(cln_nParticles(i,j),',',j=1,nBins)
    !        end do
    !
    !        call FreeUnit(FnumEndpoint7)
    !        call FreeUnit(FnumEndpoint7_out)
    !         
    !    end do
    !    
    !end subroutine MUSG_PEST_CountParticlesToWells
    !
    !subroutine M2005_PEST_CountParticlesToWells(FnumTG) !--- Given modpath5 output file and ??? file info, count particles to each well and bin to arrival time.
    !    USE IFPORT 
    !    implicit none 
    !
    !    integer :: i, j, k
    !
    !    integer :: FnumTG
    !    
    !    integer :: FnumParticlesToWells
    !    character(MAXLBL) :: FNameParticlesToWells
    !
    !    integer :: FnumMNW
    !    character(MAXLBL) :: FNameMNW
    !    
    !    integer :: FNumEndpoint5
    !    character(MAXLBL) :: FNameEndpoint5
    !    
    !    integer :: FNumEndpoint5_out
    !    character(MAXLBL) :: FNameEndpoint5_out
    !    
    !    integer :: nLines
    !    
    !    character(20), allocatable :: WellName(:)
    !    integer, allocatable :: nIJKs(:)
    !    integer, allocatable :: IJKList(:,:,:) ! up to 100 IJKs per mnw
    !    integer :: iRow, jCol, kLyr
    !    
    !    integer, allocatable :: nParticles(:)  ! number of particles reporting to this MNW(well),Bin(time)
    !    
    !
    !    character(MAXLBL) :: line
    !    integer :: nWells
    !    
    !    integer :: i1
    !    
    !   ! Paticles to wells information file
    !    read(FNumTG,'(a)') FNameParticlesToWells
    !    call OpenAscii(FnumParticlesToWells,FNameParticlesToWells)
    !    call Msg( 'ParticlesToWells file: '//trim(FNameParticlesToWells))
    !    
    !    read(FnumParticlesToWells,'(a)') FNameMNW
    !    call OpenAscii(FnumMNW,FNameMNW)
    !    call Msg( 'MNW file: '//trim(FNameMNW))
    !    
    !    read(FNumMNW,'(a)') line   ! throw away line 1
    !    read(FNumMNW,'(a)') line   ! throw away line 2
    !    read(FNumMNW,*) nLines
    !    ! count wells
    !    nWells=0
    !    do i=1,nLines
    !        read(FNumMNW,'(a)') line
    !        if(index(line,'SITE:') > 1) nWells=nWells+1
    !    end do
    !
    !    allocate(WellName(nWells), nIJKs(nWells), IJKList(nWells,3,100))
    !    nIJKs(:)=0
    !    IJKList(:,:,:)=0
    !
    !    continue
    !    
    !    rewind(FNumMNW) 
    !    read(FNumMNW,'(a)') line   ! throw away line 1
    !    read(FNumMNW,'(a)') line   ! throw away line 2
    !    read(FNumMNW,'(a)') line   ! throw away line 2
    !    ! read well name and ijk's
    !    nWells=0
    !    do i=1,nLines
    !        read(FNumMNW,'(a)') line
    !        if(index(line,'SITE:') > 1) then
    !            nWells=nWells+1
    !            nIJKs(nWells)=nIJKs(nWells)+1
    !            read(line,*) kLyr, iRow, jCol
    !            IJKList(nWells,1,nIJKs(nWells))=iRow
    !            IJKList(nWells,2,nIJKs(nWells))=jCol
    !            IJKList(nWells,3,nIJKs(nWells))=kLyr
    !            l1=index(line,'SITE:')
    !            WellName(nWells)=line(l1+5:)
    !        else
    !            nIJKs(nWells)=nIJKs(nWells)+1
    !            read(line,*) kLyr, iRow, jCol
    !            IJKList(nWells,1,nIJKs(nWells))=iRow
    !            IJKList(nWells,2,nIJKs(nWells))=jCol
    !            IJKList(nWells,3,nIJKs(nWells))=kLyr
    !        end if
    !    end do
    !    call FreeUnit(FNumMNW)
    !
    !    allocate(nParticles(nWells))
    !
    !    do 
    !        ! Particles file
    !        read(FnumParticlesToWells,'(a)',iostat=status) FNameEndpoint5
    !        if(status /= 0) then
    !            exit
    !        endif
    !        
    !        call OpenAscii(FNumEndpoint5,FNameEndpoint5)
    !        call Msg( 'Endpoint5 file: '//trim(FNameEndpoint5))
    !        
    !        read(FNumEndpoint5,'(a)') line   ! check first line
    !        if(index(line,'@ [ MODPATH 5.0') == 0) then
    !            call Msg( 'Not a Modpath 5 endpoint file')
    !            stop
    !        endif
    !        
    !        nParticles(:)=0
    !        Particle: do 
    !            read(FNumEndpoint5,*,iostat=status) i1, jCol, iRow, kLyr
    !            if(status/=0) exit Particle
    !            
    !            do k=1,nWells
    !                do j=1,nIJKs(k)
    !                    if(IJKList(k,1,j)==iRow .and. IJKList(k,2,j)==jCol .and. IJKList(k,3,j)==kLyr) then
    !                        nParticles(k)=nParticles(k)+1
    !                        cycle Particle
    !                    endif
    !                end do
    !            end do
    !        end do Particle
    !        
    !        ! Write particle count file
    !        FNameEndpoint5_out=trim(FNameEndpoint5)//'_PToWell.csv'
    !        call OpenAscii(FNumEndpoint5_out,FNameEndpoint5_out)
    !        call Msg( 'Endpoint5 particle count file: '//trim(FNameEndpoint5_out))
    !        write(FNumEndpoint5_out,'(a,30(i8,a))') 'WellName,#ParticlesReporting'
    !        do i=1,nWells
    !            write(FNumEndpoint5_out,'(a20,a,30(i8,a))') WellName(i), ',',nParticles(i)
    !        end do
    !
    !        call FreeUnit(FNumEndpoint5)
    !        call FreeUnit(FNumEndpoint5_out)
    !         
    !    end do
    !    
    !end subroutine M2005_PEST_CountParticlesToWells
    !
    !subroutine MUSG_PEST_RTWellOptimization(FnumTG) !--- Given RT pumping well info, update well .WEL file
    !    !USE IFPORT 
    !    implicit none 
    !
    !    !type (MUSG_Project) Modflow
    !
    !
    !    integer :: i, j
    !
    !    integer :: FnumTG
    !    
    !    integer :: FnumRTRates
    !    character(MAXLBL) :: FnameRTRates
    !    
    !    integer :: FnumRTOnOff
    !    character(MAXLBL) :: FNameRTOnOff
    !    integer :: FnumRTOnOffNints
    !    character(MAXLBL) :: FNameRTOnOffNints
    !
    !    integer :: FnumRTRateOut
    !    character(MAXLBL) :: FnameRTRateOut
    !
    !    integer :: FNumWel
    !    character(MAXLBL) :: FnameWel
    !    integer :: FNumWelOut
    !    character(MAXLBL) :: FnameWelOut
    !    
    ! !   integer, allocatable :: nCellList(:)
	   ! !character(31), allocatable :: NameEIScreen(:)
    ! !   integer, allocatable :: CellNumber(:,:)
    ! !   real(dr), allocatable :: CellScreenLength(:,:)
    ! !   
    !    
    !    integer :: i1, i2
    !    character(MAXSTRING) :: line
    !
    !    character(31) :: WellName(MAXCLN)
    !    integer :: RTWell_CLN_Number
    !    real(dr) :: HighRate_ft3_day(MAXCLN)
    !    real(dr) :: LowRate_ft3_day(MAXCLN)
    !    
    !    integer :: nRTOnOff
    !    integer, allocatable :: cln_number_On_Off(:)
    !    integer, allocatable :: SP_num_On_Off(:)
    !    real(dr), allocatable :: On_off(:)
    !    real(dr),allocatable :: AppliedRate_ft3_day(:)
    !    real(dr) :: State
    !    integer, allocatable :: indx_col(:)
    !    
    !    integer :: iStressperiod
    !    integer :: nStressperiods
    !    integer :: IWellOrig
    !    integer :: IWellNew(MAXSTRESS)
    !    integer :: idum1, idum2
    !    real(dr) :: RTRateTotal(MAXSTRESS)
    !    
    !    integer :: iOnOff
    !    integer :: iCLN
    !    
    !    character(MAXSTRING) :: CMDString
    !
    !    
    !    integer :: FnumLADWP
    !    character(MAXLBL) :: FnameLADWP
    !    real(dr) :: LADWPRateTotal(MAXSTRESS)
    !    real(dr) :: LADWPRatio
    !    real(dr) :: LADWPPenalty
    !
    !    
    !    integer :: FnumPenalty
    !    character(MAXLBL) :: FNamePenalty
    !    integer :: nRange
    !    real(dr), allocatable :: MinPenalty(:)
    !    real(dr), allocatable :: MaxPenalty(:)
    !    real(dr), allocatable :: MinRange(:)
    !    real(dr), allocatable :: MaxRange(:)
    !
    !    !-----------------------------------------------------------------------------------
    !    ! RT wells rate file
    !    read(FnumTG,'(a)') FnameRTRates  
    !    call OpenAscii(FnumRTRates,FnameRTRates)
    !    call Msg( 'RT well rate input file: '//trim(FnameRTRates))
    !    
    !    ! Read header
    !    read(FnumRTRates,'(a)') line
    !    do 
    !        read(FnumRTRates,'(a)',iostat=status) line
    !        if(status/=0) exit
    !        
    !        l1=index(line,',')
    !        read(line(:l1-1),'(a)') TMPStr
    !        
    !        line=line(l1+1:)
    !        l1=index(line,',')
    !        read(line(:l1),*) RTWell_CLN_Number
    !        
    !        WellName(RTWell_CLN_Number)=TMPStr
    !        
    !        line=line(l1+1:)
    !        read(line,*) HighRate_ft3_day(RTWell_CLN_Number)
    !
    !        l1=index(line,',')
    !        line=line(l1+1:)
    !        read(line,*) LowRate_ft3_day(RTWell_CLN_Number)
    !
    !    end do
    !
    !    continue
    !
    !    !-----------------------------------------------------------------------------------
    !    ! RT wells on/off file
    !    read(FnumTG,'(a)') FNameRTOnOff  
    !    call OpenAscii(FNumRTOnOff,FNameRTOnOff)
    !    call Msg( 'RT well on/off input file: '//trim(FNameRTOnOff))
    !    
    !    FNameRTOnOffNints='AppliedRates.txt'
    !    call OpenAscii(FNumRTOnOffNints,FNameRTOnOffNints)
    !    call Msg( 'RT well on/off input file: '//trim(FNameRTOnOffNints))
    !
    !    ! Read header
    !    read(FNumRTOnOff,'(a)') line
    !    write(FNumRTOnOffNints,'(a)') '   cln,Stress period,    On_off, State,  Applied rate (ft3/day)'
    !
    !    nRTOnOff=0
    !    do 
    !        read(FNumRTOnOff,'(a)',iostat=status) line
    !        if(status/=0) exit
    !        
    !        nRTOnOff=nRTOnOff+1
    !        
    !    end do
    !    
    !    allocate(cln_number_On_Off(nRTOnOff), SP_num_On_Off(nRTOnOff), On_off(nRTOnOff), indx_col(nRTOnOff), AppliedRate_ft3_day(nRTOnOff))
    !
    !    rewind(FNumRTOnOff)
    !    ! Read header
    !    read(FNumRTOnOff,'(a)') line
    !    do i=1, nRTOnOff
    !        read(FNumRTOnOff,*) cln_number_On_Off(i), SP_num_On_Off(i), On_off(i)
    !        
    !        ! Compute applied rate from inputs
    !        if(LowRate_ft3_day(cln_number_On_Off(i))==-9999.) then  ! on/off state from nint
    !            if(nint(On_off(i))==0) then 
    !                AppliedRate_ft3_day(i)=0.0d0
    !                State=0.0
    !            else if(nint(On_off(i))==1) then 
    !                AppliedRate_ft3_day(i)=HighRate_ft3_day(cln_number_On_Off(i))
    !                State=1.0
    !            else  
    !                call ErrMsg('Nint not 0 or 1')
    !            endif
    !        else if(LowRate_ft3_day(cln_number_On_Off(i))/=-9999.) then  ! off/low/high state from fraction
    !            if(On_off(i) >= 0.0 .and. On_off(i) < 0.333) then
    !                AppliedRate_ft3_day(i)=0.0d0
    !                State=0.0
    !            else if(On_off(i) >= 0.333 .and. On_off(i) < 0.666) then    
    !                AppliedRate_ft3_day(i)=LowRate_ft3_day(cln_number_On_Off(i))
    !                State=0.5
    !            else if(On_off(i) >= 0.666 .and. On_off(i) <= 1.0) then 
    !                AppliedRate_ft3_day(i)=HighRate_ft3_day(cln_number_On_Off(i))
    !                State=1.0
    !            else
    !                call ErrMsg('Nint not in range between 0.0 and 1.0')
    !
    !            endif
    !        else
    !            call ErrMsg('Bad low rate')
    !        endif
    !        write(FNumRTOnOffNints,'(i6,i6,f20.5,f5.1,f20.5)') cln_number_On_Off(i), SP_num_On_Off(i), On_off(i),State,AppliedRate_ft3_day(i)
    !    end do
    ! 
    !    
    !    !-----------------------------------------------------------------------------------
    !    ! Pass1: Read/write well file and update RT well rate, inserting well if not present
    !    !    - count new total wells for stress period
    !    
    !    ! Modflow Well .wel file
    !    read(FnumTG,'(a)') FnameWel
    !    call OpenAscii(FNumWel,FnameWel)
    !    call Msg( 'Modflow well file: '//trim(FnameWel))
    !    
    !    ! Open new well .wel output file
    !    FnameWelOut='pass1.tmp'
    !    call OpenAscii(FnumWelOut,FnameWelOut)
    !    call Msg( 'Modflow well output TEMP file: '//trim(FnameWelOut))
    !
    !    read(FNumWel,'(a)') line
    !    write(FnumWelOut,'(a)') trim(line)  ! save first line
    !
    !    iOnOff=1
    !    RTRateTotal(:)= 0.0
    !    nStressPeriods=0
    !    read_wel_file: do 
    !        read(FNumWel,'(a)',iostat=status) line
    !        if(status /= 0) then  ! end of file
    !            exit read_wel_file
    !        endif
    !        
    !        l1=index(line,'stress period ')
    !        if(l1 > 0) then ! new stress period
    !            write(FnumWelOut,'(a)') trim(line)  ! save stress period header
    !            
    !            ! Extract stress period from line
    !            l1=l1+14  ! position at end of string 'stress period '
    !            TMPStr=line(31:)
    !            l2=l1+index(line(l1:),':')-2
    !            read(line(l1:l2),*) iStressPeriod
    !            
    !            nStressPeriods=max(nStressPeriods,iStressPeriod)
    !
    !            
    !            read(line,*) idum1, idum2, iWellOrig
    !            iWellNew(iStressPeriod)=iWellOrig
    !            do 
    !                read(FNumWel,'(a)',iostat=status) line
    !                if(status/=0) exit read_wel_file
    !                l1=index(line,'stress period ')
    !                if(l1 > 0) then ! next stress period
    !                    backspace(FNumWel)
    !                    cycle read_wel_file
    !                endif
    !
    !                
    !                read(line,*) iCLN
    !                
    !                if( iOnOff<=nRTOnOff) then
    !                    if(SP_num_On_Off(iOnOff)==iStressPeriod) then
    !                        if(iCLN<cln_number_On_Off(iOnOff)) then ! NON RT CLN
    !                            write(FnumWelOut,'(a)') trim(line)  
    !                        else if(iCLN==cln_number_On_Off(iOnOff)) then  ! RT CLN already in list 
    !                            !write(FnumWelOut,'(a,i5,f20.1,5x,a)') 'overwrite ', iCLN,PlaceholderRate_ft3_day(cln_number_On_Off(iOnOff)),trim(WellName(cln_number_On_Off(iOnOff)))
    !                            i1=27
    !                            i2=index(line,'well =')+6
    !                            write(FnumWelOut,'(i11,g15.7,a)') cln_number_On_Off(iOnOff), AppliedRate_ft3_day(iOnOff), line(i1:i2)//trim(WellName(cln_number_On_Off(iOnOff)))
    !                            RTRateTotal(iStressPeriod)= RTRateTotal(iStressPeriod)+AppliedRate_ft3_day(iOnOff)
    !                            iOnOff=iOnOff+1
    !                        else if(iCLN>cln_number_On_Off(iOnOff)) then  ! RT CLN not in list
    !                            !write(FnumWelOut,'(a,i5,f20.1,5x,a)') 'add ', iCLN,PlaceholderRate_ft3_day(cln_number_On_Off(iOnOff)),WellName(cln_number_On_Off(iOnOff))
    !                            i1=27
    !                            i2=index(line,'well =')+6
    !                            write(FnumWelOut,'(i11,g15.7,a)') cln_number_On_Off(iOnOff), AppliedRate_ft3_day(iOnOff), line(i1:i2)//trim(WellName(cln_number_On_Off(iOnOff)))
    !                            RTRateTotal(iStressPeriod)= RTRateTotal(iStressPeriod)+AppliedRate_ft3_day(iOnOff)
    !                            iOnOff=iOnOff+1
    !                            iWellNew(iStressPeriod)=iWellNew(iStressPeriod)+1
    !                            backspace(FnumWel)
    !                            !write(FnumWelOut,'(a,i5)') 'inc nwells ', iWellNew(iStressPeriod)
    !                        endif
    !                        !iOnOff=min(iOnOff,nRTOnOff)
    !                    else
    !                        write(FnumWelOut,'(a)') trim(line)  
    !                    end if
    !                else
    !                    write(FnumWelOut,'(a)') trim(line)  
    !                end if
    !                
    !                    
    !            end do
    !            continue
    !            
    !        endif
    !    end do read_wel_file
    !    
    !    call freeunit(FNumWel)
    !    call freeunit(FNumWelOut)
    !    
    !    ! Pass2: update number of wells per stress period
    !    call OpenAscii(FNumWel,'pass1.tmp')
    !    
    !    ! Open new well .wel output file
    !    FnameWelOut='rt_'//FnameWel  
    !    call OpenAscii(FnumWelOut,FnameWelOut)
    !    call Msg( 'Modflow well output file: '//trim(FnameWelOut))
    !    
    !    do 
    !        read(FnumWel,'(a)',iostat=status) line
    !        if(status /= 0) then  ! end of file
    !            exit 
    !        endif
    !   
    !        l1=index(line,'stress period ')
    !        if(l1 > 0) then ! new stress period, adjust number of wells
    !            
    !            ! Extract stress period from line
    !            l1=l1+14  ! position at end of string 'stress period '
    !            TMPStr=line(31:)
    !            l2=l1+index(line(l1:),':')-2
    !            read(line(l1:l2),*) iStressPeriod
    !
    !            read(line,*) idum1, idum2, iWellOrig
    !            write(line(:30),'(3i10)') idum1, idum2,iWellNew(iStressPeriod)
    !            
    !        endif
    !       
    !        write(FnumWelOut,'(a)') trim(line) 
    !
    !    end do
    !    
    !    call freeunit(FNumWel)
    !    call freeunit(FNumWelOut)
    !    CMDString='del pass1.tmp'
    !    i=system(CMDString)
    !    
    !    
    !    ! LADPW rates input file
    !    read(FnumTG,'(a)') FnameLADWP  
    !    call OpenAscii(FnumLADWP,FnameLADWP)
    !    call Msg( 'LADPW rate input file: '//trim(FnameLADWP))
    !    
    !    read(FnumLADWP,'(a)') line
    !    do i=1,nStressPeriods
    !        read(FnumLADWP,*) j, LADWPRateTotal(i)
    !    end do
    !        
    !    ! Penalties input file
    !    read(FnumTG,'(a)') FNamePenalty  
    !    call OpenAscii(FNumPenalty,FNamePenalty)
    !    call Msg( 'LADPW rate penalties file: '//trim(FNamePenalty))
    !    
    !    read(FNumPenalty,*) nRange
    !    allocate(MinRange(nRange), MaxRange(nRange), MinPenalty(nRange), MaxPenalty(nRange))
    !    do i=1,nRange
    !        read(FNumPenalty,*) MinRange(i), MaxRange(i), MinPenalty(i), MaxPenalty(i)
    !        write(TMPStr,'(4f20.5)') MinRange(i), MaxRange(i), MinPenalty(i), MaxPenalty(i)
    !        call Msg( 'Rate1 Rate2 Penalty1 Penalty2: '//trim(TMPStr))
    !        if(i>1) then
    !            if(Minrange(i)/=MaxRange(i-1)) then
    !                write(ErrStr,'(a,2f20.5)') 'Range 1: ', MinRange(i-1), MaxRange(i-1)
    !                call Msg(ErrStr)
    !                write(ErrStr,'(a,2f20.5)') 'Range 2: ', MinRange(i), MaxRange(i)
    !                call Msg(ErrStr)
    !                call ErrMsg('Min range 2 not equal to max range 1')
    !            end if
    !            if(MinPenalty(i)/=MaxPenalty(i-1)) then
    !                write(ErrStr,'(a,2f20.5)') 'Penalty 1: ', MinPenalty(i-1), MaxPenalty(i-1)
    !                call Msg(ErrStr)
    !                write(ErrStr,'(a,2f20.5)') 'Penalty 2: ', MinPenalty(i), MaxPenalty(i)
    !                call Msg(ErrStr)
    !                call ErrMsg('Min penalty 2 not equal to max penalty 1')
    !            end if
    !        end if
    !    end do
    !
    !    
    !    ! Open RT well rate output file
    !    FnameRTRateOut='rt_rates_per_SP.lst'  
    !    call OpenAscii(FnumRTRateOut,FnameRTRateOut)
    !    call Msg( 'RT well rate output file: '//trim(FnameRTRateOut))
    !    write(FnumRTRateOut,'(a)') 'Stress period,RT Well Rate Total, LADPW Design Rate,       RT:LADWP Ratio,     PEST Penalty'
    !    do i=1,nStressPeriods
    !        LADWPRatio=RTRateTotal(i)/LADWPRateTotal(i)
    !        do j=1,nrange
    !            if(LADWPRatio>= MinRange(j) .and. LADWPRatio<= MaxRange(j)) then ! linear interpolation for penalty
    !                LADWPPenalty= MinPenalty(j)+(LADWPRatio-MinRange(j))/(MaxRange(j)-MinRange(j))*(MaxPenalty(j)-MinPenalty(j))
    !            endif
    !        end do
    !        write(FnumRTRateOut,'(i10,4f20.5)') i,RTRateTotal(i),LADWPRateTotal(i),LADWPRatio,LADWPPenalty
    !    end do
    !   
    !    continue
    !
    !
    !end subroutine MUSG_PEST_RTWellOptimization
    


    end module MUSG

