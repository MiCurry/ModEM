module ESolnManager

    use GridDef
    use SolnSpace
    use utilities

    implicit None

    type (solnVectorMTX_t) :: EsMgr_eAll

    character(len=*), parameter :: FTYPE_ASCII = "ASCII"
    character(len=*), parameter :: FTYPE_BINARY = "BINARY"
    character(len=*), parameter :: FTYPE_NETCDF = "NETCDF"
    character(len=*), parameter :: FTYPE_HDF5 = "HDF5"

    type (grid_t), pointer :: EsMgr_grid => null()
    character(len=25) :: EsMgr_ftype
    character(len=256) :: EsMgr_prefix
    logical :: EsMgr_save_in_file

    public :: EsMgr_init
    public :: EsMgr_create_eAll, EsMgr_create_e 
    public :: EsMgr_get
    public :: EsMgr_save
    public :: EsMgr_transfer
    public :: FTYPE_ASCII, FTYPE_BINARY, FTYPE_NETCDF, FTYPE_HDF5

contains

    ! EsMgr_init - Initalizes the manager sets the default filetype
    ! and determines if the Esoln files should be saved or not
    !
    subroutine EsMgr_init(grid, save_in_file, prefix, ftype)

        implicit none

        type (grid_t), target, intent(in) :: grid
        logical, intent(in), optional :: save_in_file
        character(len=*), intent(in), optional :: prefix
        character(len=*), intent(in), optional :: ftype

        logical :: save_in_file_lcl
        character(len=256) :: prefix_lcl
        character(len=256) :: ftype_lcl

        if (present(save_in_file)) then
            save_in_file_lcl = save_in_file
        else
            save_in_file_lcl = .false.
        end if

        if (.not. save_in_file_lcl .and. present(prefix)) then
            write(0,*) "Warning: Argument 'prefix' was passed, but 'save_in_file' was not present"
            write(0,*) "Warning: 'prefix' will not have an effect. Set 'save_in_file' to true to save"
            write(0,*) "Warning: esolns in files"
        end if

        if (.not. save_in_file_lcl .and. present(ftype)) then
            write(0,*) "Warning: Argument 'ftype' was passed, but 'save_in_file' was not present"
            write(0,*) "Warning: 'ftype' will not have an effect. Set 'save_in_file' to true to save"
            write(0,*) "Warning: esolns in files"
        end if

        if (present(prefix)) then
            prefix_lcl = prefix
        else
            prefix_lcl = "esoln"
        end if

        if (present(ftype)) then
            ftype_lcl = ftype
        else
            ftype_lcl = FTYPE_ASCII
        end if

        select case (ftype_lcl)
            case (FTYPE_ASCII : FTYPE_BINARY)
            case (FTYPE_NETCDF)
                write(0,*) "ERROR: NetCDF not implemented for storing esoln files"
                write(0,*) "ERROR: Valid options are: [", trim(FTYPE_ASCII), " | ", trim(FTYPE_BINARY), "]"
                call ModEM_abort()
            case (FTYPE_HDF5)
                write(0,*) "ERROR: HDF5 not implemented for storing esoln files"
                write(0,*) "ERROR: Valid options are: [", trim(FTYPE_ASCII), " | ", trim(FTYPE_BINARY), "]"
                call ModEM_abort()
            case DEFAULT
                write(0,*) "ERROR: ", trim(ftype_lcl), " is not a valid file type for Esoln"
                write(0,*) "ERROR: Valid options are: [", trim(FTYPE_ASCII), " | ", trim(FTYPE_BINARY), "]"
                call ModEM_abort()
        end select

        EsMgr_grid => grid
        EsMgr_save_in_file = save_in_file_lcl
        EsMgr_prefix = prefix_lcl
        EsMgr_ftype = ftype_lcl

    end subroutine EsMgr_init

    subroutine EsMgr_create_e(e, iTx, nPol)

        implicit none

        type (solnVector_t), intent(inout) :: e
        integer, intent(in) :: iTx
        integer, intent(in), optional :: nPol

        call create_solnVector(EsMgr_grid, iTx, e, place_holder=EsMgr_save_in_file)

    end subroutine EsMgr_create_e

    subroutine EsMgr_create_eAll(eAll, nTx)

        implicit none

        type (solnVectorMTX_t), intent(inout) :: eAll
        integer, intent(in) :: nTx
        integer :: iTx

        type(solnVector_t) :: e

        call create_solnVectorMTX(nTx, eAll)

        do iTx = 1, nTx
            call create_solnVector(EsMgr_grid, iTx, e, place_holder=EsMgr_save_in_file)
            call copy_solnVector(eAll % solns(iTx), e)
        end do

    end subroutine EsMgr_create_eAll
    
    subroutine EsMgr_get(e, iTx, nPol)

        implicit none

        type (solnVector_t), intent(inout) :: e
        integer, intent(in) :: iTx
        integer, intent(in), optional :: nPol

        if (EsMgr_save_in_file) then
            call read_esoln_from_file(e, iTx, nPol)
        else
            e = EsMgr_eAll % solns(iTx)
        end if

    end subroutine EsMgr_get

    subroutine read_esoln_from_file(e, iTx, nPol)

        implicit none

        type (solnVector_t), intent(inout) :: e
        integer, intent(in) :: iTx
        integer, intent(in), optional :: nPol

        call read_solnVector(e, EsMgr_prefix, EsMgr_ftype)

    end subroutine read_esoln_from_file

    subroutine EsMgr_save(e)

        implicit none

        type (solnVector_t), intent(in) :: e

        if (EsMgr_save_in_file) then
            call write_soln_to_file(e)
        else
            EsMgr_eAll % solns(e % tx)  = e
        end if

    end subroutine EsMgr_save

    subroutine write_soln_to_file(e)

        implicit none

        type (solnVector_t), intent(in) :: e

        call write_solnVector(e, EsMgr_prefix, EsMgr_ftype)

    end subroutine write_soln_to_file

    subroutine EsMgr_transfer(e, to, nPol)

        implicit none

        type (solnVector_t), intent(inout) :: e
        integer, intent(in) :: to
        integer, intent(in), optional :: nPol

    end subroutine EsMgr_transfer

end module ESolnManager
