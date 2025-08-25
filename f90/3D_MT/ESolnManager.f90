module ESolnManager

    use GridDef
    use SolnSpace

    implicit None

    character(len=*), parameter :: FTYPE_ASCII = "ASCII"
    character(len=*), parameter :: FTYPE_BINARY = "BINARY"
    character(len=*), parameter :: FTYPE_NETCDF = "NETCDF"
    character(len=*), parameter :: FTYPE_HDF5 = "HDF5"


    character(len=25) :: EsMgr_ftype = FTYPE_ASCII
    logical :: EsMgr_save_in_file

    public :: EsMgr_init
    public :: EsMgr_create_eAll, EsMgr_create_e 
    public :: EsMgr_get
    public :: EsMgr_save
    public :: EsMgr_transfer

contains

    ! EsMgr_init - Initalizes the manager sets the default filetype
    ! and determines if the Esoln files should be saved or not
    !
    subroutine EsMgr_init(grid, save_in_file, prefix, ftype)

        implicit none

        type (grid_t), intent(in) :: grid
        logical, intent(in), optional :: save_in_file
        character(len=*), intent(in), optional :: prefix
        character(len=*), intent(in), optional :: ftype

    end subroutine EsMgr_init

    subroutine EsMgr_create_e(e, iTx, nPol)

        implicit none

        type (solnVector_t), intent(inout) :: e
        integer, intent(in) :: iTx
        integer, intent(in), optional :: nPol

    end subroutine EsMgr_create_e

    subroutine EsMgr_create_eAll(eAll, nTx)

        implicit none

        type (solnVectorMTX_t), intent(inout) :: eAll
        integer, intent(in) :: nTx

    end subroutine EsMgr_create_eAll
    

    subroutine EsMgr_get(e, iTx, nPol)

        implicit none

        type (solnVector_t), intent(inout) :: e
        integer, intent(in) :: iTx
        integer, intent(in), optional :: nPol

    end subroutine EsMgr_get


    subroutine EsMgr_save(e)

        implicit none

        type (solnVector_t), intent(inout) :: e

    end subroutine EsMgr_save


    subroutine EsMgr_transfer(e, to, nPol)

        implicit none

        type (solnVector_t), intent(inout) :: e
        integer, intent(in) :: to
        integer, intent(in), optional :: nPol

    end subroutine EsMgr_transfer

end module ESolnManager
