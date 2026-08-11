! *****************************************************************************
!   This is the HDF5 I/O version coded by Spencer Wilbur (USGS) under the direction
!   of Anna Kelbert, Jul. 2023. File intentionally misnamed; it is renamed at the
!   level of the configuration file.
submodule (DataIO) DataIO_HDF5
  ! This module contains io routines for reading and writing the data vectors
  ! Version: 3D MT
  use hdf5
  use math_constants
  use file_units
  use utilities
  use dataspace
  use gridcalc
  use transmitters
  use receivers
  use datatypes
  use ModEM_HDF5

  implicit none

  ! private dictionary of data block info dimension (nTxt,nDt)
  ! where nTxt = number of all possible transmitter types
  !       nDt  = number of all possible data types
  ! number of transmitter types comes from the DICT/txTypes module
  ! and defines the number of conceptually different types of sources
  !type (data_file_block), pointer, save, dimension(:,:) :: fileInfo

  ! we are converting from an "old format" to a "new format"
  ! the only difference being that in the new format, there is
  ! an additional line in the head that indicates transmitter type.
  ! on output, use the same format as on input. AK 25 May 2018
  logical :: old_data_file_format = .true.
  integer(HID_T) :: group_id, attr_id, dset_id, dspace_id, atype_id, aspace_id, dtype_id ! file, data set, and dataspace handles

  character(len=*), parameter :: DATA_GRP_NAME = "/Data"

  character(len=*), parameter :: DATA_MT_GRP_NAME = "Data/MT"
  character(len=*), parameter :: DATA_MT_TXDICT_GRP_NAME = "Data/MT/txdict"
  character(len=*), parameter :: DATA_SET_PERIODS_NAME = "periods"
  character(len=*), parameter :: DATA_MT_RXDICT_GRP = "Data/MT/rxdict"
  character(len=*), parameter :: DATA_MT_TYPELIST_GRP_NAME = 'Data/MT/typelist'
  character(len=*), parameter :: DATA_MT_DATABLOCKS_BASE_NAME = 'Data/MT/datablocks'
  character(len=*), parameter :: DATA_MT_DATABLOCK_BASE_NAME = 'Data/MT/datablocks/datablock'

  character(len=*), parameter :: MT_IMPEDANCE_VAR_NAME = 'Z'
  character(len=*), parameter :: MT_TIPPER_VAR_NAME = 'T'

  ! A private derived type that will provide record keeping for 
  ! iterating through datablocks and allow functions to perform processing.
  type :: datablock_iter_type
    type(dataVectorMTX_t), pointer :: allData
    integer (kind=HID_T) :: data_block_itx_gid
    integer :: iTx
    integer :: iDt
  end type

Contains

!**********************************************************************  

subroutine write_data_hdf5(allData, cfile)

    character(len=*), intent(in)                  :: cfile
    type(dataVectorMTX_t), intent(in)        :: allData

    character(len=*), parameter :: DATA_GROUP = 'Data'
    character(len=*), parameter :: DATA_MT_GROUP = 'Data/MT'

    integer (kind=HID_T) :: file_id
    integer (kind=HID_T) :: gid

    call ModEM_HDF5_create_file(cfile, H5F_ACC_TRUNC_F, file_id)
    call ModEM_HDF5_create_group(file_id, DATA_GROUP, gid)
    call ModEM_HDF5_create_group(file_id, DATA_MT_GROUP, gid)

    call write_typelist(file_id, allData)
    call write_datablocks(file_id, allData)
    call write_txdict(file_id, allData)
    call write_rxdict(file_id)

    call ModEM_HDF5_close_file(file_id)

end subroutine write_data_hdf5

subroutine write_txdict(file_id, allData)

    implicit none

    integer (kind=HID_T), intent(in) :: file_id
    type(dataVectorMTX_t), intent(in) :: allData

    integer (kind=HID_T) :: txdict_h5_gid
    integer (kind=HID_T) :: periods_dspace_id
    integer (kind=HID_T) :: periods_dset_id
    integer :: i
    integer :: rank
    integer :: hdferr

    real (kind=prec), dimension(:), pointer :: periods

    call ModEM_HDF5_create_group(file_id, 'Data/MT/txdict', txdict_h5_gid)

    ! Write the Periods to the file
    rank = 1
    call ModEM_HDF5_create_dataspace(rank, (/size(txDict, kind=HSIZE_T)/), periods_dspace_id)
    call ModEM_HDF5_create_dataset(txdict_h5_gid, 'periods', H5T_NATIVE_DOUBLE, periods_dspace_id, periods_dset_id)

    allocate(periods(size(txdict)))

    do i = 1, size(txDict), 1
        periods(i) = txDict(i) % period
    end do

    call ModEM_HDF5_write_dataset(periods_dset_id, H5T_NATIVE_DOUBLE, periods)

    call ModEM_HDF5_add_attr(txdict_h5_gid, 'order', 'ascending')

    call ModEM_HDF5_close_dataset(periods_dset_id)
    call ModEM_HDF5_close_dataspace(periods_dspace_id)
    call ModEM_HDF5_close_group(txdict_h5_gid)

    deallocate(periods)

end subroutine write_txdict

subroutine write_rxdict(file_id)

    implicit none

    integer (kind=HID_T), intent(in) :: file_id

    integer (kind=HID_T) :: rx_group_id
    integer (kind=HID_T) :: rx_dspace_id
    integer (kind=HID_T) :: rx_dset_id
    integer (kind=HID_T) :: lat_dset_id, lon_dset_id
    integer (kind=HID_T) :: xyz_dspace_id, xyz_dset_id
    integer (kind=HID_T) :: elv_dset_id
    integer (kind=HID_T) :: codes_dset_id
    integer (kind=HID_T) :: codes_str_typeid
    integer :: n_recivers
    integer :: i

    real (kind=prec), pointer :: rxdict_elv(:),rxdict_lat(:), rxdict_lon(:)
    real (kind=prec), pointer :: rxdict_x(:),rxdict_y(:), rxdict_z(:)
    real (kind=prec), pointer :: rxdict_xyz(:,:)
    
    character (len=5), pointer, dimension(:) :: rxdict_codes 

    call ModEM_HDF5_create_group(file_id, 'Data/MT/rxdict', rx_group_id)

    n_recivers = size(rxDict)

    call ModEM_HDF5_create_dataspace(rank(rxDict), (/size(rxDict, kind=HSIZE_T)/), rx_dspace_id)

    ! Latitudes
    allocate(rxdict_lat(n_recivers))
    do i = 1, n_recivers, 1
        rxdict_lat(i) = rxDict(i) % x(1)
    end do

    call ModEM_HDF5_create_dataset(rx_group_id, 'lat', H5T_NATIVE_DOUBLE, rx_dspace_id, lat_dset_id)
    call ModEM_HDF5_write_dataset(lat_dset_id, H5T_NATIVE_DOUBLE, rxdict_lat)
    call ModEM_HDF5_close_dataset(lat_dset_id)

    deallocate(rxdict_lat)

    ! Longitudes
    allocate(rxdict_lon(n_recivers))
    do i = 1, n_recivers, 1
        rxdict_lon(i) = rxDict(i) % x(2)
    end do

    call ModEM_HDF5_create_dataset(rx_group_id, 'lon', H5T_NATIVE_DOUBLE, rx_dspace_id, lon_dset_id)
    call ModEM_HDF5_write_dataset(lon_dset_id, H5T_NATIVE_DOUBLE, rxdict_lon) 
    call ModEM_HDF5_close_dataset(lon_dset_id)

    deallocate(rxdict_lon)

    ! Elevation
    allocate(rxdict_elv(n_recivers))
    do i = 1, n_recivers, 1
        rxdict_elv(i) = rxDict(i)%x(3)
    end do

    call ModEM_HDF5_create_dataset(rx_group_id, 'elv', H5T_NATIVE_DOUBLE, rx_dspace_id, elv_dset_id)
    call ModEM_HDF5_write_dataset(elv_dset_id, H5T_NATIVE_DOUBLE, rxdict_elv)
    call ModEM_HDF5_close_dataset(elv_dset_id)

    deallocate(rxdict_elv)

    ! Station Codes
    allocate(rxdict_codes(n_recivers))
    do i = 1, n_recivers, 1
        rxdict_codes(i) = rxDict(i) % id
    end do

    call ModEM_HDF5_create_string_type(codes_str_typeid, len(rxdict_codes(1), kind=HSIZE_T))
    call ModEM_HDF5_create_dataset(rx_group_id, 'codes', codes_str_typeid, rx_dspace_id, codes_dset_id)
    call ModEM_HDF5_write_dataset(codes_dset_id, codes_str_typeid, rxdict_codes)
    call MoDEM_HDF5_close_dataset(codes_dset_id)

    deallocate(rxdict_codes)
    
    ! Close the rx_dspace_id for lat, lon, elv, codes etc...
    call ModEM_HDF5_close_dataspace(rx_dspace_id)

    ! Cartesian Coordinates
    allocate(rxdict_xyz(3,n_recivers))
    do i = 1, n_recivers, 1
        rxdict_xyz(:,i) = rxDict(i)%x
    end do

    call ModEM_HDF5_create_dataspace(rank(rxDict_xyz), (/shape(rxdict_xyz, kind=HSIZE_T)/), xyz_dspace_id)
    call ModEM_HDF5_create_dataset(rx_group_id, 'xyz', H5T_NATIVE_DOUBLE, xyz_dspace_id, xyz_dset_id)
    call ModEM_HDF5_write_dataset(xyz_dset_id, H5T_NATIVE_DOUBLE, rxdict_xyz)
    call ModEM_HDF5_close_dataset(xyz_dset_id)
    call ModEM_HDF5_close_dataspace(xyz_dspace_id)

    deallocate(rxdict_xyz)
    
    ! Add Attributes
    ! TODO: Actually write a correct origin
    !write(0,*) 'File info Origin: ', 0, 0
    call ModEM_HDF5_add_attr(rx_group_id, 'origin', (/0.0, 0.0/))

    call ModEM_HDF5_close_group(rx_group_id)

end subroutine write_rxdict

subroutine write_typelist(file_id, allData)

    implicit none

    integer (kind=HID_T), intent(in)  :: file_id
    type(dataVectorMTX_t), intent(in) :: allData

    character(len=*), parameter :: TYPELIST_GROUP_NAME= "Data/MT/typelist"

    character(len=512) :: datatype_group_name = ""
    character(len=120) :: dtype_long_name
    character(len=120) :: units
    character(len=512) :: description
    character(len=512) :: externalurl
    character(len=120) :: input
    character(len=120) :: output
    character(len=120) :: intention
    character(len=120) :: tag

    logical :: is_complex

    integer(kind=HID_T) :: typelist_group_id, datatype_group_id, comp_dspace_id, comp_dset_id
    integer(kind=HID_T) :: comp_type_id

    integer :: iDataType, txType

    call ModEM_HDF5_create_group(file_id, TYPELIST_GROUP_NAME, typelist_group_id)

    do iDataType = 1, allData % d(1) % nDt
        txType = allData % d(1) % data(iDataType) % dataType

        select case (allData % d(1) % data(iDataType) % dataType)
            case (Full_Impedance)
                ! Data name and units are already in the typedict... see 'longname/units' below
                datatype_group_name = trim('Z')
                description = 'MT Impedance'
                externalurl = 'http://www.iris.edu/dms/products/emtf/impedance.html'
                input = 'H'
                intention = 'primary data type'
                output = 'E'
                tag = 'impedance'
            case (Full_Vertical_Components)
                ! Data name and units are already in the typedict... see 'longname/units' below
                datatype_group_name = trim('T')
                description = 'Vertical Field Transfer Functions(tipper)'
                externalurl = 'http://www.iris.edu/dms/products/emtf/tipper.html'
                input = 'H'
                intention = 'primary data type'
                output = 'E'
                tag = 'tipper'
        end select

        ! Create the group
        call ModEM_HDF5_create_group(typelist_group_id, datatype_group_name, datatype_group_id)

        ! add all attrbitues
        call ModEM_HDF5_add_attr(datatype_group_id, 'longname', trim(typeDict(txType) % name))
        call ModEM_HDF5_add_attr(datatype_group_id, 'units', trim(typeDict(txType) % units))
        call ModEM_HDF5_add_attr(datatype_group_id, 'description', trim(description))
        call ModEM_HDF5_add_attr(datatype_group_id, 'externalurl', trim(externalurl))
        call ModEM_HDF5_add_attr(datatype_group_id, 'input', trim(input))
        call ModEM_HDF5_add_attr(datatype_group_id, 'output', trim(output))
        call ModEM_HDF5_add_attr(datatype_group_id, 'intention', trim(intention))
        call ModEM_HDF5_add_attr(datatype_group_id, 'tag', trim(tag))

        if (typeDict(txType) % isComplex) then
            ! No Boolean values for HDF5
            call ModEM_HDF5_add_attr(datatype_group_id, 'complex', 1)
        else 
            ! No Boolean values for HDF5
            call ModEM_HDF5_add_attr(datatype_group_id, 'complex', 0)
        end if

        ! Add the component attribute
        call ModEM_HDF5_create_dataspace(rank(typeDict(txType) % id), (/size(typeDict(txType) % id, kind=HSIZE_T)/), comp_dspace_id)
        call ModEM_HDF5_create_string_type(comp_type_id, len(typeDict(txType) % id(1), kind=HSIZE_T))
        call ModEM_HDF5_create_dataset(datatype_group_id, ' components ', comp_type_id, comp_dspace_id, comp_dset_id)
        call ModEM_HDF5_write_dataset(comp_dset_id, comp_type_id, typeDict(txType) % id)

        ! Close this datatypes dataset, dataspace and group
        call ModEM_HDF5_close_type(comp_type_id)
        call ModEM_HDF5_close_dataset(comp_dset_id)
        call ModEM_HDF5_close_dataspace(comp_dspace_id)
        call ModEM_HDF5_close_group(datatype_group_id)
    end do

    call ModEM_HDF5_close_group(typelist_group_id)

end subroutine write_typelist

!********************************************************************** 

subroutine write_datablocks(file_id, allData)

    implicit none

    integer (kind=HID_T), intent(in)  :: file_id
    type(dataVectorMTX_t), intent(in) :: allData

    integer :: iTx, nDataBlocks

    character(len=*), PARAMETER :: DATA_BLOCKS_GROUP_NAME = '/Data/MT/datablocks'
    character(len=*), PARAMETER :: DATA_BLOCK_GROUP_NAME = '/Data/MT/datablocks/datablock'

    character(len=512) :: datatype_group_name

    character(len=*), parameter :: T = "/T"
    character(len=*), parameter :: Z = "/Z"
    character(len=512) :: data_block_iTx_name, dbTZ, dblk

    integer :: nDataType
    integer (kind=HID_T) :: datablocks_group_id, datablock_group_id, datatype_group_id

    integer :: txType
    character(len=10) :: txType_name
    integer :: datatype
    logical :: conjugate

    call ModEM_HDF5_create_group(file_id, DATA_BLOCKS_GROUP_NAME, datablocks_group_id)

    ! Loop through the transmitters...
    do iTx = 1, size(allData % d)
        write(data_block_iTx_name, '(a, a1, I0.2)') trim(DATA_BLOCK_GROUP_NAME), '.', iTx
        call ModEM_HDF5_create_group(file_id, data_block_itx_name, datablock_group_id)

        ! Add the transmitter value as an attribute to this datablock
        call ModEM_HDF5_add_attr(datablock_group_id, 'Tx', txDict(iTx) % period)

        ! loop through the datatypes for each transmitters
        do nDataType = 1, allData % d(iTx) % nDt
            datatype = allData % d(iTx) % data(nDataType) % dataType
            txtype = allData % d(iTx) % txType
            txType_name = trim(tx_type_name(txtype))

            select case(datatype)
                case (Full_Impedance)
                    datatype_group_name = trim(data_block_iTx_name)//trim(Z)
                case (Full_Vertical_Components)
                    datatype_group_name = trim(data_block_iTx_name)//trim(T)
                case default
                    call errStop('ModEM_HDF5 cannot write out this datatype yet..')
            end select

            write(0,*) 'txType: ', txtype, txType_name, ' datatype: ', datatype, ' datatype_group_name: ', trim(datatype_group_name)

            ! TODO: We probably want these set above in the select case, or 
            ! just have each data type add it's attributes on its own... for now this is okay
            call ModEM_HDF5_create_group(datablock_group_id, datatype_group_name, datatype_group_id)
            call ModEM_HDF5_add_attr(datatype_group_id, 'column', 'component')
            call ModEM_HDF5_add_attr(datatype_group_id, 'row', 'Rx')
            call ModEM_HDF5_add_attr(datatype_group_id, 'comment', 'complex values sorted by real/imag pairs')

            write(0,*) 'Writing datablock for Tx: ', iTx, ' DataType: ', nDataType, ' Name: ', trim(datatype_group_name), datatype 
            ! Print out fileinfo:
            write(0,*) 'iTx: ', iTx, ' datatype: ', datatype
            write(0,*) 'FileInfo: ', fileInfo(txType, datatype) % defined
            write(0,*) 'FileINfo units_in_file: ', trim(fileInfo(txType, datatype) % units_in_file)
            call compact(fileInfo(txType, datatype) % sign_info_in_file)
            write(0,*) 'FineInfo sign_in_file: ', fileInfo(txType, datatype) % sign_in_file
            write(0,*) 'FineInfo sign_info_in_file: ', trim(fileInfo(txType, datatype) % sign_info_in_file)
            write(0,*) 'After FileInfo...'

            if (fileInfo(txType, datatype) % units_in_file == '') then
                call ModEM_HDF5_add_attr(datatype_group_id, 'units', typeDict(datatype) % units)
            else
                call ModEM_HDF5_add_attr(datatype_group_id, 'units', fileInfo(txType, datatype) % units_in_file)
            end if


            if (fileInfo(txType, datatype) % sign_in_file == ISIGN) then
                conjugate = .false.
                call ModEM_HDF5_add_attr(datatype_group_id, 'sign', ISIGN)
            else
                conjugate = .true.
                call ModEM_HDF5_add_attr(datatype_group_id, 'sign', fileInfo(txType, datatype) % sign_in_file)
            end if

            ! Finish writing the datablock - std, value, etc.
            call write_datablock(datatype_group_id, allData % d(iTx) % data(nDataType))

            call ModEM_HDF5_close_group(datatype_group_id)
        end do

        call ModEM_HDF5_close_group(datablock_group_id)
    end do

end subroutine write_datablocks

subroutine write_datablock(datablock_group_id, dataBlock)

    implicit none

    integer (kind=HID_T), intent(in) :: datablock_group_id
    type (dataBlock_t), pointer, intent(in)   :: dataBlock

    integer (kind=HID_T) :: std_dspace_id
    integer (kind=HID_T) :: std_dset_id

    integer (kind=HID_T) :: value_dspace_id
    integer (kind=HID_T) :: value_dset_id

    integer (kind=HID_T) :: irx_dspace_id
    integer (kind=HID_T) :: irx_dset_id

    ! Write standard deviation for impedance
    call ModEM_HDF5_create_dataspace(rank(dataBlock % error), shape(dataBlock % error, kind=HSIZE_T), std_dspace_id)
    call ModEM_HDF5_create_dataset(datablock_group_id, "std", H5T_NATIVE_DOUBLE, std_dspace_id, std_dset_id)

    if (datablock % errorBar) then
        call ModEM_HDF5_write_dataset(std_dset_id, H5T_NATIVE_DOUBLE, dataBlock % error)
    else
        ! Let us not allocate a new array here, so we can save memory by just doing
        ! things in place.
        dataBlock % error(:,:) = LARGE_REAL
        call ModEM_HDF5_write_dataset(std_dset_id, H5T_NATIVE_DOUBLE, dataBlock % error)
    end if

    call ModEM_HDF5_close_dataset(std_dset_id)
    call ModEM_HDF5_close_dataspace(std_dspace_id)

    ! Write error 
    call ModEM_HDF5_create_dataspace(rank(dataBlock % value), shape(dataBlock % value, kind=HSIZE_T), value_dspace_id)
    call ModEM_HDF5_create_dataset(datablock_group_id, "value", H5T_NATIVE_DOUBLE, value_dspace_id, value_dset_id)

    call ModEM_HDF5_write_dataset(value_dset_id, H5T_NATIVE_DOUBLE, dataBlock % value)

    call ModEM_HDF5_close_dataset(value_dset_id)
    call ModEM_HDF5_close_dataspace(value_dspace_id)

    ! Write component data 
    call ModEM_HDF5_create_dataspace(rank(dataBlock % rx), shape(dataBlock % rx, kind=HSIZE_T), irx_dspace_id)
    call ModEM_HDF5_create_dataset(datablock_group_id, "irx", H5T_NATIVE_INTEGER, irx_dspace_id, irx_dset_id)

    call ModEM_HDF5_write_dataset(irx_dset_id, H5T_NATIVE_INTEGER, dataBlock % rx)

    call ModEM_HDF5_close_dataset(irx_dset_id)
    call ModEM_HDF5_close_dataspace(irx_dspace_id)

end subroutine write_datablock

subroutine read_data_hdf5(allData, cfile)

    implicit none

    type(dataVectorMTX_t), intent(inout) :: allData
    character(*), intent(in) :: cfile

    integer (kind=HID_T) :: file_id

    integer :: nTxt, nDT

    call setup_typeDict()
    call ModEM_HDF5_open(cfile, file_id, H5F_ACC_RDONLY_F)

    call read_txdict(file_id)
    call read_rxdict(file_id)
    call read_typelist(file_id)

    nTxt = 5
    nDT = size(typeDict)
    call init_fileInfo(nTxt, nDT)

    call read_datablocks(file_id, allData)

    call ModEM_HDF5_close_file(file_id)

end subroutine read_data_hdf5

subroutine read_rxdict(file_id)

    implicit none

    integer (kind=HID_T) :: file_id

    integer (kind=HID_T) :: mt_rx_group_id
    integer (kind=HID_T) :: codes_dset_id, codes_dspace_id
    integer (kind=HID_T) :: codes_str_typeid
    integer (kind=HID_T) :: elv_dset_id, elv_dspace_id
    integer (kind=HID_T) :: lat_dset_id, lat_dspace_id
    integer (kind=HID_T) :: lon_dset_id, lon_dspace_id
    integer (kind=HID_T) :: xyz_dset_id, xyz_dspace_id

    integer (kind=HID_T) :: nElvs, nlat, nlon, nxyz, nSites

    real (kind=prec), pointer :: rxdict_elv(:), rxdict_lat(:), rxdict_lon(:)
    real (kind=prec), pointer :: rxdict_xyz(:,:)
    character (len=5), pointer, dimension(:) :: rxdict_codes

    real (kind=prec), dimension(:,:), allocatable :: siteLocations

    integer :: i

    call ModEM_HDF5_open_group(file_id, DATA_MT_RXDICT_GRP, mt_rx_group_id)

    ! Read the reciver (station) codes from the dataset
    call ModEM_HDF5_open_dataset(mt_rx_group_id, 'codes', codes_dset_id)
    call ModEM_HDF5_get_dataspace(codes_dset_id, codes_dspace_id)
    call ModEM_HDF5_get_dataspace_size(codes_dspace_id, nSites) 

    ! Get the string typeid for the codes dataset
    call ModEM_HDF5_get_dataset_type(codes_dset_id, codes_str_typeid)

    allocate(rxdict_codes(nSites))
    call ModEM_HDF5_read_dataset(codes_dset_id, codes_str_typeid, rxdict_codes)

    call ModEM_HDF5_close_dataspace(codes_dspace_id)
    call ModEM_HDF5_close_dataset(codes_dset_id)

    ! Read elevation 
    call ModEM_HDF5_open_dataset(mt_rx_group_id, 'elv', elv_dset_id)
    call ModEM_HDF5_get_dataspace(elv_dset_id, elv_dspace_id)
    call ModEM_HDF5_get_dataspace_size(elv_dspace_id, nElvs)

    allocate(rxdict_elv(nElvs))
    call ModEM_HDF5_read_dataset(elv_dset_id, H5T_NATIVE_DOUBLE, rxdict_elv)

    call ModEM_HDF5_close_dataspace(elv_dspace_id)
    call ModEM_HDF5_close_dataset(elv_dset_id)

    ! Read the lat/lon/xyz datasets
    call ModEM_HDF5_open_dataset(mt_rx_group_id, 'lat', lat_dset_id)
    call ModEM_HDF5_get_dataspace(lat_dset_id, lat_dspace_id)
    call ModEM_HDF5_get_dataspace_size(lat_dspace_id, nlat) 

    allocate(rxdict_lat(nlat))
    call ModEM_HDF5_read_dataset(lat_dset_id, H5T_NATIVE_DOUBLE, rxdict_lat)

    call ModEM_HDF5_close_dataspace(lat_dspace_id)
    call ModEM_HDF5_close_dataset(lat_dset_id)

    ! Read longitude
    call ModEM_HDF5_open_dataset(mt_rx_group_id, 'lon', lon_dset_id)
    call ModEM_HDF5_get_dataspace(lon_dset_id, lon_dspace_id)
    call ModEM_HDF5_get_dataspace_size(lon_dspace_id, nlon)

    allocate(rxdict_lon(nlon))
    call ModEM_HDF5_read_dataset(lon_dset_id, H5T_NATIVE_DOUBLE, rxdict_lon)

    call ModEM_HDF5_close_dataspace(lon_dspace_id)
    call ModEM_HDF5_close_dataset(lon_dset_id)

    ! Read xyz
    call ModEM_HDF5_open_dataset(mt_rx_group_id, 'xyz', xyz_dset_id)
    call ModEM_HDF5_get_dataspace(xyz_dset_id, xyz_dspace_id)
    call ModEM_HDF5_get_dataspace_size(xyz_dspace_id, nxyz)

    allocate(rxdict_xyz(3,nxyz))
    call ModEM_HDF5_read_dataset(xyz_dset_id, H5T_NATIVE_DOUBLE, rxdict_xyz)

    call ModEM_HDF5_close_dataspace(xyz_dspace_id)
    call ModEM_HDF5_close_dataset(xyz_dset_id)

    call ModEM_HDF5_close_group(mt_rx_group_id)

    ! Now Setup RX Dict
    allocate(siteLocations(nSites,3))

    do i = 1, nSites, 1
        siteLocations(i, 1) = rxdict_lat(i)
        siteLocations(i, 2) = rxdict_lon(i)    
        siteLocations(i, 3) = rxdict_elv(i)
    end do

    ! Setup rx dict convert nSites to single precision integer for setup_rxDict
    call setup_rxDict(int(nSites, kind=SP), siteLocations, rxdict_codes)

    if (output_level >= 5) then
        call print_rxDict()
    end if

    deallocate(siteLocations)
    deallocate(rxdict_codes)
    deallocate(rxdict_elv)
    deallocate(rxdict_lat)
    deallocate(rxdict_lon)

end subroutine read_rxdict

subroutine read_typelist(file_id)

    implicit none

    integer (kind=HID_T), intent(in) :: file_id

    ! TODO: Do we need to include this? The typeDict is already hard-coded in ModEM.

end subroutine read_typelist


subroutine read_txdict(file_id)

    implicit none

    integer (kind=HID_T), intent(in) :: file_id

    integer (kind=HID_T) :: mt_tx_group_id
    integer (kind=HID_T) :: periods_dset_id, periods_dspace_id

    integer (kind=HSIZE_T) :: nperiods
    integer :: i

    real (kind=prec), dimension(:), allocatable :: periods

    ! Open the MT Tx group
    call ModEM_HDF5_open_group(file_id, DATA_MT_TXDICT_GRP_NAME, mt_tx_group_id)

    ! Read the periods
    call ModEM_HDF5_open_dataset(mt_tx_group_id, DATA_SET_PERIODS_NAME, periods_dset_id)
    call ModEM_HDF5_get_dataspace(periods_dset_id, periods_dspace_id)
    call ModEM_HDF5_get_dataspace_size(periods_dspace_id, nperiods)

    allocate(periods(nperiods))

    call ModEM_HDF5_read_dataset(periods_dset_id, H5T_NATIVE_DOUBLE, periods)
    call setup_txDict(int(nPeriods, kind=SP), periods, 2)

    ! Set the tx_type field for all transmitters to 'MT'
    do i = 1, int(nPeriods, kind=SP)
        txDict(i) % tx_type = 'MT'
    end do

    deallocate(periods)

    ! CLose the MT Tx period group
    call ModEM_HDF5_close_dataset(periods_dset_id)
    call ModEM_HDF5_close_group(mt_tx_group_id)


end subroutine read_txdict

subroutine read_datablocks(file_id, allData)

    use iso_c_binding

    implicit none

    integer (kind=HID_T) :: file_id
    type(dataVectorMTX_t), intent(inout) :: allData

    character(len=*), parameter :: DATA_BLOCKS_GROUP_NAME = '/Data/MT/datablocks'
    character(len=*), parameter :: DATA_BLOCK_GROUP_NAME = '/Data/MT/datablocks/datablock'

    character(len=*), parameter :: MT_GROUP_NAME = 'MT'
    integer (kind=HID_T) :: data_group_id, mt_group_id, datablocks_group_id, datablock_itx_gid

    character(len=512) :: data_block_iTx_name
    logical :: exists

    integer :: iTx, nTx
    integer, target :: ndt
    integer (kind=HSIZE_T) :: idx

    type (c_funptr) :: funptr
    type (c_ptr) :: ptr
    integer, target :: ngrps
    integer :: ret_value, hdferr_lcl

    call ModEM_HDF5_open_group(file_id, DATA_GRP_NAME, data_group_id)
    call ModEM_HDF5_open_group(data_group_id, 'MT/datablocks', datablocks_group_id)

    nTx = size(txDict)

    call create_dataVectorMTX(nTx, allData)

    do iTx = 1, nTx
        write(data_block_iTx_name, '(a, a1, I0.2)') trim(DATA_BLOCK_GROUP_NAME), '.', iTx
        call ModEM_HDF5_open_group(file_id, data_block_itx_name, datablock_itx_gid)

        call count_number_of_datablocks(datablock_itx_gid, ndt)

        call create_dataVector(ndt, allData % d(iTx))
        allData % d(iTx) % tx = iTx
        allData % d(iTx) % txType = MT

        call data_iterate_datablocks(allData, datablock_itx_gid, iTx)

        call ModEM_HDF5_close_group(datablock_itx_gid)
    end do

    allData % allocated = .true.

    call ModEM_HDF5_close_group(datablocks_group_id)
    call ModEM_HDF5_close_group(data_group_id)

end subroutine read_datablocks

integer function count_datablock_cb(loc_id, name, info, ndt) bind(c)

    use iso_c_binding, only : c_ptr, c_null_char, c_f_pointer, c_int

    implicit none 

    integer (kind=HID_T), value:: loc_id
    character(len=1), dimension(1:10) :: name ! must have LEN=1 for bind(C) strings
    type (c_ptr) :: info
    integer, volatile :: ndt

    ndt = ndt + 1

    count_datablock_cb = 0_c_int

end function count_datablock_cb

subroutine count_number_of_datablocks(data_block_itx_gid, nDt)

    implicit none

    integer (kind=HID_T), intent(in) :: data_block_itx_gid
    integer, target, intent(out) :: ndt

    type(c_ptr) :: ptr
    type(c_funptr) :: funptr

    integer (kind=HSIZE_T) :: idx
    integer :: hdferr
    integer :: return_value

    ndt = 0 
    idx = 0

    ptr = c_loc(ndt)
    funptr = c_funloc(count_datablock_cb)
    call h5literate_f(data_block_itx_gid, H5_INDEX_NAME_F, H5_ITER_NATIVE_F, idx, funptr, ptr, return_value, hdferr)

end subroutine count_number_of_datablocks

subroutine data_iterate_datablocks(allData, data_block_itx_gid, iTx)
    ! Iterate though all the datablocks for a transmitter and read them into the allData structure
    ! To do this, we use the callback function read_datablock_func writen below.
    !
    ! We also need to pass information into the read_datablock_func for the functions that
    ! process specific data types. Thus we can use the datablock_iter_type which will
    ! contain information needed for those processing functions and contain a pointer to allData.

    use iso_c_binding, only : c_ptr, c_loc, c_funptr

    implicit none

    type (dataVectorMTX_t), target, intent(inout) :: allData
    integer (kind=HID_T), target, intent(in) :: data_block_itx_gid
    integer, target, intent(in) :: iTx
    
    type (datablock_iter_type), pointer :: datablock_info
    type (c_ptr) :: ptr
    type (c_funptr) :: funptr

    integer (kind=HSIZE_T) :: idx
    integer :: hdferr
    integer :: return_value

    allocate(datablock_info)

    datablock_info % allData => allData
    datablock_info % data_block_itx_gid = data_block_itx_gid
    datablock_info % iTx = iTx
    datablock_info % iDt = 1

    iDx = 0
    funptr = c_funloc(read_datablock_func)
    ptr = c_loc(datablock_info)

    call h5literate_f(data_block_itx_gid, H5_INDEX_NAME_F, H5_ITER_NATIVE_F, idx, funptr, ptr, &
            return_value, hdferr)

    allData % d(iTx) % allocated = .true.

    deallocate(datablock_info)

end subroutine data_iterate_datablocks

integer function read_datablock_func(loc_id, name, info, datablock_info_ptr) bind(c)
    ! We need to have this function here so that we can iterate through datablocks.
    ! data_iterate_datablocks() will use this function as a callback to process
    ! each datablock.

    use iso_c_binding, only : c_ptr, c_null_char, c_f_pointer

    implicit none

    integer (kind=HID_T), value:: loc_id
    character(len=1), dimension(1:10) :: name ! must have LEN=1 for bind(C) strings
    type (c_ptr) :: info
    type (c_ptr), value :: datablock_info_ptr

    ! Local Variables
    integer (kind=HID_T) :: block_group_id, parent_id
    integer :: i
    integer :: null_pos
    character(len=10) :: group_name_string, group_name_string_clean
    type (datablock_iter_type), pointer :: datablock_info

    call c_f_pointer(datablock_info_ptr, datablock_info)

    group_name_string = ''

    parent_id = datablock_info % data_block_itx_gid

    do i = 1, 10
        group_name_string(i:i) = name(i)(1:1)
    end do

    null_pos = index(group_name_string, c_null_char)

    group_name_string_clean = group_name_string(1:null_pos-1)
    call ModEM_HDF5_open_group(parent_id, group_name_string, block_group_id)

    select case(group_name_string_clean)
        case (trim(MT_IMPEDANCE_VAR_NAME))
            call process_mt_datablock(block_group_id, datablock_info, ImpType(trim(MT_IMPEDANCE_VAR_NAME)))
        case (trim(MT_TIPPER_VAR_NAME))
            call process_mt_datablock(block_group_id, datablock_info, ImpType(trim(MT_TIPPER_VAR_NAME)))
        case default
            write(0,*) 'This data type: ', trim(group_name_string), ' cannot yet be processed by ModEMs HDF5 module'
            write(0,*) 'Skipping it....'
    end select

    call ModEM_HDF5_close_group(block_group_id)

    datablock_info % iDt = datablock_info % idt + 1

    read_datablock_func = 0 ! Return code - 0 continues to the next iteartion (see H5literate_f)

    fileInfo(datablock_info % iTx, datablock_info % iDt) % defined = .true.
    fileInfo(datablock_info % iTx, datablock_info % iDt) % info_in_file = "# Read in by ModEM using HDF5 module"

end function read_datablock_func

subroutine process_mt_datablock(mt_group_id, datablock_info, MT_DATATYPE_NUM)

    implicit none

    integer (kind=HID_T) :: mt_group_id
    type (datablock_iter_type), pointer, intent(in) :: datablock_info
    integer, intent(in) :: MT_DATATYPE_NUM

    type(dataVectorMTX_t), pointer :: allData
    integer :: idt, iTx

    integer :: dataType, nComp, nSite
    logical :: isComplex

    integer (kind=HID_T) :: std_dset_id, value_dset_id, irx_dset_id
    integer (kind=HID_T) :: std_dspace_id, value_dspace_id, irx_dspace_id

    integer (kind=HSIZE_T) :: std_size, value_size, irx_size

    integer :: rank
    integer (kind=HSIZE_T), allocatable :: std_dims(:), std_max_dims(:)
    integer (kind=HSIZE_T), allocatable :: value_dims(:), value_max_dims(:)
    integer (kind=HSIZE_T), allocatable :: irx_dims(:), irx_max_dims(:)

    real (kind=prec), dimension(:,:), allocatable :: std, values
    integer, dimension(:), allocatable :: irx

    dataType = MT_DATATYPE_NUM
    isComplex = typeDict(datatype) % isComplex
    nComp = typeDict(datatype) % nComp

    allData => datablock_info % allData
    iTx = datablock_info % iTx
    idt = datablock_info % idt

    call ModEM_HDF5_open_dataset(mt_group_id, 'std', std_dset_id)
    call ModEM_HDF5_get_dataspace(std_dset_id, std_dspace_id)
    call ModEM_HDF5_get_dataspace_dims(std_dspace_id, std_dims, std_max_dims, rank)

    allocate(std(std_dims(1), std_dims(2)))
    call ModEM_HDF5_read_dataset(std_dset_id, H5T_NATIVE_DOUBLE, std)

    call ModEM_HDF5_close_dataspace(std_dspace_id)
    call ModEM_HDF5_close_dataset(std_dset_id)

    ! Open 'value'
    call ModEM_HDF5_open_dataset(mt_group_id, 'value', value_dset_id)
    call ModEM_HDF5_get_dataspace(value_dset_id, value_dspace_id)
    call ModEM_HDF5_get_dataspace_dims(value_dspace_id, value_dims, value_max_dims, rank)

    allocate(values(value_dims(1), value_dims(2)))
    call ModEM_HDF5_read_dataset(value_dset_id, H5T_NATIVE_DOUBLE, values)

    call ModEM_HDF5_close_dataspace(value_dspace_id)
    call ModEM_HDF5_close_dataset(value_dset_id)

    ! Open 'irx'
    call ModEM_HDF5_open_dataset(mt_group_id, 'irx', irx_dset_id)
    call ModEM_HDF5_get_dataspace(irx_dset_id, irx_dspace_id)
    call ModEM_HDF5_get_dataspace_dims(irx_dspace_id, irx_dims, irx_max_dims, rank)

    allocate(irx(irx_dims(1)))
    call ModEM_HDF5_read_dataset(irx_dset_id, H5T_NATIVE_INTEGER, irx)

    call ModEM_HDF5_close_dataspace(irx_dspace_id)
    call ModEM_HDF5_close_dataset(irx_dset_id)

    nSite = irx_dims(1)

    call create_datablock(nComp, nSite, allData % d(iTx) % data(iDt), isComplex, .true.)

    allData % d(iTx) % data (idt) % error(:,:) = std
    allData % d(iTx) % data (idt) % value(:,:) = values
    allData % d(iTx) % data (idt) % rx(:) = irx
    allData % d(iTx) % data (idt) % dataType = datatype
    allData % d(iTx) % data (idt) % tx = iTx
    allData % d(iTx) % data (idt) % txType = MT
    allData % d(iTx) % data (idt) % allocated = .true.

    deallocate(std)
    deallocate(values)
    deallocate(irx)

end subroutine process_mt_datablock

end submodule DataIO_HDF5
