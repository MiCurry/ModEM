submodule (ModelSpace:ModelSpaceIO) modelParam_IO_HDF5

#ifdef HDF5
use hdf5
use griddef
use ModEM_HDF5

implicit none

contains

	!******************************************************************
module subroutine write_modelParam_hdf5(m,cfile,comment)
    ! opens cfile on unit ioPrm, writes the object of
    ! type modelParam in HDF5/NetCDF4+ format, closes file

    type(modelParam_t), intent(in)	   :: m
    character(*), intent(in)             :: cfile
    character(*), intent(in), optional   :: comment

    integer(kind=HID_T) :: file_id

    if (gridCoords .eq. SPHERICAL) then
        write(0,*) 'Will be writing the model output in spherical HDF5 format...'
    else
        write(0,*) 'Will be writing the model output in cartesian HDF5 format...'
    end if

    ! Open file here
    call ModEM_HDF5_create_file(cfile, H5F_ACC_TRUNC_F, file_id)
    call write_geometry_hdf5(file_id, m)
    call write_gridSpacing_hdf5(file_id, m)
    call write_sigma_hdf5(file_id, m)

    ! Close file here
    call ModEM_HDF5_close_file(file_id)

end subroutine write_modelParam_hdf5

!******************************************************************
module subroutine read_modelParam_hdf5(grid,airLayers,m,cfile)

    ! opens cfile on unit ioPrm, reads the object of
    ! type modelParam in HDF5/NetCDF4+ format, closes file
    ! we can update the grid here, but the grid as an input is critical
    ! for setting the pointer to the grid in the modelParam

    type(grid_t), target, intent(inout)  :: grid
    type(airLayers_t), intent(inout)	   :: airLayers
    type(modelParam_t), intent(out)	   :: m
    character(*), intent(in)             :: cfile
    integer		                       :: istat
    ! local variables

    integer(kind=HID_T)                 :: file_id
    type(rscalar)                        :: ccond
    character(80)                        :: paramType=''
    integer                              :: Nx, Ny, NzEarth, i, j, k

    real (kind=8), dimension(:,:,:), allocatable :: Sigma ! Read data buffer for 3D Arrays

    integer (kind=HID_T) :: dset_id, dspace_id

    if (gridCoords .eq. SPHERICAL) then
        write(0,*) 'Will be reading the model input in spherical HDF5 format...'
    else
        write(0,*) 'Will be reading the model input in cartesian HDF5 format...'
    end if

    call ModEM_HDF5_open(cfile, file_id, H5F_ACC_RDONLY_F)

    ! First read grid geometry from HDF5 file
    call read_geometry_hdf5(file_id, grid, airLayers)

    paramType = ''

    ! Now, reopen HDF5 to read the conductivity
    write(0,*) cfile,' is open and ready to read electrical conductivity'

    ! Open Sigma dataset and get dataspace dimensions
    call ModEM_HDF5_open_dataset(file_id, "log10sigma", dset_id)

    ! The npoints will be a differents size for each dataset for sigma use ny,nx,and nz
    call ModEM_HDF5_get_dataspace(dset_id, dspace_id)

    !allocate the space for the local variable
    Ny = grid%ny
    Nx = grid%nx
    NzEarth = grid%nzEarth
    allocate(Sigma(Ny,Nx,NzEarth), STAT = istat)

    ! Read  grid geometries from Hdf5
    call ModEM_HDF5_read_dataset(dset_id, H5T_NATIVE_DOUBLE, Sigma)

    call ModEM_HDF5_close_file(file_id)

    ! Set ParamType to be used when reading conductivity values
    paramType = 'LOG10'

    ! Create and read in the resistivity values
    call create_rscalar(grid,ccond,CELL_EARTH)

    ! Save the conductivity
    do i=1,Nx
        do j=1,Ny
            do k=1,NzEarth
                ccond%v(i,j,k) = Sigma(j,i,k)
            end do
        end do
    end do
    write(0,*) ccond%v(1:10,1,1)

    ! Finally create the model parameter
    call create_modelParam(grid,paramType,m,ccond)

    ! In ModelSpace, save the user paramType for output
    userParamType = paramType

    ! ALWAYS convert modelParam to natural log for computations
    paramType = 'LOGE'
    call setType_modelParam(m,paramType)

    ! now done with the rscalars, so deallocate
    deallocate(Sigma, STAT = istat)
    call deall_rscalar(ccond)

end subroutine read_modelParam_hdf5

subroutine write_geometry_hdf5(file_id, m)

    integer(kind=HID_T), intent(in)      :: file_id
    type(modelParam_t), intent(in)	     :: m

    type(grid_t)                         :: grid
    integer                              :: Nx, Ny, NzEarth

    integer (kind=HID_T) :: root_group_id
    integer (kind=HID_T) :: x_dspace_id, y_dspace_id, z_dspace_id
    integer (kind=HID_T) :: x_dset_id, y_dset_id, z_dset_id

    grid = m%grid

    Nx=grid%nx !this defines the length of the data array
    Ny=grid%ny
    NzEarth=grid%nz - grid%nzAir

    ! Write grid geometry definitions
    call ModEM_HDF5_open_group(file_id, "/", root_group_id)

    ! Assign attribute values
    call ModEM_HDF5_add_attr(root_group_id, 'model_origin_x', -grid%ox)
    call ModEM_HDF5_add_attr(root_group_id, 'model_origin_y', -grid%oy)
    call ModEM_HDF5_add_attr(root_group_id, 'model_origin_z', -grid%oz)
    ! Should the below be grid % dely or grid % delz?
    call ModEM_HDF5_add_attr(root_group_id, 'model_rotation_angle', 0.0_prec)
    call ModEM_HDF5_add_attr(root_group_id, 'model_primary_coords', 'xy')
    call ModEM_HDF5_add_attr(root_group_id, 'model_rotation_units', 'degrees')

    ! write the linear data array for NX
    call ModEM_HDF5_create_dataspace(rank(grid % xCenter), (/size(grid % xCenter, kind=HSIZE_T)/), x_dspace_id)
    call ModEM_HDF5_create_dataset(root_group_id, 'x', H5T_NATIVE_DOUBLE, x_dspace_id, x_dset_id)
    call ModEM_HDF5_write_dataset(x_dset_id, H5T_NATIVE_DOUBLE, grid%xCenter)

    !write attributes for x dataset
    call ModEM_HDF5_add_attr(x_dset_id, 'CLASS', 'DIMENSION_SCALE')
    call ModEM_HDF5_add_attr(x_dset_id, 'NAME', 'x')
    call ModEM_HDF5_add_attr(x_dset_id, 'long_name', 'Latitude; positive north')
    call ModEM_HDF5_add_attr(x_dset_id, 'standard_name', 'x')
    call ModEM_HDF5_add_attr(x_dset_id, 'units', 'meters')

    call ModEM_HDF5_close_dataset(x_dset_id)
    call ModEM_HDF5_close_dataspace(x_dspace_id)

    ! write the linear data array for NY
    call ModEM_HDF5_create_dataspace(1, (/int(ny, kind=HSIZE_T)/), y_dspace_id)
    call ModEM_HDF5_create_dataset(root_group_id, 'y', H5T_NATIVE_DOUBLE, y_dspace_id, y_dset_id)
    call ModEM_HDF5_write_dataset(y_dset_id, H5T_NATIVE_DOUBLE, grid%yCenter)

    !write attributes for y dataset
    call ModEM_HDF5_add_attr(y_dset_id, 'CLASS', 'DIMENSION_SCALE')
    call ModEM_HDF5_add_attr(y_dset_id, 'NAME', 'y')
    call ModEM_HDF5_add_attr(y_dset_id, 'long_name', 'Latitude; positive east')
    call ModEM_HDF5_add_attr(y_dset_id, 'standard_name', 'y')
    call ModEM_HDF5_add_attr(y_dset_id, 'units', 'meters')

    call ModEM_HDF5_close_dataset(y_dset_id)
    call ModEM_HDF5_close_dataspace(y_dspace_id)

    ! write the linear data array for NZ
    call ModEM_HDF5_create_dataspace(1, (/int(nzEarth, kind=HSIZE_T)/), z_dspace_id)
    call ModEM_HDF5_create_dataset(root_group_id, 'z', H5T_NATIVE_DOUBLE, z_dspace_id, z_dset_id)
    call ModEM_HDF5_write_dataset(z_dset_id, H5T_NATIVE_DOUBLE, grid%zCenter(grid%NzAir+1:NzEarth+grid%NzAir))

    !write attributes for z dataset
    call ModEM_HDF5_add_attr(z_dset_id, 'CLASS', 'DIMENSION_SCALE')
    call ModEM_HDF5_add_attr(z_dset_id, 'NAME', 'z')
    call ModEM_HDF5_add_attr(z_dset_id, 'long_name', 'depth below earth surface')
    call ModEM_HDF5_add_attr(z_dset_id, 'positive', 'down')
    call ModEM_HDF5_add_attr(z_dset_id, 'units', 'meters')

    call ModEM_HDF5_close_dataset(z_dset_id)
    call ModEM_HDF5_close_dataspace(z_dspace_id)
    call ModEM_HDF5_close_group(root_group_id)

end subroutine write_geometry_hdf5

!******************************************************************
!write code for the nodes
subroutine write_gridSpacing_hdf5(file_id, m)

    integer(kind=HID_T), intent(in)       :: file_id 
    type(modelParam_t), intent(in)	      :: m

    ! local variables
    type(grid_t)                          :: grid
    type(rscalar)                         :: ccond
    character(80)                         :: paramType

    integer                               :: Nx, Ny, NzEarth
    integer (kind=HID_T) :: grid_spacing_group_id
    integer (kind=HID_T) :: dx_dset_id, dy_dset_id, dz_dset_id
    integer (kind=HID_T) :: dx_dspace_id, dy_dspace_id, dz_dspace_id 

    paramType = userParamType

    call getValue_modelParam(m,paramType,ccond)

    grid = ccond%grid
    Nx=grid%nx !this defines the length of the data array
    Ny=grid%ny
    NzEarth=grid%nz - grid%nzAir

    call ModEM_HDF5_create_group(file_id, "GridSpacing", grid_spacing_group_id)
    ! write the linear data array for NodesX
    call ModEM_HDF5_create_dataspace(rank(grid % dx), (/size(grid % dx, kind=HSIZE_T)/), dx_dspace_id)
    call ModEM_HDF5_create_dataset(grid_spacing_group_id, 'Dx', H5T_NATIVE_DOUBLE, dx_dspace_id, dx_dset_id)
    call ModEM_HDF5_write_dataset(dx_dset_id, H5T_NATIVE_DOUBLE, grid % dx)
    call ModEM_HDF5_close_dataset(dx_dset_id)
    call ModEM_HDF5_close_dataspace(dx_dspace_id)

    ! write the linear data array for NodesY
    call ModEM_HDF5_create_dataspace(rank(grid % dy), (/size(grid % dy, kind=HSIZE_T)/), dy_dspace_id)
    call ModEM_HDF5_create_dataset(grid_spacing_group_id, 'Dy', H5T_NATIVE_DOUBLE, dy_dspace_id, dy_dset_id)
    call ModEM_HDF5_write_dataset(dy_dset_id, H5T_NATIVE_DOUBLE, grid % dy)
    call ModEM_HDF5_close_dataset(dy_dset_id)
    call ModEM_HDF5_close_dataspace(dy_dspace_id)

    ! write the linear data array for NodesZ
    call ModEM_HDF5_create_dataspace(rank(grid % dz), (/size(grid % dz, kind=HSIZE_T)/), dz_dspace_id)
    call ModEM_HDF5_create_dataset(grid_spacing_group_id, 'Dz', H5T_NATIVE_DOUBLE, dz_dspace_id, dz_dset_id)
    call ModEM_HDF5_write_dataset(dz_dset_id, H5T_NATIVE_DOUBLE, grid % dz)
    call ModEM_HDF5_close_dataset(dz_dset_id)
    call ModEM_HDF5_close_dataspace(dz_dspace_id)
    call ModEM_HDF5_close_group(grid_spacing_group_id)

end subroutine write_gridSpacing_hdf5

!******************************************************************
subroutine write_sigma_hdf5(file_id, m)
    integer(kind=HID_T), intent(in)                  :: file_id
    type(modelParam_t), intent(in)	     :: m

    ! local variables
    type(grid_t)                          :: grid
    type(rscalar)                         :: ccond
    character(80)                         :: paramType =''

    integer                               :: Nx, Ny, NzEarth
    CHARACTER(LEN=10), parameter :: prop = "log10sigma"

    integer (kind=HID_T) :: sigma_dset_id, sigma_dspace_id

    ! Convert modelParam to natural log or log10 for output
    !paramType = userParamType
    paramType = 'LOG10'
    call getValue_modelParam(m,paramType,ccond)

    grid = ccond%grid
    Nx=grid%nx !this defines the length of the data array
    Ny=grid%ny
    NzEarth=grid%nz - grid%nzAir

    call ModEM_HDF5_create_dataspace(3, (/int(Ny, kind=HSIZE_T), int(Nx, kind=HSIZE_T), int(NzEarth, kind=HSIZE_T)/), sigma_dspace_id)
    call ModEM_HDF5_create_dataset(file_id, prop, H5T_NATIVE_DOUBLE, sigma_dspace_id, sigma_dset_id)
    ! Assign attribute values
    call ModEM_HDF5_add_attr(sigma_dset_id, 'display_name', 'log(10) electrical conductivity, in S/m')
    call ModEM_HDF5_add_attr(sigma_dset_id, 'long_name', 'electrical conductivity')
    call ModEM_HDF5_add_attr(sigma_dset_id, 'units', 'S/m')
    call ModEM_HDF5_add_attr(sigma_dset_id, 'missing_value', 99999.0_prec)
    ! Write the resistivity
    call ModEM_HDF5_write_dataset(sigma_dset_id, H5T_NATIVE_DOUBLE, ccond%v)

    call ModEM_HDF5_close_dataset(sigma_dset_id)
    call ModEM_HDF5_close_dataspace(sigma_dspace_id)

end subroutine write_sigma_hdf5

!******************************************************************
subroutine read_geometry_hdf5(file_id, grid, airlayers)

    integer(kind=HID_T), intent(in)                 :: file_id
    type (grid_t) , intent(inout)		:: grid
    type(airLayers_t), intent(inout)    :: airLayers

    integer                                 :: istat

    REAL(KIND=8), DIMENSION(:), allocatable :: xctr, yctr, zctr !Read data buffers for 1D Arrays
    INTEGER(HSIZE_T)                        :: nx, ny, nz
    integer                                 :: grid_x, grid_y, grid_z, NzAir, i
    real(8)                                 :: origin_x, origin_y, origin_z
    real(8)                                 :: origin(3)

    integer (kind=HID_T) :: x_dset_id, y_dset_id, z_dset_id
    integer (kind=HID_T) :: x_dspace_id, y_dspace_id, z_dspace_id

    integer (kind=HID_T) :: group_id

    !!!!!!!!! READ X DATA !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    ! Open X dataset and get dataspace dimensions
    call ModEM_HDF5_open_dataset(file_id, "x", x_dset_id)

    ! The npoints will be a differents size for each dataset
    call ModEM_HDF5_get_dataspace(x_dset_id, x_dspace_id)
    call ModEM_HDF5_get_dataspace_size(x_dspace_id, nx)
    !allocate the space for the local variable
    allocate(xctr(nx), STAT = istat)

    ! Read  grid geometries from Hdf5
    call ModEM_HDF5_read_dataset(x_dset_id, H5T_NATIVE_DOUBLE, xctr)
    call ModEM_HDF5_close_dataset(x_dset_id)
    call ModEM_HDF5_close_dataspace(x_dspace_id)

    call ModEM_HDF5_open_dataset(file_id, "y", y_dset_id)

    ! The npoints will be a differents size for each dataset
    call ModEM_HDF5_get_dataspace(y_dset_id, y_dspace_id)
    call ModEM_HDF5_get_dataspace_size(y_dspace_id, ny)

    !allocate the space for the local variable
    allocate(yctr(ny), STAT = istat)

    ! Read  grid geometries from Hdf5
    call ModEM_HDF5_read_dataset(y_dset_id, H5T_NATIVE_DOUBLE, yctr)
    call ModEM_HDF5_close_dataset(y_dset_id)
    call ModEM_HDF5_close_dataspace(y_dspace_id)

    !!!!!!!!! READ Z DATA !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    ! Open Z dataset and get dataspace dimensions
    call ModEM_HDF5_open_dataset(file_id, "z", z_dset_id)

    ! The npoints will be a differents size for each dataset
    call ModEM_HDF5_get_dataspace(z_dset_id, z_dspace_id)
    call ModEM_HDF5_get_dataspace_size(z_dspace_id, nz)

    !allocate the space for the local variable
    allocate(zctr(nz), STAT = istat)

    ! Read  grid geometries from Hdf5
    call ModEM_HDF5_read_dataset(z_dset_id, H5T_NATIVE_DOUBLE, zctr)
    call ModEM_HDF5_close_dataset(z_dset_id)
    call ModEM_HDF5_close_dataspace(z_dspace_id)

    ! Setup grid
    grid_x = nx
    grid_y = ny
    grid_z = nz
    nzAir = airLayers%Nz
    call create_grid(grid_x, grid_y, nzAir, grid_z, grid)

    !Read Origin Attributes from the file to be used in X, Y, and Z centers
    call ModEM_HDF5_open_group(file_id, "/", group_id)
    
    call ModEM_HDF5_read_attr(group_id, 'model_origin_x', origin_x)
    call ModEM_HDF5_read_attr(group_id, 'model_origin_y', origin_y)
    call ModEM_HDF5_read_attr(group_id, 'model_origin_z', origin_z)
    
    call ModEM_HDF5_close_group(group_id)

    origin(1) = -origin_x
    origin(2) = -origin_y
    origin(3) = -origin_z

    origin_x = -origin_x
    origin_y = -origin_y
    origin_z = -origin_z
    ! x = origin(1)
    do  i = 1, size(xctr)
        grid%dx(i) = 2*(xctr(i)-origin_x)
        origin_x = origin_x + grid%dx(i)
    end do
    ! y= origin(2)
    do  i = 1, size(yctr)
        grid%dy(i) = 2*(yctr(i)-origin_y)
        origin_y = origin_y + grid%dy(i)
    end do
    ! z = origin(3)
    do  i = 1, size(zctr)
        grid%dz(nzAir + i) = 2*(zctr(i)-origin_z)
        origin_z = origin_z + grid%dz(nzAir +i)
    end do

    ! Finally, insert correct air layers in the grid and run setup_grid
    call setup_airlayers(airLayers,grid)
    call update_airlayers(grid,nzAir,airLayers%Dz)
    call setup_grid(grid, origin)

end subroutine read_geometry_hdf5

#endif

end submodule ModelParam_IO_HDF5 
