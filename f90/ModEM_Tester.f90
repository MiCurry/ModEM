program ModEM_Tester


    use mpi

    use INVcore
    use Main_MPI
    use main
    use ModEM_utils
    use ModelSpace

    implicit none

    type (userdef_control) :: cUserDef
    type(modelParam_t)                           :: mHat, m_minus_m0

    type(grid_t) :: my_grid

    call  constructor_MPI

    call ModEM_utils_init()

    call ModEM_log("We are running!")
    call parseArgs('Mod3DMT',cUserDef)  

    call ModEM_log("After parse args")

    call initGlobalData(cUserDef)

    write(0,*) "Grid: ", grid % nx, grid % ny, grid % nz
    write(0,*) "Sigma0: ", sigma0 % Nx, sigma0 % Ny, sigma0 % NzEarth
    write(0,*) "Sigma1: ", sigma1 % Nx, sigma1 % Ny, sigma1 % NzEarth

    call ModEM_memory_log_report("1")


end program ModEM_Tester
