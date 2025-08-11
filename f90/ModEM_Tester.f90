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

    call CmSqrtMult(sigma0, m_minus_m0)
    call CmSqrtMult(sigma0, m_minus_m0)
    call CmSqrtMult(sigma0, m_minus_m0)
    call CmSqrtMult(sigma0, m_minus_m0)
    call CmSqrtMult(sigma0, m_minus_m0)
    call CmSqrtMult(sigma0, m_minus_m0)
    call CmSqrtMult(sigma0, m_minus_m0)
    call CmSqrtMult(sigma0, m_minus_m0)
!    call deall_modelParam(m_minus_m0)
    call ModEM_memory_log_report("2")
    call ModEM_log("m_minus_0 allocated? - $l", logicArgs=(/m_minus_m0 % allocated/))

    call CmSqrtMult(sigma0, m_minus_m0)
!    call deall_modelParam(m_minus_m0)
    call ModEM_log("m_minus_0 allocated? - $l", logicArgs=(/m_minus_m0 % allocated/))

    call ModEM_memory_log_report("3")

    call CmSqrtMult(sigma0, m_minus_m0)
!    call deall_modelParam(m_minus_m0)
    call ModEM_memory_log_report("4")
    call ModEM_log("m_minus_0 allocated? - $l", logicArgs=(/m_minus_m0 % allocated/))

    call CmSqrtMult(sigma0, m_minus_m0)
!    call deall_modelParam(m_minus_m0)
    call ModEM_memory_log_report("5")
    call ModEM_log("m_minus_0 allocated? - $l", logicArgs=(/m_minus_m0 % allocated/))

    call CmSqrtMult(sigma0, m_minus_m0)
!    call deall_modelParam(m_minus_m0)
    call ModEM_memory_log_report("6")
    call ModEM_log("m_minus_0 allocated? - $l", logicArgs=(/m_minus_m0 % allocated/))

    call CmSqrtMult(sigma0, m_minus_m0)
!    call deall_modelParam(m_minus_m0)
    call ModEM_memory_log_report("7")
    call ModEM_log("m_minus_0 allocated? - $l", logicArgs=(/m_minus_m0 % allocated/))

    call CmSqrtMult(sigma0, m_minus_m0)
!    call deall_modelParam(m_minus_m0)
    call ModEM_memory_log_report("8")
    call ModEM_log("m_minus_0 allocated? - $l", logicArgs=(/m_minus_m0 % allocated/))

    call CmSqrtMult(sigma0, m_minus_m0)
!    call deall_modelParam(m_minus_m0)
    call ModEM_memory_log_report("9")
    call ModEM_log("m_minus_0 allocated? - $l", logicArgs=(/m_minus_m0 % allocated/))

!    call deall_modelParam(m_minus_m0)
    call ModEM_memory_log_report("10")
    call ModEM_log("m_minus_1 allocated? - $l", logicArgs=(/m_minus_m0 % allocated/))

end program ModEM_Tester
