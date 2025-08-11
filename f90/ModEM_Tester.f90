program ModEM_Tester


    use mpi

    use INVcore
    use Main_MPI
    use main
    use ModEM_utils
    use ModelSpace

    implicit none

    type (userdef_control) :: cUserDef
    type(modelParam_t) :: m0, m1, m2, m3, m4, m5, m6, m7, m8, m9, m10
    type(grid_t) :: my_grid

    call  constructor_MPI

    call ModEM_utils_init()
    call ModEM_log("We are running!")

    call create_grid(200, 200, 12, 200, my_grid)

    call ModEM_log("After grid creation")

    call create_modelParam(my_grid, 'LOGE', m0)
    
    call ModEM_log("After modelParam creation")
    call ModEM_memory_log_report("Before copy")

    m1 = m0

    call ModEM_memory_log_report("After copy")

    call CmSqrtMult(m1, m2)
    call CmSqrtMult(m1, m2)
    call CmSqrtMult(m1, m2)
    call CmSqrtMult(m1, m2)
    call CmSqrtMult(m1, m2)
    call CmSqrtMult(m1, m2)
    call CmSqrtMult(m1, m2)
    call CmSqrtMult(m1, m2)
    call CmSqrtMult(m1, m2)
    call CmSqrtMult(m1, m2)
    call CmSqrtMult(m1, m2)
    call CmSqrtMult(m1, m2)
    call CmSqrtMult(m1, m2)
    call CmSqrtMult(m1, m2)
    call CmSqrtMult(m1, m2)
    call CmSqrtMult(m1, m2)
    call CmSqrtMult(m1, m2)
    call CmSqrtMult(m1, m2)
    call CmSqrtMult(m1, m2)
    call CmSqrtMult(m1, m2)
    call CmSqrtMult(m1, m2)
    call CmSqrtMult(m1, m2)
    call CmSqrtMult(m1, m2)
    call CmSqrtMult(m1, m2)
    call CmSqrtMult(m1, m2)
    call CmSqrtMult(m1, m2)
    call CmSqrtMult(m1, m2)

    call ModEM_memory_log_report("After CmSqrtMult")

end program ModEM_Tester
