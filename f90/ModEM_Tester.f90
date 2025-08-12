program ModEM_Tester

    use INVcore
    use main
    use ModEM_utils
    use ModelSpace

    implicit none

     character(80), parameter :: paramtype = "LOGE"
    type (userdef_control) :: cUserDef
    type(modelParam_t) :: m0, m1, m2, m3, m4, m5, m6, m7, m8, m9, m10
    type(grid_t) :: my_grid

    call ModEM_utils_init()
    call ModEM_log("We are running!")

    call create_grid(304, 674, 1, 47, my_grid)

    call ModEM_log("After grid creation")

    call create_modelParam(my_grid, paramtype, m0)
    
    call ModEM_log("After modelParam creation")
    call ModEM_memory_log_report("Before copy")

    m1 = m0

    call ModEM_memory_log_report("One")
    call CmSqrtMult(m1, m2)
    call ModEM_memory_log_report("two")
    call CmSqrtMult(m1, m2)
    call ModEM_memory_log_report("three")
    call CmSqrtMult(m1, m2)
    call ModEM_memory_log_report("four")
    call CmSqrtMult(m1, m2)
    call ModEM_memory_log_report("last")

end program ModEM_Tester
