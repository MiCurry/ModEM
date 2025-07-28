module ModEM_memory

! ModEM_memory - Call and log maxrss from getrusage (man getrusage)
!
! This module is intended as a diagnostic module. Because of the way ModEM is currently built,
! this module is not intended for release. However, if the build process is eventually upgraded
! so that the fmkmf.pl includes C files (or we do manual makefiles).
!
! To compile and be able to use this module with ModEM, perform the following steps:
!
! 1. Set the environment variable CC to your desired C compiler: `export CC=mpicc`
!
! 2. Add the following compilation steps to your makefile:
!
!   $(OBJDIR)/ModEM_getrusage.o:
!	    $(CC) -c UTILS/ModEM_getrusage.c -o $(OBJDIR)/ModEM_getrusage.o
!   $(OBJDIR)/ModEM_memory.o:UTILS/ModEM_memory.f90  $(OBJDIR)/ModEM_getrusage.o
!	    $(F90) -c $(MODULE) $(FFLAGS) $(MPIFLAGS) UTILS/ModEM_memory.f90 -o $(OBJDIR)/ModEM_memory.o
!
! 3. Add both `$(OBJDIR)/ModEM_getrusage.o` and `$(OBJDIR)/ModEM_memory.o` to `OBJ` (the big linking line on line ~56).
!
! 4. You'll manually need to add `$(OBJDIR)/ModEM_getrusage.o` to the prerequisite of any modules/targets
! where you want to call ModEM_memory routines.
!
! 5. Ensure you call ModEM_memory_init and (less critically) ModEM_memory_finalize.
!
    use utilities
    use iso_c_binding, only : c_long
#ifdef MPI
    use mpi
#endif

    implicit none

    private

    character (len=256) :: log_fname
    integer :: task_id
    integer :: log_fid

    public ModEM_memory_init
    public ModEM_memory_finalize
    public ModEM_memory_print_report
    public ModEM_memory_log_report
    public ModEM_memory_add_n_log_all
    public ModEM_log


contains

    subroutine get_new_unit(unit)

        implicit none

        integer :: unit

    end subroutine get_new_unit

    subroutine ModEM_memory_create_log_fname()

    end subroutine

    subroutine ModEM_memory_init()

        implicit none

        character(len=*), parameter :: LOG_FNAME_MEMORY_FMT = "(A,i4.4,A)"
        
        task_id = ModEM_mpi_get_task_rank()

        ! Create log filename
        write(log_fname, LOG_FNAME_MEMORY_FMT) "log.memory.", task_id, ".out"
        open(newunit=log_fid, file=log_fname)

    end subroutine ModEM_memory_init

    subroutine ModEM_memory_finalize()

        implicit none

        write(log_fid, *) "End of run"
        close(log_fid)

    end subroutine ModEM_memory_finalize

    subroutine ModEM_memory_get_maxrss(maxrss_bytes)

        implicit none

        integer, intent(out) :: maxrss_bytes
        integer (c_long) :: maxrss_bytes_c

        interface
            subroutine get_maxrss(maxrss_bytes) bind(c)
                use iso_c_binding, only : c_long
                integer (c_long), intent(out) :: maxrss_bytes
            end subroutine get_maxrss
        end interface

        task_id = ModEM_mpi_get_task_rank()

        call get_maxrss(maxrss_bytes_c)
        maxrss_bytes = maxrss_bytes_c

    end subroutine ModEM_memory_get_maxrss

    subroutine ModEM_memory_print_report()

        implicit none

        integer :: maxrss

        call ModEM_memory_get_maxrss(maxrss)

        write(0, "(A,i4.1,A,i16.1)") "Task: ", task_id, " Max RSS: ", maxrss
        write(0,*) "I AM: ", task_id

    end subroutine ModEM_memory_print_report

    subroutine ModEM_memory_convert_maxrss(maxrss_bytes, maxrss_kb, maxrss_mb, maxrss_gb)

        implicit none

        integer :: maxrss_bytes
        real, intent(out) :: maxrss_kb
        real, intent(out) :: maxrss_mb
        real, intent(out) :: maxrss_gb

        maxrss_kb = maxrss_bytes / 1000.0
        maxrss_mb = maxrss_kb / 1000.0
        maxrss_gb = maxrss_mb / 1000.0

    end subroutine ModEM_memory_convert_maxrss

    subroutine ModEM_memory_log_report(message)

        implicit none

        character (len=*), intent(in) :: message
        integer :: maxrss
        real :: maxrss_bytes, maxrss_kb, maxrss_mb, maxrss_gb

        call ModEM_memory_do_log(message, maxrss)

    end subroutine ModEM_memory_log_report

    subroutine ModEM_log(message)

        character (len=*), intent(in) :: message

        write(log_fid, *) trim(message)
        call flush(log_fid)

    end subroutine ModEM_log

    subroutine ModEM_memory_do_log(message, maxrss)

        implicit none

        character (len=*), intent(in) :: message
        integer, intent(in) :: maxrss
        character (len=*), parameter :: LOG_MSG_FMT = "(A, A, F18.1, A, F18.1, A, F18.1, A)"
        real :: maxrss_bytes, maxrss_kb, maxrss_mb, maxrss_gb

        call ModEM_memory_convert_maxrss(maxrss, maxrss_kb, maxrss_mb, maxrss_gb)
        write(log_fid, LOG_MSG_FMT) trim(message), ", ", maxrss_kb, ' kb, ', maxrss_mb, ' mb, ', maxrss_gb, ' gb'
        call flush(log_fid)

    end subroutine ModEM_memory_do_log

    subroutine ModEM_memory_add_n_log_all(message)

        use mpi 

        implicit none

        character(len=*), intent(in) :: message

        character(len=125) :: max_rss_message

        integer :: maxrss, sum_maxrss
        integer :: ntasks
        integer :: comm_size, rank, ierr


        call ModEM_memory_get_maxrss(maxrss)

        call MPI_comm_size(MPI_COMM_WORLD, comm_size, ierr)
        if (ierr /= MPI_SUCCESS) then
            call errStop('Unable to get comm size for MPI')
            !call MPI_abort(MPI_COMM_WORLD, error_code, ierr)
        end if

        call MPI_comm_rank(MPI_COMM_WORLD, rank, ierr)
        if (ierr /= MPI_SUCCESS) then
            call errStop('Unable to get rank in MPI')
            !call MPI_abort(MPI_COMM_WORLD, error_code, ierr)
        end if

        write(0,*) "I am calling MPI_reduce - ", rank
        call MPI_reduce(maxrss, sum_maxrss, 1, MPI_INT, MPI_SUM, 0, MPI_COMM_WORLD, ierr)
        write(0,*) "After MPI reduce - ", rank

        write(max_rss_message, '(A,I10.1)') 'Sum Maxrss: ', sum_maxrss

        if (rank == 0) then
            call ModEM_memory_log_report(max_rss_message)
        end if

    end subroutine ModEM_memory_add_n_log_all

    function ModEM_mpi_get_task_rank() result(rank)

        implicit none 

        integer :: rank 
#ifdef MPI
        integer :: ierr
#endif

#ifdef MPI
        call MPI_comm_rank(MPI_COMM_WORLD, rank, ierr)
        if (ierr /= MPI_SUCCESS) then
           call ModEM_mpi_error_abort("Error getting rank", ierr)
        end if
#else 
        rank = 0
#endif
    end function ModEM_mpi_get_task_rank

    subroutine ModEM_mpi_error_abort(message, ierr)

        implicit none

        character (*), intent(in) :: message
        integer, intent(in) :: ierr

        character(len=256) :: error_string

#ifdef MPI
        integer :: ierr_local
        integer :: error_string_length
#endif

#ifdef MPI
        call MPI_Error_string(ierr, error_string, error_string_length, ierr_local)
        if (ierr_local /= MPI_SUCCESS) then
            call errStop("There was an issue when calling MPI_Error_string in ModEM_mpi_error_abort with ierr")
        end if
        write(0, '(a9)') 'Error:', trim(message)
#else
        error_string = message
#endif

        call errStop(error_string)

    end subroutine ModEM_mpi_error_abort

end module ModEM_memory
