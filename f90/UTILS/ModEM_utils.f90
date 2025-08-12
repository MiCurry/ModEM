module ModEM_utils

! ModEM Memory Routines - Call and log maxrss from getrusage (man getrusage)
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
!
!   $(OBJDIR)/ModEM_utils.o:UTILS/ModEM_utils.f90  $(OBJDIR)/ModEM_getrusage.o $(OBJDIR)/utilities.o
!	    $(F90) -c $(MODULE) $(FFLAGS) $(MPIFLAGS) UTILS/ModEM_utils.f90 -o $(OBJDIR)/ModEM_utils.o
!
! 3. Add both `$(OBJDIR)/ModEM_getrusage.o` and `$(OBJDIR)/ModEM_utils.o` to `OBJ` (the big linking line on line ~56).
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

    integer :: rank
    integer :: comm_size
    integer :: log_fid
    integer :: task_id

! MPAS Defined RKINS
   integer, parameter :: R4KIND = selected_real_kind(6)
   integer, parameter :: R8KIND = selected_real_kind(12)
#ifdef SINGLE_PRECISION
   integer, parameter :: RKIND  = selected_real_kind(6)
#else
   integer, parameter :: RKIND  = selected_real_kind(12)
#endif

   integer, parameter :: I8KIND = selected_int_kind(18)

   integer, parameter :: StrKIND = 512
   integer, parameter :: ShortStrKIND = 64

    type :: Util_Struct
        integer :: rank
        integer :: comm_size
        integer :: log_fid
    end type Util_Struct

    private

    public ModEM_utils_init
    public ModEM_utils_get_info
    public ModEM_log
    public ModEM_memory_get_maxrss
    public ModEM_memory_print_report
    public ModEM_memory_convert_maxrss
    public ModEM_memory_log_report
    public ModEM_memory_do_log
#ifdef MPI
    public ModEM_memory_add_n_log_all
#endif


contains

   subroutine ModEM_utils_init()

       implicit none

       integer :: ierr, error_code
       character(len=*), parameter :: log_str_fmt = '(A,I4.4,A)'
       character(len=40) :: log_fname

       comm_size = 1
       rank = 0

#ifdef MPI
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
#endif


       write(log_fname, log_str_fmt) 'log.', rank, '.modem.out'

       open(newunit=log_fid, file=log_fname, status='replace')
       write(log_fid, '(A,I4.4,A,A)') "Log Initalized - ", rank, ' - ', log_fname
       call flush(log_fid)
   
   end Subroutine ModEM_utils_init

   subroutine ModEM_utils_get_info(info)

       implicit none

       type (Util_Struct), intent(out) :: info

       info % rank = rank
       info % comm_size = comm_size
       info % log_fid = log_fid

   end subroutine ModEM_utils_get_info

   function ModEM_utils_get_my_rank() result(rank_lcl)

       implicit none

       integer :: rank_lcl

       rank_lcl = rank

   end function

   function ModEM_utils_get_comm_size() result(comm_size_lcl)

       implicit none

       integer :: comm_size_lcl

       comm_size_lcl = comm_size

   end function ModEM_utils_get_comm_size

   subroutine ModEM_log(msg, intArgs, realArgs, logicArgs, fid) 

       implicit none

       character(len=*), intent(in) :: msg
       integer, dimension(:), intent(in), optional :: intArgs  !< Input: integer variable values to insert into message
       real(kind=RKIND), dimension(:), intent(in), optional :: realArgs  !< Input: real variable values to insert into message
            !< Input: exponential notation variable values to insert into message
       logical, dimension(:), intent(in), optional :: logicArgs  !< Input: logical variable values to insert into message
       integer, intent(in), optional :: fid 

       integer :: lcl_fid
       character(len=strKind) :: messageExpanded !< message after expansion of $ variable insertions

       if (present(fid)) then
           lcl_fid = fid
       else
           lcl_fid = log_fid
       end if 

       call log_expand_string(msg, messageExpanded, intArgs, logicArgs, realArgs)

       write(log_fid,*) trim(messageExpanded)
       call flush(log_fid)

   end subroutine ModEM_log

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

       call ModEM_memory_get_maxrss(maxrss)

       call ModEM_memory_do_log(message, maxrss)

   end subroutine ModEM_memory_log_report

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

#ifdef MPI
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
#endif

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

   !-----------------------------------------------------------------------
   !  routine log_expand_string
   !
   !> \brief This is a utility routine that inserts formatted variables into a string.
   !> \author Matt Hoffman
   !> \date   02/20/2017
   !> \details This routine inserts formatted variables into a string.
   !>   The variables to be expanded are represented with a '$' symbol followed
   !>   by one of these indicators:
   !>   $i -> integer, formatted to be length of integer
   !>   $l -> logical, fomatted as 'T' or 'F'
   !>   $r -> real,  formatted as 9 digits of precision for SP mode, 17 for DP mode
   !>                Floats are formatted using 'G' format which is smart about
   !>                displaying as a decimal or scientific notation based on the value.
   !>   The variable values to expand are supplied as optional arguments to this
   !>   routine.  The substitution indicators are expanded as they are encountered.
   !>   Thus, extra variable values will be ignored.  If the supplied variable values
   !>   run out before the $ expansion indicators are all replaced, the remaining
   !>   expansions will be filled with a fill value ('**').  The fill value is also
   !>   used if the expansion indicator is of an unknown type, where the valid types
   !>   are $i, $l, $r.
   !>   If the user prefers more specific formatting, they have to do it external
   !>   to this routine in a local string variable.  Similarly, character variables
   !>   can be handled by the string concatenation command (//).
   !>   This routine is based off of mpas_expand_string.
   !-----------------------------------------------------------------------
   subroutine log_expand_string(inString, outString, intArgs, logicArgs, realArgs)

      implicit none

      !-----------------------------------------------------------------
      ! input variables
      !-----------------------------------------------------------------
      character (len=*), intent(in) :: inString  !< Input: message to be expanded

      integer, dimension(:), intent(in), optional :: intArgs
         !< Input, Optional: array of integer variable values to be used in expansion
      logical, dimension(:), intent(in), optional :: logicArgs
         !< Input, Optional: array of logical variable values to be used in expansion
      real(kind=RKIND), dimension(:), intent(in), optional :: realArgs
         !< Input, Optional: array of real variable values to be used in expansion

      !-----------------------------------------------------------------
      ! input/output variables
      !-----------------------------------------------------------------

      !-----------------------------------------------------------------
      ! output variables
      !-----------------------------------------------------------------
      character (len=StrKIND), intent(out) :: outString  !< Output: expanded version of input message after expansion

      !-----------------------------------------------------------------
      ! local variables
      !-----------------------------------------------------------------
      integer :: i, curLen
      integer :: nInts, nLogicals, nReals, nExps  !< the length of the variable arrays passed in
      integer :: iInt, iLogical, iReal !< Counter for the current index into each variable array
      character (len=ShortStrKIND) :: realFormat  !< Format string to create to use for writing real variables to log file
      integer :: realPrecision !< precision of a real variable

      character (len=ShortStrKIND) :: varPart
      character (len=ShortStrKIND) :: errVarPart ! string to use if variable expansion fails
      logical :: charExpand

      ! Initialize the current index for each variable array to 1
      iInt = 1
      iLogical = 1
      iReal = 1

      ! For each variable array, get the size.  Size is 0 if not present.
      if (present(intArgs)) then
         nInts = size(intArgs)
      else
         nInts = 0
      endif

      if (present(logicArgs)) then
         nLogicals = size(logicArgs)
      else
         nLogicals = 0
      endif

      if (present(realArgs)) then
         nReals = size(realArgs)
      else
         nReals = 0
      endif

      ! Initialize strings
      write(outString,*) ''
      write(varPart,*) ''
      errVarPart = '**'  ! string to use if variable expansion fails

      !Initialize char info
      curLen = 0
      charExpand = .false.

      ! Loop over character positions in inString
      do i = 1, len_trim(inString)
         if (inString(i:i) == '$' .and. (.not. charExpand)) then
             charExpand = .true.
         else
             if (charExpand) then
                select case (inString(i:i))
                   case ('i')
                      ! make the format large enough to include a large integer (up to 17 digits for 8-byte int)
                      ! it will be trimmed below
                      if (iInt <= nInts) then
                         write(varPart,'(i17)') intArgs(iInt)
                         iInt = iInt + 1
                      else
                         varPart = errVarPart
                      endif
                   case ('l')
                      if (iLogical <= nLogicals) then
                         if (logicArgs(iLogical)) then
                            write(varPart ,'(a)') 'T'
                         else
                            write(varPart ,'(a)') 'F'
                         endif
                         iLogical = iLogical + 1
                      else
                         varPart = errVarPart
                      endif
                   case ('r')
                      if (iReal <= nReals) then
                         realPrecision = precision(realArgs(iReal))
                         ! Note 7 additional characters may be needed beyond the precision
                         ! e.g.: -1234567.89012345   G13.6   ->   -0.123457E+07
                         write(realFormat, '(a, i2.2, a, i2.2, a)') '(G', realPrecision+7,'.', realPrecision, ')'
                         write(varPart, trim(realFormat)) realArgs(iReal)
                         iReal = iReal + 1
                      else
                         varPart = errVarPart
                      endif
                   case ('$')
                         varPart = '$'
                   case default
                      varPart = errVarPart
                end select
                outString = outString(1:curLen) // trim(adjustl(varPart))

                curLen = curLen + len_trim(adjustl(varPart))
                charExpand = .false.
             else
                outString(curLen+1:curLen+1) = inString(i:i)
                curLen = curLen+1
             end if
         end if
      end do

   !--------------------------------------------------------------------
   end subroutine log_expand_string

end module ModEM_utils
