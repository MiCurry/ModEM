module ModEM_utils

    use mpi
    use utilities

    implicit none

    integer :: rank
    integer :: comm_size
    integer :: log_fid

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

contains

    subroutine ModEM_utils_init()

        implicit none

        integer :: ierr, error_code
        character(len=*), parameter :: log_str_fmt = '(A,I4.4,A)'
        character(len=40) :: log_fname

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

        write(log_fname, log_str_fmt) 'log.', rank, '.modem.out'

        open(newunit=log_fid, file=log_fname, status='replace')
        write(log_fid, *) "File opened"
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

    subroutine ModEM_utils_log_write(msg, intArgs, realArgs, logicArgs) 

        implicit none

        character(len=*), intent(in) :: msg
        integer, dimension(:), intent(in), optional :: intArgs  !< Input: integer variable values to insert into message
        real(kind=RKIND), dimension(:), intent(in), optional :: realArgs  !< Input: real variable values to insert into message
             !< Input: exponential notation variable values to insert into message
        logical, dimension(:), intent(in), optional :: logicArgs  !< Input: logical variable values to insert into message
        character(len=strKind) :: messageExpanded !< message after expansion of $ variable insertions

        call log_expand_string(msg, messageExpanded, intArgs, logicArgs, realArgs)

        write(log_fid,*) trim(messageExpanded)
        call flush(log_fid)

    end subroutine ModEM_utils_log_write

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
