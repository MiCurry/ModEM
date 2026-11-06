module ModEM_profiler

    use iso_fortran_env, only: int64, real64
    use utilities

    implicit none

    type tx_profile_t
        ! A Tx profile contains recorded times for a given tx and pol
        ! as well as the averages of those records
        integer :: iTx
        integer :: iPol
        integer :: count = 0

        integer (kind=int64) :: total_time = 0
        real(kind=real64) :: average = 0

        type(tx_record), pointer :: record => null()
    end type tx_profile_t

    type tx_record
        ! A tx record is just a time record
        integer(kind=int64) :: secs
        type(tx_record), pointer :: next => null()
    end type tx_record

    type profile_t
        ! A profile is a collection of tx_profiles
        ! For instance, we might want a profile for fwd, JmultT, Jmult etc.
        character (len=256) :: name
        integer :: nTx
        integer :: nPol

        type(tx_profile_t), pointer, dimension(:,:) :: tx_profiles => null()
        type(profile_t), pointer :: next => null()
    end type profile_t

    type (profile_t) :: profiles

contains

function ModEM_profiler_exists(profiler_name) result(exists)

    implicit none

    character (len=*), intent(in) :: profiler_name

    logical :: exists

    type (profile_t), pointer :: cur

    exists = .false.
    cur => profiles % next
    do while(associated(cur))
        if (trim(profiler_name) == trim(cur % name)) then
            exists = .true.
            exit
        end if
        cur => cur % next 
    end do

end function ModEM_profiler_exists

subroutine ModEM_profiler_print_profiles()

    implicit none

    type (profile_t), pointer :: cur

    cur => profiles % next
    do while(associated(cur))
        write(0,*) 'Profile: ', trim(cur % name), ' nTx: ', cur % nTx, ' nPol: ', cur % nPol
        cur => cur % next
    end do

end subroutine ModEM_profiler_print_profiles

subroutine ModEM_profiler_print(profile_name, itx, ipol)

    implicit none

    character (len=*), intent(in) :: profile_name
    integer, intent(in) :: itx
    integer, intent(in) :: ipol

    type (profile_t), pointer :: profile
    type (tx_profile_t), pointer :: tx_profile
    type (tx_record), pointer :: cur


    profile => ModEM_profiler_get(profile_name)

    write(0,*) 'Printing tx_profile for iTx: ', itx, ' iPol:', ipol, ' from profile_name: ', trim(profile_name)

    tx_profile => profile % tx_profiles(itx, ipol)
    cur => tx_profile % record
    do while (associated(cur)) 
        write(0,*) ' secs: ', cur % secs
        cur => cur % next
    end do

end subroutine ModEM_profiler_print

function ModEM_profiler_get(profile_name) result(profile)

    implicit none

    character (len=*), intent(in) :: profile_name


    type (profile_t), pointer :: profile
    type (profile_t), pointer :: cur

    if (.not. ModEM_profiler_exists(profile_name)) then
        call errStop("Profiler of name '" // trim(profile_name) // "' does not exist.")
    end if

    profile => null()
    cur => profiles % next
    do while (associated(cur))
        if (trim(profile_name) == trim(cur % name)) then
            profile => cur
            exit
        end if

        cur => cur % next
    end do

end function ModEM_profiler_get


subroutine ModEM_profiler_new_profile(profile_name, nTxs, nPol)

    implicit none

    character (len=*), intent(in) :: profile_name
    integer, intent(in) :: nTxs, nPol
    integer :: iTx, iPol

    type (profile_t), pointer :: profile

    if (ModEM_profiler_exists(profile_name)) then
        call errStop("A profiler already exists with the name: '" // trim(profile_name) // "'")
    end if

    allocate(profile)
    profile % name = trim(profile_name)
    profile % nTx = nTxs
    profile % nPol = nPol
    allocate(profile % tx_profiles(nTxs, nPol))

    do iTx = 1, profile % nTx
        do iPol = 1, profile % nPol
            profile % tx_profiles(iTx, iPol)%iTx = iTx
            profile % tx_profiles(iTx, iPol)%iPol = iPol
            profile % tx_profiles(iTx, iPol)%record => null()
        end do
    end do

    ! Prepend it to the first
    profile % next => profiles % next
    profiles % next => profile

end subroutine ModEM_profiler_new_profile

subroutine ModEM_profiler_record(profile_name, iTx, iPol, secs)

    implicit none

    character (len=*), intent(in) :: profile_name
    integer, intent(in) :: iTx
    integer, intent(in) :: iPol
    integer(kind=int64), intent(in) :: secs

    type (profile_t), pointer :: profile => null()
    type(tx_record), pointer :: new_record

    profile => ModEM_profiler_get(profile_name)

    allocate(new_record)
    new_record%secs = secs
    new_record%next => null()

    if (.not. associated(profile % tx_profiles(iTx, iPol)%record)) then
        profile % tx_profiles(iTx, iPol)%record => new_record
    else
        new_record%next => profile % tx_profiles(iTx, iPol)%record
        profile % tx_profiles(iTx, iPol)%record => new_record
    end if

    profile % tx_profiles(iTx, iPol)%total_time = profile % tx_profiles(iTx, iPol) % total_time + secs
    profile % tx_profiles(iTx, iPol)%count = profile % tx_profiles(iTx, iPol) % count + 1

    ! Recalculate average
    profile % tx_profiles(iTx, iPol)%average = real(profile % tx_profiles(iTx, iPol)%total_time, kind=real64) &
                        / real(profile % tx_profiles(iTx, iPol)%count, kind=real64)

end subroutine ModEM_profiler_record

function ModEM_profiler_get_average(profile_name, iTx, ipol) result(average_secs)

    implicit none

    character (len=*), intent(in) :: profile_name
    integer, intent(in) :: iTx
    integer, intent(in) :: ipol

    real (kind=int64) :: average_secs

    type (profile_t), pointer :: profile

    profile => ModEM_profiler_get(profile_name)
    average_secs = profile % tx_profiles(iTx, iPol) % average

end function ModEM_profiler_get_average

end module ModEM_profiler
