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

        real (kind=real64) :: total_cost = 0.0
        real :: average = 0.0
        real :: avg_weighted = 0.0

        type(tx_record), pointer :: record => null()
    end type tx_profile_t

    type tx_record
        ! A tx record is just a time record
        integer :: nTasks = 0
        integer :: itr = 0
        integer (kind=int64) :: secs
        integer (kind=int64) :: nsecs
        real (kind=real64) :: total_time_s
        ! Single-task-equivalent cost: total_time_s * nTasks
        real (kind=real64) :: cost_s
        real (kind=real64) :: weight = 1.0
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
        write(0,*) ' secs: ', cur % secs, ' wgt: ', cur % weight, ' itr: ', cur % itr, ' nTasks: ', cur %nTasks
        cur => cur % next
    end do

end subroutine ModEM_profiler_print

subroutine ModEM_profiler_write_csv_header(fname)

    implicit none

    character (len=*), intent(in) :: fname

    integer :: fid

    open(newunit=fid, file=trim(fname), action='write', status='unknown')

    write(fid,*) 'Iteration,iTx,iPol,nTasks,Weight,Total Time (s),Cost (s), Wgted Avg, Total Avg'
    close(fid)


end subroutine ModEM_profiler_write_csv_header

subroutine ModEM_profiler_write_csv_line(profile_name, fname, iTx, iPol)

    implicit none

    character (len=*), intent(in) :: profile_name
    character (len=*), intent(in) :: fname

    integer :: fid

    type (profile_t), pointer :: profile
    type (tx_record), pointer :: cur

    integer :: iTx, iPol

    open(newunit=fid, file=trim(fname), position='append', action='write', status='unknown')

    profile => ModEM_profiler_get(profile_name)
    cur => profile % tx_profiles(iTx, iPol) % record
    write(fid,*) cur % itr, ',', iTx, ',', iPol, ',', cur % nTasks, ',', cur % weight, ',', cur % total_time_s, ',', cur % cost_s, ',', &
        profile % tx_profiles(iTx, iPol) % avg_weighted, ',', profile % tx_profiles(iTx, iPol) % average
    close(fid)

end subroutine Modem_profiler_write_csv_line

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

subroutine ModEM_profiler_record(profile_name, iTx, iPol, secs, nsecs, weight, itr, nTasks)

    implicit none

    character (len=*), intent(in) :: profile_name
    integer, intent(in) :: iTx
    integer, intent(in) :: iPol
    integer (kind=int64), intent(in) :: secs
    integer (kind=int64), intent(in) :: nsecs
    real (kind=real64), intent(in), optional :: weight
    integer, intent(in), optional :: itr
    integer, intent(in), optional :: nTasks

    type (profile_t), pointer :: profile => null()
    type(tx_record), pointer :: new_record

    real (kind=real64) :: weight_lcl
    real (kind=real64) :: total_time_secs
    integer :: itr_lcl
    integer :: nTasks_lcl

    if (present(weight)) then
        weight_lcl = weight
    else
        weight_lcl = 1.0
    end if

    if (present(itr)) then
        itr_lcl = itr
    else
        itr_lcl = -1
    end if

    if (present(nTasks)) then
        nTasks_lcl = nTasks
    else
        nTasks_lcl = -1
    end if

    profile => ModEM_profiler_get(profile_name)
    allocate(new_record)
    total_time_secs = real(secs, kind=real64) + (real(nsecs, kind=real64) / 1e+9_real64)
    new_record % secs = secs
    new_record % nsecs = nsecs
    new_record % total_time_s = total_time_secs
    ! Normalize by the number of tasks so a transmitter that ran on several
    ! tasks does not appear cheaper than it is
    new_record % cost_s = total_time_secs * real(max(nTasks_lcl, 1), kind=real64)
    new_record % weight = weight_lcl
    new_record % itr = itr_lcl
    new_record % nTasks = nTasks_lcl
    new_record % next => null()

    if (.not. associated(profile % tx_profiles(iTx, iPol)%record)) then
        profile % tx_profiles(iTx, iPol)%record => new_record
    else
        new_record%next => profile % tx_profiles(iTx, iPol)%record
        profile % tx_profiles(iTx, iPol)%record => new_record
    end if

    profile % tx_profiles(iTx, iPol)%total_cost = profile % tx_profiles(iTx, iPol) % total_cost + new_record % cost_s
    profile % tx_profiles(iTx, iPol)%count = profile % tx_profiles(iTx, iPol) % count + 1

    ! Recalculate total average
    profile % tx_profiles(iTx, iPol)%average = profile % tx_profiles(iTx, iPol)%total_cost &
                        / real(profile % tx_profiles(iTx, iPol)%count, kind=real64)

end subroutine ModEM_profiler_record

subroutine ModEM_profiler_compute_wgt_avgs(profile_name)

    implicit none

    character (len=*), intent(in) :: profile_name

    type (profile_t), pointer :: profile
    type (tx_record), pointer :: cur
    integer :: iTx, iPol

    real (kind=real64) :: total_time_wgt
    real (kind=real64) :: total_wgt
    real (kind=real64) :: wgt_avg
    
    profile => ModEM_profiler_get(profile_name)
    do iTx = 1, profile % nTx, 1
        do iPol = 1, profile % nPol, 1
            cur => profile % tx_profiles(iTx, iPol) % record

            total_time_wgt = 0.0
            total_wgt = 0.0
            wgt_avg = 0.0
            do while (associated(cur))
                total_time_wgt = total_time_wgt + (cur % cost_s * cur % weight)
                total_wgt = total_wgt + cur % weight
                cur => cur % next
            end do

            if (total_wgt > 0.0) then
                wgt_avg = total_time_wgt / total_wgt
            end if

            profile % tx_profiles(iTx, iPol) % avg_weighted = wgt_avg
            write(0,*) 'Total weighted avg for: iTx: ', itx, ' iPol: ', ipol, 'wgt_avg: ', wgt_avg
            write(0,*) 'total_wgt:', total_wgt, 'total_time_wgt: ', total_time_wgt
        end do
    end do

end subroutine ModEM_profiler_compute_wgt_avgs

function ModEM_profiler_get_average(profile_name, iTx, ipol) result(average_secs)

    implicit none

    character (len=*), intent(in) :: profile_name
    integer, intent(in) :: iTx
    integer, intent(in) :: ipol

    real (kind=real64) :: average_secs

    type (profile_t), pointer :: profile

    profile => ModEM_profiler_get(profile_name)
    average_secs = profile % tx_profiles(iTx, iPol) % avg_weighted

end function ModEM_profiler_get_average

end module ModEM_profiler
