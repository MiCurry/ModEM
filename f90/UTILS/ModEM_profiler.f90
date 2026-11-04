module ModEM_profiler

    use iso_fortran_env, only: int64, real64
    use utilities

    implicit none

    type tx_profile
        integer :: iTx
        integer :: iPol
        integer :: count = 0

        integer (kind=int64) :: total_time = 0
        real(kind=real64) :: average = 0

        type(tx_record), pointer :: record => null()
    end type tx_profile

    type tx_record
        integer(kind=int64) :: secs
        type(tx_record), pointer :: next => null()
    end type tx_record

    type(tx_profile), pointer :: tx_profiles(:,:) => null()

    type priority_queue
        integer :: size = 0
        type(node), pointer :: heap => null()
    end type priority_queue

    type node
        integer ::  iTx   
        integer ::  iPol 
        integer :: weight
        integer :: scaled_weight


        type(node), pointer :: prev => null()
        type(node), pointer :: next => null()
    end type node

    type (priority_queue) :: que 

contains

subroutine ModEM_profiler_init(nTxs, nPol)

    implicit none

    integer, intent(in) :: nTxs, nPol
    integer :: iTx, iPol

    allocate(tx_profiles(nTxs, nPol))

    do iTx = 1, nTxs
        do iPol = 1, nPol
            tx_profiles(iTx, iPol)%iTx = iTx
            tx_profiles(iTx, iPol)%iPol = iPol
            tx_profiles(iTx, iPol)%record => null()
        end do
    end do

end subroutine ModEM_profiler_init

subroutine ModEM_profiler_record(iTx, iPol, secs)

    implicit none

    integer, intent(in) :: iTx
    integer, intent(in) :: iPol
    integer(kind=int64), intent(in) :: secs

    type(tx_record), pointer :: new_record

    allocate(new_record)
    new_record%secs = secs
    new_record%next => null()

    write(0,*) 'Recording time for iTx=', iTx, ' iPol=', iPol, ' secs=', secs

    if (.not. associated(tx_profiles(iTx, iPol)%record)) then
        tx_profiles(iTx, iPol)%record => new_record
    else
        new_record%next => tx_profiles(iTx, iPol)%record
        tx_profiles(iTx, iPol)%record => new_record
    end if

    tx_profiles(iTx, iPol)%total_time = tx_profiles(iTx, iPol) % total_time + secs
    tx_profiles(iTx, iPol)%count = tx_profiles(iTx, iPol) % count + 1

    ! Recalculate average
    tx_profiles(iTx, iPol)%average = real(tx_profiles(iTx, iPol)%total_time, kind=real64) / real(tx_profiles(iTx, iPol)%count, kind=real64)

    call ModEM_pque_push(iTx, iPol, tx_profiles(iTx, iPol)%average)

end subroutine ModEM_profiler_record

subroutine ModEM_profiler_get_slowest_txs()

    implicit none

    integer :: iTx, iPol
    integer :: slowest_iTx, slowest_iPol
    real(kind=real64) :: max_average
    real(kind=real64), dimension(:), allocatable :: averages
    integer, dimension(:), allocatable :: idx
    integer :: itx_cnt
    integer :: tx, pol

    max_average = -1.0
    slowest_iTx = -1
    
    slowest_iPol = -1  
    
    write(0,*) 'Tx Profiels size:', size(tx_profiles, 1), size(tx_profiles, 2), shape(tx_profiles)

    allocate(idx(size(tx_profiles, 1) * size(tx_profiles, 2)))
    idx(:) = -1
    allocate(averages(size(tx_profiles, 1) * size(tx_profiles, 2)))
    averages(:) = -1.0

    itx_cnt = 1

    do iTx = 1, size(tx_profiles, 1)
        do iPol = 1, size(tx_profiles, 2)
            if (iPol == 1) then
                idx(itx_cnt) = iTx_cnt
            else
                idx(itx_cnt) = iTx_cnt * 2
            end if

            averages(itx_cnt) = tx_profiles(iTx, iPol)%average

            if (tx_profiles(iTx, iPol)%average > max_average) then
                max_average = tx_profiles(iTx, iPol)%average
                slowest_iTx = iTx
                slowest_iPol = iPol
            end if

            call ModEM_pque_push(iTx_cnt, iPol, tx_profiles(iTx, iPol)%average)

            itx_cnt = itx_cnt + 1
        end do
    end do

    itx_cnt = 1
    do iTx = 1, size(averages, 1)
        if (mod(idx(iTx), 2) == 0) then
            tx = (idx(iTx) + 1) / 2
            pol = 2
        else
            tx = idx(iTx)
            pol = 1
        end if
    end do

    deallocate(idx)
    deallocate(averages)

    write(0,*) 'Slowest average time: ', max_average, 'For iTx', slowest_iTx, 'iPol', slowest_iPol

end subroutine ModEM_profiler_get_slowest_txs


subroutine ModEM_pque_push(iTx, iPol, weight)

    implicit none

    integer, intent(in) :: iTx
    integer, intent(in) :: iPol
    real(kind=real64), intent(in) :: weight

    type(node), pointer :: entry
    type(node), pointer :: cur

    allocate(entry)

    entry%iTx = iTx
    entry%iPol = iPol
    entry%weight = weight

    ! First node
    if (.not. associated(que % heap)) then
        que % heap => entry
        entry % prev => null()
        entry % next => null()
        return
    end if

    write(0,*) 'Entry is: iTx=', iTx, ' iPol=', iPol, ' weight=', weight

    cur => que % heap
    do while (associated(cur))
        ! Insert entry before cur if its weight is greater than cur's weight
        if (cur % weight < entry % weight) then
            entry % next => cur
            entry % prev => cur % prev

            write(0,*) 'Inserting entry before cur: iTx=', cur % iTx, ' iPol=', cur % iPol, ' weight=', cur % weight

            if (associated(cur % prev)) then
                cur % prev % next => entry
            else
                que % heap => entry
                entry % prev => null()
                entry % next => cur
            end if

            cur % prev => entry
            return
        end if

        cur => cur % next
    end do

end subroutine ModEM_pque_push

subroutine ModEM_pque_peek(iTx, iPol, weight, scaled_weight)

    implicit none

    integer, intent(out) :: iTx
    integer, intent(out) :: iPol
    real(kind=real64), intent(out) :: weight
    integer, intent(out) :: scaled_weight

    type(node), pointer :: entry

    if (.not. associated(que % heap)) then
        call errStop('Priority queue is empty')
    end if

    entry => que % heap
    iTx = entry % iTx
    iPol = entry % iPol
    weight = entry % weight
    scaled_weight = entry % scaled_weight

end subroutine ModEM_pque_peek


subroutine ModEM_pque_pop(iTx, iPol, weight, scaled_weight)

    implicit none

    integer, intent(out) :: iTx
    integer, intent(out) :: iPol
    real(kind=real64), intent(out) :: weight
    integer, intent(out) :: scaled_weight

    type (node), pointer :: entry

    if (.not. associated(que % heap)) then
        call errStop('Priority queue is empty')
    end if

    entry => que % heap
    iTx = entry % iTx
    iPol = entry % iPol
    weight = entry % weight
    scaled_weight = entry % scaled_weight

    que % heap => entry % next
    if (associated(que % heap)) then
        que % heap % prev => null()
    end if

    deallocate(entry)

end subroutine ModEM_pque_pop

function ModEM_pque_is_empty() result(is_empty)

    implicit none

    logical :: is_empty

    is_empty = .not. associated(que % heap)

end function ModEM_pque_is_empty

subroutine ModEM_pque_rescale_weights(total_mpi_tasks)

    implicit none

    integer, intent(in) :: total_mpi_tasks

    type(node), pointer :: cur
    integer :: tasks_remaining
    integer :: ideal_ranks
    integer :: actual_ranks
    real(kind=real64) :: max_weight, fraction

    if (.not. associated(que % heap)) then
        return
    end if

    cur => que % heap
    do while (associated(cur))
        cur % scaled_weight = 0
        cur => cur % next
    end do

    tasks_remaining = total_mpi_tasks

    max_weight = que % heap % weight

    if (max_weight <= 0) then 
        max_weight = 1.0_real64
    end if
    

    cur => que % heap
    do while (associated(cur))

        fraction = cur % weight / max_weight

        if (fraction == 1.0) then
            cur % scaled_weight = min(total_mpi_tasks, 3)
        else if (fraction >= 0.85) then
            cur % scaled_weight = min(total_mpi_tasks, 2)
        else if (fraction >= 0.75) then
            cur % scaled_weight = min(total_mpi_tasks, 2)
        else if (fraction >= 0.5) then
            cur % scaled_weight = min(total_mpi_tasks, 1)
        else if (fraction >= 0.25) then
            cur % scaled_weight = min(total_mpi_tasks, 1)
        else if (fraction >= 0.25) then
            cur % scaled_weight = min(total_mpi_tasks, 1)
        else
            cur % scaled_weight = min(total_mpi_tasks, 1)
        end if

        write(0,*) 'cur % iTx=', cur % iTx, ' iPol=', cur % iPol, ' weight=', cur % weight, ' fraction=', fraction, ' scaled_weight=', cur % scaled_weight

        cur => cur % next
    end do


end subroutine ModEM_pque_rescale_weights

subroutine ModEM_pque_print()

    implicit none

    type(node), pointer :: cur

    if (.not. associated(que % heap)) then
        write(0,*) 'Priority queue is empty'
        return
    end if


    write(0,*) 'Printing priority queue:'
    cur => que % heap
    do while (associated(cur))
        write(0,*) 'iTx:', cur % iTx, 'iPol:', cur % iPol, 'weight:', cur % weight, 'scaled: ', cur % scaled_weight
        cur => cur % next
    end do

end subroutine ModEM_pque_print
    
end module ModEM_profiler