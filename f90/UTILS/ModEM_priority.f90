module ModEM_priority_queue

    use iso_fortran_env, only: int64, real64

    use utilities
    use ModEM_logger

    implicit none

    type priority_queue
        character (len=256) :: name = ''
        integer :: size = 0
        type (node), pointer :: heap => null()
        type (priority_queue), pointer :: next => null()
    end type priority_queue

    type node
        integer ::  iTx   
        integer ::  iPol 
        integer :: weight
        integer :: scaled_weight

        type(node), pointer :: prev => null()
        type(node), pointer :: next => null()
    end type node

    type (priority_queue), target :: queues

contains


subroutine ModEM_pque_init(name)

    implicit none

    character (len=*), intent(in) :: name
    type (priority_queue), pointer :: queue

    if (ModEM_pque_exists(name)) then
        call errStop("ERROR: a pirority queue of name '" // trim(name) //"' already exsits!")
    end if

    allocate(queue)
    queue % name = trim(name)

    ! Append it to the front of the queues
    queue % next => queues % next
    queues % next => queue

end subroutine ModEM_pque_init

subroutine ModEM_pque_print_queues()
    ! Print all the PQs by name

    implicit none

    type (priority_queue), pointer :: cur

    integer :: nque

    nque = 0

    cur => queues % next
    do while(associated(cur)) 
        nque = nque + 1
        write(0,*) nque, "Priority Queue: ", cur % name

        cur => cur % next
    end do

end subroutine ModEM_pque_print_queues

function ModEM_pque_get(name) result(pque)

    implicit none

    character (len=*), intent(in) :: name
    type (priority_queue), pointer :: pque
    type (priority_queue), pointer :: cur

    pque => null()

    if (.not. ModEM_pque_exists(name)) then 
        call errStop("There is no priorty queue associated with this name: '"// trim(name) // "'")
    end if

    cur => queues
    do while (associated(cur))
        if (trim(cur % name) == trim(name)) then
            pque => cur
        end if 

        cur => cur % next
    end do


end function ModEM_pque_get

function ModEM_pque_exists(name) result(exists)

    implicit none

    character (len=*), intent(in) :: name
    logical :: exists

    type (priority_queue), pointer :: cur

    cur => queues

    exists = .false.
    do while (associated(cur))
        if (trim(cur % name) == trim(name)) then
            exists = .true.
        end if 

        cur => cur % next
    end do


end function ModEM_pque_exists

subroutine ModEM_pque_push(que_name, iTx, iPol, weight, scaled_weight)

    implicit none

    character (len=*), intent(in) :: que_name
    integer, intent(in) :: iTx
    integer, intent(in) :: iPol
    real, intent(in) :: weight
    integer, intent(in), optional :: scaled_weight

    type(node), pointer :: entry
    type(node), pointer :: cur
    type(priority_queue), pointer :: que

    allocate(entry)

    entry%iTx = iTx
    entry%iPol = iPol
    entry%weight = weight

    if (present(scaled_weight)) then
        entry % scaled_weight = scaled_weight
    else
        entry % scaled_weight = -1
    end if

    ! First node
    que => ModEM_pque_get(que_name)
    if (.not. associated(que % heap)) then
        que % heap => entry
        entry % prev => null()
        entry % next => null()
        return
    end if

    cur => que % heap
    do while (associated(cur))
    
        ! Insert entry before cur if its weight is greater than cur's weight
        if (cur % weight < entry % weight) then
            entry % next => cur
            entry % prev => cur % prev

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

        ! We are at the end of the list
        if (.not. associated(cur % next)) then
            entry % next => null()
            entry % prev => cur
            cur % next => entry
            exit
        end if

        cur => cur % next
    end do

end subroutine ModEM_pque_push

subroutine ModEM_pque_peek(que_name, iTx, iPol, weight, scaled_weight)

    implicit none

    character (len=*), intent(in) :: que_name
    integer, intent(out) :: iTx
    integer, intent(out) :: iPol
    real (kind=real64), intent(out) :: weight
    integer, intent(out) :: scaled_weight

    type (priority_queue), pointer :: que
    type (node), pointer :: entry

    que => ModEM_pque_get(que_name)
    if (.not. associated(que % heap)) then
        call errStop('Priority queue is empty')
    end if

    entry => que % heap
    iTx = entry % iTx
    iPol = entry % iPol
    weight = entry % weight
    scaled_weight = entry % scaled_weight

end subroutine ModEM_pque_peek

subroutine ModEM_pque_seek(que_name, desired_weight, iTx, iPol, weight, scaled_weight)

    implicit none

    character (len=*), intent(in) :: que_name
    integer, intent(in) :: desired_weight
    integer, intent(out) :: iTx
    integer, intent(out) :: iPol
    integer, intent(out), optional :: weight
    integer, intent(out), optional :: scaled_weight

    type (priority_queue), pointer :: que
    type (node), pointer :: cur

    que => ModEM_pque_get(que_name)
    if (.not. associated(que % heap)) then
        call errStop('Priority queue is empty')
    end if

    iTx = -1
    iPol = -1

    if (present(weight)) then
        weight = -1
    end if

    if (present(scaled_weight)) then
        scaled_weight = -1
    end if

    call ModEM_log('ModEM_pque_seek - desired_weight: $i ', intArgs=(/desired_weight/))

    cur => que % heap
    do while (associated(cur))
        if (cur % scaled_weight == desired_weight) then
            ! We found our desired weight so return it
            iTx = cur % iTx
            iPol = cur % iPol

            if (present(weight)) then
                weight = cur % weight
            end if

            if (present(scaled_weight)) then
                scaled_weight = cur % scaled_weight
            end if

            ! Now remove it from the queue
            if (.not. associated(cur % prev)) then
                ! If we are at the front of the que
                que % heap => cur % next

                if (associated(cur % next)) then
                    cur % next % prev => null()
                end if
            else
                cur % next % prev => cur % prev
                cur % next => null()
                cur % prev => null()
            end if

            deallocate(cur)
            cur => null()

            return
        end if 

        cur => cur % next
    end do

end subroutine ModEM_pque_seek

function ModEM_pque_search(que_name, desired_weight, weight_avail) result (found)

    implicit none

    character (len=*), intent(in) :: que_name
    integer, intent(in) :: desired_weight
    integer, intent(out) :: weight_avail
    type (priority_queue), pointer :: que
    type (node), pointer :: cur

    logical :: found

    found = .false.

    que => ModEM_pque_get(que_name)
    if (.not. associated(que % heap)) then
        call errStop('Priority queue is empty')
    end if

    cur => que % heap
    do while (associated(cur))
        if (cur % scaled_weight == desired_weight) then
            found = .true.
            call ModEM_log("We found: $i ", intArgs=(/cur % scaled_weight/))
            weight_avail = cur % scaled_weight
            return
        end if 

        cur => cur % next
    end do

end function ModEM_pque_search

subroutine ModEM_pque_pop(que_name, iTx, iPol, weight, scaled_weight)

    implicit none

    character (len=*), intent(in) :: que_name
    integer, intent(out) :: iTx
    integer, intent(out) :: iPol
    real (kind=real64), intent(out) :: weight
    integer, intent(out) :: scaled_weight

    type (priority_queue), pointer :: que
    type (node), pointer :: entry

    que => ModEM_pque_get(que_name)
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

function ModEM_pque_is_empty(que_name) result(is_empty)

    implicit none

    character (len=*), intent(in) :: que_name
    logical :: is_empty
    type (priority_queue), pointer :: que

    que => ModEM_pque_get(que_name)
    is_empty = .not. associated(que % heap)

end function ModEM_pque_is_empty

subroutine ModEM_pque_rescale_weights(que_name, total_mpi_tasks)

    implicit none

    character (len=*), intent(in) :: que_name
    integer, intent(in) :: total_mpi_tasks

    type(node), pointer :: cur
    integer :: tasks_remaining
    integer :: ideal_ranks
    integer :: actual_ranks
    real (kind=real64):: max_weight, fraction

    type (priority_queue), pointer :: que


    que => ModEM_pque_get(que_name)
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
        max_weight = 1.0
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

        write(0,*) 'cur % iTx=', cur % iTx, ' iPol=', cur % iPol, ' weight=', cur % weight, &
            ' fraction=', fraction, ' scaled_weight=', cur % scaled_weight

        cur => cur % next
    end do


end subroutine ModEM_pque_rescale_weights

subroutine ModEM_pque_print(que_name)

    implicit none

    character (len=*), intent(in) :: que_name
    type(priority_queue), pointer :: que
    type(node), pointer :: cur

    que => ModEM_pque_get(que_name)
    if (.not. associated(que % heap)) then
        write(0,*) 'Priority queue is empty'
        return
    end if

    write(0,*) 'Printing priority queue: ', trim(que % name)
    cur => que % heap
    do while (associated(cur))
        write(0,*) 'iTx:', cur % iTx, 'iPol:', cur % iPol, 'weight:', cur % weight, 'scaled: ', cur % scaled_weight
        cur => cur % next
    end do

end subroutine ModEM_pque_print

subroutine ModEM_pque_log(que_name, message)

    implicit none

    character (len=*), intent(in) :: message
    character (len=*), intent(in) :: que_name
    type(priority_queue), pointer :: que
    type(node), pointer :: cur
    character (len=*), parameter :: LOG_MSG_FMT = "(A, I4.4, A, I2.2, A, I5.5, A, I4.4)"
    character (len=512) :: log_message

    que => ModEM_pque_get(que_name)
    if (.not. associated(que % heap)) then
        call ModEM_log("Priority queue '"// trim(que_name)//"' is empty!")
        return
    end if

    cur => que % heap
    call ModEM_log("Printing priority queue: '"//trim(que_name)//"' "//trim(message)//":")
    do while (associated(cur))
        write(log_message, LOG_MSG_FMT) " - iTx: ", cur % iTx, ', iPol: ', cur % iPol, &
            ', weight: ', cur % weight, ' scaled: ', cur % scaled_weight
        call ModEM_log(log_message)
        cur => cur % next
    end do

end subroutine ModEM_pque_log


end module ModEM_priority_queue
