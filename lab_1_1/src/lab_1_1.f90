program lab_1_1
    use Environment

    implicit none
    integer, parameter                              :: N_RECORDS = 15, L_NAME = 15, L_POSITION = 15
    integer                                         :: i, IO, In, Out, N_unique = 0
    character(L_NAME, kind=CH_)                     :: names(N_RECORDS), positions(N_RECORDS), uniquePositions(N_RECORDS)
    character(*), parameter                         :: input_file = "../data/input.txt", &
                                                       output_file = "output.txt", &
                                                       listFormat = '(2a)', resFormat = '(2a,i1)'

    ! Чтение списка сотрудников
    open (file=input_file, encoding=E_, newunit=In)
        read (In, listFormat, iostat=IO) (names(i), positions(i), i = 1, N_RECORDS)
    close (In)

    ! Обработка статуса чтения.
    Out = OUTPUT_UNIT
    open (Out, encoding=E_)
    select case(io)
        case(0)
        case(IOSTAT_END)
           write (Out, '(a)') "Reached end of file while reading the list."
        case(1:)
           write (Out, '(a)') "Error while reading the list: ", io
        case default
           write (Out, '(a)') "Undetermined error has occured while reading the list: ", io
    end select

    ! Обработка списка
    do i = 1, N_RECORDS
        if (.not. any(positions(i) == uniquePositions)) then
            N_unique = N_unique + 1
            uniquePositions(N_unique) = positions(i)
        endif
    enddo

    ! Вывод списка сотрудников и количества профессий
    open (file=output_file, encoding=E_, newunit=Out)
        write (Out, *) 'Список сотрудников:'
        write (Out, listFormat, iostat=IO) (names(i), positions(i), i = 1, N_RECORDS)
        write (Out, *) 'Профессии:'
        write (Out, resFormat, iostat=IO) (uniquePositions(i), ' -> ',count(positions == uniquePositions(i)), i = 1, N_unique)
    close (Out)

    ! Обработка статуса записи.
    Out = OUTPUT_UNIT
    open (Out, encoding=E_)
    select case(io)
        case(0)
        case(IOSTAT_END)
           write (Out, '(a)') "Reached end of file while writing the list."
        case(1:)
           write (Out, '(a)') "Error while writing the list: ", io
        case default
           write (Out, '(a)') "Undetermined error has occured while writing the list: ", io
    end select

end program lab_1_1