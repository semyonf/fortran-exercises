program lab_1_1
    use Environment

    implicit none
    integer, parameter          :: N_RECORDS = 15, L_NAME = 15, L_POSITION = 15
    character(L_NAME, kind=CH_) :: names(N_RECORDS), positions(N_RECORDS), newPositions(N_RECORDS), unique(N_RECORDS)
    integer                     :: i = 0, IO, In, Out, Q_positions(N_RECORDS), N_unique = 0
    logical                     :: repeated(N_RECORDS) = .false.
    character(*), parameter     :: input_file = "../data/input.txt", &
                                   output_file = "output.txt", &
                                   listFormat = '(2a)', resFormat = '(2a,i1)'

    ! Чтение списка сотрудников
    open (file=input_file, encoding=E_, newunit=In)
        read (In, listFormat, iostat=IO) (names(i), positions(i), i = 1, N_RECORDS)
    close (In)

    newPositions = positions

    do while (.not. all(repeated))
        N_unique = N_unique + 1
        unique(N_unique) = newPositions(1)
        repeated = repeated .or. (unique(N_unique) == positions)
        Q_positions(N_unique) = count(unique(N_unique) == positions)
        newPositions = pack(positions, .not. repeated)
    enddo

    ! Вывод списка сотрудников и количества профессий
    open (file=output_file, encoding=E_, newunit=Out)
        write (Out, *) 'Список сотрудников:'
        write (Out, listFormat, iostat=IO) (names(i), positions(i), i = 1, N_RECORDS)
        write (Out, *) 'Встретились профессии:'
        write (Out, resFormat, iostat=IO) (unique(i), ' -> ', Q_positions(i), i = 1, N_unique)
    close (Out)
end program lab_1_1