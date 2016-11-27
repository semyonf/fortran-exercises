program lab_1_2
    use Environment

    implicit none
    integer, parameter          :: N_RECORDS = 15, L_NAME = 15, L_POSITION = 15
    character(kind=CH_)         :: names(N_RECORDS, L_NAME), positions(N_RECORDS, L_POSITION)!, types(N_RECORDS, L_POSITION)
    integer                     :: In, Out
    ! logical                     :: repeated(N_RECORDS) = .false.
    character(*), parameter     :: input_file = "../data/input.txt", &
                                   output_file = "output.txt"

    ! Чтение списка сотрудников
    call ReadEmployeeList(input_file, names, positions)

    ! Вывод списка сотрудников и количества профессий
    call WriteEmployeeList(output_file, names, positions)

    ! ! Поиск повторившихся профессий
    ! call ProcessPositions(positions, types)

    ! Вывод профессий и их повторений
    ! call WritePositionsOccured(output_file, uniquePositions)

contains

    ! ! Подпроцесс для поиска и подсчета повторений профессий
    ! pure subroutine ProcessPositions(positions, types)
    !     implicit none

    !     character(kind=CH_) positions(:,:), types(:,:)
    !     intent(in)  positions
    !     intent(out) types

    ! end subroutine ProcessPositions

    ! Подпроцесс для чтения списка сотрудников
    subroutine ReadEmployeeList(input_file, names, positions)
        implicit none

        character(*) input_file
        character(kind=CH_) names(:,:), positions(:,:)
        intent(in)  input_file
        intent(out) names, positions

        integer :: In, IO, i
        character(:), allocatable :: format

        format = '(' // L_NAME // 'a1, ' // L_POSITION // 'a1)'

        open (file=input_file, encoding=E_, newunit=In)
            read(In, format, iostat=IO) (names(i,:), positions(i,:), i = 1, N_RECORDS)
            call Handle_IO_status(IO, 'ReadEmployeeList')
        close (In)

    end subroutine ReadEmployeeList

    ! Подпроцесс для записи списка сотрудников
    subroutine WriteEmployeeList(output_file, names, positions)
        implicit none

        character(*) output_file
        character(kind=CH_) names(:,:), positions(:,:)
        intent(in)  output_file
        intent(out) names, positions

        integer :: Out, IO, i
        character(:), allocatable   ::format

        format = '(' // L_NAME // 'a1, ' // L_POSITION // 'a1)'

        open (file=output_file, encoding=E_, newunit=Out)
            write(Out, format, iostat=IO) (names(i,:), positions(i,:), i = 1, N_RECORDS)
            call Handle_IO_status(IO, 'WriteEmployeeList')
        close (Out)

    end subroutine WriteEmployeeList

end program lab_1_2