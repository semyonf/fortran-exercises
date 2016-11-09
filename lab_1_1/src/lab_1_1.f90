! Определить число одинаковых должностей. Пример выходного файла:
! ведущий инженер - 2
! старший инженер - 3
! инженер         - 8
! техник          - 2

program lab_1_1
    use Environment
    implicit none

    integer, parameter              :: N_employees = 14, L_name = 15, L_position = 15
    character(L_name, kind=CH_)     :: names(N_employees) = CH__""
    character(L_position, kind=CH_) :: positions(N_employees) = CH__""
    integer                         :: In, Out, i, IO
    character(*), parameter         :: output_file = "output.txt", &
                                       input_file = "../data/input.txt"

    open (file=input_file, newunit=In)
        read (In, '(2(a))', iostat=IO) (names(i), positions(i), i = 1, N_employees)
    close (In)

    open (file=output_file, newunit=Out)
        write (Out, '(2(a))', iostat=IO) (names(i), positions(i), i = 1, N_employees)
    close (Out)

end program lab_1_1