program lab_1_1
    use Environment
 
    implicit none
    integer, parameter                              :: N_EMPLOYEES = 15, L_NAME = 15, L_POSITION = 15
    integer                                         :: i, IO, In, Out
    integer, parameter                              :: INDEXES(*) = [(i, i = 1, N_EMPLOYEES)]
    character(L_NAME, kind=CH_)                     :: names(N_EMPLOYEES), positions(N_EMPLOYEES)
    character(*), parameter                         :: input_file = "../data/input.txt", &
                                                       output_file = "output.txt", &
                                                       format = '(2a)'

    open (file=input_file, encoding=E_, newunit=In)
       read (In, format, iostat=IO) (names(i), positions(i), i = 1, N_EMPLOYEES)
    close (In)
 
    open (file=output_file, encoding=E_, newunit=Out)
       write (Out, *) 'Исходный список'
       write (Out, format, iostat=IO) (names(i), positions(i), i = 1, N_EMPLOYEES)
    close (Out)

end program lab_1_1