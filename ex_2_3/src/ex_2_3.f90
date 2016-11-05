! Задание 2.3 в учебнике

program ex_2_3
    implicit none

        integer                 :: Out = 0, In = 0, positives = 0, size = 0
        integer, parameter      :: R_ = 4
        logical, allocatable    :: mask(:)
        real(R_), allocatable   :: values(:)
        character(*), parameter :: input_file = "../data/input.txt", &
                                   output_file = "output.txt"

        open (file=input_file, newunit=In)
            read(In, *) size
            allocate(values(size))
            read(In, *) values(:)
        close (In)

        mask = values >= 0
        positives = count(mask)

        open (file=output_file, newunit=Out)
            write(Out, *) positives
        close (Out)

end program ex_2_3