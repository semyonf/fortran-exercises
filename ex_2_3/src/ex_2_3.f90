! Задание 2.3 в учебнике

program ex_2_3
    implicit none

        integer                 :: Out = 0, In = 0, result = 0, quantity = 0
        integer, parameter      :: R_ = 4
        logical, allocatable    :: mask(:)
        real(R_), allocatable   :: values(:)
        character(*), parameter :: input_file = "../data/input.txt", &
                                   output_file = "output.txt", &
                                   E_ = "UTF-8"

        open (file=input_file, encoding=E_, newunit=In)
            read(In,'(I2)') quantity

            allocate(mask(quantity))
            allocate(values(quantity))

            read(In, '(f5.2)') values(:)
            write(*,*) values
        close (In)

        mask = values >= 0
        result = count(mask);

        open (file=output_file, encoding=E_, newunit=Out)
            write(Out,'(I3)') result
        close (Out)

end program ex_2_3