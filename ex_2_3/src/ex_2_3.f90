! Задание 2.3 в учебнике

program ex_2_3
    implicit none

        integer, parameter      :: R_ = 4
        integer(1), parameter   :: quantity = 4
        integer(1)              :: i
        logical                 :: values(quantity)
        real(R_)                :: value
        character(*), parameter :: input_file = "../data/input.txt", &
                                   output_file = "output.txt", &
                                   E_ = "UTF-8"
        integer                 :: Out = 0, In = 0, result = 0

        open (file=input_file, encoding=E_, newunit=In)
            do i = 1, quantity
                read(In,'(f5.2)') value
                if (value > 0) then
                    values(i) = .TRUE.
                endif
            end do
        close (In)

        result = count(values);

        open (file=output_file, encoding=E_, newunit=Out)
            write(Out,'(I3)') result
        close (Out)

end program ex_2_3