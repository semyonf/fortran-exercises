! Задание 1.2 в учебнике

program ex_1_2
    implicit none

    character(*), parameter :: input_file = "../data/input.txt", &
                               output_file = "output.txt", &
                               E_ = "UTF-8"
    integer                 :: Out = 0, In = 0
    real*4                  :: vals(6) = .0

    open (file=input_file, encoding=E_, newunit=In)
        read(In,'(6f5.2)') 

    close (In)

    open (file=output_file, encoding=E_, newunit=Out)
        write(Out,'(2f5.2)') temp
    close (Out)

end program ex_1_2