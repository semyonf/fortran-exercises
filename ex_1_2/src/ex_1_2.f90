! Задание 1.2 в учебнике

program ex_1_2
    implicit none

    integer, parameter      :: R_ = 4, L = 6
    character(*), parameter :: input_file = "../data/input.txt", &
                               output_file = "output.txt", &
                               E_ = "UTF-8"
    integer                 :: Out = 0, In = 0, i
    real(R_)                :: vals(L), xs(L), x

    open (file=input_file, encoding=E_, newunit=In)
        read(In,'(6f7.2 )') vals
        read(In,'(f7.2 )') x
    close (In)

    forall (i=0:L)
        xs(L-i) = x ** i
    end forall

    open (file=output_file, encoding=E_, newunit=Out)
        write(Out,*) 'Result = ', dot_product(vals, xs)
    close (Out)

end program ex_1_2