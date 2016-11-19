! 7.15e в учебнике
! минимальный в строке с минимальной суммой

program ex_7_15e
    implicit none

    integer                 :: x, y, Out, In, i, minRowSum, min
    integer, allocatable    :: B(:,:)
    character(*), parameter :: output_file = "output.txt", &
                               input_file = "../data/input.txt"

    open (file=input_file, newunit=In)
        read(In, *) x, y
        allocate(B(x,y))
        read(In, *) (B(i,:), i = 1, y)
    close (In)

    minRowSum = minloc(sum(B, 2), 1)
    min = minval(B(minRowSum,:))

    open (file=output_file, newunit=Out)
        write(Out, *) min
    close (Out)
end program ex_7_15e
