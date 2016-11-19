! 6.2e в учебнике

program ex_6_2e
    implicit none

    integer, parameter      :: R_ = 16
    character(*), parameter :: output_file = "output.txt", &
                               input_file = "../data/input.txt"
    real(R_)                :: x = 0, a = 0, currentElement = 0, newElement = 0, currentSum = 0, newSum = 0, numerator = 0
    integer                 :: In = 0, Out = 0, denominator = 0

    open (file=input_file, newunit=In)
        read(In,'(f5.2)') a
        read(In,'(f5.2)') x
    close (In)

    currentElement = 1

    do
        denominator = denominator + 1
        currentSum = newSum
        newSum = currentSum + currentElement
        numerator = x * log(a)
        newElement = currentElement * numerator / denominator
        currentElement = newElement
        if (currentSum == newSum) exit
    end do

    open (file=output_file, newunit=Out)
        write(Out, '(f5.2,a,f5.2)') currentSum, ' ~ ' , a**x
    close (Out)

end program ex_6_2e