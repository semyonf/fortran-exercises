! 4.1(в) в учебнике

program ex_4_1v
    implicit none

    real                    :: x = .01254, delta = .0002, f
    real                    :: arNumbers(1000)
    character(*), parameter :: output_file = "output.txt", &
                               E_ = "UTF-8"
    integer                 :: Out = 0, i = 0

    do i = 1, 1000
        arNumbers(i) = x
        x = x + delta
    enddo

    do i = 1, 1000
        f = arNumbers(i)
        arNumbers(i) = f**2 * TAN(f) + (SIN(f)/f)
    enddo

    open (file=output_file, encoding=E_, newunit=Out)
        do i = 1, 1000
            write(Out,'(f5.3)') arNumbers(i)
        enddo
    close (Out)

end program ex_4_1v