! 7.19a в учебнике
! количесвто элементов матрицы B, которые положительны

program ex_7_19a
    implicit none

    integer                 :: x, y, Positives = 0, Out = 0, In = 0
    integer, allocatable    :: B(:,:), input(:)
    logical, allocatable    :: mask(:,:)
    character(*), parameter :: output_file = "output.txt", &
                               input_file = "../data/input.txt", &
                               E_ = "UTF-8"

    open (file=input_file, encoding=E_, newunit=In)
        read(In,'(I3)') x
        read(In,'(I3)') y

        allocate(input(x * y))
        allocate(mask(y,x))
        allocate(B(y,x))

        read(In,'(I3)') input(:)
    close (In)

    B = reshape(input, shape=[y, x])

    mask = B >= 0
    Positives = count(mask)

    open (file=output_file, encoding=E_, newunit=Out)
        write(Out,*) 'Positive elements:', Positives
    close (Out)
end program ex_7_19a
