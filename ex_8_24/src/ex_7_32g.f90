! 7.32g в учебнике
! Упорядочить элементы в СТРОКАХ матрицы B(10,15) так, чтобы |B(i,j)| <= |B(i,(j+1))|

program ex_7_32g
    implicit none

    integer                 :: B(2,3), i, j, In, Out
    character(*), parameter :: output_file = "output.txt", &
                               input_file = "../data/input.txt", &
                               E_ = "UTF-8"

    20 format(2(/,3i3))

    B = reshape((/1, 2, 3, 4, 5, 6/), shape(B))

    write(*,*) 'Original matrix'
    write(*,20) B

    write(*,*) 'Sorted matrix'
    write(*,20) B

end program ex_7_32g