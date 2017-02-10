! 7.53
! Записать нули на места элементов, образованных пересечением главной и побочной диагоналей
! у верхних треугольных матрицы Z и Y

program ex_7_53
   implicit none

   integer, allocatable    :: Z(:,:)
   integer                 :: In, Out, size_of_Z, i, column, k
   character(*), parameter :: output_file = "output.txt", input_file = "../data/input.txt"

   open (file=input_file, newunit=In)
      read(In, *) size_of_Z
      allocate(Z(size_of_Z,size_of_Z))
      read(In, *) (Z(i,:), i = 1, size_of_Z)

   close (In)

   k = nint(size_of_Z / 2.0)

   do concurrent (column = 1:size_of_Z)
      Z(1:(k - abs(k - column)),column) = 0
   end do

   open (file=output_file, newunit=Out)
      write(Out, *) '-z-'
      write(Out,*) Z(1,:)
      write(Out,*) Z(2,:)
      write(Out,*) Z(3,:)
      write(Out,*) Z(4,:)
      write(Out,*) Z(5,:)
   close (Out)

end program ex_7_53
