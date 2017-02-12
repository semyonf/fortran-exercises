! 6.2e в учебнике

program ex_6_2e
   implicit none

   integer, parameter      :: R_ = 16
   real(R_)                :: x = 0, a = 0, currentElement = 0, currentSum = 0, newSum = 0, numeratorDiff = 0
   integer                 :: In = 0, Out = 0, i = 0
   character(*), parameter :: output_file = "output.txt", input_file = "../data/input.txt"

   open (file=input_file, newunit=In)
      read(In,'(f5.2)') a
      read(In,'(f5.2)') x
   close (In)

   currentElement = 1
   newSum = 1
   numeratorDiff = x * log(a)

   do
      i = i + 1
      currentSum = newSum

      currentElement = currentElement * numeratorDiff / i

      newSum = currentSum + currentElement
      if (currentSum == newSum) exit
   end do

   open (file=output_file, newunit=Out)
      write(Out, '(f5.2,a,f5.2)') currentSum, ' ~ ' , a**x
   close (Out)

end program ex_6_2e
