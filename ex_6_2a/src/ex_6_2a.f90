! 6.2a в учебнике

program ex_6_2a
   implicit none

   integer, parameter      :: R_ = 16
   real(R_), parameter     :: PI = 4 * ATAN(1.0)
   real(R_)                :: x = 0, numerator = 0, currentElement = 0, currentSum = 0, newSum = 0
   integer                 :: In = 0, Out = 0, denominator = 0, i = 0, denominator_fact = 0, denominator_mul = 0
   character(*), parameter :: output_file = "output.txt", input_file = "../data/input.txt"

   open (file=input_file, newunit=In)
      read(In,*) x
   close (In)

   currentElement   = 1
   numerator        = 1
   denominator      = 1
   denominator_fact = 1

   do
      i = i + 1
      newSum = currentSum + currentElement
      if (currentSum == newSum .or. i > 12) exit

      currentSum = newSum

      numerator = numerator * x**2

      denominator_fact = denominator_fact * i
      denominator_mul = (2 * i) + 1
      denominator = denominator_fact * denominator_mul

      currentElement = numerator / denominator
      if (mod(i,2) == 1) currentElement = -currentElement
   end do

   open (file=output_file, newunit=Out)
      write(Out, '(f5.2,a,f5.2)') (2 * x) / sqrt(PI) * currentSum, ' ~' , erf(x)
   close (Out)

end program ex_6_2a
