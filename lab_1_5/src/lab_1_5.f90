program lab_1_5
   use Environment
   use Emp_IO
   use Emp_Process
   implicit none

   character(*), parameter :: input_file  = "../data/input.txt", output_file = "output.txt"
   type(employee), pointer :: employees => null(), types => null()
   integer                 :: occurrences(N_RECORDS) = 0

   employees => Read_employee_list(input_file)

   if (Associated(employees)) then
      call Output_employee_list(output_file, employees, "Исходный список:", "rewind")
      call ProcessPositions(employees, types, occurrences)
      call WritePositionsOccured(output_file, types, occurrences)
   endif

end program lab_1_5
