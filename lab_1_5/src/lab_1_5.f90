program lab_1_5
   use Environment
   use Emp_IO
   use Emp_Process
   implicit none

   character(*), parameter :: Input_file  = "../data/input.txt", Output_file = "output.txt"
   type(employee), pointer :: Employee_List => null(), types => null()
   integer                 :: occurrences(N_RECORDS) = 0

   Employee_List => Read_employee_list(Input_file)

   if (Associated(Employee_List)) then
      call Output_employee_list(Output_file, Employee_List, "rewind")
      call ProcessPositions(Employee_List, types, occurrences)
      call WritePositionsOccured(Output_file, types, occurrences)
   endif

   write(*,*) sizz

end program lab_1_5
