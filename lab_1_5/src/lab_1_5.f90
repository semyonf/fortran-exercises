program lab_1_5
   use Environment
   use Emp_IO
   use Emp_Process

   implicit none

   character(*), parameter   :: InputFile  = "../data/input.txt", OutputFile = "output.txt"
   type(employee), pointer   :: EmployeeList => null()
   type(occupation), pointer :: OccupationList => null()

   EmployeeList => Read_Employee_List(InputFile)

   allocate(OccupationList)

   if (Associated(EmployeeList)) then
      call Output_Employee_List(OutputFile, EmployeeList)
      OccupationList => Process_Occupations_From(EmployeeList)
      call Output_Occupation_List(OutputFile, OccupationList)
   endif

end program lab_1_5
