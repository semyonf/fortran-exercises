module Emp_IO
   use Environment

   implicit none

   integer, parameter :: L_NAME = 15, L_OCCUPATION = 15

   ! Структура для хранения данных о сотруднике
   type employee
      character(L_NAME, kind=CH_)       :: Name       = ""
      character(L_OCCUPATION, kind=CH_) :: Occupation = ""
      type(employee), pointer           :: Next       => Null()
   end type employee

   ! Структура для хранения данных о специальности
   type occupation
      character(L_OCCUPATION, kind=CH_) :: Name        = ""
      integer                           :: Occurrences = 0
      type(occupation), pointer         :: Next        => Null()
   end type occupation

contains

   ! Чтение списка сотрудников из файла
   function Read_Employee_List(InputFile) result(EmployeeList)

      intent(in) InputFile

      character(*)             :: InputFile
      type(employee), pointer  :: EmployeeList
      integer                  :: In

      open(file=InputFile, encoding=E_, newunit=In)
         EmployeeList => Read_Employee(In)
      close(In)
   end function Read_Employee_List

   ! ------------------------------СОТРУДНИКИ------------------------------

   ! Чтение сотрудника
   recursive function Read_Employee(In) result(dude)

      intent(in) In

      type(employee), pointer :: dude
      character(*), parameter :: format = '(2a15)'
      integer                 :: In, IO

      allocate(dude)
      read (In, format, iostat=IO) dude%name, dude%occupation

      call Handle_IO_status(IO, "reading line from file")
      if (IO == 0) then
         dude%next => Read_Employee(In)
      else
         deallocate (dude)
         nullify (dude)
      end if
   end function Read_Employee

   ! Вывод списка сотрудников в файл
   subroutine Output_Employee_List(OutputFile, EmployeeList)

      intent(in) OutputFile, EmployeeList

      character(*)   :: OutputFile
      type(employee) :: EmployeeList
      integer        :: Out

      open (file=OutputFile, encoding=E_, Position="rewind", newunit=Out)
         write(out, '(a)') '---Исходный список:---'
         call Output_Employee(Out, EmployeeList)
      close (Out)
   end subroutine Output_Employee_List

   ! Вывод сотрудника
   recursive subroutine Output_Employee(Out, EmployeeList)

      intent(in) Out, EmployeeList

      integer                 :: Out, IO
      type(employee)          :: EmployeeList
      character(*), parameter :: format = '(2a15)'

      write (Out, format, iostat=IO) EmployeeList%name, EmployeeList%occupation
      call Handle_IO_status(IO, "writing employee")

      if (Associated(EmployeeList%next)) &
         call Output_Employee(Out, EmployeeList%next)
   end subroutine Output_Employee

   ! ------------------------------ПРОФЕССИИ------------------------------

   ! Вывод списка профессий в файл
   subroutine Output_Occupation_List(OutputFile, OccupationList)

      intent(in) OutputFile, OccupationList

      character(*)   :: OutputFile
      type(occupation) :: OccupationList
      integer        :: Out

      open (file=OutputFile, encoding=E_, Position="append", newunit=Out)
         write(out, '(a)') '-----Профессии:-----'
         call Output_Occupation(Out, OccupationList)
      close (Out)
   end subroutine Output_Occupation_List

   ! Вывод сотрудника
   recursive subroutine Output_Occupation(Out, OccupationList)

      intent(in) Out, OccupationList

      integer                 :: Out, IO
      type(occupation)        :: OccupationList
      character(*), parameter :: format = '(a,I3)'

      write (Out, format, iostat=IO) OccupationList%name, OccupationList%Occurrences
      call Handle_IO_status(IO, "writing occupation")

      if (Associated(OccupationList%next)) &
         call Output_Occupation(Out, OccupationList%next)
   end subroutine Output_Occupation


end module Emp_IO
