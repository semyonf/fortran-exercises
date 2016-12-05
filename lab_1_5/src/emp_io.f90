module Emp_IO
   use Environment
   implicit none

   integer, parameter :: N_RECORDS = 15, L_NAME = 15, L_POSITION = 15

   ! Структура данных для хранения данных о сотруднике
   type employee
      character(L_NAME, kind=CH_)     :: name     = ""
      character(L_POSITION, kind=CH_) :: position = ""
      type(employee), pointer         :: next     => Null()
   end type employee

contains

   ! Чтение списка сотрудников: имена и должности
   function Read_employee_list(Input_File) result(Employee_List)
      type(employee), pointer  :: Employee_List
      character(*), intent(in) :: Input_File
      integer In

      open(file=Input_File, encoding=E_, newunit=In)
         Employee_List => Read_employee(In)
      close(In)
   end function Read_employee_list

   ! Чтение следующего сотрудника
   recursive function Read_employee(In) result(dude)
      type(employee), pointer :: dude
      integer, intent(in)     :: In
      integer  IO
      character(:), allocatable :: format

      allocate (dude)

      format = '(2a15)'
      read (In, format, iostat=IO) dude%name, dude%position
      call Handle_IO_status(IO, "reading line from file")
      if (IO == 0) then
         dude%next => Read_employee(In)
      else
         deallocate (dude)
         nullify (dude)
      end if
   end function Read_employee

   ! Вывод списка сотрудников
   subroutine Output_employee_list(output_file, employees, List_name, Position)
      implicit none
      character(*), intent(in)   :: output_file, list_name, Position
      type(employee), intent(in) :: employees
      integer :: Out

      open (file=Output_File, encoding=E_, Position=Position, newunit=Out)
         write(out, '(a)') List_name
         call Output_employee(Out, employees)
      close (Out)
   end subroutine Output_employee_list

   ! Процедура для вывода каждого сотрудника
   recursive subroutine Output_employee(Out, dude)
      integer, intent(in)        :: Out
      type(employee), intent(in) :: dude

      integer :: IO
      character(:), allocatable :: format

      format = '(2a15)'
      write (Out, format, iostat=IO) dude%name, dude%position
      call Handle_IO_status(IO, "writing employee")
      if (Associated(dude%next)) &
         call Output_employee(Out, dude%next)
   end subroutine Output_employee

   ! --------------------------------------

   ! Подпроцесс для записи списка профессий
   subroutine WritePositionsOccured(output_file, types, occurrences)
      implicit none

      character(*)   output_file
      type(employee) types(:)
      integer        occurrences(:)
      intent(in) output_file, types, occurrences

      integer :: Out, IO, i
      character(:), allocatable :: format
      format = '(a,a,i)'

      open (file=output_file, encoding=E_, position="append", newunit=Out)
         write(Out, '(/a)') 'Количество профессий:'
         do i = 0, count(occurrences /= 0)
            write(Out, format, iostat=IO) types(i)%position, ' -> ',occurrences(i)
         enddo
         call Handle_IO_status(IO, 'WritePositionsOccured')
      close (Out)
   end subroutine WritePositionsOccured

end module Emp_IO
