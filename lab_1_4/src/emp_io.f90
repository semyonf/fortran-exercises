module Emp_IO
    use Environment
    implicit none

    integer, parameter :: N_RECORDS  = 15, &
                          L_NAME     = 15, &
                          L_POSITION = 15
 
    ! Структура данных для хранения данных о сотруднике
    type employee
        character(L_NAME, kind=CH_)     :: name     = ""
        character(L_POSITION, kind=CH_) :: position = ""
    end type employee

contains

    ! Создание неформатированного файла данных
    subroutine Create_data_file(input_file, data_file)
        implicit none

        character(*), intent(in)  :: input_file, data_file

        type(employee)            :: emp
        integer                   :: In, Out, IO, i, recl
        character(:), allocatable :: format

        open (file=Input_File, encoding=E_, newunit=In)
        recl = (L_NAME + L_POSITION) * CH_
        open (file=Data_File, form='unformatted', newunit=Out, access='direct', recl=recl)

        format = '(2a15)'
        do i = 1, N_RECORDS
            read(In, format, iostat=IO) emp
            call Handle_IO_status(IO, "reading formatted team list, line " // i)

            write(Out, iostat=IO, rec=i) emp
            call Handle_IO_status(IO, "creating unformatted file with team list, record " // i)
        end do

        close (In)
        close (Out)
    end subroutine Create_data_file

    ! Чтение списка сотрудников
    function Read_employee_list(data_file) result(employees)
        implicit none
        type(employee)           :: employees(N_RECORDS)
        character(*), intent(in) :: data_file

        integer :: In, IO, recl
        
        recl = (L_NAME + L_POSITION) * CH_ * N_RECORDS
        open (file=Data_File, form='unformatted', newunit=In, access='direct', recl=recl)
            read(In, iostat=IO, rec=1) employees
            call Handle_IO_status(IO, "reading unformatted team list")
        close (In)
    end function Read_employee_list
 
   ! Вывод списка сотрудников
    subroutine Output_employee_list(output_file, employees, List_name, position)
        implicit none
        character(*), intent(in)   :: output_file, list_name, position
        type(employee), intent(in) :: employees(:)
  
        integer                   :: Out, IO
        character(:), allocatable :: format
        
        open (file=Output_File, encoding=E_, position=position, newunit=Out)
            write(out, '(a)') List_name
            format = '(2a15)'
            write(Out, format, iostat=IO) employees
            call Handle_IO_status(IO, "writing " // List_name)
        close (Out)
    end subroutine Output_employee_list

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