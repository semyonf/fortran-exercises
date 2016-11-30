program lab_1_4
    use Environment
    use Emp_IO
    use Emp_Process
    implicit none

    integer                 :: occurrences(N_RECORDS) = 0
    type(employee)          :: employees(N_RECORDS), types(N_RECORDS)
    character(*), parameter :: input_file  = "../data/input.txt", &
                               output_file = "output.txt", &
                               data_file   = "test.dat"

    call Create_data_file(input_file, data_file)
    employees = Read_employee_list(data_file)
    call Output_employee_list(output_file, employees, "Исходный список:", "rewind")
    call ProcessPositions(employees, types, occurrences)
    call WritePositionsOccured(output_file, types, occurrences)

end program lab_1_4