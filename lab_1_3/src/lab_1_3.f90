program lab_1_3
    use Environment
    use Emp_IO
    use Emp_Process
    implicit none

    integer                 :: occurrences(N_RECORDS, 1), i, j
    character(kind=CH_)     :: types(N_RECORDS, L_POSITION), positions(N_RECORDS, L_POSITION)
    type(employee)          :: employees(N_RECORDS)
    character(*), parameter :: input_file  = "../data/input.txt", &
                               output_file = "output.txt", &
                               data_file   = "test.dat"

    call Create_data_file(input_file, data_file)
    employees = Read_employee_list(data_file)
    call Output_employee_list(output_file, employees, "Исходный список:", "rewind")

    do i = 1, N_RECORDS
        positions(i,:) = [(employees(i)%position(j:j), j=1, L_POSITION)]
    enddo

    call ProcessPositions(positions, types, occurrences)
    call WritePositionsOccured(output_file, types, occurrences)

end program lab_1_3