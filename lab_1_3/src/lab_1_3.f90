program lab_1_3
    use Environment
    use Emp_IO
    use Emp_Process
    implicit none

    ! integer, parameter          :: N_RECORDS = 15, L_NAME = 15, L_POSITION = 15
    ! character(kind=CH_)         :: names(N_RECORDS, L_NAME), positions(N_RECORDS, L_POSITION), types(N_RECORDS, L_POSITION)
    ! integer                     :: In, Out, occurrences(N_RECORDS, 1)
    type(employee)          :: team(N_RECORDS)
    character(*), parameter :: input_file  = "../data/input.txt", &
                               output_file = "output.txt", &
                               data_file   = "test.dat"

    call Create_data_file(input_file, data_file)
    team = Read_employee_list(data_file)
    call Output_employee_list(output_file, team, "Исходный список:", "rewind")

end program lab_1_3