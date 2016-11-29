module Emp_Process
   ! Модуль с ЧИСТЫМИ процедурами обработки данных
   use Environment
   use Emp_IO
   implicit none

contains

! Подпроцесс для поиска и подсчета повторений профессий
    pure subroutine ProcessPositions(employees, types, occurrences)
        implicit none

        type(employee) :: employees(:), types(:)
        integer        :: occurrences(:)
        intent(in) employees
        intent(out) types, occurrences

        integer                     :: N_unique, i, duplicates
        type(employee), allocatable :: newEmployees(:)
        logical                     :: repeated(N_RECORDS)

        N_unique = 0
        i = 0
        duplicates = 0
        repeated(N_RECORDS) = .false.

        newEmployees = employees

        do while (.not. all(repeated))
            N_unique = N_unique + 1
            types(N_unique)%position = newEmployees(1)%position
            occurrences(N_unique) = count(types(N_unique)%position == employees(:)%position)
            repeated = repeated .or. types(N_unique)%position == employees(:)%position
            do i = 1, N_RECORDS
                if (repeated(i) .eqv. .true.) then
                    duplicates = duplicates + 1
                else
                    newEmployees(i - duplicates)%position = employees(i)%position
                endif
            enddo
            duplicates = 0
        enddo

    end subroutine ProcessPositions

end module Emp_Process