module Emp_Process
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

        integer                     :: N_unique, i, duplicates, records
        type(employee), allocatable :: filtered(:)
        logical, allocatable        :: repeated(:)

        records = size(employees(:))
        allocate(repeated(records))

        N_unique = 1
        duplicates = 0
        repeated = .false.
        filtered = employees

        do while (.not. all(repeated))
            N_unique = N_unique + 1
            types(N_unique)%position = filtered(1)%position
            occurrences(N_unique) = count(types(N_unique)%position == employees(:)%position)
            repeated = repeated .or. types(N_unique)%position == employees(:)%position
            do i = 1, records
                if (repeated(i) .eqv. .true.) then
                    duplicates = duplicates + 1
                else
                    filtered(i - duplicates)%position = employees(i)%position
                endif
            enddo
            duplicates = 0
        enddo
    end subroutine ProcessPositions

end module Emp_Process