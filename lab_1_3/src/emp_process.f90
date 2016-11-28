module Emp_Process
   ! Модуль с ЧИСТЫМИ процедурами обработки данных
   use Environment
   use Emp_IO
   implicit none

contains

! Подпроцесс для поиска и подсчета повторений профессий
    pure subroutine ProcessPositions(positions, types, occurrences)
        implicit none

        character(kind=CH_) positions(:,:), types(:,:)
        integer occurrences(:,:)
        intent(in)  positions
        intent(out) types, occurrences

        integer                          :: N_unique = 0, i = 0, duplicates = 0
        character(kind=CH_), allocatable :: newPositions(:,:)
        logical                          :: repeated(N_RECORDS) = .false.

        newPositions = positions

        do while (.not. all(repeated))
            N_unique = N_unique + 1
            types(N_unique, :) = newPositions(1, :)
            occurrences(N_unique,1) = count([(all(types(N_unique,:) == positions(i,:)), i = 1, N_RECORDS)])
            repeated = repeated .or. [(all(types(N_unique,:) == positions(i,:)), i = 1, N_RECORDS)]
            do i = 1, N_RECORDS
                if (repeated(i) .eqv. .true.) then
                    duplicates = duplicates + 1
                else
                    newPositions(i - duplicates,:) = positions(i,:)
                endif
            enddo
            duplicates = 0
        enddo

    end subroutine ProcessPositions

end module Emp_Process