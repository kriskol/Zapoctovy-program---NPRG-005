%Transforms cell Order, where Order is the number of cells from the "beginning"
%of sudoku to the given cell, determined by position(X,Y,Z). Where
%X represents row of the given cell (starting from 0).
%Y and Z are such that (Y*N + Z) represents 
%column of the given cell (starting from 0), where
%Z < N and N*N is the length of a side of the sudoku.
%cellOrderToCellPosition(+Order,-position(X,Y,Z),+N)
cellOrderToCellPosition(Order, position(X,Y,Z), N) :- 	
		Temp is N*N,
		X is div(Order, Temp),
		Y is div(mod(Order, Temp), N),
		Z is mod(mod(Order, Temp), N).

%Transforms cell position(X,Y,Z) to its Order. Where Order represents
%number of cells in sudoku up to this cell (not including given cell).
%X represents row of the given cell (starting from 0).
%Y and Z are such that (Y*N + Z) represents
%column of the given cell (starting from 0), where
%Z < N and N*N is the length of a side of the sudoku.
%cellPositionToCellOrder(-Order, +position(X,Y,Z), +N)
cellPositionToCellOrder(Order, position(X,Y,Z), N) :- 	
		Temp is N*N,
		Order is Z + Y * N + X * Temp.

%Takes the first N elements from the List and returns them.
%take(+N,+List,-FirstNElements)
take(0, _ , []).
take(N, [], []) :- N > 0.
take(N, [H|T], [H|Elements]) :- N > 0, NTemp is N - 1, take(NTemp, T, Elements).

%Drops the first N elements of the List
%and returns the remaining part of the given List.
%drop(+N, +List, -ListExceptFirstNElements)
drop(0, X, X).
drop(N, [], []) :- N > 0.
drop(N, [_|T], Elements) :- N > 0, NTemp is N - 1, drop(NTemp, T, Elements).

%Union given sorted Lists, and returns them sorted.
%unionSortedLists(+List1, +List2, -UnionList1List2Sorted)
unionSortedLists(List, [], List).
unionSortedLists([], [H|T], [H|T]).
unionSortedLists([H|T],[H|Xs], [H|ListNew]) :- unionSortedLists(T,Xs, ListNew).
unionSortedLists([H|T],[X|Xs], [H|ListNew]) :- H < X, unionSortedLists(T,[X|Xs], ListNew).
unionSortedLists([H|T],[X|Xs], [X|ListNew]) :- X < H, unionSortedLists([H|T], Xs, ListNew).

%Makes set theoretic difference on given Lists, List1 and List2, where both are sorted.
%And after that returns sorted result (returns List1 - List2 that is sorted).
%differenceSortedLists(+List1, +List2, -DiffList1List2Sorted)
differenceSortedLists([], [_|_], []).
differenceSortedLists(List, [], List).
differenceSortedLists([H|T], [H|Xs], List) :- differenceSortedLists(T,Xs,List).
differenceSortedLists([H|T], [X|Xs], [H|List]) :- H < X, differenceSortedLists(T,[X|Xs], List).
differenceSortedLists([H|T], [X|Xs], List) :- H > X, differenceSortedLists([H|T],Xs,List). 

%Builds List containing elements from 1 to N, in that order, and then returns it.
%buildList1ToN(+N, -List)
buildList1ToN(N, List) :-buildListMToN(1, N, List). 

%Builds List containing elements from M to N, in that order, and then returns it.
%buildListMToN(+M, +N, -List)
buildListMToN(M, N, []) :- M > N.
buildListMToN(M, N, [M|List]) :- N >= M, MTemp is M + 1, buildListMToN(MTemp, N, List). 

%Transforms row of a sudoku represented as one list
%into several lists each containing N elements.
%Where N is such that N*N is length of a side of the sudoku.
%sudokuRowTransformation(+Row, -DividedRow, +N) 
sudokuRowTransformation([], [], _) :- !.
sudokuRowTransformation(Row, [FirstNElemnts|RowTransformed], N) :- 
		take(N, Row, FirstNElemnts), 
		drop(N, Row, ElementsExceptFirstN),
		sudokuRowTransformation(ElementsExceptFirstN, RowTransformed, N).

%Transforms sudoku from the representation which represents sudoku's 
%row in one list to the representation which represents
%sudoku's row using many lists each representing exactly N cells.
%Where N is such that N*N is the length of a side of the sudoku.
%sudokuTransformation(+SudokuCells, -SudokuCellsTransformed, +N)
sudokuTransformation([],[],_).
sudokuTransformation([Row|SudokuCells], [RowTransformed|SudokuCellsTransformed], N) :-	
		sudokuRowTransformation(Row, RowTransformed, N),
		sudokuTransformation(SudokuCells, SudokuCellsTransformed,N).

%Counts the number of non-empty cells,i.e. cells that contain 0, in a partial row.
%Partial row is a part of an row so that there is a square "inside" sudoku, whose
%row is this partial row.
%countFilledPartialRowCells(+PartialRow, -Count)
countFilledPartialRowCells([],0).
countFilledPartialRowCells([0|T],Count) :- !,countFilledPartialRowCells(T,Count).
countFilledPartialRowCells([_|T],Count) :- countFilledPartialRowCells(T,CountTemp), 
					Count is CountTemp + 1.

%Counts the number of non-empty cells in a row of a sudoku.
%countFilledRowCells(+Row, -Count)
countFilledRowCells([],0).
countFilledRowCells([H|T], Count) :-
	countFilledPartialRowCells(H, CountTemp1), countFilledRowCells(T, CountTemp2), 
	Count is CountTemp1 + CountTemp2.

%Counts the number of non-empty cells in a sudoku.
%countFilledSudokuCells(+SudokuCells, -Count)
countFilledSudokuCells([], 0).
countFilledSudokuCells([Row|SudokuCells], Count) :-
		countFilledRowCells(Row, CountTemp1), 
		countFilledSudokuCells(SudokuCells, CountTemp2), 
		Count is CountTemp1 + CountTemp2.

%Finds all occupied values in a partial row of a sudoku
%and then returns them (without repetition).
%Value is occupied when there exist a cell containing given value
%in a given "scope", i.e. partial row, row,..., and it is not 0.
%findAllOccupiedValuesPartialRow(+PartialRow, -OccupiedValues)
findAllOccupiedValuesPartialRow([],[]).
findAllOccupiedValuesPartialRow([H|T],[H|OccupiedValues]) :- 
		\+member(H, T), H \= 0, !,
		findAllOccupiedValuesPartialRow(T,OccupiedValues).
findAllOccupiedValuesPartialRow([_|T],OccupiedValues) :- 
		findAllOccupiedValuesPartialRow(T,OccupiedValues).

%Finds all occupied values in a row of a sudoku
%and then returns them sorted (and without repetition).
%Value is occupied when there exist a cell containing
%given value in a given "scope", i.e. partial row, row,..., and it is not 0.
%findAllOccupiedValuesRowSorted(+Row, -OccupiedValuesRowSorted)
findAllOccupiedValuesRowSorted([],[]).
findAllOccupiedValuesRowSorted([PartialRow|Row], OccupiedValuesRowSortedFinal) :- 
		findAllOccupiedValuesPartialRow(PartialRow, OccupiedValuesPartialRow),
		sort(OccupiedValuesPartialRow, OccupiedValuesPartialRowSorted),
		findAllOccupiedValuesRowSorted(Row, OccupiedValuesRowSorted),
		unionSortedLists(OccupiedValuesPartialRowSorted, OccupiedValuesRowSorted, 
		OccupiedValuesRowSortedFinal).

%Finds all occupied values in a row of a sudoku
%and then returns them sorted (and without repetition).
%Value is occupied when there exist a cell
%containing given value in a given "scope",
%i.e. partial row, row,..., and it is not 0.
%This procedure handles finding given row
%and the rest is handled by findAllOccupiedValuesRowSorted(+Row, -OccupiedValuesRowSorted).
%X contains the number of row, i.e. which row
%from the top left corner of sudoku is the one we are looking for.
%occupiedValuesRowSorted(+SudokuCells,+X,-OccupiedValuesRowSorted)
occupiedValuesRowSorted([Row|_],0,OccupiedValuesRowSorted) :- 
		findAllOccupiedValuesRowSorted(Row, OccupiedValuesRowSorted).
occupiedValuesRowSorted([_|SudokuCells], X, OccupiedValuesRowSorted) :- 
		X > 0, XTemp is X - 1,
		occupiedValuesRowSorted(SudokuCells, XTemp, OccupiedValuesRowSorted).

%Finds whether in a given partial row on a given position, represented by Z, is non-zero value.
%If so this value is returned in a list, otherwise empty list is returned.
%findOccupiedValuePartialRow(+PartialRow,+Z,-Value)
findOccupiedValuePartialRow([0|_],0,[]) :- !.
findOccupiedValuePartialRow([H|_],0,[H]).
findOccupiedValuePartialRow([_|T],Z,Value) :- Z > 0, 
		ZTemp is Z - 1, findOccupiedValuePartialRow(T,ZTemp,Value).
 
%Finds whether in a given row on a given position,
%represented by position(Y,Z), is non-zero value.
%If so this value is returned in a list,
%otherwise empty list is returned.
%In position(Y,Z) Y represents position of a partial row
%in which we should look for a value.
%Z represents position of a cell
%in a partial row in which we should look for a value.
%findOccupiedValueRow(+Row,+position(Y,Z),-Value)
findOccupiedValueRow([PartialRow|_], position(0,Z), Value) :- 
		findOccupiedValuePartialRow(PartialRow,Z,Value).
findOccupiedValueRow([_|Row], position(Y,Z), Value) :- Y > 0, YTemp is Y - 1,
		findOccupiedValueRow(Row,position(YTemp,Z),Value).

%Finds all non-zero values,i.e. occupied values,
%in given rows determined by position(Y,Z) and returns them sorted(and withou repetition).
%In position(Y,Z) Y represents position of partial row
%in which we should look for a value for each processed row.
%Z represents position of a cell in a partial row in
%which we should look for a value for each processed row.
%occupiedValuesColumnSorted(+SudokuCells, +position(Y,Z), -OccupiedValuesColumnSorted)
occupiedValuesColumnSorted([], _, []).
occupiedValuesColumnSorted([Row|SudokuCells], position(Y,Z), OccupiedValuesColumnSorted) :- 
		findOccupiedValueRow(Row, position(Y,Z), Value),
		occupiedValuesColumnSorted(SudokuCells, position(Y,Z), OccupiedValuesColumnSortedTemp),
		unionSortedLists(OccupiedValuesColumnSortedTemp, Value, OccupiedValuesColumnSorted).		

%Finds all non-zero values,i.e. occupied values,
%in a given partial row of a given square and then returns them(without repetition).
%Y is the number of consecutive partial rows
%that are not part of the given square followed by partial row
%that is contained in the square.
%findOccupiedValuesSquareRow(+Row, +Y, -OccupiedValues)
findOccupiedValuesSquareRow([PartialRow|_], 0, OccupiedValues) :- 
		findAllOccupiedValuesPartialRow(PartialRow, OccupiedValues).
findOccupiedValuesSquareRow([_|Row], Y, OccupiedValues) :- Y > 0, 
		YTemp is Y - 1,
		findOccupiedValuesSquareRow(Row, YTemp, OccupiedValues).

%Finds all occupied values of a given square in a sudoku
%and returns them sorted (and without repetition). Where
%Y is the number of consecutive partial rows
%that are not part of the given square followed by partial row that is contained in the square.
%M is the number of rows, from the beginning
%that are part of the square in SudokuCells.
%findAllOccupiedValuesSquareSorted(+SudokuCells,+Y,+M,-OccupiedValuesSquareSorted)
findAllOccupiedValuesSquareSorted([Row|_], Y, 1, OccupiedValuesSorted) :- 
	findOccupiedValuesSquareRow(Row, Y, OccupiedValuesTemp), sort(OccupiedValuesTemp, OccupiedValuesSorted).
findAllOccupiedValuesSquareSorted([Row|SudokuCells],Y, M, OccupiedValuesSquareSorted) :- 
	M > 1,
	findOccupiedValuesSquareRow(Row, Y, OccupiedValuesSquareRow),
	MTemp is M - 1,
	findAllOccupiedValuesSquareSorted(SudokuCells, Y, MTemp, OccupiedValuesSquareSortedTemp),
	sort(OccupiedValuesSquareRow, OccupiedValuesSquareRowSorted),
	unionSortedLists(OccupiedValuesSquareRowSorted, 
	OccupiedValuesSquareSortedTemp, OccupiedValuesSquareSorted).

%Finds all occupied values of a given square in a sudoku
%and returns them sorted (and without repetition). Where
%X is the number of consecutive rows that are not part of the given square
%followed by row that is contained in the square.
%Y is the number of consecutive partial rows that are not part
%of the given square followed by partial row that is contained in the square,
%when row that contains part of the square is considered.
%N is such that N*N is the length of a side of the sudoku.
%This procedure on its own finds "starting row" of the square,
%the rest is handled by findAllOccupiedValuesSquareSorted(+SudokuCells,+Y,+M,-OccupiedValuesSquareSorted).
%occupiedValuesSquareSorted(+SudokuCells, +position(X,Y), +N, -OccupiedValuesSquareSorted)
occupiedValuesSquareSorted(SudokuCells, position(0,Y), N, OccupiedValuesSquareSorted) :- 
		findAllOccupiedValuesSquareSorted(SudokuCells, Y, N, OccupiedValuesSquareSorted).
occupiedValuesSquareSorted([_|SudokuCells], position(X,Y), N, OccupiedValuesSquareSorted) :- 
		X > 0, XTemp is X - 1, 
		occupiedValuesSquareSorted(SudokuCells, position(XTemp,Y), N, OccupiedValuesSquareSorted).

%Finds all values that are occupied in a row,
%column and square determined by position(X,Y,Z), where
%position(X,Y,Z) determines a cell in a sudoku.
%These values are then sorted and returned without repetition.
%N is such that N*N is the length of a side of the sudoku.
%occupiedValuesSorted(+SudokuCells, +position(X,Y,Z), +N, -OccupiedValuesSorted)
occupiedValuesSorted(SudokuCells, position(X,Y,Z), N, OccupiedValuesSorted) :- 	
		occupiedValuesColumnSorted(SudokuCells, position(Y,Z), 
		OccupiedValuesColumnSorted),
		occupiedValuesRowSorted(SudokuCells, X, OccupiedValuesRowSorted),
		XTemp is X - mod(X,N),
		occupiedValuesSquareSorted(SudokuCells, position(XTemp,Y), N, OccupiedValuesSquareSorted),
		unionSortedLists(OccupiedValuesColumnSorted,OccupiedValuesRowSorted, ListTempSorted),
		unionSortedLists(ListTempSorted, OccupiedValuesSquareSorted, OccupiedValuesSorted).

%Finds all values that can be inserted into a cell
%determined by the position(X,Y,Z) without violating any "sudoku" rules.
%N is such that N*N is the length of a side of the sudoku.
%possibleValues(+SudokuCells, +position(X,Y,Z), +N, -Values)
possibleValues(SudokuCells, position(X,Y,Z), N, Values) :- 
		occupiedValuesSorted(SudokuCells, position(X,Y,Z), N, OccupiedValuesSorted),
		Temp is N*N,
		buildList1ToN(Temp, List),
		differenceSortedLists(List, OccupiedValuesSorted, Values).

%Finds all values that can be inserted into a cell
%determined by the position(X,Y,Z) without violating any "sudoku" rules.
%N is such that N*N is the length of a side of the sudoku.
%If the number of Values that can be inserted into given cell is 0, then
%this procedure fails.
%findPossibilitiesForCell(+SudokuCells, +position(X,Y,Z), +N, -Values)
findPossibilitiesForCell(SudokuCells, position(X,Y,Z), N, Values) :-	
		possibleValues(SudokuCells, position(X,Y,Z), N, Values), 
		length(Values, NumberOfValues),
		NumberOfValues > 0.

%Returns values depending on the length of the list called Values.
%If the list is empty, then procedure fails.
%If the list has exactly one element, then this element and 1 are returned.
%If the list has more than one element, then two 0's are returned.
%tryChooseValuePossiblyDetCell(+Values, -Value, -Label)
tryChooseValuePossiblyDetCell(Values, 0, 0) :- length(Values, NumberValues),
				NumberValues > 1.
tryChooseValuePossiblyDetCell([Value], Value, 1).

%Fills all zero cells which have uniquely determined value
%that they can have without violating "sudoku" rules with that value, in a partial row.
%N is such that N*N is the length of a side of the sudoku.
%CurrentOrder is the order of the current cell in the sudoku.
%CountFilledPartialRowCells is the number of cells that were filled.
%SudokuCellsAltered is sudoku with so far filled cells.
%PartialRowFilledCells contains partial row with filled cells.
%SudokuCells contains whole sudoku and is used for
%the determination of whether given cell has uniquely determined value
%with which it can be filled or not.
%PartialRowCellsIterated are used for iteration over sudoku cells.
%fillDeterminedPartialRowCells(+SudokuCells,+PartialRowCellsIterated, +N, +CurrentOrder, -PartialRowFilledCells, -CountFilledPartialRowCells, -SudokuCellsAltered)
fillDeterminedPartialRowCells(SudokuCells,[],_,_,[],0,SudokuCells).
fillDeterminedPartialRowCells(SudokuCells,[0|T],N,CurrentOrder,[Value|FilledPartialRow],CountFilledPartialRowCells, SudokuCellsAltered) :-
	cellOrderToCellPosition(CurrentOrder, position(X,Y,Z),N),
	findPossibilitiesForCell(SudokuCells, position(X,Y,Z), N, Values),
	tryChooseValuePossiblyDetCell(Values, Value, FilledCell),
	fillCellPosition(SudokuCells, Value, position(X,Y,Z), SudokuCellsAlteredTemp),
	CurrentOrderTemp is CurrentOrder + 1,
	fillDeterminedPartialRowCells(SudokuCellsAlteredTemp,T, N, CurrentOrderTemp,
	FilledPartialRow, CountFilledPartialRowCellsRest, SudokuCellsAltered),
	CountFilledPartialRowCells is CountFilledPartialRowCellsRest + FilledCell.
fillDeterminedPartialRowCells(SudokuCells,[X|T],N,CurrentOrder,[X|FilledPartialRow],CountFilledPartialRowCells, SudokuCellsAltered) :- 
	X \= 0,
	CurrentOrderTemp is CurrentOrder + 1,
	fillDeterminedPartialRowCells(SudokuCells, T, N, CurrentOrderTemp, FilledPartialRow,
	CountFilledPartialRowCells,SudokuCellsAltered).

%Fills all zero cells which have uniquely determined value
%that they can have without violating "sudoku" rules with that value, in a row.
%N is such that N*N is the length of a side of the sudoku.
%CurrentOrder is the order of the current cell in the sudoku.
%CountFilledRowCells is the number of cells that were filled.
%SudokuCellsAltered is sudoku with filled cells.
%RowFilledCells contains row with filled cells.
%SudokuCells contains whole sudoku and is used for the determination
%of whether given cell has uniquely determined value with which it can be filled or not.
%RowCellsIterated are used for iteration over sudoku cells.
%fillDeterminedRowCells(+SudokuCells,+RowCellsIterated, +N, +CurrentOrder, -RowFilledCells, -CountFilledRowCells, -SudokuCellsAltered)
fillDeterminedRowCells(SudokuCells,[],_,_,[],0,SudokuCells).
fillDeterminedRowCells(SudokuCells,[PartialRow|Row], N, CurrentOrder, [FilledPartialRow|FilledRow], CountFilledCellsRow, SudokuCellsAltered) :-
	fillDeterminedPartialRowCells(SudokuCells, PartialRow, N, CurrentOrder, FilledPartialRow, 
	CountFilledCellsPartialRow, SudokuCellsAlteredTemp),
	CurrentOrderTemp is CurrentOrder + N,
	fillDeterminedRowCells(SudokuCellsAlteredTemp, Row, N, CurrentOrderTemp, FilledRow, 
	CountFilledCellsRowRest,SudokuCellsAltered),
	CountFilledCellsRow is CountFilledCellsPartialRow + CountFilledCellsRowRest.

%Fills all zero cells which have uniquely determined value
%that they can have without violating "sudoku" rules with that value, in the whole sudoku.
%N is such that N*N is the length of a side of the sudoku.
%CurrentOrder is the order of the current cell in the sudoku.
%CountFilledCells is the number of cells that were filled.
%SudokuCellsAltered is whole sudoku with filled cells.
%FilledCells contains sudoku with filled cells.
%SudokuCells contains whole sudoku and is used for the determination of whether given cell
%has uniquely determined value with which it can be filled or not.
%CellsIterated are used for iteration over sudoku cells.
%fillDeterminedSudokuCells(+SudokuCells,+CellsIterated, +N, +CurrentOrder, -FilledCells, -CountFilledCells, -SudokuCellsAltered)
fillDeterminedSudokuCells(SudokuCells,[],_, _, [],0,SudokuCells).
fillDeterminedSudokuCells(SudokuCells,[Row|SudokuCellsRest], N, CurrentOrder, [FilledRow| FilledSudokuCells],CountFilledCells, SudokuCellsAltered) :-
	fillDeterminedRowCells(SudokuCells, Row, N, CurrentOrder, FilledRow, CountFilledCellsRow, SudokuCellsAlteredTemp),
	CurrentOrderTemp is CurrentOrder + N*N,
	fillDeterminedSudokuCells(SudokuCellsAlteredTemp, SudokuCellsRest, N, CurrentOrderTemp, FilledSudokuCells, 
	CountFilledSudokuCells,SudokuCellsAltered),
	CountFilledCells is CountFilledCellsRow + CountFilledSudokuCells.

%Fills all zero cells which have uniquely determined value
%that they can have without violating "sudoku" rules with that value, in the whole sudoku.
%N is such that N*N is the length of a side of the sudoku.
%If at at least on cell is filled in the sudoku at the end
%of the procedure fillDeterminedSudokuCells, then this procedure
%run procedure fillDeterminedSudokuCells on the newly gained sudoku again.
%Otherwise it returns newly gained sudoku with
%probably some celss filled.
%fillDeterminedCells(+SudokuCellsFilled, +CountFilledCells, +N, +CountFilledCellsPrevious, -SudokuCellsCellFilledFinal, -CountFilledCellsFinel)
fillDeterminedCells(SudokuCellsFilledFinal, CountFilledCellsFinal, _, 0, SudokuCellsFilledFinal, CountFilledCellsFinal) :- !.
fillDeterminedCells(SudokuCellsFilled, CountFilledCells, N, CountFilledCellsPrevious, SudokuCellsFilledFinal, CountFilledCellsFinal) :-
	CountFilledCellsPrevious > 0,
	fillDeterminedSudokuCells(SudokuCellsFilled, SudokuCellsFilled,
	N, 0, SudokuCellsFilledLast, CountFilledCellsLast,_),
	CountFilledCellsTemp is CountFilledCells + CountFilledCellsLast,
	fillDeterminedCells(SudokuCellsFilledLast,CountFilledCellsTemp,N,
	CountFilledCellsLast, SudokuCellsFilledFinal, CountFilledCellsFinal).

%Fills all zero cells which have uniquely determined value 
%that they can have without violating "sudoku" rules with that value, in the whole sudoku.
%N is such that N*N is the length of a side of the sudoku.
%This procedure achieve its goal with the help of the procedure fillDeterminedCells.
%fillDeterminedCells(+SudokuCells, +N, -SudokuCellsFilled, -CountFilledCells)
fillDeterminedCells(SudokuCells, N, SudokuCellsFilled, CountFilledCells) :- 
	fillDeterminedCells(SudokuCells, 0, N, 1, SudokuCellsFilled, CountFilledCells).

%Following procedure takes two terms best(X,Y) and returns term with lower value of Y.
%lowerPossibilities(+Best1, +Best2, -Best)
lowerPossibilities(best(_,NumberOfPossibilities), best(OrderNew, NumberPossibilitesNew), best(OrderNew, NumberPossibilitesNew)) :- 
							NumberPossibilitesNew < NumberOfPossibilities,!. 
lowerPossibilities(best(Order, NumberOfPossibilities), _, best(Order, NumberOfPossibilities)).

%Finds an order of a zero cell in a given partial row that 
%has the lowest number of possible values that can be filled
%in that cell without violating "sudoku" rules.
%But the number of possibilities of all zero cells has to be greater than 0,
%otherwise the procedure fails.
%N is such that N*N is the length of a side of the sudoku.
%CurrentOrder is the order of a current cell in the sudoku.
%PartialRowCellsIterated are sudoku cells that are used for
%iterating over sudoku cells.
%findCellPartialRowSmallestPossibilities(+SudokuCells, +PartialRowcellsIterated, +CurrentOrder, +N, +CurrentBest, -Best) 
findCellPartialRowSmallestPossibilities(_, [], _, _, Best, Best).
findCellPartialRowSmallestPossibilities(SudokuCells, [0|T], CurrentOrder, N, CurrentBest, Best) :-
	cellOrderToCellPosition(CurrentOrder, position(X,Y,Z), N),
	findPossibilitiesForCell(SudokuCells, position(X,Y,Z), N, Values),
	length(Values, NumberOfValues),
	lowerPossibilities(CurrentBest, best(CurrentOrder, NumberOfValues), 
	Better),
	CurrentOrderTemp is CurrentOrder + 1,
	findCellPartialRowSmallestPossibilities(SudokuCells, T, CurrentOrderTemp, 
	N, Better, Best).
findCellPartialRowSmallestPossibilities(SudokuCells, [X|T], CurrentOrder, N, CurrentBest,Best) :- 
	X \= 0,
	CurrentOrderTemp is CurrentOrder + 1,
	findCellPartialRowSmallestPossibilities(SudokuCells,T,CurrentOrderTemp,N,CurrentBest,Best).


%Finds an order of a zero cell in a given row
%that has the lowest number of possible values
%that can be filled in that cell without violating "sudoku" rules.
%But the number of possibilities of all zero cells has to be greater than 0,
%otherwise the procedure fails.
%N is such that N*N is the length of a side of the sudoku.
%CurrentOrder is the order of a current cell in the sudoku.
%RowCellsIterated are sudoku cells that are used for iterating over sudoku cells.
%findCellRowSmallestPossibilities(+SudokuCells, +RowCellsIterated, +CurrentOrder, +N, +CurrentBest, -Best)
findCellRowSmallestPossibilities(_, [], __, _, Best, Best).
findCellRowSmallestPossibilities(SudokuCells, [PartialRow|Row], CurrentOrder, N, CurrentBest, Best) :-
	findCellPartialRowSmallestPossibilities(SudokuCells, PartialRow, CurrentOrder,
	N, CurrentBest, Better),
	CurrentOrderTemp is CurrentOrder + N,
	findCellRowSmallestPossibilities(SudokuCells, Row, CurrentOrderTemp, N,
	Better, Best). 

%Finds an order of a zero cell in a given sudoku
%that has the lowest number of possible values
%that can be filled in that cell without violating "sudoku" rules.
%But the number of possibilities of all zero cells has to be greater than 0,
%otherwise the procedure fails.
%N is such that N*N is the length of a side of the sudoku.
%CurrentOrder is the order of a current cell in the sudoku.
%SudokuCellsIterated are sudoku cells that are used for iterating over sudoku cells.
%findCellSudokuSmallestPossibilities(+SudokuCells, +SudokuCellsIterated, +CurrentOrder, +N, +CurrentBest, -Best)
findCellSudokuSmallestPossibilities(_,[],_,_, Best, Best).
findCellSudokuSmallestPossibilities(SudokuCells,[Row |SudokuCellsRest], CurrentOrder, N, CurrentBest, Best) :-
	findCellRowSmallestPossibilities(SudokuCells, Row, CurrentOrder,
	N, CurrentBest,Better),
	CurrentOrderTemp is CurrentOrder + N*N,
	findCellSudokuSmallestPossibilities(SudokuCells, SudokuCellsRest, CurrentOrderTemp, N,
	Better, Best).

%Finds an order of a zero cell in a given sudoku
%that has the lowest number of possible values that can be filled in
%that cell without violating "sudoku" rules.
%But the number of possibilities of all zero cells has to be greater than 0,
%otherwise the procedure fails.
%N is such that N*N is the length of a side of the sudoku.
%This procedure only calls findCellSudokuSmallestPossibilities
%with starting value of CurrentBest as best(-1,N*N+1),
%so that if there is zero cell that can be filled with some value without violating "sudoku" rules
%it will be at least once considered as best,
%if no other zero cell were considered best until then.
%If there is no zero cell, then -1 is returned.
%findCellSmallestPossibilities(+SudokuCells, +N, -Best)
findCellSmallestPossibilities(SudokuCells, N, Order) :- 
	NumberOfPossibilitiesStart is  N*N + 1,
	findCellSudokuSmallestPossibilities(SudokuCells, SudokuCells, 0,
	N, best(-1, NumberOfPossibilitiesStart), best(Order, _)).

%Fill cell with a Value in a given partial row on a positon determined by Z
%and then returns the modified partial row.
%fillCellPartialRowCells(+PartialRow, +Value, +Z, -PartialRowFilled)
fillCellPartialRowCells([_|T], Value, 0, [Value|T]).
fillCellPartialRowCells([H|T], Value, Z, [H|PartialRowFilled]) :- 
	Z > 0, ZTemp is Z - 1, 
	fillCellPartialRowCells(T, Value, ZTemp, PartialRowFilled).

%Fill cell with a Value in a given row on a positon determined by position(Y,Z)
%and then returns the modified row.
%fillCellRowPosition(+Row, +Value, +position(Y,Z), -RowFilled)							
fillCellRowPosition([H|T], Value, position(0,Z), [PartialRowFilled|T]) :- 
			fillCellPartialRowCells(H, Value, Z, PartialRowFilled).
fillCellRowPosition([H|T], Value, position(Y,Z), [H|RowFilled]) :- 
			Y > 0, YTemp is Y - 1,
			fillCellRowPosition(T,  Value, position(YTemp,Z), RowFilled).	

%Fill cell with a Value in a given sudoku on a positon
%determined by position(X,Y,Z) and then returns the modified sudoku.
%fillCellPosition(+Sudoku, +Value, +position(X,Y,Z), -SudokuFilled)	
fillCellPosition([Row|SudokuCells], Value, position(0,Y,Z), [RowFilled|SudokuCells]) :-	
	fillCellRowPosition(Row, Value, position(Y,Z), RowFilled).
fillCellPosition([Row|SudokuCells], Value, position(X,Y,Z), [Row|SudokuCellsCellFilled]) :- 
	X > 0, XTemp is X - 1, 
	fillCellPosition(SudokuCells, Value, position(XTemp,Y,Z), SudokuCellsCellFilled).

%If the Order is -1, then the  input is returned
%together with 0 that represents that no cell was modified.
%Otherwise this procedure fills the cell determined by Order
%in the sudoku with some Value that doesn't violate "sudoku" rules.
%And then returns the modified sudoku. It also returns 1 symbolising that one cell was modified.
%fillCellOrderNonDet(+SudokuCells, +Order, -SudokuCellsModified, -Label, +N)
fillCellOrderNonDet(SudokuCells, -1, SudokuCells, 0, _) :- !.
fillCellOrderNonDet(SudokuCells, Order, SudokuCellsCellFilled, 1, N) :- 
	cellOrderToCellPosition(Order, position(X,Y,Z), N),
	possibleValues(SudokuCells, position(X,Y,Z), N, Values),
	member(Value, Values),
	fillCellPosition(SudokuCells, Value, position(X,Y,Z), SudokuCellsCellFilled).

%Following procedure solves sudoku represented with "divided" rows,
%i.e. row is represented with partial rows.
%if there exist a way to fill all cells in the sudoku without violating "sudoku" rules,
%then the sudoku with all cells filled is returned.
%Otherwise this procedure fails.
%N is such that N*N is the length of a side of the sudoku.
%It keeps track of the number of filled cells and if the number of filled cells is
%the number of all sudoku cells, then the current sudoku is returned.
%sudokuSolver(+SudokuCells, +N, -SudokuCellsFilled, -FilledCells) 
sudokuSolver(SudokuCells, N, SudokuCells, FilledCellsTotal) :- 
	FilledCellsTotal =:= N * N * N * N, !.
sudokuSolver(SudokuCells, N, SudokuCellsFilled, FilledCellsTotal) :-
	fillDeterminedCells(SudokuCells, N, SudokuCellsPartiallyFilledDet, CountFilledCellsDet),
	findCellSmallestPossibilities(SudokuCellsPartiallyFilledDet, N, Order),   
	fillCellOrderNonDet(SudokuCellsPartiallyFilledDet, Order,
	SudokuCellsPartiallyFilledNonDet,  FilledCellNonDet, N),
	FilledCellsTotalNew is FilledCellsTotal + FilledCellNonDet + CountFilledCellsDet,
	sudokuSolver(SudokuCellsPartiallyFilledNonDet, N,
	SudokuCellsFilled, FilledCellsTotalNew).
																		
%This procedure solves given sudoku with the help of the procedure sudokuSolver.
%if there exist a way to fill all cells in the sudoku without violating "sudoku" rules,
%then the sudoku with all cells filled is returned.
%Otherwise this procedure fails.
%N is such that N*N is the length of a side of the sudoku.
%solveDividedRowSudoku(+SudokuCells, +N, -SudokuCellsFilled)
solveDividedRowSudoku(SudokuCells, N, SudokuCellsFilled) :- 
	countFilledSudokuCells(SudokuCells, FilledCellsTotal),
	sudokuSolver(SudokuCells, N, SudokuCellsFilled, FilledCellsTotal).

%This procedure solves given sudoku with the help of the procedure solveDividedRowSudoku.
%In order to do that, 
%sudoku is transformed in a way that it has divided rows into partial rows.
%if there exist a way to fill all cells in the sudoku without violating "sudoku" rules,
%then the sudoku with all cells filled is returned.
%Otherwise this procedure fails.
%N is such that N*N is the length of a side of a sudoku.
%solveWholeRowSudoku(+SudokuCells, +N, -SudokuCellsFilled)
solveWholeRowSudoku(SudokuCells, N, SudokuCellsFilled) :-
	sudokuTransformation(SudokuCells, SudokuCellsTransformed, N),
	solveDividedRowSudoku(SudokuCellsTransformed, N, SudokuCellsFilled).


%On the following lines are few queries that can be used for testing purposes.

%Sudoku 9*9 with difficulty not determined.
solveInpt3(SudokuCellsFilled) :- solveWholeRowSudoku([[0,8,0,0,0,0,4,1,0],[0,1,0,2,0,5,0,0,9],[0,7,0,0,0,8,5,0,0],
						[9,0,0,7,6,0,0,0,8],[0,0,0,0,0,0,0,0,0],[6,0,0,0,9,2,0,0,3],
						[0,0,7,8,0,0,0,2,0],[8,0,0,3,0,7,0,6,0],[0,6,3,0,0,0,0,4,0]],
						3,SudokuCellsFilled).

%Sudoku easy 9*9 with difficulty determined as easy (according to one "sudoku" site).
solveInpt3Easy(SudokuCellsFilled) :- solveWholeRowSudoku([[0,0,0,0,0,9,0,0,0],[0,0,0,0,7,8,5,0,2],[0,5,8,3,0,0,0,7,0],
							[0,2,9,0,1,0,0,0,0],[3,0,0,0,0,0,0,0,6],[0,0,0,0,6,0,2,4,0],
							[0,4,0,0,0,2,7,6,0],[8,0,5,4,3,0,0,2,0],[0,0,0,1,0,0,0,0,0]],
							3,SudokuCellsFilled).

%Sudoku medium 9*9 with difficulty determined as medium (according to one "sudoku" site).
solveInpt3Medium(SudokuCellsFilled) :- solveWholeRowSudoku([[1,0,0,6,7,0,0,0,2],[5,7,3,1,9,2,0,6,0],[0,0,0,0,4,0,0,0,7],
							[0,8,0,9,0,0,3,0,0],[0,0,0,0,0,0,0,0,0],[4,0,0,0,3,0,7,8,0],
							[0,0,0,0,5,0,6,0,3],[0,0,5,0,0,0,0,0,0],[0,2,9,0,8,0,0,4,0]],
							3,SudokuCellsFilled).

%Sudoku hard 9*9 with difficulty determined as hard (according to one "sudoku" site).
solveInpt3Hard(SudokuCellsFilled) :- solveWholeRowSudoku([[0,0,0,0,4,0,9,0,0],[0,0,5,0,0,0,7,0,3],[3,0,0,7,0,0,0,0,6],
							[0,2,0,1,0,0,0,0,0],[1,0,0,4,0,5,0,6,9],[0,4,3,0,0,0,0,1,8],
							[0,0,6,5,0,0,0,9,0],[0,8,2,0,0,9,4,0,0],[0,0,0,0,3,0,0,0,0]],
							3,SudokuCellsFilled).

%Sudoku 16*16 with difficulty not determined.
solveInpt4Random(SudokuCellsFilled) :- solveWholeRowSudoku([[1,0,0,2,3,4,0,0,12,0,6,0,0,0,7,0],
							[0,0,8,0,0,0,7,0,0,3,0,0,9,10,6,11],
							[0,12,0,0,10,0,0,1,0,13,0,11,0,0,14,0],
							[3,0,0,15,2,0,0,14,0,0,0,9,0,0,12,0],
							[13,0,0,0,8,0,0,10,0,12,2,0,1,15,0,0],
							[0,11,7,6,0,0,0,16,0,0,0,15,0,0,5,13],
							[0,0,0,10,0,5,15,0,0,4,0,8,0,0,11,0],
							[16,0,0,5,9,12,0,0,1,0,0,0,0,0,8,0],
							[0,2,0,0,0,0,0,13,0,0,12,5,8,0,0,3],
							[0,13,0,0,15,0,3,0,0,14,8,0,16,0,0,0],
							[5,8,0,0,1,0,0,0,2,0,0,0,13,9,15,0],
							[0,0,12,4,0,6,16,0,13,0,0,7,0,0,0,5],
							[0,3,0,0,12,0,0,0,6,0,0,4,11,0,0,16],
							[0,7,0,0,16,0,5,0,14,0,0,1,0,0,2,0],
							[11,1,15,9,0,0,13,0,0,2,0,0,0,14,0,0],
							[0,14,0,0,0,11,0,2,0,0,13,3,5,0,0,12]],
		  					4,SudokuCellsFilled).
