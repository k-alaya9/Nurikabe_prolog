% Fixed cells with their numbers
fxd_cell(1, 2, 7).
fxd_cell(3,2,2).
fxd_cell(3,4,1).
fxd_cell(5,4,1).


% Solved cells with their colors (sea or green)
solve_cell(1, 2, green).
solve_cell(3,2,green).
solve_cell(3,4,green).
solve_cell(5,4,green).


solve_cell(1, 1,green).
solve_cell(1,3,green).
solve_cell(1,4,green).
solve_cell(1,5,green).
solve_cell(2,1,green).
solve_cell(2,2,sea).
solve_cell(2,3,sea).
solve_cell(2,4,sea).
solve_cell(2,5,green).
solve_cell(3,1,sea).
solve_cell(3,3,sea).
solve_cell(3,5,sea).
solve_cell(4,1,sea).
solve_cell(4,2,green).
solve_cell(4,3,sea).
solve_cell(4,4,sea).
solve_cell(4,5,sea).
solve_cell(5,1,sea).
solve_cell(5,2,sea).
solve_cell(5,3,sea).
solve_cell(5,5,sea).


%  to get all the green neighbors vertical or horzintal and put them in a list 
green_neighbors(R, C, GreenNeighbors) :-
    findall((R, C1),
            (C1 is C + 1, solve_cell(R, C1, green);
             C1 is C - 1, solve_cell(R, C1, green);
             R1 is R + 1, solve_cell(R1, C, green);
             R1 is R - 1, solve_cell(R1, C, green)),
            GreenNeighbors).
%  to get all the sea neighbors vertical or horzintal and put them in a list 
sea_neighbors(R, C, SeaNeighbors) :-
    findall((R, C1),
            (C1 is C + 1, solve_cell(R, C1, sea);
             C1 is C - 1, solve_cell(R, C1, sea);
             R1 is R + 1, solve_cell(R1, C, sea);
             R1 is R - 1, solve_cell(R1, C, sea)),
            SeaNeighbors).

% check if all seas have neighbors or they are not 
check_all_sea_cells_have_neighbors([]).
check_all_sea_cells_have_neighbors([(Row, Col)|Rest]) :-
    sea_neighbors(Row, Col, Neighbors),
    Neighbors \= [],
    check_all_sea_cells_have_neighbors(Rest).
% to check if all the sea cells are connected 
one_wall :-
    all_column_sea_cells(Column),
    all_sea_cells(Row),
    findall(Result, merge_with_common_element(Row, Column, Result), Finalist),
    findall(X, (merge_with_common_element_final(Finalist, R), sort(R, X)), FinalList),
    remove_duplicates(FinalList, UniqueFinalList),
    merge_lists_with_common_elements(UniqueFinalList, MergedFinalList),
    !,
    length(MergedFinalList,L),
    L =:=1 ,
    findall((Row, Col),
            (solve_cell(Row, Col, sea)),
            AllseaCells),
    check_all_sea_cells_have_neighbors(AllseaCells).

no_2_by_2_wall :-
    \+ (solve_cell(R, C, sea),
        solve_cell(R, C1, sea), C1 is C + 1,
        solve_cell(R1, C, sea), R1 is R + 1,
        solve_cell(R1, C1, sea)).

green_cell_between_two_fixed :-
    fxd_cell(R1, C1, _),
    fxd_cell(R2, C2, _),
    (   R1 =:= R2,
        C1 < C2,
        solve_cell(R1, C, green),
        C > C1, C < C2
    ;   C1 =:= C2,
        R1 < R2,
        solve_cell(R, C1, green),
        R > R1, R < R2
    ).
check_all_green_cells_have_neighbors([]).
check_all_green_cells_have_neighbors([(Row, Col)|Rest]) :-
 (fxd_cell(Row, Col, 1)
    ->  true
    ;
    green_neighbors(Row, Col, Neighbors),
    Neighbors \= []),
    check_all_green_cells_have_neighbors(Rest).

one_fixed_cell_in_green:-
not(green_cell_between_two_fixed),
    findall((Row, Col),
            (solve_cell(Row, Col, green)),
            AllgreenCells),
            check_all_green_cells_have_neighbors(AllgreenCells).

sum_list([], 0).
sum_list([Head|Tail], Sum) :-
    sum_list(Tail, TailSum),
    Sum is Head + TailSum.

green_number_equals_size:-
findall((Row,Col),(solve_cell(Row,Col,green)),AllgreenCells),
length(AllgreenCells,Size),
findall(N,fxd_cell(_,_,N),Numbers),
sum_list(Numbers,Sum),
Sum =:= Size.

solved :-
    one_wall,
    no_2_by_2_wall,
    one_fixed_cell_in_green,
    green_number_equals_size.

% Predicate to find neighbors of cells in a list
find_neighbors([], []).
find_neighbors([(R, C)|T], Neighbors) :-
    findall((R1, C1),
            (
                (R1 is R, C1 is C - 1, solve_cell(R1, C1, sea));
                (R1 is R, C1 is C + 1, solve_cell(R1, C1, sea))
            ),
            Neighbors1),
    find_neighbors(T, Neighbors2),
    append(Neighbors1, Neighbors2, Neighbors).

% Predicate to find all sea cells in a specific row
sea_cells_in_row(Row, SeaCells) :-
    findall((Row, Col), solve_cell(Row, Col, sea), SeaCells).

% Predicate to find all sea cells row by row and check connectivity
all_sea_cells(SortedList) :-
    findall(SeaCells, (between(1, 5, Row), sea_cells_in_row(Row, SeaCells)), AllSeaCellsList),
    maplist(process_and_filter_neighbors, AllSeaCellsList, ProcessedLists) ,
    sort(ProcessedLists, SortedList),
    !.

% Predicate to process each list and filter disconnected parts
process_and_filter_neighbors([], []).
process_and_filter_neighbors(List, FilteredNeighbors) :-
    find_neighbors(List, Neighbors),
    filter_connected(List, Neighbors, FilteredNeighbors).

% Predicate to filter only the connected neighbors
filter_connected([], _, []).
filter_connected([H|T], Neighbors, [H|Filtered]) :-
    member(H, Neighbors),
    filter_connected(T, Neighbors, Filtered).
filter_connected([H|T], Neighbors, Filtered) :-
    \+ member(H, Neighbors),
    filter_connected(T, Neighbors, Filtered).



find_column_neighbors([], []).
find_column_neighbors([(R, C)|T], Neighbors) :-
    findall((R1, C1),
            (
                (R1 is R - 1, C1 is C, solve_cell(R1, C1, sea));
                (R1 is R + 1, C1 is C, solve_cell(R1, C1, sea))
            ),
            Neighbors1),
    find_column_neighbors(T, Neighbors2),
    append(Neighbors1, Neighbors2, Neighbors).

% Predicate to find all sea cells in a specific column
column_sea_cells(Col, SeaCells) :-
    findall((Row, Col), solve_cell(Row, Col, sea), SeaCells).

% Predicate to process each list and filter disconnected parts
process_and_filter_neighborss([], []).
process_and_filter_neighborss(List, FilteredNeighbors) :-
    find_column_neighbors(List, Neighbors),
    filter_connectedd(List, Neighbors, FilteredNeighbors).

% Predicate to filter only the connected neighbors
filter_connectedd([], _, []).
filter_connectedd([H|T], Neighbors, [H|Filtered]) :-
    member(H, Neighbors),
    filter_connectedd(T, Neighbors, Filtered).
filter_connectedd([H|T], Neighbors, Filtered) :-
    \+ member(H, Neighbors),
    filter_connectedd(T, Neighbors, Filtered).



% Predicate to find all sea cells column by column
all_column_sea_cells(SortedList) :-
    findall(SeaCells, (between(1, 5, Col), column_sea_cells(Col, SeaCells)), AllSeaCellsList),
    maplist(process_and_filter_neighborss, AllSeaCellsList, ProcessedLists) ,
    sort(ProcessedLists, SortedList),
    !.
% Predicate to merge lists with common elements
merge_lists_with_common_elements([], []).
merge_lists_with_common_elements([H|T], Merged) :-
    merge_lists_with_common_elements(T, Rest),
    merge_list_with_all(H, Rest, Merged).

merge_list_with_all(List, [], [List]).
merge_list_with_all(List, [H|T], Merged) :-
    ( have_common_element(List, H) ->
        append(List, H, MergedList),
        sort(MergedList, SortedMergedList),
        merge_list_with_all(SortedMergedList, T, Merged)
    ;   Merged = [H|Rest],
        merge_list_with_all(List, T, Rest)
    ).
% Define predicate to check if two lists have a common element
have_common_element(List1, List2) :-
    member(Element, List1),
    member(Element, List2).

% Remove duplicate sublists
remove_duplicates([], []).
remove_duplicates([H|T], [H|Result]) :-
    \+ member(H, T),
    remove_duplicates(T, Result).
remove_duplicates([H|T], Result) :-
    member(H, T),
    remove_duplicates(T, Result).

test :-
    all_column_sea_cells(Column),
    all_sea_cells(Row),
    findall(Result, merge_with_common_element(Row, Column, Result), Finalist),
    findall(X, (merge_with_common_element_final(Finalist, R), sort(R, X)), FinalList),
    remove_duplicates(FinalList, UniqueFinalList),
    merge_lists_with_common_elements(UniqueFinalList, MergedFinalList),
    !,
    write(MergedFinalList),
    length(MergedFinalList,L),
    L =:=1 .
merge_with_common_element(List1, List2, SortedList) :-
    member(SubList1, List1),
    member(SubList2, List2),
    have_common_element(SubList1, SubList2),
    append(SubList1, SubList2, MergedList),
    sort(MergedList, SortedList).

merge_with_common_element_final(List1, SortedList) :-
    member(SubList1, List1),
    member(SubList2, List1),
    SubList1 \== SubList2,
    have_common_element(SubList1, SubList2),
    append(SubList1, SubList2, MergedList),
    sort(MergedList, SortedList).

example:-
SortedList = [[], [(3, 1), (4, 1), (5, 1)], [(3, 5), (4, 5), (5, 5)]],
N = [[], [(4, 3), (4, 4), (4, 5)], [(5, 1), (5, 2), (5, 3)]],
findall(Result, merge_with_common_element(SortedList, N, Result), Finalist),
% write(Finalist),
merge_with_common_element_final(Finalist,Final),
write(Final).


