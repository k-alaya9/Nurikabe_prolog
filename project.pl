
fxd_cell(1, 2, 7).
fxd_cell(3,2,2).
fxd_cell(3,4,1).
fxd_cell(5,4,1).

:- dynamic solved_cell/3.
%% static solve

solved_cell(1, 2, green).
solved_cell(3,2,green).
solved_cell(3,4,green).
solved_cell(5,4,green).


solved_cell(1, 1,green).
solved_cell(1,3,green).
solved_cell(1,4,green).
solved_cell(1,5,green).
solved_cell(2,1,green).
solved_cell(2,2,sea).
solved_cell(2,3,sea).
solved_cell(2,4,sea).
solved_cell(2,5,green).
solved_cell(3,1,sea).
solved_cell(3,3,sea).
solved_cell(3,5,sea).
solved_cell(4,1,sea).
solved_cell(4,2,green).
solved_cell(4,3,sea).
solved_cell(4,4,sea).
solved_cell(4,5,sea).
solved_cell(5,1,sea).
solved_cell(5,2,sea).
solved_cell(5,3,sea).
solved_cell(5,5,sea).



green_neighbors(R, C, GreenNeighbors) :-
    findall((R, C1),
            (C1 is C + 1, solved_cell(R, C1, green);
             C1 is C - 1, solved_cell(R, C1, green);
             R1 is R + 1, solved_cell(R1, C, green);
             R1 is R - 1, solved_cell(R1, C, green)),
            GreenNeighbors).

sea_neighbors(R, C, SeaNeighbors) :-
    findall((R, C1),
            (C1 is C + 1, solved_cell(R, C1, sea);
             C1 is C - 1, solved_cell(R, C1, sea);
             R1 is R + 1, solved_cell(R1, C, sea);
             R1 is R - 1, solved_cell(R1, C, sea)),
            SeaNeighbors).


check_all_sea_cells_have_neighbors([]).
check_all_sea_cells_have_neighbors([(Row, Col)|Rest]) :-
    sea_neighbors(Row, Col, Neighbors),
    Neighbors \= [],
    check_all_sea_cells_have_neighbors(Rest).

one_wall :-
    all_column_sea_cells(Column),
    all_sea_cells(Row),
    findall(Result, merge_with_common_element(Row, Column, Result), Finalist),
    findall(X, (merge_with_common_element_final(Finalist, R), sort(R, X)), FinalList),
    remove_duplicates(FinalList, UniqueFinalList),
    merge_lists_with_common_elements(UniqueFinalList, MergedFinalList),
    !,
    length(MergedFinalList,L),
    L =:= 1 ,
    findall((Row, Col),
            (solved_cell(Row, Col, sea)),
            AllseaCells),
    check_all_sea_cells_have_neighbors(AllseaCells).

no_2_by_2_wall :-
    \+ (solved_cell(R, C, sea),
        solved_cell(R, C1, sea), C1 is C + 1,
        solved_cell(R1, C, sea), R1 is R + 1,
        solved_cell(R1, C1, sea)).

green_cell_between_two_fixed:-
    fxd_cell(R1, C1, _),
    fxd_cell(R2, C2, _),
    (   R1 =:= R2,
        C1 < C2,
        solved_cell(R1, C, green),
        C > C1, C < C2
    ;   C1 =:= C2,
        R1 < R2,
        solved_cell(R, C1, green),
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
            (solved_cell(Row, Col, green)),
            AllgreenCells),
            check_all_green_cells_have_neighbors(AllgreenCells).

sum_list([], 0).
sum_list([Head|Tail], Sum) :-
    sum_list(Tail, TailSum),
    Sum is Head + TailSum.

green_number_equals_size:-
findall((Row,Col),(solved_cell(Row,Col,green)),AllgreenCells),
length(AllgreenCells,Size),
findall(N,fxd_cell(_,_,N),Numbers),
sum_list(Numbers,Sum),
Sum =:= Size.

solved :-
    one_wall,
    no_2_by_2_wall,
    one_fixed_cell_in_green,
    green_number_equals_size.


find_neighbors([], []).
find_neighbors([(R, C)|T], Neighbors) :-
    findall((R1, C1),
            (
                (R1 is R, C1 is C - 1, solved_cell(R1, C1, sea));
                (R1 is R, C1 is C + 1, solved_cell(R1, C1, sea))
            ),
            Neighbors1),
    find_neighbors(T, Neighbors2),
    append(Neighbors1, Neighbors2, Neighbors).


sea_cells_in_row(Row, SeaCells) :-
    findall((Row, Col), solved_cell(Row, Col, sea), SeaCells).


all_sea_cells(SortedList) :-
    findall(SeaCells, (between(1, 5, Row), sea_cells_in_row(Row, SeaCells)), AllSeaCellsList),
    maplist(process_and_filter_neighbors, AllSeaCellsList, ProcessedLists) ,
    sort(ProcessedLists, SortedList),
    !.


process_and_filter_neighbors([], []).
process_and_filter_neighbors(List, FilteredNeighbors) :-
    find_neighbors(List, Neighbors),
    filter_connected(List, Neighbors, FilteredNeighbors).


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
                (R1 is R - 1, C1 is C, solved_cell(R1, C1, sea));
                (R1 is R + 1, C1 is C, solved_cell(R1, C1, sea))
            ),
            Neighbors1),
    find_column_neighbors(T, Neighbors2),
    append(Neighbors1, Neighbors2, Neighbors).

column_sea_cells(Col, SeaCells) :-
    findall((Row, Col), solved_cell(Row, Col, sea), SeaCells).


process_and_filter_neighborss([], []).
process_and_filter_neighborss(List, FilteredNeighbors) :-
    find_column_neighbors(List, Neighbors),
    filter_connectedd(List, Neighbors, FilteredNeighbors).


filter_connectedd([], _, []).
filter_connectedd([H|T], Neighbors, [H|Filtered]) :-
    member(H, Neighbors),
    filter_connectedd(T, Neighbors, Filtered).
filter_connectedd([H|T], Neighbors, Filtered) :-
    \+ member(H, Neighbors),
    filter_connectedd(T, Neighbors, Filtered).




all_column_sea_cells(SortedList) :-
    findall(SeaCells, (between(1, 5, Col), column_sea_cells(Col, SeaCells)), AllSeaCellsList),
    maplist(process_and_filter_neighborss, AllSeaCellsList, ProcessedLists) ,
    sort(ProcessedLists, SortedList),
    !.

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

have_common_element(List1, List2) :-
    member(Element, List1),
    member(Element, List2).


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

within_bounds(R, C) :-
  R > 0, R < 6,
    C > 0, C < 6.


print_cell(R, C) :-
    (   
    fxd_cell(R, C, N)
    ->  write(N)    
    ;  solved_cell(R, C, green)
    ->  write('G')  
    ;   solved_cell(R, C, sea)
    ->  write('S') 
    ;    write('.')  
    ),
    write(' ')
    .



print_row(R, MaxC) :-
    between(1, MaxC, C),
    print_cell(R, C),
    (C =:= MaxC -> nl; true),
    fail.
print_row(_, _).

print_puzzle :-
    nl,
    MaxR = 5,
    MaxC = 5,
    between(1, MaxR, R),
    print_row(R, MaxC),
    fail.

clear_solved_cells :-
    retractall(solved_cell(_, _, _)).

add_solved_cell(R,C,Color):-
    \+ solved_cell(R,C,_),
    assertz(solved_cell(R,C,Color)).
%%rule 1 
island_of_1 :-
    fxd_cell(R, C, 1),
    NeighborOffsets = [(0, 1), (0, -1), (1, 0), (-1, 0)],
    mark_neighbors_sea(R, C, NeighborOffsets).

mark_neighbors_sea(_, _, []).
mark_neighbors_sea(R, C, [(DR, DC)|Offsets]) :-
    NR is R + DR,
    NC is C + DC,

    (   \+ fxd_cell(NR, NC, _),
        \+ solved_cell(NR, NC, _),
            within_bounds(NR, NC)
    ->  add_solved_cell(NR, NC, sea)
    ;   true
    ),
    mark_neighbors_sea(R, C, Offsets).
%%rule 2
clues_separated_by_one :-
    (   fxd_cell(R, C1, _),
        fxd_cell(R, C2, _),
        C2 =:= C1 + 2,
        C_between is C1 + 1,
        within_bounds(R, C_between),
        \+ solved_cell(R, C_between, _)
    ->  add_solved_cell(R, C_between, sea)
    ;   true
    ),
    (   fxd_cell(R1, C, _),
        fxd_cell(R2, C, _),
        R2 =:= R1 + 2,
        R_between is R1 + 1,
        within_bounds(R_between, C),
        \+ solved_cell(R_between, C, _)
    ->  add_solved_cell(R_between, C, sea)
    ;   true
    ).
%% rule 3 
diagonally_adjacent_clues :-
    (   fxd_cell(R1, C1, _),
        fxd_cell(R2, C2, _),
        R2 =:= R1 + 1,
        C2 =:= C1 + 1
    ->  mark_diagonal_sea(R1, C1, R2, C2)
    ;   true
    ),
    (   fxd_cell(R1, C1, _),
        fxd_cell(R2, C2, _),
        R2 =:= R1 + 1,
        C2 =:= C1 - 1
    ->  mark_diagonal_sea(R1, C1, R2, C2)
    ;   true
    ).

mark_diagonal_sea(R1, C1, R2, C2) :-
    (   NR1 is R1,
        NC1 is C2,
        within_bounds(NR1, NC1),
        \+ solved_cell(NR1, NC1, _)
    ->  add_solved_cell(NR1, NC1, sea)
    ;   true
    ),
    (   NR2 is R2,
        NC2 is C1,
        within_bounds(NR2, NC2),
        \+ solved_cell(NR2, NC2, _)
    ->  add_solved_cell(NR2, NC2, sea)
    ;   true
    ).

%%rule 4
mark_islands :-
    fxd_cell(X, Y, _),
    add_solved_cell(X, Y, green),
    fail.
mark_islands.

%% rule 5
surrounded_square :-
    solved_cell(R, C, _),
    (   solved_cell(R, C1, sea), C1 is C + 1,
        solved_cell(R, C2, sea), C2 is C - 1,
        solved_cell(R1, C, sea), R1 is R + 1,
        solved_cell(R2, C, sea), R2 is R - 1
        
    ->  (   \+ solved_cell(R, C, sea),
            within_bounds(R, C)
        ->  add_solved_cell(R, C, sea)
        ;   true
        )
    ;   true
    ).

%% rule 6
sea_expansion :-
    solved_cell(R, C, sea),
    expand_sea(R, C).

expand_sea(R, C) :-
    NeighborOffsets = [(0, 1), (0, -1), (1, 0), (-1, 0)],
    mark_neighbors_sea(R, C, NeighborOffsets).



unsolved_cell_exists :-
    between(1, 5, R),
    between(1, 5, C),
    \+ solved_cell(R, C, _),
    \+ fxd_cell(R, C, _),
    !.

solve_puzzle :-
    clear_solved_cells,
    mark_islands,
    solve_with_rules.
solve_with_rules :-
    ( true
    ->  (   
           
            island_of_1,
            clues_separated_by_one,
            diagonally_adjacent_clues,
            surrounded_square,
            sea_expansion,
            island_expansion,
            print_puzzle,
            ( solved -> ! ; fail )
        )
    ;   true
    ).
%% rule 7
island_expansion :-
    fxd_cell(R, C, N),
    expand_island(R, C, N).

expand_island(R, C, N) :-
    (   N =:= 1
    ->  true  
    ;   Offsets = [(0, 1), (0, -1), (1, 0), (-1, 0)],
        expand_island(R, C, N, Offsets)
    ).

expand_island(_, _, 1, _).
expand_island(R, C, N, [(DR, DC)|Offsets]) :-
    NR is R + DR,
    NC is C + DC,
    within_bounds(NR, NC),
    add_solved_cell(NR, NC, green),
    (( not(( 
        NCR is NC + 1 ,
        NCL is NC - 1 ,
        fxd_cell(NR,NCR,_),
        fxd_cell(NR,NCL,_)
    ;  
        NRR is NR + 1 ,
        NRL is NR - 1 ,
        fxd_cell(NRR,NC,_),
        fxd_cell(NRL,NC,_)
    ))
   )
    -> N1 is N - 1,
    expand_island(R,C,N1,Offsets),
    expand_island(NR, NC, N1, [(DR, DC)|Offsets])
    ; retract(solved_cell(NR, NC, green)), add_solved_cell(NR,NC,sea)
    ,expand_island(R, C, N, Offsets)
    ,expand_island(NR, NC, N, [(DR, DC)|Offsets])
    ).
