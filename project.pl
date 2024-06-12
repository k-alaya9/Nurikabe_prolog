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

green_neighbors(R, C, GreenNeighbors) :-
    findall((R, C1),
            (C1 is C + 1, solve_cell(R, C1, green);
             C1 is C - 1, solve_cell(R, C1, green);
             R1 is R + 1, solve_cell(R1, C, green);
             R1 is R - 1, solve_cell(R1, C, green)),
            GreenNeighbors).

sea_neighbors(R, C, SeaNeighbors) :-
    findall((R, C1),
            (C1 is C + 1, solve_cell(R, C1, sea);
             C1 is C - 1, solve_cell(R, C1, sea);
             R1 is R + 1, solve_cell(R1, C, sea);
             R1 is R - 1, solve_cell(R1, C, sea)),
            SeaNeighbors).


all_sea_neighbors(Row, Col, AllseaNeighbors) :-
    solve_cell(Row, Col, sea), 
    sea_neighbors(Row, Col, SeaNeighbors),
    findall((NeighborRow, NeighborCol),
            (member((NeighborRow, NeighborCol), SeaNeighbors),
             solve_cell(NeighborRow, NeighborCol, sea)),
            AllseaNeighbors).


all_sea_neighbors_in_grid(AllseaNeighbors) :-
    findall((Row, Col, SeaNeighbors),
            (solve_cell(Row, Col, sea),
             all_sea_neighbors(Row, Col, SeaNeighbors)),
            AllseaNeighbors).


all_connected_sea_cell_vertical_horizntal(B):-
all_sea_neighbors_in_grid(AllseaNeighbors),
length(AllseaNeighbors,B).


check_all_sea_cells_have_neighbors([]).
check_all_sea_cells_have_neighbors([(Row, Col)|Rest]) :-
    sea_neighbors(Row, Col, Neighbors),
    Neighbors \= [],
    check_all_sea_cells_have_neighbors(Rest).

one_wall :-
    all_connected_sea_cell_vertical_horizntal(NumConnectedseaCells),
    findall((Row, Col),
            (solve_cell(Row, Col, sea)),
            AllseaCells),
    length(AllseaCells, NumTotalseaCells),
    NumConnectedseaCells =:= NumTotalseaCells,
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
        C > C1, C < C2,
        \+ (solve_cell(R1, C3, green), C3 > C1, C3 < C2, C3 \= C)
    ;   C1 =:= C2,
        R1 < R2,
        solve_cell(R, C1, green),
        R > R1, R < R2,
        \+ (solve_cell(R3, C1, green), R3 > R1, R3 < R2, R3 \= R)
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


% has_one_fixed_cell(Component) :-
%     findall(_, (member((X, Y), Component), fxd_cell(X, Y, _)), FxdCells),
%     length(FxdCells, 1).

% connected_component([(X, Y)|Component], Color) :-
%     connected(X, Y, Color),
%     connected_component(Component, Color).
% connected_component([], _).

% path(C1, Color) :-
%     setof((X, Y), solve_cell(X, Y, Color), Component),
%     print(Component),
%     create_lists(Component,C1),
%      writeln(C1).

% connect_lists(List1, List2, ConnectedList) :-
%     intersection(List1, List2, CommonElements),
%     CommonElements \= [],
%     append(List1, List2, ConnectedList).
% connect_elements(List, Paths) :-
%     connect_elements(List, [], [], Paths).

% connect_elements([], _, Acc, Acc).
% connect_elements([Point|Rest], Visited, Acc, Paths) :-
%     (   member(Point, Visited)
%     ->  connect_elements(Rest, Visited, Acc, Paths)
     
%     ;   dfs(Point, Rest, Visited, Path),
%         (   length(Path, Len),
%             (   check_lengths(Acc, Len) -> AccNew = [Path]
%             ;   AccNew = [Path|Acc]
%             )
%         ),
%         connect_elements(Rest, [Point|Visited], AccNew, Paths)
%     ).

% check_lengths([], _).
% check_lengths([P|Ps], Len) :-
%     length(P, LenP),
%     LenP < Len,
%     check_lengths(Ps, Len).

% dfs(Point, List, Visited, Path) :-
%     findall(ConnectedPoint, (member(ConnectedPoint, List), connected(Point, ConnectedPoint), \+ member(ConnectedPoint, Visited)), ConnectedPoints),
%     dfs_loop(Point, ConnectedPoints, List, Visited, [Point], Path).

% dfs_loop(_, [], _, _, Path, Path).
% dfs_loop(Point, [ConnectedPoint|ConnectedPoints], List, Visited, Acc, Path) :-
%     \+ member(ConnectedPoint, Acc),
%     dfs(ConnectedPoint, List, [ConnectedPoint|Visited], NewPath),
%     append(NewPath, [Point], AccPath),
%     dfs_loop(ConnectedPoint, ConnectedPoints, List, [ConnectedPoint|Visited], AccPath, Path).
% connected((X1, Y1), (X2, Y2)) :-
%     (X1 =:= X2 ; Y1 =:= Y2),
%     abs(X1 - X2) + abs(Y1 - Y2) =:= 1.


% create_lists(L, Lists) :-
%     findall(fxd_cell(R, C, N), fxd_cell(R, C, N), FxdCells),
%     create_lists(L, FxdCells, Lists).

% create_lists(_, [], []).
% create_lists(L, [fxd_cell(R, C, N)|FxdCells], [Sublist|Lists]) :-
%     findall(Cell, (member(Cell, L), connected(Cell, (R, C))), SublistCells),
%     length(SublistCells, N),
%     append(SublistCells, Rest, L),
%     connected_sublist(SublistCells, L),
%     create_lists(Rest, FxdCells, Lists).
% connected_sublist([], _).
% connected_sublist([_], _).
% connected_sublist([Cell1, Cell2|Cells], L) :-
%     connected(Cell1, Cell2),
%     connected_sublist([Cell2|Cells], L).

