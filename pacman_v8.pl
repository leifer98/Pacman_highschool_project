:-dynamic order/1.
:-dynamic winner/1.
:-dynamic mouse/1.
:-dynamic point/1.
:-dynamic user_level/1.
:-dynamic game_mode/2.
:-dynamic point/3.
:-dynamic spoint/3.
:-dynamic difficulty/1.


show_gameover:-
	new(W,window('game over',size(200,100))),
	send(W,background,bitmap('mad.bmp')),
	send(W,open),
	point(P),
	retractall(point(_)),
	send(W, display,text('score : '), point(62, 52)),
	send(W, display,text(P), point(102, 52)),
	new(B1, button(exit,message(@prolog, close_program,W))),
	send(W,display,B1,point(10,75)),
	new(B2, button(manu,message(@prolog, start,W))),
	send(W,display,B2,point(110,75)).


show_good_feedback:-
	new(W,window('good job',size(200,100))),
	send(W,background,bitmap('goodjob.bmp')),
	send(W,open),
	send(W, display,text('congrats! '), point(72, 22)),
	new(B1, button(exit,message(@prolog, close_program,W))),
	send(W,display,B1,point(10,75)),
	new(B2, button(continue,message(@prolog, next_level,W))),
	send(W,display,B2,point(110,75)).


next_level(W):-
	free(W),
	user_level(X),
	X1 is X+1,
	level(X1).

start(W1):-
	free(W1),
	retractall(difficulty(_)),
	new(W,window('pacman',size(525,625))),
	send(W,open),
	send(W,background,colour(black)),
	send(W,display,new(_,bitmap('title.bmp')),point(125,100)),
	new(B1, button(exit,message(@prolog, close_program,W))),
	send(W,display,B1,point(225,325)),
	new(B2, button(easy,message(@prolog, easy,W))),
	send(W,display,B2,point(225,175)),
	new(B3, button(medium,message(@prolog,medium,W))),
	send(W,display,B3,point(225,225)),
	new(B4, button(hard,message(@prolog, hard,W))),
	send(W,display,B4,point(225,275)).


start:-
	retractall(difficulty(_)),
	new(W,window('pacman',size(525,625))),
	send(W,open),
	send(W,background,colour(black)),
	send(W,display,new(_,bitmap('title.bmp')),point(125,100)),
	new(B1, button(exit,message(@prolog, close_program,W))),
	send(W,display,B1,point(225,325)),
	new(B2, button(easy,message(@prolog, easy,W))),
	send(W,display,B2,point(225,175)),
	new(B3, button(medium,message(@prolog,medium,W))),
	send(W,display,B3,point(225,225)),
	new(B4, button(hard,message(@prolog, hard,W))),
	send(W,display,B4,point(225,275)).




close_program(W):-
	free(@window),
	free(W).
close_program(W):-
	free(W).

easy(W):-
	assert(difficulty(easy)),
	free(W),
	game(1).
medium(W):-
	assert(difficulty(medium)),
	free(W),
	game(1).
hard(W):-
	assert(difficulty(hard)),
	free(W),
	game(1).


close:-
	assert(winner(ghost)).

game(X):-
	(object( @window),!,free(@window);true),
	retractall(winner(_)),
	retractall(point(_)),
	new(@window,window('pacman',size(525,625))),
	send(@window,open),
	level(X).

level(X):-
	send(@window,clear),
	retractall,
	board_map1(Map),
	send(@window,display,new(_,box(525,100)),point(0,525)),
	new(B,bitmap('pacman_right.bmp')),
	send(@window,display,B,point(250,275)),
	new(B1, button(quit,message(@prolog, close))),
	send(@window,display,B1,point(2,600)),
	assert(mouse(B)),
	bots(X,L),
	sleep(2),
	assert(game_mode(normal,0)),
	assert(order(right)),
	assert(user_level(X)),
	assert(point(0)),
	send(@window, display,text('turn : '), point(2, 527)),
	send(@window, display,text('points : '), point(2, 547)),
	turn((_,0,_,_),Map,L).







bots(0,[]).
bots(N,List):-
	new( B,bitmap('ghost_eating.bmp')), %creates cat
	send( @window,display,B,point(250,150)), %shows Object
	N1 is N-1,
	bots(N1,L),
	append([B],L,List).


anilist([],_,1).
anilist([],L,Frames):-
	Delay is 1/150,
	FramesLeft is Frames - 1,
	new(B1,box(525,625)),
	send(@window,display,B1,point(0,0)),
	catch_order(B1),
	sleep(Delay),
	free(B1),
	anilist(L,L,FramesLeft).
anilist([(B,Step_x,Step_y)|T],L,Frames):-
	get(B, x,Cx),
	get(B, y,Cy),
	send( B,x(Cx+Step_x)), %moves box to the right or left
	send( B,y(Cy+Step_y)), %moves box to the down or up
	send( @window,flush), %flushes
	repeat,
	anilist(T,L,Frames).


turn((Tp,N,T1,D),Map,Lbots):-
	not((winner(_))),
	player_move(Map,D,X5,Y5,Dnew),
	check_mode,
	bot_move(Map,Lbots,(X5,Y5),L1),
	check_wincat(L1,(X5,Y5),L1,NewL1,NewLbots),
	changemouse(X5,Y5),
	mouse(B1),
	append(NewL1,[(B1,X5,Y5)],L),
	check_winmouse(NewLbots),
	findall((B,Step_x,Step_y),(member((B,X,Y),L),Step_x is ((X)/25),Step_y is ((Y)/25)),List),
	anilist(List,List,25),
	free(T1),
	new(T, text(N)),N1 is N+1,
	send(@window, display,T, point(32, 527)),
	point(P),
	check_point(P,P1),
	new(Tp1, text(P1)),free(Tp),
	send(@window, display,Tp1, point(47, 547)),
	retractall(point(_)),
	assert(point(P1)),
	change_ghost(NewLbots,NewLbots1),
	turn((Tp1,N1,T,Dnew),Map,NewLbots1).

turn(_,_,_):-
	winner(ghost),
	retractall(winner(_)),
	free(@window),
	show_gameover.


turn(_,_,_):-
	winner(pacman),
	send(@window,clear),
	show_good_feedback.


change_ghost([],[]).
change_ghost([B|T1],NewLbots):-
	game_mode(normal,X),X=<1,
	new(B1,bitmap('ghost_eating.bmp')),
	get(B,x,Cx),
	get(B,y,Cy),
	send(@window,display,B1,point(Cx,Cy)),
	free(B),
	change_ghost(T1,OldLbots),
	append([B1],OldLbots,NewLbots).
change_ghost([B|T1],NewLbots):-
	game_mode(normal,X),X>1,
	new(B1,bitmap('ghost_eatable.bmp')),
	get(B,x,Cx),
	get(B,y,Cy),
	send(@window,display,B1,point(Cx,Cy)),
	free(B),
	change_ghost(T1,OldLbots),
	append([B1],OldLbots,NewLbots).



changemouse(0,0).
changemouse(X5,Y5):-
	mouse(B),
	retractall(mouse(B)),
	move(D,X5,Y5),
	term_string(D,S),
	string_concat('pacman_',S,Ns1),
	string_concat(Ns1,'.bmp',Ns),
	new(B1,bitmap(Ns)),
	get(B,x,Cx),
	get(B,y,Cy),
	send(@window,display,B1,point(Cx,Cy)),
	free(B),
	assert(mouse(B1)).


bot_move(Map,Lbots,_,L1):-
	difficulty(easy),
	random_move(Map,Lbots,L1).
bot_move(Map,Lbots,(X5,Y5),L1):-
	smart_move(Map,Lbots,(X5,Y5),L1).

/*     random move      */


random_move(_,[],[]).
random_move(M,[B|T],List):-
	findall((D,X,Y),(move(D,X,Y),possible(M,B,D)),L),
	random_member((D1,X1,Y1),L),
	change_place(B,D1),
	random_move(M,T,L1),
	append([(B,X1,Y1)],L1,List).

/*          smart move          */


smart_move(M,L,(X5,Y5),List_of_Moves):-
	findall((B3,Cx1,Cy1),(member(B3,L),get(B3,x,Cx1),get(B3,y,Cy1)),B),
	position_of_mouse((X5,Y5),(Cx_Mouse,Cy_Mouse)),
	get_chosen_board(M,(Cx_Mouse,Cy_Mouse),B,The_Board),
	find_moves(The_Board,List_of_Moves),
	change_place_for_bots(List_of_Moves).

get_chosen_board(M,(Cx_Mouse,Cy_Mouse),CurrentBoard,The_Board):-
	difficulty(medium),
	target_for_bots(M,(Cx_Mouse,Cy_Mouse),Final_List),
	change_board_term_to_string(CurrentBoard,CurrentBoard1),
	append([(pacman,Cx_Mouse,Cy_Mouse)],CurrentBoard1,CurrentBoard2),
	mini_max(CurrentBoard2,M,Final_List,CurrentBoard2,(The_Board1,_),1,max,(-1000000,1000000)),
	delete(The_Board1,(pacman,_,_),Board),
	get_new_board(CurrentBoard,Board,The_Board).
get_chosen_board(M,(Cx_Mouse,Cy_Mouse),CurrentBoard,The_Board):-
	difficulty(hard),
	target_for_bots(M,(Cx_Mouse,Cy_Mouse),Final_List),
	change_board_term_to_string(CurrentBoard,CurrentBoard1),
	append([(pacman,Cx_Mouse,Cy_Mouse)],CurrentBoard1,CurrentBoard2),
	mini_max(CurrentBoard2,M,Final_List,CurrentBoard2,(The_Board1,_),3,max,(-1000000,1000000)),
	delete(The_Board1,(pacman,_,_),Board),
	get_new_board(CurrentBoard,Board,The_Board).

get_new_board([],_,[]).
get_new_board([(B,_,_)|T],L,The_Board):-
	term_string(B,S),
	member((B1,X,Y),L),
	term_string(B1,S1),
	string_concat('@',S1,S),
	get_new_board(T,L,Old_Board),
	append([(B,X,Y)],Old_Board,The_Board).


change_board_term_to_string([],[]).
change_board_term_to_string([(B,P1,P2)|T],[(B1,P1,P2)|T1]):-
	term_string(B,S),
	string_concat('@',S1,S),
	term_string(B1,S1),
	change_board_term_to_string(T,T1).


mini_max(_,_,Final_List,B,(B,P),0,_,_):-
	board_points(B,Final_List,P).

mini_max(_,M,Final_List,CurrentBoard,(Board,P),X,max,(Alpha,Beta)):-
	dif(X,0),
	find_possible_boards_for_board(M,CurrentBoard,List_of_Boards),
	X1 is X-1,
	find_minimax_for_list(List_of_Boards,List,CurrentBoard,M,Final_List,X1,min,(-1000000,Alpha,Beta)),
	max_member((P,Board),List).



mini_max(_,M,Final_List,CurrentBoard,(Board,P),X,min,(Alpha,Beta)):-
	dif(X,0),
	find_possible_boards_for_pacman(M,CurrentBoard,List),
	X1 is X-1,
	find_minimax_for_list(List,List1,CurrentBoard,M,Final_List,X1,max,(1000000,Alpha,Beta)),
	min_member((P,Board),List1).




find_minimax_for_list([],[],_,_,_,_,_,_).
find_minimax_for_list(_,[],_,_,_,_,min,(Value,_,Beta)):-
	Value>Beta.
find_minimax_for_list(_,[],_,_,_,_,max,(Value,Alpha,_)):-
	Value<Alpha.

find_minimax_for_list([B|T],L,CurrentBoard,M,Final_List,X1,max,(Value,Alpha,Beta)):-
	edit_board(B,B2),
	edit(B2,CurrentBoard,B3),
	mini_max(CurrentBoard,M,Final_List,B3,(_,V1),X1,max,(Alpha,Beta)),
	min_member(V2,[Value,V1]),
	min_member(NewBeta,[Beta,V1]),
	find_minimax_for_list(T,L1,CurrentBoard,M,Final_List,X1,max,(V2,Alpha,NewBeta)),
	append([(V1,B3)],L1,L).
find_minimax_for_list([B|T],L,CurrentBoard,M,Final_List,X1,min,(Value,Alpha,Beta)):-
	edit_board(B,B2),
	edit(B2,CurrentBoard,B3),
	mini_max(CurrentBoard,M,Final_List,B3,(_,V1),X1,min,(Alpha,Beta)),
	max_member(V2,[Value,V1]),
	max_member(NewAlpha,[Alpha,V1]),
	find_minimax_for_list(T,L1,CurrentBoard,M,Final_List,X1,min,(V2,NewAlpha,Beta)),
	append([(V1,B3)],L1,L).


edit_board(B,B2):-
	member((pacman,X,Y),B),
	delete(B,(pacman,_,_),B1),
	append(B1,[(pacman,X,Y)],B2).
edit_board(B,B):-
	not((member((pacman,_,_),B))).




edit(L,_,L):-
	not((member((pacman,_,_),L))).
edit([(pacman,X,Y)],_,[(pacman,X,Y)]).
edit([(B,X,Y)|T],O,NewBoard2):-
	game_mode(normal,0),
	dif(B,pacman),
	member((pacman,X,Y),T),
	edit(T,O,NewBoard),
	delete(NewBoard,(pacman,_,_),NewBoard1),
	append([(B,X,Y)],NewBoard1,NewBoard2).
edit([(B,X,Y)|T],OldBoard,NewBoard2):-
	game_mode(normal,0),
	dif(B,pacman),
	member((pacman,X1,Y1),T),
	member((B,X1,Y1),OldBoard),
	member((pacman,X,Y),OldBoard),
	edit(T,OldBoard,NewBoard),
	delete(NewBoard,(pacman,_,_),NewBoard1),
	append([(B,X,Y)],NewBoard1,NewBoard2).
edit([(B,X,Y)|T],OldBoard,NewBoard1):-
	game_mode(normal,N),N>0,
	dif(B,pacman),
	member((pacman,X,Y),T),
	edit(T,OldBoard,NewBoard1).
edit([(B,X,Y)|T],OldBoard,NewBoard1):-
	game_mode(normal,N),N>0,
	dif(B,pacman),
	member((pacman,X1,Y1),T),
	member((B,X1,Y1),OldBoard),
	member((pacman,X,Y),OldBoard),
	edit(T,OldBoard,NewBoard1).
edit([(B,X,Y)|T],O,NewBoard):-
	not((dif(B,pacman),member((pacman,X,Y),T))),
	not((dif(B,pacman),member((pacman,X1,Y1),T),member((B,X1,Y1),O),
	member((pacman,X,Y),O))),
	edit(T,O,NewBoard1),
	append([(B,X,Y)],NewBoard1,NewBoard).


find_possible_boards_for_pacman(_,Board,[Board]):-
	not((member((pacman,_,_),Board))).
find_possible_boards_for_pacman(M,Board,SBoard2):-
	member((pacman,Cx,Cy),Board),
	findall((pacman,(Cx,Cy),(X1,Y1)),(
		    (move(D,Move_X,Move_Y),possible(M,Cx,Cy,D),final_position((Cx,Cy),(Move_X,Move_Y),D,X1,Y1)))
	       ,List1),
	delete(Board,(pacman,_,_),Rest),
	findall(F,(get_board(List1,Board1),append(Rest,Board1,F)),Board2),
	sort(Board2,SBoard2).


find_possible_boards_for_board(_,Board,[Board]):-
	not((member((pacman,_,_),Board))).

find_possible_boards_for_board(_,Board,[Board]):-
	member((pacman,_,_),Board),
	delete(Board,(pacman,_,_),[]).
find_possible_boards_for_board(M,Board,SBoard2):-
	member((pacman,X,Y),Board),
	findall((B1,(Cx,Cy),(X1,Y1)),(
		    member((B1,Cx,Cy),Board),dif(B1,pacman),(move(D,Move_X,Move_Y),possible(M,Cx,Cy,D),final_position((Cx,Cy),(Move_X,Move_Y),D,X1,Y1)))
	       ,List),
	findall(F,(get_board(List,Board1),append([(pacman,X,Y)],Board1,F)),Board2),
	sort(Board2,SBoard2).



possible(_,X,Y,left):-
	X is 25,
	Y is 225.

possible(_,X,Y,right):-
	X is 475,
	Y is 225.


possible(Map,X,Y,Direction):-
	not((compare(=,X,25),compare(=,Y,225),compare(=,Direction,left))),
	not((compare(=,X,475),compare(=,Y,225),compare(=,Direction,right))),
	move(Direction,X1,Y1),
	Fx is (X+X1),Fy is (Y+Y1),
	not((member((blank,Fx,Fy),Map))).



get_board([],[]).
get_board(L,Board):-
	member((B,(0,225),(475,225)),L),
	delete(L,(B,_,_),L1),
	get_board(L1,OldBoard),
	append(OldBoard,[(B,475,225)],Board).
get_board(L,Board):-
	member((B,(500,225),(25,225)),L),
	delete(L,(B,_,_),L1),
	get_board(L1,OldBoard),
	append(OldBoard,[(B,25,225)],Board).
get_board(L,Board):-
	member((B,_,(X,Y)),L),
	delete(L,(B,_,_),L1),
	get_board(L1,OldBoard),
	append(OldBoard,[(B,X,Y)],Board).

final_position((X,Y),(0,0),_,X,Y).
final_position((X,Y),(Move_x,Move_y),Direction,X1,Y1):-
	move(Direction,Move_x,Move_y),X is 475,Y is 225,compare(=,Direction,right),X1 is 25,Y1 is 225/*,writeln('c1')*/.

final_position((X,Y),(Move_x,Move_y),Direction,X1,Y1):-
	move(Direction,Move_x,Move_y),X is 25,Y is 225,compare(=,Direction,left),X1 is 475,Y1 is 225/*,writeln('c2')*/.

final_position((Cx,Cy),(Move_X,Move_Y),Direction,X1,Y1):-
	move(Direction,Move_X,Move_Y),not((compare(=,X,25),compare(=,Y,225),compare(=,Direction,left))),not((compare(=,X,475),compare(=,Y,225),compare(=,Direction,right))),X1 is (Cx+Move_X),Y1 is (Cy+Move_Y).


/* the bait */
target_for_bots(Map,(Cx,Cy),List):-
	difficulty(hard),
	game_mode(normal,X),X>0,
	findall((X1,Y1),(move(D,Move_X,Move_Y),final_position((Cx,Cy),(Move_X,Move_Y),D,X1,Y1),not((member((blank,X1,Y1),Map)))),Final_List),
	findall_closer((Cx,Cy),Final_List,List).

/* objective gaming */
target_for_bots(_,_,Final_List):-
	difficulty(hard),
	findall((X1,Y1),(point(X1,Y1,_)),L1),
	length(L1,Length),Length=<10,append([],L1,Final_List).


/* the one way hallway*/
target_for_bots(_,(Cx,225),List):-
	(   (   between(0,225,Cx),T is 1);(between(375,500,Cx),T is 1);(between(225,375,Cx),T is 2);true),
	compare(=,T,1),dif(T,2),
	append([],[(100,225),(400,225)],Final_List),
	findall_closer((Cx,225),Final_List,List).



/* mid strategy*/
target_for_bots(_,(Cx,Cy),[]):-
	between(150,350,Cx),between(100,350,Cy).



/* the corner trapper*/
target_for_bots(Map,(Cx,Cy),List):-
	(   (Cx<250,T1 is 1);(Cx>=250,T1 is 2)),
	find_list_of_line_x(Map,Cx,T1,Lx),
	(   (Cy<225,T2 is 1);(Cy>=225,T2 is 2)),
	find_list_of_line_y(Map,Cy,T2,Ly),
	findall(N1,(member(L1,Lx),length(L1,N1)),Lx_of_N),
	findall(N2,(member(L2,Ly),length(L2,N2)),Ly_of_N),
	min_list(Lx_of_N,Nx),
	min_list(Ly_of_N,Ny),
	findall(L3,(member(L3,Lx),length(L3,Nx)),Final_Lx),
	findall(L4,(member(L4,Ly),length(L4,Ny)),Final_Ly),
	get_final_spots((Cx,Cy),Final_Lx,Final_Ly,Final_List),
	findall_closer((Cx,Cy),Final_List,List).



get_final_spots((Cx,Cy),Final_Lx,Final_Ly,Final_List):-
	findall(X1,member([(X1,_)|_],Final_Lx),List_of_X),
	findall(Y1,member([(_,Y1)|_],Final_Ly),List_of_Y),
	(   (Cx<250,max_list(List_of_X,X2));(Cx>=250,min_list(List_of_X,X2))),
	(   (Cy<225,max_list(List_of_Y,Y3));(Cy>=225,min_list(List_of_Y,Y3))),
	member([(X2,Y2)|T1],Final_Lx),member([(X3,Y3)|T2],Final_Ly),
	append([(X2,Y2)],T1,Line_x),append([(X3,Y3)],T2,Line_y),
	findall((X4,Y4),(member((X4,Y4),Line_x),(   (Cy<225,Y4=<Y3);(Cy>=225,Y4>=Y3))),Final_Line_X),
	findall((X5,Y5),(member((X5,Y5),Line_y),(   (Cx<250,X5=<X2);(Cx>=250,X5>=X2))),Final_Line_Y),
	append(Final_Line_X,Final_Line_Y,Final_List0),
	sort(Final_List0,Final_List).


find_list_of_line_x(Map,Cx,1,L2):-
	findall((X1,Y1),(member((_,X1,Y1),Map),not((member((blank,X1,Y1),Map))),X1>=Cx,X1=<250),L1),
	findall(X5,member((X5,_),L1),L5),
	sort(L5,L3),
	findall(L4,(member(X2,L3),findall((X2,Y2),(member((X2,Y2),L1)),L4)),L2).
find_list_of_line_x(Map,Cx,2,L2):-
	findall((X1,Y1),(member((_,X1,Y1),Map),not((member((blank,X1,Y1),Map))),X1=<Cx,X1>=250),L1),
	findall(X5,member((X5,_),L1),L5),
	sort(L5,L3),
	findall(L4,(member(X2,L3),findall((X2,Y2),(member((X2,Y2),L1)),L4)),L2).


find_list_of_line_y(Map,Cy,1,L2):-
	findall((X1,Y1),(member((_,X1,Y1),Map),not((member((blank,X1,Y1),Map))),Y1>=Cy,Y1=<225),L1),
	findall(Y5,member((_,Y5),L1),L5),
	sort(L5,L3),
	findall(L4,(member(Y2,L3),findall((X2,Y2),(member((X2,Y2),L1)),L4)),L2).
find_list_of_line_y(Map,Cy,2,L2):-
	findall((X1,Y1),(member((_,X1,Y1),Map),not((member((blank,X1,Y1),Map))),Y1=<Cy,Y1>=225),L1),
	findall(Y5,member((_,Y5),L1),L5),
	sort(L5,L3),
	findall(L4,(member(Y2,L3),findall((X2,Y2),(member((X2,Y2),L1)),L4)),L2).


find_moves([],[]).
find_moves([(B,X,Y)|T],L):-
	get(B,x,Cx),
	get(B,y,Cy),
	final_position((Cx,Cy),(Move_x,Move_y),_,X,Y),
	find_moves(T,L1),
	append(L1,[(B,Move_x,Move_y)],L).



board_points(Board,Final_List,Points):-
	get_distance_points(Final_List,Board,P1),
	massive_points(Board,P2),
	Points is ((P1*10)+P2).

massive_points(Board,P2):-
	not((member((pacman,_,_),Board))),
	P2 is 8000.
massive_points(Board,P2):-
	difficulty(hard),
	delete(Board,(pacman,_,_),F),
	length(F,L),
	user_level(L1),
	L<L1,
	P2 is -8000.
massive_points(_,0).



get_distance_points(_,Board,0.8):-
	not((member((pacman,_,_),Board))).
get_distance_points(Final_List,Board,P):-
	findall((X,Y),(member((B,X,Y),Board),dif(B,pacman)),Places),
	member((pacman,Cx,Cy),Board),
	get_distance_points(Final_List,Places,(Cx,Cy),P).
get_distance_points(_,[],_,0).
get_distance_points([],[Place|T],(Cx,Cy),P):-
	distance_between_place_and_object((Cx,Cy),Place,D),
	FD is D+1,delete(T,Place,NewBoard),P1 is (1/FD),
	get_distance_points([],NewBoard,(Cx,Cy),OldP),
	P is OldP+P1.
get_distance_points([(Cx,Cy)|T],Board,(Cx1,Cy1),P1):-
	findall(D1,(member((X1,Y1),Board),distance_between_place_and_object((Cx,Cy),(X1,Y1),D1)),List_of_Distances),
	min_list(List_of_Distances,Min_D),
	member(Place,Board),distance_between_place_and_object((Cx,Cy),Place,Min_D),FMin_D is (Min_D+1),
	delete(Board,Place,NewBoard),delete(T,(Cx,Cy),T1),
	Points is (100/FMin_D),
	get_distance_points(T1,NewBoard,(Cx1,Cy1),OldP),
	P1 is OldP+Points.






distance_between_place_and_object((Cx,Cy),(Cx1,Cy1),Distance):-
	X_d1 is abs(Cx1-Cx),Y_d1 is abs(Cy1-Cy),
	pitaguras(X_d1,Y_d1,Distance1),
	X_d2_1 is abs(Cx1),Y_d2_1 is abs(Cy1-225),X_d2_2 is abs(Cx-500),Y_d2_2 is abs(Cy-225),
	pitaguras(X_d2_1,Y_d2_1,Distance2_1),pitaguras(X_d2_2,Y_d2_2,Distance2_2),sum_list([Distance2_1,Distance2_2],Distance2),
	X_d3_1 is abs(Cx),Y_d3_1 is abs(Cy-225),X_d3_2 is abs(Cx1-500),Y_d3_2 is abs(Cy1-225),
	pitaguras(X_d3_1,Y_d3_1,Distance3_1),pitaguras(X_d3_2,Y_d3_2,Distance3_2),sum_list([Distance3_1,Distance3_2],Distance3),
	min_list([Distance1,Distance2,Distance3],Distance).

pitaguras(X,Y,Result):-
	X1 is (X*X),Y1 is (Y*Y),Result is sqrt(X1+Y1).

findall_closer(_,[],[]).
findall_closer((Cx,Cy),Final_List,List):-
	findall(Distance1,(member((Cx11,Cy11),Final_List),distance_between_place_and_object((Cx,Cy),(Cx11,Cy11),Distance1)),L1),
	min_list(L1,Min),
	findall((Distance,Cx1,Cy1),(member((Cx1,Cy1),Final_List),distance_between_place_and_object((Cx,Cy),(Cx1,Cy1),Distance)),L2),
	member((Min,X,Y),L2),
	delete(Final_List,(X,Y),L3),
	findall_closer((Cx,Cy),L3,OldList),
	append([(X,Y)],OldList,List).




position_of_mouse((X5,Y5),(X,Y)):-
	mouse(B),
	get(B,x,Cx),
	get(B,y,Cy),
	X is (Cx+X5),Y is (Cy+Y5).


/*    end of smart move   */
player_move(M,_,X,Y,D):-
	order(D),
	mouse(B),
	possible(M,B,D),
	change_place(B,D),
	move(D,X,Y).
player_move(M,Direction,X,Y,Direction):-
	mouse(B),
	possible(M,B,Direction),
	change_place(B,Direction),
	move(Direction,X,Y).
player_move(_,D,0,0,D).




catch_order(B):-
	new(Kw,key_binding('w')),
	send(Kw,function,'w',message(@prolog, change_o, up)),
	send(B,recogniser,Kw),
	new(Kw1,key_binding('cursor_up')),
	send(Kw1,function,'cursor_up',message(@prolog, change_o, up)),
	send(B,recogniser,Kw1),

	new(Ks,key_binding('s')),
	send(Ks,function,'s',message(@prolog, change_o,down)),
	send(B,recogniser,Ks),
	new(Ks1,key_binding('cursor_down')),
	send(Ks1,function,'cursor_down',message(@prolog, change_o, down)),
	send(B,recogniser,Ks1),


	new(Kd,key_binding('d')),
	send(Kd,function,'d',message(@prolog, change_o,right)),
	send(B,recogniser,Kd),
	new(Kd1,key_binding('cursor_right')),
	send(Kd1,function,'cursor_right',message(@prolog, change_o, right)),
	send(B,recogniser,Kd1),


	new(Ka,key_binding('a')),
	send(Ka,function,'a',message(@prolog, change_o,left)),
	send(B,recogniser,Ka),
	new(Ka1,key_binding('cursor_left')),
	send(Ka1,function,'cursor_left',message(@prolog, change_o, left)),
	send(B,recogniser,Ka1).



move(left,-25,0).
move(right,25,0).
move(down,0,25).
move(up,0,-25).

possible(_,B,left):-
	get(B,x,X),X is 25,
	get(B,y,Y),Y is 225.

possible(_,B,right):-
	get(B,x,X),X is 475,
	get(B,y,Y),Y is 225.


possible(Map,B,Direction):-
	get(B,x,X),
	get(B,y,Y),
	not((compare(=,X,25),compare(=,Y,225),compare(=,Direction,left))),not((compare(=,X,475),compare(=,Y,225),compare(=,Direction,right))),
	move(Direction,X1,Y1),
	Fx is (X+X1),Fy is (Y+Y1),
	not((member((blank,Fx,Fy),Map))).

change_o(D):-
	retractall(order(_)),
	assert(order(D)).




check_point(P,NP):-
	mouse(B1),
	get(B1,x,X),
	get(B1,y,Y),
	point(X,Y,_),
	findall(B,point(X,Y,B),L),
	delete_all(L),
	NP is P+1.
check_point(P,P):-
	mouse(B1),
	get(B1,x,X),
	get(B1,y,Y),
	spoint(X,Y,_),
	findall(B,spoint(X,Y,B),L),
	delete_all(L),
	retractall(game_mode(_,0)),
	assert(game_mode(normal,20)).

check_point(P,P).



delete_all([]).
delete_all([B|T]):-
	retractall(point(_,_,B)),
	retractall(spoint(_,_,B)),
	free(B),
	delete_all(T).



check_wincat([],_,L1,L1,NewLbots):-
	findall(B,member((B,_,_),L1),NewLbots).
check_wincat([(H,_,_)|_],_,_,_,_):-
	game_mode(normal,0),
	get(H, x,Cx1),
	get(H, y,Cy1),
	mouse(B),
	get(B, x,Cx),
	get(B, y,Cy),
	Cx1 is Cx, Cy1 is Cy,
	assert(winner(ghost)).
check_wincat([(H,X,Y)|_],(X5,Y5),_,_,_):-
	game_mode(normal,0),
	get(H, x,Cx1),
	get(H, y,Cy1),
	mouse(B),
	get(B, x,Cx),
	get(B, y,Cy),
	Cx1 is (Cx+X5), Cy1 is (Cy+Y5),
	Cx is (Cx1+X), Cy is (Cy1+Y),
	assert(winner(ghost)).
check_wincat([(H,_,_)|_],_,L1,NewL1,NewLbots):-
	game_mode(normal,_),
	get(H, x,Cx1),
	get(H, y,Cy1),
	mouse(B),
	get(B, x,Cx),
	get(B, y,Cy),
	Cx1 is Cx, Cy1 is Cy,
	delete(L1,(H,_,_),NewL1),
	findall(B1,(member((B1,_,_),NewL1),dif(H,B1)),NewLbots),
	free(H).

check_wincat([(H,X,Y)|_],(X5,Y5),L1,NewL1,NewLbots):-
	game_mode(normal,_),
	get(H, x,Cx1),
	get(H, y,Cy1),
	mouse(B),
	get(B, x,Cx),
	get(B, y,Cy),
	Cx1 is (Cx+X5), Cy1 is (Cy+Y5),
	Cx is (Cx1+X), Cy is (Cy1+Y),
	delete(L1,(H,_,_),NewL1),
	findall(B1,(member((B1,_,_),NewL1),dif(H,B1)),NewLbots),
	free(H).
check_wincat([_|T],(X5,Y5),L1,NewL1,NewLbots):-
	check_wincat(T,(X5,Y5),L1,NewL1,NewLbots).


check_winmouse(_):-
	findall(B,point(_,_,B),[]),
	assert(winner(pacman)).

check_winmouse([]):-
	assert(winner(pacman)).

check_winmouse(_).

check_mode:-
	game_mode(normal,0).
check_mode:-
	game_mode(normal,N),
	retractall(game_mode(normal,N)),
	N1 is N-1,
	assert(game_mode(normal,N1)).


retractall:-
	retractall(order(_)),
	retractall(winner(_)),
	retractall(mouse(_)),
	retractall(game_mode(_,_)),
	retractall(user_level(_)),
	retractall(point(_,_,_)),
	retractall(spoint(_,_,_)).


change_place(B,left):-
	get(B,x,X),X is 25,
	get(B,y,Y),Y is 225,
	send(B,x(500)).

change_place(B,right):-
	get(B,x,X),X is 475,
	get(B,y,Y),Y is 225,
	send(B,x(0)).
change_place(_,_).
change_place_for_bots([]).
change_place_for_bots([(B,X,Y)|T]):-
	move(D,X,Y),
	change_place(B,D),
	change_place_for_bots(T).


/*        from here its only on the map/board        */

board_map1(L):-
	places_map1(L),
	display_map1.



places_map1(L):-
	places_map1([0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20],[0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20],[0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20],L).


places_map1([],_,_,[]).
places_map1([_|T],[],L,List):-
	places_map1(T,L,L,List).

/*    s_points    */
places_map1([H1|T1],[H2|T2],L,NList):-
	H1 = 1,H2 is 1,
	X is H1*25,
	Y is H2*25,
	X1 is X+5,Y1 is Y+5,
	places_map1([H1|T1],T2,L,List),
	append([(spoint,X,Y)],List,NList),
	new(B,circle(15)),
	send(B,fill_pattern,colour(red)),
	send(@window,display,B,point(X1,Y1)),
	assert(spoint(X,Y,B)).
places_map1([H1|T1],[H2|T2],L,NList):-
	H1 is 19,H2 is 1,
	X is H1*25,
	Y is H2*25,
	X1 is X+5,Y1 is Y+5,
	places_map1([H1|T1],T2,L,List),
	append([(spoint,X,Y)],List,NList),
	new(B,circle(15)),
	send(B,fill_pattern,colour(red)),
	send(@window,display,B,point(X1,Y1)),
	assert(spoint(X,Y,B)).
places_map1([H1|T1],[H2|T2],L,NList):-
	H1 is 19,H2 is 13,
	X is H1*25,
	Y is H2*25,
	X1 is X+5,Y1 is Y+5,
	places_map1([H1|T1],T2,L,List),
	append([(spoint,X,Y)],List,NList),
	new(B,circle(15)),
	send(B,fill_pattern,colour(red)),
	send(@window,display,B,point(X1,Y1)),
	assert(spoint(X,Y,B)).
places_map1([H1|T1],[H2|T2],L,NList):-
	H1 is 1,H2 is 13,
	X is H1*25,
	Y is H2*25,
	X1 is X+5,Y1 is Y+5,
	places_map1([H1|T1],T2,L,List),
	append([(spoint,X,Y)],List,NList),
	new(B,circle(15)),
	send(B,fill_pattern,colour(red)),
	send(@window,display,B,point(X1,Y1)),
	assert(spoint(X,Y,B)).




/*  special blank boxes   */

/*     0     */

places_map1([H1|T1],[H2|T2],L,NList):-
	H1>=2,H1=<5,
	H2>=2,H2=<4,
	X is H1*25,
	Y is H2*25,
	places_map1([H1|T1],T2,L,List),
	append([(blank,X,Y)],List,NList).
places_map1([H1|T1],[H2|T2],L,NList):-
	H1>=7,H1=<8,
	H2>=2,H2=<4,
	X is H1*25,
	Y is H2*25,
	places_map1([H1|T1],T2,L,List),
	append([(blank,X,Y)],List,NList).
places_map1([H1|T1],[H2|T2],L,NList):-
	H1>=10,H1=<10,
	H2>=1,H2=<4,
	X is H1*25,
	Y is H2*25,
	places_map1([H1|T1],T2,L,List),
	append([(blank,X,Y)],List,NList).
places_map1([H1|T1],[H2|T2],L,NList):-
	H1>=15,H1=<18,
	H2>=2,H2=<4,
	X is H1*25,
	Y is H2*25,
	places_map1([H1|T1],T2,L,List),
	append([(blank,X,Y)],List,NList).
places_map1([H1|T1],[H2|T2],L,NList):-
	H1>=12,H1=<13,
	H2>=2,H2=<4,
	X is H1*25,
	Y is H2*25,
	places_map1([H1|T1],T2,L,List),
	append([(blank,X,Y)],List,NList).

/*     1     */


places_map1([H1|T1],[H2|T2],L,NList):-
	H1>=5,H1=<5,
	H2>=10,H2=<12,
	X is H1*25,
	Y is H2*25,
	places_map1([H1|T1],T2,L,List),
	append([(blank,X,Y)],List,NList).
places_map1([H1|T1],[H2|T2],L,NList):-
	H1>=3,H1=<3,
	H2>=12,H2=<14,
	X is H1*25,
	Y is H2*25,
	places_map1([H1|T1],T2,L,List),
	append([(blank,X,Y)],List,NList).
places_map1([H1|T1],[H2|T2],L,NList):-
	H1>=15,H1=<15,
	H2>=10,H2=<12,
	X is H1*25,
	Y is H2*25,
	places_map1([H1|T1],T2,L,List),
	append([(blank,X,Y)],List,NList).
places_map1([H1|T1],[H2|T2],L,NList):-
	H1>=17,H1=<17,
	H2>=12,H2=<14,
	X is H1*25,
	Y is H2*25,
	places_map1([H1|T1],T2,L,List),
	append([(blank,X,Y)],List,NList).

/*     2     */

places_map1([H1|T1],[H2|T2],L,NList):-
	H1>=1,H1=<3,
	H2>=8,H2=<8,
	X is H1*25,
	Y is H2*25,
	places_map1([H1|T1],T2,L,List),
	append([(blank,X,Y)],List,NList).
places_map1([H1|T1],[H2|T2],L,NList):-
	H1>=17,H1=<19,
	H2>=8,H2=<8,
	X is H1*25,
	Y is H2*25,
	places_map1([H1|T1],T2,L,List),
	append([(blank,X,Y)],List,NList).
places_map1([H1|T1],[H2|T2],L,NList):-
	H1>=1,H1=<3,
	H2>=10,H2=<10,
	X is H1*25,
	Y is H2*25,
	places_map1([H1|T1],T2,L,List),
	append([(blank,X,Y)],List,NList).
places_map1([H1|T1],[H2|T2],L,NList):-
	H1>=17,H1=<19,
	H2>=10,H2=<10,
	X is H1*25,
	Y is H2*25,
	places_map1([H1|T1],T2,L,List),
	append([(blank,X,Y)],List,NList).

/*     3     */

places_map1([H1|T1],[H2|T2],L,NList):-
	H1>=5,H1=<5,
	H2>=7,H2=<8,
	X is H1*25,
	Y is H2*25,
	places_map1([H1|T1],T2,L,List),
	append([(blank,X,Y)],List,NList).
places_map1([H1|T1],[H2|T2],L,NList):-
	H1>=5,H1=<5,
	H2>=14,H2=<15,
	X is H1*25,
	Y is H2*25,
	places_map1([H1|T1],T2,L,List),
	append([(blank,X,Y)],List,NList).
places_map1([H1|T1],[H2|T2],L,NList):-
	H1>=15,H1=<15,
	H2>=7,H2=<8,
	X is H1*25,
	Y is H2*25,
	places_map1([H1|T1],T2,L,List),
	append([(blank,X,Y)],List,NList).
places_map1([H1|T1],[H2|T2],L,NList):-
	H1>=15,H1=<15,
	H2>=14,H2=<15,
	X is H1*25,
	Y is H2*25,
	places_map1([H1|T1],T2,L,List),
	append([(blank,X,Y)],List,NList).
places_map1([H1|T1],[H2|T2],L,NList):-
	H1>=10,H1=<10,
	H2>=13,H2=<14,
	X is H1*25,
	Y is H2*25,
	places_map1([H1|T1],T2,L,List),
	append([(blank,X,Y)],List,NList).

/*     4     */

places_map1([H1|T1],[H2|T2],L,NList):-
	H1>=2,H1=<8,
	H2>=6,H2=<6,
	X is H1*25,
	Y is H2*25,
	places_map1([H1|T1],T2,L,List),
	append([(blank,X,Y)],List,NList).
places_map1([H1|T1],[H2|T2],L,NList):-
	H1>=12,H1=<18,
	H2>=6,H2=<6,
	X is H1*25,
	Y is H2*25,
	places_map1([H1|T1],T2,L,List),
	append([(blank,X,Y)],List,NList).
places_map1([H1|T1],[H2|T2],L,NList):-
	H1>=7,H1=<13,
	H2>=12,H2=<12,
	X is H1*25,
	Y is H2*25,
	places_map1([H1|T1],T2,L,List),
	append([(blank,X,Y)],List,NList).

/*     5     */

places_map1([1|T1],[14|T2],L,NList):-
	X is 1*25,
	Y is 14*25,
	places_map1([1|T1],T2,L,List),
	append([(blank,X,Y)],List,NList).
places_map1([H1|T1],[H2|T2],L,NList):-
	H1>=2,H1=<2,
	H2>=12,H2=<12,
	X is H1*25,
	Y is H2*25,
	places_map1([H1|T1],T2,L,List),
	append([(blank,X,Y)],List,NList).
places_map1([H1|T1],[H2|T2],L,NList):-
	H1>=18,H1=<18,
	H2>=12,H2=<12,
	X is H1*25,
	Y is H2*25,
	places_map1([H1|T1],T2,L,List),
	append([(blank,X,Y)],List,NList).
places_map1([H1|T1],[H2|T2],L,NList):-
	H1>=19,H1=<19,
	H2>=14,H2=<14,
	X is H1*25,
	Y is H2*25,
	places_map1([H1|T1],T2,L,List),
	append([(blank,X,Y)],List,NList).

/*     6     */

places_map1([H1|T1],[H2|T2],L,NList):-
	H1>=7,H1=<8,
	H2>=14,H2=<14,
	X is H1*25,
	Y is H2*25,
	places_map1([H1|T1],T2,L,List),
	append([(blank,X,Y)],List,NList).
places_map1([H1|T1],[H2|T2],L,NList):-
	H1>=12,H1=<13,
	H2>=14,H2=<14,
	X is H1*25,
	Y is H2*25,
	places_map1([H1|T1],T2,L,List),
	append([(blank,X,Y)],List,NList).

/*     7     */

places_map1([H1|T1],[H2|T2],L,NList):-
	H1>=2,H1=<9,
	H2>=16,H2=<18,
	X is H1*25,
	Y is H2*25,
	places_map1([H1|T1],T2,L,List),
	append([(blank,X,Y)],List,NList).
places_map1([H1|T1],[H2|T2],L,NList):-
	H1>=11,H1=<18,
	H2>=16,H2=<18,
	X is H1*25,
	Y is H2*25,
	places_map1([H1|T1],T2,L,List),
	append([(blank,X,Y)],List,NList).

/*     8     */

places_map1([H1|T1],[H2|T2],L,NList):-
	H1>=7,H1=<13,
	H2>=8,H2=<10,
	X is H1*25,
	Y is H2*25,
	places_map1([H1|T1],T2,L,List),
	append([(blank,X,Y)],List,NList).


/*    borders    */
places_map1([0|T1],[H2|T2],L,NList):-
	X is 0,
	Y is H2*25,
	places_map1([0|T1],T2,L,List),
	append([(blank,X,Y)],List,NList).
places_map1([H1|T1],[0|T2],L,NList):-
	X is H1*25,
	Y is 0,
	places_map1([H1|T1],T2,L,List),
	append([(blank,X,Y)],List,NList).
places_map1([20|T1],[H2|T2],L,NList):-
	X is 500,
	Y is H2*25,
	places_map1([20|T1],T2,L,List),
	append([(blank,X,Y)],List,NList).
places_map1([H1|T1],[20|T2],L,NList):-
	X is H1*25,
	Y is 500,
	places_map1([H1|T1],T2,L,List),
	append([(blank,X,Y)],List,NList).


/*    valid ways without points    */

places_map1([H1|T1],[H2|T2],L,NList):-
	H1>=9,H1=<11,
	H2>=6,H2=<6,
	X is H1*25,
	Y is H2*25,
	places_map1([H1|T1],T2,L,List),
	append([(nopoint,X,Y)],List,NList).
places_map1([H1|T1],[H2|T2],L,NList):-
	H1>=6,H1=<14,
	H2>=7,H2=<7,
	X is H1*25,
	Y is H2*25,
	places_map1([H1|T1],T2,L,List),
	append([(nopoint,X,Y)],List,NList).
places_map1([H1|T1],[H2|T2],L,NList):-
	H1>=6,H1=<14,
	H2>=11,H2=<11,
	X is H1*25,
	Y is H2*25,
	places_map1([H1|T1],T2,L,List),
	append([(nopoint,X,Y)],List,NList).
places_map1([H1|T1],[H2|T2],L,NList):-
	H1>=6,H1=<6,
	H2>=8,H2=<10,
	X is H1*25,
	Y is H2*25,
	places_map1([H1|T1],T2,L,List),
	append([(nopoint,X,Y)],List,NList).
places_map1([H1|T1],[H2|T2],L,NList):-
	H1>=14,H1=<14,
	H2>=8,H2=<10,
	X is H1*25,
	Y is H2*25,
	places_map1([H1|T1],T2,L,List),
	append([(nopoint,X,Y)],List,NList).


/*    valid ways with points    */
places_map1([H1|T1],[H2|T2],L,NList):-
	X is H1*25,
	Y is H2*25,
	X1 is X+10,Y1 is Y+10,
	places_map1([H1|T1],T2,L,List),
	append([(point,X,Y)],List,NList),
	new(B,box(5,5)),
	send(B,fill_pattern,colour(yellow)),
	send(@window,display,B,point(X1,Y1)),
	assert(point(X,Y,B)).



display_map1:-

	/*     border     */
	box_style(25,225,0,0),
	box_style(25,275,0,250),
	box_style(25,225,500,0),
	box_style(25,275,500,250),
	box_style(475,25,25,0),
	box_style(475,25,25,500),

	/*     0     */
	box_style(100,75,50,50),
	box_style(50,75,175,50),
	box_style(25,100,250,25),
	box_style(100,75,375,50),
	box_style(50,75,300,50),

	/*     1     */
	box_style(25,75,75,300),
	box_style(25,75,125,250),
	box_style(25,75,375,250),
	box_style(25,75,425,300),

	/*     2     */
	box_style(75,25,25,200),
	box_style(75,25,25,250),
	box_style(75,25,425,200),
	box_style(75,25,425,250),

	/*     3     */
	box_style(25,50,125,175),
	box_style(25,50,125,350),
	box_style(25,50,375,175),
	box_style(25,50,375,350),
	box_style(25,50,250,325),

	/*     4     */
	box_style(175,25,50,150),
	box_style(175,25,300,150),
	box_style(175,25,175,300),

	/*     5     */
	box_style(25,25,25,350),
	box_style(25,25,50,300),
	box_style(25,25,475,350),
	box_style(25,25,450,300),

	/*     6     */
	box_style(50,25,175,350),
	box_style(50,25,300,350),

	/*     7     */
	box_style(200,75,50,400),
	box_style(200,75,275,400),

	/*     8     */
	box_style(175,75,175,200)

	.



box_style(Size_w,Size_h,Point_x,Point_y):-
	new(B,box(Size_w,Size_h)),
	send(B,fill_pattern,colour(black)),
	send(@window,display,B,point(Point_x,Point_y)).



/*        The End          */
