:- module(_,
    [
        connect_to_postal_service/2
    ]).

:- include(calendar12(include/common)).

connect_to_postal_service(Host, Port) :-
    mutex_create(mongo_mutex),
    setup_call_catcher_cleanup(
        tcp_socket(Socket),
        tcp_connect(Socket, Host:Port),
        exception(_),
        tcp_close_socket(Socket)),
    setup_call_cleanup(
        tcp_open_socket(Socket, In, Out),
        run_program(postal(In,Out)),
        close_connection(In, Out)).

run_program(postal(PostalIn,PostalOut)) :-
    register_for_actions(postal(PostalIn,PostalOut), [walk,look]),
    setup_call_cleanup(
        (
            mongo:new_connection(Connection),
            mongo:get_database(Connection, 'fnnl_room_service', Database),
            mongo:get_collection(Database, 'rooms', Collection)
        ),
        handle_service(PostalIn, PostalOut, Collection),
        mongo:free_connection(Connection)).

register_for_actions(postal(_PostalIn,PostalOut), Actions) :-
    json:doc_json(json([actions-Actions]), Payload),
    core:atomic_list_concat(['register: ',Payload], Message),
    send_and_flush(PostalOut, Message).

close_connection(In, Out) :-
    close(In, [force(true)]),
    close(Out, [force(true)]).

handle_service(In, Out, Collection) :-
    receive_line(In, Bytes),
    handle_client_response(In, Out, Collection, Bytes).

handle_client_response(In, Out, Collection, Bytes) :-
    Bytes = [a,c,t,i,o,n,:,' '|BytesJson],
    !,
    core:atom_chars(JsonAtom, BytesJson),
    json:doc_json(Doc, JsonAtom),
    log('Parsed: ~w', [Doc]),
    json_get(Doc, command, Command),
    handle_action(Command, postal(In,Out), Collection, Doc),
    handle_service(In, Out, Collection).
handle_client_response(In, Out, Collection, Bytes) :-
    log('Unhandled: ~w', [Bytes]),
    handle_service(In, Out, Collection).

handle_action(walk, postal(In,Out), Collection, Doc) :-
    !,
    handle_action_walk(postal(In,Out), Collection, Doc).
handle_action(look, postal(In,Out), Collection, Doc) :-
    !,
    handle_action_look(postal(In,Out), Collection, Doc).
handle_action(Command, _, _, _) :-
    log('Unhandled command: ~w', [Command]).

handle_action_walk(postal(In,Out), Collection, Doc) :-
    json_get(Doc, args, [User|_]),
    get_all_rooms(Collection, Rooms),
    move_user(Rooms, User, Out, num-Num, Rooms1),
    mongo:delete(Collection, []),
    mongo:insert(Collection, [rooms-Rooms1]),
    core:atomic_list_concat(['event: {"msg":"',User,' awoke in room ',Num,'"}'], Msg),
    send_and_flush(Out, Msg).

handle_action_look(postal(In,Out), Collection, Doc) :-
    json_get(Doc, args, [User|_]),
    find_roommates(Collection, User, Roommates),
    create_roommates_msg(Roommates, User, MsgAtom),
    json:doc_json(json([msg-MsgAtom]), JsonAtom),
    core:atomic_list_concat(['event: ',JsonAtom], Msg),
    send_and_flush(Out, Msg).

create_roommates_msg([], User, MsgAtom) :-
    !,
    core:atomic_list_concat([User,' is alone'], MsgAtom).
create_roommates_msg(Roommates, User, MsgAtom) :-
    core:atomic_list_concat(Roommates, ', ', RoommatesAtom),
    core:atomic_list_concat([User,' is in the same room as ',RoommatesAtom], MsgAtom).

find_roommates(Collection, User, Roommates) :-
    get_all_rooms(Collection, Rooms),
    search_rooms(Rooms, User, Roommates).

search_rooms([], _, []).
search_rooms([[Num,users-Users]|Rooms], User, Roommates) :-
    delete(Users, User, Roommates),
    !.
search_rooms([Room|Rooms], User, Roommates) :-
    search_rooms(Rooms, User, Roommates).

move_user(Rooms, User, Out, num-N1, Rooms2) :-
    remove_user_from_room(Rooms, User, Out, num-N, Rooms1),
    N1 is N + 1,
    add_user_to_room(Rooms1, User, Out, num-N1, Rooms2).

add_user_to_room([[num-N,users-Users]|Rooms], User, Out, num-N, [[num-N,users-[User|Users]]|Rooms]) :-
    !.
add_user_to_room([Room|Rooms], User, Out, Num, [Room|Rooms1]) :-
    add_user_to_room(Rooms, User, Out, Num, Rooms1).

remove_user_from_room([[num-N,users-Users]], User, Out, num-0, [[num-N,users-Users1]]) :-
    delete(Users, User, Users1),
    !,
    core:atomic_list_concat(['event: {"msg":"',User,' fell off a cliff"}'], Msg),
    send_and_flush(Out, Msg),
    core:atomic_list_concat(['event: {"msg":"',User,' was reincarnated in room 1"}'], Msg2),
    send_and_flush(Out, Msg2).
remove_user_from_room([[num-N,users-Users]], User, Out, num-0, [[num-N,users-Users1]]) :-
    !,
    try_delete(Users, User, Users1).
remove_user_from_room([[num-N,users-Users]|Rooms], User, Out, num-N, [[num-N,users-Users1]|Rooms]) :-
    delete(Users, User, Users1),
    !.
remove_user_from_room([Room|Rooms], User, Out, Num, [Room|Rooms1]) :-
    remove_user_from_room(Rooms, User, Out, Num, Rooms1).

try_delete([], _, []) :- !.
try_delete([X|Xs], X, Xs) :- !.
try_delete([Y|Xs], X, [Y|Xs1]) :-
    try_delete(Xs, X, Xs1).

delete([X|Xs], X, Xs) :- !.
delete([Y|Xs], X, [Y|Xs1]) :-
    delete(Xs, X, Xs1).

get_all_rooms(Collection, Rooms1) :-
    mongo:find_all(Collection, [], [], Rooms),
    %log('get_all_rooms:~n~w~n', [Rooms]),
    init_rooms(Collection, Rooms, Rooms1).

init_rooms(Collection, [], Rooms1) :-
    !,
    create_rooms(Collection, Rooms1).
init_rooms(_Collection, [RoomsDoc], RoomsList) :-
    bson:doc_get(RoomsDoc, rooms, RoomsList).

create_rooms(Collection, RoomsRev) :-
    create_n_rooms(10, Rooms),
    lists:reverse(Rooms, RoomsRev),
    mongo:upsert(Collection, [rooms-1], [rooms-RoomsRev]).

create_n_rooms(0, []) :- !.
create_n_rooms(N, [[num-N,users-[]]|Rooms]) :-
    N1 is N - 1,
    create_n_rooms(N1, Rooms).

send_and_flush(Socket, Message) :-
    core:format(Socket, '~w~n', [Message]),
    core:flush_output(Socket),
    sleep(0.1). % Just to please Akka ...

log(Format, Args) :-
    core:atomic_list_concat([Format,'~n'], Format1),
    core:format(Format1, Args),
    core:flush_output.

receive_line(In, Bytes) :-
    core:get_char(In, Byte),
    receive_line_aux(In, Byte, Bytes).

receive_line_aux(In, Byte, []) :-
    stop_char(Byte),
    !.
receive_line_aux(In, Byte, [Byte|Bytes]) :-
    receive_line(In, Bytes).

stop_char('\n').
stop_char('\r').

json_get(json(Pairs), Key, Value) :-
    json_get_aux(Pairs, Key, Value).

json_get_aux([Key-Value|_], Key, Value) :- !.
json_get_aux([_Pair|Pairs], Key, Value) :-
    json_get_aux(Pairs, Key, Value).
